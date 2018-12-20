library(ggplot2)
library(ggpubr)
library(tree)
library(ISLR)
library(rpart)
library(rand)
library(randomForest)
library(gbm)
library(C50)
library(VIM)
library(FactoMineR)

attach(Carseats)
High = ifelse(Sales <= 8, "No", "Yes")
Carseats = data.frame(Carseats, High)

arbol_carseat <- tree(High ~ . -Sales, data = Carseats)
summary(arbol_carseat)
plot(arbol_carseat)
text(arbol_carseat, pretty= 0)

set.seed(1)
train <- sample(1:nrow(Boston), size = nrow(Boston)/2)
arbol_regresion <- tree(formula = medv ~ ., data = Boston, subset = train,
                        split = "deviance")
plot(x = arbol_regresion, type = "proportional")
text(x = arbol_regresion, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

set.seed(3)
cv_arbol <- cv.tree(arbol_regresion, K = 10)
cv_arbol


resultados_cv <- data.frame(n_nodos = cv_arbol$size, deviance = cv_arbol$dev,
                            alpha = cv_arbol$k)
p1 <- ggplot(data = resultados_cv, aes(x = n_nodos, y = deviance)) +
  geom_line() + 
  geom_point() +
  labs(title = "Error vs tamaño del árbol") + theme_bw() 


p2 <- ggplot(data = resultados_cv, aes(x = alpha, y = deviance)) +
  geom_line() + 
  geom_point() +
  labs(title = "Error vs hiperparámetro alpha") + theme_bw() 

ggarrange(p1, p2)

arbol_pruning <- prune.tree(tree = arbol_regresion, best = 8)
plot(x = arbol_pruning, type = "proportional")
text(x = arbol_pruning, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

predicciones <- predict(arbol_pruning, newdata = Boston[-train,])
test_mse     <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del árbol de regresión tras podado:", round(test_mse,2))

predicciones <- predict(arbol_pruning, newdata = Carseats_test, type = "class")
table(predicciones, Carseats_test$ventas_altas)


# Regresion  --------------------------------------------------------------


# Método 1 - Incluir de a una variable

regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~.,data=Hitters ,nvmax =19)
reg.summary = summary(regfit.full)
names(reg.summary)

plot(reg.summary$adjr2,
     reg.summary$rsq)

par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",
     type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",
     ylab=" Adjusted RSq",type="l")
which.max (reg.summary$adjr2)
points (11, reg.summary$adjr2[11], col ="red",cex =2, pch =20)
plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",
     type="l")
which.min(reg.summary$cp)


plot(reg.summary$cp ,xlab =" Number of Variables ",ylab="Cp",
     type="l")
which.min(reg.summary$cp)
points(10, reg.summary$cp [10], col ="red",cex =2, pch =20)
which.min(reg.summary$bic)
plot(reg.summary$bic ,xlab=" Number of Variables ",ylab=" BIC",
     type="l")
points(6, reg.summary$bic [6], col =" red",cex =2, pch =20)
coef(regfit.full, 6)
plot(regfit.full ,scale ="r2")

# Método 2 - Forward and Stepwise Selection

regfit.fw=regsubsets(Salary~.,data=Hitters ,nvmax =19, method = "forward")
plot(regfit.fw ,scale ="bic")

regfit.bw=regsubsets(Salary~.,data=Hitters ,nvmax =19, method = "backward")
plot(regfit.bw ,scale ="bic")

coef(regfit.full, 7)


# Con una muestra de entrenamiento y prueba
set.seed(1)
train = sample(c(T,F), nrow(Hitters), rep = T)
test=(!train)
regfit.best = regsubsets(Salary~., data = Hitters[train,])
test.mat = model.matrix(Salary~.,data=Hitters [test ,])

val.errors = rep(NA, 19)
for(i in 1:(ncol(Hitters)-1)){
  coefi = coef(regfit.best, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}


k=10
set.seed (1)
folds=sample (1:k,nrow(Hitters ),replace =TRUE)
cv.errors =matrix (NA ,k,19, dimnames =list(NULL , paste (1:19) ))
for(j in 1:k){
  best.fit =regsubsets(Salary~.,data=Hitters [folds !=j,], nvmax =19)
  for(i in 1:8) {
    coefi = coef(regfit.best, id = i)
    pred = model.matrix(Salary~.,data=Hitters [folds ==j,]) %>% .[, names(coefi)]
    pred = pred%*%coefi
    cv.errors[j,i]=mean( (Hitters$Salary[folds ==j]-pred)^2)
  }
}
mean.cv.errors =apply(cv.errors ,2, mean)
mean.cv.errors
par(mfrow =c(1,1))
plot(mean.cv.errors ,type="b")
reg.best=regsubsets (Salary~.,data=Hitters , nvmax =19)
coef(reg.best ,11)


# Ridge regression and Lasso


x=model.matrix(Salary∼.,Hitters )[,-1]
y=Hitters$Salary

grid =10^seq(10,-2, length =100)
ridge.mod =glmnet(x,y,alpha =0, lambda =grid)
coef(ridge.mod)
predict(ridge.mod ,s=50, type ="coefficients")

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

ridge.mod =glmnet (x[train ,],y[train],alpha =0, lambda =grid ,
                   thresh =1e-12)
ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,])
mean(( ridge.pred -y.test)^2)
mean(( mean(y[train ])-y.test)^2)


set.seed(1)
cv.out =cv.glmnet (x[train ,],y[train],alpha =0)
plot(cv.out)
bestlam =cv.out$lambda.min
ridge.pred=predict (ridge.mod ,s=bestlam ,newx=x[test ,])
mean(( ridge.pred -y.test)^2)
out=glmnet(x,y,alpha =0)
predict(out ,type= "coefficients",s=bestlam )[1:20 ,]


#LASSO
lasso.mod =glmnet(x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)


cv.out =cv.glmnet(x[train ,],y[train],alpha =1)
bestlam = cv.out$lambda.min
lasso.pred=predict(lasso.mod ,s=bestlam ,newx=x[test ,])
mean(( lasso.pred -y.test)^2)

out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)

