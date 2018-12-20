library(ISLR)
library(leaps)
library(glmnet)
library(fTrading)
library(zoo)
library(forecast)
library(tseries)
library(MASS)
library(e1071)
require(foreign)
require(nnet)
require(ggplot2)
library(randomForest)
library(tree)
require(reshape2)
library(car)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

set.seed(1234)


facebook <- read.delim("dataset_Facebook.csv",sep = ";")
head(facebook)
names(facebook) <- c("total_likes_pagina",
                     "tipo",
                     "categoria",
                     "mes",
                     "dia",
                     "hora",
                     "pago",
                     "personas_vieron",
                     "numero_impresiones",
                     "usuarios_comprometidos",
                     "usuarios_comprometidos_unicos",
                     "numero_cliks",
                     "usuarios_like_impresiones",
                     "usuarios_like_vieron",
                     "usuarios_like_comprometidos",
                     "comentarios",
                     "likes",
                     "compartida",
                     "interacciones")

facebook$categoria <- factor(facebook$categoria)

# Reto # 1 - Limpiando la base --------------------------------------------

facebook <- na.omit(facebook)


# Reto Numero 2 -----------------------------------------------------------

tiempo <- paste0("2014-",facebook$mes,"-", facebook$dia, " ", facebook$hora,":00:00") %>% as.POSIXct()
base_grafica <- data.frame(Dia = tiempo, Likes = facebook$total_likes_pagina, Tipo = facebook$tipo)
p <- ggplot(base_grafica, aes(x = Dia, y = Likes, colour = Tipo))
p + geom_point(alpha = 0.4)

plot(tiempo, facebook$total_likes_pagina, pch = 20, col = "lightblue")

# Resumenes por tipo de publicacion

facebook_tipo <- group_by(facebook, tipo) %>% summarise(.,
                                                        impresiones = mean(numero_impresiones),
                                                        interacciones = mean(interacciones),
                                                        comprometidos = mean(usuarios_comprometidos_unicos),
                                                        compartidos = mean(compartida),
                                                        likes = mean(likes),
                                                        comentarios = mean(comentarios),
                                                        click_usuario = mean(usuarios_comprometidos/numero_cliks))

tiempo <- paste0("2014-",facebook$mes,"-", facebook$dia, " ", facebook$hora,":00:00") %>% as.POSIXct()

base_compr <- data.frame(Dia = tiempo,
                         Unicos = facebook$usuarios_comprometidos_unicos,
                         Tipo = facebook$tipo,
                         compr_likes = facebook$usuarios_like_comprometidos,
                         compr = facebook$usuarios_comprometidos)

p <- ggplot(base_compr, aes(x = Dia, y = compr, colour = Tipo))
p + geom_point(alpha = 0.4)

mean(facebook$usuarios_like_comprometidos)
mean(facebook$usuarios_comprometidos)
mean(facebook$usuarios_comprometidos_unicos)
mean(facebook$numero_cliks)/mean(facebook$usuarios_comprometidos) # Numero promedio de click por usuario unico o no

# Evaluando la posibilidad de la existencia de datos atipicos -------------

p <- ggplot(facebook, aes(x = categoria, y = interacciones, fill = tipo))
p + geom_boxplot()

p1 <- ggplot(facebook, aes(x = categoria, y = numero_impresiones, fill = tipo))
p1 + geom_boxplot()

p2 <- ggplot(facebook, aes(x = categoria, y = personas_vieron, fill = tipo))
p2 + geom_boxplot()

# Buscando datos atípicos multivariados

facebook_continuas <- facebook[, -c(1:7, 19)] # Se quitan las interacciones por ser una combinacion lineal de las otras
medias <- colMeans(facebook_continuas)
varianza <- cov(facebook_continuas)
distancias <- mahalanobis(facebook_continuas, medias, varianza)
boxplot(log(distancias))
k <- which(distancias > 86)

scatterplotMatrix(facebook_continuas)

log_face <- log(facebook[,-c(1:7)] + 1)
scatterplotMatrix(log_face )

train <- sample(1:nrow(facebook), nrow(facebook)*0.8)

log_face_uso <- data.frame(facebook[1:7], log_face)

# Arboles de desicion -----------------------------------------------------

arbol_clase <- tree(categoria ~ ., data = facebook)
summary(arbol_clase)
plot(arbol_clase)
text(arbol_clase, pretty = 0)

arbol_clase <- tree(categoria ~ ., data = log_face_uso)
summary(arbol_clase)
plot(arbol_clase)
text(arbol_clase, pretty = 0)


#Dividiendo en conjunto de entrenamiento y prueba

arbol_clase <- tree(categoria ~ ., data = facebook[train,])
summary(arbol_clase)
plot(arbol_clase)
text(arbol_clase, pretty = 0)

predicciones <- predict(arbol_clase, newdata = facebook[-train,])
predicciones <- apply(predicciones, 1, which.is.max)
validacion <- table(predicciones, facebook$categoria[-train])
error <- validacion/sum(validacion)
sum(diag(error))

log_arbol_clase <- tree(categoria ~ ., data = log_face_uso[train,])
summary(log_arbol_clase)
plot(log_arbol_clase)
text(log_arbol_clase, pretty = 0)

predicciones_log <- predict(log_arbol_clase, newdata = log_face_uso[-train,])
predicciones_log <- apply(predicciones_log, 1, which.is.max)
validacion <- table(predicciones, log_face_uso$categoria[-train])
log_error <- validacion/sum(validacion)
sum(diag(log_error))

# Discriminante lineal ----------------------------------------------------

facebook_lda <- facebook[-k, -19] # La 19 genera problemas de colinealidad
facebook_lda_log <- log_face_uso[-k, -19] # La 19 genera problemas de colinealidad

ajuste_lda <- lda(categoria ~ ., data = facebook_lda, subset = train)
ajuste_lda
plot(ajuste_lda)
lda_predicciones <- predict(ajuste_lda, newdata = facebook_lda[-train,])
names(lda_predicciones)
vcruz <- table(lda_predicciones$class, facebook_lda$categoria[-train])
1 - sum(diag(vcruz))/sum(vcruz)

ajuste_lda_log <- lda(categoria ~ ., data = facebook_lda_log, subset = train)
ajuste_lda_log
plot(ajuste_lda_log)
lda_predicciones_log <- predict(ajuste_lda_log, newdata = facebook_lda_log[-train,])
names(lda_predicciones_log)
vcruz_log <- table(lda_predicciones_log$class, facebook_lda_log$categoria[-train])
sum(diag(vcruz_log))/sum(vcruz_log)
1 - sum(diag(vcruz_log))/sum(vcruz_log)

# Discriminante Cuadratico ------------------------------------------------

facebook_qda <- facebook[-k , -c(1:2, 4:7, 19)]
facebook_qda_log <- log_face_uso[-k , -c(1:2, 4:7, 19)]

ajuste_qda <- qda(categoria ~ ., data = facebook_qda, subset = train)
summary(ajuste_qda)
qda_predicciones <- predict(ajuste_qda, newdata = facebook_qda[-train,])
names(qda_predicciones)
vcruz <- table(qda_predicciones$class, facebook_qda$categoria[-train])
sum(diag(vcruz))/sum(vcruz)

ajuste_qda_log <- qda(categoria ~ ., data = facebook_qda_log, subset = train)
summary(ajuste_qda_log)
qda_predicciones_log <- predict(ajuste_qda_log, newdata = facebook_qda_log[-train,])
names(qda_predicciones_log)
vcruz_log <- table(qda_predicciones_log$class, facebook_qda_log$categoria[-train])
sum(diag(vcruz_log))/sum(vcruz_log)
1 - sum(diag(vcruz_log))/sum(vcruz_log)

facebook_qda <- facebook[, -c(1:2, 4:7, 19)]


# k-nn --------------------------------------------------------------------

categoria <- facebook[-k, "categoria"]
facebook_knn <- facebook[-k , -c(1:7, 17, 19)]
train <- sample(1:nrow(facebook_knn), nrow(facebook_knn)*0.8)
  
ajuste_knn <- knn(facebook_knn[train, ],
                  facebook_knn[-train, ],
                  categoria[train],
                  k = 1)

vcruz <- table(ajuste_knn, categoria[-train])
sum(diag(vcruz))/sum(vcruz)

facebook_knn <- log_face_uso[-k , -c(1:7, 17, 19)]
train <- sample(1:nrow(facebook_knn), nrow(facebook_knn)*0.8)

ajuste_knn <- knn(facebook_knn[train, ],
                  facebook_knn[-train, ],
                  categoria[train],
                  k = 1)

vcruz <- table(ajuste_knn, categoria[-train])
sum(diag(vcruz))/sum(vcruz)

# Regresion Logistica -----------------------------------------------------

set.seed(1234)
facebook_log <- facebook[-k, -c(1, 4:6, 19)]
facebook_log_log <- log_face_uso[-k, -c(1, 4:6, 19)]

ajuste_logistico1 <- polr(categoria ~ pago +
                           log(personas_vieron)+
                           log(numero_impresiones) +
                           usuarios_comprometidos +
                           numero_cliks +
                           log(usuarios_like_impresiones)+
                           log(usuarios_like_vieron)+
                           usuarios_like_comprometidos +
                           comentarios +
                           likes + 
                           compartida
                           , data = facebook_log)


salida <- summary(ajuste_logistico)
train <- sample(1:nrow(facebook_log), nrow(facebook_log)*0.8)


ajuste_logistico2 <- polr(categoria ~
                            tipo+
                            pago +
                            personas_vieron+
                            # log(numero_impresiones) +
                            usuarios_comprometidos +
                            numero_cliks +
                            # log(usuarios_like_impresiones)+
                            # log(usuarios_like_vieron)+
                            # log(usuarios_like_comprometidos) +
                            comentarios +
                            likes +
                            compartida
                          , data = facebook_log[train,])

vcruz_log <- predict(ajuste_logistico2, newdata = facebook_log[-train,]) %>%
  table(., facebook_log$categoria[-train])
sum(diag(vcruz_log))/sum(vcruz_log)

ajuste_log_log <- polr(categoria ~., data = facebook_log_log)
vcruz_log <- predict(ajuste_log_log, newdata = facebook_log_log[-train,]) %>% 
  table(., facebook_log$categoria[-train]) 
sum(diag(vcruz_log))/sum(vcruz_log)


# Agrupando ---------------------------------------------------------------

hcl <- hclust(dist(facebook_continuas))
plot(hcl, cex = 0.6, hang = -1)

grupos <- kmeans(facebook_continuas, centers = 5)

hc2 <- agnes(facebook_continuas, method = "complete")
hc2$ac

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")


for(i in 1:length(m)){
  print(agnes(facebook_continuas, method = m[i])$ac)
}

hc3 <- agnes(facebook_continuas, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

hc4 <- hclust(dist(facebook_continuas), method = "ward.D2")
sub_grp <- cutree(hc4, k = 4)


plot(hc4, cex = 0.6)
rect.hclust(hc4, k = 4, border = 2:5)

plot(hc4, cex = 0.6)
rect.hclust(hc4, k = 6, border = 2:5)

agrupacion <- fviz_cluster(list(data = facebook_continuas, cluster = sub_grp))


res.dist <- dist(facebook_continuas, method = "euclidean")
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

fviz_nbclust(facebook_continuas, FUN = hcut, method = "wss")
fviz_nbclust(facebook_continuas, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(facebook_continuas, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

sub_grp <- cutree(hc4, k = 2)
facebook[, "grupo"] <- sub_grp


p <- ggplot(facebook, aes(x = factor(grupo), y = interacciones, fill = tipo))
p + geom_boxplot()

p <- ggplot(facebook, aes(x = factor(grupo), y = numero_cliks, fill = tipo))
p + geom_boxplot()


# Ajustando la regresion adecuada -----------------------------------------

# Lasso regression for facebook data

segunda$Paid <- as.factor(segunda$Paid)
names(segunda)
segunda <- na.omit(segunda)
x=model.matrix(Paid~.,segunda)[,-1]
y=segunda$Paid

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

grid =10^seq(10,-2, length =100)

lasso.mod =glmnet(x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)


cv.out =cv.glmnet(x[train ,],y[train],alpha =1)
bestlam = cv.out$lambda.min
lasso.pred=predict(lasso.mod ,s=bestlam ,newx=x[test ,])
mean((lasso.pred[,1] -y.test)^2)

out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef = predict(out, type = "coefficients", s = bestlam)


facebook  <-  na.omit(facebook)
fotos_comp <- subset(facebook, tipo == "Photo") %>% .$compartida 
link_comp <- subset(facebook, tipo == "Link") %>% .$compartida
fotos_like <- subset(facebook, tipo == "Photo") %>% .$like 
link_like <- subset(facebook, tipo == "Link") %>% .$like

t.test(facebook$compartida) 
t.test(fotos_comp, link_comp)
t.test(fotos_comp, link_comp, mu = 22, alternative = "less")
t.test(fotos_comp, fotos_like)


cor.test(link_like, link_comp)
cor.test(facebook$likes, facebook$compartida)
