library(ggplot2)
library(tree)
library(randomForest)
library(VIM)
library(FactoMineR)
library(factoextra)


setwd("Proyecto Ciencia de datos/")
cancer <- read.delim("cancer.csv", sep = ",", 
                     colClasses = c("numeric",
                                    "numeric",
                                    "numeric",
                                    "numeric",
                                    "factor",
                                    "numeric",
                                    "numeric",
                                    "factor",
                                    "numeric",
                                    "factor",
                                    "numeric",
                                    "factor",
                                    "numeric",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "numeric",
                                    "numeric",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor",
                                    "factor"))
head(cancer)


# Datos faltantes ---------------------------------------------------------

sum(is.na(cancer)) # Total de registros faltantes en la base de datos
faltantes_variable <- is.na(cancer) %>% apply(., 2, sum) %>% sort() %>% data.frame()
faltantes_variable[, "Porcentaje"] <- round(faltantes_variable$./nrow(cancer)*100, 2)
# Dos de las variables tienen mas de un 90% de los datos faltantes
# Se quitan estas variables, además, se puede ver que el porcentaje de respuestas faltantes
# de las ets son tambien altas.

cancer_pr <- cancer[,!(names(cancer) %in% c("tiempo_desde_ultimo_diagnostico", "tiempo_desde_primer_diagnostico"))]
# Caracterizando los faltantes
aggr(cancer_pr)

cancer_pr <- na.omit(cancer_pr)
dim(cancer_pr)


# Reto # 2  ---------------------------------------------------------------

grafico <- boxplot(cancer$parejas_sexuales)
grafico$stats
grafico$out

atipicos <- function(x){
  if(is.factor(x)){
    return(table(x))
  }else{
    lim_inf <- quantile(x, 0.25)-1.5*(quantile(x, 0.75)- quantile(x, 0.25))
    lim_sup <- quantile(x, 0.75)+1.5*(quantile(x, 0.75)- quantile(x, 0.25))
    atip <- which(x>lim_sup | x< lim_inf) %>% x[.]
    return(c(summary(atip), n = length(atip)))
  }
}

# Caracterizacion de los atípicos por variable - Si la variable es factor, retorna cuantos hay de cada categoría
lapply(cancer_pr, atipicos)


t.test(edad~ diag_cancer, data = cancer_pr) # Si hay diferencias estadísticamente significativas en las edades de las mujeres con cancer frente a las que no

chisq.test(cancer_pr$fuma, cancer_pr$diag_cancer) # Al parecer no hay diferencias estadísticamente significativas

p <- ggplot(cancer, aes(x = as.factor(diag_cancer), y = paquetes_anio, fill = as.factor(fuma)))
p + geom_boxplot()


# Modelando el cancer -----------------------------------------------------

# Un modelo con todo el conjutno de datos
arbol_cancer <- tree(diag_cancer ~ .-diag, data = cancer_pr)
summary(arbol_cancer)
plot(arbol_cancer)
text(arbol_cancer, pretty = 0)

#Dividiendo en conjunto de entrenamiento y prueba

smp_train <- sample(1:nrow(cancer_pr), nrow(cancer_pr)*0.8)
cancer_train <- cancer_pr[smp_train, ]
cancer_test <- cancer_pr[-smp_train, ]

arbol_cancer <- tree(diag_cancer ~ .-diag, data = cancer_train)
summary(arbol_cancer)
plot(arbol_cancer)
text(arbol_cancer, pretty = 0)

predicciones <- predict(arbol_cancer, newdata = cancer_test)
predicciones <- ifelse(predicciones[,1] > 0.8, 0, 1)
validacion <- table(predicciones, cancer_test$diag_cancer)
error <- validacion/sum(validacion)

# Cuarto Reto -------------------------------------------------------------
# Seleccionando solo las variables de interés
variables <- c(26:34)
cuantis <- c(1:4, 6, 7, 9, 11, 13)
cancer_acm <- cancer_pr[, - variables]
names(cancer_acm)[cuantis]
# Creando las componentes para las correspondencias
analisis <- MCA(cancer_acm, quanti.sup = cuantis)
analisis$eig
analisis$var
