library(aod)
require(ISLR)
library(InformationValue)
library(vcd)

datos <- Default
levels(datos$default) <- c("0","1")

table(datos$default)
input_ones <- datos[which(datos$default == 1), ]  
input_zeros <- datos[which(datos$default == 0), ]  

input_ones_training_rows <- sample(1:nrow(input_ones), 0.8*nrow(input_ones))  # 1's for tra
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.8*nrow(input_ones))

# Creando los datos de entrenamiento
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)

# Creando los datos de test
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)



# Creando el modelo -------------------------------------------------------

modelo <- glm(default ~ student + balance + income, data = trainingData, family = binomial(link = "logit"))
summary(modelo)

# Predicciones - Dos formas de hacerlo

predicciones <- plogis(predict(modelo, testData))
predicciones <- predict(modelo, testData, type="response")


optCutOff <- optimalCutoff(testData$default, predicted)[1] 


# Diagnostico del modelo

summary(modelo)

misClassError(testData$default, predicciones, threshold = optCutOff)
plotROC(testData$default, predicciones)
Concordance(testData$default, predicciones)

pred_fact <- ifelse(test = predicciones > optCutOff,
                       yes = 1, no = 0) %>%  as.factor()

tabla_contingencia <- table(pred_fact, testData$default)
matriz_confusion <- prop.table(tabla_contingencia)
sum(diag(matriz_confusion))


mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))
