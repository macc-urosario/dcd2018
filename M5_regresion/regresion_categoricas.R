
# Cargando librerias ------------------------------------------------------

library(carData)
library(dplyr)

# Explorando los datos ----------------------------------------------------

datos <- UN

datos1 <- datos %>% na.omit() %>% within(., group <- relevel(group, ref="africa"))
levels(datos1$group)

p <- ggplot(datos1, aes(x = region, y = lifeExpF, fill = region))
p + geom_boxplot()+
  labs(title = "Expectativa de vida",
       subtitle = "diferentes regiones",
       x = "Región",
       y = "Expectativa en años")

p <- ggplot(datos1, aes(x = group, y = lifeExpF, fill = group))
p + geom_boxplot()+
  labs(title = "Expectativa de vida",
       subtitle = "Por grupos",
       x = "Grupo",
       y = "Expectativa en años",
       fill = "Grupo")

tapply(datos1$lifeExpF, datos1$group, mean)

# Diferencias en la mortalidad infantil
p <- ggplot(datos1, aes(x = infantMortality, fill = group))
p + geom_density(alpha = 0.1, colour = "white") +
  geom_histogram(aes(y = ..density..), colour = "gray", alpha = 0.4)
  
tapply(datos1$infantMortality, datos1$group, mean)
tapply(datos1$infantMortality, datos1$group, sd)


# Modelos de regresión ----------------------------------------------------

modelo1 <- lm(lifeExpF ~ group , data = datos1)
summary(modelo1)

modelo2 <- lm(lifeExpF ~ infantMortality + group , data = datos1)
summary(modelo2)

modelo3 <- lm(lifeExpF ~ infantMortality + fertility + group , data = datos1)
summary(modelo3)

modelo4 <- lm(lifeExpF ~ infantMortality + group +  infantMortality*group, data = datos1)
summary(modelo4)

modelo5 <- lm(log(lifeExpF) ~ infantMortality + group, data = datos1)
summary(modelo5)



AIC(modelo3)
AIC(modelo2)
AIC(modelo1)

summary(modelo)

