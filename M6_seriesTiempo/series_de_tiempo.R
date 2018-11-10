library(fTrading)
library(zoo)
library(forecast)
library(tseries)

x <- 1:30
t <- 2*x
s <- 12*sin(2*x/pi)

e <- 4*rnorm(30)

datos <- data.frame(fenomeno = y,
                    tendencia = t,
                    estacionalidad = s,
                    error = e,
                    tiempo = x)

p <- ggplot(datos, aes(x = tiempo, y = fenomeno))
p + geom_line(colour = "darkturquoise", lwd= 1) +
  labs(title = "Serie de Tiempo",
         y = "Fenómeno",
         x = "Tiempo")

p1 <- ggplot(datos, aes(x = tiempo, y = t))
p1 + geom_line(colour = "firebrick3", lwd= 1) +
  labs(title = "Componente de Tendencia",
       y = "Tendencia - T",
       x = "Tiempo")

p2 <- ggplot(datos, aes(x = tiempo, y = s))
p2 + geom_line(colour = "darkorchid2", lwd= 1) +
  labs(title = "Componente Estacional",
       ylab = "Estacionalidad - E",
       xlab = "Tiempo")


p3 <- ggplot(datos, aes(x = tiempo, y = e))
p3 + geom_line(colour = "dodgerblue1", lwd= 1) +
  labs(title = "Componente Error",
       ylab = "Error - e",
       xlab = "Tiempo")

# Calculo de una media movil ----------------------------------------------

x <- 1:20 %>% .^2 + 15*rnorm(20)
plot(x, type = "l")
y = rollmean(x, k = 3)
plot(y, type = "l")

# Primer Ejemplo ----------------------------------------------------------

gas = scan('http://verso.mat.uam.es/~joser.berrendero/datos/gas6677.dat')
plot(gas)

gas.ts = ts(gas, start = c(1966,1), frequency = 12)
plot(gas.ts)

boxplot(gas.ts ~ cycle(gas.ts))

cycle(gas.ts) #El comando cycle determina la unidad de tiempo a la que pertenece cada observación de la serie

gas.ts.desc = decompose(gas.ts)
plot(gas.ts.desc, xlab='Año')

# Posibles transformaciones
plot(log(gas.ts))
x = log(gas.ts)  # Reduccion de la varianza
dif1.x = diff(x) # Quitar la tendencia de una serie
plot(dif1.x) 
dif12.dif1.x = diff(dif1.x, lag=12) # Quitar la estacionalidad
plot(dif12.dif1.x)

# Segundo Ejemplo ---------------------------------------------------------


# Análisis de la Serie Ventas al Menudeo (Diebold, sección 4.5)

library(xtable)

D = read.table("Series de Tiempo/rsales.dat",header=T)
attach(D) # utiliza el nombre de las columnas como variables

# La variable RTRR del archivo tiene datos faltantes NA
y = na.omit(RTRR)

# Converir los datos en un objeto tipo ts
y = ts(y,frequency=12,start=c(1955,01))
fechas = seq(as.Date("1955/1/1"), length.out = length(y), by = "months")
ts.plot(y,main="Ventas al menudeo en UDS de 1999")

np = length(y)
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.ano = seq(fechas[1],fechas[np],"years")
plot(fechas,y, xaxt="n", panel.first = grid(),type="l",
     ylab="ventas.mes", lwd = 2)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.ano, labels = FALSE, tcl = 0.2)

layout(1:2)
plot(y,main = "Ventas al menudeo en UDS de 1999")
plot(log(y),main = "Log(Ventas)")

# Generar datos para los dos períodos

N = length(y)
yi = y[1:(N-12)]
yf = y[(N-12+1):N]

# Ajustar 5 modelos: lineal, cuadrático, cúbico, log-lineal,
# exponencial

t = seq(1:(N-12))
t2 = t^2
t3 = t^3
lyi = log(yi)

mod.lin = lm(yi~t)
mod.cuad = lm(yi~t+t2)
mod.cub = lm(yi~t+t2+t3)
mod.log.lin = lm(lyi~t)


summary(mod.lin)
summary(mod.cuad)
summary(mod.cub)
summary(mod.log.lin)

# El modelo exponencial es no lineal

Ds = data.frame(yi,t)
beta0 = mod.log.lin$coefficient[1]
beta1 = mod.log.lin$coefficient[2]
mod.exp = nls(yi~exp(beta0+beta1*t),data=Ds,
              start=list(beta0=beta0, beta1=beta1)) # Minimos cuadrados NO lineales

summary(mod.exp)

# Calcular AIC y BIC

AIC.tot = c(AIC(mod.lin), AIC(mod.cuad), AIC(mod.cub),
            AIC(mod.log.lin), AIC(mod.exp))
BIC.tot = c(BIC(mod.lin), BIC(mod.cuad), BIC(mod.cub),
            BIC(mod.log.lin), BIC(mod.exp))




summary(mod.cuad)

yest = mod.cuad$fitted.values

plot(t,yi,type='b')
lines(t,yest,type='l',col='blue')

# Nomalidad residuos

r2 = mod.cuad$residuals

layout(1:2)
qqnorm(r2)
qqline(r2,col="red")
hist(r2,40)
shapiro.test(r2)

# Pronósticos

tt=seq((N-12+1),N,1)
tt2 = tt*tt

pr2 = predict(mod.cuad,data.frame(t=tt,t2=tt2))

plot(tt,yf,type='b',xlab='t')
lines(tt,pr2,col='red')
acf(r2,ci.type="ma",60)

# Tercer Ejemplo ----------------------------------------------------------

licencias <- read.delim("Series de Tiempo/licencias.txt")
attach(licencias)
yw = loess(y ~ time(y))
np = length(y)
fecha = seq(as.Date("1986/01/01"), as.Date("2003/06/03"),
            by="months")
fechas = strptime(as.character(fecha), "%Y-%m-%d")
plot(fechas,y, xaxt="n",panel.first = grid(),type="l",ylab="")
axis.POSIXct(1, at=seq(as.Date(fechas[1]),as.Date(fechas[np]),
                       "months"), format="%m/%y")
axis.POSIXct(1, at=seq(as.Date(fechas[1]),as.Date(fechas[np]),
                       "years"), labels = FALSE, tcl = -0.2)

lines(fechas,yw$fitted, xaxt="n", panel.first = grid(),
      type="l",col="blue",lwd=2)


# Suavizamiento exponencial
x <- MSFT #Activos de fondos suizos, Precio y volumen diaro de microsoft y tazas de cambio
x <- x[, "Close"] # PRecios de cierre
y <-  emaTA(x, lambda = 0.189)
seriesPlot(x)
lines(y, col="red")


# Pronosticos de las series de tiempo -------------------------------------

serie <- co2
plot(serie)
fit <- stl(serie, s.window="period")
plot(fit)
monthplot(serie)
seasonplot(serie)

# Ajuste de los modelos de prediccion
# Ajustando sólamente la tendencia de la serie
m <- HoltWinters(serie, beta = FALSE, gamma = FALSE)
plot(m)
# Ajustando el nivel y la tendencia
m1 <- HoltWinters(serie, gamma = FALSE)
plot(m1)
# Ajustando el nivel, la tendencia y la estacionalidad
m2 <- HoltWinters(serie)
plot(m2)

# Predecir valores futuros
pred <- forecast(m, 12)
plot(pred)
accuracy(pred)


# Ejemplo 7 - Autorregresivos -----------------------------------------------

# Serie ldeaths - Registro de la muerte por diversas enfermedades

serie_train <- ldeaths[1:60]
serie_test <- ldeaths[61:72]

acf(serie_train)
pacf(serie_train)

ggtsdisplay(serie_train)

ajuste1 <- ar(serie_train, order.max = 1)
predicciones <- forecast(ajuste1, 12)
plot(predicciones)

ajuste2 <- ar(serie_train, order.max = 2)
predicciones <- forecast(ajuste2, 12)
plot(predicciones)

ajuste2 <- ar(serie_train, order.max = 3)
predicciones <- forecast(ajuste2, 12)
plot(predicciones)

ajuste3 <- ar(serie_train, order.max = 4)
predicciones <- forecast(ajuste3, 12)
plot(predicciones)

ajuste4 <- ar(serie_train)
predicciones <- forecast(ajuste4, 12)
plot(predicciones)
predichos <- as.numeric(predicciones$mean)
plot(1:12, serie_test)
lines(predichos)

# Modelos - Promedios móviles ---------------------------------------------

ajuste_ma1 <- arima(serie_train, order = c(0, 0, 1))
predicciones <- forecast(ajuste_ma1, 12)
plot(predicciones)

# Probando si una serie es estacionaria -----------------------------------

adf.test(serie_train)
ggtsdisplay(diff(serie_train))


# Ajustando un modelo autoregresivo de promedios móviles ------------------

Modelo1 <- arima(serie_train, order = c(3, 2, 1))
predicciones_arima <- forecast(Modelo1, 12)
plot(predicciones_arima)

Modelo2 <- auto.arima(serie_train)
summary(Modelo2)
predicciones2 <- forecast(Modelo2)
plot(predicciones2)
predichos2 <- as.numeric(predicciones2$mean)
plot(1:12, serie_test)
lines(predichos)
