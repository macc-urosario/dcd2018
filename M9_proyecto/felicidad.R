felicidad_2017 <- read.delim("felicidad.csv", sep = ",")
felicidad_2015 <- read.delim("felicidad_2015.csv", sep = ",")
head(felicidad_2017)
head(felicidad_2015)

nrow(felicidad_2017)
nrow(felicidad_2015)


cruces <- felicidad_2017$Country %in% felicidad_2015$Country
felicidad_2017$Country[!cruces]

cruces2 <- felicidad_2015$Country %in% felicidad_2017$Country
felicidad_2015$Country[!cruces2]


felicidad_completo <- inner_join(felicidad_2015, felicidad_2017, by = "Country")
dim(felicidad_completo)

cor.test(felicidad_completo$Happiness.Rank.x, felicidad_completo$Happiness.Rank.y)

head(felicidad_completo)

scatterplotMatrix(felicidad_completo[,-c(1:2)])
