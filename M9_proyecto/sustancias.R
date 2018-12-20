consumo <- read.csv("PREVALENCIA_CONSUMO_ULTIMO_A_O_GENERAL.csv", encoding = "UTF-8")
head(consumo)

rownames(consumo) <- consumo$nombre
consumo_uso <- consumo[, -c(1:4)]

# Agrupamiento Jerarquico -------------------------------------------------

hcl <- hclust(dist(consumo_uso))
plot(hcl, cex = 0.6, hang = -1)

hc2 <- agnes(consumo_uso, method = "complete")
hc2$ac

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")


for(i in 1:length(m)){
  print(agnes(consumo_uso, method = m[i])$ac)
}

hc3 <- agnes(consumo_uso, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

hc4 <- hclust(dist(consumo_uso), method = "ward.D2")
sub_grp <- cutree(hc4, k = 4)


plot(hc4, cex = 0.6)
rect.hclust(hc4, k = 4, border = 2:5)

plot(hc4, cex = 0.6)
rect.hclust(hc4, k = 6, border = 2:5)

agrupacion <- fviz_cluster(list(data = consumo_uso, cluster = sub_grp))


res.dist <- dist(consumo_uso, method = "euclidean")
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

fviz_nbclust(consumo_uso, FUN = hcut, method = "wss")
fviz_nbclust(consumo_uso, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(consumo_uso, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

sub_grp <- cutree(hc4, k = 3)
consumo_uso[, "grupo"] <- factor(sub_grp)

grupos <- kmeans(consumo_uso, centers = 3)


acp_ciudades <- PCA(consumo_uso)
acp_ciudades$eig
base_ind <- acp_ciudades$ind$coord %>% data.frame()

names(base_ind)
base_ind[, "grupo"] <- grupos$cluster

base_graf <- melt(consumo_uso, by = grupo)
p <- ggplot(base_graf, aes(x = variable, y = value, fill = grupo))
p + geom_boxplot() + coord_flip()



# Base Hurtos -------------------------------------------------------------

hurtos <- read.csv("Hurto_a_personas_2017.csv", encoding = "UTF-8")
head(hurtos)

cambia <- function(x, y){ifelse(x == "BOGOTÁ D.C. (CT)", "bogota d.c.", y)}

hurtos[, "Departamento"] <- cambia(as.character(hurtos$Municipio), as.character(hurtos$Departamento))

numero_hurtos <- tapply(hurtos$Municipio,hurtos$Departamento, length ) %>% data.frame()
names(numero_hurtos) <- "hurtos"
numero_hurtos[, "departamento"] <- rownames(numero_hurtos)
consumo_uso[, "departamento"] <- rownames(consumo_uso)
consumo_uso[, "grupos"] <- factor(grupos$cluster)
inner_join(consumo_uso, numero_hurtos, by = "departamento") #Solo cruzan 16

numero_hurtos[, "departamento"] <- numero_hurtos[, "departamento"] %>% tolower() %>% iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')
consumo_uso[, "departamento"] <- consumo_uso[, "departamento"] %>% tolower() %>% iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT')

inner_join(consumo_uso, numero_hurtos, by = "departamento") #Solo cruzan 19
k <- which(consumo_uso$departamento %in% numero_hurtos$departamento == FALSE)
consumo_uso$departamento[k] <- c("guajira", "amazonas", "norte de santander", 
                                 "valle", "antioquia", "san andres", "valle", "antioquia")


base_dep <- inner_join(consumo_uso, numero_hurtos, by = "departamento") #Solo cruzan 16



# Ajustando un modelo de regresion ----------------------------------------


# Método 1 - Incluir de a una variable
scatterplotMatrix(base_dep[-8])
base_dep[, "log_hurtos"] <- log(base_dep$hurtos +1)
modelo <- lm(log_hurtos~. -departamento - hurtos, base_dep)
summary(modelo)

regfit.full=regsubsets(hurtos~.
                       -departamento, base_dep)
summary(regfit.full)

regfit.full=lm(log_hurtos ~ tabaco +  alcohol +
                          cocaina + grupos, base_dep[-8])

plot(base_dep[-8])

p <- ggplot(base_dep, aes(x = grupos, y = hurtos, fill = grupos))
p + geom_boxplot()

p <- ggplot(base_dep, aes(x = grupos, y = log_hurtos, fill = grupos))
p + geom_boxplot()

# Ajustar mas -------------------------------------------------------------

reg.summary = summary(regfit.full)
reg.summary
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

