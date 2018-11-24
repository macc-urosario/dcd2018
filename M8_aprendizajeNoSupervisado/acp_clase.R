library(FactoClass)
library(GGally)
library(aplpack)
library(dplyr)
library(FactoMineR)
library(factoextra)

data("cafe")
cafe

plot(cafe, pch = 20) # Diagramas de dispersión

ggpairs(cafe, lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")

faces(cafe)
# Otro ejemplo
faces(longley)


# Normalizacion de los datos

est <- function(x){(x-mean(x))/sd(x)}
cafe_est <- apply(cafe, 2, est) 
cafe_rs <- melt(cafe_est)
boxplot(cafe_rs)


# Analisis de componentes principales -------------------------------------

acp <- PCA(cafe2[,-1])
summary(acp)
plot(acp)
acp$eig
acp$var
acp$ind
acp$svd
acp$call

# Otro ejemplo
data(decathlon2)
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])
res.pca <- prcomp(decathlon2.active, scale = TRUE)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


# Usando ACP para hacer predicciones

ind.sup <- decathlon2[24:27, 1:10]
ind.sup[, 1:6]
ind.sup.coord <- predict(res.pca, newdata = ind.sup)
ind.sup.coord[, 1:4]

# Grafico de los individuos activos
p <- fviz_pca_ind(res.pca, repel = TRUE)
# Add supplementary individuals
fviz_add(p, ind.sup.coord, color ="blue")


# Inclusion de variables suplementarias - Cualitativas

groups <- as.factor(decathlon2$Competition[1:23])
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

quanti.sup <- decathlon2[1:23, 11:12, drop = FALSE]
head(quanti.sup)


quanti.coord <- cor(quanti.sup, res.pca$x)
quanti.cos2 <- quanti.coord^2
# Graph of variables including supplementary variables
p <- fviz_pca_var(res.pca)
fviz_add(p, quanti.coord, color ="blue", geom="arrow")

# Análisis de correspondencias --------------------------------------------
data(Bogota) # Manzanas de Bogotá
sum(Bogota)
res.ca <- CA(Bogota)
get_ca_row(res.ca)
fviz_contrib(res.ca, choice = "row", axes = 1)
fviz_contrib(res.ca, choice = "col", axes = 1)
fviz_ca_row(res.ca, repel = TRUE)
fviz_ca_col(res.ca)
fviz_ca_biplot(res.ca, repel = TRUE)


# Análisis de correspondencias múltiples

data(DogBreeds)
res.mca <- MCA(DogBreeds)
fviz_contrib(res.mca, choice ="var", axes = 1)
fviz_contrib(res.mca, choice ="ind", axes = 1, top = 20)
grp <- as.factor(DogBreeds[, "WEIG"])
fviz_mca_ind(res.mca, col.ind = "blue", habillage = grp,
             addEllipses = TRUE, repel = TRUE) + theme_minimal()
fviz_mca_var(res.mca, repel = TRUE)

fviz_mca_biplot(res.mca, 
                select.ind = list(contrib = 30), 
                select.var = list(contrib = 10))
