library(tidyverse)
library(cluster)
library(factoextra)
library(janitor)
library(ggplot2)
library(corrplot)
install.packages("corrplot")
install.packages("class")
#cargar base de datos
beats

#ver cuantos casos NA hay 
colSums(is.na(beats))


#al ver que los NA corresponden a columnos insignificativas para el desarrollo del proyecto simplemente las eliminamos
beats <- beats[, -c(6, 28)]

summary(beats)


#filtrar solamente data numerica importante
beats <- beats[, c(6:17, 20, 22)]
beats <- beats[, c(2:14)]

#escalamos datos
esc_beats <- scale(beats) %>% as_tibble()
esc_beats %>% summary()

#correlacion
corrplot(cor(esc_beats))

#al encontrarme con el error de capacidad de espacio, eligo una muestra de los datos para realizar la tarea
beats_error <- esc_beats
beats_error <- beats_error[c(1:500),]


#ahora aplicamos kmeans
beats_kmeans <- kmeans(beats_error, centers = 10 )
beats_error$clus <- beats_error$cluster %>% as.factor()


ggplot(beats_error, aes(energy, danceability, color = clus)) +
  geom_point(alpha=0.5, show.legend = F) +
  theme_bw()
     

#probar para distintos k

ssinterior <- numeric(100)

for (k in 1:100) {
  modelo <- kmeans(esc_beats, centers = k)
  ssinterior[k] <- modelo$tot.withinss
  
}
plot(ssinterior)

#al usar regla del codo podemos observar que el punto de inflexion es 10
set.seed(123)
kmbeats <- kmeans(esc_beats,10,nstart = 20)

kmcl <- kmbeats$cluster
kmct <- data.frame(kmbeats$centers,clust = rownames(kmbeats$centers)) 

#evluacion 

esc_beats %>% head()


#al encontrarme con el error de capacidad de espacio, eligo una muestra de los datos para realizar la tarea
beats_error <- esc_beats
beats_error <- beats_error[c(1:500),]

#dist euclideana
d <- dist(beats_error)
hist(d)

mod_com <- hclust(d, method = "complete")
summary(mod_com)

install.packages("ggdendro")
library(ggdendro)

ggdendrogram(mod_com, rotate = TRUE, theme_dendro = TRUE)


#ahora cortamos el arbol en "5"

cort <- cutree(mod_com, h= 5)
coefsil <- silhouette(cort, d)
cort %>% unique() %>% length()

summary(coefsil)


