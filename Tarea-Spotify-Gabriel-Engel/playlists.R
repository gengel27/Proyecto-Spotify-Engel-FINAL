library(tidyverse)
library(cluster)
library(factoextra)
library(janitor)
library(ggplot2)
library(corrplot)
library(class)
library(dplyr)
library(ggdendro)
install.packages("rdata")

load("/Users/gabrielengel/Downloads/beats.RData")
play<- beats



#ver cuantos casos NA hay 
colSums(is.na(play))

#al ver que los NA corresponden a columnos insignificativas para el desarrollo del proyecto simplemente las eliminamos
play <- play[, -c(6, 28)]

#ahora se filtra columnas irrelevantes (se mantiene el track track_name para identificar la cancion)
play <- play[, c(1, 7:17, 22, 26)]

#por falta de memoria en el sistema usamos una muestra de 10000 canciones
play_muestra <- play[1:10000,]

#separamos el nombre, artista y duracion
id_tiempo <- play_muestra[, c(1, 13:14)]

#datos que usaremos para clustering
play_muestra_k <- play_muestra[, c(2:12)]

#escalamos datos
esc_play <- scale(play_muestra_k) %>% as_tibble()
esc_play %>% summary()


#ahora elegimos nuestra cancion de referencia, lo cual es la primera cancion de la base de datos
california_love <- esc_play[1,]

#ahora aplicamos kmeans, usando k=10
set.seed(123) #usamos set.seed para obtener el mismo resultado de cluster
esc_play_1 <- esc_play
kmeans_play_1 <- kmeans(esc_play_1, centers = 10 )
esc_play_1$clus <- kmeans_play_1$cluster %>% as.factor()

summary(esc_play_1$clus)

#como vemos, california love pertenece al cluster, o playlist, 2
play_con_nombre_1 <- cbind(id_tiempo,esc_play_1)
playlist_cali_1 <- filter(play_con_nombre_1, clus==2)

#veamos la duracion de la playlist
duracion<- sum( playlist_cali_1$duration_ms)
duracion
#como vemos la duracion es de 3000 minutos, mucho mas de tres horas

#probemos el clustering de kmeans con mas clusters para obtener una playlist mas corta
set.seed(123)
esc_play_2 <- esc_play
kmeans_play_2 <- kmeans(esc_play_2, centers = 100 )
esc_play_2$clus <- kmeans_play_2$cluster %>% as.factor()

summary(esc_play_2$clus)

#como vemos, california love pertenece al cluster, o playlist, 53
play_con_nombre_2 <- cbind(id_tiempo,esc_play_2)
playlist_cali_2 <- filter(play_con_nombre_2, clus==53)

#veamos la duracion de la playlist
duracion_2<- sum( playlist_cali_2$duration_ms)
duracion_2
#ya nos estamos acercando a las tres horas(180 minutos) ya que esta playlist dura 821 minutos

#volvemos a iterar
set.seed(123)
esc_play_3 <- esc_play
kmeans_play_3 <- kmeans(esc_play_3, centers = 420 )
esc_play_3$clus <- kmeans_play_3$cluster %>% as.factor()

summary(esc_play_3$clus)

#como vemos, california love pertenece al cluster, o playlist, 182
play_con_nombre_3 <- cbind(id_tiempo,esc_play_3)
playlist_cali_3 <- filter(play_con_nombre_3, clus==354)

#veamos la duracion de la playlist
duracion_3<- sum(playlist_cali_3$duration_ms)
duracion_3
#aca la playlist dura aproximadamente 3 horas



#ahora probamos usando las distnacias eucledianas
install.packages("fields")
library(fields)
rd<- rdist(california_love, esc_play)
rdm<- as.data.frame(rd)
rdm_t<- as.data.frame(t(rdm))

data_dist<- cbind(rdm_t,id_tiempo)

data_dist_hr<-data_dist$duration_ms <- as.numeric(as.character(data_dist$duration_ms)) / 3.6e+6
data_dist_hr<- as.data.frame(data_dist_hr)

data_dist_final <- cbind(data_dist, data_dist_hr)

#sumamos las horas hasta llegar a 3
data_dist_final1<- data_dist_final[order(data_dist_final$V1),]
data_dist_final1[,"hrs:sum_real"] <- cumsum(data_dist_final$data_dist_hr)

data_dist_final1<- data_dist_final1[,-c(6)]
#la playlist llega a las 3 horas en la cancion 43, entonces hacemos el corte ahi
playlist_cali_euc<- data_dist_final1[1:43,]
