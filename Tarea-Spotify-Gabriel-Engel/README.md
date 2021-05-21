Tarea spotify final
================

Primero cargamos los packages y luego cargamos la base de datos “beats”
y le ponemos el nombre “play”

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.0     ✓ dplyr   1.0.5
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(cluster)
library(factoextra)
```

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(ggplot2)
library(corrplot)
```

    ## corrplot 0.88 loaded

``` r
library(class)
library(dplyr)
library(ggdendro)
library(knitr)
load("/Users/gabrielengel/Downloads/beats.RData")
play<- beats
```

Para comenzar la limpieza de datos, lo primero que se debe hacer es
revisar si existen datos NA. Al ver que las columnas a las que
pertenecen los NA son insignificante para el analisis de datos las
podemos descartar. Además descartamos las otras columnas que no nos
interesan.

``` r
#ver cuantos casos NA hay 
colSums(is.na(play))
```

    ##                  artist_name                    artist_id 
    ##                            0                            0 
    ##                     album_id                   album_type 
    ##                            0                            0 
    ##           album_release_date           album_release_year 
    ##                            0                          447 
    ## album_release_date_precision                 danceability 
    ##                            0                            0 
    ##                       energy                          key 
    ##                            0                            0 
    ##                     loudness                         mode 
    ##                            0                            0 
    ##                  speechiness                 acousticness 
    ##                            0                            0 
    ##             instrumentalness                     liveness 
    ##                            0                            0 
    ##                      valence                        tempo 
    ##                            0                            0 
    ##                     track_id                 analysis_url 
    ##                            0                            0 
    ##               time_signature                  disc_number 
    ##                            0                            0 
    ##                  duration_ms                     explicit 
    ##                            0                            0 
    ##                   track_href                     is_local 
    ##                            0                            0 
    ##                   track_name            track_preview_url 
    ##                            0                       174714 
    ##                 track_number                         type 
    ##                            0                            0 
    ##                    track_uri        external_urls.spotify 
    ##                            0                            0 
    ##                   album_name                     key_name 
    ##                            0                            0 
    ##                    mode_name                     key_mode 
    ##                            0                            0

``` r
#al ver que los NA corresponden a columnas insignificativas para el desarrollo del proyecto simplemente las eliminamos
play <- play[, -c(6, 28)]

#ahora se filtra columnas irrelevantes (se mantiene el track track_name para identificar la cancion)
play <- play[, c(1, 7:17, 22, 26)]
```

Por un tema de falla en el sistema debido a una falta de espacio debido
a la cantidad de datos de la base de datos, se usa una muestra de
solamente 10.000 datos

``` r
#por falta de memoria en el sistema usamos una muestra de 10000 canciones
play_muestra <- play[1:10000,]
```

El primer metodo que se utiliza para crear la playlist es “kmeans”.
Separamos los datos que no queremos usar para el calculo(nombre,
artista, duración).

``` r
#separamos el nombre, artista y duracion
id_tiempo <- play_muestra[, c(1, 13:14)]
```

Ahora separamos los datos que si se usaran

``` r
#datos que usaremos para clustering
play_muestra_k <- play_muestra[, c(2:12)]
```

Escalamos los datos

``` r
#escalamos datos
esc_play <- scale(play_muestra_k) %>% as_tibble()
esc_play %>% summary()
```

    ##   danceability          energy             key             loudness      
    ##  Min.   :-2.53985   Min.   :-1.6867   Min.   :-1.5291   Min.   :-5.5825  
    ##  1st Qu.:-0.73459   1st Qu.:-0.9584   1st Qu.:-0.9572   1st Qu.:-0.7351  
    ##  Median : 0.06156   Median : 0.1055   Median : 0.1866   Median : 0.2972  
    ##  Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.72078   3rd Qu.: 0.9109   3rd Qu.: 1.0445   3rd Qu.: 0.8000  
    ##  Max.   : 2.32827   Max.   : 1.6235   Max.   : 1.6164   Max.   : 1.6772  
    ##       mode          speechiness       acousticness     instrumentalness 
    ##  Min.   :-1.4277   Min.   :-0.7627   Min.   :-1.1943   Min.   :-0.6380  
    ##  1st Qu.:-1.4277   1st Qu.:-0.4225   1st Qu.:-1.0447   1st Qu.:-0.6380  
    ##  Median : 0.7003   Median :-0.3459   Median :-0.1758   Median :-0.6359  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.7003   3rd Qu.:-0.1559   3rd Qu.: 1.1075   3rd Qu.: 0.5927  
    ##  Max.   : 0.7003   Max.   : 8.4083   Max.   : 1.4168   Max.   : 2.2542  
    ##     liveness          valence            tempo        
    ##  Min.   :-0.9884   Min.   :-1.4947   Min.   :-3.8669  
    ##  1st Qu.:-0.5792   1st Qu.:-0.8823   1st Qu.:-0.7793  
    ##  Median :-0.4402   Median :-0.1709   Median : 0.1130  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.1871   3rd Qu.: 0.8008   3rd Qu.: 0.6335  
    ##  Max.   : 3.3977   Max.   : 2.1063   Max.   : 3.4129

Ahora elegimos la cancion que usaremos como referencia, lo cual sera
“California Love” de 2PAC

``` r
california_love <- esc_play[1,]
```

Ahora aplicamos kmeans, usando 10 clusters. Luego agregamos la columna
que indica el cluster de cada canción

``` r
#ahora aplicamos kmeans, usando k=10
set.seed(123) #usamos set.seed para obtener el mismo resultado de cluster
esc_play_1 <- esc_play
kmeans_play_1 <- kmeans(esc_play_1, centers = 10 )
esc_play_1$clus <- kmeans_play_1$cluster %>% as.factor()
```

Revisando la matriz de canciones, se puede ver que California Love
pertenece al playlist, o cluster, 2. Entonces se genera esa matriz que
tenga solamente las canciones de ese cluster

``` r
play_con_nombre_1 <- cbind(id_tiempo,esc_play_1)
playlist_cali_1 <- filter(play_con_nombre_1, clus==2)
```

Vemos la duracion de la playlist

``` r
duracion<- sum( playlist_cali_1$duration_ms)
duracion
```

    ## [1] 219351537

Como se puede ver, dura mas de 3000 minutos, mucho mas de 3 horas.

Volvemos a iterar la funcion de k means, ahora con el valor K=100

``` r
set.seed(123)
esc_play_2 <- esc_play
kmeans_play_2 <- kmeans(esc_play_2, centers = 100 )
esc_play_2$clus <- kmeans_play_2$cluster %>% as.factor()

summary(esc_play_2$clus)
```

    ##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
    ##  98  87  76  97 114  81 151  86 135  76  84  33  89  74  61 113  86  69  46 105 
    ##  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 
    ##  93 120  96 115   4  81 111 310  97  69 228 230 201  51  34  62 101  35 114  36 
    ##  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60 
    ## 130  55  52 143  91 140  91 316 133  64 107 108 165  60  79  85 136  82  83 128 
    ##  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80 
    ##  55 115  90  25  54  90 123  58  69 101 131  94 112 112 150 129  66  37 112  85 
    ##  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99 100 
    ## 114  79 136  91 138 127 122  65  16  75 169 106 107 167  56  84  82 224  29   8

``` r
#como vemos, california love pertenece al cluster, o playlist, 53
play_con_nombre_2 <- cbind(id_tiempo,esc_play_2)
playlist_cali_2 <- filter(play_con_nombre_2, clus==53)

#veamos la duracion de la playlist
duracion_2<- sum( playlist_cali_2$duration_ms)
duracion_2
```

    ## [1] 39182065

``` r
#ya nos estamos acercando a las tres horas(180 minutos) ya que esta playlist dura 821 minutos
```

Iteramos una vez mas, ahora con k= 420

``` r
set.seed(123)
esc_play_3 <- esc_play
kmeans_play_3 <- kmeans(esc_play_3, centers = 420 )
esc_play_3$clus <- kmeans_play_3$cluster %>% as.factor()

summary(esc_play_3$clus)
```

    ##     283      95     399     149      27     370      12      55     105     385 
    ##      86      84      80      72      71      70      68      64      64      64 
    ##     136     200      39     365      19     223     295     323      24     249 
    ##      63      63      59      59      58      58      56      55      52      52 
    ##      22     293     395       5     286     408     359     252     420      97 
    ##      51      51      51      50      49      49      48      47      47      46 
    ##     120     335     379     210       9     173     316     214      26      36 
    ##      46      46      46      45      44      44      44      43      42      42 
    ##      62     279     206     243     152     236     264     285     392     412 
    ##      42      42      41      40      39      39      39      39      39      39 
    ##     163     307     313      48     195     271     308     327     342     350 
    ##      38      38      38      37      37      37      37      37      37      37 
    ##      17      79     148     296     301     314     326     178     284     341 
    ##      36      36      36      36      36      36      36      35      35      35 
    ##     378      69     143     211     328     103     135     137     228     406 
    ##      35      34      34      34      34      33      33      33      33      33 
    ##      99     111     158     176       3      15     100     108     126     199 
    ##      32      32      32      32      31      31      31      31      31      31 
    ##     354     363     380     384      37      45     132     247     261 (Other) 
    ##      31      31      31      31      30      30      30      30      30    5718

``` r
#como vemos, california love pertenece al cluster, o playlist, 182
play_con_nombre_3 <- cbind(id_tiempo,esc_play_3)
playlist_cali_3 <- filter(play_con_nombre_3, clus==354)

#veamos la duracion de la playlist
duracion_3<- sum(playlist_cali_3$duration_ms)
duracion_3
```

    ## [1] 6007109

``` r
#aca la playlist dura aproximadamente 3 horas
head(playlist_cali_3)
```

    ##   artist_name duration_ms
    ## 1  31 Minutos      165293
    ## 2  31 Minutos      109213
    ## 3        ABBA      226400
    ## 4        ABBA      173786
    ## 5        ABBA      200746
    ## 6        ABBA      213266
    ##                                                  track_name danceability
    ## 1 La Regla Primordial - Retrete Navarrete y los Bulliciosos    1.0706761
    ## 2                                   Rin Raja - Juan Tástico    1.3698627
    ## 3                              Lovelight - Original Version    0.2998904
    ## 4                                            Dum Dum Diddle    0.8374120
    ## 5                                 Why Did It Have To Be Me?    0.2846775
    ## 6                                                 Mamma Mia    1.2583016
    ##      energy        key  loudness      mode speechiness acousticness
    ## 1 1.4279262 -0.9572155 0.9803396 0.7003471  -0.2902734   -0.3579999
    ## 2 1.1594629 -1.5291313 0.6898112 0.7003471  -0.2298997   -0.5100504
    ## 3 0.7087097 -1.5291313 0.8417489 0.7003471  -0.4704362   -0.9549291
    ## 4 1.0235741 -0.3852997 0.7419573 0.7003471  -0.4551032   -0.5965619
    ## 5 1.2953518 -0.9572155 0.7987593 0.7003471  -0.3918545    0.1610688
    ## 6 0.7915688 -0.9572155 0.6752227 0.7003471  -0.4531866   -0.4209174
    ##   instrumentalness  liveness  valence       tempo clus
    ## 1       -0.6380136 1.4283424 1.248198  0.38089317  354
    ## 2       -0.6380136 0.1695071 1.801904 -0.14822358  354
    ## 3       -0.6375582 0.7616422 1.042851  0.28412901  354
    ## 4       -0.6379658 0.3537269 1.735899  0.35839450  354
    ## 5       -0.6219427 1.3581635 1.669895  0.02954239  354
    ## 6       -0.6367285 1.0686752 1.537885  0.69743406  354

Usando este metodo, las canciones en la playlist “playlist\_cali\_3” son
las mas adecuadas y que duran aproximadamente tres horas.

Para comparar resultados, usaremos otro metodo. Usaremos las distancias
euclidianas de California Love con todas las otras canciones. La idea de
este metodo sera ordenar las distancias de menor a mayor, y luego ir
formando una playlist hasta que su duracion llegue a tres horas.

Primero creamos un vector de todas las distancias de California Love

``` r
library(fields)
```

    ## Loading required package: spam

    ## Loading required package: dotCall64

    ## Loading required package: grid

    ## Spam version 2.6-0 (2020-12-14) is loaded.
    ## Type 'help( Spam)' or 'demo( spam)' for a short introduction 
    ## and overview of this package.
    ## Help for individual functions is also obtained by adding the
    ## suffix '.spam' to the function name, e.g. 'help( chol.spam)'.

    ## 
    ## Attaching package: 'spam'

    ## The following objects are masked from 'package:base':
    ## 
    ##     backsolve, forwardsolve

    ## Loading required package: viridis

    ## Loading required package: viridisLite

    ## See https://github.com/NCAR/Fields for
    ##  an extensive vignette, other supplements and source code

``` r
rd<- rdist(california_love, esc_play)
rdm<- as.data.frame(rd)
rdm_t<- as.data.frame(t(rdm))
```

Ahora juntamos la matriz para poder identificar la cancion

``` r
data_dist<- cbind(rdm_t,id_tiempo)
```

Convertimos los ms a horas y unimos esa columna

``` r
data_dist_hr<-data_dist$duration_ms <- as.numeric(as.character(data_dist$duration_ms)) / 3.6e+6
data_dist_hr<- as.data.frame(data_dist_hr)

data_dist_final <- cbind(data_dist, data_dist_hr)
```

Ahora reordenamos la columna de distancia de menor a mayor. Despues de
eso creamos una nueva columna que suma el tiempo acumulado en ese orden.
Al tener todo esto, nos fijamos en la cancion donde la playlist llegue a
las tres horas y hacemos el corte ahi.

``` r
#sumamos las horas hasta llegar a 3
data_dist_final1<- data_dist_final[order(data_dist_final$V1),]
data_dist_final1[,"hrs:sum_real"] <- cumsum(data_dist_final$data_dist_hr)

data_dist_final1<- data_dist_final1[,-c(6)]
#la playlist llega a las 3 horas en la cancion 43, entonces hacemos el corte ahi
playlist_cali_euc<- data_dist_final1[1:43,]
head(playlist_cali_euc)
```

    ##              V1       artist_name duration_ms                       track_name
    ## V1    0.0000000              2Pac  0.09665917                  California Love
    ## V3884 0.9816103    Alejandro Sanz  0.12069250 El Alma Al Aire - Extended Remix
    ## V176  1.4442856              2Pac  0.09280722                  California Love
    ## V3030 1.5700258 Alanis Morissette  0.07799250                         Bent 4 U
    ## V443  1.6051775              2Pac  0.09187778                       Can't C Me
    ## V5384 1.6174946           America  0.08785167   Save It for a Rainy Day - Live
    ##       data_dist_hr
    ## V1      0.09665917
    ## V3884   0.12069250
    ## V176    0.09280722
    ## V3030   0.07799250
    ## V443    0.09187778
    ## V5384   0.08785167

Al analizar ambas plyalists creados por los dos metodos, se puede llegar
a la conclusion de que la mas adecuada es la que usa distancias
eucledianas. El metodo kmeans no es tan preciso, ya que no crea una
playlist especialmente hecha para la cancion elegida, mientras que con
el metodo euclediano si se hace.
