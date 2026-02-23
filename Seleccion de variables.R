library(rgbif)
library(TeachingDemos)
library(dismo)
library(biomod2)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(usdm)
library(ENMeval)
library(foreign)
library(spocc)
library(corrplot)
library(usdm)
library(XML)
library(ecospat)
library(dplyr)
library(reshape)
library(car)
library(terra)
library(rgdal)
library(car)

install.packages("rgdal", repos="http://R-Forge.R-project.org")


#############################################################################################
###########SELECCION DE VARIABLES PARA LOS MODELOS ##########################################
#############################################################################################
rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo

setwd("D:/1.Limpieza de datos/Datos limpios")###directorio donde tengo mis archivos de puntos limpios y listos
#data<- read.dbf("amazona_xantholora2.dbf")## leer el archivo de puntos de inter?s en caso de qgis
data<- read.csv2("Aechmea_bromeliifolia_limpios.csv", sep = ",", header = TRUE)## leer el archivo de puntos de inter?s

species<-data$species ## seleccionar la columna del nombre de la especie
lat<-as.numeric(data$lat)## seleccionar la columna del nombre de la latitud
lon<-as.numeric(data$lon)## seleccionar la columna del nombre de la longitud

datos<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables!


#2.Verificar informacion ambiental en mis localidades de ocurrencia
#localizacion de la carpeta donde se encuentra colocadas las variables ambientales de mi zona de estudio
setwd("D:/0.Capas climáticas/Presente")###directorio donde estan las capas climaticas del presente
pca_path <- list.files(".",pattern = "*.asc$",full.names = T)###crea el stack de las 19 variables climaticas del presente
capas_presente<- stack(pca_path)


##extraemos los valores ambientales para esas localidad y omitimos los datos sin informacion
presencias_clima <- data.frame(raster::extract(capas_presente,datos[,2:3]))###extrae los valores climaticos para cada uno de los puntos de presencia
presencias_clima2<-data.frame(datos,presencias_clima)
presencias_clima3 <- na.omit(presencias_clima2)## omite mis datos de presencia sin valores climaticos.


##Guardamos esos datos (con estos datos se va a construir los modelos)
setwd("D:/2.Seleccion de variables")
write.csv(presencias_clima3[,1:3], file = "Aechmea_bromeliifolia_model.csv", row.names = FALSE) ##carpeta donde guardar el archivo .csv


#############################################################################################
###########SELECCION DE VARIABLES PARA LOS MODELOS ##########################################
#############################################################################################
### Matriz de colinearidad y estimaciOn del Factor de inflaciOn de Varianza (VIF)
### Matriz de colinearidad y estimaciOn del Factor de inflaciOn de Varianza (VIF)
cormatriz <- cor(presencias_clima3[,4:15])#Definir cu?les son las columnas con las variables para hacer la correlaci?n (de la 1 a la 19) las columnas de la 1 a la 3 son especie, lon, lat... por eso se hace de la 4 a la 22

setwd("D:/2.Seleccion de variables/Correlaciones")##carpeta donde guardar el archivo jpg de las correlaciones
windows() ###abre una ventana gr?fica
#quartz() para Mac
library(corrplot)
corrplot(cormatriz, outline = T, tl.col = "black", mar = c(2,0,1,1.5), title = "Aechmea_bromeliifolia")###gr?fica las correlaciones entre variables. OJO no guarda el archivo, hay que darle en la ventana y mandar a guardarlo

no_corr <- vifstep(presencias_clima2[,4:15], th=5) # th: es el valor de umbral deseado;Para ver cuales fueron las variables que quedaron sin colinearidad. OJO: el valor "10" se puede modificar...

no_corr ### aqu? R dar? como resultado cuales son las variables "no correlacionadas" entre s?. Dar? una lista final con los nombres de esas variables, ya es solo cuesti?n de anotarlas y entonces con base a ellas hacer el modelo.

