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
rm(list=ls())
###leer el archivo original donde est??n los datos limpios finales
datos_limpios_buffer <- read.csv2("D:/1.Limpieza de datos/Datos limpios/Aechmea_bracteata_limpios.csv", sep = ",", header = TRUE, row.name =1) 
datos_limpios_buffer$lat <- as.numeric(datos_limpios_buffer$lat)
datos_limpios_buffer$lon <- as.numeric(datos_limpios_buffer$lon)
datos_limpios_buffer$species_name <- as.character(datos_limpios_buffer$species_name)

##selecionar los datos finales con los que hacer los calculos2
species <- datos_limpios_buffer$species_name
lat<-as.numeric(datos_limpios_buffer$lat)
lon<-as.numeric(datos_limpios_buffer$lon)
datos<-data.frame(species,lon,lat) 

##convertir el archivo en un archivo espacial para puntos s
points_occ2020 <- SpatialPointsDataFrame(datos[,2:3],datos)##co
##calcular los valores de distancia entre puntos
DistanciaPuntos<-gDistance(points_occ2020, byid=TRUE)##calcula la distancia promedio de los puntos de 1970 al 2000
##calcular los valores de distancia buffer promedio (en grados decimales)
buffer <- (quantile(DistanciaPuntos, probs = 0.05, na.rm = TRUE))/25

##calcular el valor pero en km2
distancia_km = buffer/0.008333

##Gracias! ??