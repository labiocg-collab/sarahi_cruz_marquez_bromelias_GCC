library(rgbif)
library(TeachingDemos)
library(dismo)
library(biomod2)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
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
library(kuenm)

##################################################################
rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo
##################################################################

##leer archivos de interes
##datos de la especie
EME <- readOGR("E:/sarahi_proyecto_bromelias/M_final/M_final/Guzmania_lingulata.shp")##indicar el directorio y el archivo de la M "final"
plot(EME)


##Capas ambientales del Presente
setwd("E:/sarahi_proyecto_bromelias/capas_climaticas_originales/presente_tiff2/")##directorio de las capas climáticas del presente 
bio14 <- raster("Bio14.tif")
bio15 <- raster("Bio15.tif")
def <- raster("def.tif")
eleva <- raster("Elev.tif")
et0 <- raster("et0.tif")
ptt <- raster("ptt.tif")
soil <- raster("soil.tif")
tmax <- raster("tmax.tif")
tmin <- raster("tmin.tif")
vpd <- raster("vpd.tif")

##Combinar las 10 variables en un stack
varclim_2000 <- stack(bio14, bio15, def, eleva, et0, ptt, soil, tmax, tmin, vpd)#"crear el stack"

###########################################################
#1. crear directorios de la carpeta y datos de la especie##
###########################################################
setwd("E:/sarahi_proyecto_bromelias/")
dir.create("kuenm_models_spp")###comando para crear una carpeta con el nombre indicado entre comillas

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/")
dir.create("Guzmania_lingulata")###comando para crear una carpeta con el nombre indicado entre comillas

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata")
dir.create("M_variables")###comando para crear una carpeta con el nombre indicado entre comillas

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/M_variables")
dir.create("Set_1")###comando para crear una carpeta con el nombre indicado entre comillas

##########################################################
#2.Recortar las capas climaticas a la forma de la M ######
##########################################################
#Recortar todas las capas climaticas del presente a la forma de la M... se recortan todas al mismo momento
CROPPED_EME <- crop(varclim_2000, extent(EME))##cortar las capas climaticas segun la extensión máxima de la M (en cuadrado)
plot(CROPPED_EME[[1]])###

MASKED_EME <- mask(CROPPED_EME, EME)##cortar las capas climaticas segun la forma exacta de la M
plot(MASKED_EME[[5]])#grafica la variable numero 5

#Funcion para mandar a guardar de forma automatica las capas recortadas:
lapply(names(MASKED_EME), function(x){
  writeRaster(MASKED_EME[[x]], paste0("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/M_variables/Set_1/", x,".asc"),overwrite=TRUE)})##directorio donde quiero que guarde las capas recortadas Capas "SET 1"

#################################################
##########Crear PCA para la variable Set2########
#################################################
rm(list =ls())
##leer donde esta la carpeta que tiene todas las variables del presente de la especie segun la forma de la M
setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/M_variables/Set_1/")
capas1 = list.files(".", pattern = "*.asc$", full.names = T)
capas_presente <- stack(capas1)

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/M_variables")
dir.create("Set_2")###comando para crear una carpeta con el nombre indicado entre comillas

rcpa1 <- kuenm_rpca(variables = capas_presente, var.scale = TRUE, write.result = TRUE, project = FALSE, 
                    proj.vars = capas_futuro, n.pcs = 5)###hacer el PCA

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/M_variables/PCA_results/Initial/")
capaspca = list.files(".", pattern = "*.tif$", full.names = T)
pca_presente <- stack(capaspca)

#Funcion para mandar a guardar de forma automatica las capas recortadas:
lapply(names(pca_presente), function(x){
  writeRaster(pca_presente[[x]], paste0("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/M_variables/Set_2/", x,".asc"),overwrite=TRUE)})##directorio donde quiero que guarde las capas recortadas Capas "SET 2"

#Eliminar la carpeta extra que ya no se necesita
setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/M_variables")#directorio de la carpeta
unlink ("PCA_results" , recursive = TRUE)#eliminar la carpeta de resultados del PCA


#########################################################
###################################################################################################
#Parte 1: Dividir en los datos en entrenamiento y validacion para hacer el protocolo de modelado###
###################################################################################################
#Llamar el archivo CSV final que tiene los datos definitivos de la especie
points <- read.csv2("E:/sarahi_proyecto_bromelias/Datos limpios/Datos limpios/Guzmania_lingulata_limpios.csv", sep = ",", header = T, row.names = 1)##indicar el directorio de los puntos finales limpios
points$lon <- as.numeric(points$lon)
points$lat <- as.numeric(points$lat)

write.csv(points[,1:3], "E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/Guzmania_lingulata_limpios.csv", row.names = FALSE)

data3 <- points
###dividir datos
todos <- unique(data3)###seleccionar el archivo base donde estan todos los datos
todos$sp <- paste(todos[,2], todos[,3])###seleccionar la columna lon y lat de los datos
train <- todos[sample(nrow(todos), round((length(todos[,1])/4 *3))), ]###indicar como se hará la seleccion de los datos (en este caso es seleccionar 4 de las 5 partes = 80%)
test_ind <- todos[!todos[,4] %in% train[,4], ]### coloca una nueva columna donde se indica la seleccion 

#guardamos nuestros primero subconjuntos de datos
#Datos de testing Independientes para evaluar el modelo final
write.csv(test_ind[,1:3], "E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/Guzmania_lingulata_ind.csv", row.names = FALSE)###nombre con el que guardará el archivo .csv que contiene el 20% de datos independientes de evaluacion

#Datos de training juntos (80%) para construir el modelo final
write.csv(train[,1:3], "E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/Guzmania_lingulata_joint.csv", row.names = FALSE)###nombre con el que guardará el archivo .csv que contiene el 80% de datos originales

#5. Dividir el archivo de los datos de trainning en 2 archivos (training [calibration]vs. testing [calibration]) para la calibracion del modelo
rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo
data1<- read.csv2("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/Guzmania_lingulata_joint.csv", sep=",", header=TRUE) ###seleccionar el archivo que ocntine los datos de presencia de la especie a modelar
data1$lat <- as.numeric(as.character(data1$lat))##convertir la informaci?n en n?mero
data1$lon <- as.numeric(as.character(data1$lon))##convertir la informaci?n en n?mero

todos <- unique(data1)###seleccionar el archivo base donde estan todos los datos
todos$sp <- paste(todos[,2], todos[,3])###seleccionar la columna lon y lat de los datos
train <- todos[sample(nrow(todos), round((length(todos[,1])/4 *3))), ]###indicar como se hará la seleccion de los datos (en este caso es seleccionar 4 de las 5 partes = 80%)
test_test <- todos[!todos[,4] %in% train[,4], ]### coloca una nueva columna donde se indica la seleccion realizada
#los registros han sido dividos en tres objetos

#guardamos nuestros subconjuntos
write.csv(train[,1:3], "E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/Guzmania_lingulata_train.csv", row.names = FALSE)###nombre con el que guardara el archivo .csv que contiene el 80% de datos para calibrar los modelos

write.csv(test_test[,1:3], "E:/sarahi_proyecto_bromelias/kuenm_models_spp/Guzmania_lingulata/Guzmania_lingulata_test.csv", row.names = FALSE)###nombre con el que guardara el archivo .csv que contiene el 20% de los datos para la calibraci?n del modelo


####FIN comenzar otra especie!