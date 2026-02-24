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
EME <- readOGR("E:/sarahi_proyecto_bromelias/M_final/M_final/Tillandsia_schiedeana.shp")##indicar el directorio y el archivo de la M "final"
plot(EME)


##Capas ambientales del presente
setwd("E:/sarahi_proyecto_bromelias/capas_climaticas_originales/presente_corrigida_final_tiff/")##directorio de las capas climáticas del presente 
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

#Recortar todas las capas climaticas del presente a la forma de la M... se recortan todas al mismo momento
#Presente
CROPPED_EME <- crop(varclim_2000, extent(EME))##cortar las capas climaticas segun la extensión máxima de la M (en cuadrado)
#plot(CROPPED_EME[[1]])###
MASKED_EME <- mask(CROPPED_EME, EME)##cortar las capas climaticas segun la forma exacta de la M
plot(MASKED_EME[[1]])#grafica la variable numero 5

#Eliminar la carpeta extra que ya no se necesita
setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_schiedeana/M_variables/")#directorio de la carpeta
unlink ("Set_1" , recursive = TRUE)#eliminar la carpeta de resultados del PCA
dir.create("Set_1")###comando para crear una carpeta con el nombre indicado entre comillas

#Funcion para mandar a guardar de forma automatica las capas recortadas:
lapply(names(MASKED_EME), function(x){
  writeRaster(MASKED_EME[[x]], paste0("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_schiedeana/M_variables/Set_1/", x,".asc"),overwrite=TRUE)})##directorio donde quiero que guarde las capas recortadas Capas "SET 1"


##Capas ambientales del futuro 1
setwd("E:/sarahi_proyecto_bromelias/capas_climaticas_originales/F_10var_N_D_T_2C_tiff/")##directorio de las capas 
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
varclim_futuro2c <- stack(bio14, bio15, def, eleva, et0, ptt, soil, tmax, tmin, vpd)#"crear el stack"


##Capas ambientales del futuro 2
setwd("E:/sarahi_proyecto_bromelias/capas_climaticas_originales/F_10var_N_D_T_4C_tiff/")##directorio de las capas 
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
varclim_futuro4c <- stack(bio14, bio15, def, eleva, et0, ptt, soil, tmax, tmin, vpd)#"crear el stack"


###########################################################
#1. crear directorios de la carpeta y datos de la especie##
###########################################################
setwd("E:/sarahi_proyecto_bromelias/")
dir.create("kuenm_models_spp")###comando para crear una carpeta con el nombre indicado entre comillas

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/")
dir.create("Tillandsia_schiedeana")###comando para crear una carpeta con el nombre indicado entre comillas

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_schiedeana")
dir.create("G_variables")###comando para crear una carpeta con el nombre indicado entre comillas

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_schiedeana/G_variables")
dir.create("Set_1")###comando para crear una carpeta con el nombre indicado entre comillas

setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_schiedeana/G_variables/Set_1")
dir.create("2C")###comando para crear una carpeta con el nombre indicado entre comillas
dir.create("4C")###comando para crear una carpeta con el nombre indicado entre comillas

############################################
##########Crear las variable Set1 2C########
############################################
#Recortar todas las capas climaticas del presente a la forma de la M... se recortan todas al mismo momento
CROPPED_EME_2C <- crop(varclim_futuro2c, extent(EME))##cortar las capas climaticas segun la extensión máxima de la M (en cuadrado)
#plot(CROPPED_EME_2C[[1]])###
MASKED_EME_2C <- mask(CROPPED_EME_2C, EME)##cortar las capas climaticas segun la forma exacta de la M
plot(MASKED_EME_2C[[1]])#grafica la variable numero 5

#Funcion para mandar a guardar de forma automatica las capas recortadas:
lapply(names(MASKED_EME_2C), function(x){
  writeRaster(MASKED_EME_2C[[x]], paste0("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_schiedeana/G_variables/Set_1/2C/", x,".asc"),overwrite=TRUE)})##directorio donde quiero que guarde las capas recortadas Capas "SET 1"

############################################
##########Crear las variable Set1 4C########
############################################
#Recortar todas las capas climaticas del presente a la forma de la M... se recortan todas al mismo momento
CROPPED_EME_4C <- crop(varclim_futuro4c, extent(EME))##cortar las capas climaticas segun la extensión máxima de la M (en cuadrado)
#plot(CROPPED_EME_4C[[1]])###

MASKED_EME_4C <- mask(CROPPED_EME_4C, EME)##cortar las capas climaticas segun la forma exacta de la M
plot(MASKED_EME_4C[[1]])#grafica la variable numero 5

#Funcion para mandar a guardar de forma automatica las capas recortadas:
lapply(names(MASKED_EME_4C), function(x){
  writeRaster(MASKED_EME_4C[[x]], paste0("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_schiedeana/G_variables/Set_1/4C/", x,".asc"),overwrite=TRUE)})##directorio donde quiero que guarde las capas recortadas Capas "SET 1"


####FIN comenzar otra especie!
