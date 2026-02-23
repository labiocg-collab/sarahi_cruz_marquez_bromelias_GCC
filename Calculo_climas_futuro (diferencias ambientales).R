library(rgbif)
library(TeachingDemos)
library(dismo)
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
library(dplyr)
library(reshape)
library(beepr)
library(kuenm)

rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo para evitar confusiones en archivos

##cargar capas necesarias###
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

##Capas ambientales del futuro 1
setwd("E:/sarahi_proyecto_bromelias/capas_climaticas_originales/F_10var_N_D_T_2C_tiff/")##directorio de las capas 
bio14_1 <- raster("Bio14.tif")
bio15_1 <- raster("Bio15.tif")
def_1 <- raster("def.tif")
eleva_1 <- raster("Elev.tif")
et0_1 <- raster("et0.tif")
ptt_1 <- raster("ptt.tif")
soil_1 <- raster("soil.tif")
tmax_1 <- raster("tmax.tif")
tmin_1 <- raster("tmin.tif")
vpd_1 <- raster("vpd.tif")


##Capas ambientales del futuro 2
setwd("E:/sarahi_proyecto_bromelias/capas_climaticas_originales/F_10var_N_D_T_4C_tiff/")##directorio de las capas 
bio14_2 <- raster("Bio14.tif")
bio15_2 <- raster("Bio15.tif")
def_2 <- raster("def.tif")
eleva_2 <- raster("Elev.tif")
et0_2 <- raster("et0.tif")
ptt_2 <- raster("ptt.tif")
soil_2 <- raster("soil.tif")
tmax_2 <- raster("tmax.tif")
tmin_2 <- raster("tmin.tif")
vpd_2 <- raster("vpd.tif")

################################################################
####Calcular las diferencias de las condiciones climaticas######
################################################################
##futuro 2C
dif_bio14_2C <- bio14_1 - bio14
dif_bio15_2C <- bio15_1 - bio15
dif_def_2C <- def_1 - def
dif_eleva_2C <- eleva_1 - eleva
dif_et0_2C <- et0_1 - et0
dif_ptt_2C <- ptt_1 - ptt
dif_soil_2C <- soil_1 - soil
dif_tmax_2C <- tmax_1 - tmax
dif_tmin_2C <- tmin_1 - tmin
dif_vpd_2C <- vpd_1 - vpd
##Combinar las 10 variables de los valores de diferencia futuro1 vs presente en un stack
varclim_futuro2c <- stack(dif_bio14_2C, dif_bio15_2C, dif_def_2C, dif_eleva_2C, dif_et0_2C, dif_ptt_2C, dif_soil_2C, dif_tmax_2C, dif_tmin_2C, dif_vpd_2C)#"crear el stack"



##Futuro 4C
dif_bio14_4C <- bio14_2 - bio14
dif_bio15_4C <- bio15_2 - bio15
dif_def_4C <- def_2 - def
dif_eleva_4C <- eleva_2 - eleva
dif_et0_4C <- et0_2 - et0
dif_ptt_4C <- ptt_2 - ptt
dif_soil_4C <- soil_2 - soil
dif_tmax_4C <- tmax_2 - tmax
dif_tmin_4C <- tmin_2 - tmin
dif_vpd_4C <- vpd_2 - vpd
##Combinar las 10 variables de los valores de diferencia futuro1 vs presente en un stack
varclim_futuro4c <- stack(dif_bio14_4C, dif_bio15_4C, dif_def_4C, dif_eleva_4C, dif_et0_4C, dif_ptt_4C, dif_soil_4C, dif_tmax_4C, dif_tmin_4C, dif_vpd_4C)#"crear el stack"


#######################################################
###llamar a los mapas de los tiempos analizados######
#######################################################
presente <- raster("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/presente/Aechmea_bracteata.tif")#indicar donde esta el mapa del presente

futuro_2Cdisp <- raster("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/dispersion_2C/Aechmea_bracteata.tif")#indicar donde esta el mapa del futuro 2C dispersion

futuro_4Cdisp <- raster("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/dispersion_4C/Aechmea_bracteata.tif")#indicar donde esta el mapa del futuro 4C dispersion

##OJO: el calculo solo se hace con los mapas de DISPERSION porque no dispersion ya esta dentro de Dispersion (es un subconjunto)

##no mover nada de esto
r <-function(x){
  x[x<=0.9]<-0; x[x>=1]<-5  
  return(x)}

present_bin<-calc(presente, fun= r )##convierte las zonas "1 = presencia" a zonas de valor "5 = presencia" esto con el objetivo de poder diferenciar la presencia del presente vs la del futuro
raster::freq(present_bin)##solo para confirmar el valor "5" tiene el mismo numero de km2 que ya debe aparecer en el excell en la zona del presente

#######################################################################
diferencia_2c <- present_bin + futuro_2Cdisp##suma el mapa del presente con el futuro correspondiente. Esto me va a dar un mapa nuevo que tiene valores: NA, CERO (ausencia), 1 (presencia en el futuro), 5 (presencia en el presente) y 6 (presencia tanto en futuro como en presente)

diferencia_4c <- present_bin + futuro_4Cdisp##suma el mapa del presente con el futuro correspondiente. Esto me va a dar un mapa nuevo que tiene valores: NA, CERO (ausencia), 1 (presencia en el futuro), 5 (presencia en el presente) y 6 (presencia tanto en futuro como en presente)
################################################################################################

################################################################################################
#4.convertir el mapa en puntos y seleccionar solo el ?rea que nos interesa (perdida = valor 5)##
################################################################################################
##2C
point_presencia2C <- data.frame(rasterToPoints(diferencia_2c))##convertir el mapa del presente en puntos
point_areas_loss2C <- subset(point_presencia2C, layer == 5)###selecciona solo los puntos que son "presencia" solo en el presente (es decir, zonas de perdida)
point_areas_loss2C$x <- as.numeric(point_areas_loss2C$x)
point_areas_loss2C$y <- as.numeric(point_areas_loss2C$y)


#4C
point_presencia4c <- data.frame(rasterToPoints(diferencia_4c))##convertir el mapa del presente en puntos
point_areas_loss4c <- subset(point_presencia4c, layer == 5)###selecciona solo los puntos que son "presencia" solo en el presente (es decir, zonas de perdida)
point_areas_loss4c$x <- as.numeric(point_areas_loss4c$x)
point_areas_loss4c$y <- as.numeric(point_areas_loss4c$y)


################################################################################################
#extraemos los valores de las diferencias climaticas para cada punto############################
################################################################################################
#2C
point2C <- SpatialPointsDataFrame(point_areas_loss2C[,1:2],point_areas_loss2C)

#extraemos los valores ambientales para esas localidad y omitimos los datos sin informacion
presencias_clima2c <- data.frame(raster::extract(varclim_futuro2c,point2C[,1:2]))###extrae los valores climaticos para cada punto
presencias_clima2c2<-data.frame(point2C,presencias_clima2c)
presencias_clima2c3 <- na.omit(presencias_clima2c2)## omite mis datos de presencia sin valores climaticos.


#4C
point4C <- SpatialPointsDataFrame(point_areas_loss4c[,1:2],point_areas_loss4c)

#extraemos los valores ambientales para esas localidad y omitimos los datos sin informacion
presencias_clima4c <- data.frame(raster::extract(varclim_futuro4c,point4C[,1:2]))###extrae los valores climaticos para cada punto
presencias_clima4c2<-data.frame(point4C,presencias_clima4c)
presencias_clima4c3 <- na.omit(presencias_clima4c2)## omite mis datos de presencia sin valores climaticos.


#########################################################
###Obtener los valores promedios que van en el excell####
#########################################################
##2C
mean(presencias_clima2c3$layer.1)##cambio promedio en bio14 para el 2C, va en el excell
mean(presencias_clima2c3$layer.2)##cambio promedio en bio15 para el 2C, va en el excell
mean(presencias_clima2c3$layer.3)##cambio promedio en def para el 2C, va en el excell
mean(presencias_clima2c3$layer.4)##cambio promedio en eleva para el 2C, va en el excell
mean(presencias_clima2c3$layer.5)##cambio promedio en et0 para el 2C, va en el excell
mean(presencias_clima2c3$layer.6)##cambio promedio en ptt para el 2C, va en el excell
mean(presencias_clima2c3$layer.7)##cambio promedio en soil para el 2C, va en el excell
mean(presencias_clima2c3$layer.8)##cambio promedio en tmax para el 2C, va en el excell
mean(presencias_clima2c3$layer.9)##cambio promedio en tmin para el 2C, va en el excell
mean(presencias_clima2c3$layer.10)##cambio promedio en vpd para el 2C, va en el excell

 	 	 	 	 	 	 
##4C
mean(presencias_clima4c3$layer.1)##cambio promedio en bio14 para el 4C, va en el excell
mean(presencias_clima4c3$layer.2)##cambio promedio en bio15 para el 4C, va en el excell
mean(presencias_clima4c3$layer.3)##cambio promedio en def para el 4C, va en el excell
mean(presencias_clima4c3$layer.4)##cambio promedio en eleva para el 4C, va en el excell
mean(presencias_clima4c3$layer.5)##cambio promedio en et0 para el 4C, va en el excell
mean(presencias_clima4c3$layer.6)##cambio promedio en ptt para el 4C, va en el excell
mean(presencias_clima4c3$layer.7)##cambio promedio en soil para el 4C, va en el excell
mean(presencias_clima4c3$layer.8)##cambio promedio en tmax para el 4C, va en el excell
mean(presencias_clima4c3$layer.9)##cambio promedio en tmin para el 4C, va en el excell
mean(presencias_clima4c3$layer.10)##cambio promedio en vpd para el 4C, va en el excell

