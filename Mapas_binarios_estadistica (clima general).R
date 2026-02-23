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
library(dplyr)
library(reshape)
library(kuenm)
library(terra)

rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo
#####Especies----

###################################################
############EVALUACION DEL MODELO##################
###################################################
#1. Evaluar el rendimiento del modelo creado para la especie
present <- raster("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Aechmea_bracteata/Final_Models/M_0.2_F_lqp_Set_2_EC/Aechmea_bracteata_avg.asc")##leer el archivo PROMEDIO de las 10 replicas que se corrieron en el modelo final
plot(present)##grafica el mapa nada mas como metodo de observacion del mapa.

#2. Llamar al archivo .csv de los puntos utilizados para el training del modelo final.
data_spp <- read.csv2("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Aechmea_bracteata/Aechmea_bracteata_joint.csv", sep = ",", header = TRUE)##lee el archivo excell final que tiene TODOS los datos con los que se hicieron los modelos finales (Training data)
data_spp$lat <- as.numeric(as.character(data_spp$lat))###volver las variables en formato n?merico
data_spp$lon <- as.numeric(as.character(data_spp$lon))###volver las variables en formato n?merico
  
#Convertir los puntos en datos de presencia para un SIG
points_occ_fin <- SpatialPointsDataFrame(data_spp[,2:3],data_spp)#convertir a un archivo shp de puntos 
  
#3. Extraer los valores de idoneidad que el modelo predice para cada uno de los puntos de presencia (trainning)
presencia_model <- data.frame(raster::extract(present, points_occ_fin [,2:3]))###extrae los valores para cada uno de los puntos
presencia_model2 <- na.omit(presencia_model)## omite mis datos de presencia sin valores climaticos (es sólo por confirmar y evitar errores porque un NA no puede correr en la selección de umbral)

#4. calcular el valor del 10% de los datos de presencia de la especie
umbral_models <- (quantile(presencia_model2$raster..extract.present..points_occ_fin...2.3.., prob= 0.09))##me indica el valor del umbral, en este caso como dice la metodologia “10th percentile training presence [10PTP]”, se coloca 0.09 porque el valor que sigue (0.10 = 10%) es el que nos interesa. 

umbral_model_fin <- as.data.frame(umbral_models)##aplica el umbral indicado (en este caso es 0.41: ESTE VALOR SE COLOCA EN EL EXCEL)
  
#5. Transformar la capa del anf_present a un mapa binario
present_bin <- present >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
plot(present_bin)###me grafica el mapa
plot(points_occ_fin, add= T, pch=19, col= "red")###grafica los puntos de entrenamiento
  
#6. Llamar al archivo .csv de los puntos utilizados para el testing del modelo (es el archivo que se llama "IND").
data_testing_spp <- read.csv2("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Aechmea_bracteata/Aechmea_bracteata_ind.csv", sep = ",", header = TRUE)
data_testing_spp$lat <- as.numeric(as.character(data_testing_spp$lat))###volver las variables en formato n?merico
data_testing_spp$lon <- as.numeric(as.character(data_testing_spp$lon))###volver las variables en formato n?merico
  
#7. Convertir los puntos en datos de presencia para un SIG
points_test_fin <- SpatialPointsDataFrame(data_testing_spp[,2:3],data_testing_spp)#convertir a un archivo shp de puntos 
#8. Extraer los valores de idoneidad que el modelo predice para cada uno de los puntos de presencia (Testing)
presencia_model_test <- data.frame(raster::extract(present_bin, points_test_fin [,2:3]))###extrae los valores para saber cuantos datos del TESTING son predichos correctamente (es decir cuantos puntos testing tienen valor 1 [zona verde del mapa binario = correcta prediccion])

presencia_model_test2 <- na.omit(presencia_model_test)## omite mis datos de presencia sin valores climaticos
count_test <- as.data.frame(count(presencia_model_test2))##cuenta cuantos datos testing en total hay
suma <- sum(presencia_model_test2$raster..extract.present_bin..points_test_fin...2.3..)###me indica en numero cuantos datos si fueron bien predichos
  
#9. Calcular el porcentaje de omision de datos en el modelo
omission_error <- 100 -((suma/(count(presencia_model_test2)))*100)
omission_error##me da el valor que debo colocar en el archivo excell

###################################################
############binarización DEL MODELO################
###################################################
###Salvar el mapa de la especie
setwd("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/presente/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(present_bin, filename= "Aechmea_bracteata.tif", overwrite=T, suffix='names')
  

#######################
#######FUTURO 2C#######
#######################
model_2C <- raster("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Aechmea_bracteata/Final_Models/M_0.2_F_lqp_Set_2_EC/Aechmea_bracteata_2C_avg.asc")

#.Transformar los mapas a mapas binarios
model_2C_bin = model_2C >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
plot(model_2C_bin)  

###Salvar el mapa de la especie
setwd("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/dispersion_2C/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(model_2C_bin, filename= "Aechmea_bracteata.tif", overwrite=T, suffix='names')
  
#######################
#######FUTURO 4C#######
#######################
model_4C <- raster("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Aechmea_bracteata/Final_Models/M_0.2_F_lqp_Set_2_EC/Aechmea_bracteata_4C_avg.asc")

#.Transformar los mapas a mapas binarios
model_4C_bin = model_4C >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
plot(model_4C_bin)  

###Salvar el mapa de la especie
setwd("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/dispersion_4C/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(model_4C_bin, filename= "Aechmea_bracteata.tif", overwrite=T, suffix='names')


###################################
###FUTUROS NO DISPERSION###########
###################################
model_2c_nd <- model_2C_bin + present_bin###suma los mapas del futuro y el presente para buscar zonas consenso
model_2c_nd_bin <- model_2c_nd >= 2 ##trasnforma el mapa en solo presencia (sitios 2 = no dispersion)
plot(model_2c_nd_bin)  

model_4c_nd <- model_4C_bin + present_bin###suma los mapas del futuro y el presente para buscar zonas consenso
model_4c_nd_bin <- model_4c_nd >= 2 ##trasnforma el mapa en solo presencia (sitios 2 = no dispersion)
plot(model_4c_nd_bin) 


###Salvar los mapas de la especie
setwd("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/no_dispersion_2C/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(model_2c_nd_bin, filename= "Aechmea_bracteata.tif", overwrite=T, suffix='names')

setwd("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/no_dispersion_4C/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(model_4c_nd_bin, filename= "Aechmea_bracteata.tif", overwrite=T, suffix='names')

###################################
###areas estables en el tiempo#####
###################################
suma_mapas <- present_bin + model_2C_bin + model_4C_bin + model_2c_nd_bin + model_4c_nd_bin ##suma todos los mapas en el timepo de la especie para buscar zonas consenso
plot(suma_mapas)##muestra el mapa sumado
estable_areas_bin <- suma_mapas >= 5 ##trasnforma el mapa en solo presencia (sitios con valor "5" en la suma = presencia)
plot(estable_areas_bin)##muestra el mapa final

###Salvar el mapa de la especie
setwd("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/estable_areas/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(estable_areas_bin, filename= "Aechmea_bracteata.tif", overwrite=T, suffix='names')



####################################################
####################################################
####Calcular los impactos del cambio climatico######
####################################################
####################################################
raster::freq(present_bin)###me indica el valor de sitios "1 = presencia" en el presente, el valor va en el excell

raster::freq(model_2C_bin)###me indica el valor de sitios "1 = presencia" en el futuro 2C con dispersion, el valor va en el excell. Si no hay valor "1" reportado en el resultado entonces se escribe cero, lo cual indica extinción

raster::freq(model_4C_bin)###me indica el valor de sitios "1 = presencia" en el futuro 4C con dispersion, el valor va en el excell. Si no hay valor "1" reportado en el resultado entonces se escribe cero, lo cual indica extinción

raster::freq(model_2c_nd_bin)###me indica el valor de sitios "1 = presencia" en el futuro 2C con NO dispersion, el valor va en el excell. Si no hay valor "1" reportado en el resultado entonces se escribe cero, lo cual indica extinción

raster::freq(model_4c_nd_bin)###me indica el valor de sitios "1 = presencia" en el futuro 4C con NO dispersion, el valor va en el excell. Si no hay valor "1" reportado en el resultado entonces se escribe cero, lo cual indica extinción

raster::freq(estable_areas_bin)###me indica el valor de sitios "1 = presencia" para areas estables, el valor va en el excell. Si no hay valor "1" reportado en el resultado entonces se escribe cero, lo cual indica que esta especie no tiene sitios estables en el tiempo











