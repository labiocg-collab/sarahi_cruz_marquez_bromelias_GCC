library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(stats)
library(sf)
library(terra)
library(raster)

install.packages("sf")
library(sf)
rm(list=ls())

# Binarios model
mapa_distrib_spp <- raster("F:/Resultados/mapas_binarios_finales/mapas_binarios_finales/presente/Aechmea_bromeliifolia.tif")##indica donde esta el mapa binario (presencia y ausencia) de la especie a analizar
plot(mapa_distrib_spp)#3grafica el mapa


# IDONEIDAD model
mapa_idoneidad_spp <- raster("F:/Resultados/Modelos finales/Aechmea_bromeliifolia/Final_Models/M_0.6_F_lq_Set_1_EC/Aechmea_bromeliifolia_median.asc")##indica donde esta el mapa binario (presencia y ausencia) de la especie a analizar
plot(mapa_idoneidad_spp)#3grafica el mapa


# Polígono de ANPs y No-ANPs (ya incluye las areas OMEC)
ANPs = readOGR("F:/Resultados/codigo_prueba_anps")#indicar donde está el shapefile que es de ANPs existentes
plot(ANPs)

noANPs = readOGR("F:/Resultados/codigo_prueba_anps/no_anps.shp")#indicar donde está el shapefile de areas que NO son ANPs
plot(noANPs)


################################################################################
CROPPED_ANP <- crop(mapa_distrib_spp, extent(ANPs))
mapa_ANP <- mask(CROPPED_ANP, ANPs)
plot(mapa_ANP)

CROPPED_noANP <- crop(mapa_distrib_spp, extent(noANPs))
mapa_noANP <- mask(CROPPED_noANP, noANPs)
plot(mapa_noANP)


################################################################################
# Convertir los pixeles de presencia (con valor = 1) en puntos
puntos_ANP <- rasterToPoints(mapa_ANP, fun = function(x) x > 0)##de esta manera selecciona solo las areas que era "1" dentro de esta categoria (ANPS)
puntos_noANP <- rasterToPoints(mapa_noANP, fun = function(x) x > 0)##de esta manera selecciona solo las areas que era "1" dentro de esta categoria (NO ANPS)

# Crear un data frame con las coordenadas de los puntos, es decir, de cada pixel de presencia
puntos_ANP_df <- as.data.frame(puntos_ANP)
puntos_ANP_df$y <- as.numeric(as.character(puntos_ANP_df$y))###volver las variables en formato n?merico
puntos_ANP_df$x <- as.numeric(as.character(puntos_ANP_df$x))
# Crear el archivo final de puntos georreferenciados
puntos_ANP_sp <- SpatialPointsDataFrame(puntos_ANP_df[,1:2],puntos_ANP_df)
plot(puntos_ANP_sp, add= T, pch=19, col= "red")

puntos_noANP_df <- as.data.frame(puntos_noANP)
puntos_noANP_df$y <- as.numeric(as.character(puntos_noANP_df$y))###volver las variables en formato n?merico
puntos_noANP_df$x <- as.numeric(as.character(puntos_noANP_df$x))
puntos_noANP_sp <- SpatialPointsDataFrame(puntos_noANP_df[,1:2],puntos_noANP_df)
plot(puntos_noANP_sp, add= T, pch=19, col= "blue")

# Extraer los valores de idoneidad para cada punto dentro y fuera de las ANPs en un data frame y cambiar el nombre de la columna
ANP_idoneidad <- data.frame(raster::extract(mapa_idoneidad_spp, puntos_ANP_sp [,1:2]))
colnames(ANP_idoneidad)
colnames(ANP_idoneidad)[colnames(ANP_idoneidad) == "raster..extract.mapa_idoneidad_spp..puntos_ANP_sp...1.2.."] <- "Idoneidad"

noANP_idoneidad <- data.frame(raster::extract(mapa_idoneidad_spp, puntos_noANP_sp [,1:2]))
colnames(noANP_idoneidad)
colnames(noANP_idoneidad)[colnames(noANP_idoneidad) == "raster..extract.mapa_idoneidad_spp..puntos_noANP_sp...1.2.."] <- "Idoneidad"


#########################################
###    Prueba de Kolmogorov-Smirnov #####
#########################################
##valores promedio: se anotan los valores en la tabla excell
mean(ANP_idoneidad$Idoneidad)##valores en ANPS
sd(ANP_idoneidad$Idoneidad)##valores en ANPS

mean(noANP_idoneidad$Idoneidad)##valores en NO ANPS
sd(noANP_idoneidad$Idoneidad)##valores en NO ANPS


#  Se prueba la distribución de todos los valores de idoneidad, a estos no se les transforma de ninguna forma
ks_test_result <- ks.test(ANP_idoneidad$Idoneidad, noANP_idoneidad$Idoneidad)
print(ks_test_result)##se anotan los valores de P value en la tabla excell


###Explicación de los resultados:
##Para este caso, el Estadístico D = 0.09108 Representa la máxima diferencia entre las dos curvas de distribución acumulada.
##por su parte, el p-valor < 2.2e-16 es muchísimo menor que 0.05. Esto significa que: Se rechaza completamente la hipótesis nula por lo cual Las dos distribuciones de idoneidad NO son iguales. En otras palabras: ""La idoneidad ambiental dentro de ANPs y fuera de ANPs es estadísticamente diferente""

########################################################################################
######"Repetir el analisis para CADA ESPECIE EN CADA ESCENARIO DE MODELACION"###########
########################################################################################

#intento moderno

library(terra)
library(sf)
library(ggplot2)
library(stats)

rm(list = ls())

# --------------------
# 1. LEER RASTERS
# --------------------

mapa_distrib_spp <- rast("F:/Resultados/mapas_binarios_finales/mapas_binarios_finales/dispersion_2C/Aechmea_bracteata.tif")
plot(mapa_distrib_spp)

mapa_idoneidad_spp <- rast("F:/Resultados/Modelos finales/Aechmea_bracteata/Final_Models/M_0.2_F_lqp_Set_2_EC/Aechmea_bracteata_2C_median.asc")
plot(mapa_idoneidad_spp)


# --------------------
# 2. LEER SHAPEFILES (ANTES readOGR, AHORA vect)
# --------------------

ANPs    <- vect("F:/Resultados/codigo_prueba_anps")          # carpeta con shapefile
noANPs  <- vect("F:/Resultados/codigo_prueba_anps/no_anps.shp")

plot(ANPs)
plot(noANPs)


# --------------------
# 3. RECORTAR Y ENMASCARAR
# --------------------

CROPPED_ANP   <- crop(mapa_distrib_spp, ANPs)
mapa_ANP      <- mask(CROPPED_ANP, ANPs)
plot(mapa_ANP)

CROPPED_noANP <- crop(mapa_distrib_spp, noANPs)
mapa_noANP    <- mask(CROPPED_noANP, noANPs)
plot(mapa_noANP)


# --------------------
# 4. PIXELES DE PRESENCIA → PUNTOS
# --------------------

puntos_ANP     <- as.data.frame(mapa_ANP, xy = TRUE, na.rm = TRUE)
puntos_ANP     <- puntos_ANP[puntos_ANP[,3] > 0, ]  # solo valores > 0

puntos_noANP   <- as.data.frame(mapa_noANP, xy = TRUE, na.rm = TRUE)
puntos_noANP   <- puntos_noANP[puntos_noANP[,3] > 0, ]


# Convertir a puntos sf
puntos_ANP_sf   <- st_as_sf(puntos_ANP, coords = c("x","y"), crs = crs(ANPs))
puntos_noANP_sf <- st_as_sf(puntos_noANP, coords = c("x","y"), crs = crs(ANPs))

plot(st_geometry(puntos_ANP_sf), col="red", pch=19)
plot(st_geometry(puntos_noANP_sf), col="blue", add=TRUE, pch=19)


# --------------------
# 5. EXTRAER VALORES DE IDONEIDAD
# --------------------

ANP_idoneidad   <- terra::extract(mapa_idoneidad_spp, vect(puntos_ANP_sf))
noANP_idoneidad <- terra::extract(mapa_idoneidad_spp, vect(puntos_noANP_sf))

colnames(ANP_idoneidad)[2]   <- "Idoneidad"
colnames(noANP_idoneidad)[2] <- "Idoneidad"


# --------------------
# 6. ESTADÍSTICAS
# --------------------

mean(ANP_idoneidad$Idoneidad)
sd(ANP_idoneidad$Idoneidad)

mean(noANP_idoneidad$Idoneidad)
sd(noANP_idoneidad$Idoneidad)


# Prueba KS
ks_test_result <- ks.test(ANP_idoneidad$Idoneidad,
                          noANP_idoneidad$Idoneidad)
print(ks_test_result)
