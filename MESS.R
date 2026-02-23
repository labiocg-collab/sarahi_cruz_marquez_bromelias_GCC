# Paquetes
library(terra)
library(ggplot2)

##leer los archivos de los MESS que se hicieron en cada replica
##escenario 1: +2C
MESS_files <- paste0(
  "D:/Resultados/Modelos finales/Aechmea_bracteata/Final_Models/M_0.2_F_lqp_Set_2_EC/Aechmea_bracteata_",
  0:9,
  "_2C_novel.asc"
)
MESS_stack <- rast(MESS_files)

##PROMEDIO DEL MESS para el escenario 1: +2C
MESS_promedio <- app(MESS_stack, mean, na.rm = TRUE)
plot(MESS_promedio, main = "MESS promedio (+2C)")

#################################################################
####Ahora reclasificamos la capa para proceder a los calculos####
#################################################################
#1) Reclasificación: extrapolación estricta (< -50) -> 0
#2) Valores >= -50: estandarizar a [0,1] usando min/max (solo de esos valores)
vals <- values(MESS_promedio, na.rm = TRUE)
vals_ge <- vals[vals >= -50]

min_ge <- min(vals_ge, na.rm = TRUE)
max_ge <- max(vals_ge, na.rm = TRUE)

# Estandarización y luego categorías: Cortes equitativos de 0: (0-0.20], (0.20-0.40], (0.40-0.60], (0.60-0.80], (0.80-1] + categoría 0 para extrapolación estricta
reclass_mess <- function(x){
  out <- rep(NA_real_, length(x))
  
  # extrapolación estricta
  out[x < -50] <- 0
  
  # estandarizar los >= -50
  idx <- which(x >= -50)
  if(length(idx) > 0){
    z <- (x[idx] - min_ge) / (max_ge - min_ge)
    
    # por seguridad numérica
    z[z < 0] <- 0
    z[z > 1] <- 1
    
    # 5 categorías “equidistantes” en pasos de 0.20 sumando hasta 1:
    # cat 1: 0.00–0.20
    # cat 2: 0.20–0.40
    # cat 3: 0.40–0.60
    # cat 4: 0.60–0.80
    # cat 5: 0.80-1.00
    # (la 5ta categoría ya la tienes como 0 = extrapolación estricta)
    out[idx] <- cut(
      z,
      breaks = c(-Inf, 0.20, 0.40,0.60, 0.85, 1.0000001),
      labels = c(1, 2, 3, 4, 5),
      right = TRUE
    )
    out[idx] <- as.numeric(as.character(out[idx]))
  }
  
  return(out)
}

MESS_cat <- app(MESS_promedio, reclass_mess)
MESS_cat <- as.factor(MESS_cat)

levels(MESS_cat) <- data.frame(
  value = c(0,1,2,3,4,5),
  class = c("Extrapolación estricta",
            "Muy Alto (0–0.20)",
            "Alto (0.20–0.40)",
            "Media (0.40–0.60)",
            "Baja (0.80–0.80)",
            "Muy Baja (0.80–1.00)")
)
windows(width = 12, height = 6)

# 2 paneles: mapa (izquierda) + leyenda (derecha)
layout(matrix(c(1, 2), ncol = 2), widths = c(4.5, 1.5))

# ===== Panel 1: mapa =====
par(mar = c(4, 4, 4, 1))
plot(
  MESS_cat,
  main = "MESS +2°C",
  plg = FALSE
)

# ===== Panel 2: leyenda =====
par(mar = c(4, 0, 4, 0))
plot.new()
legend(
  "center",
  legend = levels(MESS_cat)[[1]]$class,
  fill   = terra::coltab(MESS_cat)[, 2],
  cex    = 0.9,
  bty    = "n"
)

#####################################################################
#### Calcular pixeles en extrapolación estricta (clase = 0) en M ####
#####################################################################
# Tabla de frecuencias de todas las categorías
tabla_freq <- freq(MESS_cat)
print(tabla_freq)

# Opción A (recomendada): identificar por etiqueta
pix_extrap <- tabla_freq$count[tabla_freq$value == "Extrapolación estricta"]
if (length(pix_extrap) == 0) pix_extrap <- 0  # por si no existe

# Total de pixeles válidos (sin NA)
total_pix <- global(!is.na(MESS_cat), "sum", na.rm = TRUE)[1, 1]

# Porcentaje
porcentaje_extrap <- (pix_extrap / total_pix) * 100

cat("Pixeles en extrapolación estricta:", pix_extrap, "/n")
cat("Total pixeles válidos:", total_pix, "/n")
cat("Porcentaje extrapolación estricta:", round(porcentaje_extrap, 2), "%/n")



#################################################################################################
#### Calcular pixeles en extrapolación estricta (clase = 0) en la distribcuión de la especie ####
#################################################################################################
# Leer mapa binario de la especie con terra (recomendado)
mapa_spp <- rast("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/dispersion_2C/Aechmea_bracteata.tif")

# (Recomendado) trabajar con versión numérica del MESS (0–5) para comparar con 0 sin líos de factor
MESS_cat_num <- app(MESS_promedio, reclass_mess)  # 0 = extrapolación estricta, 1–5 = categorías

# 1) Alinear grillas (resample del mapa de especie al MESS) "near" mantiene valores binarios 0/1 sin interpolar
mapa_spp_aligned <- resample(mapa_spp, MESS_cat_num, method = "near")

# Opcional: asegurar que sea 0/1 (por si trae 255 o valores raros)
mapa_spp_aligned <- ifel(mapa_spp_aligned > 0, 1, 0)

# 2) Crear máscara de coincidencia: presencia (1) y extrapolación estricta (0)
coinciden <- (mapa_spp_aligned == 1) & (MESS_cat_num == 0)

# 3) Contar pixeles
n_coinciden <- global(coinciden, "sum", na.rm = TRUE)[1, 1]
n_presencia <- global(mapa_spp_aligned == 1, "sum", na.rm = TRUE)[1, 1]

# 4) Proporción (de pixeles de presencia que caen en extrapolación estricta)
prop_extrap_en_presencia <- n_coinciden / n_presencia

# 5) Imprimir resultados
cat("Pixeles presencia (spp=1):", n_presencia, "/n")
cat("Pixeles presencia en extrapolación estricta (spp=1 & MESS=0):", n_coinciden, "/n")
cat("Proporción de presencia en extrapolación estricta:", round(prop_extrap_en_presencia, 4),
    " (", round(prop_extrap_en_presencia * 100, 2), "% )/n", sep = "")




###############################
####ESCENARIO 2: +4°c##########
###############################
##leer los archivos de los MESS que se hicieron en cada replica
MESS_files2 <- paste0(
  "E:/sarahi_proyecto_bromelias/kuenm_models_spp/Aechmea_bracteata/Final_Models/M_0.2_F_lqp_Set_2_EC/Aechmea_bracteata_",
  0:9, "_4C_novel.asc"
)

MESS_stack2 <- rast(MESS_files2)

##PROMEDIO DEL MESS para el escenario 1: +2C
MESS_promedio2 <- app(MESS_stack2, mean, na.rm = TRUE)
plot(MESS_promedio2, main = "MESS promedio (+4C)")

#################################################################
####Ahora reclasificamos la capa para proceder a los calculos####
#################################################################
#1) Reclasificación: extrapolación estricta (< -50) -> 0
#2) Valores >= -50: estandarizar a [0,1] usando min/max (solo de esos valores)
vals2 <- values(MESS_promedio2, na.rm = TRUE)
vals_ge2 <- vals[vals2 >= -50]

min_ge2 <- min(vals_ge2, na.rm = TRUE)
max_ge2 <- max(vals_ge2, na.rm = TRUE)

MESS_cat2 <- app(MESS_promedio2, reclass_mess)
MESS_cat2 <- as.factor(MESS_cat2)

levels(MESS_cat2) <- data.frame(
  value = c(0,1,2,3,4,5),
  class = c("Extrapolación estricta",
            "Muy Alto (0–0.20)",
            "Alto (0.20–0.40)",
            "Media (0.40–0.60)",
            "Baja (0.80–0.80)",
            "Muy Baja (0.80–1.00)")
)

windows()
plot(MESS_cat2, main = "MESS +4°C")


#####################################################################
#### Calcular pixeles en extrapolación estricta (clase = 0) en M ####
#####################################################################
# Tabla de frecuencias de todas las categorías
tabla_freq2 <- freq(MESS_cat2)
print(tabla_freq2)

# Opción A (recomendada): identificar por etiqueta
pix_extrap2 <- tabla_freq2$count[tabla_freq2$value == "Extrapolación estricta"]
if (length(pix_extrap2) == 0) pix_extrap2 <- 0  # por si no existe

# Total de pixeles válidos (sin NA)
total_pix2 <- global(!is.na(MESS_cat2), "sum", na.rm = TRUE)[1, 1]

# Porcentaje
porcentaje_extrap2 <- (pix_extrap2 / total_pix2) * 100

cat("Pixeles en extrapolación estricta:", pix_extrap2, "/n")
cat("Total pixeles válidos:", total_pix2, "/n")
cat("Porcentaje extrapolación estricta:", round(porcentaje_extrap2, 2), "%/n")



#################################################################################################
#### Calcular pixeles en extrapolación estricta (clase = 0) en la distribcuión de la especie ####
#################################################################################################
# Leer mapa binario de la especie con terra (recomendado)
mapa_spp2 <- rast("E:/sarahi_proyecto_bromelias/mapas_binarios_finales/dispersion_4C/Aechmea_bracteata.tif")

# (Recomendado) trabajar con versión numérica del MESS (0–5) para comparar con 0 sin líos de factor
MESS_cat_num2 <- app(MESS_promedio2, reclass_mess)  # 0 = extrapolación estricta, 1–5 = categorías

# 1) Alinear grillas (resample del mapa de especie al MESS) "near" mantiene valores binarios 0/1 sin interpolar
mapa_spp_aligned2 <- resample(mapa_spp2, MESS_cat_num2, method = "near")

# Opcional: asegurar que sea 0/1 (por si trae 255 o valores raros)
mapa_spp_aligned2 <- ifel(mapa_spp_aligned2 > 0, 1, 0)

# 2) Crear máscara de coincidencia: presencia (1) y extrapolación estricta (0)
coinciden2 <- (mapa_spp_aligned2 == 1) & (MESS_cat_num2 == 0)

# 3) Contar pixeles
n_coinciden2 <- global(coinciden2, "sum", na.rm = TRUE)[1, 1]
n_presencia2 <- global(mapa_spp_aligned2 == 1, "sum", na.rm = TRUE)[1, 1]

# 4) Proporción (de pixeles de presencia que caen en extrapolación estricta)
prop_extrap_en_presencia2 <- n_coinciden2 / n_presencia2

# 5) Imprimir resultados
cat("Pixeles presencia en extrapolación estricta (spp=1 & MESS=0):", n_coinciden2, "/n")
cat("Proporción de presencia en extrapolación estricta:", round(prop_extrap_en_presencia2, 4),
    " (", round(prop_extrap_en_presencia2 * 100, 2), "% )/n", sep = "")



