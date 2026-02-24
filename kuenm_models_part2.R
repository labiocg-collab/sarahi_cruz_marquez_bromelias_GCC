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
setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_schiedeana/")### directorio donde estan todos los archivos 

############################################
######### The model creation ###############
############################################
occ_joint <- "Tillandsia_schiedeana_joint.csv"##archivo que tiene los datos completos
occ_tra <- "Tillandsia_schiedeana_train.csv"##archivo que tiene el 80% de los datos para entrenar
M_var_dir <- "M_variables"##carpeta donde estan las capas del presente
batch_cal <- "Candidate_models"##carpeta donde se guardaran los resultados de los 620 modelos candidatos
out_dir <- "Candidate_Models"##carpeta donde se guardaran los resultados de los 620 modelos candidatos
reg_mult <- c(seq(0.2, 2, 0.2), seq(2, 6, 0.5),8)##indicar todas las combinaciones de RM que se usaran
f_clas <- "no.h"###indicar cuales combinaciones de "Feature" se utilizara
args <- NULL 

#Correr el programa para que haga 620 modelos simultaneos que leugo se usaran para definir al mejor.
maxent_path <- "C:/Users/Alexander/AppData/Local/R/win-library/4.2/dismo/java" ##esto debe localizarlo en su computadora
wait <- FALSE
run <- TRUE

############################################
######### The model evaluation #############
############################################
occ_test <- "Tillandsia_schiedeana_test.csv"##indicar donde esta el archivo para evaluar a los 620 modelos
out_eval <- "Calibration_results"##indicar donde guardar los resultados a obtener
threshold <- 5##cual es el criterio de seleccion en referencia al valor de error de omision
rand_percent <- 50##porcentaje a utilizar de los datos de forma aleatoria
iterations <- 500##numero de iteracciones
kept <- TRUE
selection <- "OR_AICc"##criterio de seleccion de modelos: omission error and Akaike criteria
paral_proc <- FALSE ##no correr en paralelo

############################################
######### The FINAL model creation ######### 
############################################
batch_fin <- "Final_models"#donde guardar el(los) modelos finales que se seleccionaron
mod_dir <- "Final_Models"#donde guardar el(los) modelos finales que se seleccionaron
rep_n <- 10#indicar cuantas replicas se haran
rep_type <- "Bootstrap"##cual es el criterio para hacer replicas
jackknife <- TRUE
out_format <- "cloglog"
project <- TRUE##indicar que queremos que haga modelos a futuro
G_var_dir <- "G_variables"##indicar donde estan las capas del futuro
ext_type <- "ext_clam"#indicar que queremos que permita extrapolacion y docampling
write_mess <- TRUE
write_clamp <- TRUE
wait1 <- FALSE
run1 <- TRUE
args <- NULL 

kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, rep.n = rep_n,
          rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir, out.format = out_format, project = project,
          G.var.dir = G_var_dir, ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, 
          maxent.path = maxent_path, args = args, wait = wait1, run = run1)
###Se abrira una ventana negra con letras blancas que ira indicando como va el avance para la elaboracion de (los) modelos finales... este sera el modelo utilizando los mejores parametros de configuracion esa ventana se cierra sola... cuando se cierre significa que ya termino y puede seguir ejecutando las otras partes del script.
#esto puede tardar un poco (quizas un par de horas)... depende de la PC, la cantidad de datos y el tamano del area


############################################
####### The model evaluation final #########
############################################
occ_ind <- "Tillandsia_schiedeana_ind.csv"##Elegir el archivo de 20% de los datos que nunca se usaron para evaluar el modelo
replicates <- TRUE##indicar que hay replicas
threshold <- 10
out_feval <- "Final_Models_evaluation"##nombre del archivo donde se guardaran los resultados

fin_eval <- kuenm_feval(path = mod_dir, occ.joint = occ_joint, occ.ind = occ_ind, replicates = replicates,
                        out.eval = out_feval, threshold = threshold, rand.percent = rand_percent,
                        iterations = iterations, parallel.proc = paral_proc)


best <- read.csv("Final_Models_evaluation/fm_evaluation_results.csv")
knitr::kable(best, caption = "Models selected based on significance, omission rates, and AICc, in that order.")

