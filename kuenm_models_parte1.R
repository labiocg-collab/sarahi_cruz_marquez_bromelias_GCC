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

rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo
##CORRER MODELOS DE KUENM
setwd("E:/sarahi_proyecto_bromelias/kuenm_models_spp/Tillandsia_complanata/")### directorio donde estan todos los archivos 

############################################
######### The model creation ###############
############################################
occ_joint <- "Tillandsia_complanata_joint.csv"##archivo que tiene los datos completos
occ_tra <- "Tillandsia_complanata_train.csv"##archivo que tiene el 80% de los datos para entrenar
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

kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir, batch = batch_cal,
          out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas, args = args,
          maxent.path = maxent_path, wait = wait, run = run)
###Se abrir? una ventana negra con letras blancas que ir? indicando como va el avance para la elaboraci?n de los 620 modelos... esa ventana se cierra sola... cuando se cierre significa que ya termino y puede seguir ejecutando las otras partes del script.ESTO PUEDE TARDAR VARIOS MINUTOS E INCLUSO HORAS (DEPENDE DE LA CAPACIDAD DEL COMPUTADOR)...

############################################
######### The model evaluation #############
############################################
occ_test <- "Tillandsia_complanata_test.csv"##indicar donde esta el archivo para evaluar a los 620 modelos
out_eval <- "Calibration_results"##indicar donde guardar los resultados a obtener
threshold <- 5##cual es el criterio de seleccion en referencia al valor de error de omision
rand_percent <- 50##porcentaje a utilizar de los datos de forma aleatoria
iterations <- 500##numero de iteracciones
kept <- TRUE
selection <- "OR_AICc"##criterio de seleccion de modelos: omission error and Akaike criteria
paral_proc <- FALSE ##no correr en paralelo


cal_eval <- kuenm_ceval(path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, batch = batch_cal,
                        out.eval = out_eval, threshold = threshold, rand.percent = rand_percent, iterations = iterations,
                        kept = kept, selection = selection)

###Aparecera una ventana en blanco con una barra de progreso.. esa barra se ir? colocando de color azul a medida que el programa avance en el an?lisis... al terminar (llegar al 100%) se cerrar? y entonces podr? avanzar en el script. ESTO IGUAL PUEDE TARDAR VARIOS MINUTOS Y/O HORAS, recuerde que esta evaluando 620 modelos (PERO NO es normal que no avance nada en m?s de 2 horas... si eso ocurre hay alg?n problema y debe revisarse).
