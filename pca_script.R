library(kuenm)
library(raster)
library(rgdal)
library(ENMGadgets)

rm(list =ls())

setwd("C:/david_prieto/capas_climaticas_tiff/Presente_2000/")
capas1 = list.files(".", pattern = "*.tif$", full.names = T)
capas_presente <- stack(capas1)

setwd("C:/david_prieto/capas_climaticas_tiff/2070_UKESM1_spp370/")
capas2 = list.files(".", pattern = "*.tif$", full.names = T)
capas_futuro <- stack(capas2)

setwd("C:/david_prieto/PCAresults")

rcpa1 <- kuenm_rpca(variables = capas_presente, var.scale = TRUE, write.result = TRUE, project = TRUE, 
                    proj.vars = capas_futuro, n.pcs = 5)
