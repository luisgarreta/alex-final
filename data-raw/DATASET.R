#!/usr/bin/Rscript
## codigo para preparar los datos de desistimiento 

library (openxlsx)

# sources
datosFinales = read.csv ("datos_finales.csv", check.names=F)
#datosPreparados  = read.csv ("Estadisticas-Ingresados-PREPARADOS.csv", check.names=F)
#datosEvaluacion  = read.xlsx ("datos_Evaluacion.xlsx")
#parametrosModelo = readRDS  ("modelo.rds")

usethis::use_data (datosFinales, overwrite=T)
#usethis::use_data (datosPreparados, overwrite=T)
#usethis::use_data (parametrosModelo, overwrite=T)
#usethis::use_data (datosEvaluacion, overwrite=T)
