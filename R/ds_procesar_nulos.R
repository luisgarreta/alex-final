#' Muestra y procesa los valores nulos
#'
#' Muesta el numero de nulos por variable de los datos de entrada.
#' @import openxlsx
#' @param  archivoDatos Nombre del archivo con los datos a procesar.
#' @return Retorna y escribe los resultados a ul archivo "datos_noNulos.csv".
#' @export
ds_procesar_nulos <- function (archivoDatos) {
	datos = read.csv (archivoDatos)
	nombresVariables = names (datos)

	tablaNulos = NULL
	for (VARIABLE in nombresVariables) {
  		NroNulosAntes = sum (is.na (datos [, VARIABLE]))
  		tablaNulos = c (tablaNulos, NroNulosAntes) 
	}

	# Se dejan solo las observaciones con datos completos (sin nulos)
	datosLimpios <- datos[complete.cases(datos), ]

	# Se guardan los resultados
	archivoSalida  = "datos_NoNulos.csv"
	write.csv (datosLimpios, archivoSalida, row.names=F)
	archivoSalida  = "datos_NoNulos.xlsx"
	write.xlsx (datosLimpios, archivoSalida, rowNames=F)

	tablaNoNulos = NULL
	for (VARIABLE in nombresVariables) {
  		NroNulosDespues = sum (is.na (datosLimpios [, VARIABLE]))
  		tablaNoNulos = c (tablaNoNulos, NroNulosDespues) 
	}
	tablaResultados = data.frame (VARIABLE=nombresVariables, NroNulosAntes=tablaNulos, NroNulosDespues=tablaNoNulos)
	return (tablaResultados)
}
