#' Procesa la variable objetivo
#'
#' Se cambian los nombres y se toman solo los valores de "EXITO" ("Culminado") 
#' y "FRACASO" ("Fuera del Proceso")
#' @param archivoDatos Nombre del archivo con los datos.
#' @return Retorma y escribe los nuevos datos al archivo "datos_Preparados.csv"
#' @import dplyr openxlsx
#' @export
ds_procesar_varobjetivo <- function (archivoDatos) {
	datosEntrada = read.csv (archivoDatos)

	# Se renombran los estados
	SituacionFinal = as.character (datosEntrada$SituacionFinal)
	SituacionFinal [SituacionFinal=="Culminado"] = "EXITO"
	SituacionFinal [SituacionFinal=="Fuera del Proceso"] = "FRACASO"

	datosEntrada$SituacionFinal = SituacionFinal

	# Se seleccionan solo los desmovilizados objetivo
	datosBin = datosEntrada %>% 
		filter (SituacionFinal=="EXITO" | SituacionFinal=="FRACASO") %>% 
		droplevels

	# Se mEntradaueve la variable objetivo al principio
	SituacionFinal = as.character (datosBin$SituacionFinal)
	posicionVar = which (colnames (datosBin)=="SituacionFinal")
	datosSalida = cbind (SituacionFinal, datosBin [,-posicionVar])

	# Se guarda en un nuevo archivo
	write.csv (datosSalida, "datos_Procesados.csv", row.names=F)
	write.xlsx (datosSalida, "datos_Procesados.xlsx", rowNames=F)
	return (datosSalida)
}
