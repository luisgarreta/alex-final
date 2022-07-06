#' Balancear la variable objetivo.
#'
#' @param  archivoDatos Nombre del archivo con los datos.
#' @return No retorna nada. Crea dos archivos: uno con los datos 
#'         entrenamiento y otro con los datos de pruebas.
#' @import openxlsx
#' @export 
ds_dividir_datos <- function (archivoDatos) {
	#library (openxlsx)
	#archivoDatos = "datos_Filtrados.csv"
	datos = read.csv (archivoDatos)

	# Fijar una "semilla" para que se obtengan siempre los mismos resultados
	set.seed(12345) 

	# Ahora se selecciona una muestra del 70% del total de los datos
	muestra <- sample.int(n = nrow(datos), size = floor(.70*nrow(datos)), replace = F)

	# Se realiza la partición
	datosEntrenamiento <- datos[muestra, ]
	datosPrueba  <- datos [-muestra, ]
	nE =  nrow (datosEntrenamiento)
	nP =  nrow (datosPrueba)

	write.csv (datosEntrenamiento, "datos_Entrenamiento.csv", row.names=F)
	write.csv (datosPrueba, "datos_Pruebas.csv", row.names=F)
	write.xlsx (datosEntrenamiento, "datos_Entrenamiento.xlsx")
	write.xlsx (datosPrueba, "datos_Pruebas.xlsx")

	cat (sprintf ("Se crearon dos archivos:\n datos_Entrenamiento.csv con %s individuos\n datos_Pruebas.csv con %s individuos\n", nE, nP))
}

#ds_dividir_datos ("datos_Filtrados.csv")
