#' Obtiene los conjuntos de datos
#'
#' Obtiene los conjuntos de datos relacionados con desistimiento
#'
#' @param tipo Cadena de texto que indica el tipo de datos a obtener. Dos posibles valores: "preparado" (por defecto) obtiene los datos ya corregidos y preparados para iniciar los análisis, y "original", obtiene los datos originales del problema.
#' @return Retorna un dataframe con los datos solicitados y además escribe los archivos en el directorio de trabajo en formato CSV (.csv) y Excel (.xlsx)
#' @import openxlsx
ds_obtenerDatos <- function (tipo="preparado") {
	if (tipo=="finales") {
		data ("datosFinales")
		write.csv  (datosFinales, "DatosFinales.csv", row.names=F)
		write.xlsx (datosFinales, "DatosFinales.xlsx", rowNames=F)
		return (datosFinales)
	}
	return (NULL)
}


ds_obtenerDatos <- function (tipo="preparado") {
	nombreArchivo = "datos"

	if (tipo=="finales") {
		data ("datosFinales")
		write.csv  (datosFinales, "datos_finales.csv", row.names=F)
		write.xlsx (datosFinales, "datos_finales.xlsx", rowNames=F)
		return (datosFinales)
	}else if (tipo=="originales") {
		data ("datosOriginales")
		write.csv  (datosOriginales, paste0(nombreArchivo, "_originales.csv"), row.names=F)
		write.xlsx (datosOriginales, paste0(nombreArchivo, "_originales.xlsx"), rowNames=F)
		return (datosOriginales)
	}else if (tipo=="formateados") {
		data ("datosFormateados")
		write.csv  (datosFormateados, paste0(nombreArchivo, "_formateados.csv"), row.names=F)
		write.xlsx (datosFormateados, paste0(nombreArchivo, "_formateados.xlsx"), rowNames=F)
		return (datosFormateados)
	}else if (tipo=="preparados") {
		data ("datosPreparados")
		write.csv  (datosPreparados, paste0(nombreArchivo, "_preparados.csv"), row.names=F)
		write.xlsx (datosPreparados, paste0(nombreArchivo, "_preparados.xlsx"), rowNames=F)
		return (datosPreparados)
	}else {
		message ("Error: Tipo de datos no existe")
	}
	return (NULL)
}

#' Retorna y escribe en un archivo los datos finales de desistimiento.
#'
#' @export
ds_datos_finales <- function () {
	datos = ds_obtenerDatos ("finales")
	nVars = ncol (datos)
	nRegs = nrow (datos)
	cat (sprintf ("Conjunto de datos con %d variables y %d registros.\n\n", nVars, nRegs))
	ver (datos, n=15)
	return (datos)
}

#' Crea un archivo para evaluacion del modelo
#'
#' @param   archivoDatos Nombre del archivo de donde se obtendrán los datos de prueba.
#' @import openxlsx dplyr
#' @export
ds_datos_evaluacion <- function () {
	#varObjetivo     = as.character (modelo$formula[-1])[1]
	#varPredictoras  = strsplit(as.character (modelo$formula[-1])[2], " [+] ")[[1]]

	datosPruebas = read.csv ("datos_Pruebas_Seleccionado.csv")
	datosEvaluacion = sample_n (datosPruebas, 10) 
	names (datosEvaluacion) = c("REAL", colnames (datosEvaluacion)[-1])
	datosEvaluacion$REAL [datosEvaluacion$REAL=="0"] = "Fracaso"
	datosEvaluacion$REAL [datosEvaluacion$REAL=="1"] = "Exito"

	write.csv  (datosEvaluacion, "datos_Evaluacion.csv", row.names=F)
	write.xlsx (datosEvaluacion, "datos_Evaluacion.xlsx", rowNames=F)

	#datosPruebas = datos [sample (nrow (datosEvaluacion), n),]
	#datosPruebas$SituacionFinal [datosPruebas$SituacionFinal==1] = "EXITO"
	#datosPruebas$SituacionFinal [datosPruebas$SituacionFinal==0] = "FRACASO"
	#colnames (datosPruebas)[1] = "REAL"
	
	#write.csv (datosPruebas, "datos_Evaluacion.csv", row.names=F)
	#write.xlsx (datosPruebas, "datos_Evaluacion.xlsx", rowNames=F)
	cat ("\nSe creó el archivo 'datos_Evaluacion.xlsx'\n")
}


#' Retorna y escribe en un archivo los datos formateados de desistimiento.
#' Estos datos todavía no están preparados para el análisis pero ya se pueden leer en R.
#' @export
ds_datosFormateados <- function () {
	datos = ds_obtenerDatos ("formateados")
	ver (datosFormateados, n=15)
	return (datos)
}

#' Retorna y escribe en un archio los datos preparados de desistimiento.
#' Estos datos ya se han limpiado y corregido y están listos para los análisis.
#' @export
ds_datosPreparados <- function () {
	datos = ds_obtenerDatos ("preparados")
	return (datos)
}

#' Visualiza rápidamente la estructura de los datos
#'
#' @param  datos Conjunto de datos a visualizar
#' @export
ver <- function (datos, n=5,m=6, suffix="") {
	filename = deparse (substitute (datos))
	name = paste (deparse (substitute (datos)),":  ")
	subDatos = ""
	if (is.null (dim (datos))) {
		dimensions = paste (length (datos))
		#cat (">>>", paste0 (name, class(datos), " : (", paste0 (dimensions),")"))
		if (length (datos) < 6) n = length(datos)
		subDatos = datos [1:n, drop=F]
	}else {
		dimensions = dim (datos)
		#cat (">>>", sprintf ("%s %s: %sx%s", name, class(datos), dimensions[1], dimensions[2]))
		if (n==0 | nrow (datos) < 5) n = nrow(datos)
		if (m==0 | ncol (datos) < 6) m = ncol(datos)
		subDatos = datos[1:n,1:m, drop=F]
	}
	print (subDatos)
	#write.csv (datos, paste0("x-", filename, suffix, ".csv"))
}
verx <- function (...){
	ver (...)
	quit()
}

