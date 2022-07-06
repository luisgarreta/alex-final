#' Imprime y crea un archivo con la estructura actual de los datos del archivo de entrada	 
#' @param archivoDatos Nombre de archivo con los datos (observaciones X variables).
#' @import openxlsx
#' @export	
ds_mostrar_estructura <- function (archivoDatos){
	archivoSalida = "estructura_datos"
	datos = read.csv (archivoDatos, stringsAsFactors=T)
	texto = "________________________"
	mostrarCategorias <- function (variable) {
		if (class(datos[[variable]]) == "factor") {
			xcat ("CUALITATIVA")
			xcat (variable)
			valores = levels(datos[[variable]])
			n = length(valores)
			for (i in 1: n) {
				texto = sprintf('[%s] "%s"', i, valores[i])
				xcat (texto)
			}
		} else {
			xcat ("CUANTITATIVA")
			xcat (variable)
			s = summary(datos[[variable]])
			xcat (sprintf ("MINIMO: %s MAXIMO: %s", s[1], s[6]))
		}
		xcat (texto)
	}

	sink(paste0 (archivoSalida, ".csv"))
	xcat ("VARIABLES:")
	ls = lapply(names(datos), mostrarCategorias);
	sink()
	estructura = read.csv (paste0 (archivoSalida, ".csv"))
	write.xlsx (estructura, paste0(archivoSalida, ".xlsx"))
	
	ver (estructura, n=30)
}

xcat <- function (texto) {
	cat (texto, "\n")
}



#' Imprime y crea un archivo con la estructura actual de los datos del archivo de entrada	 
#' @param archivoDatos Nombre de archivo con los datos (observaciones X variables).
#' @import openxlsx
#' @export	
ds_obtener_estructura <- function (archivoDatos){
	archivoSalida = "estructura_datos"
	datos = read.csv (archivoDatos)
	mostrarCategorias <- function (variable) {
		if (class(datos[[variable]]) == "factor") {
			pprint ("CUALITATIVA")
			pprint (variable)
			valores = levels(datos[[variable]])
			n = length(valores)
			for (i in 1: n) {
				texto = sprintf('[%s] "%s"', i, valores[i])
				pprint (texto)
			}
		} else {
			pprint ("CUANTITATIVA")
			pprint (variable)
			s = summary(datos[[variable]])
			pprint (sprintf ("MINIMO: %s MAXIMO: %s", s[1], s[6]))
		}
		texto = "------------------------"
		pprint (texto)
	}

	sink(paste0 (archivoSalida, ".csv"))
	cat ("ESTRUCTURA_ORIGINAL", ",", "ESTRUCTURA_CAMBIOS","\n")
	ls = lapply(names(datos), mostrarCategorias);
	sink()
	estructura = read.csv (paste0 (archivoSalida, ".csv"))
	write.xlsx (estructura, paste0(archivoSalida, ".xlsx"))
	
	ver (estructura, n=30)

	return (estructura)
}


pprint <- function (texto) {
	cat (texto, ",", texto, "\n")
}



#' Modifica la estructura de los datos
#'
#' Modifica la estructura de un conjunto de datos de acuerdo a una  
#' estructura dada como entrada
#' @param  archivoDatos      Nombre del archivo con los datos a cambia
#' @param  archivoEstructura Nombre del archivo con la estructura 
#' @return No retorna nada sino que escribe los cambios a un nuevo 
#'         archivo de datos llamado "datos_MODIFICADOS.csv"
#' @import openxlsx
#' @export
ds_modificar_estructura <- function (archivoDatos, archivoEstructura) {
	datos      = read.csv (archivoDatos); 
	if (!file.exists (archivoEstructura)) {
		message ("Precaución: Archivo de estructura modificado no existe!!. No se realiza ningún cambio.")
		return (NA)
	} 
	
	estructura = read.csv (archivoEstructura)
	nFilas     = nrow (estructura)

	cambios = c()
	i = 1
	while (i <= nFilas) {
		textoFila = as.character (estructura [i,2])
		cambios = c (cambios, paste0 (i, ": > ", textoFila))

		i=i+1
		nombreActual = trimws (as.character (estructura [i,1]))
		nombreNuevo  = trimws (as.character (estructura [i,2]))
		cambios = c (cambios, paste0 (i,": >> ", nombreActual," ==> ", nombreNuevo))
		colnames(datos)[colnames (datos)==nombreActual] = nombreNuevo

		if (grepl ("CUALITATIVA", textoFila)) {
	 		patron = "(\\[.*\\][ ])"
			while (grepl ("------------", textoFila)==FALSE){
				i=i+1
				textoFila   = as.character (estructura [i,1])
				valorActual = trimws (strsplit (as.character (estructura[i,1]), patron)[[1]][2])
				valorNuevo  = trimws (strsplit (as.character (estructura[i,2]), patron)[[1]][2])
				cambios = c (cambios, paste0 (i, ": ", valorActual," ==> ", valorNuevo))
				  
				datos [,nombreNuevo] = as.character (datos [,nombreNuevo])
				if (is.na (valorActual))
					next
	  			datos[datos[,nombreNuevo]==valorActual & !is.na (datos [,nombreNuevo]),nombreNuevo] = valorNuevo
			}
		}else if (grepl ("FECHA", textoFila)) {
			valorActual = as.character (datos [1,nombreNuevo])
			datos [,nombreNuevo] = paste0 (datos [,nombreNuevo],"-01-01")
			valorNuevo = as.character (datos [1,nombreNuevo])
			while (grepl ("------------", textoFila)==FALSE){
				i=i+1
				textoFila   = as.character (estructura [i,1])
				cambios = c (cambios, paste0 (i, ": ", valorActual," ==> ", valorNuevo))
			}
		}else if (grepl ("CUANTITATIVA", textoFila)) {
			while (grepl ("------------", textoFila)==FALSE){
				i=i+1
				textoFila   = as.character (estructura [i,1])
			}
		}
		cambios = c (cambios, paste0 ("--------------"))
		i=i+1
	}

	# Se guardan los cambios en un nuevo archivo
	write.csv (datos, "datos_MODIFICADOS.csv", row.names=F)
	write.xlsx (datos, "datos_MODIFICADOS.xlsx", rowNames=F)
	cambiosDF = data.frame (CAMBIOS=cambios)
	write.csv (cambiosDF, "cambios_estructura.csv", row.names=T)
	#cambiosDF = read.csv ("cambios_estructura.csv")
	cat ("\nSe creó el archivo 'cambios_estructura.csv' con los cambios.\n")
	return (cambiosDF)
}



