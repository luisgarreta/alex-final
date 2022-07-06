#'/usr/bin/Rscript
#' Balancear la variable objetivo.
#'
#' @param  archivoDatos Nombre del archivo con los datos.
#' @return No retorna nada. Modifica los datos con la cantidad
#'         balanceada de Exito y Fracaso de la variable objetivo.
#'         Adicionalmente, crea los histogramas de balanceo.
#' @import ggplot2 openxlsx
#' @export 
ds_balancear_varobjetivo <- function (archivoDatos) {
	#library (ggplot2)
	#library (openxlsx)
	#archivoDatos = "datos_Filtrados.csv"

	datos = read.csv (archivoDatos, stringsAsFactors=T)

	# Fijar una "semilla" para que se obtengan siempre los mismos resultados
	set.seed(123)

	s = summary (datos$SFinal)
	nroObservacionesFRACASO = s["Fracaso"]

	# Se obtienen todas las observaciones de cada categoría
	obsEXITO <- which(datos$SFinal == "Exito")
	obsFRACASO     <- which(datos$SFinal == "Fracaso")

	# Se obtiene de las observaciones mayoritarias un nro igual a las minoritarias
	obsMayoritarios  <- sample (obsEXITO, nroObservacionesFRACASO)

	# Se vuelve a crear el conjunto de datos con los dos tipos de observaciones
	datosBalanceados <- datos[c(obsFRACASO, obsMayoritarios), ]

	# Se guardan los resultados
	archivoSalida = "datos_Balanceados.csv"
	write.csv (datosBalanceados, archivoSalida, row.names=F)
	archivoSalida = "datos_Balanceados.xlsx"
	write.xlsx (datosBalanceados, archivoSalida, rowNames=F)

	# Se imprime los totales
	ANTES = summary(datos$SFinal)
	DESPUES = summary(datosBalanceados$SFinal)

	summ  = data.frame (ANTES, DESPUES)
    summT = data.frame (SFinal=rownames (summ),summ)
    dat = reshape2::melt (id=c("SFinal"),summT, value.name="Desmovilizados")

	p = ggplot(dat, aes(x = SFinal, y = Desmovilizados, fill = SFinal)) + 
  		facet_wrap (~variable) + 
		geom_bar (stat = "identity") +
  		geom_text (aes(label = Desmovilizados), vjust = 2)

	ggsave ("histogramas_balanceo.pdf", width=7, height=7);
	#ggsave ("histogramas_balanceo.png", width=7, height=7);
	p
}

#' Elimina observaciones con muy pocos datos en categorías (0.5% del total de los datos).
#' @param  archivoDatos Nombre del archivo con los datos.
#' @return Nada. Guarda los datos en un nuevo archivo: "datos_Limpios.csv"
#' @import openxlsx
#' @export
ds_limpiar_observaciones <- function (archivoDatos) {
	#archivoDatos = "datos_Filtrados.csv"
	datosObs = read.csv (archivoDatos, stringsAsFactors=T)
	listaVariables = names (datosObs)
	listaQuienes   = c()
	nroObservaciones = nrow (datosObs)
	for (variable in listaVariables) {
	  listaCategorias = levels (datosObs[,variable])
	  for (categoria in listaCategorias) {
		n = sum (datosObs[,variable]==categoria)
		ratio = round (100*n/nroObservaciones, 1)
		if (ratio >0 & ratio < 0.5) {
		  quienes = which (datosObs[,variable]==categoria)
		  listaQuienes = union (listaQuienes, quienes)
		  #print (sprintf ("   Obs total: %s, Obs cat: %s, Ratio: %s%s, Categoría: %s", nroObservaciones, n, ratio, "%", categoria))
		}
	  }
	}
	cat (sprintf ("\nNro. de observaciones eliminadas: %s\n", length (listaQuienes)))
	cat ("Los resultados se guardaron en el archivo 'datos_Limpios.csv'\n")
	datosObs = datosObs[-listaQuienes,]
	write.csv (datosObs, "datos_Limpios.csv", row.names=F)
	write.xlsx (datosObs, "datos_Limpios.xlsx", rowNames=F)
}

