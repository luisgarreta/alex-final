#' Analiza variables cuantitativas.
#'
#' @param archivoDatos Nombre del archivo con los datos
#' @return Retorna la matrix de correlaciones y crea un nuevo
#'         archivo llamado "datos_NoCorrelacionados.csv" con
#'         una de las variables correlaciondas eliminad.
#' @export
ds_analizar_cuantitativas <- function (archivoDatos) {
	datos = read.csv (archivoDatos)
	# Se calcula la matrix de correlaciones
	numeric.var <- sapply(datos, is.numeric) ## Encuentra variables númericas
	corr.matrix <- cor(datos[,numeric.var])  ## Calcula la matrix de correlación

	# Create dataframe from matrix
	m   = corr.matrix
	mdf = data.frame(row=rownames(m)[row(m)[upper.tri(m)]], 
           col=colnames(m)[col(m)[upper.tri(m)]], 
           corr=m[upper.tri(m)])

	for (i in 1:nrow (mdf)) {
		corr = mdf [1,3]
		if (corr > 0.8) {
			var1  = as.character (mdf[i,1])
			var2  = as.character (mdf[i,2])
			index = match (var2, names(datos))
			datos [index] = NULL

			s = sprintf ("Correlación de %s entre %s y %s. Se eliminó la variable %s.\n", round (corr, 2), var1, var2, var2)
			cat (s)
		}
	}
	write.csv (datos, "datos_NoCorrelacionados.csv", row.names=F)

	return (corr.matrix)
}
