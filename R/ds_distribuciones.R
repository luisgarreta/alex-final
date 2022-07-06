#' Graficar las distribuciones de las variables cualitativas.
#'
#' @param  archivoDatos Nombre del archivo con los datos.
#' @return No retorna nada sino que crea un archivo con los gráficos
#'         de las distribuciones llamado "graficos_distribuciones.png"
#' @import ggplot2 tidyr dplyr ggthemes
#' @export 
ds_distribuciones <- function (archivoDatos) {
#	library (ggplot2)
#	library (tidyr)
#	library (dplyr)
#	library (ggthemes)

	datos = read.csv (archivoDatos, stringsAsFactors=T)
	# ```{r fig.asp = 0.8, fig.width = 11, warning=FALSE}
	datosTB = as_tibble (datos)

	datosPlot = datosTB %>% select_if(is.factor) %>% 
		pivot_longer(cols = -SFinal, names_to = "key", values_to = "value")

	p = ggplot(datosPlot, aes( x = value, fill = SFinal)) + geom_bar() + 
			theme(axis.title.x=element_blank(), axis.text.x=element_blank()) +
			facet_wrap(~key, scales='free', ncol=5) + scale_x_discrete(labels = NULL) +
			labs(title = 'Distribucion de variables categoricas con relacion a SFinal', x = '') +
			#scale_fill_economist() + theme(axis.text.x = element_text(angle = 45, vjust = 0.1, hjust=0.1))
			scale_fill_economist() 

	ggsave ("graficos-distribuciones.pdf", plot=p, width=15, height=15)
	#ggsave ("graficos_distribuciones.png", plot=p, width=15, height=15)
	p
}

old_ds_distribuciones <- function (archivoDatos) {
	pngPath = system.file ("extdata", "graficos_distribuciones.png", package="desistimiento")

	file.copy (pngPath, getwd())

}

#ds_distribuciones ("datos_NoNulos.csv")
