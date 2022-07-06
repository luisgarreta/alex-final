#' Elimina variables de un conjunto de datos.
#'
#' @param  archivoDatos Nombre del archivo con los datos.
#' @param  variables Arreglo de nombres de variables para eliminar.
#' @return Retorna una tabla que compara las variables antes y despues.
#'         y guarda los datos a un archivo llamado "datos_Filtrado.csv".
#' @import openxlsx
#' @export
ds_eliminar_variables <- function (archivoDatos, variables) {
	library (openxlsx)
	datos = read.csv (archivoDatos)
	indices = match (variables, names (datos))
	datosRm = datos [, -c (indices)]
	write.csv  (datosRm, "datos_Filtrados.csv", row.names=F)
	write.xlsx (datosRm, "datos_Filtrados.xlsx", rowNames=F)

	nombresAntes = names (datos)
	nombresDespues = c (names (datosRm), rep ("", ncol(datos)-ncol(datosRm)))
	resultados = data.frame (ANTES=nombresAntes,
							 DESPUES=nombresDespues)
	return (resultados)
}

#variables = c("NroGrupoFamiliar", "AñoIndependizacion", "BeneficioFA", 
#			  "BeneficioFPT", "BeneficioPDT", "BeneficioTRV", 
#			  "ClasificaciónComponente", "Ingreso", "MunicipioResidencia")
#dts = ds_eliminar_variables ("datos_NoNulos.csv", variables)
#dts
