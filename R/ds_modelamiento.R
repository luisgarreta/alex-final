#' Balancear la variable objetivo.
#'
#' @param  archivoDatos Nombre del archivo con los datos.
#' @return No retorna nada solo retorna una tabla con la variable
#'         objetivo y las variables predictoras.
#' @export 
ds_resumen_modelo <- function (archivoDatos) {
	datosEntrada   = read.csv (archivoDatos)
	variables      = names (datosEntrada)
	varObjetivo    = variables [1]
	varPredictoras = variables [-1]
	nPred          = length(varPredictoras)

	rowsVarObj = c (varObjetivo, rep ("", nPred-1))
	df = data.frame (Variable_Objetivo=rowsVarObj,
					 Variables_Predictoras=varPredictoras)
	return (df)
}

#' Retorna la formula del modelo a ajustar deacuerdo a los datos de entrada.
#' @param  archivoDatos Nombre del archivo con los datos.
#' @export
ds_formula_modelo <- function (archivoDatos) {
	datosEntrada   = read.csv (archivoDatos)
	variables      = names (datosEntrada)
	varObjetivo    = variables [1]
	varPredictoras = variables [-1]
	nPred          = length(varPredictoras)
	formulaModelo  = as.formula (paste (varObjetivo,"~", paste (varPredictoras, collapse="+")))
	return (formulaModelo)
}

#' Binariza la variable objetivo a 1: Exito y 0: Fracaso
#' @param  archivoDatos Nombre del archivo con los datos sin tranformar.
#' @return datos transformados.
#' @import openxlsx
#' @export
ds_binarizar <- function (archivoDatos) {
	datos = read.csv (archivoDatos)
	datos$SFinal = as.character (datos$SFinal)
	datos$SFinal [datos$SFinal=="Exito"] = 1
	datos$SFinal [datos$SFinal=="Fracaso"] = 0
	datos$SFinal = as.factor (datos$SFinal)
	write.csv (datos, "datos_Binarizados.csv", row.names=F)
	write.xlsx (datos, "datos_Binarizados.xlsx", rowNames=F)
	cat ("\nSe guardo los nuevos datos en el archivo 'datos_Binarizados.csv'\n") 
	ver (datos)
}

#' Entrena un modelo de regresión lógica
#'
#' @param  archivoDatos Nombre del archivo con los datos.
#' @export
ds_entrenar_modelo <- function (archivoDatos, WARNINGS=F) {
	formulaModelo = ds_formula_modelo (archivoDatos)
	datosEntrenamiento = read.csv (archivoDatos)
	if (WARNINGS==F) 
		suppressWarnings (modelo <<- glm (formulaModelo, family=binomial(link="logit"), maxit=100, data=datosEntrenamiento))
	else
		modelo <<- glm (formulaModelo, family=binomial(link="logit"), maxit=100, data=datosEntrenamiento)

	nDatos = nrow (datosEntrenamiento)
	cat (sprintf ("\nModelo logit entrenado con %s datos de desmovilizados\n", nDatos))
	return (modelo)
}

#' Prueba un modelo de regresión lógica.
#'
#' @param  archivoDatos Nombre del archivo con los datos.
#' @export
ds_probar_modelo <- function (modelo, archivoDatos, WARNINGS=F) {
	datosPrueba = read.csv (archivoDatos)
	if (WARNINGS==F)
		suppressWarnings (resultados <<- predict (modelo,newdata=datosPrueba,type='response'))
	else
		resultados <<- predict (modelo,newdata=datosPrueba,type='response')

	resultados <- ifelse(resultados > 0.5,1,0)
	errorDeClasificacion <- mean(resultados != datosPrueba$SFinal)

	cat (sprintf ("\nPrecision de la Regresión Lógistica: %s\n", 1-errorDeClasificacion))
}

#' Selecciona las variables más significativas de un modelo de Regresión Logística.
#' 
#' @param  archivoDatos Nombre del archivo con los datos.
#' @export
ds_seleccionar_variables <- function (archivoDatosEntrenamiento, archivoDatosPruebas, WARNINGS=F) {
	set.seed(123)
	datosEntrenamiento = read.csv (archivoDatosEntrenamiento)
	# 1. Se define el modelo sin variables: solo con el intercepto
	# 2. Se define el modelo con todas las variables
	# 3. Ejecutla la regresión por pasos
	if (WARNINGS) {
		modeloSimple <<- glm (SFinal~1, family=binomial(link="logit"), data=datosEntrenamiento)
		modeloCompleto <<- glm (SFinal~., family=binomial(link="logit"), data=datosEntrenamiento)
		forward  <<- step(modeloSimple, direction='forward', scope=formula(modeloCompleto), trace=0)
	}
	else {
		suppressWarnings (modeloSimple <<- glm (SFinal~1, family=binomial(link="logit"), data=datosEntrenamiento))
		suppressWarnings (modeloCompleto <<- glm (SFinal~., family=binomial(link="logit"), data=datosEntrenamiento))
		suppressWarnings (forward  <<- step(modeloSimple, direction='forward', scope=formula(modeloCompleto), trace=0))
	}

	# Se muestran los resultados
	resultados = forward$anova [,c(1,3,6)]
	resultados [,1] = gsub ("[+] ", "", resultados [,1])

	# Seleccion de variables en los archivos de datos
	variablesSeleccionadas = c("SFinal", resultados [c(-1),1])
	datosPruebas           = read.csv (archivoDatosPruebas)
	datosEntrenamientoSel = datosEntrenamiento [,variablesSeleccionadas]
	datosPruebasSel        = datosPruebas [,variablesSeleccionadas]
	write.csv (datosEntrenamientoSel, "datos_Entrenamiento_Seleccionado.csv", row.names=F)
	write.csv (datosPruebasSel, "datos_Pruebas_Seleccionado.csv", row.names=F)

	cat (sprintf ('\n Se crearon dos nuevos archivos: "datos_Entrenamiento_Seleccionado.csv" y "datos_Pruebas_Seleccionado.csv"\n'))

	return (resultados)
}

#' Evalúa diferentes métricas a un modelo.
#'
#' @importFrom caret confusionMatrix
#' @import ROCR Metrics
#' @export
ds_evaluar_modelo <- function (modelo, archivoDatosPruebas) {
	# suppressMessages (library(caret))

	datosPruebas = read.csv (archivoDatosPruebas)

	# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
	pdata <- predict(modelo, newdata = datosPruebas, type = "response")

	# use caret and compute a confusion matrix
	reales    = as.factor (datosPruebas$SFinal)
	predichos = as.factor (as.numeric (pdata > 0.5))
	res = caret::confusionMatrix(data = predichos, reference = reales)

	Matrix = res$table
	names (dimnames (Matrix)) = c ("Predichos", "Reales")
	Exactitud     = res$byClass ["Balanced Accuracy"]
	Sensibilidad  = res$byClass ["Sensitivity"]
	Especificidad = res$byClass ["Specificity"]
	Recall        = res$byClass ["Recall"]

	cat (sprintf ("\nMatriz de confusión: \n"))
	cat ("--------------------------\n")
	print (Matrix)
	cat ("--------------------------")
	cat (sprintf ("\nExactitud     : %s%s\n", round (100*Exactitud,1),"%"))
	cat (sprintf ("\nSensibilidad  : %s%s\n", round (100*Sensibilidad,1),"%"))
	cat (sprintf ("\nEspecificidad : %s%s\n", round (100*Especificidad,1),"%"))

	# Calculo de la curva ROC
	pr   = ROCR::prediction(strtoi (predichos), strtoi (reales))
	perf = ROCR::performance(pr,measure = "tpr",x.measure = "fpr") 
	plot(perf) > Metrics::auc(reales,predichos)
}


#' Predice la variable objetivo "SFinal".
#' 
#' Realiza la predicción de la variable objetivo "SFinal"
#' a partir de los datos de ocho variables: DesembolsoBIE, 
#' PoseeServicioSocial, OcupacionEconomica, EstadoISUN, 
#' NivelEducativo, ExGrupo, ServiciosPúblicos, y TipoVivienda  
#'
#' @param modelo Modelo cargado o entrenado a utilizar.
#' @param archivoDatos Nombre del archivo de datos con la 
#' información de las 8 variables para cada individuo.
#' @return Retorna el mismo conjunto de datos adicionado al
#' inicio la columna "SFinal" con los valores predichos
#' de Exito o FRACASO.
#' @export
ds_predecir <- function (archivoDatos) {
	modelo = ds_cargar_modelo ()
	datosPruebas = read.xlsx (archivoDatos)
	# Use your model to make predictions, in this example newdata = training set, but replace with your test set    
	pdata <- predict (modelo, newdata = datosPruebas, type = "response")

	# use caret and compute a confusion matrix
	predichos = as.numeric (pdata > 0.5)
	datosResultados = data.frame (PREDICCION=predichos, datosPruebas)
	datosResultados$PREDICCION [datosResultados$PREDICCION==1] = "Exito"
	datosResultados$PREDICCION [datosResultados$PREDICCION==0] = "Fracaso"

	write.xlsx (datosResultados, "datos_Predichos.xlsx", rowNames=F)
	write.csv (datosResultados, "datos_Predichos.csv", row.names=F)
	cat ("\nSe creó el archivo de resultados 'datos_Predichos.xlsx'\n")
	ver (datosResultados)
}


#' Guarda el modelo de predicción en un archivo "modelo.rds"
#'
#' @param modelo: Modelo de regresión lógica a guardar en el archivo "modelo.rds".
#' @export
ds_guardar_modelo <- function (modelo) {
	cat ("\n Se guardó el modelo en el archivo binario 'modelo.rds'\n")
	saveRDS (modelo, file="modelo.rds")
}


#' Carga modelo optimizado previamente entrenado.
#'
#' @export
ds_cargar_modelo <- function () {
	cat ("\nSe ha cargado el siguiente modelo de regresión lógica:\n\n") 
	if (file.exists ("modelo.rds")) {
		mdl = readRDS  ("modelo.rds")
	}else {
		data (parametrosModelo)
		mdl = parametrosModelo
	}

	cat (paste (mdl$formula[-1][1],"~", mdl$formula[-1][2], "\n"))
	return (mdl)
}

