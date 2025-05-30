## -*- coding: utf-8 -*-
#"""
#Created on Fri Dec 11 2024
#
#@author: Laura Garcia
#@edit: Nerieth Leuro
#"""

##Script que permite hacer una validacion general de un conjunto de datos, validando la presencia de las columnas obligatorias y sugeridas para el ingreso
##de los datos a BioModelos.
##Para la ejecución de este script es necesario tener un archivo con los datos
##Como resultado se obtienen dos archivos txt, uno que contiene el nombre de las columnas obligatorias y sugeridas, un indicativo de si estan presentes o no
##y un alerta indicando si la columna es obligatoria. En el segundo archivo se encuentran listadas las inconsistencias encontradas en cada una de las columnas
## indicando la fila correspondiente y una descripcion de la inconsistencia encontrada

#Cargar librerias
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(writexl)

##Ver el directorio principal
getwd()

##Cambiar el directorio principal
setwd("/directorio/principal")


##Cargar archivo a validar
file_path<-"/archivo-registros-validar"


##Crear el archivo donde se almacenaran los errores encontrados
output_path<-'/ruta-salida'

########################################################################################################################
################################################Validar campos##########################################################
########################################################################################################################

##Función para validar el formato del archivo, la presencia de las columnas y los parametros minimos de la información contenida
validar_archivo <- function(file_path, output_path) {
  # Validar el formato del archivo y leerlo
  file_ext <- tools::file_ext(file_path)
  
  # Leer el archivo sin forzar tipos de datos
  if (file_ext == "csv") {
    data <- read_csv(file_path, show_col_types = TRUE, trim_ws = FALSE)
  } else if (file_ext %in% c("txt", "tsv")) {
    data <- read_delim(file_path, delim = "\t", show_col_types = FALSE, trim_ws = FALSE)
  } else if (file_ext %in% c("xls", "xlsx")) {
    data <- read_excel(file_path)
  } else {
    stop("Formato de archivo no compatible. Use csv, txt, tsv o excel.")
  }
  
  # Ver estructura de los datos
  str(data)

  ##Se crean dos vectores uno con las columnas sugeridas que deberia llevar el arhivo (requeridas) y otro con las obligatarias (obligatorias)
  ##Estas últimas no pueden ir vacias en ningún caso
  ## Columnas esperadas
  requeridas <- c(
    ##Obligatorias
    "scientificName","decimalLatitude", "decimalLongitude","visualizationPrivileges","ID",
    ##Recomendadas
    "basisOfRecord", "institutionCode","collectionCode", "catalogNumber","country", "stateProvince", "county", "day", "month", "year", "institutionID","collectionID",
    "locality","minimumElevationInMeters"
  )
  
  obligatorias<- c(
    ##Obligatorias
    "scientificName","decimalLatitude", "decimalLongitude","visualizationPrivileges","ID", "day", "month", "year")



  ##Se crea la función validarNombresColumnas en la cual se valida la presencia de las columnas sugeridas y obligatorias dentro del archivo. 
  ##como resultado se muestra una tabla con los nombres de las columnas que se espera esten dentro del archivo, indicando si existe y en caso de que
  ##sea una columna obligatoria se muestra un flag indicando que el campo es obligatorio
  validarNombresColumnas <- function(data, requeridas, obligatorias) {
    # Evaluar si cada columna requerida está en el data.frame
    colCheck <- requeridas %in% colnames(data)
    
    # Crear un resultado con el estado de cada columna
    result <- data.frame(
      columna = requeridas,
      existe = colCheck,
      stringsAsFactors = FALSE
    )
    
    # Agregar una columna flag para indicar que un campo es obligatorio
    result$flag <- ifelse(
      !result$existe & result$columna %in% obligatorias, "Este campo es obligatorio", ""
    )
    
    # Identificar columnas adicionales en el data.frame que no están en requeridas
    columnas_extra <- setdiff(colnames(data), requeridas)
    
    # Crear un data.frame para estas columnas adicionales
    if (length(columnas_extra) != 0) {
      columnas_extra_result <- data.frame(
        columna = columnas_extra,
        existe = TRUE,
        flag = "Columna no requerida, revise el nombre o elimínela",
        stringsAsFactors = FALSE
      )
    } else {
      columnas_extra_result <- NULL  # Data frame vacío si no hay columnas extra
    }
    
    # Combinar los resultados de columnas requeridas y adicionales
    final_result <- rbind(result, columnas_extra_result)
    
    # Verificar si falta alguna columna obligatoria
    columnas_faltantes <- result$columna[!result$existe & result$columna %in% obligatorias]
    
    if (length(columnas_faltantes) > 0) {
      stop(paste("Error: Faltan las siguientes columnas obligatorias en el archivo:", 
                 paste(columnas_faltantes, collapse = ", ")))
    }
    
    return(final_result)  
  }
  
  
  
  
  ##Ejecutar función
  val_columnas <-validarNombresColumnas(data, requeridas,obligatorias)
  write.table(val_columnas, file = "ResultadovalidarNombresColumnas.txt", row.names = TRUE)
  
  ## Inicializar errores
  errores <- list()
  
  ## Validar cada fila (ignorando encabezados)
  for (i in seq_len(nrow(data))) {
    fila <- data[i, ]
    
    for (col in colnames(data)) {
      valor <- fila[[col]]
      
      # Validar valores vacíos, NA o "NULL" (pero aceptar "null")
      if (is.na(valor) || valor == "NULL" || tolower(valor) == "null" || valor == "") {
        errores <- append(errores, paste("Fila", i + 1, "Columna", col, "- valor vacío o inválido."))
      }

      # Validar si hay caracteres extraños o símbolos no estándar 
      if (grepl("[#©Ã]", valor)) {
        errores <- c(errores, paste("Fila", i + 1, "columna", col, ": contiene caracteres extraños (# o ©)."))
      }
      
      # Validar espacios en blanco
      if (col %in% c("occurrenceID", "decimalLatitude", "decimalLongitude", "privateData", "visualizationPrivileges", "day", "month", "year", "coordinateUncertaintyInMeters") && 
          grepl("^\\s|\\s$", valor, perl=TRUE)) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- se encuentran espacios en este campo, elimínelos:", valor))
      }
      
      # Validar las características de acceptedNameUsage
      if (col == "acceptedNameUsage" && (is.na(valor) || valor == "NULL" || tolower(valor) == "null" || valor == "")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- se encuentra vacía y es un valor obligatorio.", valor))
      }
      
      # Validar decimalLatitude y decimalLongitude
      if (col %in% c("decimalLatitude", "decimalLongitude") && (is.na(as.numeric(valor)) || !is.numeric(as.numeric(valor)))) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- no es un número decimal válido o está vacío:", valor))
      }
      
      # Validar privateData
      if (col == "privateData" && !valor %in% c("0", "1", "2")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- valor no válido o vacío y es obligatorio:", valor))
      }
      
      # Validar occurrenceID y duplicados
      if (col == "occurrenceID") {
        if (is.na(valor) || valor == "NULL" || tolower(valor) == "null" || valor == "") {
          errores <- append(errores, paste("Fila", i, "Columna", col, "- valor no válido o vacío y es obligatorio."))
        }
        duplicados <- which(data[[col]] == valor)
        if (length(duplicados) > 1) {
          errores <- append(errores, paste("Fila", i, "Columna", col, "- valor duplicado", valor, "en filas:", paste(duplicados, collapse=", ")))
        }
      }
      
      # Validar visualizationPrivileges
      if (col == "visualizationPrivileges" && !valor %in% c("0", "1", "2")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- valor no válido:", valor))
      }
      
      # Validar gbifID
      if (col == "gbifID" && !is.na(valor) && (!grepl("^\\d+$", valor) || grepl("[., ]", valor))) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- Contiene caracteres no válidos o espacios:", valor))
      }
      
      # Validar resourceName
      if (col == "resourceName" && (is.na(valor) || valor == "NULL" || tolower(valor) == "null" || valor == "")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- se encuentra vacía o con valores inválidos:", valor))
      }
      
      # Validar basisOfRecord
      if (col == "basisOfRecord" && !valor %in% c("PreservedSpecimen", "LivingSpecimen", "HumanObservation", "MachineObservation", "MaterialSample", "FossilSpecimen", "null", "Occurrence", "MaterialEntity", "FossilSpecimen", "Event", "Taxon", "MaterialCitation")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- valor no válido:", valor))
      }
      
      # Validar institutionCode, collectionCode y catalogNumber
      if (col %in% c("institutionCode", "collectionCode", "catalogNumber") && (is.na(valor) || valor == "NULL" || tolower(valor) == "null" || valor == "")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- el campo se encuentra vacío:", valor))
      }
      
      # Validar country, stateProvince, county y verbatimLocality
      if (col %in% c("country", "stateProvince", "county", "verbatimLocality") && (is.na(valor) || valor == "NULL" || tolower(valor) == "null" || valor == "")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- el campo se encuentra vacío:", valor))
      }
      
      # Validar continent
      if (col == "continent" && !valor %in% c("América del Sur", "América del Norte", "Europa", "África", "Asia", "Oceanía", "Antártida")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- valor no válido:", valor))
      }
      
      # Validar verbatimElevation
      if (col == "verbatimElevation" && (is.na(valor) || valor == "NULL" || tolower(valor) == "null" || valor == "")) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- se encuentra vacía o con valores inválidos:", valor))
      }
      
      # Validar fechas (día, mes, año)
      if (col == "day" && (!grepl("^\\d{2}$", valor) || as.numeric(valor) > 31)) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- formato inválido para día:", valor))
      }
      if (col == "month" && (!grepl("^\\d{2}$", valor) || as.numeric(valor) > 12)) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- formato inválido para mes:", valor))
      }
      if (col == "year" && (!grepl("^\\d{4}$", valor) || as.numeric(valor) > as.numeric(format(Sys.Date(), "%Y")))) {
        errores <- append(errores, paste("Fila", i, "Columna", col, "- formato inválido para año:", valor))
      }
    
      
      }
      
      ## Validar las caracteristicas de coordinateUncertaintyInMeters
      ifelse (col == "coordinateUncertaintyInMeters" && (is.na(as.numeric(valor)) || !is.numeric(as.numeric(valor)) || tolower(valor) == ""),
              errores <- append(errores, paste("Fila", i, "Columna", col, "- se encuentra vacio o tiene un valor inválido ", valor)),
              "Valor válido"
      )

      ## Validar las caracteristicas de recordedBy
      ifelse (col == "recordedBy" && (is.na(valor) || is.null(valor) || tolower(valor) == ""),
              errores <- append(errores, paste("Fila", i, "Columna", col, "- se encuentra vacio", valor)),
              "Valor válido"
      )

      ## Validar las caracteristicas de downloadDate
      ifelse (col == "downloadDate" && (is.na(as.Date(valor, format="%Y-%m-%d"))),
              errores <- append(errores, paste("Fila", i, "Columna", col, "- formato inválido para fecha:", valor)),
              "Valor válido"
      )
      
      
      # Guardar los errores si existen
      if (length(errores) > 0) {
        writeLines(unlist(errores), output_path)
        message(length(errores), " errores encontrados. Detalles en '", output_path, "'.")
      } else {
        data$day <- as.integer(data$day)
        data$month <- as.integer(data$month)
        # Si no hay errores, crear la columna eventDate combinando day, month y year
        data$eventDate <- sprintf("%02d-%02d-%d", data$day, data$month, data$year)
        
        # Generar el nuevo nombre del archivo con "_revisado"
        file_name <- tools::file_path_sans_ext(basename(file_path))  # Nombre base sin extensión
        revised_file <- file.path(dirname(output_path), paste0(file_name, "_revisado.xlsx"))
        
        # Guardar el archivo revisado
        write_xlsx(data, revised_file)
        message("Archivo sin errores revisado y guardado en: ", revised_file)
      }
        
      }
    
    }
    

  




##Ejecutar la función de validación
validar_archivo(file_path, output_path)



