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
#setwd("/directorio/principal")

## Rutas
file_path  <- "C:/Users/laura/Downloads/BDliteratura_21_Oct_excel.xlsx"
output_dir <- "C:/Users/laura/Downloads"  # carpeta

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

########################################################################################################################
############################################### Validar campos #########################################################
########################################################################################################################

validar_archivo <- function(file_path, output_dir) {
  # 1) Leer archivo sin forzar tipos
  file_ext <- tolower(tools::file_ext(file_path))
  if (file_ext == "csv") {
    data <- read_csv(file_path, show_col_types = FALSE, trim_ws = FALSE, progress = FALSE)
  } else if (file_ext %in% c("txt","tsv")) {
    data <- read_delim(file_path, delim = "\t", show_col_types = FALSE, trim_ws = FALSE, progress = FALSE)
  } else if (file_ext %in% c("xls","xlsx")) {
    data <- read_excel(file_path)
  } else {
    stop("Formato no compatible. Use csv, txt, tsv o xls/xlsx.")
  }
  
  # 2) Definir columnas requeridas/obligatorias
  requeridas <- c(
    # Obligatorias dentro del set requerido
    "scientificName","decimalLatitude","decimalLongitude","visualizationPrivileges","ID",
    # Recomendadas
    "basisOfRecord","institutionCode","collectionCode","catalogNumber","country","stateProvince",
    "county","day","month","year","institutionID","collectionID","locality","minimumElevationInMeters"
  )
  
  # >>> Ajuste: ya NO son obligatorias day, month, year <<<
  obligatorias <- c("scientificName","decimalLatitude","decimalLongitude","visualizationPrivileges","ID")
  
  # Helpers
  to_chr  <- function(x) { y <- as.character(x); y[is.na(y)] <- ""; y }
  to_num  <- function(x) suppressWarnings(as.numeric(x))
  is_blank <- function(x) {
    if (is.null(x)) return(TRUE)
    x_chr <- trimws(as.character(x))
    length(x_chr) == 0 || is.na(x_chr) || x_chr == ""
  }
  safe_int <- function(x) suppressWarnings(as.integer(x))
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  # 3) Validación de nombres de columnas (SIN detener ejecución)
  validarNombresColumnas <- function(df, requeridas, obligatorias) {
    colCheck <- requeridas %in% colnames(df)
    result <- data.frame(
      columna = requeridas,
      existe  = colCheck,
      stringsAsFactors = FALSE
    )
    result$flag <- ifelse(!result$existe & result$columna %in% obligatorias,
                          "FALTA (obligatoria)",
                          ifelse(!result$existe, "No presente (recomendada)", ""))
    
    columnas_extra <- setdiff(colnames(df), requeridas)
    extra_df <- NULL
    if (length(columnas_extra)) {
      extra_df <- data.frame(
        columna = columnas_extra,
        existe  = TRUE,
        flag    = "Columna no requerida (revise nombre o elimínela)",
        stringsAsFactors = FALSE
      )
    }
    final <- rbind(result, extra_df)
    final
  }
  
  # 4) Ejecutar validación de columnas
  val_columnas <- validarNombresColumnas(data, requeridas, obligatorias)
  oblig_faltantes <- val_columnas$columna[val_columnas$flag == "FALTA (obligatoria)"]
  
  # 5) Validaciones por filas/columnas (solo sobre las columnas que existan)
  errores <- list()
  
  nfilas <- nrow(data)
  if (nfilas == 0) {
    errores <- append(errores, list(list(fila = NA_integer_, columna = NA_character_,
                                         tipo = "Estructura", mensaje = "El archivo no tiene filas de datos.", valor = NA_character_)))
  }
  
  cols_en_reglas <- c(
    "occurrenceID","decimalLatitude","decimalLongitude","privateData","visualizationPrivileges",
    "day","month","year","coordinateUncertaintyInMeters","acceptedNameUsage","gbifID","resourceName",
    "basisOfRecord","institutionCode","collectionCode","catalogNumber","country","stateProvince","county",
    "verbatimLocality","continent","verbatimElevation","recordedBy","downloadDate","ID","scientificName"
  )
  
  if (nfilas > 0) {
    # Preconvertir columnas a character para búsquedas que lo requieran
    data_chr <- data
    for (cc in intersect(colnames(data_chr), cols_en_reglas)) {
      data_chr[[cc]] <- to_chr(data_chr[[cc]])
    }
    
    for (i in seq_len(nfilas)) {
      for (col in intersect(colnames(data), cols_en_reglas)) {
        valor_chr <- to_chr(data[i, col, drop = TRUE])
        
        # Vacíos/NULL (solo error fuerte si es obligatoria o lo exige la regla)
        if (is_blank(valor_chr)) {
          if (col %in% obligatorias || col %in% c("occurrenceID","resourceName","institutionCode","collectionCode",
                                                  "catalogNumber","country","stateProvince","county",
                                                  "verbatimLocality","verbatimElevation","acceptedNameUsage","recordedBy")) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Vacío/Inválido",
                                                 mensaje = "Campo vacío o 'NULL' (obligatorio para esta regla).", valor = valor_chr)))
          }
          # Importante: si está vacío y NO es obligatorio, no seguimos con más validaciones de esa celda
          next
        }
        
        # Caracteres extraños
        if (grepl("[#©Ã]", valor_chr)) {
          errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Caracteres",
                                               mensaje = "Contiene caracteres extraños (#, ©, Ã).", valor = valor_chr)))
        }
        
        # Espacios al inicio/fin
        if (col %in% c("occurrenceID","decimalLatitude","decimalLongitude","privateData","visualizationPrivileges",
                       "day","month","year","coordinateUncertaintyInMeters")) {
          if (grepl("^\\s|\\s$", valor_chr, perl = TRUE)) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Espacios",
                                                 mensaje = "Espacios al inicio/fin.", valor = valor_chr)))
          }
        }
        
        # acceptedNameUsage
        if (col == "acceptedNameUsage" && is_blank(valor_chr)) {
          errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Vacío/Inválido",
                                               mensaje = "acceptedNameUsage vacío.", valor = valor_chr)))
        }
        
        # Lat/Long
        if (col %in% c("decimalLatitude","decimalLongitude")) {
          vn <- to_num(valor_chr)
          if (is.na(vn)) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Numérico",
                                                 mensaje = "No es número decimal válido.", valor = valor_chr)))
          }
        }
        
        # privateData / visualizationPrivileges: 0/1/2
        if (col %in% c("privateData","visualizationPrivileges")) {
          if (!valor_chr %in% c("0","1","2")) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Dominio",
                                                 mensaje = "Valor no válido (use 0,1,2).", valor = valor_chr)))
          }
        }
        
        # occurrenceID duplicados
        if (col == "occurrenceID") {
          dups <- which(to_chr(data[[col]]) == valor_chr)
          if (length(dups) > 1) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Duplicado",
                                                 mensaje = paste0("Duplicado con filas: ", paste(dups + 1, collapse = ", ")),
                                                 valor = valor_chr)))
          }
        }
        
        # gbifID dígitos
        if (col == "gbifID") {
          if (!grepl("^\\d+$", valor_chr) || grepl("[., ]", valor_chr)) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Formato",
                                                 mensaje = "Solo dígitos, sin puntos/comas/espacios.", valor = valor_chr)))
          }
        }
        
        # basisOfRecord
        if (col == "basisOfRecord") {
          permitidos <- c("PreservedSpecimen","LivingSpecimen","HumanObservation","MachineObservation",
                          "MaterialSample","FossilSpecimen","Occurrence","MaterialEntity","Event","Taxon","MaterialCitation","null")
          if (!valor_chr %in% permitidos) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Dominio",
                                                 mensaje = "Valor no válido.", valor = valor_chr)))
          }
        }
        
        # country/stateProvince/county/verbatimLocality
        if (col %in% c("country","stateProvince","county","verbatimLocality") && is_blank(valor_chr)) {
          errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Vacío/Inválido",
                                               mensaje = "Campo vacío.", valor = valor_chr)))
        }
        
        # continent
        if (col == "continent") {
          permitidos <- c("América del Sur","América del Norte","Europa","África","Asia","Oceanía","Antártida")
          if (!valor_chr %in% permitidos) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Dominio",
                                                 mensaje = "Valor no válido.", valor = valor_chr)))
          }
        }
        
        # verbatimElevation
        if (col == "verbatimElevation" && is_blank(valor_chr)) {
          errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Vacío/Inválido",
                                               mensaje = "Campo vacío o inválido.", valor = valor_chr)))
        }
        
        # >>> Ajuste: Fechas solo si NO están vacías <<<
        if (col == "day") {
          if (!is_blank(valor_chr)) {
            val <- safe_int(valor_chr)
            if (is.na(val) || val < 1 || val > 31) {
              errores <- append(errores, list(list(fila = i + 1, columna = "day", tipo = "Fecha",
                                                   mensaje = "Día inválido (1-31).", valor = valor_chr)))
            }
          }
        }
        if (col == "month") {
          if (!is_blank(valor_chr)) {
            val <- safe_int(valor_chr)
            if (is.na(val) || val < 1 || val > 12) {
              errores <- append(errores, list(list(fila = i + 1, columna = "month", tipo = "Fecha",
                                                   mensaje = "Mes inválido (1-12).", valor = valor_chr)))
            }
          }
        }
        if (col == "year") {
          if (!is_blank(valor_chr)) {
            yy <- safe_int(valor_chr)
            # Ajusta mínimo si lo necesitas (ej. 1500)
            if (is.na(yy) || yy < 1500 || yy > current_year) {
              errores <- append(errores, list(list(fila = i + 1, columna = "year", tipo = "Fecha",
                                                   mensaje = paste0("Año inválido (1500–", current_year, ")."), valor = valor_chr)))
            }
          }
        }
        
        # coordinateUncertaintyInMeters
        if (col == "coordinateUncertaintyInMeters") {
          vn <- to_num(valor_chr)
          if (is.na(vn)) {
            errores <- append(errores, list(list(fila = i + 1, columna = col, tipo = "Numérico",
                                                 mensaje = "Vacío o no numérico.", valor = valor_chr)))
          }
        }
        
        # recordedBy
        if (col == "recordedBy" && is_blank(valor_chr)) {
          errores <- append(errores, list(list(fila = i + 1, columna = "recordedBy", tipo = "Vacío/Inválido",
                                               mensaje = "Campo vacío.", valor = valor_chr)))
        }
        
        # downloadDate (YYYY-MM-DD)
        if (col == "downloadDate") {
          f <- suppressWarnings(as.Date(valor_chr, format = "%Y-%m-%d"))
          if (is.na(f)) {
            errores <- append(errores, list(list(fila = i + 1, columna = "downloadDate", tipo = "Fecha",
                                                 mensaje = "Formato inválido (YYYY-MM-DD).", valor = valor_chr)))
          }
        }
        
      } # fin loop columnas
    }   # fin loop filas
  }
  
  # 6) Data frame de errores + resumen de obligatorias faltantes
  if (length(oblig_faltantes)) {
    errores <- append(
      list(list(fila = NA_integer_, columna = NA_character_, tipo = "Resumen",
                mensaje = paste0("FALTAN COLUMNAS OBLIGATORIAS: ", paste(oblig_faltantes, collapse = ", ")),
                valor = NA_character_),
           list(fila = NA_integer_, columna = NA_character_, tipo = "Resumen",
                mensaje = "Las reglas sobre esas columnas no se aplicaron, pero el proceso continuó.",
                valor = NA_character_)),
      errores
    )
  }
  
  if (length(errores) == 0) {
    errores_df <- data.frame(
      fila = NA_integer_,
      columna = NA_character_,
      tipo = "OK",
      mensaje = "Sin errores de validación detectados.",
      valor = NA_character_,
      stringsAsFactors = FALSE
    )
  } else {
    errores_df <- dplyr::bind_rows(lapply(errores, as.data.frame))
  }
  
  # 7) Guardar un ÚNICO Excel de reporte con dos hojas
  file_base   <- tools::file_path_sans_ext(basename(file_path))
  report_file <- file.path(output_dir, paste0(file_base, "_reporte_validacion.xlsx"))
  write_xlsx(list(Columnas = val_columnas, Errores = errores_df), report_file)
  
  message("Reporte de validación (2 hojas) guardado en: ", report_file)
  
  # 8) Guardar archivo revisado (SIN eventDate)
  revised_file <- file.path(output_dir, paste0(file_base, "_revisado.xlsx"))
  suppressWarnings(write_xlsx(data, revised_file))
  message("Archivo revisado guardado en: ", revised_file)
  
  invisible(list(
    reporte_validacion = report_file,
    archivo_revisado   = revised_file,
    columnas_faltantes_obligatorias = oblig_faltantes
  ))
}


## Ejecutar
validar_archivo(file_path, output_dir)



