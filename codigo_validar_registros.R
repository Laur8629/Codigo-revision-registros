## -*- coding: utf-8 -*-
#"""
# Created on Fri Dec 11 2024
# Edited on ?
# Edited on Thur Aug 13 2026
#
# @author: Laura Garcia
# @edit: Nerieth Leuro
# @edit2: Nathalia Otero Santamaría
#"""
# ###################################################################################################################### #
#                                                          README                                                        #
# ###################################################################################################################### #

# Este script permite hacer una validación general de la presencia de las columnas obligatorias y sugeridas por BioModelos
# en un conjunto de datos correspondiente a registros de ocurrencia.
#
# Para la ejecución de este script es necesario tener un archivo con los datos.
#
# Como resultado, se obtienen dos archivos .txt.  Uno de ellos contiene el nombre de las columnas obligatorias y sugeridas, 
# un indicativo de su presencia o ausencia en el conjunto de datos y un alerta para indicadndo la obligatoriedad de las 
# columnas respectivas. El otro archivo contiene listadas las inconsistencias encontradas en cada una de las columnas, 
# indicando la fila correspondiente y una descripción de la inconsistencia encontrada.



# ###################################################################################################################### #
#                           Cargar librerías, y definir directorios y archivos de entrada                                #
# ###################################################################################################################### #

# Cargar librerías
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(writexl)
library(openxlsx)
library(ggplot2)
library(viridis)

# Ver/cambiar el directorio principal
getwd()
main_dir <- "H:/Mi unidad/BioModelos/Agendas/Anfibios_amazonicos/"
setwd(main_dir)


# Definir las rutas de archivo de entrada y del directorio de salida
file_path  <- "F1_registros_originales/Registros_Fabio-Zabala_Anfibios-amazonicos_2026.csv" 
output_dir <- "F2_revision_estructura_campos/Campos_minimos_expertos/"

# Asignar nombre del grupo temático o taxonómico
# En vez de espacios para separar, utilice '-'.Ejemplo: Aves-Endemicas
group <- "Anfibios-amazonicos"

# Asignar variables de procedencia de los datos (GBIF o no GBIF) y nombre de experto o fecha de descarga 
# según corresponda
gbif <- FALSE
expert <- "ANM-JCD-FZ"
#downloadDate <- "2026-09-01"


# Crear carpetas y subcarpetas del directorio de salida en caso de que no existan
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# ########################################################################################################### #
#                                       Definir vectores de opciones                                          #
# ########################################################################################################### #

# Requeridas (req): Las columnas que debe tener el archivo (así el campo esté vacío para alguna(s) fila(s)

req <- c("occurrenceID", "acceptedNameUsage", "speciesOriginal", "basisOfRecord","institutionCode",
         "collectionCode","catalogNumber", "recordedBy","day","month","year", "identifiedBy","dateIdentified",
         "country", "stateProvince", "county", "locality","minimumElevationInMeters", "decimalLatitude",
         "decimalLongitude", "source","createdCitationBm", "visualizationPrivileges")


# Oblitarias (mand): Las columnas que debe tener en el archivo y que deben estar documentadas para todos los 
# registros
mand <- c("occurrenceID", "acceptedNameUsage", "speciesOriginal", "country", "stateProvince",
          "decimalLatitude","decimalLongitude","visualizationPrivileges")

# Cuando son datos de GBIF, 'downloadDate' y 'source' son columnas obligatorias
if (gbif){
  req <- c(req, "downloadDate")
  mand <- c(mand, "downloadDate", "source")
}


# Valores que puede asumir el campo basisOfRecord
basis_of_record <- c("PreservedSpecimen","LivingSpecimen","HumanObservation","MachineObservation",
                     "MaterialSample","FossilSpecimen","Occurrence","MaterialEntity","Event","Taxon",
                     "MaterialCitation","OtherSpecimen")


# ########################################################################################################### #
#                                       Crear funciones auxiliares                                            #
# ########################################################################################################### #

# *************************************************************************************************************
# 1) Función para castear una variable
# *************************************************************************************************************

# Función para convertir valores a caracteres válidos
to_character  <- function(x) {y <- as.character(x); y[is.na(y)] <- "" ; y} 

# ***********************************************************************
# 2) Funciones para realizar validaciones
# ***********************************************************************

# Función para validar si un campo está vacío, es NULL o NA
is_blank <- function(x) {
  if (is.null(x)) return(TRUE) 
  x_chr <- trimws(as.character(x))
  
  if (length(x_chr) == 0 || is.na(x_chr) || x_chr == "") {
    return(TRUE)
  }
  
  return(FALSE)
}

# Función para validar si se encuentran todas las columnas requeridas y obligatorias
validate_col_names <- function(df, required, mandatory) {
  
  # Crear vector de presencia-ausencia de las columnas
  col_check <- required %in% colnames(df)
  
  # Crear dataframe con info de presencia de columnas
  result <- data.frame(
    Columna = required,
    Existe  = ifelse(col_check,"Sí", "No"),
    stringsAsFactors = FALSE
  )
  
  # Crear columna llamada flag que registra columnas faltantes
  result$Flag <- ifelse(!col_check & result$Columna %in% mandatory,
                        "Falta columna obligatoria",
                        ifelse(!col_check, "Falta columna requerida", ""))
  
  # Identificar columnas extra
  columnas_extra <- setdiff(colnames(df), required)
  
  # Crear dataframe para guardar las columnas extra
  extra_df <- NULL
 
    # Añadir flag a columnas extras e incluirlas en el dataframe a retornar 
    if (length(columnas_extra)) {
    extra_df <- data.frame(
      Columna = columnas_extra,
      Existe  = "Sí",
      Flag    = "Columna no requerida (cambie su nombre o elimínela)",
      stringsAsFactors = FALSE
    )
    
    final <- rbind(result, extra_df)
  } else {
      final <- result
  }
  
  final
}

# Función para validar una fecha completa 
validate_date <- function(date, format) {
  tryCatch({
    
    # Año: YYYY
    if (format == "%Y") {
      
      year <- as.integer(date)
      
      if (is.na(year)) {
        return("porque el formato empleado es inválido.")
      }
      
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      
      if (year > current_year) {
        return("porque el año está en el futuro.")
      }
      
      if (year < 1700) {
        return("porque el año es anterior a 1700. Revise si la antigüedad del registro es correcta.")
      }
      
      # No hay más información (mes/día) que validar; se retorna aquí
      # porque as.Date(date, format = "%Y") siempre da NA al no tener mes/día.
      return(TRUE)
    }
    
    # Fecha completa: YYYY-MM-DD
    if (format == "%Y-%m-%d") {
      
      date_parts <- strsplit(date, "-", fixed = TRUE)[[1]]
      month <- as.integer(date_parts[2])
      day <- as.integer(date_parts[3])
      
      if (is.na(month)) {
        return("porque el formato empleado es inválido.")
      }
      if (month < 1 || month > 12) {
        return("porque el mes no existe.")
      }
      if (is.na(day)) {
        return("porque el formato empleado es inválido.")
      }
      if (day < 1 || day > 31) {
        return("porque el día no existe.")
      }
    }
    
    # Año y mes: YYYY-MM
    if (format == "%Y-%m") {
      
      date_parts <- strsplit(date, "-", fixed = TRUE)[[1]]
      month <- as.integer(date_parts[2])
      
      if (is.na(month)) {
        return("porque el formato empleado es inválido.")
      }
      if (month < 1 || month > 12) {
        return("porque el mes no existe.")
      }
    }
    
    # Convertir la fecha (solo llega aquí para "%Y-%m-%d" y "%Y-%m")
    date_ok <- as.Date(date, format = format)
    
    if (is.na(date_ok)) {
      return("porque la fecha no existe o el formato empleado es inválido.")
    }
    if (date_ok > Sys.Date()) {
      return("porque la fecha está en el futuro.")
    }
    if (date_ok < as.Date("1700-01-01")) {
      return("porque la fecha es anterior al año 1700. Revise si la antigüedad del registro es correcta.")
    }
    
    TRUE
    
  },
  error = function(e) {
    "porque el formato empleado es inválido."
  })
}

# Función para validar rangos de fechas
validate_date_range <- function(x) {
  
  # Si no contiene slash
  if (!grepl("/", x, fixed = TRUE)) {
    return("porque el formato es inválido. Revise los formatos válidos en la plantilla de registros.")
  }
  
  # Separar inicio y fin
  parts <- strsplit(x, "/", fixed = TRUE)[[1]]
  
  # Debe haber exactamente dos partes
  if (length(parts) != 2) {
    return("porque hay más de un '/'.")
  }
  
  starting <- parts[1]
  ending <- parts[2]
  
  
  # 1) Validar caso aaaa/AAAA
  if (grepl("^\\d{4}/\\d{4}$", x)) {
    
    start_year <- as.integer(starting)
    end_year    <- as.integer(ending)
    
    if(is.na(start_year) || is.na(end_year)){
      return("porque el formato es inválido.")
    }
    if(start_year < 1700 || end_year < 1700){
     return("porque hay un año que es anterior a 1700. Revise si la antigüedad del registro es correcta.")
    } 
    if(start_year > as.integer(format(Sys.Date(), "%Y")) || end_year > as.integer(format(Sys.Date(), "%Y"))){
      return("porque hay un año que está en el futuro.")
    } 
    if(start_year >= end_year){
      return("porque el segundo año es menor o igual al primero.")
    }
    return(TRUE)
  }

  # 2) Validar caso aaaa-mm/MM
  if (grepl("^\\d{4}-\\d{2}/\\d{2}$", x)) {
    
    year <- as.integer(substr(starting, 1, 4))
    start_month <- as.integer(substr(starting, 6, 7))
    end_month <- as.integer(ending)
 
    
    if(start_month < 1 || end_month < 1 || start_month > 12 || end_month > 12){
      return("porque el mes es inválido.")
    }
    if(start_month >= end_month){
      return("porque el segundo mes es menor o igual al primero.")
    }
    
    start_date <- as.Date(paste(as.character(year), as.character(start_month), sep = "-"),format = "%Y-%m")
    end_date <- as.Date(paste(as.character(year), as.character(end_month), sep = "-"),format = "%Y-%m")
    
    start_date_ok <- validate_date(start_date, format = "%Y-%m-%d")
    end_date_ok <- validate_date(end_date,format = "%Y-%m-%d")
    
    if(start_date_ok != TRUE){
      return(start_date_ok) 
    }
    if(end_date_ok != TRUE){
      return(end_date_ok) 
    }
    
    return(TRUE)
  }
  
  # 3) Validar caso aaaa-mm/AAAA-MM
  if (grepl("^\\d{4}-\\d{2}/\\d{4}-\\d{2}$", x)) {
    
    start_year <- as.integer(substr(starting, 1, 4))
    end_year <- as.integer(substr(ending, 1, 4))
    
    start_month <- as.integer(substr(starting, 6, 7))
    end_month <- as.integer(substr(ending, 6, 7))    
 
    if(start_month < 1 || end_month < 1 || start_month > 12 || end_month > 12){
      return("porque hay un mes inválido.")
    }

    start_date <- as.Date(starting, format = "%Y-%m")
    end_date <- as.Date(ending, format = "%Y-%m")
    
    start_date_ok <- validate_date(start_date, format = "%Y-%m-%d")
    end_date_ok <- validate_date(end_date,format = "%Y-%m-%d")
    
    if(start_date_ok != TRUE){
      return(start_date_ok) 
    }
    if(end_date_ok != TRUE){
      return(end_date_ok) 
    }
    if(start_date >= end_date){
      return("porque el inicio del rango corresponde a un tiempo después o equivalente al final del rango.")
    }
    
    return(TRUE)
  }
  
  # 4) Validar caso aaaa-mm-dd/DD
  if (grepl("^\\d{4}-\\d{2}-\\d{2}/\\d{2}$", x)) {
    
    year <- as.integer(substr(starting, 1, 4))
    month  <- as.integer(substr(starting, 6, 7))
    
    start_day <- as.integer(substr(starting, 9, 10))
    end_day <- as.integer(ending)
    

    if(month < 1 || month > 12){
      return("porque el mes es inválido.")
    } 
    if(start_day < 1 || end_day < 1 || start_day > 31 || end_day > 31){
      return("porque hay un día inválido.")
    }

    start_date <- as.Date(starting, format = "%Y-%m-%d")
    end_date <- as.Date(paste(as.character(year), as.character(month), ending, sep = "-"),format = "%Y-%m-%d")
    
    start_date_ok <- validate_date(start_date, format = "%Y-%m-%d")
    end_date_ok <- validate_date(end_date,format = "%Y-%m-%d")
    
    if(start_date_ok != TRUE){
      return(start_date_ok) 
    }
    if(end_date_ok != TRUE){
      return(end_date_ok) 
    }
    if(start_date >= end_date){
      return("porque el inicio del rango corresponde a un tiempo después o equivalente al final del rango.")
    }
    return(TRUE)
  }
  
  # 5) Validar caso aaaa-mm-dd/AAAA-MM-DD
  if (grepl("^\\d{4}-\\d{2}-\\d{2}/\\d{4}-\\d{2}-\\d{2}$", x)) {

    
    start_year <- as.integer(substr(starting, 1, 4))
    end_year <- as.integer(substr(ending, 1, 4))
    
    start_month <- as.integer(substr(starting, 6, 7))
    end_month <- as.integer(substr(ending, 6, 7))
    
    start_day <- as.integer(substr(starting, 9, 10))
    end_day <- as.integer(substr(ending, 9, 10))
    

    if(start_month < 1 || end_month < 1 || start_month > 12 || end_month > 12){
      return("porque hay un mes inválido.")
    }
    if(start_day < 1 || end_day < 1 || start_day > 31 || end_day > 31){
      return("porque hay un día inválido.")
    }
    
    start_date <- as.Date(starting, format = "%Y-%m-%d")
    end_date    <- as.Date(ending, format = "%Y-%m-%d")
    
    start_date_ok <- validate_date(start_date, format = "%Y-%m-%d")
    end_date_ok <- validate_date(end_date,format = "%Y-%m-%d")
    
    if(start_date_ok != TRUE){
      return(start_date_ok) 
    }
    if(end_date_ok != TRUE){
      return(end_date_ok) 
    }
    if(start_date >= end_date){
      return("porque el inicio del rango corresponde a un tiempo después o equivalente al final del rango.")
    }
    return(TRUE)
  }
  # Si tiene slash pero no corresponde a ninguna estructura
  return("porque el formato es inválido. Revise los formatos válidos en la plantilla de registros.")
}

# Función para validar si el campo occurrenceID cumple con el formato indicado para la 
# celda siendo revisada
validate_occ_ID <- function(x, gbif, group, downloadDate = NULL) {
  
  # Validar formato del campo completo
  if(gbif){
    patron <- paste0("^gbifID:[0-9]+:", group, ":", downloadDate, "$")
  } else {
  patron <- "^expertID:[A-Za-z-]+:[0-9]+:[A-Za-z-]+:[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  }
  if (!grepl(patron, x)) return(FALSE)
  
  # Validar formato de fecha
  date <- sub(".*:([0-9]{4}-[0-9]{2}-[0-9]{2})$", "\\1", x)
  return(validate_date(date, format = "%Y-%m-%d"))
}


# *************************************************************************************************************
# 3) Funciones que generan listas documentando errores
# *************************************************************************************************************

# Función para crear error de celda obligatoria vacía
empty_cell_err <- function(i, col, value){
  list(Fila = i + 1, 
      Columna = col, 
      Tipo = "Vacío",
      Mensaje = paste0("El campo '", col ,"' está vacío, pero es obligatorio."), 
      Valor = value)
}

na_char_err <- function(i, col, value){
  list(Fila = i + 1, 
       Columna = col, 
       Tipo = "Vacío",
       Mensaje = paste0("El campo '", col ,"' dice NA o alguna variante. Si no hay datos para el campo por favor elimine su contenido y déjelo vacío."), 
       Valor = value)
}

# Función para crear error por caracteres inesperados
unexpected_char_err <- function(i, col, value){
  list(Fila = i + 1, 
       Columna = col, 
       Tipo = "Caracteres",
       Mensaje = paste0("El campo '", col, "' contiene caracteres inesperados (©, �, ¿, ?)."), 
       Valor = value)
}

duplicated_col_err <- function(duplicated_col) {
  list(
  Fila = NA_integer_,
  Columna = NA_character_,
  Tipo = "Estructura",
  Mensaje = paste0(
    "La columna '",
    duplicated_col, 
    "' está duplicada en el conjunto de datos."),
  Valor = NA_character_
  )
}

# Función para crear error por formato inválido de una columna
format_err <-  function(i, col, value, explanation = NULL){
  list(Fila = i + 1, 
       Columna = col, 
       Tipo = "Formato",
       Mensaje = paste0("El formato del campo '", col, "' es inválido. ", explanation), 
       Valor = value)
}
# Función para crear error por identificadores duplicados
duplicate_err <- function(i, col, value, dups){
  list(Fila = i + 1, 
       Columna = col, 
       Tipo = "Duplicado",
       Mensaje = paste0("El identificador del campo 'occurrenceID' ya existe en la(s) fila(s): ", paste(dups + 1, collapse = ", ")),
       Valor = value)
}

# Función para crear error por vocabulario controlado no cumplido
vocab_err <- function(i, col, value){
  list(Fila = i + 1, 
       Columna = col, 
       Tipo = "Vocabulario controlado",
       Mensaje = paste0("El campo '", col ,"' no tiene un valor permitido. Revise los valores permitidos en la plantilla."), 
       Valor = value)
}

# Función para crear error por fecha inválida
date_err <- function(i, col, value, explanation = NULL){
  list(Fila = i + 1, 
       Columna = col, 
       Tipo = "Fecha",
       Mensaje = paste0("El campo '", col, "' contiene una fecha inválida ", explanation), 
       Valor = value)  
  
}

# Función para crear error por columnas faltantes
missing_cols_err <- function(col, type){
  list(Fila = NA_integer_, 
       Columna = NA_character_, 
       Tipo = "Estructura",
       Mensaje = paste0(
         "Al conjunto de datos le falta la columna '",
         col, 
         "', las cuales es ", type
       ), 
       Valor = NA_character_) 
}

# Función para crear error por columnas extra no requeridas
extra_cols_err <- function(col){
  list(Fila = NA_integer_, 
       Columna = NA_character_, 
       Tipo = "Estructura",
       Mensaje = paste0(
         "La columna '",
         col, 
         "' no es requerida. Por favor elimínela."
       ), 
       Valor = NA_character_) 
}

# Función para crear error por ausencia de filas
missing_rows_err <- function(){
  list(Fila = NA_integer_, 
       Columna = NA_character_,
       Tipo = "Estructura", 
       Mensaje = "El archivo no tiene filas/registros", 
       Valor = NA_character_)
}

# *************************************************************************************************************
# 4) Funciones para definir el estilo de un grupo de filas y columnas en un libro de trabajo
# *************************************************************************************************************

# Función para añadir el estilo de negrilla, centrado y ajustado a los nombres de las columnas de una
# pestaña de un libro de trabajo
est_bold <- createStyle(
  fontName = "Times New Roman",
  textDecoration = "bold",
  halign = "left",
  valign = "center",
  wrapText = TRUE
)

cols_name_style_bold <- function(wb, tab_name, start_row, end_row, start_col, end_col){
  addStyle(
    wb,
    sheet = tab_name,
    style = est_bold,
    rows = start_row:end_row,
    cols = start_col:end_col,
    gridExpand = TRUE
  )
}

# Función para añadir el estilo de itálica, centrado y ajustado a títulos en una pestaña de un libro
# de trabajo
est_italic <- createStyle(
  fontName = "Times New Roman",
  textDecoration = "italic",
  halign = "left",
  valign = "center",
  wrapText = TRUE
)
cols_name_style_italics <- function(wb, tab_name, start_row, end_row, start_col, end_col){
  addStyle(
    wb,
    sheet = tab_name,
    style = est_italic,
    rows = start_row:end_row,
    cols = start_col:end_col,
    gridExpand = TRUE
  )
}

# Función para añadir el estilo centrado y ajustado a los datos en una pestaña de un libro de trabajo
# pestaña de un workbook
data_style <- function(wb, tab_name, start_row, end_row, start_col, end_col){
  addStyle(
    wb,
    sheet = tab_name,
    style = createStyle(wrapText = TRUE, valign = "center", halign = "left", 
                        fontName = "Times New Roman"),
    rows = start_row:end_row,
    cols = start_col:end_col,
    gridExpand = TRUE
  )
}

# Función para asignar el ancho a una lista de columnas dentro de una pestaña
# de un libro de trabajo
set_cols_width <- function(wb, tab_name,list){
  for (col in names(list)){
    setColWidths(wb, tab_name, as.integer(col), widths = list[[col]])
  }
}

# *************************************************************************************************************
# 5) Funciones para generar gráficas de errores e insertarlas en una pestaña de un libro de trabajo
# *************************************************************************************************************

# Función para generar una gráfica de errores agrupados por algún criterio
err_stats_plot <- function(group, df, y_col = 2) {
  
  cat_col <- names(df)[1]
  val_col <- names(df)[y_col]
  
  # Reordenar los niveles del factor según la cantidad (de mayor a menor)
  df[[cat_col]] <- factor(
    df[[cat_col]],
    levels = df[[cat_col]][order(-df[[val_col]])]
  )
  
  stats_plot <- ggplot(df, aes(
    x = .data[[val_col]],
    y = reorder(.data[[cat_col]], .data[[val_col]]),
    fill = .data[[cat_col]]
  )) + 
    geom_col() +
    scale_fill_viridis_d(option = "D", direction = -1) +
    theme_minimal() +
    labs(
      title = paste0("Errores por ", group),
      x = "Cantidad de errores",
      y = group
    ) +
    theme(
      text = element_text(family = "serif"), 
      plot.title = element_text(
        family = "serif",
        hjust = 0.5, 
        face = "bold", 
        size = 14
      ),
      legend.position = "none"
    )
  
  stats_plot
}

# Función para insertar una gráfica dentro de la pestaña 'Estadísticas'
insert_stats_plot <- function(wb, startRow){
  insertPlot(
    wb,
    sheet = "Estadísticas",
    startRow = startRow,
    startCol = 1,
    width = 6,
    height = 4,
    fileType = "png"
  )
}


# ########################################################################################################### #
#                                    Crear función de validación de campos                                    #
# ########################################################################################################### #

# Función para validar los campos
validate_file <- function(file_path, output_dir, req, mand, basis_of_record, gbif, group, downloadDate = NULL, expert = NULL) {
  
  # ***********************************************************************************************************
  # 1) Leer archivo teniendo en cuenta la extension del archivo de entrada
  # ***********************************************************************************************************
  
  # Convertir la extensión a minúsculas 
  file_ext <- tolower(tools::file_ext(file_path))
  
  # Definir una lista con extensiones y sus delimitadores de columnas
  ext_delim <- list("csv" = ",", "txt" = "\t", "tsv" = "\t")
  
  # Leer todos los campos como si fueran objetos tipo "character" para evitar transformaciones en los datos
  if (file_ext %in% names(ext_delim)){
    data <- read_delim(file_path, delim = ext_delim[[file_ext]], show_col_types = FALSE, trim_ws = TRUE, 
                       progress = FALSE, col_types = "c",  name_repair = "minimal", na = character())
    
    # Guardar los nombres originales (pueden tener duplicados)
    original_col_names <- colnames(data)
    
    # Asignar nombres temporales únicos solo para poder operar con dplyr
    colnames(data) <- make.unique(original_col_names)
    
    # Limpiar espacios no estándares de ASCII
    data <- data |> 
      mutate(across(everything(), ~ stringi::stri_trim_both(.x)))
    
  } else if (file_ext %in% c("xls","xlsx")) {
    # Leer únicamente la primera fila para obtener los nombres originales
    headers <- read_excel(file_path, col_names = FALSE, n_max = 1)
    
    # Convertir los nombres a character
    original_col_names <- as.character(headers[1, ])
    
    # Leer los datos
    data <- read_excel(file_path, col_types = "text", .name_repair = "minimal")
    
    # Asignar nombres temporales únicos solo para poder operar con dplyr
    colnames(data) <- make.unique(original_col_names)
    
    # Limpiar espacios no estándares de ASCII
    data <- data |> 
      mutate(across(everything(), ~ stringi::stri_trim_both(.x)))
             
  } else {
      stop("Formato no compatible. Use .csv, .txt, .tsv o .xls/.xlsx.")
  }
  
  # Identificar nombres de columnas duplicados
  duplicated_cols <- unique(
    original_col_names[
      duplicated(original_col_names) &
        !is.na(original_col_names) &
        original_col_names != ""
    ]
  )
  
  # ***********************************************************************************************************
  # 2) Validar existencia de campos 
  # ***********************************************************************************************************
  
  # Crear lista para almacenar errores
  errors <- list()
  
  # Generar error por nombres de columnas duplicados
  if (length(duplicated_cols) > 0) {
    for (duplicated_col in duplicated_cols){
      errors <- append(errors,list(duplicated_col_err(duplicated_col)))      
    }
  }
  
  # Generar flags para las columnas ausentes o extras
  val_cols_df <- validate_col_names(data, req, mand)
  
  # Almacenar las columnas obligatorias faltantes y añadir source si es de GBIF condicional
  mand_missing <- val_cols_df$Columna[val_cols_df$Flag == "Falta columna obligatoria"]
  if (length(mand_missing) > 0) {
    for(col in mand_missing){
      # Añadir error de columna obligatoria faltante
      errors <- append(errors, list(missing_cols_err(col, " obligatoria.")))
    }
  }
  
  # Almacenar las columnas requeridas faltantes -> añadir download Date si es de GBIF condicional
  req_missing <- val_cols_df$Columna[val_cols_df$Flag == "Falta columna requerida"]
  if (length(req_missing) > 0) {
    for(col in req_missing){
      # Añadir error de columna requerida faltante
      errors <- append(errors, list(missing_cols_err(col, " requerida.")))
    }
  }
  
  # Almacenar las columnas extras no requeridas
  extra_cols <- val_cols_df$Columna[val_cols_df$Flag == "Columna no requerida (cambie su nombre o elimínela)"]
  if (length(extra_cols) > 0) {
    for (col in extra_cols){
      # Añadir error de columnas no requeridas
      errors <- append(errors, list(extra_cols_err(col)))
    }
  }

  
  # ***********************************************************************************************************
    # 3) Validaciones por filas y columnas 
  # *************************************************************************
  
  # Determinar cantidad de filas y columnas
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  
  # Añadir error si no hay filas
  if (n_rows == 0) {
    errors <- append(errors, list(missing_rows_err()))
  }
  
  if (n_rows > 0) {
    
    col_names <- colnames(data)
    
    day <- ""
    month <- ""
    year <- ""
    
    for (i in seq_len(n_rows)) {
      if (i %% 1000 == 0){
        print(paste0("Se está iniciando la revisión de la fila ", i, "."))
      }
      for (col in seq_len(n_cols)) {
      
        # Extraer el nombre de la columna
        col_name <- col_names[col]
        
        # Convertir valor de la celda a caracter
        valor_chr <- to_character(data[i, col, drop = TRUE])
        
        # Colapsar espacios múltiples en uno solo
        valor_chr <- gsub("\\s+", " ", valor_chr) 
        
        # Validar que se llenó el campo si es obligatorio
        if (is_blank(valor_chr)) {
          
          if (col_name %in% mand) {
            # Añadir error ante ausencia de valores
            errors <- append(errors, list(empty_cell_err(i, col_name, valor_chr)))
          }
          
          # Si el campo está vacío, sea o no obligatorio, no se siguen con más validaciones para la celda
          next
        }
        
        # Validar que un campo vacío está vacío y no dice NA o alguna variante
        valor_chr_min <- tolower(valor_chr)
        if (valor_chr_min %in% c("na","n a", "vacio", "vacío", "no aplica", "noaplica", "no hay", "nohay", "-", "--", "---", "n/a")){
          # Añadir error ante caracter de vacío
          errors <- append(errors, list(na_char_err(i, col_name, valor_chr)))
          
          # Si el campo está vacío, sea o no obligatorio, no se siguen con más validaciones para la celda
          next
        }
        
        # Validar la presencia de caracteres inesperados
        if (grepl("©|\uFFFD|\\?|¿", valor_chr)) {
          # Añadir error ante presencia de caracteres inesperados
          errors <- append(errors, list(unexpected_char_err(i, col_name, valor_chr)))
        }
  
        # Validar los campos 'acceptedNameUsage' y 'speciesOriginal'
        if (col_name %in% c("acceptedNameUsage","speciesOriginal")) {
          pattern_names <- "^[A-Z][a-z]{1,} [a-z]{1,}$"
          name_form <- grepl(pattern_names, valor_chr)
          
          if(!name_form){
            # Añadir error de formato de los campos 'acceptedNameUsage' y 'speciesOriginal'
            errors <- append(errors, list(format_err(i, col_name, valor_chr, "Revise los formatos válidos en la plantilla de registros.")))
          }
        }
        
        # Validar los campos 'decimalLatitude' y 'decimalLongitude'
        if (col_name %in% c("decimalLatitude", "decimalLongitude")) {
          
          # Validar formato numérico sin letras ni notación científica
          formato_valido <- grepl("^-?[0-9]+(\\.[0-9]+)?$", valor_chr)
          
          if (!formato_valido) {
            
            errors <- append(errors, list(format_err(i, col_name, valor_chr, "Revise los formatos válidos en la plantilla de registros.")))

            # No continuar con la validación de esta coordenada
            next
          }
          
          # Convertir a numérico
          value <- suppressWarnings(as.numeric(valor_chr))
          
          # Validar rango
          if (col_name == "decimalLatitude" && (value < -90 || value > 90)) {
            errors <- append(errors,list(format_err(i, col_name, valor_chr,"La latitud debe estar entre -90 y 90.")))
            
          } else if (col_name == "decimalLongitude" && (value < -180 || value > 180)) {
            errors <- append(errors,list(format_err(i, col_name, valor_chr,"La longitud debe estar entre -180 y 180.")))
          }
        }
        
        # Validar el campo 'minimumElevationInMeters'
        if (col_name == "minimumElevationInMeters") {
          value <- suppressWarnings(as.numeric(valor_chr))
          
          if (is.na(value)) {
            # Añadir error de formato para los campos numéricos
            errors <- append(errors, list(format_err(i, col_name, valor_chr, "Revise los formatos válidos en la plantilla de registros.")))
          }
        }
        
        # Validar el campo 'visualizationPrivileges'
        if (col_name == "visualizationPrivileges" && !valor_chr %in% c("0","1")) {
          # Añadir error de formato para el campo 'visualizationPrivileges'
          errors <- append(errors, list(vocab_err(i, col_name, valor_chr)))
        }
        
        # Validar el campo 'occurrenceID'
        if (col_name == "occurrenceID"){ 
          
          # Ejecutar la función de validación del campo
          date_occ <- validate_occ_ID(valor_chr, gbif, group, downloadDate)
          
          if (identical(date_occ, FALSE)) {
            
            # Error de formato
            errors <- append(errors,
                             list(format_err(i, col_name, valor_chr, "Por favor no lo modifique puesto que ya fue corregido."))
            )
            
            # Crear occurrenceID automático según el tipo de registro
            if (!gbif) { 
              data[i, col_name] <- paste0(
                "expertID:", expert, ":", i, ":", group, ":", Sys.Date()
              )
            }
            
          } else if (is.character(date_occ)) {
            
            # Error de fecha
            errors <- append(
              errors, list(date_err(i, col_name, valor_chr, date_occ))
            )
            
          }
          
          valor_chr <- to_character(data[i, col, drop = TRUE])
          
          dups <- which(to_character(data[[col]]) == valor_chr)
          
          if (length(dups) > 1) {
            # Añadir error por identificador duplicado en el campo 'occurrence_ID'
            errors <- append(errors, list(duplicate_err(i, col_name, valor_chr, dups)))
          }
        }
        
        # Validar el campo 'basisOfRecord'  
        if (col_name == "basisOfRecord" && !valor_chr %in% basis_of_record && !is_blank(valor_chr)) {
          # Añadir error por no seguir el vocabulario controlado en el campo 'basisOfRecord'
          errors <- append(errors, list(vocab_err(i, col_name, valor_chr)))
        }

        # Validar el campo 'day'
        if (col_name == "day" && !is_blank(valor_chr)) {
            value_int <- suppressWarnings(as.integer(valor_chr))
            if (is.na(value_int) || value_int < 1 || value_int > 31) {
              # Añadir error de formato para el campo 'day' por no  
              errors <- append(errors, list(date_err(i, col_name, valor_chr, "porque el día no está entre 1 y 31")))
            }
            # Guardar día
            day <- valor_chr
        }
        
        # Validar el campo 'month'
        if (col_name == "month" && !is_blank(valor_chr)) {
          value_int <- suppressWarnings(as.integer(valor_chr))
          if (is.na(value_int) || value_int < 1 || value_int > 12) {
            # Añadir error de formato para el campo 'month' por no  
            errors <- append(errors, list(date_err(i, col_name, valor_chr, "porque el mes no está entre 1 y 12")))
          }
          # Guardar mes
          month <- valor_chr
        }
        
        # Validar el campo 'year'
        if (col_name == "year" && !is_blank(valor_chr)) {
          value_int <- suppressWarnings(as.integer(valor_chr))
          current_year <- as.integer(format(Sys.Date(), "%Y"))
          if (is.na(value_int) || value_int < 1900 || value_int > current_year) {
            # Añadir error de formato para el campo 'year' por no  
            errors <- append(errors, list(date_err(i, col_name, valor_chr, "porque el año no está entre 1900 y el año actual")))
          }
          # Guardar año
          year <- valor_chr
        }
        
        # Validar el campo 'downloadDate'
        if (col_name == "downloadDate" && !is_blank(valor_chr)) {
          date <- validate_date(valor_chr, format = "%Y-%m-%d")
          if (date != TRUE) {
            errors <- append(errors, list(date_err(i, col_name, valor_chr, date)))
          }
        }
        
        # Validar el campo 'dateIdentified'
        if (col_name == "dateIdentified" && !is_blank(valor_chr)) {
          date <- FALSE
          formats <- c("%Y", "%Y-%m", "%Y-%m-%d")
          
          for (format in formats) {
            if (validate_date(valor_chr, format) == TRUE) {
              date <- TRUE
              break
            }
          }
          if (!date) {
            date <- validate_date_range(valor_chr)
          }
          if (date != TRUE) {
            errors <- append(errors, list(date_err(i, col_name, valor_chr, date)))
          }
        }
        
      } 
      
      # Validar la fecha en la que se tomó el registro
      if(!is_blank(day) && !is_blank(month) && !is_blank(year)){
        date_occ <- paste0(year, "-", month, "-", day)
        date_check <- validate_date(paste0(year,"-", month, "-", day), format = "%Y-%m-%d")
        if(date_check != TRUE){
          # Añadir error de fecha asociado a la fecha en la que se tomó el registro 
          errors <- append(errors, list(list(Fila = i + 1, 
                                             Columna = NA_character_, 
                                             Tipo = "Fecha", 
                                             Mensaje = paste0("Al juntar los campos 'day', 'month' y 'year' se encontró que la fecha de registro no es válida ", 
                                                              date_check),
                                             Valor = date_occ)))  
          
        }
      }
      # Resetear los datos de la fecha de recolección del registro
      day <- ""
      month <- ""
      year <-""
    } 
  }
  # Crear nombre del archivo para guardar los datos corregidos
  # (la única corrección es el occurrenceID en caso de presentar error),
  # conservando el nombre del archivo de entrada
  file_base <- tools::file_path_sans_ext(basename(file_path))
  val_file <- file.path(output_dir, paste0(file_base, "_val1.xlsx"))
  
  # Guardar archivo
  write_xlsx(data, val_file)
  
  # Guardar errores en un dataframe
  if (length(errors) == 0) {
    errors_df <- data.frame(
      Fila = NA_integer_,
      Columna = NA_character_,
      Tipo = "OK",
      Mensaje = "Sin errores de validación detectados.",
      Valor = NA_character_,
      stringsAsFactors = FALSE
    )
    
  } else {
    errors_df <- dplyr::bind_rows(lapply(errors, as.data.frame))
  }

  # Retornar errores y validación de columnas
  return(list(
    val_cols_df = val_cols_df,
    errors_df = errors_df
  ))
}
  
# ########################################################################################################### #
#                           Crear función para calcular estadísticos de los errores                           #
# ########################################################################################################### #

errors_stat <- function(file_path, output_dir, val_cols_df, errors_df, req){

  # Crear el libro de trabajo 
  wb <- createWorkbook()
  addWorksheet(wb, "README")
  addWorksheet(wb, "Campos")
  addWorksheet(wb, "Errores")
  addWorksheet(wb, "Estadísticas")
  
  
  # ***********************************************************************************************************
  # 1) Insertar todos los errores en el libro de trabajo
  # ***********************************************************************************************************
  
  # Dataframe solo para mostrar en la pestaña "Errores": excluye 'Estructura'
  # (errors_df, sin filtrar, se sigue usando más abajo para las estadísticas/gráficas)
  errors_df_sheet <- errors_df %>% filter(!Tipo == "Estructura")
  
  # Si al quitar 'Estructura' no queda ninguna fila, mostrar un mensaje de "sin errores"
  if (nrow(errors_df_sheet) == 0) {
    errors_df_sheet <- data.frame(
      Fila = NA_integer_,
      Columna = NA_character_,
      Tipo = "OK",
      Mensaje = "Sin errores de validación detectados (fuera de los de estructura).",
      Valor = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  
  # Insertar dataframe
  writeData(wb, "Errores", errors_df_sheet)
  
  # Calcular cantidad de filas
  n_row <- nrow(errors_df_sheet)
  
  # Poner nombres de columnas alineados, en negrilla y ajustado a la celda
  cols_name_style_bold(wb, "Errores", start_row = 1, end_row = 1, start_col = 1, end_col = 5)
  
  # Poner el contenido de la tabla alineado y ajustado a la celda
  data_style(wb, "Errores", start_row = 2, end_row = n_row + 1, start_col = 1, end_col = 5)

  # Asignar ancho de cada columna
  widths <- list("1" = 8, "2" = 25, "3" = 15, "4" = 50, "5" = 30)
  set_cols_width(wb, "Errores", widths)

  # ***********************************************************************************************************
  # 2) Crear tablas de conteo de errores
  # ***********************************************************************************************************

  
  # Sustraer los errores que son de tipo 'OK' (es decir, no hay)
  errors_df_filter <- errors_df %>%
    filter(!Tipo == "OK")
  
  # Crear dataframe con la cantidad de errores por columna
  errors_cols <- as.data.frame(table(errors_df_filter$Columna))
  colnames(errors_cols) <- c("Columna", "Cantidad_de_errores")
  
  # Crear dataframe con la información de todas las columnas
  # y la cantidad de errores para cada una
  info_cols_df <- val_cols_df %>%
    left_join(errors_cols, by = "Columna") %>%
    mutate(
      Cantidad_de_errores = ifelse(
        is.na(Cantidad_de_errores),
        0,
        Cantidad_de_errores
      )
    )
  
  
  # Crear dataframe con la cantidad de errores por tipo
  errors_type <- as.data.frame(table(errors_df_filter$Tipo))
  colnames(errors_type) <- c("Tipo", "Cantidad_de_errores")  
  
  # Quitar errores de tipo Estructura
  errors_message <- errors_df_filter %>% 
    filter(!Tipo == "Estructura")
  # Crear dataframe con la cantidad de errores por mensaje
  errors_message <- as.data.frame(table(errors_message$Mensaje))
  colnames(errors_message) <- c("Mensaje", "Cantidad_de_errores")
  
  # Asignar una clase numérica a cada mensaje
  errors_message <- errors_message %>%
    mutate(Clase = row_number()) %>%
    dplyr::select(Clase, Cantidad_de_errores, Mensaje)
  
  # Extraer tabla de convenciones
  errors_message_class <- errors_message %>%
    dplyr::select(Clase, Mensaje)
  
  # ***********************************************************************************************************
  # 3) Escribir en el libro los errores sobre columnas extras o faltantes
  # ***********************************************************************************************************
  
  # Crear la fila de totales
  total_errs <- tibble(
    Columna = "Total",
    Cantidad_de_errores = sum(info_cols_df$Cantidad_de_errores, na.rm = TRUE)
  )
  
  # Agregarla al final del data frame
  info_cols_df <- bind_rows(info_cols_df, total_errs)
  
  # Insertar dataframe
  writeData(wb, "Campos", info_cols_df, startRow = 1, startCol = 1)
  
  # Calcular cantidad de filas
  n_row <- nrow(info_cols_df)
  
  # Poner el contenido de la tabla alineado y ajustado a la celda
  data_style(wb, "Campos", start_row = 2, end_row = n_row, start_col = 1, end_col = 4)
  
  # Poner nombres de columnas y el total alineados, en negrilla y ajustado a la celda
  cols_name_style_bold(wb, "Campos", start_row = 1, end_row = 1, start_col = 1, end_col = 4)
  cols_name_style_bold(wb, "Campos", start_row = n_row + 1, end_row =  n_row + 1, start_col = 1, end_col = 4)
  
  # Asignar ancho de cada columna
  widths <- list("1" = 25, "2" = 6, "3" = 50, "4" = 20)
  set_cols_width(wb, "Campos", widths)
  
  # ***********************************************************************************************************
  # 4) Crear gráficas de las estadísticas de los errores e insertarlas en el libro de trabajo
  # ***********************************************************************************************************

  # Ordenar de menor a mayor los dataframes
  info_cols_df <- info_cols_df %>%
    arrange(desc(Cantidad_de_errores))
  errors_type <- errors_type %>%
    arrange(desc(Cantidad_de_errores))
  
  # Quitar el total del dataframe de errores por campo 
  info_cols_df <- info_cols_df[info_cols_df$Columna != "Total", ]
  # Generar gráfica de errores por campo
  errors_cols_plot <- err_stats_plot("Columna", info_cols_df, 4)
  print(errors_cols_plot)
  # Insertar la gráfica en la hoja 'Estadísticas'
  insert_stats_plot(wb, 5)
  
  # Generar gráfica de errores por tipo
  errors_type_plot <- err_stats_plot("Tipo", errors_type, 2)
  print(errors_type_plot)
  # Insertar la gráfica en la hoja 'Estadísticas'
  insert_stats_plot(wb, 40)

  # Generar gráfica de errores por clase 
  errors_message_plot <- err_stats_plot("Clase", errors_message, 2)
  print(errors_message_plot)
  # Insertar la gráfica en la hoja 'Estadísticas'
  insert_stats_plot(wb, 66)
  
  # ***********************************************************************************************************
  # 5) Escribir descripciones de las gráficas de estadísticas de errores en el libro de trabajo
  # ***********************************************************************************************************
  
  # Escribir descripción de la primera gráfica
  writeData(wb, "Estadísticas", "Cantidad de errores por campo", startRow = 1, startCol = 1)
  writeData(wb, "Estadísticas", 
            "En esta gráfica encontrará la cantidad de registros con errores de documentación para cada una de las columnas requeridas en el archivo.", startRow = 2, startCol = 1)
  
  # Escribir descripción de la segunda gráfica
  writeData(wb, "Estadísticas", "Cantidad de errores por tipo", startRow = 26, startCol = 1)
  writeData(wb, "Estadísticas", paste(
    "En esta gráfica encontrará la cantidad de registros con errores de documentación por cada tipo de error.", 
    "Los errores se agrupan en las siguientes categorías generales: ",
    "VOCABULARIO CONTROLADO: Los datos documentados no corresponden al vocabulario controlado específico para el campo. ",
    "FECHA: Errores en las fechas documentadas. Esto puede ocurrir bien sea porque no está diligenciadas en el campo, porque están en un formato distinto al de AAAA-MM-DD; porque los días, meses y años no se encuentran en un rango coherente; porque la fecha está en el futuro; o porque la fecha no existe .",
    "FORMATO: La información documentada no corresponde al formato establecido para el campo",
    "VACÍO: El campo está vacío, pero es obligatorio su diligenciamiento",
    "CARACTERES: La información documentada en el campo incluye caracteres inesperados",
    "DUPLICADOS: Se encontró un identificador de registro, en el campo 'occurrenceID', que está duplicado",
    "ESTRUCTURA: El conjunto de datos presenta inconsistencias al faltar columnas obligatorias o requeridas, haber columnas extra o no consignarse ningún registro.",
    sep = "\n"), startRow = 27, startCol = 1)
  
  # Escribir descripción de la tercera gráfica
  writeData(wb, "Estadísticas", "Cantidad de errores por clase", startRow = 61, startCol = 1)
  writeData(wb, "Estadísticas", "En esta gráfica encontrará la cantidad de registros con errores de documentación por cada clase de error. Al costado derecho encontrará el mensaje completo al que hace referencia cada clase de error.", startRow = 62, startCol = 1)
  writeData(wb, "Estadísticas", errors_message_class, startRow = 65, startCol = 12)
  
  # Formato del encabezado de la tabla de clases
  cols_name_style_bold(
    wb,
    "Estadísticas",
    start_row = 65,
    end_row = 65,
    start_col = 12,
    end_col = 13
  )
  
  # Formato del contenido de la tabla de clases
  if (nrow(errors_message_class) > 0) {
    data_style(
      wb,
      "Estadísticas",
      start_row = 66,
      end_row = 65 + nrow(errors_message_class),
      start_col = 12,
      end_col = 13
    )
  }
  
  
  # Combinar celdas dentro de la pestaña 'Estadísticas'
  rows_merge_1 <- list("1" = 1, "2" = 3, "4" = 24, "25" = 25, "26" = 26, "27"= 38, "39" = 59, "60"= 60, "61"= 61, "62" = 64, "65" = 86)
  for(row_start in names(rows_merge_1)){
      mergeCells(wb, "Estadísticas", cols = 1:10, rows = (as.integer(row_start)):rows_merge_1[[row_start]]) 
  }
  
  # Poner nombres de columnas alineados, en negrilla, ajustada a la celda y en Times New Roman
  for (row in c(1, 26, 61)){
    cols_name_style_bold(wb, "Estadísticas", start_row = row, end_row = row, start_col = 1, end_col = 10)
  }
  
  # Poner demás información alineada, ajustada a la celda y en Times New Roman
  normal <- setdiff(1:86, c(1, 26, 61))
  for (row in normal){
    data_style(wb, "Estadísticas", start_row = row, end_row = row, start_col = 1, end_col = 10)
  }

  # Asignar ancho de cada columna
  widths <- list("13" = 80)
  set_cols_width(wb, "Estadísticas", widths)
  
  # ***********************************************************************************************************
  # 6) Escribir el README
  # ***********************************************************************************************************
  
  
  writeData(wb, "README", "REPORTE DE VALIDACIÓN DE DATOS", startRow = 1, startCol = 1)
  writeData(wb, "README",
    "En este archivo podrá consultar las inconsistencias encontradas, el tipo de inconsistencia, la descripción de la inconsistencia, el valor que se debe ajustar y la ubicación de este dentro del conjunto de datos para realizar el ajuste. A continuación, encontrará una explicación de cada una de las pestañas y la información que contienen.", startRow = 2, startCol = 1)
  
  writeData(wb, "README", "Pestaña Campos", startRow = 6, startCol = 1)
  writeData(wb, "README", "Esta pestaña corresponde a la validación de la presencia o ausencia de los campos requeridos en el archivo. A continuación se explicarán cada una de las columnas presentes en la pestaña.", startRow = 7, startCol = 1)
  
  writeData(wb, "README", "Columna", startRow = 9, startCol = 1)
  writeData(wb, "README", "Esta primera columna  lista cada uno de los campos que contiene el conjunto de datos, junto con posibles campos faltantes.", startRow = 9, startCol = 3)
  
  writeData(wb, "README", "Existe", startRow = 12, startCol = 1)
  writeData(wb, "README", "En esta segunda columna encontrará los valores: 'Sí' y 'No', indicando la existencia de la columna dentro de su conjunto de datos.", startRow = 12, startCol = 3)
  
  writeData(wb, "README", "Flag", startRow = 15, startCol = 1)
  writeData(wb, "README", "En esta tercera columna encontrará las siguientes advertencias: 'Falta columna requerida', la cual indica que en su conjunto de datos no existe la columna y, aunque no es obligatorio diligenciar los campos de cada registro para esta columna, la columna sí debe aparecer en el conjunto de datos; 'Falta columna obligatoria', la cual indica que la columna, la cual es obligatorio que esté diligenciada para cada registro, pero no existe en su conjunto de datos y 'Columna no requerida (cambie su nombre o elimínela)', la cual indica que su conjunto de datos contiene una columna que no es aceptada por la base de datos de BioModelos, por lo cual debe revisar si tuvo un error al escribir el nombre que pueda corregir o si efectivamente corresponde a una columna diferente a las solicitadas en la plantilla de registros.",
    startRow = 15, startCol = 3)
  
  writeData(wb, "README", "Cantidad de errores", startRow = 24, startCol = 1)
  writeData(wb, "README", "Finalmente, en esta columna encontrará la cantidad de registros que presentaron alguna inconsistencia para cada campo.",  
    startRow = 24, startCol = 3)
  
  writeData(wb, "README", "Pestaña Errores", startRow = 28, startCol = 1)
  writeData(wb, "README", "Esta pestaña consigna los errores encontrados en el conjunto de datos. A continuación se explicarán cada una de las columnas presentes en la pestaña.",  startRow = 29, startCol = 1)
  
  writeData(wb, "README", "Fila", startRow = 32, startCol = 1)
  writeData(wb, "README", "Indica la fila a la que corresponde el registro en el que se detectó la inconsistencia.", 
            startRow = 32, startCol = 3)
  
  
  writeData(wb, "README", "Columna", startRow = 34, startCol = 1)
  writeData(wb, "README", "Indica la columna a la que corresponde el registro en el que se detectó la inconsistencia.", 
            startRow = 34, startCol = 3)
  
  
  writeData(wb, "README", "Tipo", startRow = 36, startCol = 1)
  writeData(wb, "README", "Indica el tipo de inconsistencia presentada en el registro.", startRow = 36, startCol = 3)
  
  writeData(wb, "README", "Mensaje", startRow = 38, startCol = 1)
  writeData(wb, "README", "Realiza una descripción detallada de la inconsistencia presentada en el registro", startRow = 38, 
            startCol = 3)
  
  writeData(wb, "README", "Valor", startRow = 40, startCol = 1)
  writeData(wb, "README", "Muestra el valor actual que presenta la inconsistencia presentada en el registro.", startRow = 40, 
            startCol = 3)
  
  writeData(wb, "README", "Nota: En el campo de 'occurrenceID', cuando este no tiene el formato especificado en la plantilla de registros de BioModelos, se creará automáticamente un identificador. Por lo tanto, en esta pestaña podrá encontrar filas donde se presenté alguna inconsistencia con el campo y se indicará que se creó un identificador automático, de modo que NO deberá realizar cambios.", startRow = 42, startCol = 1)
  
  writeData(wb, "README", "Pestaña Estadísticas", startRow = 47, startCol = 1)
  writeData(wb, "README", "En esta pestaña encontrara algunas gráficas mostrando la cantidad de errores agrupados bajo diferentes criterios. Entre ellos se encuentran cantidad de errores por campo, cantidad de errores por tipo y cantidad de errores por clase.", startRow = 48, startCol = 1)
  
  # Combinar celdas
  # En estas se combinas fila de la columna 1 a 10
  rows_merge_2 <- list("1" = 1, "2" = 4, "5" = 5, "6" = 6, "7" = 8, "27" = 27, "28" = 28, "29" = 31, "42" = 45, "46" = 46, "47" = 47,
                       "48" = 50)
  
  for (start_row in names(rows_merge_2)){
    mergeCells(wb, "README", cols = 1:10, rows = start_row:rows_merge_2[[start_row]])
  }
  
  # En estas se combinan filas de la columna 1 a la 2, y de la 3 a la 10
  rows_merge_3 <-  list("9" = 11, "12" = 14, "15" = 23, "24" = 26, "32" = 33, "34" = 35, "36" = 37, "38" = 39, "40" = 41)
  
  for (start_row in names(rows_merge_3)){
    mergeCells(wb, "README", cols = 1:2, rows = start_row:rows_merge_3[[start_row]])
    mergeCells(wb, "README", cols = 3:10, rows = start_row:rows_merge_3[[start_row]])
  }
  
  # Ajustar el estilo
  bold <- c(1, 6, 28, 47)
  for (i in bold){  
    cols_name_style_bold(wb, "README", i, i, 1, 10)
  }
  
  italics <-  c(9, 12, 15, 24, 32, 34, 36, 38, 40)
  for (i in italics){
    cols_name_style_italics(wb, "README", i, i, 1, 2)
  }
  
  normal <- setdiff(1:54, c(bold, italics))
  for (i in normal){
    data_style(wb, "README", i, i, 1, 7)
  }
  for (i in italics){
    data_style(wb, "README", i, i, 3, 10)
  }
  
  
  # ***********************************************************************************************************
  # 7) Guardar el libro de trabajo
  # ***********************************************************************************************************
  
  # Crear nombre del archivo de reporte
  file_base   <- tools::file_path_sans_ext(basename(file_path))
  report_file <- file.path(output_dir, paste0(file_base, "_reporte_validacion.xlsx"))
  
  saveWorkbook(wb, report_file, overwrite = TRUE)
  
  message("Reporte de validación guardado en: ", report_file)

}


# ########################################################################################################### #
#                                          Ejectutar la validación                                            #
# ########################################################################################################### #

validate <- validate_file(
  file_path = file_path,
  output_dir = output_dir,
  req = req,
  mand = mand,
  basis_of_record = basis_of_record,
  gbif = gbif,
  group = group,
  downloadDate = downloadDate,
  expert = expert
)

errors_stat(file_path, output_dir, validate[[1]], validate[[2]], req)

