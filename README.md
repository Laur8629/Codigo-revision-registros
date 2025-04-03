# Código para Validación de Registros de BioModelos

Este repositorio contiene herramientas para la validación y revisión de registros de especies en BioModelos, facilitando la identificación y corrección de posibles inconsistencias en los datos.

## Contenido del Repositorio

- **`codigo_validar_registros.R`**: Script en R que automatiza la validación de registros de especies, verificando la consistencia y calidad de los datos. Este script realiza las siguientes funciones:

  - **Carga de Datos**: Importa los registros desde un archivo CSV especificado por el usuario.
  - **Validación de Campos**: Verifica la presencia y el formato correcto de los campos requeridos, como nombres científicos, coordenadas geográficas y fechas de recolección.
  - **Detección de Duplicados**: Identifica registros duplicados basados en criterios como coordenadas y fecha de recolección.
  - **Verificación de Coordenadas**: Comprueba que las coordenadas geográficas estén dentro de los límites permitidos y no correspondan a ubicaciones improbables.
  - **Generación de Informe**: Produce un informe detallado en formato CSV con los hallazgos de la validación, indicando los registros que requieren corrección.

- **`Plantilla_campos_registros_BioModelos.xlsx`**: Plantilla en Excel que define los campos requeridos y su formato para los registros de especies en BioModelos. Se recomienda usar esta plantilla para estructurar los datos antes de la validación.

## Requisitos

- **R**: Se requiere tener instalado R para ejecutar el script de validación. Puedes descargarlo desde [CRAN](https://cran.r-project.org/).

- **Paquetes de R**: Asegúrate de tener instalados los siguientes paquetes necesarios para la ejecución del script:

  ```r
  install.packages(c("dplyr", "readr", "lubridate"))
  ```

## Uso

1. **Preparación de Datos**:  
   - Descarga y usa la plantilla `Plantilla_campos_registros_BioModelos.xlsx`.  
   - Completa los datos asegurándote de seguir el formato especificado.  
   - Guarda el archivo en **formato CSV (delimitado por comas, `.csv`)** para que el script pueda leerlo correctamente.  

2. **Ejecución del Script**:  
   - Abre R o RStudio.  
   - Establece el directorio de trabajo al que contiene el script `codigo_validar_registros.R` y el archivo CSV con los datos.  
   - Ejecuta el script `codigo_validar_registros.R`. Durante la ejecución, se te solicitará que ingreses el nombre del archivo CSV que contiene los registros a validar.  

3. **Revisión de Resultados**:  
   - Se generará un informe en formato CSV con los registros que presenten errores y las observaciones correspondientes.  
   - Corrige los errores en la plantilla original y vuelve a ejecutar el script si es necesario.  

## Posibles Errores y Soluciones

Al ejecutar el script, podrían aparecer algunos errores comunes. A continuación, se listan algunos problemas y cómo solucionarlos:

| Error | Posible Causa | Solución |
|-------|-------------|---------|
| `Error in read_csv(): cannot open file` | El archivo CSV no está en el mismo directorio que el script o el nombre del archivo es incorrecto. | Verifica que el archivo CSV esté en el mismo directorio y que el nombre ingresado sea exacto. |
| `Error: Missing columns` | El archivo CSV no tiene los encabezados esperados. | Asegúrate de que el archivo provenga de la plantilla `Plantilla_campos_registros_BioModelos.xlsx`. |
| `Error in mutate(): object not found` | Algunas columnas esenciales pueden estar vacías o con nombres incorrectos. | Usa la plantilla de Excel correctamente y revisa que los nombres de las columnas sean los correctos. |
| `Error: unexpected symbol` | Problema con caracteres especiales en el CSV. | Guarda el archivo en formato **CSV UTF-8** y evita caracteres especiales. |
| `Coordinates outside valid range` | Algunas coordenadas están fuera del rango permitido. | Revisa que los valores de latitud y longitud sean correctos. |


## Contacto

Para cualquier consulta o sugerencia, por favor contacta a biomodelos@humboldt.org.co.

