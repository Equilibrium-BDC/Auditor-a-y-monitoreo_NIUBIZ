#### Script NIUBIZ --------------------------------------------------------------
# Proyecto: NIUBIZ
"Este script crea las alertas de auditoría y exporta los resultados a unos
google sheets que alimentan un looker"

# Autor: Julián López - Equilibrium SDC

rm(list = ls())

### Script Maestro 

# Instalar y cargar paquetes necesarios
install.packages("tibble")
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(
  dplyr,
  tidyr,
  httr,
  jsonlite,
  googledrive,
  googlesheets4,
  writexl,
  haven,
  stringr,
  labelled,
  lubridate,
  gtsummary,
  dotenv,
  lubridate,
  readxl,
  beepr
)

# Configurar el directorio base del proyecto
project_path <- getwd()  # Ruta del proyecto en GitHub Actions
message("Directorio base: ", project_path)


# Configurar correo

R_user <- Sys.info()[["user"]]

if(R_user == "julia") {
  email <- "jlopez@equilibriumbdc.com"
} else {
  email <- "zlimachi@equilibriumbdc.com"
}


### Importar datos ####


source(file.path(project_path, "/scripts/Import_data_NIUBIZ.R"))


### Crear alertas ####


source(file.path(project_path, "/scripts/Alertas_NIUBIZ.R"))

### Data comparativo 2024 ####


source(file.path(project_path, "/scripts/Comparativo_data_NIUBIZ.R"))


#### Exportar datos a google sheets ####



source(file.path(getwd(), "/scripts/Export_data_NIUBIZ.R"))

# Cambio git
