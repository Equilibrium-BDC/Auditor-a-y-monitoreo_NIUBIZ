# Limpieza base de datos
rm(list=ls())
pacman::p_load(dplyr,tidyr,googlesheets4)

# Cargar datos crudos
datos_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1BQVBHXKOFuT0kuC-aD-2hqgk1b9X0XxWYN_xsqZ36v8/edit?gid=1021927750#gid=1021927750",
                        sheet = "data_raw")


### Correcciones recontacto -------------------------------------------------------------------------

# Leer la matriz de correcciones desde Google Sheets
matriz_correcciones <- read_sheet("https://docs.google.com/spreadsheets/d/1BQVBHXKOFuT0kuC-aD-2hqgk1b9X0XxWYN_xsqZ36v8/edit?gid=1021927750#gid=1021927750",
                                  sheet = "correcciones")


# Seleccionar solo las columnas necesarias y renombrarlas
matriz_correcciones <- matriz_correcciones %>%
  select(id, nombres_variables,`respuesta corregida`) %>%
  rename(codigo_pregunta = nombres_variables, respuesta_corregida = `respuesta corregida`)

# Recorre cada fila de la matriz de correcciones y aplica la corrección en la base de datos
for (i in 1:nrow(matriz_correcciones)) {
  # Extrae el ID, la pregunta a corregir y el valor corregido
  id_encuestado <- matriz_correcciones$id[i]
  pregunta <- matriz_correcciones$codigo_pregunta[i]
  valor_corregido <- matriz_correcciones$respuesta_corregida[i]
  
  # Sobrescribe el valor en 'data' solo en la fila y columna especificadas
  datos_raw[datos_raw$id == id_encuestado, pregunta] <- valor_corregido
}

### Correcciones salarios rango

data_clean <- datos_raw %>%
  mutate(a19_ingresos = case_when(
    a20_ingresos_monto < 500000 ~ 1,
    a20_ingresos_monto >= 500000 & a20_ingresos_monto < 1000000 ~ 2,
    a20_ingresos_monto >= 1000000 & a20_ingresos_monto < 1500000 ~ 3,
    a20_ingresos_monto >= 1500000 & a20_ingresos_monto < 2000000 ~ 4,
    a20_ingresos_monto >= 2000000 & a20_ingresos_monto < 2500000 ~ 5,
    a20_ingresos_monto >= 2500000 & a20_ingresos_monto < 3000000 ~ 6,
    a20_ingresos_monto >= 3000000 & a20_ingresos_monto < 3500000 ~ 7,
    a20_ingresos_monto >= 3500000 ~ 8,
    TRUE ~ NA
  ),
  b7_ingresos_propios = case_when(
    b7_ingresos_propios_monto < 500000 ~ 1,
    b7_ingresos_propios_monto >= 500000 & b7_ingresos_propios_monto < 1000000 ~ 2,
    b7_ingresos_propios_monto >= 1000000 & b7_ingresos_propios_monto < 1500000 ~ 3,
    b7_ingresos_propios_monto >= 1500000 & b7_ingresos_propios_monto < 2000000 ~ 4,
    b7_ingresos_propios_monto >= 2000000 & b7_ingresos_propios_monto < 2500000 ~ 5,
    b7_ingresos_propios_monto >= 2500000 & b7_ingresos_propios_monto < 3000000 ~ 6,
    b7_ingresos_propios_monto >= 3000000 & b7_ingresos_propios_monto < 3500000 ~ 7,
    b7_ingresos_propios_monto >= 3500000 ~ 8,
    TRUE ~ NA
  ))
library(stringr) 

data_clean <- data_clean %>%
  mutate(contact_nombre = str_to_sentence(contact_nombre))


## Exportar base sin etiquetas


writexl::write_xlsx(data_clean,path = "G:/Mi unidad/KIVA/auditoría_monitoreo/KIVA_local/data/data_kiva_clean_nolab.xlsx")



