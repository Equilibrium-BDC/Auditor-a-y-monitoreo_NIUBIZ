#### Importar datos desde Survey ####

# Asegurarse de que las credenciales necesarias estén disponibles
if (exists("username") && exists("password") && exists("server") && exists("formid")) {
  message("Credenciales de Survey cargadas correctamente.")
} else {
  stop("No se encontraron las credenciales de Survey. Asegúrate de cargarlas desde el script maestro.")
}


data_ejemplo <-read_excel("data/base_niubiz_ejemplo.xlsx")
vars_needed <- colnames(data_ejemplo)
vars_needed <- c(vars_needed,paste("six_",c(1:6)))


# Completar aquí para descargar datos de survey 

# Download de API ----------------------------------------------------------

## Conect to SurveyCTO ----------------------------------------------------------------

API <- paste0('https://',server,'.surveycto.com/api/v2/forms/data/wide/json/',formid,'?date=0')


## Import data -------------------------------------------------------------

max_attempts <- 20
attempt <- 1

repeat {
  # Llamada a la API
  dataset_json <- POST(
    url = API,
    config = authenticate(email, password),
    add_headers("Content-Type: application/json"),
    encode = 'json'
  )
  
  # Convertir JSON a data frame
  data <- jsonlite::fromJSON(rawToChar(dataset_json$content), flatten = TRUE)
  
  # Si df es un data frame válido, salir del ciclo
  if (is.data.frame(data)) break
  
  # Si se alcanzó el número máximo de intentos, lanzar error y salir
  if (attempt >= max_attempts) {
    stop("Se alcanzó el número máximo de intentos sin obtener un data frame válido.")
  }
  
  # Esperar antes de reintentar
  Sys.sleep(300)
  attempt <- attempt + 1
}

# Transformar base de datos ----------------------------------------------------


for (v in vars_needed) {
  if (!(v %in% names(data))) {
    data[[v]] <- rep(NA, nrow(data))
  }
}

# Organizar variables

# Reordenar y dejar las demás al final
otras_vars <- setdiff(names(data), vars_needed)
data <- data[ , c(vars_needed, otras_vars)]


## Importar datos sin API
#
#data <- read_excel("data/Estudio de uso y experiencia de soluciones de pago 2025_WIDE.xlsx")
#
## Filtrar piloto
#
## Convertir a fecha y hora
#data <- data %>%
#  mutate(
#    endtime = ymd_hms(endtime),  # convierte a POSIXct
#    endtime = with_tz(endtime, tzone = "America/Lima"),
#    starttime = ymd_hms(starttime),
#    starttime = with_tz(starttime, zone = "America/Lima")
#  ) %>%
#  filter(KEY != "uuid:37137e5e-5b70-4aa3-9a0f-7558f365b44e")%>%
#  filter(endtime >= mdy("07-24-25"))

# Compilar variables multiples


# Nombres de las variables de opción múltiple
multi_vars <- c(
  "GIRO", "MP_USO", "TOM_SP",
  "TM_MEDIOS_Niubiz", "TM_MEDIOS_Izipay", "TM_MEDIOS_Culqi",
  "TM_MEDIOS_Yape", "TM_MEDIOS_Plin", "TM_MEDIOS_Otro",
  "TM_SP_A", "TM_USO", "TM_USO_A", "MOT_PREF", "NO_USO_NIUBIZ",
  "MOT_RECHAZO", "six", "NIUBIZ_TIP_PROB_EXP", "IZIPAY_TIP_PROB_EXP",
  "CULQI_TIP_PROB_EXP", "NIUBIZ_INCO_ABO", "IZIPAY_INCO_ABO",
  "CULQI_INCO_ABO", "REP_ABO", "ATRIB_CAMBIO_MP", "USO_BANCOS",
  "MOT_NOUSO", "TIPO_MP", "MED_COM"
)

for (var in multi_vars) {
  var_cols <- names(data)[startsWith(names(data), paste0(var, "_")) & !grepl("_o$", names(data))]
  
  if (length(var_cols) > 0) {
    data <- data %>%
      rowwise() %>%
      mutate(!!var := {
        vals <- c_across(all_of(var_cols))
        
        if (all(is.na(vals))) {
          NA_character_
        } else {
          activos <- which(vals == 1)
          if (length(activos) == 0) NA_character_ else {
            seleccionados <- gsub(paste0("^", var, "_"), "", var_cols[activos])
            paste(seleccionados, collapse = ",")
          }
        }
      }) %>%
      ungroup()
  }
}
