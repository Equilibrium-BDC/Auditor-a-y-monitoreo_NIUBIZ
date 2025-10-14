# Ajustes base de datos --------------------------------------------------------

## Importar datos y ajustar errores de campo -----------------------------------

data_1 <- data

data_sin_transformar <- data_1

data <- data_sin_transformar

# Eliminar encuestas de prueba

data <- data %>%
  filter(!username %in% c("encuestasmarketshare@equilibriumbdc.com",
                           "anonymousUser","pablo.rodriguez"))

# Filtrar encuestas no válidas

data_no_valido <- data %>%
  mutate(part_valido = if_else((as.numeric(F1) == 1 | as.numeric(F2) == 1) &
                                 as.numeric(F3) == 1, 1, 0)) %>%
  filter(part_valido == 0)

no_validas <- nrow(data_no_valido)

# Filtrar válidas

data <- data %>%
  mutate(part_valido = if_else((as.numeric(F1) == 1 | as.numeric(F2) == 1) &
    as.numeric(F3) == 1, 1, 0)) %>%
  filter(part_valido == 1) %>%
  mutate(duration_minutes = round(as.numeric(duration) / 60, 2),
         n_no_validas = no_validas)

## Correcciones de campo -------------------------------------------------------

# Eliminar encuestas dudosas

# Leer la hoja llamada "alertas
grabaciones_no_validas <- read_sheet(id_keys, sheet = "KEYS")%>%pull(KEY)

data <- data %>%
  filter(!KEY %in% grabaciones_no_validas)


# Eliminar empresa

data <- data %>%
  filter(as.numeric(ruc) != 20608280333)

# Corregir RUC

data <- data %>%
  mutate(
    raz_social = trimws(raz_social),
    raz_social = case_when(
      username == "michina.011@gmail.com" & raz_social == "Impresiónes copias artículos librería y agente banco nación y Interbank" ~ "Multiservicios Géminis",
      username == "michina.011@gmail.com" & raz_social == "Dubraska" ~ "Lavanderías Madrid",
      TRUE ~ raz_social
    ),
    ruc = case_when(
      username == "michina.011@gmail.com" & raz_social == "Multiservicios Géminis" ~ "20608613243",
      username == "michina.011@gmail.com" & raz_social == "Lavanderías Madrid" ~ "20547813007" ,
      TRUE ~ ruc
    )
    
  )

data <- data %>%
  mutate(
    username = if_else(trimws(username)=="johannace02@gmail.com (not yet authenticated)",
                       "johannace02@gmail.com",username)
  )

# Corregir información faltante

data <- data %>%
  mutate(
    EDAD = if_else(is.na(EDAD) & KEY == "uuid:fefa9f87-b9bb-4405-ac34-b1007940c510", "3", EDAD),
    GEN = if_else(is.na(GEN) & KEY == "uuid:fefa9f87-b9bb-4405-ac34-b1007940c510", "2", GEN)
  )


# Eliminar duplicado

data <- data %>%
  filter(
    !KEY %in% c("uuid:a4ea83e3-ee8e-46b5-b905-e9e538624d5b", "uuid:3ade56a5-9281-4f4d-8bb1-ba7329146a2d")
  )

#Eliminar encuestas falsas

encuestas_falsas_daniel <- data %>%
  filter(username == "danielviuri@gmail.com" &  mdy_hms(starttime) > ymd_hms("2025-08-11 23:59:59"))%>%
  pull(KEY)


data <- data %>%
  filter(!KEY %in% encuestas_falsas_daniel)

# Corregir RUC

data <- data %>%
  mutate(
    raz_social = if_else(KEY == "uuid:32359b96-5164-47d5-916d-9fd146699a65","Jessica Rivera Huaman (Cafetería Fresita)",raz_social),
    ruc = if_else(KEY == "uuid:32359b96-5164-47d5-916d-9fd146699a65","Jessica Rivera Huaman (Cafetería Fresita)",ruc)
  )

data <- data %>%
  mutate(
    ruc = case_when(KEY == "uuid:feca5852-42fe-48ac-a4fe-5cfbd411ea4c" ~ "20607540676",
                    KEY == "uuid:0be501bd-bcde-4cd5-ba31-58e9b05815e8" ~ "20607274429",
                    KEY == "uuid:98ab2209-f08b-4613-b3c9-ddb93ef9a742" ~ "20610609016",
                    TRUE ~ ruc)
  )

data <- data %>%
  mutate(
    raz_social = case_when(KEY == "uuid:c4817849-d589-429d-b5e7-c1a5dfebe226" ~ "Anthony Ruiz Yupanqui",
                           KEY == "uuid:26a50b9d-377c-4a47-883d-63de728be53d" ~ "Lizarzaburo Tejeda Roberto Carlos",
                           KEY == "uuid:b7681de5-37a0-4e2f-a2d0-af04301f9000" ~ "Evangelista Tito Geraldine Mirella",
                            KEY == "uuid:89c645f0-3072-44e3-8b35-fbcad80e62cf" ~ "Unsihuay Ponce Jhon Quenide",
                            TRUE ~ raz_social)
  )
data <- data %>%
  mutate(
    ruc = case_when(KEY == "uuid:c4817849-d589-429d-b5e7-c1a5dfebe226" ~ "10713597487",
                           KEY == "uuid:26a50b9d-377c-4a47-883d-63de728be53d" ~ "10423601928",
                           KEY == "uuid:b7681de5-37a0-4e2f-a2d0-af04301f9000" ~ "10753509751",
                           KEY == "uuid:89c645f0-3072-44e3-8b35-fbcad80e62cf" ~ "10460744640",
                           TRUE ~ ruc)
  )

data <- data %>%
  mutate(
    ruc = case_when(KEY == "uuid:febf8e7e-1391-4744-937f-07ab81a8fb11" ~ "10483201911",
                    KEY == "uuid:849ab4bf-8981-4515-9d64-d8002dcfd8ff" ~ "15607626887",
                    KEY == "uuid:3102e251-db5d-4263-bb96-615cb4653cbf" ~ "10485374073",
                    KEY == "uuid:9d2af2c1-8a31-41cf-b5e8-9e11972dde75" ~ "20609487471",
                    KEY == "uuid:44119bca-156b-4406-9c4f-e16a0d332eb2" ~ "10422511658",
                    KEY == "uuid:0fdf8708-d78c-4237-a7e5-c6afe2e60e65" ~ "10413801236", 
                    KEY == "uuid:e62abf8e-31c0-4ace-94ac-691784cb4709" ~ "10436103749", 
                    KEY == "uuid:b8e34377-5848-4028-a9b5-e5401b1fb8f4" ~ "20609121671", 
                    KEY == "uuid:6afc3103-6542-475b-a5f5-2a3dc2a3be29" ~ "10421220692", 
                    KEY == "uuid:42870139-8c56-4948-bedf-9486033122c2" ~ "20613675401", 
                    KEY == "uuid:4a60ae18-0af5-419b-b627-ce848f5844a5" ~ "10411480696", 
                    TRUE ~ ruc)
  )

data <- data %>%
  mutate(
    raz_social = case_when(KEY == "uuid:febf8e7e-1391-4744-937f-07ab81a8fb11" ~ "VALDEZ SANCHEZ MAYRA CAMILA",
                           KEY == "uuid:849ab4bf-8981-4515-9d64-d8002dcfd8ff" ~ "MORENO PINTO ALFREDO ALEJANDRO ",
                           KEY == "uuid:3102e251-db5d-4263-bb96-615cb4653cbf" ~ "MARCOS POLO LEYDIS ESMILDA",
                           KEY == "uuid:9d2af2c1-8a31-41cf-b5e8-9e11972dde75" ~ "Pizzabela Pizzas y Pastas S.A.C.S.",
                           KEY == "uuid:44119bca-156b-4406-9c4f-e16a0d332eb2" ~ "Flores Delgado Gilmer",
                           KEY == "uuid:44119bca-156b-4406-9c4f-e16a0d332eb2" ~ "ALARCON APONTE NANCY DOMINGA",
                           KEY == "uuid:e62abf8e-31c0-4ace-94ac-691784cb4709" ~ "James Donovan Izaguirre Talledo", 
                           KEY == "uuid:b8e34377-5848-4028-a9b5-e5401b1fb8f4" ~ "Top Vision Opticas E.I.R.L.", 
                           KEY == "uuid:6afc3103-6542-475b-a5f5-2a3dc2a3be29" ~ "VALENZUELA CABANILLAS Edelicia", 
                           KEY == "uuid:42870139-8c56-4948-bedf-9486033122c2" ~ "Ralah SAC", 
                           KEY == "uuid:4a60ae18-0af5-419b-b627-ce848f5844a5" ~ "Vanessa Limas Ortiz", 
                           TRUE ~ raz_social)
  )


"uuid:e62abf8e-31c0-4ace-94ac-691784cb4709"

# Corregir RUC

data <- data %>%
  mutate(
    ruc = if_else(KEY == "uuid:749a062a-781d-4eac-a733-421a606423cb", "10070856561",ruc)
  )


data <- data %>%
  mutate(
    ruc = if_else(KEY == "uuid:deaae751-beee-41a3-a979-a6aa7b640c97", "20600315324",ruc)
  )

## Ajustar información geográfica ----------------------------------------------

# Separar coordenadas

data <- data %>%
  mutate(
    coords = trimws(coords),          # quita espacios al inicio/fin
    coords = na_if(coords, "")        # convierte cadenas vacías en NA
  ) %>%
  separate(
    coords,
    into = c("lat", "lon", "alt", "acc"),
    sep = "\\s+",                     # separa por uno o más espacios
    fill = "right",                   # si faltan valores, rellena con NA a la derecha
    extra = "drop",                   # si vienen más de 4, descarta el resto
    convert = TRUE                    # convierte a numérico automáticamente
  )


data <- data %>%
  mutate(
    lat_lon = paste0(lat,",",lon)
  )

data <- data %>%
  mutate(
    coords_final = trimws(coords_final),          # quita espacios al inicio/fin
    coords_final = na_if(coords_final, "")        # convierte cadenas vacías en NA
  ) %>%
  separate(
    coords_final,
    into = c("lat_final", "lon_final", "alt_final", "acc_final"),
    sep = "\\s+",                     # separa por uno o más espacios
    fill = "right",                   # si faltan valores, rellena con NA a la derecha
    extra = "drop",                   # si vienen más de 4, descarta el resto
    convert = TRUE                    # convierte a numérico automáticamente
  )

data <- data %>%
  mutate(
    lat_lon_final = paste0(lat_final,",",lon_final)
  )

## Corrección zona horaria e identificar rondas --------------------------------

data <- data %>%
  mutate(
    # Primero, parsea la fecha y hora si no lo están,
    starttime = mdy_hms(starttime, tz = "UTC", locale = "C"),
    endtime = mdy_hms(endtime, tz = "UTC", locale = "C"),
    SubmissionDate = mdy_hms(SubmissionDate, tz = "UTC", locale = "C"),
    starttime = with_tz(starttime, tzone = "America/Lima"),
    endtime = with_tz(endtime, tzone = "America/Lima"),
    SubmissionDate = with_tz(SubmissionDate, tzone = "America/Lima"))
    
      
# Crear identificador ronda 

# 2) Definir umbrales en la MISMA zona
limite_r1 <- dmy_hms("7-09-2025 12:00:00", tz = "America/Lima")
limite_r2 <- dmy_hms("8-09-2025 08:00:00", tz = "America/Lima")

# 3) Identificador de ronda (tipado entero y NA explícito)
data <- data %>%
  mutate(
    ronda = case_when(
      starttime <= limite_r1 ~ 1L,
      starttime >= limite_r2 ~ 2L,
      TRUE                   ~ NA_integer_
    )
  )
## Análisis de cuotas ----------------------------------------------------------

# Categoría empresa

data <- data %>%
  mutate(
    tamanio_empleados = case_when(
      NRO_EMP %in% as.character(c(1:3)) ~ "Micro",
      NRO_EMP %in% as.character(c(4:5)) ~ "Pequeña",
      NRO_EMP %in% as.character(c(6:7)) ~ "Mediana",
      NRO_EMP %in% as.character(c(8:10)) ~ "Grande",
    ),
    tamanio_ingresos = case_when(
      VOL_MEN %in% as.character(c(1:2)) ~ "Micro",
      VOL_MEN %in% as.character(c(3:6)) ~ "Pequeña",
      VOL_MEN %in% as.character(c(7:11)) ~ "Mediana",
    )
  )

# Nombrar departamentos 

# Departamento
data <- data %>%
  mutate(
    DEP_str = case_when(
      DEP == "1" ~ "Lima",
      DEP == "2" ~ "Callao",
      DEP == "3" ~ "Cusco",
      DEP == "4" ~ "Arequipa",
      DEP == "5" ~ "Trujillo",
      DEP == "6" ~ "Piura",
      TRUE ~ NA_character_
    )
  )

# Cuotas levantamiento 1

cuotas_1 <- tribble(
  ~Regiones,    ~Categoria,   ~Cuota,
  "Lima",       "Micro",     280,
  "Lima",       "Pequeña",    88,
  "Lima",       "Mediana",    32,
  "Lima",       "Meta",      400,
  "Callao",     "Micro",      35,
  "Callao",     "Pequeña",    11,
  "Callao",     "Mediana",     4,
  "Callao",     "Meta",       50,
  "Arequipa",   "Micro",      35,
  "Arequipa",   "Pequeña",    11,
  "Arequipa",   "Mediana",     4,
  "Arequipa",   "Meta",       50,
  "Cusco",      "Micro",      35,
  "Cusco",      "Pequeña",    11,
  "Cusco",      "Mediana",     4,
  "Cusco",      "Meta",       50,
  "Trujillo",   "Micro",      35,
  "Trujillo",   "Pequeña",    11,
  "Trujillo",   "Mediana",     4,
  "Trujillo",   "Meta",       50,
  "Piura",      "Micro",      35,
  "Piura",      "Pequeña",    11,
  "Piura",      "Mediana",     4,
  "Piura",      "Meta",       50
)

# Añadir ronda 

cuotas_1["ronda"] <- 1


# Cuotas segunda ronda

cuotas_2 <- tribble(
  ~Regiones,    ~Categoria,   ~Cuota_2,
  "Lima",       "Micro",     346,
  "Lima",       "Pequeña",    88,
  "Lima",       "Mediana",    42,
  "Lima",       "Meta",      476,
  "Callao",     "Micro",      35,
  "Callao",     "Pequeña",    11,
  "Callao",     "Mediana",     4,
  "Callao",     "Meta",       50,
  "Arequipa",   "Micro",      50,
  "Arequipa",   "Pequeña",    11,
  "Arequipa",   "Mediana",     4,
  "Arequipa",   "Meta",       65,
  "Cusco",      "Micro",      47,
  "Cusco",      "Pequeña",    11,
  "Cusco",      "Mediana",     6,
  "Cusco",      "Meta",       64,
  "Trujillo",   "Micro",      35,
  "Trujillo",   "Pequeña",    11,
  "Trujillo",   "Mediana",     4,
  "Trujillo",   "Meta",       50,
  "Piura",      "Micro",      45,
  "Piura",      "Pequeña",    11,
  "Piura",      "Mediana",     8,
  "Piura",      "Meta",       64
)

cuotas_3 <- tribble(
  ~Regiones,    ~Categoria,   ~Cuota_3,
  "Lima",       "Micro",     302,
  "Lima",       "Pequeña",    95,
  "Lima",       "Mediana",    34,
  "Lima",       "Meta",      431,
  "Callao",     "Micro",      38,
  "Callao",     "Pequeña",    12,
  "Callao",     "Mediana",    4,
  "Callao",     "Meta",       54,
  "Arequipa",   "Micro",      38,
  "Arequipa",   "Pequeña",    12,
  "Arequipa",   "Mediana",     4,
  "Arequipa",   "Meta",       54,
  "Cusco",      "Micro",      38,
  "Cusco",      "Pequeña",    12,
  "Cusco",      "Mediana",     4,
  "Cusco",      "Meta",       54,
  "Trujillo",   "Micro",      38,
  "Trujillo",   "Pequeña",    12,
  "Trujillo",   "Mediana",     4,
  "Trujillo",   "Meta",       54,
  "Piura",      "Micro",      38,
  "Piura",      "Pequeña",    12,
  "Piura",      "Mediana",     4,
  "Piura",      "Meta",       54
)

# Añadir ronda

cuotas_2["ronda"] <- 2
cuotas_3["ronda"] <- 3

# Añadir información de cuotas

# Unimos por Región y tamaño de ingresos
data <- data %>%
  left_join(
    cuotas_1,
    by = c("DEP_str" = "Regiones", 
           "tamanio_ingresos" = "Categoria",
           "ronda" = "ronda")
  )

# Analizar cuotas válidas

# Este apartado analiza las cuotas válidas de la ronda 1 y asigna las encuestas
# que excedieron las cuotas para ser parte de la ronda 2

data <- data %>%
  arrange(DEP_str, tamanio_ingresos, endtime) %>%  # orden por prioridad
  group_by(DEP_str, tamanio_ingresos, ronda) %>%
  mutate(n_en_segmento = row_number(),
         cuota_valida_1 = case_when(!is.na(ronda) & 
           n_en_segmento <= Cuota & ronda == 1 ~ "Válida",
           ronda == 2 ~ NA_character_,
           TRUE ~ "Exceso"
         ),
         ronda = if_else(cuota_valida_1 == "Exceso" & ronda == 1,2,ronda))%>% # Los excesos de ronda 1 pasan a ronda 2
  ungroup()%>%
  left_join(cuotas_2, by = c("DEP_str" = "Regiones", 
                             "tamanio_ingresos" = "Categoria",
                             "ronda" = "ronda"))%>%
  group_by(DEP_str, tamanio_ingresos, ronda)%>%
  mutate(n_en_segmento = row_number(),
         cuota_valida_2 = case_when(!is.na(Cuota_2) &
           n_en_segmento <= Cuota_2 & ronda == 2 ~ "Válida",
           ronda == 1 ~ NA_character_,
           TRUE ~ "Exceso"
         ))%>%
  ungroup()%>%
  mutate(cuota_valida_total = if_else(cuota_valida_1 == "Válida" | 
                                        cuota_valida_2 == "Válida","Válida","Exceso"))

           
## Filtrar sólo ronda 2 para levantar alertas ----------------------------------

data_ronda_1 <- data %>% filter(ronda == 1 & ruc != "99999999999")
data_ronda_2 <- data %>% filter(ronda == 2)

alertas <- data %>% filter(ronda == 2)


# Alertas ----------------------------------------------------------------------

## Flag duración ---------------------------------------------------------------

alertas <- alertas %>%
  mutate(flag_duration_menos = if_else(is.na(cuota_valida_1), duration_minutes < 10,
                                       0))

## Validación de saltos --------------------------------------------------------

empresas <- c("Niubiz", "Izipay", "Culqi", "Yape", "Plin", "Otro")
tom_codes <- c(1, 2, 3, 4, 5, 66)

alertas <- alertas %>%
  mutate(s_TOM_SP_o = if_else(!is.na(TOM_SP_o) & TOM_SP_66 != 1, 1, 0))

for (i in seq_along(empresas)) {
  empresa <- empresas[i]
  code <- tom_codes[i]

  var1 <- paste0("TM_MEDIOS_", empresa)
  var2 <- paste0("TM_MEDIOS_", empresa, "_o")
  var66 <- paste0("TM_MEDIOS_", empresa, "_66")
  nueva1 <- paste0("s_TM_MEDIOS_", empresa)
  nueva2 <- paste0("s_TM_MEDIOS_", empresa, "_o")
  tom_var <- paste0("TOM_SP_", code)

  alertas <- alertas %>%
    mutate(
      !!nueva1 := if_else(!is.na(.data[[var1]]) & .data[[tom_var]] != 1, 1, 0),
      !!nueva2 := if_else(!is.na(.data[[var2]]) & .data[[var66]] != 1, 1, 0)
    )
}

alertas <- alertas %>%
  mutate(
    s_TM_USO_o = if_else(!is.na(TM_USO_o) & TM_USO_66 != 1, 1, 0),
    s_TM_USO_A_o = if_else(!is.na(TM_USO_A_o) & TM_USO_A_66 != 1, 1, 0),
    s_MOT_PREF_o = if_else(!is.na(MOT_PREF_o) & MOT_PREF_66 != 1, 1, 0)
  )

empresas_acro <- c("NB", "IZI", "CQ", "OP", "MPAGO", "YAPE", "PLIN")
tm_uso <- c(1:7)

for (i in seq_along(empresas_acro)) {
  empresa <- empresas_acro[i]
  tm_num <- tm_uso[i]

  var_1 <- paste0("NPS_", empresa)
  var_2 <- paste0("TM_USO_", tm_num)
  new_var <- paste0("s_", var_1)

  alertas <- alertas %>%
    mutate(
      !!new_var := if_else(!is.na(.data[[var_1]]) & .data[[var_2]] != 1, 1, 0)
    )
}

alertas <- alertas %>%
  mutate(
    s_NO_USO_NIUBIZ = if_else(!is.na(NO_USO_NIUBIZ_o) & TM_USO_1 != 1 & TM_USO_A_1 == 1, 1, 0),
    s_NO_USO_NIUBIZ_o = if_else(!is.na(NO_USO_NIUBIZ_o) & NO_USO_NIUBIZ_66 != 1, 1, 0)
  )

empresas_2 <- c("niubiz", "izipay")
num_empresas <- c(1:2)

for (i in seq_along(empresas_2)) {
  empresa <- empresas_2[i]
  num <- num_empresas[i]

  var1 <- paste0("DIF_", empresa)
  var2 <- paste0("DIF_A_", empresa)
  var3 <- paste0("REL_", empresa)
  var4 <- paste0("FAM_", empresa)
  var5 <- paste0("IMA_", empresa)
  tm_var <- paste0("TM_USO_", num)
  nvar1 <- paste0("s_DIF_", empresa)
  nvar2 <- paste0("s_DIF_A_", empresa)
  nvar3 <- paste0("s_REL_", empresa)
  nvar4 <- paste0("s_FAM_", empresa)
  nvar5 <- paste0("s_IMA", empresa)

  alertas <- alertas %>%
    mutate(
      !!nvar1 := if_else(!is.na(.data[[var1]]) & .data[[tm_var]] != 1, 1, 0),
      !!nvar2 := if_else(!is.na(.data[[var2]]) & .data[[tm_var]] != 1, 1, 0),
      !!nvar3 := if_else(!is.na(.data[[var3]]) & .data[[tm_var]] != 1, 1, 0),
      !!nvar4 := if_else(!is.na(.data[[var4]]) & .data[[tm_var]] != 1, 1, 0),
      !!nvar5 := if_else(!is.na(.data[[var5]]) & .data[[tm_var]] != 1, 1, 0)
    )
}

# Valoración atributos


empresas_3 <- c("Niubiz", "Izipay", "Culqi", "Openpay")
num_empresas <- c(1:3,8)

for (i in seq_along(empresas_3)) {
  empresa <- empresas_3[i]
  num <- num_empresas[i]

  tm_var <- paste0("TM_USO_", num)

  for (j in 1:17) {
    at_var <- paste0(empresa, "_At_", sprintf("%02d", j))
    s_var <- paste0("s_", at_var)

    alertas <- alertas %>%
      mutate(
        !!s_var := if_else(!is.na(.data[[at_var]]) & .data[[tm_var]] != 1, 1, 0)
      )
  }
}


# Variables versus

alertas <- alertas %>%
  mutate(
    filtro_comisiones_1 = as.integer(if_any(all_of(paste0("TM_SP_A_", 1:4)), ~ . == 1)),
    filtro_comisiones_2 = as.integer(if_any(all_of(paste0("TM_USO_", 1:4)), ~ . == 1))
  )

num_empresas <- c(1:4)

for (i in seq_along(str_to_upper(empresas_3))) {
  empresa <- str_to_upper(empresas_3)[i]
  num <- num_empresas[i]

  var1 <- paste0("VERSUS_PRODUCTO_", empresa)
  nvar1 <- paste0("s_", var1)
  var2 <- paste0("VERSUS_PRECIO_", empresa)
  nvar2 <- paste0("s_", var2)
  var3 <- paste0("VERSUS_EXP_", empresa)
  nvar3 <- paste0("s_", empresa)
  var4 <- paste0("VERSUS_ABONO_", empresa)
  nvar4 <- paste0("s_", var4)
  var5 <- paste0("VERSUS_SEGURIDAD_", empresa)
  nvar5 <- paste0("s_", var5)
  tm_var1 <- paste0("TM_SP_A_", num)
  tm_var2 <- paste0("TM_USO_", num)

  alertas <- alertas %>%
    mutate(
      !!nvar1 := if_else(!is.na(.data[[var1]]) & .data[[tm_var1]] != 1 & filtro_comisiones_1 != 1, 1, 0),
      !!nvar2 := if_else(!is.na(.data[[var2]]) & .data[[tm_var2]] != 1 & filtro_comisiones_2 != 1, 1, 0),
      !!nvar3 := if_else(!is.na(.data[[var3]]) & .data[[tm_var2]] != 1 & filtro_comisiones_2 != 1, 1, 0),
      !!nvar4 := if_else(!is.na(.data[[var4]]) & .data[[tm_var2]] != 1, 1, 0),
      !!nvar5 := if_else(!is.na(.data[[var5]]) & .data[[tm_var2]] != 1, 1, 0)
    )
}


# Experiencia de uso


empresas_4 <- c("NIUBIZ", "IZIPAY", "CULQI")
uso <- c(1:3)

for (i in seq_along(empresas_4)) {
  empresa <- empresas_4[i]
  num <- uso[i]
  var1 <- paste0(empresa, "_PROB_EXP")
  nvar1 <- paste0("s_", var1)
  var2 <- paste0(empresa, "_FREC_PROB_USO")
  nvar2 <- paste0("s_", var2)
  var3 <- paste0(empresa, "_FREC_PROB_USO_o")
  nvar3 <- paste0("s_", var3)
  var4 <- paste0(empresa, "_TIP_PROB_EXP")
  nvar4 <- paste0("s_", var4)
  var5 <- paste0(empresa, "_TIP_PROB_EXP_o")
  nvar5 <- paste0("s_", var5)
  var6 <- paste0(empresa, "_PROM_USO")
  nvar6 <- paste0("s_", var6)
  var_tm <- paste0("TM_USO_", num)

  alertas <- alertas %>%
    mutate(
      !!nvar1 := if_else(!is.na(.data[[var1]]) & .data[[var_tm]] != 1, 1, 0),
      !!nvar2 := if_else(!is.na(.data[[var2]]) & .data[[var_tm]] != 1 & .data[[var1]] != 1, 1, 0),
      !!nvar3 := if_else(!is.na(.data[[var3]]) & .data[[var_tm]] != 1 & .data[[var2]] != 66, 1, 0),
      !!nvar4 := if_else(!is.na(.data[[var4]]) & .data[[var_tm]] != 1 & .data[[var1]] != 1, 1, 0),
      !!nvar5 := if_else(!is.na(.data[[var5]]) & .data[[var_tm]] != 1 & .data[[var4]] != 66, 1, 0),
      !!nvar6 := if_else(!is.na(.data[[var6]]) & .data[[var_tm]] != 1, 1, 0)
    )
}


# Abonos

for (i in seq_along(empresas_4)) {
  empresa <- empresas_4[i]
  num <- uso[i]
  var1 <- paste0(empresa, "_PROB_ABO")
  nvar1 <- paste0("s_", var1)
  var2 <- paste0(empresa, "_FREC_ABO")
  nvar2 <- paste0("s_", var2)
  var3 <- paste0(empresa, "_FREC_ABO_o")
  nvar3 <- paste0("s_", var3)
  var4 <- paste0(empresa, "_INCO_ABO")
  nvar4 <- paste0("s_", var4)
  var5 <- paste0(empresa, "_INCO_ABO_o")
  nvar5 <- paste0("s_", var5)
  var_tm <- paste0("TM_USO_", num)

  alertas <- alertas %>%
    mutate(
      !!nvar1 := if_else(!is.na(.data[[var1]]) & .data[[var_tm]] != 1, 1, 0),
      !!nvar2 := if_else(!is.na(.data[[var2]]) & .data[[var_tm]] != 1 & .data[[var1]] != 1, 1, 0),
      !!nvar3 := if_else(!is.na(.data[[var3]]) & .data[[var_tm]] != 1 & .data[[var2]] != 66, 1, 0),
      !!nvar4 := if_else(!is.na(.data[[var4]]) & .data[[var_tm]] != 1 & .data[[var1]] != 1, 1, 0),
      !!nvar5 := if_else(!is.na(.data[[var5]]) & .data[[var_tm]] != 1 & .data[[var4]] != 66, 1, 0)
    )
}


alertas <- alertas %>%
  mutate(
    s_TEMP_ABO_o = if_else(!is.na(TEMP_ABO_o) & TEMP_ABO != "TEMP_ABO_66", 1, 0),
    s_REP_ABO_o = if_else(!is.na(REP_ABO_o) & REP_ABO_REP_ABO_66 != 1, 1, 0),
    s_NOTF_ABO_o = if_else(!is.na(NOTF_ABO_o) & NOTF_ABO != 4, 1, 0),
    s_ATRIB_CAMBIO_MP = if_else(!is.na(ATRIB_CAMBIO_MP) & as.numeric(PROB_CAMBIO_MP) <= 4, 1, 0),
    s_USO_BANCOS = if_else(!is.na(USO_BANCOS) & PRES_BANCA != 1, 1, 0),
    s_USO_BANCOS_o = if_else(!is.na(USO_BANCOS_o) & USO_BANCOS_66 != 1, 1, 0),
    s_PRINC_BANCO_o = if_else(!is.na(PRINC_BANCO_o) & PRINC_BANCO != 66, 1, 0),
    s_MOT_NOUSO = if_else(!is.na(MOT_NOUSO) & USO_POS != 2, 1, 0),
    s_MOT_NOUSO_o = if_else(!is.na(MOT_NOUSO_o) & MOT_NOUSO_66 != 1, 1, 0),
    s_MED_COM_o = if_else(!is.na(MED_COM_o) & MED_COM_66 != 1, 1, 0)
  )



### Sumar total de saltos irregulares ------------------------------------------

variables_salto <- names(alertas %>%
  select(matches("^s_")))

alertas <- alertas %>%
  mutate(
    total_saltos = rowSums(alertas[, variables_salto], na.rm = T)
  )


## Validación de preguntas obligatorias (missings) -----------------------------

vars <- c(
  "F1",
  "GEN",
  "EDAD",
  "DEP",
  "GIRO",
  "MP_USO",
  "TOM_SP",
  "TM_SP_A",
  "TM_USO",
  "TM_USO_A",
  "TM_PREF",
  "MOT_PREF",
  "TM_REC",
  "MOT_RECHAZO",
  "AT01",
  "AT02",
  "AT03",
  "AT04",
  "AT05",
  "AT06",
  "AT07",
  "AT08",
  "AT09",
  "AT10",
  "AT11",
  "AT12",
  "AT13",
  "AT14",
  "AT15",
  "AT16",
  "AT17",
  paste0("six_",c(1:6)),
  "TEMP_ABO",
  "REP_ABO",
  "NOTF_ABO",
  "DEF_SEGU",
  "PROB_CAMBIO_MP",
  "ATRIB_CAMBIO_MP",
  "TP_OPER",
  "NRO_EMP",
  "VOL_MEN",
  "PRES_BANCA",
  "USO_BANCOS",
  "USO_POS",
  "TIPO_MP",
  "TIPO_AGENTE",
  "MED_COM",
  "raz_social",
  "ruc"
)


# Crear las variables dummy de missing
alertas <- alertas %>%
  mutate(across(all_of(vars), ~ if_else(is.na(.x), 1, 0), .names = "m_{.col}"))


# Missings con condiciones de relevancia

mp_sw <- c("MP_SW_efectivo",
           "MP_SW_tarjeta",
           "MP_SW_transferencia",
           "MP_SW_billetera",
           "MP_SW_pago")

num_sp <- c(1:5)

for(i in seq_along(mp_sw)){
  var1 <- mp_sw[i]
  nvar <- paste0("m_",var1)
  mpvar <- paste0("MP_USO_",i)
  
  alertas <- alertas %>%
    mutate(
      !!nvar := if_else(is.na(.data[[var1]]) & .data[[mpvar]] == 1,1,0)
    )
  
}

empresas <- c("Niubiz", "Izipay", "Culqi", "Yape", "Plin", "Otro")
tom_codes <- c(1, 2, 3, 4, 5, 66)

alertas <- alertas %>%
  mutate(m_TOM_SP_o = if_else(is.na(TOM_SP_o) & TOM_SP_66 == 1, 1, 0))

for (i in seq_along(empresas)) {
  empresa <- empresas[i]
  code <- tom_codes[i]

  var1 <- paste0("TM_MEDIOS_", empresa)
  var2 <- paste0("TM_MEDIOS_", empresa, "_o")
  var66 <- paste0("TM_MEDIOS_", empresa, "_66")
  nueva1 <- paste0("m_TM_MEDIOS_", empresa)
  nueva2 <- paste0("m_TM_MEDIOS_", empresa, "_o")
  tom_var <- paste0("TOM_SP_", code)

  alertas <- alertas %>%
    mutate(
      !!nueva1 := if_else(is.na(.data[[var1]]) & .data[[tom_var]] == 1, 1, 0),
      !!nueva2 := if_else(is.na(.data[[var2]]) & .data[[var66]] == 1, 1, 0)
    )
}


alertas <- alertas %>%
  mutate(
    m_TM_USO_o = if_else(is.na(TM_USO_o) & TM_USO_66 == 1, 1, 0),
    m_TM_USO_A_o = if_else(is.na(TM_USO_A_o) & TM_USO_A_66 == 1, 1, 0),
    m_MOT_PREF_o = if_else(is.na(MOT_PREF_o) & MOT_PREF_66 == 1, 1, 0)
  )

empresam_acro <- c("NB", "IZI", "CQ", "OP", "MPAGO", "YAPE", "PLIN")
tm_uso <- c(1:7)

for (i in seq_along(empresam_acro)) {
  empresa <- empresam_acro[i]
  tm_num <- tm_uso[i]

  var_1 <- paste0("NPS_", empresa)
  var_2 <- paste0("TM_USO_", tm_num)
  new_var <- paste0("m_", var_1)

  alertas <- alertas %>%
    mutate(
      !!new_var := if_else(is.na(.data[[var_1]]) & .data[[var_2]] == 1, 1, 0)
    )
}

alertas <- alertas %>%
  mutate(
    m_NO_USO_NIUBIZ = if_else(is.na(NO_USO_NIUBIZ) & TM_USO_1 == 1 & TM_USO_A_1 != 1, 1, 0),
    m_NO_USO_NIUBIZ_o = if_else(is.na(NO_USO_NIUBIZ_o) & NO_USO_NIUBIZ_66 == 1, 1, 0),
    m_TIPO_MS = if_else(is.na(TIPO_MS) & TIPO_MP_2 == 1,1,0)
  )

empresam_2 <- c("niubiz", "izipay")
num_empresas <- c(1:2)

for (i in seq_along(empresam_2)) {
  empresa <- empresam_2[i]
  num <- num_empresas[i]

  var1 <- paste0("DIF_", empresa)
  var2 <- paste0("DIF_A_", empresa)
  var3 <- paste0("REL_", empresa)
  var4 <- paste0("FAM_", empresa)
  var5 <- paste0("IMA_", empresa)
  tm_var <- paste0("TM_USO_", num)
  nvar1 <- paste0("m_DIF_", empresa)
  nvar2 <- paste0("m_DIF_A_", empresa)
  nvar3 <- paste0("m_REL_", empresa)
  nvar4 <- paste0("m_FAM_", empresa)
  nvar5 <- paste0("m_IMA", empresa)

  alertas <- alertas %>%
    mutate(
      !!nvar1 := if_else(is.na(.data[[var1]]) & .data[[tm_var]] == 1, 1, 0),
      !!nvar2 := if_else(is.na(.data[[var2]]) & .data[[tm_var]] == 1, 1, 0),
      !!nvar3 := if_else(is.na(.data[[var3]]) & .data[[tm_var]] == 1, 1, 0),
      !!nvar4 := if_else(is.na(.data[[var4]]) & .data[[tm_var]] == 1, 1, 0),
      !!nvar5 := if_else(is.na(.data[[var5]]) & .data[[tm_var]] == 1, 1, 0)
    )
}

# Valoración atributos


empresam_3 <- c("Niubiz", "Izipay", "Culqi", "Openpay")
num_empresas <- c(1:3,8)

for (i in seq_along(empresam_3)) {
  empresa <- empresam_3[i]
  num <- num_empresas[i]

  tm_var <- paste0("TM_USO_", num)

  for (j in 1:17) {
    at_var <- paste0(empresa, "_At_", sprintf("%02d", j))
    m_var <- paste0("m_", at_var)

    alertas <- alertas %>%
      mutate(
        !!m_var := if_else(is.na(.data[[at_var]]) & .data[[tm_var]] == 1, 1, 0)
      )
  }
}

# Variables versus

alertas <- alertas %>%
  mutate(
    filtro_comisionem_1 = as.integer(if_any(all_of(paste0("TM_SP_A_", 1:4)), ~ . == 1)),
    filtro_comisionem_2 = as.integer(if_any(all_of(paste0("TM_USO_", 1:4)), ~ . == 1))
  )

num_empresas <- c(1:4)


for (i in seq_along(str_to_upper(empresam_3))) {
  empresa <- str_to_upper(empresam_3)[i]
  num <- num_empresas[i]

  var1 <- paste0("VERSUS_PRODUCTO_", empresa)
  nvar1 <- paste0("m_", var1)
  var2 <- paste0("VERSUS_PRECIO_", empresa)
  nvar2 <- paste0("m_", var2)
  var3 <- paste0("VERSUS_EXP_", empresa)
  nvar3 <- paste0("m_", empresa)
  var4 <- paste0("VERSUS_ABONO_", empresa)
  nvar4 <- paste0("m_", var4)
  var5 <- paste0("VERSUS_SEGURIDAD_", empresa)
  nvar5 <- paste0("m_", var5)
  tm_var1 <- paste0("TM_SP_A_", num)
  tm_var2 <- paste0("TM_USO_", num)

  alertas <- alertas %>%
    mutate(
      !!nvar1 := if_else(is.na(.data[[var1]]) & .data[[tm_var1]] == 1 & filtro_comisionem_1 == 1, 1, 0),
      !!nvar2 := if_else(is.na(.data[[var2]]) & .data[[tm_var2]] == 1 & filtro_comisionem_2 == 1, 1, 0),
      !!nvar3 := if_else(is.na(.data[[var3]]) & .data[[tm_var2]] == 1 & filtro_comisionem_2 == 1, 1, 0),
      !!nvar4 := if_else(is.na(.data[[var4]]) & .data[[tm_var2]] == 1, 1, 0),
      !!nvar5 := if_else(is.na(.data[[var5]]) & .data[[tm_var2]] == 1, 1, 0)
    )
}


# Experiencia de uso


empresam_4 <- c("NIUBIZ", "IZIPAY", "CULQI")
uso <- c(1:3)

for (i in seq_along(empresam_4)) {
  empresa <- empresam_4[i]
  num <- uso[i]
  var1 <- paste0(empresa, "_PROB_EXP")
  nvar1 <- paste0("m_", var1)
  var2 <- paste0(empresa, "_FREC_PROB_USO")
  nvar2 <- paste0("m_", var2)
  var3 <- paste0(empresa, "_FREC_PROB_USO_o")
  nvar3 <- paste0("m_", var3)
  var4 <- paste0(empresa, "_TIP_PROB_EXP")
  nvar4 <- paste0("m_", var4)
  var5 <- paste0(empresa, "_TIP_PROB_EXP_o")
  nvar5 <- paste0("m_", var5)
  var6 <- paste0(empresa, "_PROM_USO")
  nvar6 <- paste0("m_", var6)
  var_tm <- paste0("TM_USO_", num)

  alertas <- alertas %>%
    mutate(
      !!nvar1 := if_else(is.na(.data[[var1]]) & .data[[var_tm]] == 1, 1, 0),
      !!nvar2 := if_else(is.na(.data[[var2]]) & .data[[var_tm]] == 1 & .data[[var1]] == 1, 1, 0),
      !!nvar3 := if_else(is.na(.data[[var3]]) & .data[[var_tm]] == 1 & .data[[var2]] == 66, 1, 0),
      !!nvar4 := if_else(is.na(.data[[var4]]) & .data[[var_tm]] == 1 & .data[[var1]] == 1, 1, 0),
      !!nvar5 := if_else(is.na(.data[[var5]]) & .data[[var_tm]] == 1 & .data[[var4]] == 66, 1, 0),
      !!nvar6 := if_else(is.na(.data[[var6]]) & .data[[var_tm]] == 1, 1, 0)
    )
}


# Abonos

for (i in seq_along(empresam_4)) {
  empresa <- empresam_4[i]
  num <- uso[i]
  var1 <- paste0(empresa, "_PROB_ABO")
  nvar1 <- paste0("m_", var1)
  var2 <- paste0(empresa, "_FREC_ABO")
  nvar2 <- paste0("m_", var2)
  var3 <- paste0(empresa, "_FREC_ABO_o")
  nvar3 <- paste0("m_", var3)
  var4 <- paste0(empresa, "_INCO_ABO")
  nvar4 <- paste0("m_", var4)
  var5 <- paste0(empresa, "_INCO_ABO_o")
  nvar5 <- paste0("m_", var5)
  var_tm <- paste0("TM_USO_", num)

  alertas <- alertas %>%
    mutate(
      !!nvar1 := if_else(is.na(.data[[var1]]) & .data[[var_tm]] == 1, 1, 0),
      !!nvar2 := if_else(is.na(.data[[var2]]) & .data[[var_tm]] == 1 & .data[[var1]] == 1, 1, 0),
      !!nvar3 := if_else(is.na(.data[[var3]]) & .data[[var_tm]] == 1 & .data[[var2]] == 66, 1, 0),
      !!nvar4 := if_else(is.na(.data[[var4]]) & .data[[var_tm]] == 1 & .data[[var1]] == 1, 1, 0),
      !!nvar5 := if_else(is.na(.data[[var5]]) & .data[[var_tm]] == 1 & .data[[var4]] == 66, 1, 0)
    )
}


alertas <- alertas %>%
  mutate(
    m_TEMP_ABO_o = if_else(is.na(TEMP_ABO_o) & TEMP_ABO == 66, 1, 0),
    m_REP_ABO_o = if_else(is.na(REP_ABO_o) & REP_ABO == 66, 1, 0),
    m_NOTF_ABO_o = if_else(is.na(NOTF_ABO_o) & NOTF_ABO == 66, 1, 0),
    m_ATRIB_CAMBIO_MP = if_else(is.na(ATRIB_CAMBIO_MP) & PROB_CAMBIO_MP > 4, 1, 0),
    m_USO_BANCOS = if_else(is.na(USO_BANCOS) & PRES_BANCA == 1, 1, 0),
    m_USO_BANCOS_o = if_else(is.na(USO_BANCOS_o) & USO_BANCOS_66 == 1, 1, 0),
    m_PRINC_BANCO_o = if_else(is.na(PRINC_BANCO_o) & PRINC_BANCO == 66, 1, 0),
    m_MOT_NOUSO = if_else(is.na(MOT_NOUSO) & USO_POS == 2, 1, 0),
    m_MOT_NOUSO_o = if_else(is.na(MOT_NOUSO_o) & MOT_NOUSO_66 == 1, 1, 0),
    m_MED_COM_o = if_else(is.na(MED_COM_o) & MED_COM_66 == 1, 1, 0),
    m_PRINC_BANCO = if_else(is.na(PRINC_BANCO) & PRES_BANCA == 1,1,0)
  )

### Sumar total missings -------------------------------------------------------

variables_missing <- names(alertas %>%
  select(matches("^m_")))

alertas <- alertas %>%
  mutate(
    total_missing = rowSums(alertas[, variables_missing], na.rm = T)
  )

##  Duplicados -----------------------------------------------------------------

caract_especi <- c(
  "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u",
  "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U",
  "ñ" = "n", "Ñ" = "N"
)

alertas <- alertas %>%
  mutate(
    razon_social_clean = str_squish(str_replace_all(toupper(raz_social), caract_especi))
  )


alertas <- alertas %>%
  mutate(
    dup_razon_soc = if_else(duplicated(razon_social_clean), 1, 0),
    dup_ruc = case_when(duplicated(ruc) ~ 1,
                        ruc %in% data_ronda_1$ruc ~ 1,
                        TRUE ~ 0))


## Alertas lógicas -------------------------------------------------------------

# Validación porcentajes y tiempo negocio

vars_mp <- c("MP_SW_efectivo", "MP_SW_tarjeta", "MP_SW_transferencia", "MP_SW_billetera", "MP_SW_pago")
vars_sw <- c("SW_SP_A_01", "SW_SP_A_02", "SW_SP_A_03", "SW_SP_A_04", "SW_SP_A_05", "SW_SP_A_06", "SW_SP_A_07", "SW_SP_A_08", "SW_SP_A_66")

# Crear las flags con conversión a numérico
alertas <- alertas %>%
  mutate(
    flag_mp_sw_suma = if_else(rowSums(across(all_of(vars_mp), ~as.numeric(.)), na.rm = TRUE) != 100, 1, 0,missing=0),
    flag_sw_sp_suma = if_else(rowSums(across(all_of(vars_sw), ~as.numeric(.)), na.rm = TRUE) != 100, 1, 0,missing = 0),
    flag_suma_100 = if_else(flag_mp_sw_suma == 1 | flag_sw_sp_suma == 1, 1, 0,missing = 0),
    flag_tiempo_negocio = if_else(
      as.numeric(TP_OPER) < as.numeric(FAM_niubiz) | as.numeric(TP_OPER) < as.numeric(FAM_izipay),1,0,missing = 0
    )
  )

# Alertas RUC

ruc_no_validos <- c(strrep(c(0:9), 11),12345678910,10987654321)

alertas <- alertas %>%
  mutate(
    flag_ruc = if_else(ruc %in% ruc_no_validos,1,0)
  )

## Alertas geográficas ---------------------------------------------------------

sf_use_s2(TRUE)

k_factor <- 1.5   # factor de tolerancia
tol_min  <- 20    # tolerancia mínima en metros

# 1) Inicializa distancia como NA
alertas <- alertas %>%
  mutate(dist_m = NA_real_)

# 2) Filas con coordenadas completas para INICIO y FIN
ok_coords <- with(alertas, complete.cases(lon, lat, lon_final, lat_final))

# 3) Calcula distancia geodésica sólo en esas filas
if (any(ok_coords)) {
  pts_ini <- st_as_sf(alertas[ok_coords, ], coords = c("lon", "lat"),
                      crs = 4326, remove = FALSE)
  pts_fin <- st_as_sf(alertas[ok_coords, ], coords = c("lon_final", "lat_final"),
                      crs = 4326, remove = FALSE)
  
  alertas$dist_m[ok_coords] <- as.numeric(
    st_distance(st_geometry(pts_ini), st_geometry(pts_fin), by_element = TRUE)
  )
}

# 4) Tolerancia basada en accuracy y flag (solo si nada es NA)
alertas <- alertas %>%
  mutate(
    tol_acc = case_when(
      !is.na(acc) & !is.na(acc_final) ~ k_factor * sqrt(acc^2 + acc_final^2),
      TRUE ~ NA_real_
    ),
    tol_m = if_else(!is.na(tol_acc), pmax(tol_min, tol_acc), NA_real_),
    
    flag_movido = case_when(
      # calcula flag sólo si dist_m y tol_m existen (y por construcción, coords y accuracies también)
      !is.na(dist_m) & !is.na(tol_m) ~ if_else(dist_m > tol_m, 1L, 0L),
      TRUE ~ NA_integer_
    )
  )

## Consolidación de alertas ----------------------------------------------------

# Encuestas rechazadas y duplicados, valores faltante y atípicos

alertas <- alertas %>%
  mutate(
    flag_saltos = if_else(total_saltos > 0, 1, 0),
    flag_duplicated = if_else(dup_razon_soc == 1 | dup_ruc == 1, 1, 0),
    flag_missing = if_else(total_missing > 0, 1, 0),
    flag_extreme_values = if_else(flag_suma_100 >0 | flag_tiempo_negocio >0,1,0
    ))

# Corrección para encuestas de ronda 1 que pasan como exceso

# Esta corrección obedece a que en ronda 1 ya se levantaron las alertas y los
# cambios de formulario hacen que no todas las alertas sean válidas

alertas <- alertas %>%
  mutate(across(matches("^(m_|s_|flag|total)"), 
                ~ if_else(cuota_valida_1 == "Exceso", 0, .,missing = .)))


# Bajar alertas para encuestas validadas

alertas_validadas <- read_sheet(id_keys, sheet = "VALIDAS")%>%pull(KEY)

if(any(!is.na(alertas_validadas))) {
  alertas <- alertas %>%
    mutate(across(matches("^(m_|s_|flag|total)"), 
                  ~ if_else(KEY %in% alertas_validadas, 0, .,missing = .)))
  
  
} else {
  message("No hay encuestas para corregir")
}

## Creamos los variable coordinador--------------------------------------------------------
# Ururi es coordinaro 1
# William es coordniador 2

alertas$coordinador <- 1
alertas <- alertas %>%
  mutate(
    coordinador = case_when(
      username == "william.adrianzen@gmail.com" ~ 2,
      username == "lorenax1099@gmail.com" ~ 2,
      username == "mollie.allen@unmsm.edu.pe" ~ 2,
      username == "econ.cristobalperez@gmail.com" ~ 2,
      username == "melanygomezvalencia204@gmail.com" ~ 2,
      username == "greciam.oliver@gmail.com" ~ 2,
      username == "naye131101@gmail.com" ~ 2,
      username == "deyanirahito@gmail.com" ~ 2,
      username == "urpi.solis@unmsm.edu.pe" ~ 2,
      username == "Kemiko.cruz17@gmail.com" ~ 2,
      username == "Gabriel.bocanegra@pucp.edu.pe" ~ 2,
      username == "angiex312x@gmail.com" ~ 2,
      username == "kvaleroa@unsa.edu.pe" ~ 2,
      username == "151707@unsaac.edu.pe" ~ 2,
      username == "giulianamoscol@gmail.com" ~ 2,
      username == "valeria.gonzalez.7306@gmail.com" ~ 2,
      username == "alisa.trillo@pucp.edu.pe" ~ 2,
      username == "diazm.reginaa@gmail.com" ~ 2,
      username == "carlamianl05@gmail.com" ~ 2, 
      username == "m.catanoch@gmail.com" ~ 2, 
      username == "valsof.valcarcel@gmail.com" ~ 2, 
      username == "kpihuaycho@gmail.com" ~ 2, 
      username == "jimena.aguilar@pucp.edu.pe" ~ 2, 
      username == "coordinaciondtm@equilibriumbdc.com" ~ 2, 
      username == "edgarelazo@gmail.com" ~ 2, 
      TRUE ~ coordinador
    ))




## Crear alertas LOOKER --------------------------------------------------------


alertas <- alertas %>%
  mutate(
    Exitos = if_else(flag_movido == 0 & flag_ruc == 0 & flag_duration_menos == 0 & flag_duplicated == 0 &
      flag_missing == 0 & flag_saltos == 0 & flag_mp_sw_suma == 0 & flag_sw_sp_suma == 0 &
        flag_tiempo_negocio == 0, 1, 0),
    Alertas = if_else(flag_movido == 1 | flag_ruc == 1 | flag_duration_menos == 1 | flag_duplicated == 1 |
      flag_missing == 1 | flag_saltos == 1 | flag_mp_sw_suma == 1 | flag_sw_sp_suma == 1 |
        flag_tiempo_negocio == 1, 1, 0),
    tiempos_anomalos_menos = if_else(flag_duration_menos == 1, "Sí", "No"),
    duplicados = if_else(flag_duplicated == 1, "Sí", "No"),
    valores_faltantes = if_else(flag_missing == 1, "Sí", "No"),
    saltos_irregulares = if_else(flag_saltos == 1, "Sí", "No"),
    valores_extremos = if_else(flag_extreme_values == 1, "Sí", "No"),
    ruc_invalido = if_else(flag_ruc == 1,"Sí","No"),
    gps_movido = if_else(flag_movido == 1, "Sí", "No")
  )

cat("Exitos:",as.character(sum(alertas$Exitos==1,na.rm = TRUE))," ","Alertas:",
    as.character(sum(alertas$Alertas==1,na.rm = TRUE)), " ", "Invalidas:", as.character(unique(alertas$n_no_validas)))

alertas <- alertas %>%
  mutate(porcentaje_avance = (sum(Exitos, na.rm = TRUE) / 769))

alertas <- alertas %>%
  mutate(porcentaje_avance2 = (sum(Exitos, na.rm = TRUE) / 701))

# Congelar fechas

alertas <- alertas %>% arrange(SubmissionDate)

to_lima_str <- function(x) format(x, tz = "America/Lima", usetz = FALSE, "%Y-%m-%d %H:%M:%S")

alertas <- alertas %>%
  mutate(
    starttime_fixed      = to_lima_str(starttime),
    endtime_fixed        = to_lima_str(endtime),
    SubmissionDate_fixed = to_lima_str(SubmissionDate)
  )


# Exportar exitosas

data_ronda_2 <- data_ronda_2 %>% filter(KEY %in% alertas[alertas$Exitos == 1,]$KEY)

# Reporte avance de cuotas -----------------------------------------------------

## Tabla de cuotas -------------------------------------------------------------

cuotas_ronda_2 <- alertas %>%
  group_by(DEP_str,tamanio_ingresos)%>%
  summarise(total = sum(Exitos,na.rm = T)) %>%
  full_join(cuotas_2%>%select(-ronda), by = c("DEP_str" = "Regiones",
                             "tamanio_ingresos" = "Categoria"))%>%
  ungroup()%>%
  rename(Alcanzado = total, Regiones = DEP_str, Meta = Cuota_2,
         Categoria = tamanio_ingresos)%>%
  mutate(Alcanzado = if_else(is.na(Alcanzado),0,Alcanzado) ,
          Avance = round((Alcanzado/Meta)*100,2),
         Faltan = Meta - Alcanzado,
         Categoria = factor(Categoria, levels = c("Micro","Pequeña","Mediana"), ordered = T))%>%
  arrange(Regiones, Categoria)%>%
  filter(Categoria != "Meta")

cuotas_ronda_3 <- alertas %>%
  group_by(DEP_str,tamanio_ingresos)%>%
  summarise(total = sum(Exitos,na.rm = T)) %>%
  full_join(cuotas_3%>%select(-ronda), by = c("DEP_str" = "Regiones",
                                              "tamanio_ingresos" = "Categoria"))%>%
  ungroup()%>%
  rename(Alcanzado = total, Regiones = DEP_str, Meta = Cuota_3,
         Categoria = tamanio_ingresos)%>%
  mutate(Alcanzado = if_else(is.na(Alcanzado),0,Alcanzado) ,
         Avance = round((Alcanzado/Meta)*100,2),
         Faltan = Meta - Alcanzado,
         Categoria = factor(Categoria, levels = c("Micro","Pequeña","Mediana"), ordered = T))%>%
  arrange(Regiones, Categoria)%>%
  filter(Categoria != "Meta")




#Data para el comparativo 2024 vs 2025 -----------------------------------------
           

# Filtrar TM_USO_1 == 1 y seleccionar variables necesarias
data_filtrada <- alertas |>
  filter(TM_USO_1 == 1 & Exitos == 1) |>
  select(
    instanceID,
    uso_1, uso_a_1, TM_REC,
    NPS_NB, NPS_IZI, NPS_CQ, NPS_OP, NPS_YAPE, NPS_PLIN,
    DEP_str, tamanio_empleados, tamanio_ingresos,
    NO_USO_NIUBIZ_1, NO_USO_NIUBIZ_2, NO_USO_NIUBIZ_3, NO_USO_NIUBIZ_4, 
    NO_USO_NIUBIZ_5, NO_USO_NIUBIZ_6, NO_USO_NIUBIZ_66
  )

#Etiquetas para TM_REC


data_filtrada <- data_filtrada |>
  mutate(TM_REC = case_when(
    TM_REC == 1 ~ "Niubiz",
    TM_REC == 2 ~ "Izipay",
    TM_REC == 3 ~ "Culqi",
    TM_REC == 4 ~ "OpenPay",
    TM_REC == 5 ~ "Vendemas",
    TM_REC == 6 ~ "YAPE",
    TM_REC == 7 ~ "Plin",
    TM_REC == 999 ~ "Ninguno",
    TRUE ~ as.character(TM_REC)  # Para mantener otros valores si los hubiera
  ))

set.seed(123)  # para que el muestreo sea reproducible

data_40 <- data_filtrada |> 
  slice_sample(n = 50)


## 1. Motivo deja de usar Niubiz (NO_USO_NIUBIZ) - multiple ---------------------


# Vector con las variables múltiples
variables_no_uso <- c(
  "NO_USO_NIUBIZ_1",
  "NO_USO_NIUBIZ_2",
  "NO_USO_NIUBIZ_3",
  "NO_USO_NIUBIZ_4",
  "NO_USO_NIUBIZ_5",
  "NO_USO_NIUBIZ_6",
  "NO_USO_NIUBIZ_66"
)

# Crear la tabla resumen
no_uso_niubiz_25 <- variables_no_uso %>%
  setNames(., .) %>%
  lapply(function(x) sum(data_40[[x]] == 1, na.rm = TRUE)) %>%  # ya no necesitas filtrar Exitos
  enframe(name = "Motivo_no_uso_Niubiz", value = "Total de marcaciones") %>%
  mutate(Motivo_no_uso_Niubiz = recode(Motivo_no_uso_Niubiz,
                                       "NO_USO_NIUBIZ_1" = "Me cobraban tasas elevadas",
                                       "NO_USO_NIUBIZ_2" = "El tiempo de abono era muy alto",
                                       "NO_USO_NIUBIZ_3" = "Tenía problemas técnicos (CON POS)",
                                       "NO_USO_NIUBIZ_4" = "No aceptaban todas las tarjetas",
                                       "NO_USO_NIUBIZ_5" = "Problemas al conciliar ventas y depósitos",
                                       "NO_USO_NIUBIZ_6" = "Mala atención Posventa",
                                       "NO_USO_NIUBIZ_66" = "Otros (especifique)"
  ))


## 2. Recomendación de plataformas (NPS_*) - única -----------------------------

data_40 <- data_40 %>%
  mutate(
    NPSNB = case_when(
      NPS_NB >= 0 & NPS_NB <= 6 ~ "Detractores",
      NPS_NB %in% c(7, 8)       ~ "Pasivos",
      NPS_NB %in% c(9, 10)      ~ "Promotores",
      TRUE                      ~ NA_character_
    ),
    NPSIZI = case_when(
      NPS_IZI >= 0 & NPS_IZI <= 6 ~ "Detractores",
      NPS_IZI %in% c(7, 8)        ~ "Pasivos",
      NPS_IZI %in% c(9, 10)       ~ "Promotores",
      TRUE                        ~ NA_character_
    ),
    NPSCQ = case_when(
      NPS_CQ >= 0 & NPS_CQ <= 6 ~ "Detractores",
      NPS_CQ %in% c(7, 8)       ~ "Pasivos",
      NPS_CQ %in% c(9, 10)      ~ "Promotores",
      TRUE                      ~ NA_character_
    ),
    NPSOP = case_when(
      NPS_OP >= 0 & NPS_OP <= 6 ~ "Detractores",
      NPS_OP %in% c(7, 8)       ~ "Pasivos",
      NPS_OP %in% c(9, 10)      ~ "Promotores",
      TRUE                      ~ NA_character_
    ),
    NPSYAPE = case_when(
      NPS_YAPE >= 0 & NPS_YAPE <= 6 ~ "Detractores",
      NPS_YAPE %in% c(7, 8)         ~ "Pasivos",
      NPS_YAPE %in% c(9, 10)        ~ "Promotores",
      TRUE                          ~ NA_character_
    ),
    NPSPLIN = case_when(
      NPS_PLIN >= 0 & NPS_PLIN <= 6 ~ "Detractores",
      NPS_PLIN %in% c(7, 8)         ~ "Pasivos",
      NPS_PLIN %in% c(9, 10)        ~ "Promotores",
      TRUE                          ~ NA_character_
    )
  )


## Análisis NPS ----------------------------------------------------------------

# 1. Definir variables NPS categorizadas
variables_categorizadas <- c("NPSNB", "NPSIZI", "NPSCQ", "NPSOP", "NPSYAPE", "NPSPLIN")

# 2. Pasar a formato largo
nps_categorizado_long <- data_40 |>
  select(all_of(variables_categorizadas)) |>
  pivot_longer(
    cols = everything(),
    names_to = "solucion_pago",
    values_to = "categoria"
  )

# 3. Reetiquetar solucion_pago
nps_categorizado_long <- nps_categorizado_long |>
  mutate(solucion_pago = recode(solucion_pago,
                                "NPSNB" = "Niubiz",
                                "NPSIZI" = "Izipay",
                                "NPSCQ" = "Culqui",
                                "NPSOP" = "OpenPay",
                                "NPSYAPE" = "Yape",
                                "NPSPLIN" = "Plin"
  ))

# 4. Contar valores y calcular porcentaje por solución
nps_tabla_25 <- nps_categorizado_long |>
  filter(!is.na(categoria)) |>
  group_by(solucion_pago, categoria) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(solucion_pago) |>
  mutate(porcentaje = round(n / sum(n) * 100, 2)) |>
  ungroup()


# Confirmación de finalización
message("Alertas creadas exitosamente.")
