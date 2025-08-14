#### Alertas ####

### Tranformar variables en numéricas y ver participantes válidos

data_no_valido <- data %>%
  mutate(part_valido = if_else((as.numeric(F1) == 1 | as.numeric(F2) == 1) &
                                 as.numeric(F3) == 1, 1, 0)) %>%
  filter(part_valido == 0)

no_validas <- nrow(data_no_valido)

data <- data %>%
  mutate(part_valido = if_else((as.numeric(F1) == 1 | as.numeric(F2) == 1) &
    as.numeric(F3) == 1, 1, 0)) %>%
  filter(part_valido == 1) %>%
  mutate(duration_minutes = round(as.numeric(duration) / 60, 2),
         n_no_validas = no_validas)


# Eliminar mass
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
    EDAD = if_else(is.na(EDAD) & KEY == "uuid:fefa9f87-b9bb-4405-ac34-b1007940c510", 3, EDAD),
    GEN = if_else(is.na(GEN) & KEY == "uuid:fefa9f87-b9bb-4405-ac34-b1007940c510", 2, GEN)
  )


# Eliminar duplicado

data <- data %>%
  filter(
    KEY != "uuid:a4ea83e3-ee8e-46b5-b905-e9e538624d5b"
  )

#Eliminar encuestas falsas

encuestas_falsas_daniel <- data %>%
  filter(username == "danielviuri@gmail.com" &  ymd_hms(starttime) > ymd_hms("2025-08-11 23:59:59"))%>%
  pull(KEY)


data <- data %>%
  filter(!KEY %in% encuestas_falsas_daniel)


# Separar coordenadas

#data <- data %>%
#  mutate(
#    coords = trimws(coords),          # quita espacios al inicio/fin
#    coords = na_if(coords, "")        # convierte cadenas vacías en NA
#  ) %>%
#  separate(
#    coords,
#    into = c("lat", "lon", "alt", "acc"),
#    sep = "\\s+",                     # separa por uno o más espacios
#    fill = "right",                   # si faltan valores, rellena con NA a la derecha
#    extra = "drop",                   # si vienen más de 4, descarta el resto
#    convert = TRUE                    # convierte a numérico automáticamente
#  )


data <- data %>%
  mutate(
    lat = trimws(`coords-Latitude`),
    lon = trimws(`coords-Longitude`),
    lat_lon = paste0(lat,",",lon)
  )

alertas <- data

# Corrección time

# Carga las librerías necesarias
library(tidyverse)
library(lubridate)

alertas <- alertas %>%
  mutate(
    # Primero, parsea la fecha y hora si no lo están,
    starttime_corregido = with_tz(force_tz(ymd_hms(starttime), tzone = "UTC"), tzone = "America/Lima"),
    endtime_corregido = with_tz(force_tz(ymd_hms(endtime), tzone = "UTC"), tzone = "America/Lima")
  )

# (Opcional) Visualiza los resultados para verificar la corrección
alertas %>%
  select(starttime, starttime_corregido, endtime, endtime_corregido)


# Flag duración
dev_estandar <- alertas %>%
  filter(duration_minutes <= 100) %>%
  summarise(
    sd_duracion = sd(duration_minutes),
    median_duracion = mean(duration_minutes)
  ) %>%
  pull(sd_duracion)

median_duration <- alertas %>%
  filter(duration_minutes <= 100) %>%
  summarise(
    sd_duracion = sd(duration_minutes),
    median_duracion = median(duration_minutes)
  ) %>%
  pull(median_duracion)


alertas <- alertas %>%
  mutate(
    flag_duration_mas = if_else(
      ((duration_minutes - median_duration) / dev_estandar > 3) &
        part_valido == 1, 1, 0, missing = 0
    ),
    flag_duration_menos = if_else(
      ((duration_minutes - median_duration) / dev_estandar < -3) &
        part_valido == 1, 1, 0, missing = 0
    )
  )

#### Validación de saltos #####

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
num_empresas <- c(1:4)

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



## Sumar total de saltos irregulares

variables_salto <- names(alertas %>%
  select(matches("^s_")))

alertas <- alertas %>%
  mutate(
    total_saltos = rowSums(alertas[, variables_salto], na.rm = T)
  )


#### Validación de preguntas obligatorias (missings) #####

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
num_empresas <- c(1:4)

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

## Sumar total missings

variables_missing <- names(alertas %>%
  select(matches("^m_")))

alertas <- alertas %>%
  mutate(
    total_missing = rowSums(alertas[, variables_missing], na.rm = T)
  )


# Alerta de valores numéricos extremos ####

integer_vars <- c(
  "MP_SW_efectivo",
  "MP_SW_tarjeta",
  "MP_SW_transferencia",
  "MP_SW_billetera",
  "MP_SW_pago",
  "SW_SP_A_01",
  "SW_SP_A_02",
  "SW_SP_A_03",
  "SW_SP_A_04",
  "SW_SP_A_05",
  "SW_SP_A_06",
  "SW_SP_A_07",
  "SW_SP_A_66",
  "TIPO_MS"
)

alertas <- alertas %>%
  mutate(
    across(all_of(integer_vars),~if_else(abs((as.numeric(.x) - median(as.numeric(.x),na.rm = T))/sd(as.numeric(.x),na.rm = T)) > 3,1,0,missing = 0),
           .names = "ex_{.col}")
  )

# Flag valores extremos

variables_extremos <- names(alertas %>%
                             select(matches("^ex_")))

alertas <- alertas %>%
  mutate(
    total_extremos = rowSums(alertas[, variables_extremos], na.rm = T)
  )


## DUPLICADOS ----

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
    dup_razon_soc = if_else((duplicated(razon_social_clean) | duplicated(razon_social_clean, fromLast = TRUE)), 1, 0),
    dup_ruc = if_else((duplicated(ruc) | duplicated(ruc, fromLast = TRUE)), 1, 0))


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

# Encuestas rechazadas y duplicados, valores faltante y atípicos


alertas <- alertas %>%
  mutate(
    flag_saltos = if_else(total_saltos > 0, 1, 0),
    flag_duplicated = if_else(dup_razon_soc == 1 | dup_ruc == 1, 1, 0),
    flag_missing = if_else(total_missing > 0, 1, 0),
    flag_extreme_values = if_else(flag_suma_100 >0 | flag_tiempo_negocio >0,1,0
    ))


### Crear alertas LOOKER


alertas <- alertas %>%
  mutate(
    Exitos = if_else(flag_duration_menos == 0 & flag_duplicated == 0 &
      flag_missing == 0 & flag_saltos == 0 & flag_extreme_values == 0 & flag_mp_sw_suma == 0 & flag_sw_sp_suma == 0 &
        flag_tiempo_negocio == 0, 1, 0),
    Alertas = if_else(flag_duration_menos == 1 | flag_duplicated == 1 |
      flag_missing == 1 | flag_saltos == 1 | flag_extreme_values == 1 | flag_mp_sw_suma == 1 | flag_sw_sp_suma == 1 |
        flag_tiempo_negocio == 1, 1, 0),
    tiempos_anomalos_mas = if_else(flag_duration_mas == 1, "Sí", "No"),
    tiempos_anomalos_menos = if_else(flag_duration_menos == 1, "Sí", "No"),
    duplicados = if_else(flag_duplicated == 1, "Sí", "No"),
    valores_faltantes = if_else(flag_missing == 1, "Sí", "No"),
    saltos_irregulares = if_else(flag_saltos == 1, "Sí", "No"),
    valores_extremos = if_else(flag_extreme_values == 1, "Sí", "No")
  )

cat("Exitos:",as.character(sum(alertas$Exitos==1,na.rm = TRUE))," ","Alertas:",
    as.character(sum(alertas$Alertas==1,na.rm = TRUE)), " ", "Invalidas:", as.character(unique(alertas$n_no_validas)))

alertas <- alertas %>%
  mutate(porcentaje_avance = (sum(Exitos, na.rm = TRUE) / 650))

    
### Demográficos looker

# Departamento
alertas <- alertas %>%
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
# Categoría empresa

alertas <- alertas %>%
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


# TABLA DE CUOTAS MICRO (Ciudad) - select_one
#------------------------------------------------

cuotas_micro <- alertas %>%
  filter(Exitos == 1 & tamanio_ingresos=="Micro") %>%             # Solo encuestas válidas en el segmento micro
  group_by(DEP_str) %>%                # Agrupar por Ciudad
  summarise(Alcanzado = n(), .groups = "drop") %>% # Contar número de éxitos por ciudad y segmento micro
  
  rename(Regiones = DEP_str)

micro_cuotas <- tribble(
  ~Regiones,        ~Meta,
  "Lima",         280,
  "Callao",          35,
  "Arequipa",          35,
  "Cusco",          35,
  "Trujillo",          35,
  "Piura",          35
)

# Paso 3: Unir ambas tablas por Regiones
cuotas_micro <- cuotas_micro %>%
  left_join(micro_cuotas, by = "Regiones") %>%
  mutate(
    Meta = replace_na(Meta, 0), # En caso no haya coincidencia
    `% de avance` = round(Alcanzado / Meta * 100, 1),  # Calcular avance en porcentaje
    Faltan = Meta - Alcanzado                          # Calcular cuántos faltan
  ) %>%
  select(Regiones, Meta, Alcanzado, `% de avance`, Faltan)  # Reordenar columnas

# TABLA DE CUOTAS PEQUEÑA (Ciudad) - select_one
#------------------------------------------------

cuotas_peque <- alertas %>%
  filter(Exitos == 1 & tamanio_ingresos=="Pequeña") %>%             # Solo encuestas válidas en el segmento pequeño
  group_by(DEP_str) %>%                # Agrupar por Ciudad
  summarise(Alcanzado = n(), .groups = "drop") %>% # Contar número de éxitos por ciudad y segmento pequeño
  rename(Regiones = DEP_str)

peque_cuotas <- tribble(
  ~Regiones,        ~Meta,
  "Lima",         88,
  "Callao",          11,
  "Arequipa",          11,
  "Cusco",          11,
  "Trujillo",          11,
  "Piura",          11
)

# Paso 3: Unir ambas tablas por Regiones
cuotas_peque <- cuotas_peque %>%
  left_join(peque_cuotas, by = "Regiones") %>%
  mutate(
    Meta = replace_na(Meta, 0), # En caso no haya coincidencia
    `% de avance` = round(Alcanzado / Meta * 100, 1),  # Calcular avance en porcentaje
    Faltan = Meta - Alcanzado                          # Calcular cuántos faltan
  ) %>%
  select(Regiones, Meta, Alcanzado, `% de avance`, Faltan)  # Reordenar columnas

# TABLA DE CUOTAS MEDIANA (Ciudad) - select_one
#------------------------------------------------

cuotas_mediana <- alertas %>%
  filter(Exitos == 1 & tamanio_ingresos=="Mediana") %>%             # Solo encuestas válidas en el segmento MEDIANA
  group_by(DEP_str) %>%                # Agrupar por Ciudad
  summarise(Alcanzado = n(), .groups = "drop") %>% # Contar número de éxitos por ciudad y segmento MEDIANA
  rename(Regiones = DEP_str)

mediana_cuotas <- tribble(
  ~Regiones,        ~Meta,
  "Lima",         32,
  "Callao",          4,
  "Arequipa",          4,
  "Cusco",          4,
  "Trujillo",          4,
  "Piura",          4
)

# Paso 3: Unir ambas tablas por Regiones
cuotas_mediana <- cuotas_mediana %>%
  left_join(mediana_cuotas, by = "Regiones") %>%
  mutate(
    Meta = replace_na(Meta, 0), # En caso no haya coincidencia
    `% de avance` = round(Alcanzado / Meta * 100, 1),  # Calcular avance en porcentaje
    Faltan = Meta - Alcanzado                          # Calcular cuántos faltan
  ) %>%
  select(Regiones, Meta, Alcanzado, `% de avance`, Faltan)  # Reordenar columnas



#Data para el comparativo 2024 vs 2025
library(tibble)            


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
library(dplyr)

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


# 1. Motivo deja de usar Niubiz (NO_USO_NIUBIZ) - multiple
#--------------------------------------------

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


# 2. Recomendación de plataformas (NPS_*) - única
#-----------------------------------------------------------
library(dplyr)

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



library(dplyr)
library(tidyr)

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
