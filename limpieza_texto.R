
# Limpieza base de datos Niubiz ------------------------------------------------

# Filtrar cuotas válidas

data_niubiz <- data %>%
  filter(cuota_valida == "Válida")%>%
  filter(ruc != "99999999999" & ruc != "00000000000")

# Limpiar preguntas de texto

### *******************************************************************************
# Realizar categorización de variables otro
# *******************************************************************************
var_otro <- data_niubiz %>% 
  select(ends_with("_o")) # Esto lo saqué sólo para que veas todas las variables

data_niubiz <- data_niubiz %>%
  mutate(across(all_of(colnames(var_otro)),~ trimws(str_to_lower(.x)))) # Estandarizar


# *******************************************************************************
# TOM_SP_o
# *******************************************************************************

table(data_niubiz$TOM_SP_o) # Tabla inicial

data_niubiz <- data_niubiz %>%
  mutate(
    TOM_SP_o = if_else(TOM_SP_o == "open pay","openpay",TOM_SP_o)
  )

table(data_niubiz$TOM_SP_o) # Tabla limpia


# *******************************************************************************
# TM_MEDIOS_Niubiz_o
# *******************************************************************************

table(data_niubiz$TM_MEDIOS_Niubiz_o) # Tabla inicial

data_niubiz <- data_niubiz %>%
  mutate(
    TM_MEDIOS_Niubiz_6 = if_else(str_detect(TM_MEDIOS_Niubiz_o,"conocidos|vecinos"),1,TM_MEDIOS_Niubiz_6),
    TM_MEDIOS_Niubiz_66 = if_else(TM_MEDIOS_Niubiz_66 == 1 & str_detect(TM_MEDIOS_Niubiz_o,"conocidos|vecinos"),0,TM_MEDIOS_Niubiz_66),
    TM_MEDIOS_Niubiz_o = if_else(!is.na(TM_MEDIOS_Niubiz_o) & str_detect(TM_MEDIOS_Niubiz_o,"conocidos|vecinos"),NA,TM_MEDIOS_Niubiz_o),
    
    TM_MEDIOS_Niubiz_o = if_else(str_detect(TM_MEDIOS_Niubiz_o,"trabajo"),"en el trabajo",TM_MEDIOS_Niubiz_o)) 

table(data_niubiz$TM_MEDIOS_Niubiz_o) # Tabla limpia


# *******************************************************************************
# TM_MEDIOS_Izipay_o
# *******************************************************************************

table(data_niubiz$TM_MEDIOS_Izipay_o) #Tabla inicial
table(data_niubiz$TM_MEDIOS_Izipay_7)

data_niubiz <- data_niubiz %>%
  mutate(
    TM_MEDIOS_Izipay_7 = if_else(str_detect(TM_MEDIOS_Izipay_o,"el mismo banco mando un representante al negocio"),1,TM_MEDIOS_Izipay_7),
    TM_MEDIOS_Izipay_66 = if_else(TM_MEDIOS_Izipay_66 == 1 & str_detect(TM_MEDIOS_Izipay_o,"el mismo banco mando un representante al negocio"),0,TM_MEDIOS_Izipay_66),
    TM_MEDIOS_Izipay_o = if_else(!is.na(TM_MEDIOS_Izipay_o) & str_detect(TM_MEDIOS_Izipay_o,"el mismo banco mando un representante al negocio"),NA,TM_MEDIOS_Izipay_o),
    
    TM_MEDIOS_Izipay_o = if_else(str_detect(TM_MEDIOS_Izipay_o,"trabajo"),"en el trabajo",TM_MEDIOS_Izipay_o),
    TM_MEDIOS_Izipay_o = if_else(str_detect(TM_MEDIOS_Izipay_o, "banco"),"por un banco",TM_MEDIOS_Izipay_o))

table(data_niubiz$TM_MEDIOS_Izipay_o) #Tabla limpia


# *******************************************************************************
# TM_MEDIOS_Culqi_o
# *******************************************************************************  

table(data_niubiz$TM_MEDIOS_Culqi_o) #Tabla inicial

data_niubiz <- data_niubiz %>%
  mutate(
    TM_MEDIOS_Culqi_o = if_else(str_detect(TM_MEDIOS_Culqi_o,"trabajo"),"en el trabajo",TM_MEDIOS_Culqi_o),
    TM_MEDIOS_Culqi_o = if_else(str_detect(TM_MEDIOS_Culqi_o,"por el uso"),"por uso",TM_MEDIOS_Culqi_o))

table(data_niubiz$TM_MEDIOS_Culqi_o) #Tabla limpia

# *******************************************************************************
# TM_MEDIOS_Yape_o
# *******************************************************************************  

table(data_niubiz$TM_MEDIOS_Yape_o) #Tabla inicial

data_niubiz <- data_niubiz %>%
  mutate(
    TM_MEDIOS_Yape_6 = if_else(str_detect(TM_MEDIOS_Yape_o,"hijos"),1,TM_MEDIOS_Yape_6),
    TM_MEDIOS_Yape_66 = if_else(TM_MEDIOS_Yape_66 == 1 & str_detect(TM_MEDIOS_Yape_o,"hijos"),0,TM_MEDIOS_Yape_66),
    TM_MEDIOS_Yape_o = if_else(!is.na(TM_MEDIOS_Yape_o) & str_detect(TM_MEDIOS_Yape_o,"hijos"),NA,TM_MEDIOS_Yape_o),
    
    TM_MEDIOS_Yape_6 = if_else(str_detect(TM_MEDIOS_Yape_o,"solicitud de"),1,TM_MEDIOS_Yape_6),
    TM_MEDIOS_Yape_66 = if_else(TM_MEDIOS_Yape_66 == 1 & str_detect(TM_MEDIOS_Yape_o,"solicitud de"),0,TM_MEDIOS_Yape_66),
    TM_MEDIOS_Yape_o = if_else(!is.na(TM_MEDIOS_Yape_o) & str_detect(TM_MEDIOS_Yape_o,"solicitud de"),NA,TM_MEDIOS_Yape_o),
    
    TM_MEDIOS_Yape_o = if_else(str_detect(TM_MEDIOS_Yape_o,"banc"),"por un banco",TM_MEDIOS_Yape_o),
    TM_MEDIOS_Yape_o = if_else(str_detect(TM_MEDIOS_Yape_o,"trabajo"),"en el trabajo",TM_MEDIOS_Yape_o),
    TM_MEDIOS_Yape_o = if_else(str_detect(TM_MEDIOS_Yape_o,"uso"),"por uso",TM_MEDIOS_Yape_o))

table(data_niubiz$TM_MEDIOS_Yape_o) #Tabla limpia

# *******************************************************************************
# TM_MEDIOS_Plin_o
# *******************************************************************************  

table(data_niubiz$TM_MEDIOS_Plin_o) #Tabla inicial

data_niubiz <- data_niubiz %>%
  mutate(
    TM_MEDIOS_Plin_6 = if_else(str_detect(TM_MEDIOS_Plin_o,"familia"),1,TM_MEDIOS_Plin_6),
    TM_MEDIOS_Plin_66 = if_else(TM_MEDIOS_Plin_66 == 1 & str_detect(TM_MEDIOS_Plin_o,"familia"),0,TM_MEDIOS_Plin_66),
    TM_MEDIOS_Plin_o = if_else(!is.na(TM_MEDIOS_Plin_o) & str_detect(TM_MEDIOS_Plin_o,"familia"),NA,TM_MEDIOS_Plin_o),
    
    TM_MEDIOS_Plin_6 = if_else(str_detect(TM_MEDIOS_Plin_o,"conocidos"),1,TM_MEDIOS_Plin_6),
    TM_MEDIOS_Plin_66 = if_else(TM_MEDIOS_Plin_66 == 1 & str_detect(TM_MEDIOS_Plin_o,"conocidos"),0,TM_MEDIOS_Plin_66),
    TM_MEDIOS_Plin_o = if_else(!is.na(TM_MEDIOS_Plin_o) & str_detect(TM_MEDIOS_Plin_o,"conocidos"),NA,TM_MEDIOS_Plin_o),
    
    TM_MEDIOS_Plin_6 = if_else(str_detect(TM_MEDIOS_Plin_o,"hijos"),1,TM_MEDIOS_Plin_6),
    TM_MEDIOS_Plin_66 = if_else(TM_MEDIOS_Plin_66 == 1 & str_detect(TM_MEDIOS_Plin_o,"hijos"),0,TM_MEDIOS_Plin_66),
    TM_MEDIOS_Plin_o = if_else(!is.na(TM_MEDIOS_Plin_o) & str_detect(TM_MEDIOS_Plin_o,"hijos"),NA,TM_MEDIOS_Plin_o),
    
    TM_MEDIOS_Plin_6 = if_else(str_detect(TM_MEDIOS_Plin_o,"dueña"),1,TM_MEDIOS_Plin_6),
    TM_MEDIOS_Plin_66 = if_else(TM_MEDIOS_Plin_66 == 1 & str_detect(TM_MEDIOS_Plin_o,"dueña"),0,TM_MEDIOS_Plin_66),
    TM_MEDIOS_Plin_o = if_else(!is.na(TM_MEDIOS_Plin_o) & str_detect(TM_MEDIOS_Plin_o,"dueña"),NA,TM_MEDIOS_Plin_o),
    
    TM_MEDIOS_Plin_o = if_else(str_detect(TM_MEDIOS_Plin_o,"banc"),"por un banco",TM_MEDIOS_Plin_o),
    TM_MEDIOS_Plin_o = if_else(str_detect(TM_MEDIOS_Plin_o,"trabajo"),"en el trabajo",TM_MEDIOS_Plin_o),
    TM_MEDIOS_Plin_o = if_else(str_detect(TM_MEDIOS_Plin_o,"por gente que le paga"),"en el trabajo",TM_MEDIOS_Plin_o),
    TM_MEDIOS_Plin_o = if_else(str_detect(TM_MEDIOS_Plin_o,"uso"),"por uso",TM_MEDIOS_Plin_o))

table(data_niubiz$TM_MEDIOS_Plin_o) #Tabla limpia

# *******************************************************************************
# TM_MEDIOS_Otro_o
# ******************************************************************************* 

table(data_niubiz$TM_MEDIOS_Otro_o) #Tabla inicial

data_niubiz <- data_niubiz %>%
  mutate(
    TM_MEDIOS_Otro_o = if_else(str_detect(TM_MEDIOS_Otro_o,"trabajo"),"en el trabajo",TM_MEDIOS_Otro_o),
    TM_MEDIOS_Otro_o = if_else(str_detect(TM_MEDIOS_Otro_o,"banco"),"por un banco",TM_MEDIOS_Otro_o))

table(data_niubiz$TM_MEDIOS_Otro_o)

# *******************************************************************************
# TM_USO_o
# ******************************************************************************* 

table(data_niubiz$TM_USO_o) #Tabla inicial sin ajustes


# *******************************************************************************
# TM_USO_A_o
# ******************************************************************************* 

table(data_niubiz$TM_USO_A_o) #Tabla inicial sin ajustes


# *******************************************************************************
# MOT_PREF_o
# ******************************************************************************* 

table(data_niubiz$MOT_PREF_o) #Tabla inicial

data_niubiz <- data_niubiz %>%
  mutate(
    MOT_PREF_o = case_when(
      str_detect(MOT_PREF_o,"clientes|cliente|gente") & str_detect(MOT_PREF_o,"conocen|prefieren|conocida|piden|común|usa") ~ "los clientes la conocen y prefieren",
      str_detect(MOT_PREF_o,"facil|fácil|rápida|rapida|simplicidad") ~ "es rápida y fácil de usar",
      str_detect(MOT_PREF_o, "comision|comisiones") ~ "no cobra comisiones",
      str_detect(MOT_PREF_o, "sonido") ~ "avisa con un sonido al celular",
      TRUE ~ MOT_PREF_o
    )
  )

table(data_niubiz$MOT_PREF_o) #Tabla limpia


# *******************************************************************************
# NO_USO_NIUBIZ_o
# ******************************************************************************* 

table(data_niubiz$NO_USO_NIUBIZ_o) #Tabla inicial

data_niubiz <- data_niubiz %>%
  mutate(
    NO_USO_NIUBIZ_o = str_replace(NO_USO_NIUBIZ_o,"probehedor","proveedor")
    )
  
# *******************************************************************************
# MOT_RECHAZO_o
# ******************************************************************************* 

table(data_niubiz$MOT_RECHAZO_o)


data_niubiz <- data_niubiz %>%
  mutate(
    MOT_RECHAZO_o = case_when(
      # Falta de interés
      grepl("no está interesado|interesada", MOT_RECHAZO_o, ignore.case = TRUE) ~ "No interesado",
      
      # No aplica / Ninguna razón
      grepl("no aplica|ninguna|ninguno", MOT_RECHAZO_o, ignore.case = TRUE) ~ "No aplica / Ninguna razón",
      
      # No especifica
      grepl("no especifica|no seleccionó", MOT_RECHAZO_o, ignore.case = TRUE) ~ "No especifica",
      
      # Solo por trabajo (expresiones variadas)
      grepl("solo.*trabajo|por trabajo|tema de trabajo", MOT_RECHAZO_o, ignore.case = TRUE) ~ "Solo por trabajo",
      
      # Solo usa un proveedor específico
      grepl("izipay", MOT_RECHAZO_o, ignore.case = TRUE) ~ "Solo usa Izipay",
      grepl("culqi|culque", MOT_RECHAZO_o, ignore.case = TRUE) ~ "Solo usa Culqi",
      grepl("niubix", MOT_RECHAZO_o, ignore.case = TRUE) ~ "Solo usa Niubiz",
      
      # Problemas técnicos
      grepl("cobros fantasmas|plataforma se cae|se cuelga|suele fallar|se desconectaba", MOT_RECHAZO_o, ignore.case = TRUE) ~ "Problemas técnicos/servicio",
      
      # Mala atención / postventa
      grepl("no fueron responsables|no solucionan|no hay respuesta", MOT_RECHAZO_o, ignore.case = TRUE) ~ "Mala atención / postventa",
      
      # Problema con aparato
      grepl("aparato|conectar al wifi", MOT_RECHAZO_o, ignore.case = TRUE) ~ "Problema con aparato / conexión",
      
      # Falta de conocimiento
      grepl("no conoce|falta conocimiento|menos conocido|los clientes lo conocen más", MOT_RECHAZO_o, ignore.case = TRUE) ~ "Falta de conocimiento",
      
      # Todas o cualquiera
      grepl("cualquiera|todas", MOT_RECHAZO_o, ignore.case = TRUE) ~ " Usaría cualquiera / Todas",
      
      # Por defecto dejar el texto original
      TRUE ~ MOT_RECHAZO_o
    )
  )

table(data_niubiz$MOT_RECHAZO_o)


# *******************************************************************************
# NIUBIZ_FREC_PROB_USO_o
# ******************************************************************************* 

table(data_niubiz$NIUBIZ_FREC_PROB_USO_o)

data_niubiz <- data_niubiz %>%
  mutate(NIUBIZ_FREC_PROB_USO_o = if_else(str_detect(NIUBIZ_FREC_PROB_USO_o,"una"), "en una ocasión", NIUBIZ_FREC_PROB_USO_o)
  )

# *******************************************************************************
# NIUBIZ_TIP_PROB_EXP_o
# ******************************************************************************* 

table(data_niubiz$NIUBIZ_TIP_PROB_EXP_o)


data_niubiz <- data_niubiz %>%
  mutate(NIUBIZ_TIP_PROB_EXP_o = if_else(str_detect(NIUBIZ_TIP_PROB_EXP_o,"demoran|demoraban|demora"), "los depósitos se demoran", NIUBIZ_TIP_PROB_EXP_o)
  )

# *******************************************************************************
# IZIPAY_FREC_PROB_USO_o
# ******************************************************************************* 

table(data_niubiz$IZIPAY_FREC_PROB_USO_o)

data_niubiz <- data_niubiz %>%
  mutate(
    IZIPAY_FREC_PROB_USO_o = case_when(
      # Muy frecuente
      grepl("5 veces|dos veces al mes|3 veces en un mes", IZIPAY_FREC_PROB_USO_o, ignore.case = TRUE) ~ "Muy frecuente (varias veces al mes)",
      
      # Frecuente
      grepl("cada dos meses|tres meses|trimestral", IZIPAY_FREC_PROB_USO_o, ignore.case = TRUE) ~ "Frecuente (cada 2-3 meses)",
      
      # Ocasional
      grepl("una sola vez|solo una vez|una vez al año|una vez cada mucho tiempo|hace meses|más de 3 meses|más de un año", 
            IZIPAY_FREC_PROB_USO_o, ignore.case = TRUE) ~ "Ocasional",
      
      # Alguna vez / puntual
      grepl("solo fueron 3 veces|un par de veces|al inicio|al principio|algún momento|solo 2 veces", 
            IZIPAY_FREC_PROB_USO_o, ignore.case = TRUE) ~ "Alguna vez / puntual",
      
      # Si no corresponde a frecuencia → se mantiene la respuesta original
      TRUE ~ IZIPAY_FREC_PROB_USO_o
    )
  )

# *******************************************************************************
# IZIPAY_TIP_PROB_EXP_o
# ******************************************************************************* 

table(data_niubiz$IZIPAY_TIP_PROB_EXP_o)

data_niubiz <- data_niubiz %>%
  mutate(
    IZIPAY_TIP_PROB_EXP_o = if_else(str_detect(IZIPAY_TIP_PROB_EXP_o,"depositar|deposito|depósito|depósitos"), "demoras en los depósitos",
                                    IZIPAY_TIP_PROB_EXP_o)
  )

table(data_niubiz$IZIPAY_TIP_PROB_EXP_o)

# *******************************************************************************
# CULQI_FREC_PROB_USO_o
# ******************************************************************************* 

table(data_niubiz$CULQI_FREC_PROB_USO_o) # Sin cambios

# *******************************************************************************
# CULQI_TIP_PROB_EXP_o
# ******************************************************************************* 

table(data_niubiz$CULQI_TIP_PROB_EXP_o) # Sin cambios

# *******************************************************************************
# NIUBIZ_FREC_ABO_o
# *******************************************************************************

table(data_niubiz$NIUBIZ_FREC_ABO_o)

data_niubiz <- data_niubiz %>%
  mutate(
    NIUBIZ_FREC_ABO_o = case_when(
      # Muy ocasional
      grepl("una vez|una ocasión|ocurrió solo una vez", NIUBIZ_FREC_ABO_o, ignore.case = TRUE) ~ "Muy ocasional (una sola vez)",
      
      # Ocasional (pocas veces)
      grepl("un par de veces|algunas veces", NIUBIZ_FREC_ABO_o, ignore.case = TRUE) ~ "Ocasional (pocas veces)",
      
      # Recurrente anual
      grepl("dos veces al año|una vez al año", NIUBIZ_FREC_ABO_o, ignore.case = TRUE) ~ "Recurrente anual",
      
      # Dejar igual si no corresponde a frecuencia
      TRUE ~ NIUBIZ_FREC_ABO_o
    )
  )

table(data_niubiz$NIUBIZ_FREC_ABO_o)

# *******************************************************************************
# NIUBIZ_INCO_ABO_o
# *******************************************************************************

table(data_niubiz$NIUBIZ_INCO_ABO_o) # Sin cambios


# *******************************************************************************
# IZIPAY_FREC_ABO_o
# *******************************************************************************

table(data_niubiz$IZIPAY_FREC_ABO_o)

data_niubiz <- data_niubiz %>%
  mutate(
    IZIPAY_FREC_ABO_o = case_when(
      # MUY FRECUENTE (varias veces al mes)
      grepl("veces al mes|\\bmensual\\b", IZIPAY_FREC_ABO_o, ignore.case = TRUE) ~ 
        "Muy frecuente (varias veces al mes)",
      
      # FRECUENTE (cada 2-3 meses)
      grepl("cada\\s*3\\s*meses|cada\\s*tres\\s*meses|cada\\s*dos\\s*meses|hace\\s*meses\\s*cada\\s*3\\s*meses",
            IZIPAY_FREC_ABO_o, ignore.case = TRUE) ~ 
        "Frecuente (cada 2-3 meses)",
      
      # OCASIONAL (pocas veces)
      grepl("algunas veces|dos oportunidades|dos ocasiones|un par de veces|excepcionalmente|hace\\s*meses\\b|hace\\s*2\\s*meses|en algún momento",
            IZIPAY_FREC_ABO_o, ignore.case = TRUE) ~ 
        "Ocasional (pocas veces)",
      
      # MUY OCASIONAL (una sola vez)
      grepl("\\buna vez\\b|una ocasión|una vez un inconveniente", 
            IZIPAY_FREC_ABO_o, ignore.case = TRUE) ~ 
        "Muy ocasional (una sola vez)",
      
      # PUNTUAL / INICIO (onboarding)
      grepl("^al\\s*principio|inicio del servicio|acloplarse", 
            IZIPAY_FREC_ABO_o, ignore.case = TRUE) ~ 
        "Puntual (al inicio / adaptación)",
      
      # Si NO es frecuencia, se deja tal cual
      TRUE ~ IZIPAY_FREC_ABO_o
    )
  )

table(data_niubiz$IZIPAY_FREC_ABO_o)

# *******************************************************************************
# IZIPAY_INCO_ABO_o
# *******************************************************************************

table(data_niubiz$IZIPAY_INCO_ABO_o) # Sin cambios

# *******************************************************************************
# CULQI_FREC_ABO_o
# *******************************************************************************

table(data_niubiz$CULQI_FREC_ABO_o)

data_niubiz <- data_niubiz %>%
  mutate(
    CULQI_FREC_ABO_o = case_when(
      # Muy ocasional (una sola vez)
      grepl("^1 vez|una vez|muy ocasional", CULQI_FREC_ABO_o, ignore.case = TRUE) ~ 
        "Muy ocasional (una sola vez)",
      
      # Ocasional (pocas veces)
      grepl("un par de veces", CULQI_FREC_ABO_o, ignore.case = TRUE) ~ 
        "Ocasional (pocas veces)",
      
      # Ocasional (tiempo pasado)
      grepl("hace muchos meses", CULQI_FREC_ABO_o, ignore.case = TRUE) ~ 
        "Ocasional (hace meses)",
      
      # Si no es frecuencia → se deja original
      TRUE ~ CULQI_FREC_ABO_o
    )
  )

table(data_niubiz$CULQI_FREC_ABO_o)

# *******************************************************************************
# CULQI_INCO_ABO_o
# *******************************************************************************

table(data_niubiz$CULQI_INCO_ABO_o) # sin obs

# *******************************************************************************
# TEMP_ABO_o
# *******************************************************************************

table(data_niubiz$TEMP_ABO_o)

data_niubiz <- data_niubiz %>%
  mutate(
    TEMP_ABO_o = if_else(str_detect(TEMP_ABO_o,"instante"),"al instante",TEMP_ABO_o)
  )

table(data_niubiz$TEMP_ABO_o)

# *******************************************************************************
# REP_ABO_o
# *******************************************************************************

table(data_niubiz$REP_ABO_o)

data_niubiz <- data_niubiz %>%
  mutate(
    REP_ABO_o = if_else(str_detect(REP_ABO_o,"mismo"),"el mismo día",TEMP_ABO_o)
  )

table(data_niubiz$REP_ABO_o)

# *******************************************************************************
# NOTF_ABO_o
# *******************************************************************************

table(data_niubiz$NOTF_ABO_o) # Sin cambios

# *******************************************************************************
# USO_BANCOS_o
# *******************************************************************************

table(data_niubiz$USO_BANCOS_o)

data_niubiz <- data_niubiz %>%
  mutate(
    USO_BANCOS_o = case_when(
      # Banco de la Nación
      grepl("nacion|nación", USO_BANCOS_o, ignore.case = TRUE) ~ "Banco de la Nación",
      
      # Caja Cusco
      grepl("caja cusco", USO_BANCOS_o, ignore.case = TRUE) ~ "Caja Cusco",
      
      # No respuesta / no especifica
      grepl("no brindo|no quiere|no quiso", USO_BANCOS_o, ignore.case = TRUE) ~ "No especifica",
      
      # Si no entra en ningún caso → mantener
      TRUE ~ USO_BANCOS_o
    )
  )

table(data_niubiz$USO_BANCOS_o)

# *******************************************************************************
# PRINC_BANCO_o
# *******************************************************************************

table(data_niubiz$PRINC_BANCO_o)


data_niubiz <- data_niubiz %>%
  mutate(
    PRINC_BANCO_o = case_when(
      # Banco de la Nación
      grepl("nacion|nación", PRINC_BANCO_o, ignore.case = TRUE) ~ "Banco de la Nación",
      
      # No respuesta / no especifica
      grepl("no accedio|no brindo|no desea|no quiere", PRINC_BANCO_o, ignore.case = TRUE) ~ "No especifica",
      
      # Si no entra en ningún caso → mantener
      TRUE ~ PRINC_BANCO_o
    )
  )

table(data_niubiz$PRINC_BANCO_o)

# *******************************************************************************
# MOT_NOUSO_o
# *******************************************************************************

table(data_niubiz$MOT_NOUSO_o) # sin cambios

# *******************************************************************************
# MED_COM_o
# *******************************************************************************

table(data_niubiz$MED_COM_o)

data_niubiz <- data_niubiz %>%
  mutate(
    MED_COM_o = case_when(
      # SMS / texto
      grepl("mensaje de texto|mensajería|sms", MED_COM_o, ignore.case = TRUE) ~ "SMS / Texto",
      
      # Presencial
      grepl("local|personalmente|asesor personal", MED_COM_o, ignore.case = TRUE) ~ "Presencial",
      
      # Documento físico
      grepl("documento físico", MED_COM_o, ignore.case = TRUE) ~ "Documento físico",
      
      # No desea comunicación
      grepl("ninguno|no me gustaría|prefiero que no", MED_COM_o, ignore.case = TRUE) ~ "No desea comunicación",
      
      # Dejar igual si no encaja
      TRUE ~ MED_COM_o
    )
  )

table(data_niubiz$MED_COM_o)

# Eliminar variables innecesarias

data_niubiz <- data_niubiz %>%
  select(-(contains("coords")), -c(audio_audit, devicephonenum, username,
                                   deviceid,device_info,duration,duration_minutes,
                                   VERI_1,VERI_2,instanceID,formdef_version,`atributo[1]/six`,
                                   n_no_validas,tamanio_empleados,DEP_str,cuota_valida,Cuota,
                                   n_en_segmento,lat,lon,lat_lon,part_valido,caseid,starttime,endtime,
                                   date_onlyday,date_time_today))



write_xlsx(data_niubiz, "data/base_niubiz_clean_nolab.xlsx")




