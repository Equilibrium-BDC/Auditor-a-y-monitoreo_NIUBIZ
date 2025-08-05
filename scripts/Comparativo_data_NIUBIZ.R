# Cargar los paquetes necesarios
library(googlesheets4)
library(dplyr)

gs4_auth(email = email)
# Enlace del Google Sheet "alertas 2025"
url <- "https://docs.google.com/spreadsheets/d/1RGhRLpn9JqASujLU7ofuddjJm1tD8_6OyigWuHNvSqc/edit?gid=0#gid=0"

# Leer la hoja llamada "alertas"
data_24 <- read_sheet(url, sheet = "Hoja 1")

library(dplyr)
library(purrr)
library(labelled)


# Filtrar TM_USO_1 == 1
set.seed(123)
data_niubiz_2024 <- data_24 |>
  filter(TM_USO_1 == 1) |>
  slice_sample(n = 50)

# 1. Motivo deja de usar Niubiz (NO_USO_NIUBIZ) - multiple
#--------------------------------------------

# Vector con las variables múltiples
variables_no_uso_24 <- c(
  "NO_USO_NIUBIZ_1",
  "NO_USO_NIUBIZ_2",
  "NO_USO_NIUBIZ_3",
  "NO_USO_NIUBIZ_4",
  "NO_USO_NIUBIZ_5",
  "NO_USO_NIUBIZ_6",
  "NO_USO_NIUBIZ_66"
)

# Crear la tabla resumen
no_uso_niubiz_24 <- variables_no_uso_24 %>%
  setNames(., .) %>%
  lapply(function(x) sum(data_niubiz_2024[[x]] == 1, na.rm = TRUE)) %>%  # ya no necesitas filtrar Exitos
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

data_niubiz_2024 <- data_niubiz_2024 %>%
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
nps_categorizado_long <- data_niubiz_2024 |>
  select(all_of(variables_categorizadas)) |>
  pivot_longer(
    cols = everything(),
    names_to = "solucion_pago",
    values_to = "categoria"
  )

# 3. Reetiquetar solucion_pago
nps_categorizado_long <- nps_categorizado_long |>
  mutate(solucion_pago = recode(solucion_pago,
                                "NPSNB" = "Niubiz/Vendemás",
                                "NPSIZI" = "Izipay",
                                "NPSCQ" = "Culqui",
                                "NPSOP" = "OpenPay",
                                "NPSYAPE" = "Yape",
                                "NPSPLIN" = "Plin"
  ))

# 4. Contar valores y calcular porcentaje por solución
nps_tabla_24 <- nps_categorizado_long |>
  filter(!is.na(categoria)) |>
  group_by(solucion_pago, categoria) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(solucion_pago) |>
  mutate(porcentaje = round(n / sum(n) * 100, 2)) |>
  ungroup()





