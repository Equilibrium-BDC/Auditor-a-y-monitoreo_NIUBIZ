### Exportar alertas

gs4_auth(email = email)

sheet_url <- "https://docs.google.com/spreadsheets/d/1sXXsPwjS1XnCqDIo9CV3hGVVY-vsitUeM8UechBRV5k/edit?gid=0#gid=0"
sheet <- gs4_get(sheet_url)

sheet_write(alertas, ss = sheet, sheet = "alertas_2")

sheet_write(data, ss = sheet, sheet = "data_raw_2")

sheet_write(data_40, ss = sheet, sheet = "data_niubiz_2025_2")

sheet_write(no_uso_niubiz_25, ss = sheet, sheet = "no_uso_niubiz_2025_2")

sheet_write(data_niubiz_2024, ss = sheet, sheet = "data_niubiz_2024")

sheet_write(no_uso_niubiz_24, ss = sheet, sheet = "no_uso_niubiz_2024")

sheet_write(nps_tabla_25, ss = sheet, sheet = "nps_tabla_25_2")
sheet_write(nps_tabla_24, ss = sheet, sheet = "nps_tabla_24")

sheet_write(cuotas_ronda_2, ss = sheet, sheet = "cuotas_ronda_2")

cat("Datos exportados exitosamente")
beepr::beep(sound = 2)


