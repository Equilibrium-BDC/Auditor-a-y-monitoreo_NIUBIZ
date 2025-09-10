### Exportar alertas

# Verificar si las credenciales están disponibles
if (!exists("temp_creds_file") || !file.exists(temp_creds_file)) {
  stop("No se encontraron las credenciales de Google Sheets. Asegúrate de cargarlas desde el script maestro.")
}

sheet <- tryCatch({
  gs4_get(id_alertas)
}, error = function(e) {
  stop("Error al conectar con el Google Sheet de alertas: ", e)
})

# Helper genérico para exportar a Google Sheets con manejo de errores y pausa opcional
export_sheet <- function(df, ss, sheet_name, label = sheet_name, pause = 0) {
  message(sprintf("Exportando %s...", label))
  tryCatch({
    sheet_write(df, ss = ss, sheet = sheet_name)
    message(sprintf("Datos de %s exportados correctamente.", label))
  }, error = function(e) {
    stop(sprintf("Error al exportar %s: %s", label, conditionMessage(e)))
  })
  if (pause > 0) Sys.sleep(pause)
}

# Llamadas usando el wrapper
export_sheet(alertas,             sheet, "alertas_2",            label = "alertas",                 pause = 5)
export_sheet(data,                sheet, "data_raw_2",           label = "datos crudos",            pause = 5)
export_sheet(data_40,             sheet, "data_niubiz_2025_2",   label = "data Niubiz 2025",        pause = 5)
export_sheet(no_uso_niubiz_25,    sheet, "no_uso_niubiz_2025_2", label = "no uso Niubiz 2025",      pause = 5)
export_sheet(data_niubiz_2024,    sheet, "data_niubiz_2024",     label = "data Niubiz 2024",        pause = 5)
export_sheet(no_uso_niubiz_24,    sheet, "no_uso_niubiz_2024",   label = "no uso Niubiz 2024",      pause = 5)
export_sheet(nps_tabla_25,        sheet, "nps_tabla_25_2",       label = "NPS 2025",                pause = 5)
export_sheet(nps_tabla_24,        sheet, "nps_tabla_24",         label = "NPS 2024",                pause = 5)
export_sheet(cuotas_ronda_2,      sheet, "cuotas_ronda_2",       label = "cuotas ronda 2",          pause = 5)

message("✅ Todos los datos fueron exportados exitosamente.")


