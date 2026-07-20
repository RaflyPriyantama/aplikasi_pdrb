# UI ringkas — komponen dipisahkan sesuai Algoritma 17.
source("R/00_ui_helpers.R", local = FALSE, encoding = "UTF-8")
source("R/01_header.R", local = FALSE, encoding = "UTF-8")
source("R/02_sidebar.R", local = FALSE, encoding = "UTF-8")
source("R/03_body.R", local = FALSE, encoding = "UTF-8")

ui <- dashboardPage(
  header,
  sidebar,
  body,
  title = "Eksplorasi PDRB",
  skin = "blue"
)

# Objek terakhir harus berupa UI Shiny agar kompatibel dengan mode ui.R/server.R.
ui
