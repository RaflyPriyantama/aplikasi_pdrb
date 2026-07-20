# Entry point aplikasi.
source("global.R", local = FALSE, encoding = "UTF-8")
source("ui.R", local = FALSE, encoding = "UTF-8")
source("server.R", local = FALSE, encoding = "UTF-8")

shiny::shinyApp(ui = ui, server = server)
