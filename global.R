# Konfigurasi global Eksplorasi PDRB — implementasi Algoritma 1–17
options(
  shiny.maxRequestSize = 200 * 1024^2,
  stringsAsFactors = FALSE,
  scipen = 999
)

required_packages <- c(
  "shiny", "shinydashboard", "readxl", "dplyr", "tidyr", "purrr",
  "stringr", "tibble", "plotly", "DT", "scales", "openxlsx", "ggplot2", "knitr"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Package R belum terpasang: ", paste(missing_packages, collapse = ", "),
    ". Instal terlebih dahulu sebelum menjalankan aplikasi."
  )
}

source("R/10_core_data_indikator.R", local = FALSE, encoding = "UTF-8")
source("R/11_flexible_excel_reader.R", local = FALSE, encoding = "UTF-8")
source("R/90_quality_assurance.R", local = FALSE, encoding = "UTF-8")
source("R/99_security_deployment.R", local = FALSE, encoding = "UTF-8")
