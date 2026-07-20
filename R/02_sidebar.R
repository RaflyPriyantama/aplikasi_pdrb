sidebar <- dashboardSidebar(
  width = 270,
  sidebarMenu(
    id = "tabs",
    selected = "input_data",
    
    tags$li(class = "header sidebar-section-label", "WORKSPACE"),
    menuItem("Upload Data", tabName = "input_data", icon = icon("upload")),
    menuItem("Overview", tabName = "ringkasan", icon = icon("tachometer")),
    
    tags$li(class = "header sidebar-section-label", "ANALYTICS"),
    menuItem("Tren PDRB", tabName = "tren", icon = icon("line-chart")),
    menuItem("Distribusi Data", tabName = "kernel", icon = icon("bar-chart")),
    menuItem("Struktur Ekonomi", tabName = "struktur", icon = icon("pie-chart")),
    menuItem("Komparasi Wilayah", tabName = "perbandingan", icon = icon("balance-scale")),
    menuItem("Potensi Wilayah", tabName = "analisis_wilayah", icon = icon("map-marker", class = "sidebar-location-icon")),
    
    tags$li(class = "header sidebar-section-label", "OUTPUT"),
    menuItem("Tabel Data", tabName = "tabel", icon = icon("table")),
    menuItem("Laporan", tabName = "unduh_laporan", icon = icon("file-lines")),
    
    tags$li(class = "header sidebar-section-label", "SUPPORT"),
    menuItem("Bantuan", tabName = "penjelasan", icon = icon("question-circle"))
  ),
  conditionalPanel(
    condition = "output.data_ready === true && !['input_data', 'unduh_laporan', 'penjelasan'].includes(input.tabs)",
    tags$hr(class = "sidebar-divider"),
    div(
      class = "sidebar-filter modern-sidebar-filter",
      div(
        class = "filter-heading",
        icon("filter"),
        span("Filter Global")
      ),
      pdrb_selectize("kelompok", "Provinsi", choices = NULL, placeholder = "Pilih provinsi/agregat"),
      pdrb_selectize("wilayah", "Wilayah", choices = NULL, placeholder = "Pilih wilayah"),
      
      conditionalPanel(
        condition = "input.tabs == 'ringkasan'",
        advanced_filter(
          "Overview",
          pdrb_selectize(
            "tahun_global", "Tahun",
            choices = c("Tahun Terbaru" = "__LATEST__"),
            selected = "__LATEST__"
          ),
          pdrb_selectize(
            "periode_global", "Periode",
            choices = c(
              "Periode Terbaru" = "__LATEST__",
              "Tahun" = "Total",
              "Triwulan I" = "I",
              "Triwulan II" = "II",
              "Triwulan III" = "III",
              "Triwulan IV" = "IV"
            ),
            selected = "__LATEST__"
          ),
          pdrb_selectize(
            "jumlah_top_ringkasan",
            "Top Sektor",
            choices = c(
              "Top 5" = 5,
              "Top 10" = 10,
              "Top 17" = 17
            ),
            selected = 5,
            placeholder = "Pilih jumlah sektor"
          )
        )
      )
    )
  )
)
