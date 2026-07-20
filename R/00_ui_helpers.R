# REVISI BERDASARKAN DOKUMEN ALGORITMA 1-16 DAN 17
# - Label periode tahunan ditampilkan sebagai "Tahun" (nilai internal tetap "Total").
# - Distribusi Data memakai histogram frekuensi rapat dengan kurva kernel.
# - ID input/output dipertahankan agar sinkron dengan server.
# - Modularisasi penuh Algoritma 17 diterapkan setelah fungsi dashboard dinyatakan stabil.

# Paket utama
library(shiny)
library(shinydashboard)

# Komponen dropdown standar.
# Dropdown ditempel ke body agar tidak tertutup card, box, atau grafik.
pdrb_selectize <- function(id, label, choices = NULL, selected = NULL,
                           multiple = FALSE, placeholder = NULL,
                           max_options = 1000) {
  shiny::selectizeInput(
    inputId = id,
    label = label,
    choices = choices,
    selected = selected,
    multiple = multiple,
    options = list(
      dropdownParent = "body",
      placeholder = placeholder,
      maxOptions = max_options,
      closeAfterSelect = !multiple,
      plugins = if (multiple) list("remove_button") else list()
    )
  )
}

page_intro <- function(eyebrow, title, description = NULL, icon_name) {
  icon_node <- if (identical(icon_name, "custom-location")) {
    tags$span(class = "custom-location-icon")
  } else {
    icon(icon_name)
  }
  
  div(
    class = "page-intro",
    div(class = "page-intro-icon", icon_node),
    div(
      class = "page-intro-copy",
      span(class = "eyebrow", eyebrow),
      h2(title),
      if (!is.null(description) && nzchar(description)) p(description)
    )
  )
}

step_card <- function(number, title, description) {
  div(
    class = "step-card",
    span(class = "step-number", number),
    div(
      class = "step-copy",
      strong(title),
      p(description)
    )
  )
}

quick_card <- function(icon_name, title, text = NULL, class = "") {
  icon_node <- if (identical(icon_name, "custom-location")) {
    tags$span(class = "custom-location-icon")
  } else {
    icon(icon_name)
  }
  
  div(
    class = paste("quick-card", class),
    div(class = "quick-card-icon", icon_node),
    div(
      class = "quick-card-copy",
      strong(title),
      if (!is.null(text) && nzchar(text)) p(text)
    )
  )
}

pill_badge <- function(text, icon_name = NULL) {
  span(
    class = "pill-badge",
    if (!is.null(icon_name)) icon(icon_name),
    span(text)
  )
}

advanced_filter <- function(summary_label, ...) {
  tags$details(
    open = if (summary_label %in% c("Sektor")) "open" else NULL,
    class = "advanced-filter-details",
    tags$summary(tagList(icon("chevron-right"), span(summary_label))),
    div(class = "advanced-filter-content", ...)
  )
}



# Output Plotly standar dengan lapisan loading. Grafik baru ditampilkan setelah
# ukuran container dan layout Plotly sudah stabil di browser.
pdrb_plotly_output <- function(output_id, height = 400,
                               loading_text = "Memuat grafik...") {
  height_css <- if (is.numeric(height)) {
    paste0(height, "px")
  } else {
    as.character(height)
  }

  shiny::div(
    id = paste0(output_id, "_shell"),
    class = "pdrb-plot-shell pdrb-plot-loading",
    style = paste0("--pdrb-plot-height:", height_css, ";"),
    `data-loading-text` = loading_text,
    shiny::div(
      class = "pdrb-plot-loading-overlay",
      role = "status",
      `aria-live` = "polite",
      shiny::div(class = "pdrb-plot-spinner", `aria-hidden` = "true"),
      shiny::span(class = "pdrb-plot-loading-text", loading_text)
    ),
    plotly::plotlyOutput(output_id, height = height_css)
  )
}

# Komponen khusus Menu Bantuan V9.17.
help_icon_node <- function(icon_name) {
  if (identical(icon_name, "custom-location")) {
    return(tags$span(class = "custom-location-icon"))
  }

  # Gunakan nama ikon Font Awesome yang aman untuk versi shinydashboard.
  icon_alias <- c(
    "file-text-o" = "file-text",
    "calendar-check-o" = "calendar",
    "calendar-times-o" = "calendar",
    "file-pdf-o" = "download",
    "html5" = "file-text",
    "money" = "bar-chart",
    "file-excel-o" = "table"
  )

  safe_icon <- if (icon_name %in% names(icon_alias)) {
    unname(icon_alias[[icon_name]])
  } else {
    icon_name
  }

  icon(safe_icon)
}

help_step_card <- function(number, icon_name, title, text, note = NULL) {
  div(
    class = "help-step-card",
    div(
      class = "help-step-top",
      span(class = "help-step-number", number),
      span(class = "help-step-icon", help_icon_node(icon_name))
    ),
    h4(title),
    p(text),
    if (!is.null(note) && nzchar(note)) span(class = "help-step-note", note)
  )
}

help_item_card <- function(icon_name, title, text, class = "") {
  div(
    class = paste("help-item-card", class),
    div(class = "help-item-icon", help_icon_node(icon_name)),
    div(
      class = "help-item-copy",
      strong(title),
      p(text)
    )
  )
}

help_menu_card <- function(icon_name, title, text) {
  div(
    class = "help-menu-card",
    div(class = "help-menu-icon", help_icon_node(icon_name)),
    div(
      class = "help-menu-copy",
      strong(title),
      p(text)
    )
  )
}

help_accordion_item <- function(icon_name, title, text) {
  tags$details(
    class = "help-accordion-item",
    tags$summary(
      help_icon_node(icon_name),
      span(title),
      icon("chevron-down", class = "help-accordion-chevron")
    ),
    div(class = "help-accordion-body", p(text))
  )
}
