server <- function(input, output, session) {
  message("Eksplorasi PDRB build: 2026-07-14-v9.19-dynamic-download-filenames")
  trend_year_filter_state <- reactiveVal(NULL)
  processed_upload_files <- reactiveVal(NULL)
  processed_upload_version <- reactiveVal(0L)

  # V9.0 — Mesin data bersama -------------------------------------------------
  # Seluruh menu membaca objek backend yang sama. File Excel, normalisasi, dan
  # indikator dasar hanya dibentuk ketika tombol Proses Data ditekan.
  shared_data_store <- reactiveValues(
    workbook = NULL,
    indicators = tibble::tibble(),
    regions = tibble::tibble(),
    validation = tibble::tibble(),
    data_version = 0L,
    processed_at = NULL
  )

  lq_result_cache <- new.env(parent = emptyenv())
  dlq_result_cache <- new.env(parent = emptyenv())
  shift_result_cache <- new.env(parent = emptyenv())

  clear_shared_analysis_cache <- function() {
    for (cache_env in list(lq_result_cache, dlq_result_cache, shift_result_cache)) {
      keys <- ls(envir = cache_env, all.names = TRUE)
      if (length(keys) > 0L) rm(list = keys, envir = cache_env)
    }
    invisible(NULL)
  }

  cache_key_v90 <- function(...) {
    values <- vapply(list(...), function(value) {
      if (is.null(value) || length(value) == 0L) return("<NULL>")
      paste(as.character(value), collapse = ",")
    }, character(1))
    paste(c(as.character(shared_data_store$data_version), values), collapse = "|")
  }

  cache_get_or_compute_v90 <- function(cache_env, key, compute) {
    if (exists(key, envir = cache_env, inherits = FALSE)) {
      return(get(key, envir = cache_env, inherits = FALSE))
    }
    value <- compute()
    assign(key, value, envir = cache_env)
    value
  }

  empty_workbook_result_v90 <- function() {
    list(
      data = tibble::tibble(),
      regions = tibble::tibble(),
      validation = tibble::tibble(),
      duplicate_diagnostics = tibble::tibble(),
      error = NULL,
      uploaded = FALSE,
      file_names = character(0),
      file_roles = character(0),
      province_file_names = character(0),
      kabkota_file_names = character(0)
    )
  }

  observeEvent(input$process_pdrb, {
    province_files <- isolate(input$file_pdrb_provinsi)
    kabkota_files <- isolate(input$file_pdrb_kabkota)

    has_province <- !is.null(province_files) && nrow(province_files) > 0
    has_kabkota <- !is.null(kabkota_files) && nrow(kabkota_files) > 0

    if (!has_province && !has_kabkota) {
      session$sendCustomMessage(
        "pdrbUploadLoading",
        list(
          action = "finish",
          success = FALSE,
          percent = 0,
          message = "Belum ada file dipilih.",
          detail = "Pilih minimal satu file provinsi/agregat atau kabupaten/kota."
        )
      )
      showNotification(
        "Pilih minimal satu file provinsi/agregat atau kabupaten/kota sebelum menekan Proses Data.",
        type = "warning", duration = 6
      )
      return(invisible(NULL))
    }

    empty_upload_tbl <- tibble::tibble(
      paths = character(), names = character(), roles = character()
    )

    province_tbl <- if (has_province) {
      tibble::tibble(
        paths = province_files$datapath,
        names = province_files$name,
        roles = "Provinsi/Agregat"
      )
    } else {
      empty_upload_tbl
    }

    kabkota_tbl <- if (has_kabkota) {
      tibble::tibble(
        paths = kabkota_files$datapath,
        names = kabkota_files$name,
        roles = "Kabupaten/Kota"
      )
    } else {
      empty_upload_tbl
    }

    files <- dplyr::bind_rows(province_tbl, kabkota_tbl)
    next_version <- isolate(processed_upload_version()) + 1L

    processed_upload_files(list(
      paths = files$paths,
      names = files$names,
      roles = files$roles,
      province_names = province_tbl$names,
      kabkota_names = kabkota_tbl$names,
      process_version = next_version
    ))
    processed_upload_version(next_version)

    # Paksa seluruh proses utama selesai di dalam klik Proses Data agar
    # loading dan persentase tidak berhenti sebelum data siap digunakan.
    process_result <- tryCatch(
      {
        result <- isolate(workbook_builder_v90())
        if (!is.null(result$error)) stop(result$error)

        session$sendCustomMessage(
          "pdrbUploadLoading",
          list(
            action = "progress",
            percent = 90,
            message = "Menghitung indikator PDRB...",
            detail = "90% • Menyusun PDRB, distribusi, pertumbuhan, indeks implisit, dan sumber pertumbuhan."
          )
        )

        prepared_data <- isolate(pdrb_data_builder_v90())

        # Publikasikan hasil secara atomik setelah seluruh pipeline berhasil.
        # Output lama tidak disentuh bila file baru gagal diproses.
        shared_data_store$workbook <- result
        shared_data_store$indicators <- prepared_data
        shared_data_store$regions <- if (is.null(result$regions)) tibble::tibble() else result$regions
        shared_data_store$validation <- if (is.null(result$validation)) tibble::tibble() else result$validation
        shared_data_store$data_version <- next_version
        shared_data_store$processed_at <- Sys.time()
        clear_shared_analysis_cache()
        message(
          "Shared data engine ready: version=", next_version,
          ", rows=", nrow(prepared_data),
          ", regions=", nrow(shared_data_store$regions)
        )

        session$sendCustomMessage(
          "pdrbUploadLoading",
          list(
            action = "progress",
            percent = 97,
            message = "Menyiapkan dashboard...",
            detail = "97% • Menyusun status validasi, wilayah, tahun, periode, dan pilihan filter."
          )
        )

        invisible(isolate(validation_with_context()))
        invisible(isolate(region_reference()))

        session$sendCustomMessage(
          "pdrbUploadLoading",
          list(
            action = "finish",
            success = TRUE,
            percent = 100,
            message = "Data selesai diproses.",
            detail = paste0(
              "100% • ", formatC(nrow(prepared_data), format = "f", digits = 0, big.mark = ",", decimal.mark = "."),
              " baris data siap digunakan pada dashboard."
            )
          )
        )
        TRUE
      },
      error = function(e) {
        session$sendCustomMessage(
          "pdrbUploadLoading",
          list(
            action = "finish",
            success = FALSE,
            percent = 100,
            message = "Data gagal diproses.",
            detail = conditionMessage(e)
          )
        )
        showNotification(
          paste("Data gagal diproses:", conditionMessage(e)),
          type = "error", duration = 8
        )
        FALSE
      }
    )

    invisible(process_result)
  }, ignoreInit = TRUE)

  active_files <- reactive({
    processed_upload_files()
  })

  output$process_upload_button_ui <- renderUI({
    province_files <- input$file_pdrb_provinsi
    kabkota_files <- input$file_pdrb_kabkota
    ready <- (!is.null(province_files) && nrow(province_files) > 0) ||
      (!is.null(kabkota_files) && nrow(kabkota_files) > 0)

    button_args <- list(
      inputId = "process_pdrb",
      label = "Proses Data",
      icon = icon("cogs"),
      class = "btn-primary btn-lg"
    )
    if (!ready) button_args$disabled <- "disabled"
    do.call(actionButton, button_args)
  })
  outputOptions(output, "process_upload_button_ui", suspendWhenHidden = FALSE)
  
  workbook_builder_v90 <- reactive({
    files <- active_files()

    if (is.null(files)) {
      return(list(
        data = tibble(), regions = tibble(), error = NULL,
        uploaded = FALSE, file_names = character(0)
      ))
    }

    session$sendCustomMessage(
      "pdrbUploadLoading",
      list(
        action = "start",
        percent = 0,
        message = "Memproses data PDRB...",
        detail = paste0("0% • Menyiapkan ", length(files$paths), " file untuk dibaca.")
      )
    )

    final_result <- withProgress(message = "Membaca file dan seluruh sheet PDRB...", value = 0, {
      tryCatch(
        {
          file_total <- length(files$paths)
          sheet_counts <- integer(file_total)

          setProgress(
            value = 0,
            detail = "0% • Menyiapkan pembacaan file Excel."
          )

          for (file_index in seq_along(files$paths)) {
            sheet_counts[file_index] <- tryCatch(
              nrow(build_sheet_index(files$paths[file_index])),
              error = function(e) length(readxl::excel_sheets(files$paths[file_index]))
            )

            inspect_percent <- max(1L, round((file_index / max(1L, file_total)) * 10))
            inspect_detail <- paste0(
              inspect_percent, "% • Memeriksa struktur file ",
              file_index, "/", file_total, ": ", files$names[file_index]
            )
            setProgress(value = inspect_percent / 100, detail = inspect_detail)
            session$sendCustomMessage(
              "pdrbUploadLoading",
              list(
                action = "progress",
                percent = inspect_percent,
                message = "Memeriksa file Excel...",
                detail = inspect_detail
              )
            )
          }

          total_steps <- max(1L, sum(sheet_counts, na.rm = TRUE))
          current_step <- 0L

          # Validasi slot dilakukan per sheet di reader. Sheet yang tidak sesuai
          # slot upload diabaikan tanpa menggagalkan sheet valid dalam file yang sama.
          update_upload_progress <- function(file_name, sheet_name, sheet_no, sheet_total, file_index, file_total) {
            current_step <<- min(total_steps, current_step + 1L)
            percent <- min(85L, 10L + round((current_step / total_steps) * 75))
            detail_text <- paste0(
              percent, "% • File ", file_index, "/", file_total,
              ": ", file_name,
              " • Sheet ", sheet_no, "/", sheet_total,
              " (", sheet_name, ")"
            )

            setProgress(
              value = percent / 100,
              detail = detail_text
            )
            session$sendCustomMessage(
              "pdrbUploadLoading",
              list(
                action = "progress",
                percent = percent,
                message = "Membaca file dan seluruh sheet PDRB...",
                detail = detail_text
              )
            )
          }

          result <- read_multiple_workbooks(
            files$paths,
            files$names,
            roles = files$roles,
            progress_callback = update_upload_progress
          )

          setProgress(
            value = 0.88,
            detail = "88% • Selesai membaca seluruh file dan sheet."
          )
          session$sendCustomMessage(
            "pdrbUploadLoading",
            list(
              action = "progress",
              percent = 88,
              message = "Menyelesaikan pembacaan data...",
              detail = "88% • Menyusun hasil validasi dan metadata wilayah."
            )
          )

          result$error <- NULL
          result$uploaded <- TRUE
          result$file_names <- files$names
          result$file_roles <- files$roles
          result$province_file_names <- files$province_names
          result$kabkota_file_names <- files$kabkota_names
          result
        },
        error = function(e) {
          list(
            data = tibble(), regions = tibble(), validation = tibble(), duplicate_diagnostics = tibble(), error = e$message,
            uploaded = TRUE, file_names = files$names, file_roles = files$roles,
            province_file_names = files$province_names, kabkota_file_names = files$kabkota_names
          )
        }
      )
    })

    final_result
  })
  
  
  pdrb_data_builder_v90 <- reactive({
    base_data <- workbook_builder_v90()$data
    if (nrow(base_data) == 0) return(base_data)
    raw_data <- base_data %>%
      filter(indikator %in% c("PDRB ADHB", "PDRB ADHK")) %>%
      canonicalize_pdrb_rows() %>%
      mutate(nilai = suppressWarnings(as.numeric(nilai))) %>%
      filter(!is.na(nilai), is.finite(nilai), abs(nilai) > 1e-12) %>%
      filter_available_pdrb_periods() %>%
      filter_complete_pdrb_value_periods()
    final_data <- bind_rows(
      raw_data,
      derive_distribution_indicators_v5(raw_data),
      derive_implicit_indicators_v5(raw_data),
      derive_growth_indicators_v5(raw_data),
      derive_source_growth_indicators_v5(raw_data)
    ) %>%
      canonicalize_pdrb_rows() %>%
      mutate(
        periode = factor(as.character(periode), levels = c("I", "II", "III", "IV", "Total"), ordered = TRUE),
        level = factor(as.character(level), levels = c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian", "Lainnya"))
      ) %>%
      arrange(kode_kelompok, kode_wilayah, indikator, level, source_row, tahun, periode)
    final_data
  })
  # Public accessors. Seluruh menu hanya membaca dari shared_data_store.
  workbook_result <- reactive({
    if (is.null(shared_data_store$workbook)) empty_workbook_result_v90() else shared_data_store$workbook
  })

  pdrb_data <- reactive({
    data <- shared_data_store$indicators
    if (is.null(data)) tibble::tibble() else data
  })

  region_reference <- reactive({
    data <- shared_data_store$regions
    if (is.null(data)) tibble::tibble() else data
  })


  # V9.19 — Nama file unduhan dinamis ----------------------------------------
  # Nama XLSX dan CSV dibentuk dari filter aktif. Karakter yang tidak valid
  # pada Windows dibersihkan, sedangkan nilai numerik dalam data tidak diubah.
  download_scalar_v919 <- function(x, default = NULL) {
    if (is.null(x) || length(x) == 0L) return(default)
    value <- trimws(as.character(x)[1])
    if (is.na(value) || !nzchar(value)) default else value
  }

  sanitize_download_part_v919 <- function(x) {
    value <- download_scalar_v919(x, "")
    value <- iconv(value, from = "", to = "ASCII//TRANSLIT", sub = "")
    if (is.na(value)) value <- ""
    value <- toupper(value)
    value <- gsub("&", " DAN ", value, fixed = TRUE)
    value <- gsub("%", " PERSEN ", value, fixed = TRUE)
    value <- gsub("[\\\\/:*?\"<>|]", " ", value)
    value <- gsub("[^A-Z0-9]+", "_", value)
    value <- gsub("_+", "_", value)
    gsub("^_+|_+$", "", value)
  }

  build_download_filename_v919 <- function(parts, extension = "xlsx") {
    values <- unlist(parts, use.names = FALSE)
    values <- values[!is.na(values) & nzchar(trimws(as.character(values)))]
    values <- vapply(values, sanitize_download_part_v919, character(1))
    values <- values[nzchar(values)]
    stem <- paste(values, collapse = "_")
    if (!nzchar(stem)) stem <- "DATA_PDRB"

    # Sisakan ruang untuk ekstensi dan hindari nama file terlalu panjang.
    if (nchar(stem, type = "chars") > 190L) {
      stem <- substr(stem, 1L, 190L)
      stem <- gsub("_+$", "", stem)
    }
    extension <- tolower(gsub("[^A-Za-z0-9]", "", extension))
    paste0(stem, ".", extension)
  }

  qualify_region_name_v919 <- function(label, code = NULL) {
    label <- download_scalar_v919(label, download_scalar_v919(code, "WILAYAH"))
    code <- download_scalar_v919(code, "")
    upper <- toupper(label)

    is_province_code <- nzchar(code) && grepl("00$", code)
    has_region_prefix <- grepl(
      "^(PROVINSI|KABUPATEN|KOTA|NASIONAL|INDONESIA)( |$)",
      upper
    )
    if (is_province_code && !has_region_prefix) paste("Provinsi", label) else label
  }

  download_region_name_v919 <- function(code) {
    code <- download_scalar_v919(code, "")
    data <- tryCatch(pdrb_data(), error = function(e) tibble::tibble())
    label <- character(0)
    if (nrow(data) > 0L && all(c("kode_wilayah", "wilayah") %in% names(data))) {
      label <- data %>%
        filter(as.character(kode_wilayah) == code) %>%
        distinct(wilayah) %>%
        pull(wilayah)
    }
    qualify_region_name_v919(if (length(label) > 0L) label[[1]] else code, code)
  }

  download_group_name_v919 <- function(code) {
    code <- download_scalar_v919(code, "")
    data <- tryCatch(pdrb_data(), error = function(e) tibble::tibble())
    label <- character(0)
    if (nrow(data) > 0L && all(c("kode_kelompok", "kelompok") %in% names(data))) {
      label <- data %>%
        filter(as.character(kode_kelompok) == code) %>%
        distinct(kelompok) %>%
        pull(kelompok)
    }
    qualify_region_name_v919(if (length(label) > 0L) label[[1]] else code, code)
  }

  download_period_part_v919 <- function(value, include_all = FALSE) {
    value <- download_scalar_v919(value, "")
    switch(
      value,
      "__ALL__" = if (isTRUE(include_all)) "Semua Periode" else NULL,
      "__QUARTERS__" = "Semua Triwulan",
      "I" = "Triwulan I",
      "II" = "Triwulan II",
      "III" = "Triwulan III",
      "IV" = "Triwulan IV",
      "Total" = "Tahun",
      if (nzchar(value)) value else NULL
    )
  }

  download_year_part_v919 <- function(value, include_all = FALSE) {
    value <- download_scalar_v919(value, "")
    if (identical(value, "__ALL__")) {
      if (isTRUE(include_all)) "Semua Tahun" else NULL
    } else if (nzchar(value)) {
      value
    } else {
      NULL
    }
  }

  analytics_filename_parts_v919 <- function(group, basis = NULL, index = NULL, growth = NULL) {
    group <- download_scalar_v919(group, "PDRB")
    basis <- download_scalar_v919(basis, "ADHK")
    index <- download_scalar_v919(index, "Indeks Implisit")
    growth <- download_scalar_v919(growth, "Q-to-Q")

    switch(
      group,
      "PDRB" = c("PDRB", basis),
      "Distribusi" = c("Distribusi PDRB", basis),
      "Pertumbuhan" = c("Pertumbuhan PDRB", basis, growth),
      "Indeks Implisit" = c(index),
      "Sumber Pertumbuhan" = c("Sumber Pertumbuhan", basis, growth),
      c(group, basis)
    )
  }

  validation_reference <- reactive({
    data <- shared_data_store$validation
    if (is.null(data)) tibble::tibble() else data
  })

  output$upload_attempted <- reactive({
    isTRUE(workbook_result()$uploaded)
  })
  outputOptions(output, "upload_attempted", suspendWhenHidden = FALSE)

  validation_with_context <- reactive({
    result <- workbook_result()
    validation <- validation_reference()
    data <- result$data
    regions <- result$regions

    if (is.null(validation) || nrow(validation) == 0) {
      validation <- tibble::tibble(
        source_file = character(), sheet_name = character(), kode_wilayah = character(),
        status = character(), reason = character(), jumlah_baris = integer(),
        detected_table_type = character()
      )
    }

    if (!is.null(data) && nrow(data) > 0) {
      data_summary <- data %>%
        mutate(
          source_file = as.character(source_file),
          source_sheet = as.character(source_sheet),
          kode_wilayah = as.character(kode_wilayah),
          periode = as.character(periode),
          jenis_data = dplyr::recode(as.character(indikator),
            "PDRB ADHB" = "ADHB", "PDRB ADHK" = "ADHK", .default = as.character(indikator)
          )
        ) %>%
        group_by(source_file, source_sheet, kode_wilayah) %>%
        summarise(
          jenis_data = paste(sort(unique(jenis_data)), collapse = ", "),
          tahun_tersedia = {
            yy <- sort(unique(as.integer(tahun)))
            yy <- yy[!is.na(yy)]
            if (length(yy) == 0) "-" else if (length(yy) == 1) as.character(yy) else paste0(min(yy), "–", max(yy))
          },
          periode_tersedia = {
            pp <- c("I", "II", "III", "IV", "Total")
            ada <- pp[pp %in% unique(periode)]
            lab <- c(I = "Triwulan I", II = "Triwulan II", III = "Triwulan III", IV = "Triwulan IV", Total = "Tahun")
            if (length(ada) == 0) "-" else paste(unname(lab[ada]), collapse = ", ")
          },
          jumlah_baris_data = dplyr::n(),
          .groups = "drop"
        )
    } else {
      data_summary <- tibble::tibble(
        source_file = character(), source_sheet = character(), kode_wilayah = character(),
        jenis_data = character(), tahun_tersedia = character(), periode_tersedia = character(),
        jumlah_baris_data = integer()
      )
    }

    if (!is.null(regions) && nrow(regions) > 0) {
      region_summary <- regions %>%
        transmute(
          source_file = as.character(source_file),
          kode_wilayah = as.character(kode_wilayah),
          wilayah = as.character(wilayah),
          level_wilayah = as.character(jenis_wilayah)
        ) %>%
        distinct(source_file, kode_wilayah, .keep_all = TRUE)
    } else {
      region_summary <- tibble::tibble(
        source_file = character(), kode_wilayah = character(),
        wilayah = character(), level_wilayah = character()
      )
    }

    validation %>%
      mutate(
        source_file = as.character(source_file),
        sheet_name = as.character(sheet_name),
        kode_wilayah = as.character(kode_wilayah),
        detected_table_type = as.character(detected_table_type)
      ) %>%
      left_join(data_summary, by = c("source_file", "sheet_name" = "source_sheet", "kode_wilayah")) %>%
      left_join(region_summary, by = c("source_file", "kode_wilayah")) %>%
      mutate(
        wilayah = dplyr::coalesce(wilayah, "Tidak terdeteksi"),
        level_wilayah = dplyr::coalesce(level_wilayah, "Tidak terdeteksi"),
        jenis_data = dplyr::coalesce(detected_table_type, jenis_data, "Tidak dikenali"),
        tahun_tersedia = dplyr::coalesce(tahun_tersedia, "-"),
        periode_tersedia = dplyr::coalesce(periode_tersedia, "-"),
        jumlah_baris_data = dplyr::coalesce(jumlah_baris_data, as.integer(jumlah_baris), 0L)
      )
  })

  failed_file_rows <- reactive({
    result <- workbook_result()
    failed <- result$failed_files
    if (!is.null(failed) && length(failed) > 0) {
      parsed <- stringr::str_match(as.character(failed), "^`([^`]+)`: (.*)$")
      return(tibble::tibble(
        source_file = dplyr::coalesce(parsed[, 2], as.character(failed)),
        sheet_name = "Seluruh file",
        kode_wilayah = NA_character_,
        status = "File Gagal Dibaca",
        reason = dplyr::coalesce(parsed[, 3], as.character(failed)),
        jumlah_baris = 0L,
        jenis_data = "Tidak dikenali",
        tahun_tersedia = "-",
        periode_tersedia = "-",
        jumlah_baris_data = 0L,
        wilayah = "Tidak terdeteksi",
        level_wilayah = "Tidak terdeteksi",
        detected_table_type = "File Excel"
      ))
    }
    if (!is.null(result$error) && length(result$file_names) > 0 && nrow(validation_reference()) == 0) {
      return(tibble::tibble(
        source_file = as.character(result$file_names),
        sheet_name = "Seluruh file",
        kode_wilayah = NA_character_,
        status = "File Gagal Dibaca",
        reason = rep(paste(result$error, collapse = " "), length(result$file_names)),
        jumlah_baris = 0L,
        jenis_data = "Tidak dikenali",
        tahun_tersedia = "-",
        periode_tersedia = "-",
        jumlah_baris_data = 0L,
        wilayah = "Tidak terdeteksi",
        level_wilayah = "Tidak terdeteksi",
        detected_table_type = "File Excel"
      ))
    }
    tibble::tibble()
  })
  
  valid_lq_years <- reactive({
    data <- pdrb_data()
    req(nrow(data) > 0, input$kelompok, input$wilayah)
    dasar <- if (identical(as.character(input$tabs)[1], "analisis_wilayah") && !is.null(input$dasar_harga_lq_tren)) {
      as.character(input$dasar_harga_lq_tren)[1]
    } else if (!is.null(input$dasar_harga_lq_tabel)) {
      as.character(input$dasar_harga_lq_tabel)[1]
    } else if (!is.null(input$dasar_harga_lq_tren)) {
      as.character(input$dasar_harga_lq_tren)[1]
    } else {
      "ADHK"
    }
    if (is.na(dasar) || !dasar %in% c("ADHB", "ADHK")) dasar <- "ADHK"
    
    lq_data <- tryCatch(
      shared_lq_data_v90(data, input$kelompok, input$wilayah, "Kategori Utama", dasar),
      error = function(e) tibble::tibble()
    )
    if (nrow(lq_data) > 0 && all(c("tahun", "LQ") %in% names(lq_data))) {
      years <- lq_data %>%
        filter(!is.na(LQ), is.finite(LQ)) %>%
        distinct(tahun) %>%
        pull(tahun)
      years <- sort(unique(as.integer(years)))
      return(years[!is.na(years)])
    }
    
    indikator_pilih <- paste("PDRB", dasar)
    target_years <- data %>%
      mutate(level = as.character(level), periode = as.character(periode)) %>%
      filter(kode_wilayah == input$wilayah, indikator == indikator_pilih, level == "Total PDRB", kode_kategori == "PDRB", !is.na(nilai)) %>%
      distinct(tahun) %>% pull(tahun)
    acuan_years <- data %>%
      mutate(level = as.character(level), periode = as.character(periode)) %>%
      filter(kode_wilayah == input$kelompok, indikator == indikator_pilih, level == "Total PDRB", kode_kategori == "PDRB", !is.na(nilai)) %>%
      distinct(tahun) %>% pull(tahun)
    years <- sort(intersect(as.integer(target_years), as.integer(acuan_years)))
    years[!is.na(years)]
  })
  
  # Tahun tahunan dianggap lengkap jika baris Total tersedia dan:
  # (a) tidak ada data triwulanan sama sekali (file tahunan murni), atau
  # (b) Triwulan I-IV semuanya tersedia. Total parsial seperti Tahun 2026 yang
  # baru berisi Triwulan I tidak boleh dipakai untuk DLQ/Extended Shift Share tahunan.
  complete_annual_years_v90 <- function(data, region_code, indicator_name) {
    if (is.null(data) || nrow(data) == 0) return(integer(0))
    quarters <- c("I", "II", "III", "IV")

    years <- data %>%
      mutate(
        level = as.character(level),
        periode = as.character(periode),
        kode_wilayah = as.character(kode_wilayah),
        nilai = suppressWarnings(as.numeric(nilai))
      ) %>%
      filter(
        kode_wilayah == as.character(region_code),
        indikator == indicator_name,
        level == "Total PDRB",
        kode_kategori == "PDRB",
        periode %in% c(quarters, "Total"),
        !is.na(nilai),
        is.finite(nilai)
      ) %>%
      canonicalize_pdrb_rows() %>%
      group_by(tahun) %>%
      summarise(
        .has_total = any(periode == "Total"),
        .quarter_count = dplyr::n_distinct(periode[periode %in% quarters]),
        .groups = "drop"
      ) %>%
      filter(.has_total, .quarter_count == 0L | .quarter_count == 4L) %>%
      pull(tahun)

    years <- sort(unique(suppressWarnings(as.integer(years))))
    years[!is.na(years)]
  }

  valid_dlq_years <- reactive({
    data <- pdrb_data()
    req(nrow(data) > 0, input$kelompok, input$wilayah)
    indikator_pilih <- "PDRB ADHK"

    target_years <- complete_annual_years_v90(
      data, input$wilayah, indikator_pilih
    )
    acuan_years <- complete_annual_years_v90(
      data, input$kelompok, indikator_pilih
    )

    years <- sort(intersect(as.integer(target_years), as.integer(acuan_years)))
    years[!is.na(years)]
  })
  
  valid_shift_years <- reactive({
    years <- sort(unique(as.integer(valid_dlq_years())))
    years <- years[!is.na(years)]
    if (length(years) <= 1) return(integer(0))
    years[-1]
  })
  
  valid_shift_start_years <- reactive({
    years <- sort(unique(as.integer(valid_dlq_years())))
    years <- years[!is.na(years)]
    if (length(years) <= 1) return(integer(0))
    years[-length(years)]
  })
  
  valid_year_choices <- function(years) {
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    stats::setNames(as.character(years), as.character(years))
  }
  
  output$data_ready <- reactive({
    result <- workbook_result()
    is.null(result$error) && nrow(result$data) > 0
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
  
  # Filter card Analytics berdiri sendiri; tidak lagi disinkronkan ke input kanonik.

  category_choices_for_level <- function(level_value) {
    data <- pdrb_data()
    if (!is.data.frame(data) || nrow(data) == 0 || is.null(input$wilayah)) {
      return(list(choices = character(0), values = character(0)))
    }
    level_value <- as.character(level_value)[1]
    if (is.na(level_value) || !nzchar(level_value)) level_value <- "Semua"
    category_data <- data %>%
      filter(kode_wilayah == input$wilayah) %>%
      mutate(level = as.character(level))
    if (!identical(level_value, "Semua")) {
      category_data <- category_data %>% filter(level == level_value)
    }
    if (!"kode_utama" %in% names(category_data)) category_data$kode_utama <- NA_character_
    if (!"source_row" %in% names(category_data)) category_data$source_row <- NA_integer_
    category_data <- category_data %>%
      group_by(item_id, kategori_label, level, kode_kategori, kode_utama) %>%
      summarise(
        source_row = if (all(is.na(source_row))) NA_integer_ else min(source_row, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      category_hierarchy_arrange()
    list(
      choices = stats::setNames(category_data$item_id, category_data$kategori_label),
      values = as.character(category_data$item_id)
    )
  }

  update_card_sector <- function(
    level_value,
    input_id,
    current_value = NULL,
    preferred_value = NULL
  ) {
    info <- category_choices_for_level(level_value)
    if (length(info$values) == 0L) {
      updateSelectizeInput(session, input_id, choices = character(0), selected = NULL, server = TRUE)
      return(invisible(NULL))
    }

    selected <- if (
      !is.null(current_value) &&
      length(current_value) == 1L &&
      current_value %in% info$values
    ) {
      current_value
    } else if (
      !is.null(preferred_value) &&
      length(preferred_value) == 1L &&
      preferred_value %in% info$values
    ) {
      preferred_value
    } else {
      info$values[[1]]
    }

    updateSelectizeInput(
      session, input_id,
      choices = info$choices,
      selected = selected,
      server = TRUE
    )
  }

  default_total_pdrb_sector <- "Total PDRB__PDRB__PRODUK DOMESTIK REGIONAL BRUTO"

  observe({
    req(input$wilayah, input$tren_tingkat_analisis)
    update_card_sector(
      input$tren_tingkat_analisis,
      "tren_jenis_sektor",
      isolate(input$tren_jenis_sektor),
      preferred_value = default_total_pdrb_sector
    )
  })
  observe({
    req(input$wilayah, input$distribusi_tingkat_analisis)
    update_card_sector(
      input$distribusi_tingkat_analisis,
      "distribusi_jenis_sektor",
      isolate(input$distribusi_jenis_sektor),
      preferred_value = default_total_pdrb_sector
    )
  })
  observe({
    req(
      input$wilayah,
      input$komparasi_tingkat_analisis,
      input$komparasi_jenis_nilai
    )

    indicator_value <- comparison_indicator()
    req(!is.na(indicator_value), nzchar(indicator_value))

    category_data <- pdrb_data() %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == indicator_value,
        !is.na(nilai),
        is.finite(nilai)
      ) %>%
      mutate(level = as.character(level))

    if (identical(as.character(input$komparasi_jenis_nilai)[1], "Distribusi") &&
        "dasar_harga" %in% names(category_data)) {
      basis_value <- as.character(input$komparasi_dasar_harga)[1]
      if (is.na(basis_value) || !basis_value %in% c("ADHB", "ADHK")) basis_value <- "ADHB"
      category_data <- category_data %>% filter(dasar_harga == basis_value)
    }

    level_value <- as.character(input$komparasi_tingkat_analisis)[1]
    if (is.na(level_value) || !nzchar(level_value)) level_value <- "Semua"
    if (!identical(level_value, "Semua")) {
      category_data <- category_data %>% filter(level == level_value)
    }

    if (!"kode_utama" %in% names(category_data)) category_data$kode_utama <- NA_character_
    if (!"source_row" %in% names(category_data)) category_data$source_row <- NA_integer_

    category_data <- category_data %>%
      group_by(item_id, kategori_label, level, kode_kategori, kode_utama) %>%
      summarise(
        source_row = if (all(is.na(source_row))) NA_integer_ else min(source_row, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      category_hierarchy_arrange()

    # Distribusi Total PDRB tetap tersedia. Nilainya dibentuk sebagai 100%
    # terhadap dirinya sendiri pada setiap wilayah, tahun, dan periode.
    total_pdrb_id <- "Total PDRB__PDRB__PRODUK DOMESTIK REGIONAL BRUTO"
    if (identical(as.character(input$komparasi_jenis_nilai)[1], "Distribusi") &&
        level_value %in% c("Semua", "Total PDRB")) {
      basis_value <- as.character(input$komparasi_dasar_harga)[1]
      if (is.na(basis_value) || !basis_value %in% c("ADHB", "ADHK")) basis_value <- "ADHB"
      total_available <- pdrb_data() %>%
        filter(
          kode_wilayah == input$wilayah,
          item_id == total_pdrb_id,
          indikator == paste("PDRB", basis_value),
          !is.na(nilai),
          is.finite(nilai)
        )

      if (nrow(total_available) > 0L) {
        total_choice <- tibble::tibble(
          item_id = total_pdrb_id,
          kategori_label = "Produk Domestik Regional Bruto",
          level = "Total PDRB",
          kode_kategori = "PDRB",
          kode_utama = NA_character_,
          source_row = NA_integer_
        )
        category_data <- bind_rows(total_choice, category_data) %>%
          distinct(item_id, .keep_all = TRUE) %>%
          category_hierarchy_arrange()
      }
    }

    values <- as.character(category_data$item_id)
    choices <- stats::setNames(values, as.character(category_data$kategori_label))

    if (length(values) == 0L) {
      updateSelectizeInput(
        session,
        "komparasi_jenis_sektor",
        choices = character(0),
        selected = NULL,
        server = TRUE
      )
      return(invisible(NULL))
    }

    current_value <- isolate(input$komparasi_jenis_sektor)

    selected_value <- if (!is.null(current_value) &&
                          length(current_value) == 1L &&
                          current_value %in% values) {
      current_value
    } else if (!identical(as.character(input$komparasi_jenis_nilai)[1], "Distribusi") &&
               total_pdrb_id %in% values) {
      total_pdrb_id
    } else {
      values[[1]]
    }

    updateSelectizeInput(
      session,
      "komparasi_jenis_sektor",
      choices = choices,
      selected = selected_value,
      server = TRUE
    )
  })

  # Input kanonik tetap dipakai oleh Overview/Tabel Data dan tidak diubah oleh menu Analytics.

  indicator_choices_for_group <- function(group_name, available_indicators) {
    group_name <- as.character(group_name)[1]
    available_indicators <- as.character(available_indicators)
    if (is.na(group_name) || !nzchar(group_name)) return(character(0))
    if (group_name %in% c("Pertumbuhan", "Sumber Pertumbuhan", "LQ", "DLQ", "Extended Shift Share")) return(character(0))
    choices <- indicator_groups[[group_name]]
    if (is.null(choices)) return(character(0))
    choices[unname(choices) %in% available_indicators]
  }
  
  output$indikator_filter_ui <- renderUI({
    req(input$kelompok_indikator)
    group_name <- as.character(input$kelompok_indikator)[1]
    available_indicators <- get_indikator_values(pdrb_data())
    
    if (identical(group_name, "PDRB")) {
      choices <- c("ADHB" = "PDRB ADHB", "ADHK" = "PDRB ADHK")
      choices <- choices[unname(choices) %in% available_indicators]
      if (length(choices) == 0L) {
        return(div(class = "filter-short-note", icon("info-circle"), span("Data PDRB ADHB/ADHK belum tersedia.")))
      }
      selected <- if (!is.null(input$indikator_non_growth) && input$indikator_non_growth %in% unname(choices)) input$indikator_non_growth else if ("PDRB ADHK" %in% unname(choices)) "PDRB ADHK" else unname(choices[[1]])
      return(pdrb_selectize("indikator_non_growth", "Dasar Harga", choices = choices, selected = selected, placeholder = "Pilih dasar harga"))
    }
    
    if (identical(group_name, "Distribusi")) {
      available_pdrb <- available_indicators[available_indicators %in% c("PDRB ADHB", "PDRB ADHK")]
      if (length(available_pdrb) == 0L) {
        return(div(class = "filter-short-note", icon("info-circle"), span("Distribusi PDRB belum tersedia. Pastikan data PDRB ADHB/ADHK sudah terbaca.")))
      }
      basis_choices <- c("ADHB" = "ADHB", "ADHK" = "ADHK")
      basis_choices <- basis_choices[paste("PDRB", unname(basis_choices)) %in% available_pdrb]
      selected_basis <- if (!is.null(input$dasar_distribusi) && input$dasar_distribusi %in% unname(basis_choices)) {
        input$dasar_distribusi
      } else if ("PDRB ADHB" %in% available_pdrb) {
        "ADHB"
      } else {
        "ADHK"
      }
      return(pdrb_selectize(
        "dasar_distribusi", "Dasar Harga",
        choices = basis_choices,
        selected = selected_basis,
        placeholder = "Pilih dasar harga"
      ))
    }
    
    if (identical(group_name, "Pertumbuhan")) {
      basis_choices <- c("ADHB" = "ADHB", "ADHK" = "ADHK")
      basis_choices <- basis_choices[vapply(unname(basis_choices), function(basis) {
        any(stringr::str_detect(available_indicators, paste0("^Pertumbuhan ", basis, " ")))
      }, logical(1))]
      if (length(basis_choices) == 0L) {
        return(div(class = "filter-short-note", icon("info-circle"), span("Pertumbuhan belum tersedia. Minimal perlu dua periode pembanding.")))
      }
      selected_basis <- if (!is.null(input$dasar_pertumbuhan) && input$dasar_pertumbuhan %in% unname(basis_choices)) {
        input$dasar_pertumbuhan
      } else if ("ADHK" %in% unname(basis_choices)) {
        "ADHK"
      } else {
        unname(basis_choices[[1]])
      }
      method_choices_all <- c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C")
      method_choices <- method_choices_all[paste("Pertumbuhan", selected_basis, unname(method_choices_all)) %in% available_indicators]
      if (length(method_choices) == 0L) method_choices <- method_choices_all
      selected_method <- if (!is.null(input$jenis_pertumbuhan) && input$jenis_pertumbuhan %in% unname(method_choices)) {
        input$jenis_pertumbuhan
      } else if ("Y-on-Y" %in% unname(method_choices)) {
        "Y-on-Y"
      } else {
        unname(method_choices[[1]])
      }
      return(tagList(
        pdrb_selectize(
          "dasar_pertumbuhan", "Dasar Harga",
          choices = basis_choices,
          selected = selected_basis,
          placeholder = "Pilih dasar harga"
        ),
        pdrb_selectize(
          "jenis_pertumbuhan", "Jenis Pertumbuhan",
          choices = method_choices,
          selected = selected_method,
          placeholder = "Pilih jenis pertumbuhan"
        )
      ))
    }
    
    if (identical(group_name, "Indeks Implisit")) {
      choices <- c(
        "Indeks Implisit" = "Indeks Implisit",
        "Laju Indeks Implisit Q-to-Q" = "Laju Indeks Implisit Q-to-Q",
        "Laju Indeks Implisit Y-on-Y" = "Laju Indeks Implisit Y-on-Y",
        "Laju Indeks Implisit C-to-C" = "Laju Indeks Implisit C-to-C"
      )
      choices <- choices[unname(choices) %in% available_indicators]
      if (length(choices) == 0L) {
        return(div(class = "filter-short-note", icon("info-circle"), span("Indeks implisit belum tersedia. Pastikan data ADHB dan ADHK tersedia.")))
      }
      selected <- if (!is.null(input$indikator_non_growth) && input$indikator_non_growth %in% unname(choices)) input$indikator_non_growth else unname(choices[[1]])
      return(pdrb_selectize("indikator_non_growth", "Jenis Indeks", choices = choices, selected = selected, placeholder = "Pilih jenis indeks"))
    }
    
    if (identical(group_name, "Sumber Pertumbuhan")) {
      available_source <- available_indicators[stringr::str_detect(available_indicators, "^Sumber Pertumbuhan ")]
      if (length(available_source) == 0L) {
        return(div(
          class = "filter-short-note",
          icon("info-circle"),
          span("Sumber pertumbuhan belum tersedia. Minimal perlu data sektor, total PDRB, dan periode pembanding.")
        ))
      }
      basis_choices <- c("ADHB" = "ADHB", "ADHK" = "ADHK")
      basis_choices <- basis_choices[vapply(unname(basis_choices), function(basis) {
        any(stringr::str_detect(available_source, paste0("^Sumber Pertumbuhan ", basis, " ")))
      }, logical(1))]
      selected_basis <- if (!is.null(input$dasar_sumber_pertumbuhan) && input$dasar_sumber_pertumbuhan %in% unname(basis_choices)) {
        input$dasar_sumber_pertumbuhan
      } else if ("ADHK" %in% unname(basis_choices)) {
        "ADHK"
      } else {
        unname(basis_choices[[1]])
      }
      method_choices_all <- c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C")
      method_choices <- method_choices_all[paste("Sumber Pertumbuhan", selected_basis, unname(method_choices_all)) %in% available_source]
      if (length(method_choices) == 0L) method_choices <- method_choices_all
      selected_method <- if (!is.null(input$jenis_sumber_pertumbuhan) && input$jenis_sumber_pertumbuhan %in% unname(method_choices)) {
        input$jenis_sumber_pertumbuhan
      } else if ("Y-on-Y" %in% unname(method_choices)) {
        "Y-on-Y"
      } else {
        unname(method_choices[[1]])
      }
      return(tagList(
        pdrb_selectize(
          "dasar_sumber_pertumbuhan", "Dasar Harga",
          choices = basis_choices,
          selected = selected_basis,
          placeholder = "Pilih dasar harga"
        ),
        pdrb_selectize(
          "jenis_sumber_pertumbuhan", "Jenis Pertumbuhan",
          choices = method_choices,
          selected = selected_method,
          placeholder = "Pilih jenis pertumbuhan"
        )
      ))
    }
    
    if (identical(group_name, "LQ")) {
      basis_choices <- c("ADHB" = "ADHB", "ADHK" = "ADHK")
      available_pdrb <- available_indicators[available_indicators %in% c("PDRB ADHB", "PDRB ADHK")]
      basis_choices <- basis_choices[paste("PDRB", unname(basis_choices)) %in% available_pdrb]
      if (length(basis_choices) == 0L) basis_choices <- c("ADHB" = "ADHB", "ADHK" = "ADHK")
      selected_basis <- if (!is.null(input$dasar_lq_analytics) && input$dasar_lq_analytics %in% unname(basis_choices)) {
        input$dasar_lq_analytics
      } else if ("ADHK" %in% unname(basis_choices)) {
        "ADHK"
      } else {
        unname(basis_choices[[1]])
      }
      return(pdrb_selectize(
        "dasar_lq_analytics", "Dasar Harga",
        choices = basis_choices,
        selected = selected_basis,
        placeholder = "Pilih dasar harga"
      ))
    }
    
    if (identical(group_name, "DLQ")) {
      return(div(class = "filter-short-note", icon("info-circle"), span("DLQ membutuhkan minimal dua tahun dan lebih tepat dibaca pada menu Potensi Wilayah.")))
    }
    
    if (identical(group_name, "Extended Shift Share")) {
      return(div(class = "filter-short-note", icon("info-circle"), span("Extended Shift Share membutuhkan tahun pembanding dan lebih tepat dibaca pada menu Potensi Wilayah.")))
    }
    
    choices <- indicator_choices_for_group(input$kelompok_indikator, available_indicators)
    if (length(choices) == 0L) {
      return(div(class = "filter-short-note", icon("info-circle"), span("Indikator pada kelompok ini belum tersedia untuk data/filter aktif.")))
    }
    current <- isolate(input$indikator_non_growth)
    selected <- if (!is.null(current) && length(current) == 1L && current %in% unname(choices)) current else unname(choices[[1]])
    pdrb_selectize("indikator_non_growth", "Jenis Indikator", choices = choices, selected = selected, placeholder = "Pilih indikator")
  })
  outputOptions(output, "indikator_filter_ui", suspendWhenHidden = FALSE)
  
  active_indicator <- reactive({
    req(input$kelompok_indikator)
    group_name <- as.character(input$kelompok_indikator)[1]
    
    if (identical(group_name, "PDRB")) {
      available <- get_indikator_values(pdrb_data())
      choices <- c("PDRB ADHB", "PDRB ADHK")
      choices <- choices[choices %in% available]
      value <- as.character(input$indikator_non_growth)
      if (length(value) != 1L || is.na(value) || !value %in% choices) {
        value <- if ("PDRB ADHK" %in% choices) "PDRB ADHK" else if (length(choices) > 0L) choices[[1]] else NA_character_
      }
      return(value)
    }
    
    if (identical(group_name, "Distribusi")) {
      return("Distribusi PDRB")
    }
    
    if (identical(group_name, "Pertumbuhan")) {
      available <- get_indikator_values(pdrb_data())
      basis <- input$dasar_pertumbuhan
      method <- input$jenis_pertumbuhan
      if (is.null(basis) || length(basis) != 1L || !basis %in% c("ADHB", "ADHK")) basis <- "ADHK"
      if (is.null(method) || length(method) != 1L || !method %in% c("Q-to-Q", "Y-on-Y", "C-to-C")) method <- "Q-to-Q"
      candidate <- paste(c("Pertumbuhan", basis, method), collapse = " ")
      if (!candidate %in% available) {
        candidates <- available[stringr::str_detect(available, "^Pertumbuhan (ADHB|ADHK) ")]
        candidate <- if (length(candidates) > 0L) candidates[[1]] else candidate
      }
      return(candidate)
    }
    
    if (identical(group_name, "Sumber Pertumbuhan")) {
      available <- get_indikator_values(pdrb_data())
      basis <- input$dasar_sumber_pertumbuhan
      method <- input$jenis_sumber_pertumbuhan
      if (is.null(basis) || length(basis) != 1L || !basis %in% c("ADHB", "ADHK")) basis <- "ADHK"
      if (is.null(method) || length(method) != 1L || !method %in% c("Q-to-Q", "Y-on-Y", "C-to-C")) method <- "Q-to-Q"
      candidate <- paste(c("Sumber Pertumbuhan", basis, method), collapse = " ")
      if (!candidate %in% available) {
        candidates <- available[stringr::str_detect(available, "^Sumber Pertumbuhan (ADHB|ADHK) ")]
        candidate <- if (length(candidates) > 0L) candidates[[1]] else candidate
      }
      return(candidate)
    }
    
    if (group_name %in% c("LQ", "DLQ", "Extended Shift Share")) return(NA_character_)
    
    available_indicators <- get_indikator_values(pdrb_data())
    choices <- indicator_choices_for_group(group_name, available_indicators)
    if (length(choices) == 0L) return(NA_character_)
    value <- input$indikator_non_growth
    value <- as.character(value)
    if (length(value) != 1L || is.na(value) || !nzchar(value) || !value %in% unname(choices)) value <- unname(choices[[1]])
    value
  })
  
  # ---------------------------------------------------------------------------
  # Reactive khusus tiap menu Analytics
  # ---------------------------------------------------------------------------
  # Setiap grafik membaca input card-nya sendiri. Filter lokal tidak lagi harus
  # menunggu sinkronisasi ke input kanonik tersembunyi, sehingga perubahan filter
  # langsung menginvalidasi grafik pada menu yang bersangkutan.
  analytics_indicator_value <- function(group_name, basis_value = NULL, growth_value = NULL) {
    group_name <- as.character(group_name)[1]
    raw_basis_value <- as.character(basis_value)[1]
    basis_value <- raw_basis_value
    growth_value <- as.character(growth_value)[1]
    available <- get_indikator_values(pdrb_data())

    if (is.na(group_name) || !nzchar(group_name)) return(NA_character_)
    if (!identical(group_name, "Indeks Implisit") &&
        (is.na(basis_value) || !basis_value %in% c("ADHB", "ADHK"))) basis_value <- "ADHK"
    if (is.na(growth_value) || !growth_value %in% c("Q-to-Q", "Y-on-Y", "C-to-C")) growth_value <- "Q-to-Q"

    candidate <- switch(
      group_name,
      "PDRB" = paste("PDRB", basis_value),
      "Distribusi" = "Distribusi PDRB",
      "Pertumbuhan" = paste("Pertumbuhan", basis_value, growth_value),
      "Indeks Implisit" = {
        value <- raw_basis_value
        if (is.na(value) || !nzchar(value) || value %in% c("ADHB", "ADHK")) "Indeks Implisit" else value
      },
      "Sumber Pertumbuhan" = paste("Sumber Pertumbuhan", basis_value, growth_value),
      NA_character_
    )

    if (!is.na(candidate) && candidate %in% available) return(candidate)

    pattern <- switch(
      group_name,
      "PDRB" = "^PDRB (ADHB|ADHK)$",
      "Distribusi" = "^Distribusi PDRB$",
      "Pertumbuhan" = "^Pertumbuhan (ADHB|ADHK) (Q-to-Q|Y-on-Y|C-to-C)$",
      "Indeks Implisit" = "^(Indeks Implisit|Laju Indeks Implisit)",
      "Sumber Pertumbuhan" = "^Sumber Pertumbuhan (ADHB|ADHK) (Q-to-Q|Y-on-Y|C-to-C)$",
      NULL
    )
    fallback <- if (is.null(pattern)) character(0) else available[stringr::str_detect(available, pattern)]
    if (length(fallback) > 0L) fallback[[1]] else candidate
  }

  analytics_item_meta <- function(item_id) {
    item_id_value <- as.character(item_id)[1]
    if (is.na(item_id_value) || !nzchar(item_id_value) || is.null(input$wilayah)) return(tibble::tibble())
    pdrb_data() %>%
      filter(kode_wilayah == input$wilayah, .data$item_id == item_id_value) %>%
      distinct(item_id, level, kode_kategori, kategori_label, uraian) %>%
      slice(1)
  }

  analytics_context_for <- function(item_id) {
    meta <- analytics_item_meta(item_id)
    region_name <- pdrb_data() %>%
      filter(kode_wilayah == input$wilayah) %>%
      distinct(wilayah) %>%
      pull(wilayah)
    list(
      category = if (nrow(meta) == 0L) "Sektor terpilih" else as.character(meta$kategori_label[[1]]),
      region = if (length(region_name) == 0L) "Wilayah terpilih" else as.character(region_name[[1]])
    )
  }

  analytics_series_for <- function(group_name, basis_value, growth_value, item_id) {
    validate(need(
      !is.null(input$wilayah) && length(input$wilayah) == 1L && nzchar(as.character(input$wilayah)),
      "Pilih wilayah pada Filter Global terlebih dahulu."
    ))
    item_id <- as.character(item_id)[1]
    validate(need(!is.na(item_id) && nzchar(item_id), "Jenis sektor belum dipilih."))
    indicator_value <- analytics_indicator_value(group_name, basis_value, growth_value)
    validate(need(!is.na(indicator_value) && nzchar(indicator_value), "Indikator belum tersedia untuk filter yang dipilih."))

    if (identical(as.character(group_name)[1], "Distribusi")) {
      basis_value <- as.character(basis_value)[1]
      if (is.na(basis_value) || !basis_value %in% c("ADHB", "ADHK")) basis_value <- "ADHB"
      return(
        trend_distribution_data(pdrb_data(), input$wilayah, item_id, basis_value) %>%
          add_time_columns() %>%
          arrange(waktu_index)
      )
    }

    pdrb_data() %>%
      filter(
        kode_wilayah == input$wilayah,
        .data$item_id == .env$item_id,
        indikator == indicator_value,
        !is.na(nilai), is.finite(nilai)
      ) %>%
      distinct(kode_wilayah, item_id, indikator, tahun, periode, .keep_all = TRUE) %>%
      add_time_columns() %>%
      arrange(waktu_index)
  }

  trend_indicator <- reactive({
    group <- as.character(input$tren_jenis_nilai)[1]
    basis <- if (identical(group, "Indeks Implisit")) input$tren_jenis_indeks else input$tren_dasar_harga
    analytics_indicator_value(group, basis, input$tren_jenis_pertumbuhan)
  })

  distribution_indicator <- reactive({
    group <- as.character(input$distribusi_jenis_nilai)[1]
    basis <- if (identical(group, "Indeks Implisit")) input$distribusi_jenis_indeks else input$distribusi_dasar_harga
    analytics_indicator_value(group, basis, input$distribusi_jenis_pertumbuhan)
  })

  comparison_indicator <- reactive({
    group <- as.character(input$komparasi_jenis_nilai)[1]
    basis <- if (identical(group, "Indeks Implisit")) input$komparasi_jenis_indeks else input$komparasi_dasar_harga
    analytics_indicator_value(group, basis, input$komparasi_jenis_pertumbuhan)
  })

  observeEvent(pdrb_data(), {
    data <- pdrb_data()
    
    if (nrow(data) == 0) {
      updateSelectizeInput(
        session, "preview_indikator",
        choices = character(0), selected = character(0), server = FALSE
      )
      return()
    }
    
    available_indicators <- get_indikator_values(data)
    preview_choices <- c("ADHB" = "PDRB ADHB", "ADHK" = "PDRB ADHK")
    preview_choices <- preview_choices[unname(preview_choices) %in% available_indicators]
    selected_choice <- if ("PDRB ADHB" %in% unname(preview_choices)) {
      "PDRB ADHB"
    } else if (length(preview_choices) > 0) {
      unname(preview_choices[[1]])
    } else {
      character(0)
    }
    
    updateSelectizeInput(
      session, "preview_indikator",
      choices = preview_choices,
      selected = selected_choice,
      server = FALSE
    )
  }, ignoreInit = FALSE)
  
  observeEvent(workbook_result(), {
    result <- workbook_result()
    
    if (!isTRUE(result$uploaded)) return()
    
    if (!is.null(result$error)) {
      showNotification(
        paste("File tidak dapat dibaca:", paste(result$error, collapse = " ")),
        type = "error", duration = NULL
      )
      updateTabItems(session, "tabs", selected = "input_data")
      return()
    }
    
    if (nrow(result$data) > 0) {
      showNotification(
        paste0("Data berhasil dibaca (", dplyr::n_distinct(result$data$kode_wilayah), " wilayah)."),
        type = "message", duration = 5
      )
      updateTabItems(session, "tabs", selected = "ringkasan")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$tabs, {
    result <- workbook_result()
    ready <- is.null(result$error) && nrow(result$data) > 0
    
    protected_tabs <- c("ringkasan", "tabel", "tren", "kernel", "struktur", "perbandingan", "analisis_wilayah")
    if (!ready && input$tabs %in% protected_tabs) {
      showNotification(
        "Unggah file melalui menu Unggah Data.",
        type = "warning", duration = 4
      )
      updateTabItems(session, "tabs", selected = "input_data")
    }
  }, ignoreInit = TRUE)
  
  
  observeEvent(pdrb_data(), {
    data <- pdrb_data()
    req(nrow(data) > 0)
    
    group_data <- data %>%
      distinct(kode_kelompok, kelompok) %>%
      arrange(kelompok)
    
    group_choices <- stats::setNames(group_data$kode_kelompok, group_data$kelompok)
    updateSelectInput(
      session, "kelompok",
      choices = group_choices,
      selected = group_data$kode_kelompok[1]
    )
  }, ignoreInit = FALSE)
  
  observe({
    data <- pdrb_data()
    req(nrow(data) > 0, input$kelompok)
    
    region_data <- data %>%
      filter(kode_kelompok == input$kelompok) %>%
      distinct(kode_wilayah, kode_kelompok, wilayah, jenis_wilayah) %>%
      mutate(
        jenis_wilayah = normalize_region_type(jenis_wilayah, kode_wilayah, kode_kelompok),
        is_agregat = is_aggregate_region(kode_wilayah, kode_kelompok, jenis_wilayah)
      ) %>%
      arrange(desc(is_agregat), wilayah)
    
    region_data <- region_data %>%
      mutate(
        label_wilayah = case_when(
          is_agregat &
            !stringr::str_detect(stringr::str_to_lower(wilayah), "^provinsi\\s+") ~
            paste("Provinsi", wilayah),
          TRUE ~ wilayah
        )
      )
    
    region_choices <- stats::setNames(
      as.character(region_data$kode_wilayah),
      as.character(region_data$label_wilayah)
    )
    aggregate_code <- region_data %>%
      filter(is_agregat) %>%
      slice(1) %>%
      pull(kode_wilayah)
    selected_region <- if (length(aggregate_code) > 0) aggregate_code else region_data$kode_wilayah[1]
    
    updateSelectInput(session, "wilayah", choices = region_choices, selected = selected_region)
  })
  
  
  observe({
    data <- pdrb_data()
    req(nrow(data) > 0, input$wilayah)
    years <- data %>%
      mutate(
        periode = as.character(periode),
        level = as.character(level),
        kode_kategori = as.character(kode_kategori),
        nilai_num = suppressWarnings(as.numeric(nilai))
      ) %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator %in% c("PDRB ADHB", "PDRB ADHK"),
        level == "Total PDRB",
        kode_kategori == "PDRB",
        !is.na(nilai_num),
        is.finite(nilai_num),
        nilai_num > 0
      ) %>%
      distinct(tahun) %>%
      arrange(tahun) %>%
      pull(tahun)
    years <- years[!is.na(years)]
    validate(need(length(years) > 0, "Tahun data belum tersedia."))
    choices <- c("Tahun Terbaru" = "__LATEST__", stats::setNames(as.character(years), as.character(years)))
    current <- isolate(input$tahun_global)
    selected <- if (!is.null(current) && current %in% unname(choices)) current else "__LATEST__"
    updateSelectizeInput(session, "tahun_global", choices = choices, selected = selected, server = TRUE)
  })
  
  observe({
    data <- pdrb_data()
    req(nrow(data) > 0, input$wilayah)
    selected_year <- input$tahun_global
    periods_data <- data %>%
      mutate(
        periode = as.character(periode),
        level = as.character(level),
        kode_kategori = as.character(kode_kategori),
        nilai_num = suppressWarnings(as.numeric(nilai))
      ) %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator %in% c("PDRB ADHB", "PDRB ADHK"),
        level == "Total PDRB",
        kode_kategori == "PDRB",
        !is.na(nilai_num),
        is.finite(nilai_num),
        nilai_num > 0
      )
    if (!is.null(selected_year) && selected_year != "__LATEST__") {
      periods_data <- periods_data %>% filter(tahun == suppressWarnings(as.integer(selected_year)))
    }
    periods <- periods_data %>%
      distinct(periode) %>%
      pull(periode)
    periods <- periods[order(period_rank(periods), na.last = NA)]
    validate(need(length(periods) > 0, "Periode data belum tersedia."))
    choices <- c("Periode Terbaru" = "__LATEST__", stats::setNames(periods, period_label(periods)))
    current <- isolate(input$periode_global)
    selected <- if (!is.null(current) && current %in% unname(choices)) {
      current
    } else {
      "__LATEST__"
    }
    updateSelectizeInput(session, "periode_global", choices = choices, selected = selected, server = TRUE)
  })
  
  observe({
    data <- pdrb_data()
    req(nrow(data) > 0, input$wilayah, input$level_kategori)
    
    category_data <- data %>%
      filter(kode_wilayah == input$wilayah) %>%
      mutate(level = as.character(level))
    
    if (input$level_kategori != "Semua") {
      category_data <- category_data %>% filter(level == input$level_kategori)
    }
    
    if (!"kode_utama" %in% names(category_data)) category_data$kode_utama <- NA_character_
    if (!"source_row" %in% names(category_data)) category_data$source_row <- NA_integer_
    
    category_data <- category_data %>%
      group_by(item_id, kategori_label, level, kode_kategori, kode_utama) %>%
      summarise(
        source_row = if (all(is.na(source_row))) NA_integer_ else min(source_row, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      category_hierarchy_arrange()
    
    validate(need(nrow(category_data) > 0, "Jenis sektor tidak tersedia untuk tingkat analisis yang dipilih."))
    
    choices <- setNames(category_data$item_id, category_data$kategori_label)
    current <- isolate(input$kategori)
    selected_value <- if (!is.null(current) && length(current) == 1L && current %in% category_data$item_id) {
      current
    } else {
      first_non_total <- category_data %>% filter(as.character(level) != "Total PDRB") %>% slice(1) %>% pull(item_id)
      if (length(first_non_total) > 0) first_non_total else category_data$item_id[1]
    }
    
    updateSelectizeInput(session, "kategori", choices = choices, selected = selected_value, server = TRUE)
  })
  
  latest_snapshot_period <- reactive({
    req(input$wilayah, input$kategori)
    data <- pdrb_data() %>%
      filter(
        kode_wilayah == input$wilayah,
        item_id == input$kategori,
        indikator %in% c("PDRB ADHB", "PDRB ADHK")
      ) %>%
      mutate(periode = as.character(periode))
    
    selected_year <- input$tahun_global
    selected_period <- input$periode_global
    if (!is.null(selected_year) && selected_year != "__LATEST__") {
      data_year <- data %>% filter(tahun == suppressWarnings(as.integer(selected_year)))
      if (!is.null(selected_period) && selected_period != "__LATEST__") {
        data_year_period <- data_year %>% filter(periode == selected_period)
        if (nrow(data_year_period) > 0) {
          return(list(
            tahun = suppressWarnings(as.integer(selected_year)),
            periode = selected_period,
            label = paste(period_label(selected_period), selected_year)
          ))
        }
      }
      if (nrow(data_year) > 0) return(latest_period_from(data_year, prefer_quarter = TRUE))
    }
    
    latest_period_from(data, prefer_quarter = TRUE)
  })
  
  selected_point <- reactive({
    data <- pdrb_data()
    req(nrow(data) > 0, input$wilayah, input$kategori)
    snapshot <- latest_snapshot_period()
    req(!is.na(snapshot$tahun), !is.na(snapshot$periode))
    
    data %>%
      filter(
        kode_wilayah == input$wilayah,
        item_id == input$kategori,
        tahun == snapshot$tahun,
        as.character(periode) == snapshot$periode
      )
  })
  
  selected_category_meta <- reactive({
    req(input$wilayah, input$kategori)
    pdrb_data() %>%
      filter(kode_wilayah == input$wilayah, item_id == input$kategori) %>%
      distinct(level, kode_kategori, kategori_label) %>%
      slice(1)
  })
  
  current_context <- reactive({
    req(input$wilayah, input$kategori)
    data <- pdrb_data()
    snapshot <- latest_snapshot_period()
    
    category_name <- data %>%
      filter(kode_wilayah == input$wilayah, item_id == input$kategori) %>%
      distinct(kategori_label) %>%
      pull(kategori_label)
    
    region_name <- data %>%
      filter(kode_wilayah == input$wilayah) %>%
      distinct(wilayah) %>%
      pull(wilayah)
    
    list(
      category = if (length(category_name) == 0) "Kategori terpilih" else category_name[1],
      region = if (length(region_name) == 0) "Wilayah terpilih" else region_name[1],
      period = snapshot$label
    )
  })
  
  overview_base_data <- reactive({
    req(input$wilayah)
    pdrb_data() %>%
      mutate(
        level = as.character(level),
        periode = as.character(periode),
        kode_kategori = as.character(kode_kategori)
      ) %>%
      filter(kode_wilayah == input$wilayah) %>%
      filter_complete_pdrb_value_periods() %>%
      filter(
        level == "Total PDRB",
        kode_kategori == "PDRB",
        indikator %in% c("PDRB ADHB", "PDRB ADHK"),
        !is.na(nilai),
        is.finite(as.numeric(nilai)),
        as.numeric(nilai) > 0
      )
  })
  
  overview_snapshot_period <- reactive({
    data <- overview_base_data() %>%
      filter(indikator %in% c("PDRB ADHB", "PDRB ADHK"), !is.na(nilai))
    validate(need(nrow(data) > 0, "Data total PDRB belum tersedia untuk wilayah terpilih."))
    
    selected_year <- as.character(input$tahun_global)
    selected_period <- as.character(input$periode_global)
    if (length(selected_year) == 0 || is.na(selected_year) || !nzchar(selected_year)) selected_year <- "__LATEST__"
    if (length(selected_period) == 0 || is.na(selected_period) || !nzchar(selected_period)) selected_period <- "__LATEST__"
    
    # Mode default benar-benar mengikuti data yang diunggah.
    # Jika data terakhir sudah lengkap sampai Total, pakai Tahunan/Total.
    # Jika data terakhir baru sampai Triwulan I-III, pakai triwulan terakhir tersebut.
    if (identical(selected_year, "__LATEST__") && identical(selected_period, "__LATEST__")) {
      return(latest_overview_period_from_data(data))
    }
    
    if (!identical(selected_year, "__LATEST__")) {
      year_value <- suppressWarnings(as.integer(selected_year))
      if (!is.na(year_value)) {
        data_year <- data %>% filter(tahun == year_value)
        if (nrow(data_year) > 0) {
          if (identical(selected_period, "__LATEST__")) {
            return(latest_overview_period_from_data(data_year))
          }
          data_year_period <- data_year %>% filter(periode == selected_period)
          if (nrow(data_year_period) > 0) {
            return(list(
              tahun = year_value,
              periode = selected_period,
              label = paste(period_label(selected_period), year_value)
            ))
          }
          return(latest_overview_period_from_data(data_year))
        }
      }
    }
    
    if (!identical(selected_period, "__LATEST__")) {
      data_period <- data %>% filter(periode == selected_period)
      if (nrow(data_period) > 0) {
        latest_year <- max(data_period$tahun, na.rm = TRUE)
        return(list(
          tahun = as.integer(latest_year),
          periode = selected_period,
          label = paste(period_label(selected_period), latest_year)
        ))
      }
    }
    
    latest_overview_period_from_data(data)
  })
  
  overview_selected_point <- reactive({
    snapshot <- overview_snapshot_period()
    req(!is.na(snapshot$tahun), !is.na(snapshot$periode), input$wilayah)
    
    # Ambil titik overview dari data lengkap, bukan hanya PDRB ADHB/ADHK.
    # Ini penting agar card Pertumbuhan ADHK Y-on-Y tetap terbaca pada
    # tahun/periode terpilih, misalnya Triwulan I 2026 vs Triwulan I 2025.
    pdrb_data() %>%
      mutate(
        level = as.character(level),
        periode = as.character(periode),
        kode_kategori = as.character(kode_kategori)
      ) %>%
      filter(
        kode_wilayah == input$wilayah,
        tahun == snapshot$tahun,
        periode == snapshot$periode,
        level == "Total PDRB",
        kode_kategori == "PDRB",
        indikator %in% c("PDRB ADHB", "PDRB ADHK", "Pertumbuhan ADHK Y-on-Y"),
        !is.na(nilai),
        is.finite(as.numeric(nilai))
      )
  })
  
  overview_context <- reactive({
    req(input$wilayah)
    data <- pdrb_data()
    snapshot <- overview_snapshot_period()
    
    region_name <- data %>%
      filter(kode_wilayah == input$wilayah) %>%
      distinct(wilayah) %>%
      pull(wilayah)
    
    list(
      region = if (length(region_name) == 0) "Wilayah terpilih" else region_name[1],
      period = snapshot$label
    )
  })
  
  output$hero_panel <- renderUI({
    data <- overview_base_data()
    validate(need(nrow(data) > 0, "Data belum tersedia."))
    ctx <- overview_context()
    snapshot <- overview_snapshot_period()
    year_range <- data %>%
      filter(
        indikator %in% c("PDRB ADHB", "PDRB ADHK"),
        periode == snapshot$periode,
        !is.na(tahun),
        !is.na(nilai),
        is.finite(as.numeric(nilai)),
        as.numeric(nilai) > 0
      ) %>%
      summarise(
        min_year = suppressWarnings(min(tahun, na.rm = TRUE)),
        max_year = suppressWarnings(max(tahun, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(
        min_year = ifelse(is.infinite(min_year), NA_integer_, as.integer(min_year)),
        max_year = ifelse(is.infinite(max_year), NA_integer_, as.integer(max_year))
      )
    range_text <- if (nrow(year_range) == 0 || is.na(year_range$min_year[[1]]) || is.na(year_range$max_year[[1]])) {
      "Rentang data belum tersedia"
    } else {
      paste0(year_range$min_year[[1]], "–", year_range$max_year[[1]])
    }
    
    div(
      class = "hero-panel",
      h2("Overview PDRB"),
      p(paste(ctx$region, "Ringkasan total wilayah", sep = " • ")),
      span(
        class = "dataset-badge",
        paste0(ctx$period, " • ", range_text)
      ),
      div(
        class = "palette-key",
        span(class = "palette-dot dot-adhb"), "ADHB",
        span(class = "palette-dot dot-adhk"), "ADHK"
      )
    )
  })
  
  output$vb_adhb <- renderValueBox({
    value <- safe_value(overview_selected_point(), "PDRB ADHB")
    ctx <- overview_context()
    valueBox(
      format_pdrb_card(value),
      paste0("PDRB ADHB · ", ctx$period),
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$vb_adhk <- renderValueBox({
    value <- safe_value(overview_selected_point(), "PDRB ADHK")
    ctx <- overview_context()
    valueBox(
      format_pdrb_card(value),
      paste0("PDRB ADHK · ", ctx$period),
      icon = icon("balance-scale"),
      color = "aqua"
    )
  })
  
  format_growth_percent_ringkasan <- function(value) {
    if (length(value) == 0 || is.na(value) || !is.finite(value)) return("Belum tersedia")
    paste0(scales::number(value, accuracy = 0.01, big.mark = ",", decimal.mark = "."), "%")
  }
  
  output$vb_yoy_adhk <- renderValueBox({
    value <- safe_value(overview_selected_point(), "Pertumbuhan ADHK Y-on-Y")
    ctx <- overview_context()
    box_color <- if (is.na(value) || !is.finite(value)) {
      "yellow"
    } else if (value >= 0) {
      "green"
    } else {
      "red"
    }
    
    valueBox(
      format_growth_percent_ringkasan(value),
      paste0("ADHK Y-on-Y · ", ctx$period),
      icon = icon("line-chart"),
      color = box_color
    )
  })
  
  summary_top_sector_adhb <- reactive({
    req(input$wilayah)
    selected_period <- overview_snapshot_period()
    req(!is.na(selected_period$tahun), !is.na(selected_period$periode))
    
    pdrb_data() %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == "PDRB ADHB",
        as.character(level) == "Kategori Utama",
        tahun == selected_period$tahun,
        as.character(periode) == selected_period$periode,
        !is.na(nilai)
      ) %>%
      slice_max(nilai, n = 1, with_ties = FALSE)
  })
  
  output$vb_top_sector <- renderValueBox({
    data <- summary_top_sector_adhb()
    ctx <- overview_context()
    
    if (nrow(data) == 0) {
      return(valueBox(
        "–",
        paste0("Sektor terbesar ADHB · ", ctx$period),
        icon = icon("industry"),
        color = "yellow"
      ))
    }
    
    sector_code <- clean_text(data$kode_kategori[1])
    sector_name <- clean_text(data$uraian[1])
    if (is.na(sector_name) || !nzchar(sector_name)) {
      sector_name <- clean_text(data$kategori_label[1])
    }
    if (is.na(sector_name) || !nzchar(sector_name)) {
      sector_name <- "Kategori utama"
    }
    
    value_label <- if (!is.na(sector_code) && nzchar(sector_code)) {
      paste0(sector_code, " - ", stringr::str_trunc(sector_name, 27))
    } else {
      stringr::str_trunc(sector_name, 32)
    }
    
    subtitle_label <- paste0(
      "Sektor terbesar ADHB · ",
      format_pdrb_card(data$nilai[1])
    )
    
    valueBox(value_label, subtitle_label, icon = icon("industry"), color = "yellow")
  })
  
  trend_distribution_data <- function(data, kode_wilayah_pilih, item_id_pilih, dasar_harga) {
    dasar_harga <- as.character(dasar_harga)[1]
    if (is.na(dasar_harga) || !dasar_harga %in% c("ADHB", "ADHK")) dasar_harga <- "ADHB"
    indikator_pilih <- paste("PDRB", dasar_harga)
    if (is.null(data) || nrow(data) == 0) return(tibble::tibble())
    required_cols <- c("kode_wilayah", "item_id", "indikator", "tahun", "periode", "nilai", "level", "kode_kategori")
    if (!all(required_cols %in% names(data))) return(tibble::tibble())
    base <- data %>%
      mutate(
        periode = as.character(periode),
        level = as.character(level),
        kode_kategori = as.character(kode_kategori),
        nilai = suppressWarnings(as.numeric(nilai))
      ) %>%
      filter(
        kode_wilayah == kode_wilayah_pilih,
        indikator == indikator_pilih,
        !is.na(tahun), !is.na(periode),
        !is.na(nilai), is.finite(nilai)
      )
    if (nrow(base) == 0) return(tibble::tibble())
    selected <- base %>% filter(item_id == item_id_pilih)
    if (nrow(selected) == 0) return(tibble::tibble())
    total <- base %>%
      filter(level == "Total PDRB", kode_kategori == "PDRB") %>%
      group_by(tahun, periode) %>%
      summarise(total_pdrb = dplyr::first(nilai), .groups = "drop")
    if (nrow(total) == 0) return(tibble::tibble())
    selected %>%
      left_join(total, by = c("tahun", "periode")) %>%
      mutate(
        indikator = "Distribusi PDRB",
        satuan = "Persen",
        nilai = dplyr::if_else(!is.na(total_pdrb) & is.finite(total_pdrb) & total_pdrb != 0, nilai / total_pdrb * 100, NA_real_),
        source_file = "Dihitung otomatis V5",
        source_row = NA_integer_
      ) %>%
      filter(!is.na(nilai), is.finite(nilai)) %>%
      select(-total_pdrb)
  }
  
  trend_data_raw <- reactive({
    req(input$wilayah, input$tren_jenis_sektor, input$tren_jenis_nilai)
    group <- as.character(input$tren_jenis_nilai)[1]
    basis <- if (identical(group, "Indeks Implisit")) input$tren_jenis_indeks else input$tren_dasar_harga
    analytics_series_for(
      group_name = group,
      basis_value = basis,
      growth_value = input$tren_jenis_pertumbuhan,
      item_id = input$tren_jenis_sektor
    )
  })

  trend_data <- reactive({
    data <- trend_data_raw()
    validate(need(nrow(data) > 0, "Data tren tidak tersedia untuk kombinasi filter ini."))

    year_start <- suppressWarnings(as.integer(input$tahun_awal_tren))
    year_end <- suppressWarnings(as.integer(input$tahun_akhir_tren))
    if (!is.na(year_start)) data <- data %>% filter(tahun >= year_start)
    if (!is.na(year_end)) data <- data %>% filter(tahun <= year_end)

    period_mode <- input$mode_periode_tren
    if (is.null(period_mode) || length(period_mode) != 1L) period_mode <- "quarterly"
    if (period_mode == "quarterly") {
      data <- data %>% filter(as.character(periode) %in% c("I", "II", "III", "IV"))
    } else {
      data <- data %>% filter(as.character(periode) == period_mode)
    }

    data %>% arrange(waktu_index)
  })

  distribution_source_data <- reactive({
    req(input$wilayah, input$distribusi_jenis_sektor, input$distribusi_jenis_nilai)
    group <- as.character(input$distribusi_jenis_nilai)[1]
    basis <- if (identical(group, "Indeks Implisit")) input$distribusi_jenis_indeks else input$distribusi_dasar_harga
    analytics_series_for(
      group_name = group,
      basis_value = basis,
      growth_value = input$distribusi_jenis_pertumbuhan,
      item_id = input$distribusi_jenis_sektor
    )
  })

  observe({
    data <- trend_data_raw()
    req(nrow(data) > 0)
    years <- sort(unique(as.integer(data$tahun)))
    years <- years[!is.na(years)]
    req(length(years) > 0)
    
    min_year <- min(years, na.rm = TRUE)
    max_year <- max(years, na.rm = TRUE)
    current_start <- suppressWarnings(as.integer(isolate(input$tahun_awal_tren)))
    current_end <- suppressWarnings(as.integer(isolate(input$tahun_akhir_tren)))
    
    if (is.na(current_start) || !current_start %in% years) current_start <- min_year
    if (is.na(current_end) || !current_end %in% years) current_end <- max_year
    if (!is.na(current_start) && !is.na(current_end) && current_start > current_end) {
      current_start <- min_year
      current_end <- max_year
    }
    
    year_choices <- stats::setNames(as.character(years), as.character(years))
    new_state <- list(
      choices = unname(year_choices),
      start_selected = as.character(current_start),
      end_selected = as.character(current_end)
    )
    old_state <- trend_year_filter_state()
    if (!identical(old_state, new_state)) {
      trend_year_filter_state(new_state)
      updateSelectizeInput(session, "tahun_awal_tren", choices = year_choices, selected = as.character(current_start), server = TRUE)
      updateSelectizeInput(session, "tahun_akhir_tren", choices = year_choices, selected = as.character(current_end), server = TRUE)
    }
  })
  
  # Filter waktu untuk halaman Distribusi Data.
  # Filter wilayah, kategori, dan indikator mengikuti sidebar utama.
  observeEvent(distribution_source_data(), {
    data <- distribution_source_data()
    req(nrow(data) > 0)
    
    years <- sort(unique(as.integer(data$tahun)))
    years <- years[!is.na(years)]
    year_choices <- c(
      "Semua Tahun" = "__ALL__",
      stats::setNames(as.character(years), as.character(years))
    )
    
    current_year <- isolate(input$tahun_distribusi)
    selected_year <- if (
      !is.null(current_year) &&
      length(current_year) == 1L &&
      current_year %in% unname(year_choices)
    ) current_year else "__ALL__"
    
    update_pdrb_selectize(
      session, "tahun_distribusi",
      choices = year_choices,
      selected = selected_year
    )
  }, ignoreInit = FALSE)
  
  observe({
    data <- distribution_source_data()
    req(nrow(data) > 0)
    
    selected_year <- input$tahun_distribusi
    if (
      !is.null(selected_year) &&
      length(selected_year) == 1L &&
      !identical(selected_year, "__ALL__")
    ) {
      data <- data %>% filter(tahun == suppressWarnings(as.integer(selected_year)))
    }
    
    periods <- data %>%
      transmute(
        periode = as.character(periode),
        periode_urut = period_rank(periode)
      ) %>%
      filter(!is.na(periode), !is.na(periode_urut)) %>%
      distinct(periode, periode_urut) %>%
      arrange(periode_urut) %>%
      pull(periode)
    
    # Pilihan Tahun memakai nilai internal `Total` dan hanya ditampilkan
    # ketika data tahunan tersedia pada kombinasi filter aktif.
    quarterly_periods <- periods[periods %in% c("I", "II", "III", "IV")]
    annual_available <- "Total" %in% periods
    period_choices <- c(
      "Semua Triwulan" = "__QUARTERS__",
      stats::setNames(quarterly_periods, period_label(quarterly_periods)),
      if (annual_available) c("Tahun" = "Total") else character(0)
    )
    
    current_period <- isolate(input$periode_distribusi)
    selected_period <- if (
      !is.null(current_period) &&
      length(current_period) == 1L &&
      current_period %in% unname(period_choices)
    ) current_period else "__QUARTERS__"
    
    update_pdrb_selectize(
      session, "periode_distribusi",
      choices = period_choices,
      selected = selected_period
    )
  })
  
  analytics_filter_notice <- reactive({
    data <- pdrb_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    tab <- as.character(input$tabs)[1]
    jenis_nilai <- switch(tab,
      tren = as.character(input$tren_jenis_nilai)[1],
      kernel = as.character(input$distribusi_jenis_nilai)[1],
      as.character(input$kelompok_indikator)[1]
    )
    indikator_aktif <- switch(tab,
      tren = tryCatch(trend_indicator(), error = function(e) NA_character_),
      kernel = tryCatch(distribution_indicator(), error = function(e) NA_character_),
      tryCatch(active_indicator(), error = function(e) NA_character_)
    )
    years <- data %>% filter(!is.na(tahun)) %>% distinct(tahun) %>% pull(tahun) %>% as.integer()
    years <- sort(unique(years[!is.na(years)]))
    
    if (length(years) <= 1 && jenis_nilai %in% c("Pertumbuhan", "Sumber Pertumbuhan", "DLQ", "Extended Shift Share")) {
      return(paste0(jenis_nilai, " membutuhkan minimal dua tahun data."))
    }
    if (identical(jenis_nilai, "LQ")) {
      dasar <- if (is.null(input$dasar_lq_analytics)) "ADHK" else as.character(input$dasar_lq_analytics)[1]
      ok_ref <- tryCatch(reference_available_by_dasar_v5(dasar), error = function(e) FALSE)
      if (!isTRUE(ok_ref)) return("LQ membutuhkan data wilayah pembanding/provinsi.")
    }
    if (identical(jenis_nilai, "DLQ")) return("DLQ lebih tepat dibaca pada menu Potensi Wilayah karena membandingkan dua tahun.")
    if (identical(jenis_nilai, "Extended Shift Share")) return("Extended Shift Share lebih tepat dibaca pada menu Potensi Wilayah karena memakai tahun pembanding.")
    if (!is.na(indikator_aktif) && nzchar(indikator_aktif)) {
      available_years <- tryCatch(
        trend_data_raw() %>% filter(!is.na(nilai)) %>% distinct(tahun) %>% pull(tahun),
        error = function(e) integer(0)
      )
      available_years <- sort(unique(as.integer(available_years)))
      available_years <- available_years[!is.na(available_years)]
      if (length(available_years) <= 1 && jenis_nilai %in% c("Pertumbuhan", "Sumber Pertumbuhan")) {
        return(paste0(jenis_nilai, " hanya punya satu tahun valid untuk filter ini."))
      }
    }
    NULL
  })
  
  output$tren_filter_notice <- renderUI({
    msg <- analytics_filter_notice()
    if (is.null(msg) || !nzchar(msg)) return(NULL)
    div(class = "filter-short-note", icon("info-circle"), span(msg))
  })
  
  output$distribusi_filter_notice <- renderUI({
    msg <- analytics_filter_notice()
    if (is.null(msg) || !nzchar(msg)) return(NULL)
    div(class = "filter-short-note", icon("info-circle"), span(msg))
  })
  
  trend_plot_values <- function(data, indikator) {
    if (indikator %in% c("PDRB ADHB", "PDRB ADHK")) {
      scale_info <- pdrb_value_scale(data$nilai, force_triliun = any(as.character(data$level) == "Total PDRB", na.rm = TRUE))
      data %>% mutate(nilai_tampil = nilai / scale_info$scale, satuan_tampil = scale_info$title)
    } else {
      fallback_unit <- unname(indicator_units[[indikator]])
      if (is.null(fallback_unit) || length(fallback_unit) == 0 || is.na(fallback_unit)) {
        fallback_unit <- "Nilai"
      }
      data %>%
        mutate(
          nilai_tampil = nilai,
          satuan_tampil = dplyr::coalesce(as.character(satuan), fallback_unit)
        )
    }
  }
  
  trend_title_text <- function(ctx, indikator) {
    category_label <- clean_text(ctx$category)
    region_label <- clean_text(ctx$region)
    if (!is.na(category_label) && nzchar(category_label) && category_label != "PDRB") {
      paste("Tren", indikator, "-", category_label)
    } else {
      paste("Tren", indikator, "-", region_label)
    }
  }
  
  trend_change_summary <- function(data, indikator) {
    if (nrow(data) < 2) {
      return(list(value = "–", subtitle = "Perubahan belum tersedia"))
    }
    
    start_value <- as.numeric(data$nilai[1])
    end_value <- as.numeric(data$nilai[nrow(data)])
    
    if (is.na(start_value) || is.na(end_value)) {
      return(list(value = "–", subtitle = "Data awal/akhir tidak lengkap"))
    }
    
    if (indikator %in% c("PDRB ADHB", "PDRB ADHK", "Indeks Implisit")) {
      if (start_value == 0) {
        return(list(value = "–", subtitle = "Nilai awal nol"))
      }
      change_pct <- (end_value - start_value) / abs(start_value) * 100
      sign <- ifelse(change_pct > 0, "+", "")
      list(
        value = paste0(sign, scales::number(change_pct, accuracy = 0.01, big.mark = ",", decimal.mark = "."), "%"),
        subtitle = "Perubahan relatif · awal ke akhir"
      )
    } else {
      change_point <- end_value - start_value
      sign <- ifelse(change_point > 0, "+", "")
      list(
        value = paste0(sign, scales::number(change_point, accuracy = 0.01, big.mark = ",", decimal.mark = "."), " poin"),
        subtitle = "Selisih nilai akhir - awal"
      )
    }
  }
  
  output$vb_tren_awal <- renderValueBox({
    data <- trend_data()
    validate(need(nrow(data) > 0, "Data tren tidak tersedia."))
    first_row <- data[1, , drop = FALSE]
    valueBox(
      format_indicator_value_card(first_row$nilai, trend_indicator()),
      paste0("Nilai awal · ", first_row$waktu),
      icon = icon("play"),
      color = "blue"
    )
  })
  
  output$vb_tren_akhir <- renderValueBox({
    data <- trend_data()
    validate(need(nrow(data) > 0, "Data tren tidak tersedia."))
    last_row <- data[nrow(data), , drop = FALSE]
    valueBox(
      format_indicator_value_card(last_row$nilai, trend_indicator()),
      paste0("Nilai akhir · ", last_row$waktu),
      icon = icon("flag-checkered"),
      color = "aqua"
    )
  })
  
  output$vb_tren_perubahan <- renderValueBox({
    data <- trend_data()
    validate(need(nrow(data) > 0, "Data tren tidak tersedia."))
    change_info <- trend_change_summary(data, trend_indicator())
    valueBox(
      change_info$value,
      change_info$subtitle,
      icon = icon("arrows-v"),
      color = "yellow"
    )
  })
  
  make_trend_plot <- function() {
    data <- trend_data()
    validate(need(nrow(data) > 0, "Data tidak tersedia untuk kombinasi filter ini."))
    ctx <- analytics_context_for(input$tren_jenis_sektor)
    indikator <- trend_indicator()
    data_plot <- trend_plot_values(data, indikator)
    
    hover_text <- paste0(
      "Waktu: ", data_plot$waktu,
      "<br>", indikator, ": ", format_indicator_value(data_plot$nilai, indikator)
    )
    
    chart_color <- indicator_color(indikator)
    
    plot_ly(
      data_plot,
      x = ~waktu, y = ~nilai_tampil,
      type = "scatter", mode = "lines+markers",
      text = hover_text,
      hoverinfo = "text",
      line = list(width = 3, color = chart_color),
      marker = list(size = 7, color = chart_color, line = list(color = "#FFFFFF", width = 1))
    ) %>%
      layout(
        title = list(text = trend_title_text(ctx, indikator), x = 0.02),
        xaxis = list(
          title = "Waktu",
          type = "category",
          categoryorder = "array",
          categoryarray = data_plot$waktu,
          tickangle = -45,
          gridcolor = PDRB_COLORS$grid,
          zeroline = FALSE
        ),
        yaxis = list(title = {
          units <- unique(as.character(data_plot$satuan_tampil))
          units <- units[!is.na(units) & nzchar(units)]
          if (length(units) > 0L) units[[1]] else "Nilai"
        }, separatethousands = TRUE, gridcolor = PDRB_COLORS$grid, zeroline = TRUE, rangemode = "tozero"),
        hovermode = "x unified",
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        margin = list(l = 70, r = 25, b = 100, t = 70)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  }
  
  output$plot_trend <- renderPlotly({
    req(identical(as.character(input$tabs)[1], "tren"), cancelOutput = TRUE)
    make_trend_plot()
  })
  
  kernel_histogram_data <- reactive({
    data <- distribution_source_data() %>%
      filter(!is.na(nilai), is.finite(nilai))
    
    selected_year <- input$tahun_distribusi
    selected_period <- input$periode_distribusi
    
    if (
      !is.null(selected_year) &&
      length(selected_year) == 1L &&
      !identical(selected_year, "__ALL__")
    ) {
      data <- data %>%
        filter(tahun == suppressWarnings(as.integer(selected_year)))
    }
    
    if (!is.null(selected_period) && length(selected_period) == 1L) {
      if (identical(selected_period, "__QUARTERS__")) {
        data <- data %>% filter(as.character(periode) %in% c("I", "II", "III", "IV"))
      } else if (!identical(selected_period, "__ALL__")) {
        data <- data %>% filter(as.character(periode) == selected_period)
      }
    }
    
    validate(need(
      nrow(data) > 0,
      "Data tidak tersedia untuk tahun dan periode yang dipilih."
    ))
    data %>% arrange(waktu_index)
  })
  
  distribution_numeric_values <- function(data) {
    values <- suppressWarnings(as.numeric(unlist(
      data[["nilai"]], recursive = TRUE, use.names = FALSE
    )))
    values[is.finite(values)]
  }
  
  distribution_plot_values <- function(data, indikator) {
    if (indikator %in% c("PDRB ADHB", "PDRB ADHK")) {
      scale_info <- pdrb_value_scale(data$nilai, force_triliun = any(as.character(data$level) == "Total PDRB", na.rm = TRUE))
      data %>% mutate(nilai_tampil = nilai / scale_info$scale, satuan_tampil = scale_info$title)
    } else {
      fallback_unit <- unname(indicator_units[[indikator]])
      if (is.null(fallback_unit) || length(fallback_unit) == 0 || is.na(fallback_unit)) {
        fallback_unit <- "Nilai"
      }
      data %>%
        mutate(
          nilai_tampil = nilai,
          satuan_tampil = if_else(
            !is.na(as.character(satuan)) & nzchar(as.character(satuan)),
            as.character(satuan),
            fallback_unit
          )
        )
    }
  }
  
  distribution_title_text <- function(ctx, indikator) {
    if (!is.null(ctx$category) && !is.na(ctx$category) && nzchar(ctx$category) && ctx$category != "PDRB") {
      paste("Sebaran", indikator, "-", ctx$category)
    } else {
      paste("Sebaran", indikator, "-", ctx$region)
    }
  }
  
  format_distribution_range <- function(min_value, max_value, indikator) {
    if (length(min_value) == 0 || length(max_value) == 0 ||
        is.na(min_value) || is.na(max_value) ||
        !is.finite(min_value) || !is.finite(max_value)) {
      return("–")
    }
    if (indikator %in% c("PDRB ADHB", "PDRB ADHK")) {
      return(paste0(
        "Rp ",
        scales::number(min_value / 1e6, accuracy = 0.01, big.mark = ",", decimal.mark = "."),
        "–",
        scales::number(max_value / 1e6, accuracy = 0.01, big.mark = ",", decimal.mark = "."),
        " Triliun"
      ))
    }
    if (indikator == "Indeks Implisit") {
      return(paste0(
        scales::number(min_value, accuracy = 0.01, big.mark = ",", decimal.mark = "."),
        "–",
        scales::number(max_value, accuracy = 0.01, big.mark = ",", decimal.mark = ".")
      ))
    }
    suffix <- if (stringr::str_detect(indikator, "Sumber Pertumbuhan")) " poin" else "%"
    paste0(
      scales::number(min_value, accuracy = 0.01, big.mark = ",", decimal.mark = "."),
      "–",
      scales::number(max_value, accuracy = 0.01, big.mark = ",", decimal.mark = "."),
      suffix
    )
  }
  
  format_distribution_range_card <- function(min_value, max_value, indikator) {
    if (length(min_value) == 0 || length(max_value) == 0 ||
        is.na(min_value) || is.na(max_value) ||
        !is.finite(min_value) || !is.finite(max_value)) {
      return("–")
    }
    if (indikator %in% c("PDRB ADHB", "PDRB ADHK")) {
      return(paste0(
        scales::number(min_value / 1000, accuracy = 1, big.mark = ",", decimal.mark = "."),
        "–",
        scales::number(max_value / 1000, accuracy = 1, big.mark = ",", decimal.mark = "."),
        " Miliar Rupiah"
      ))
    }
    format_distribution_range(min_value, max_value, indikator)
  }
  
  distribution_cv <- function(values) {
    values <- values[is.finite(values)]
    n_value <- length(values)
    if (n_value < 2L) return(NA_real_)
    mean_value <- mean(values, na.rm = TRUE)
    sd_value <- stats::sd(values, na.rm = TRUE)
    if (is.na(sd_value) || !is.finite(mean_value) || mean_value == 0) return(NA_real_)
    abs(sd_value / mean_value) * 100
  }
  
  output$vb_distribusi_median <- renderValueBox({
    data <- kernel_histogram_data()
    values <- distribution_numeric_values(data)
    validate(need(length(values) > 0L, "Data distribusi tidak tersedia."))
    valueBox(
      format_indicator_value_card(stats::median(values, na.rm = TRUE), distribution_indicator()),
      "Median · nilai tengah data",
      icon = icon("exchange"),
      color = "blue"
    )
  })
  
  output$vb_distribusi_cv <- renderValueBox({
    data <- kernel_histogram_data()
    values <- distribution_numeric_values(data)
    cv_value <- distribution_cv(values)
    cv_label <- if (is.na(cv_value)) "–" else paste0(scales::number(cv_value, accuracy = 0.01, big.mark = ",", decimal.mark = "."), "%")
    valueBox(
      cv_label,
      "Koefisien variasi · keragaman relatif",
      icon = icon("percent"),
      color = "green"
    )
  })
  
  output$vb_distribusi_rentang <- renderValueBox({
    data <- kernel_histogram_data()
    values <- distribution_numeric_values(data)
    validate(need(length(values) > 0L, "Data distribusi tidak tersedia."))
    valueBox(
      format_distribution_range_card(min(values, na.rm = TRUE), max(values, na.rm = TRUE), distribution_indicator()),
      "Rentang nilai · minimum–maksimum",
      icon = icon("arrows-v"),
      color = "yellow"
    )
  })
  
  output$plot_kernel_histogram <- renderPlotly({
    req(identical(as.character(input$tabs)[1], "kernel"), cancelOutput = TRUE)
    data <- kernel_histogram_data()
    indikator <- distribution_indicator()
    data_plot <- distribution_plot_values(data, indikator)
    
    values <- suppressWarnings(as.numeric(unlist(
      data_plot[["nilai_tampil"]], recursive = TRUE, use.names = FALSE
    )))
    values <- values[is.finite(values)]
    
    validate(need(length(values) > 0L, "Tidak ada nilai numerik yang dapat dibuat menjadi histogram."))
    
    n_values <- length(values)
    bins <- max(5L, min(30L, as.integer(ceiling(log2(max(n_values, 1L)) + 1L))))
    ctx <- analytics_context_for(input$distribusi_jenis_sektor)
    
    display_units <- unique(as.character(data_plot$satuan_tampil))
    display_units <- display_units[!is.na(display_units) & nzchar(display_units)]
    unit_label <- if (length(display_units) > 0L) display_units[[1]] else "Nilai"
    
    format_plot_number <- function(x, accuracy = 0.01) {
      scales::number(x, accuracy = accuracy, big.mark = ",", decimal.mark = ".")
    }
    
    value_range <- range(values, na.rm = TRUE)
    if (length(values) == 1L || diff(value_range) == 0) {
      center <- values[[1]]
      padding <- max(abs(center) * 0.05, 0.5)
      breaks_used <- seq(center - padding, center + padding, length.out = 6L)
    } else {
      breaks_used <- bins
    }
    
    histogram_result <- graphics::hist(values, breaks = breaks_used, plot = FALSE)
    histogram_data <- tibble(
      titik_tengah = as.numeric(histogram_result$mids),
      frekuensi = as.numeric(histogram_result$counts),
      batas_bawah = as.numeric(head(histogram_result$breaks, -1)),
      batas_atas = as.numeric(tail(histogram_result$breaks, -1)),
      lebar_kelas = as.numeric(diff(histogram_result$breaks))
    ) %>%
      mutate(
        hover_label = paste0(
          "Rentang: ",
          format_plot_number(batas_bawah),
          "–",
          format_plot_number(batas_atas),
          " ", unit_label,
          "<br>Frekuensi: ", frekuensi
        )
      )
    
    # Batang diberi lebar kelas aktual dan bargap = 0 agar benar-benar
    # tampak sebagai histogram kontinu, bukan bar chart kategori.
    p <- plot_ly(
      x = histogram_data$titik_tengah,
      y = histogram_data$frekuensi,
      width = histogram_data$lebar_kelas,
      type = "bar",
      name = "Frekuensi",
      opacity = 0.78,
      hovertext = histogram_data$hover_label,
      hoverinfo = "text",
      marker = list(
        color = indicator_color(indikator),
        line = list(color = PDRB_COLORS$surface, width = 0.35)
      )
    )
    
    # Kurva kernel ditransformasikan dari skala density ke skala frekuensi:
    # density × jumlah observasi × lebar kelas histogram.
    kernel_requested <- identical(input$tampilkan_kurva_kernel, "show")
    kernel_available <- length(values) >= 2L && length(unique(values)) >= 2L
    if (kernel_requested && kernel_available) {
      density_result <- tryCatch(
        stats::density(
          values,
          na.rm = TRUE,
          from = min(histogram_result$breaks),
          to = max(histogram_result$breaks),
          n = 512
        ),
        error = function(e) NULL
      )
      
      if (!is.null(density_result)) {
        bin_width <- stats::median(histogram_data$lebar_kelas, na.rm = TRUE)
        kernel_frequency <- density_result$y * n_values * bin_width
        
        p <- p %>%
          add_trace(
            x = density_result$x,
            y = kernel_frequency,
            type = "scatter",
            mode = "lines",
            name = "Kurva Kernel",
            hovertemplate = paste0(
              "Nilai: %{x:,.4f} ", unit_label,
              "<br>Frekuensi estimasi: %{y:,.2f}<extra></extra>"
            ),
            line = list(color = PDRB_COLORS$orange, width = 3),
            inherit = FALSE
          )
      }
    }
    
    p %>%
      layout(
        title = list(
          text = paste0(
            if (identical(input$tampilkan_kurva_kernel, "show")) {
              "Histogram Frekuensi dan Kurva Kernel"
            } else {
              "Histogram Frekuensi"
            },
            "<br><sup>", distribution_title_text(ctx, indikator), "</sup>"
          ),
          x = 0.02
        ),
        xaxis = list(
          title = unit_label,
          separatethousands = TRUE,
          gridcolor = PDRB_COLORS$grid,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Frekuensi",
          rangemode = "tozero",
          gridcolor = PDRB_COLORS$grid,
          zeroline = FALSE
        ),
        bargap = 0,
        barmode = "overlay",
        legend = list(orientation = "h", x = 0, y = 1.10),
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        margin = list(l = 70, r = 25, b = 65, t = 100)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  })

  output$descriptive_context <- renderUI({
    data <- kernel_histogram_data()
    ctx <- analytics_context_for(input$distribusi_jenis_sektor)
    time_labels <- unique(as.character(unlist(data[["waktu"]], recursive = TRUE, use.names = FALSE)))
    time_range <- if (length(time_labels) > 1) {
      paste0(time_labels[1], " sampai ", time_labels[length(time_labels)])
    } else if (length(time_labels) == 1) {
      time_labels[1]
    } else {
      "Periode tidak tersedia"
    }
    cakupan_label <- ctx$category
    div(
      class = "kernel-context",
      strong(ctx$region),
      tags$br(),
      span(cakupan_label),
      tags$br(),
      span(paste0(distribution_indicator(), " • ", time_range)),
      tags$br(),
      span(paste0("Satuan tampilan: ", {
        units <- unique(distribution_plot_values(data, distribution_indicator())$satuan_tampil)
        units <- units[!is.na(units) & nzchar(units)]
        if (length(units) > 0) units[[1]] else "Nilai"
      }))
    )
  })
  
  output$descriptive_stats <- renderDT({
    data <- kernel_histogram_data()
    data_plot <- distribution_plot_values(data, distribution_indicator())
    values <- suppressWarnings(as.numeric(unlist(
      data_plot[["nilai_tampil"]], recursive = TRUE, use.names = FALSE
    )))
    values <- values[is.finite(values)]
    validate(need(length(values) > 0L, "Tidak ada nilai numerik untuk dihitung."))
    n_value <- length(values)
    mean_value <- mean(values, na.rm = TRUE)
    sd_value <- if (n_value >= 2L) stats::sd(values, na.rm = TRUE) else NA_real_
    variance_value <- if (n_value >= 2L) stats::var(values, na.rm = TRUE) else NA_real_
    q_value <- stats::quantile(values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
    cv_value <- if (!is.na(sd_value) && is.finite(mean_value) && mean_value != 0) {
      abs(sd_value / mean_value) * 100
    } else {
      NA_real_
    }
    
    format_number <- function(x, accuracy = 0.0001) {
      if (length(x) == 0 || is.na(x) || !is.finite(x)) return("–")
      scales::number(x, accuracy = accuracy, big.mark = ",", decimal.mark = ".")
    }

    format_percent <- function(x) {
      if (length(x) == 0 || is.na(x) || !is.finite(x)) return("–")
      paste0(
        scales::number(x, accuracy = 0.01, big.mark = ",", decimal.mark = "."),
        "%"
      )
    }
    
    stats_table <- tibble(
      Statistik = c(
        "Jumlah observasi", "Rata-rata", "Median", "Simpangan baku",
        "Varians", "Minimum", "Kuartil 1", "Kuartil 3", "Maksimum",
        "Rentang", "Koefisien variasi"
      ),
      Nilai = c(
        as.character(n_value),
        format_number(mean_value),
        format_number(stats::median(values, na.rm = TRUE)),
        format_number(sd_value),
        format_number(variance_value),
        format_number(min(values, na.rm = TRUE)),
        format_number(q_value[1]),
        format_number(q_value[2]),
        format_number(max(values, na.rm = TRUE)),
        format_number(diff(range(values, na.rm = TRUE))),
        format_percent(cv_value)
      )
    )
    
    datatable(
      stats_table,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE
      )
    )
  }, server = FALSE)
  
  make_summary_trend_plot <- function(indikator_value) {
    req(input$wilayah, input$kategori)
    
    data <- pdrb_data() %>%
      filter(
        kode_wilayah == input$wilayah,
        item_id == input$kategori,
        indikator == indikator_value
      ) %>%
      quarterly_time_series() %>%
      arrange(waktu_index)
    
    validate(need(nrow(data) > 0, paste("Data", indikator_value, "belum tersedia.")))
    ctx <- analytics_context_for(input$distribusi_jenis_sektor)
    scale_info <- pdrb_value_scale(data$nilai, force_triliun = any(as.character(data$level) == "Total PDRB", na.rm = TRUE))
    data <- data %>% mutate(nilai_plot = nilai / scale_info$scale)
    
    hover_text <- paste0(
      "Waktu: ", data$waktu,
      "<br>", indikator_value, ": ", format_indicator_value(data$nilai, indikator_value)
    )
    
    chart_color <- indicator_color(indikator_value)
    
    plot_ly(
      data,
      x = ~waktu, y = ~nilai_plot,
      type = "scatter", mode = "lines+markers",
      text = hover_text,
      hoverinfo = "text",
      line = list(width = 3, color = chart_color),
      marker = list(size = 7, color = chart_color, line = list(color = "#FFFFFF", width = 1))
    ) %>%
      layout(
        title = list(text = paste("Tren", indikator_value, "-", ctx$region), x = 0.02),
        xaxis = list(
          title = "Waktu",
          type = "category",
          categoryorder = "array",
          categoryarray = data$waktu,
          tickangle = -45,
          gridcolor = PDRB_COLORS$grid,
          zeroline = FALSE
        ),
        yaxis = list(title = scale_info$title, separatethousands = TRUE, gridcolor = PDRB_COLORS$grid, zeroline = FALSE),
        hovermode = "x unified",
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        margin = list(l = 70, r = 25, b = 100, t = 70)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  }
  
  output$plot_trend_adhb_overview <- renderPlotly(
    make_summary_trend_plot("PDRB ADHB")
  )
  
  output$plot_trend_adhk_overview <- renderPlotly(
    make_summary_trend_plot("PDRB ADHK")
  )
  
  make_overview_trend_plot <- function() {
    req(input$wilayah)
    
    data <- overview_base_data() %>%
      filter(indikator %in% c("PDRB ADHB", "PDRB ADHK"), !is.na(nilai)) %>%
      quarterly_time_series() %>%
      arrange(waktu_index, indikator)
    
    validate(need(nrow(data) > 0, "Data tren PDRB ADHB dan ADHK belum tersedia."))
    ctx <- overview_context()
    scale_info <- pdrb_value_scale(data$nilai, force_triliun = TRUE)
    data <- data %>% mutate(nilai_plot = nilai / scale_info$scale)
    
    y_axis_max <- suppressWarnings(max(data$nilai_plot, na.rm = TRUE))
    if (!is.finite(y_axis_max) || is.na(y_axis_max) || y_axis_max <= 0) y_axis_max <- 1
    y_axis_max <- y_axis_max * 1.05
    
    p <- plot_ly()
    for (indikator_value in c("PDRB ADHB", "PDRB ADHK")) {
      data_i <- data %>% filter(indikator == indikator_value)
      if (nrow(data_i) == 0) next
      hover_text <- paste0(
        "Waktu: ", data_i$waktu,
        "<br>", indikator_value, ": ", format_indicator_value(data_i$nilai, indikator_value)
      )
      p <- p %>%
        add_trace(
          data = data_i,
          x = ~waktu,
          y = ~nilai_plot,
          type = "scatter",
          mode = "lines+markers",
          name = indikator_value,
          text = hover_text,
          hoverinfo = "text",
          line = list(width = 3, color = indicator_color(indikator_value)),
          marker = list(size = 7, color = indicator_color(indikator_value), line = list(color = "#FFFFFF", width = 1))
        )
    }
    
    p %>%
      layout(
        title = list(text = paste("Tren PDRB ADHB dan ADHK -", ctx$region), x = 0.02),
        xaxis = list(
          title = "Waktu",
          type = "category",
          categoryorder = "array",
          categoryarray = unique(data$waktu),
          tickangle = -45,
          gridcolor = PDRB_COLORS$grid,
          zeroline = FALSE
        ),
        yaxis = list(
          title = scale_info$title,
          separatethousands = TRUE,
          gridcolor = PDRB_COLORS$grid,
          zeroline = TRUE,
          rangemode = "tozero",
          range = c(0, y_axis_max)
        ),
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0, y = 1.12),
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        margin = list(l = 70, r = 25, b = 100, t = 85)
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
  }
  
  output$plot_trend_pdrb_overview <- renderPlotly(
    make_overview_trend_plot()
  )
  
  
  structure_extract_year <- function(x) {
    x_chr <- as.character(x)
    value <- suppressWarnings(as.integer(x_chr))
    missing_value <- is.na(value)
    if (any(missing_value)) {
      extracted <- stringr::str_extract(x_chr[missing_value], "(19|20)[0-9]{2}")
      value[missing_value] <- suppressWarnings(as.integer(extracted))
    }
    value
  }
  
  filter_structure_time <- function(data, selected_period) {
    data %>%
      mutate(
        tahun_pilih_struktur = structure_extract_year(tahun),
        periode_pilih_struktur = as.character(periode)
      ) %>%
      filter(
        tahun_pilih_struktur == selected_period$tahun,
        periode_pilih_struktur == selected_period$periode
      )
  }
  

  output$structure_basis_control_ui <- renderUI({
    req(input$kelompok_indikator)
    group_name <- as.character(input$kelompok_indikator)[1]
    available_indicators <- get_indikator_values(pdrb_data())

    if (identical(group_name, "PDRB")) {
      choices <- c("ADHB" = "PDRB ADHB", "ADHK" = "PDRB ADHK")
      choices <- choices[unname(choices) %in% available_indicators]
      selected <- if (!is.null(input$indikator_non_growth) && input$indikator_non_growth %in% unname(choices)) input$indikator_non_growth else if ("PDRB ADHK" %in% unname(choices)) "PDRB ADHK" else unname(choices[1])
      return(pdrb_selectize("indikator_non_growth", "Dasar Harga", choices = choices, selected = selected))
    }
    if (identical(group_name, "Distribusi")) {
      bases <- c("ADHB" = "ADHB", "ADHK" = "ADHK")
      selected <- if (!is.null(input$dasar_distribusi) && input$dasar_distribusi %in% unname(bases)) input$dasar_distribusi else "ADHB"
      return(pdrb_selectize("dasar_distribusi", "Dasar Harga", choices = bases, selected = selected))
    }
    if (identical(group_name, "Pertumbuhan")) {
      bases <- c("ADHB" = "ADHB", "ADHK" = "ADHK")
      selected <- if (!is.null(input$dasar_pertumbuhan) && input$dasar_pertumbuhan %in% unname(bases)) input$dasar_pertumbuhan else "ADHK"
      return(pdrb_selectize("dasar_pertumbuhan", "Dasar Harga", choices = bases, selected = selected))
    }
    if (identical(group_name, "Indeks Implisit")) {
      choices <- c(
        "Indeks Implisit" = "Indeks Implisit",
        "Laju Indeks Implisit Q-to-Q" = "Laju Indeks Implisit Q-to-Q",
        "Laju Indeks Implisit Y-on-Y" = "Laju Indeks Implisit Y-on-Y",
        "Laju Indeks Implisit C-to-C" = "Laju Indeks Implisit C-to-C"
      )
      choices <- choices[unname(choices) %in% available_indicators]
      selected <- if (!is.null(input$indikator_non_growth) && input$indikator_non_growth %in% unname(choices)) input$indikator_non_growth else unname(choices[1])
      return(pdrb_selectize("indikator_non_growth", "Jenis Indeks", choices = choices, selected = selected))
    }
    if (identical(group_name, "Sumber Pertumbuhan")) {
      bases <- c("ADHB" = "ADHB", "ADHK" = "ADHK")
      selected <- if (!is.null(input$dasar_sumber_pertumbuhan) && input$dasar_sumber_pertumbuhan %in% unname(bases)) input$dasar_sumber_pertumbuhan else "ADHK"
      return(pdrb_selectize("dasar_sumber_pertumbuhan", "Dasar Harga", choices = bases, selected = selected))
    }
    NULL
  })

  output$structure_growth_control_ui <- renderUI({
    req(input$kelompok_indikator)
    group_name <- as.character(input$kelompok_indikator)[1]
    methods <- c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C")
    if (identical(group_name, "Pertumbuhan")) {
      selected <- if (!is.null(input$jenis_pertumbuhan) && input$jenis_pertumbuhan %in% unname(methods)) input$jenis_pertumbuhan else "Y-on-Y"
      return(pdrb_selectize("jenis_pertumbuhan", "Jenis Pertumbuhan", choices = methods, selected = selected))
    }
    if (identical(group_name, "Sumber Pertumbuhan")) {
      selected <- if (!is.null(input$jenis_sumber_pertumbuhan) && input$jenis_sumber_pertumbuhan %in% unname(methods)) input$jenis_sumber_pertumbuhan else "Y-on-Y"
      return(pdrb_selectize("jenis_sumber_pertumbuhan", "Jenis Pertumbuhan", choices = methods, selected = selected))
    }
    # Untuk indikator selain Pertumbuhan dan Sumber Pertumbuhan, kontrol ini tidak ditampilkan.
    # Baris kedua Struktur Ekonomi hanya berisi Tampilan Komposisi serta filter hierarki yang relevan.
    NULL
  })

  structure_active_indicator <- reactive({
    group <- as.character(input$struktur_jenis_nilai)[1]
    basis <- if (identical(group, "Indeks Implisit")) input$struktur_jenis_indeks else input$struktur_dasar_harga
    indikator <- analytics_indicator_value(group, basis, input$struktur_jenis_pertumbuhan)
    validate(need(!is.na(indikator) && nzchar(indikator),
                  "Indikator terpilih belum tersedia untuk Struktur Ekonomi."))
    indikator
  })

  structure_is_distribution <- reactive({
    identical(as.character(input$struktur_jenis_nilai)[1], "Distribusi") ||
      identical(structure_active_indicator(), "Distribusi PDRB")
  })
  
  observe({
    data <- pdrb_data()
    req(nrow(data) > 0, input$wilayah, structure_active_indicator())
    
    structure_time_data <- data %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == structure_active_indicator(),
        as.character(level) %in% c("Kategori Utama", "Subkategori", "Rincian")
      )
    available_years <- structure_time_data %>%
      mutate(tahun_pilih_struktur = structure_extract_year(tahun)) %>%
      filter(!is.na(tahun_pilih_struktur)) %>%
      distinct(tahun_pilih_struktur) %>%
      arrange(tahun_pilih_struktur) %>%
      pull(tahun_pilih_struktur)
    
    req(length(available_years) > 0)
    available_years <- sort(unique(as.integer(available_years)), decreasing = FALSE)
    year_choices <- stats::setNames(as.character(available_years), as.character(available_years))
    current_year <- isolate(as.character(input$tahun_struktur))
    selected_year <- if (!is.null(current_year) && length(current_year) > 0 && current_year %in% unname(year_choices)) current_year else tail(unname(year_choices), 1)
    
    update_pdrb_selectize(
      session, "tahun_struktur",
      choices = year_choices,
      selected = selected_year
    )
  })
  
  observe({
    data <- pdrb_data()
    req(nrow(data) > 0, input$wilayah, structure_active_indicator(), input$tahun_struktur)
    
    selected_year <- suppressWarnings(as.integer(input$tahun_struktur))
    req(!is.na(selected_year))
    
    structure_period_data <- data %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == structure_active_indicator(),
        as.character(level) %in% c("Kategori Utama", "Subkategori", "Rincian")
      )
    available_periods <- structure_period_data %>%
      mutate(tahun_pilih_struktur = structure_extract_year(tahun)) %>%
      filter(tahun_pilih_struktur == selected_year) %>%
      distinct(periode) %>%
      mutate(
        periode_chr = as.character(periode),
        periode_urut = match(periode_chr, c("I", "II", "III", "IV", "Total"))
      ) %>%
      arrange(periode_urut) %>%
      pull(periode_chr)
    
    req(length(available_periods) > 0)
    choices <- setNames(available_periods, period_label(available_periods))
    current_period <- isolate(as.character(input$triwulan_struktur))
    default_period <- if ("Total" %in% available_periods) "Total" else tail(available_periods, 1)
    selected_period <- if (!is.null(current_period) && length(current_period) > 0 && current_period %in% available_periods) current_period else default_period
    
    update_pdrb_selectize(
      session, "triwulan_struktur",
      choices = choices,
      selected = selected_period
    )
  })
  
  structure_period <- reactive({
    req(input$tahun_struktur, input$triwulan_struktur)
    list(
      tahun = as.integer(input$tahun_struktur),
      periode = as.character(input$triwulan_struktur),
      label = paste(period_label(input$triwulan_struktur), input$tahun_struktur)
    )
  })
  
  structure_region_name <- reactive({
    region_name <- pdrb_data() %>%
      filter(kode_wilayah == input$wilayah) %>%
      distinct(wilayah) %>%
      pull(wilayah)
    if (length(region_name) > 0) region_name[1] else "Wilayah terpilih"
  })
  
  structure_basis_label <- reactive({
    indikator <- structure_active_indicator()
    kelompok <- as.character(input$kelompok_indikator)[1]
    if (identical(kelompok, "Distribusi")) {
      dasar <- as.character(input$dasar_distribusi)[1]
      if (is.na(dasar) || !nzchar(dasar)) dasar <- "ADHB"
      return(paste("Distribusi PDRB", dasar))
    }
    indikator
  })

  output$structure_data_label <- renderUI({
    span(paste0("Data: ", structure_basis_label(), "."))
  })
  
  filter_structure_indicator_basis <- function(data) {
    group <- as.character(input$struktur_jenis_nilai)[1]
    if (identical(group, "Distribusi") && "dasar_harga" %in% names(data)) {
      dasar <- as.character(input$struktur_dasar_harga)[1]
      if (is.na(dasar) || !dasar %in% c("ADHB", "ADHK")) dasar <- "ADHB"
      data <- data %>% filter(dasar_harga == dasar)
    }
    data
  }

  structure_display_type <- reactive({
    value <- as.character(input$tampilan_komposisi_struktur)[1]
    if (is.na(value) || !value %in% c("kategori_utama", "subkategori", "rincian")) {
      "kategori_utama"
    } else {
      value
    }
  })
  
  structure_data <- reactive({
    req(input$wilayah, structure_active_indicator(), input$tahun_struktur, input$triwulan_struktur)
    selected_period <- structure_period()
    
    data <- pdrb_data() %>%
      filter_structure_indicator_basis() %>%
      filter_structure_time(selected_period) %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == structure_active_indicator(),
        as.character(level) == "Kategori Utama",
        !is.na(nilai)
      ) %>%
      group_by(kode_kategori, kategori_label, uraian) %>%
      summarise(nilai = sum(nilai, na.rm = TRUE), .groups = "drop")
    
    total_value <- sum(data$nilai, na.rm = TRUE)
    use_share <- structure_is_distribution() || stringr::str_detect(structure_active_indicator(), "^PDRB ")
    data %>%
      mutate(
        label_kode = kode_kategori,
        label_uraian = uraian,
        kontribusi = if (use_share && total_value > 0) nilai / total_value * 100 else NA_real_,
        nilai_label = format_pdrb_plot(nilai),
        kontribusi_label = paste0(scales::number(kontribusi, accuracy = 0.01, decimal.mark = "."), "%")
      ) %>%
      arrange(desc(nilai))
  })
  
  observe({
    req(input$wilayah, structure_active_indicator(), input$tahun_struktur, input$triwulan_struktur)
    selected_period <- structure_period()
    
    category_choices <- pdrb_data() %>%
      filter_structure_indicator_basis() %>%
      filter_structure_time(selected_period) %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == structure_active_indicator(),
        as.character(level) == "Kategori Utama",
        !is.na(kode_kategori), !is.na(kategori_label),
        !is.na(nilai)
      ) %>%
      transmute(kode_utama = kode_kategori, nama_utama = dplyr::coalesce(uraian, kategori_label)) %>%
      distinct(kode_utama, nama_utama) %>%
      arrange(kode_utama)
    
    
    if (nrow(category_choices) == 0) {
      updateSelectizeInput(session, "kategori_utama_struktur", choices = character(0), selected = NULL, server = TRUE)
      updateSelectizeInput(session, "subkategori_struktur", choices = character(0), selected = NULL, server = TRUE)
      return()
    }
    
    choices <- stats::setNames(
      as.character(category_choices$kode_utama),
      paste0(category_choices$kode_utama, " - ", category_choices$nama_utama)
    )
    current <- isolate(input$kategori_utama_struktur)
    selected <- if (!is.null(current) && length(current) > 0 && current %in% unname(choices)) current else unname(choices[1])
    updateSelectizeInput(
      session,
      "kategori_utama_struktur",
      choices = choices,
      selected = selected,
      options = list(dropdownParent = "body", placeholder = "Pilih kategori utama"),
      server = TRUE
    )
  })
  
  subcategory_data <- reactive({
    req(
      input$wilayah, structure_active_indicator(), input$tahun_struktur,
      input$triwulan_struktur, input$kategori_utama_struktur
    )
    selected_period <- structure_period()
    
    data <- pdrb_data() %>%
      filter_structure_indicator_basis() %>%
      filter_structure_time(selected_period) %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == structure_active_indicator(),
        as.character(level) == "Subkategori",
        kode_utama == input$kategori_utama_struktur,
        !is.na(nilai)
      ) %>%
      group_by(kode_kategori, kategori_label, uraian, kode_utama, nama_utama) %>%
      summarise(nilai = sum(nilai, na.rm = TRUE), .groups = "drop")
    
    total_value <- sum(data$nilai, na.rm = TRUE)
    use_share <- structure_is_distribution() || stringr::str_detect(structure_active_indicator(), "^PDRB ")
    data %>%
      mutate(
        label_kode = kode_kategori,
        label_uraian = uraian,
        kontribusi = if (use_share && total_value > 0) nilai / total_value * 100 else NA_real_,
        nilai_label = format_pdrb_plot(nilai),
        kontribusi_label = paste0(scales::number(kontribusi, accuracy = 0.01, decimal.mark = "."), "%")
      ) %>%
      arrange(desc(nilai))
  })
  
  observe({
    data <- subcategory_data()
    if (nrow(data) == 0) {
      updateSelectizeInput(session, "subkategori_struktur", choices = character(0), selected = NULL, server = TRUE)
      return()
    }
    
    choices <- stats::setNames(
      as.character(data$kode_kategori),
      paste0(data$kode_kategori, " - ", data$uraian)
    )
    current <- isolate(input$subkategori_struktur)
    selected <- if (!is.null(current) && length(current) > 0 && current %in% unname(choices)) current else unname(choices[1])
    updateSelectizeInput(
      session,
      "subkategori_struktur",
      choices = choices,
      selected = selected,
      options = list(dropdownParent = "body", placeholder = "Pilih subkategori"),
      server = TRUE
    )
  })
  
  structure_rows_with_parent_subcategory <- reactive({
    req(input$wilayah, structure_active_indicator(), input$tahun_struktur, input$triwulan_struktur, input$kategori_utama_struktur)
    selected_period <- structure_period()
    
    pdrb_data() %>%
      filter_structure_indicator_basis() %>%
      filter_structure_time(selected_period) %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == structure_active_indicator(),
        kode_utama == input$kategori_utama_struktur,
        as.character(level) %in% c("Subkategori", "Rincian")
      ) %>%
      mutate(
        .level_chr = as.character(level),
        parent_sub_code = if_else(.level_chr == "Subkategori", as.character(kode_kategori), NA_character_),
        parent_sub_name = if_else(.level_chr == "Subkategori", as.character(uraian), NA_character_)
      ) %>%
      arrange(source_row, .level_chr) %>%
      tidyr::fill(parent_sub_code, parent_sub_name, .direction = "down")
  })
  
  rincian_subcategory_data <- reactive({
    req(input$subkategori_struktur)
    data <- structure_rows_with_parent_subcategory() %>%
      filter(
        .level_chr == "Rincian",
        parent_sub_code == input$subkategori_struktur,
        !is.na(nilai)
      ) %>%
      group_by(kode_kategori, kategori_label, uraian, kode_utama, nama_utama, parent_sub_code, parent_sub_name) %>%
      summarise(nilai = sum(nilai, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(nilai))
    
    total_value <- sum(data$nilai, na.rm = TRUE)
    use_share <- structure_is_distribution() || stringr::str_detect(structure_active_indicator(), "^PDRB ")
    data %>%
      mutate(
        label_kode = paste0("R", row_number()),
        label_uraian = uraian,
        kontribusi = if (use_share && total_value > 0) nilai / total_value * 100 else NA_real_,
        nilai_label = format_pdrb_plot(nilai),
        kontribusi_label = paste0(scales::number(kontribusi, accuracy = 0.01, decimal.mark = "."), "%")
      ) %>%
      arrange(desc(nilai))
  })
  
  structure_display_data <- reactive({
    display_type <- structure_display_type()
    if (identical(display_type, "subkategori")) {
      subcategory_data()
    } else if (identical(display_type, "rincian")) {
      rincian_subcategory_data()
    } else {
      structure_data()
    }
  })
  
  structure_display_title <- reactive({
    display_type <- structure_display_type()
    selected_period <- structure_period()
    basis_label <- structure_basis_label()
    region_label <- structure_region_name()
    
    if (identical(display_type, "subkategori")) {
      data <- subcategory_data()
      parent_label <- if (nrow(data) > 0) paste0(data$kode_utama[1], " - ", data$nama_utama[1]) else "kategori utama terpilih"
      paste0("Komposisi Subkategori ", parent_label, " - ", basis_label, " - ", region_label, " - ", selected_period$label)
    } else if (identical(display_type, "rincian")) {
      data <- rincian_subcategory_data()
      parent_label <- if (nrow(data) > 0) paste0(data$parent_sub_code[1], " - ", data$parent_sub_name[1]) else "subkategori terpilih"
      paste0("Komposisi Rincian Subkategori ", parent_label, " - ", basis_label, " - ", region_label, " - ", selected_period$label)
    } else {
      paste0("Komposisi Kategori Utama ", basis_label, " - ", region_label, " - ", selected_period$label)
    }
  })
  
  output$vb_structure_total <- renderValueBox({
    data <- structure_data()
    validate(need(nrow(data) > 0, ""))
    selected_period <- structure_period()
    use_total <- structure_is_distribution() || stringr::str_detect(structure_active_indicator(), "^PDRB ")
    summary_value <- if (use_total) sum(data$nilai, na.rm = TRUE) else mean(data$nilai, na.rm = TRUE)
    valueBox(
      format_indicator_value_card(summary_value, structure_active_indicator()),
      paste0(if (use_total) "Total " else "Rata-rata ", structure_basis_label(), " · ", selected_period$label),
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$vb_structure_dominant <- renderValueBox({
    data <- structure_data()
    validate(need(nrow(data) > 0, ""))
    
    top_sector <- data %>%
      slice_max(order_by = nilai, n = 1, with_ties = FALSE)
    
    sector_label <- paste0(
      top_sector$kode_kategori[1],
      " - ",
      stringr::str_trunc(top_sector$uraian[1], 30)
    )
    
    valueBox(
      sector_label,
      paste0(
        if (is.na(top_sector$kontribusi[1])) "Nilai tertinggi · " else "Sektor dominan · ",
        if (is.na(top_sector$kontribusi[1])) scales::number(top_sector$nilai[1], accuracy = 0.0001, big.mark = ",", decimal.mark = ".") else paste0(scales::number(top_sector$kontribusi[1], accuracy = 0.01, decimal.mark = "."), "% dari total")
      ),
      icon = icon("industry"),
      color = "green"
    )
  })
  
  output$vb_structure_top3 <- renderValueBox({
    data <- structure_data()
    validate(need(nrow(data) > 0, ""))
    top3 <- data %>% slice_max(order_by = nilai, n = 3, with_ties = FALSE)
    has_share <- any(!is.na(top3$kontribusi))
    display_value <- if (has_share) {
      paste0(scales::number(sum(top3$kontribusi, na.rm = TRUE), accuracy = 0.01, decimal.mark = "."), "%")
    } else {
      format_indicator_value_card(mean(top3$nilai, na.rm = TRUE), structure_active_indicator())
    }
    valueBox(
      display_value,
      paste0(if (has_share) "Kontribusi" else "Rata-rata", " 3 sektor tertinggi · ", paste(top3$kode_kategori, collapse = ", ")),
      icon = icon("list-ol"), color = "yellow"
    )
  })
  
  prepare_structure_plot_data <- function(data, other_description) {
    # Pie chart selalu memakai proporsi positif terhadap total nilai yang sedang ditampilkan.
    # Ini memastikan indikator non-distribusi tetap memiliki label kode dan persentase seperti Distribusi PDRB.
    total_positive <- sum(data$nilai[data$nilai >= 0], na.rm = TRUE)
    plot_data <- data %>%
      mutate(
        kontribusi_hitung = if (is.finite(total_positive) && total_positive > 0) nilai / total_positive * 100 else NA_real_,
        kontribusi = dplyr::coalesce(kontribusi, kontribusi_hitung)
      ) %>%
      select(-kontribusi_hitung) %>%
      arrange(desc(nilai)) %>%
      mutate(plot_rank = row_number())
    
    if (nrow(plot_data) > 10) {
      top_data <- plot_data %>% slice_head(n = 10)
      other_data <- plot_data %>% filter(plot_rank > 10)
      other_row <- other_data %>%
        summarise(
          kode_kategori = "Lainnya",
          kategori_label = "Lainnya",
          uraian = other_description,
          label_kode = "Lainnya",
          label_uraian = other_description,
          nilai = sum(nilai, na.rm = TRUE),
          kontribusi = sum(kontribusi, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(plot_rank = 11L)
      plot_data <- bind_rows(top_data, other_row)
    }
    
    plot_data %>%
      mutate(
        nilai_label = format_pdrb_plot(nilai),
        kontribusi_label = paste0(scales::number(kontribusi, accuracy = 0.01, decimal.mark = "."), "%"),
        label_pie = if_else(
          kontribusi >= 3,
          paste0(label_kode, "<br>", kontribusi_label),
          ""
        )
      )
  }
  
  make_structure_plot <- function() {
    display_type <- structure_display_type()
    data <- structure_display_data()
    validate(need(nrow(data) > 0, "Data struktur ekonomi belum tersedia untuk kombinasi filter yang dipilih."))

    basis_label <- structure_basis_label()
    has_negative <- any(data$nilai < 0, na.rm = TRUE)

    if (!has_negative) {
      data <- data %>% filter(!is.na(nilai), nilai >= 0)
      validate(need(nrow(data) > 0 && any(data$nilai > 0), "Nilai positif belum tersedia untuk kombinasi filter yang dipilih."))
      other_description <- switch(display_type, "subkategori" = "Subkategori lainnya", "rincian" = "Rincian lainnya", "Kategori lainnya")
      plot_data <- prepare_structure_plot_data(data, other_description)
      contribution_label <- switch(display_type, "subkategori" = "Kontribusi dalam kategori utama", "rincian" = "Kontribusi dalam subkategori", "Kontribusi terhadap total")
      hover_text <- paste0(
        "<b>", plot_data$label_kode, " - ", plot_data$label_uraian, "</b>",
        "<br>", basis_label, ": ", scales::number(plot_data$nilai, accuracy = 0.0001, big.mark = ",", decimal.mark = "."),
        "<br>", contribution_label, ": ", plot_data$kontribusi_label
      )
      return(plot_ly(
        plot_data, labels = ~label_kode, values = ~nilai, type = "pie", hole = 0,
        text = ~label_pie, hovertext = hover_text, hoverinfo = "text", textinfo = "text",
        textposition = "inside", insidetextorientation = "horizontal", automargin = TRUE,
        sort = FALSE, direction = "clockwise",
        marker = list(colors = rep(PDRB_CATEGORY_PALETTE, length.out = nrow(plot_data)), line = list(color = "#FFFFFF", width = 1))
      ) %>% layout(
        title = list(text = structure_display_title(), x = 0.02), showlegend = FALSE,
        uniformtext = list(minsize = 10, mode = "hide"), paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface, font = list(color = PDRB_COLORS$ink),
        margin = list(l = 65, r = 65, b = 45, t = 95)
      ) %>% config(displaylogo = FALSE))
    }

    plot_data <- data %>%
      filter(!is.na(nilai)) %>%
      arrange(nilai) %>%
      mutate(label_plot = paste0(label_kode, " - ", stringr::str_trunc(label_uraian, 45)))
    validate(need(nrow(plot_data) > 0, "Nilai indikator belum tersedia untuk kombinasi filter yang dipilih."))
    hover_text <- paste0("<b>", plot_data$label_kode, " - ", plot_data$label_uraian, "</b><br>", basis_label, ": ",
                         scales::number(plot_data$nilai, accuracy = 0.0001, big.mark = ",", decimal.mark = "."))
    plot_ly(
      plot_data, x = ~nilai, y = ~reorder(label_plot, nilai), type = "bar", orientation = "h",
      hovertext = hover_text, hoverinfo = "text", name = basis_label,
      marker = list(color = indicator_color(structure_active_indicator()))
    ) %>% layout(
      title = list(text = structure_display_title(), x = 0.02),
      xaxis = list(title = basis_label, zeroline = TRUE), yaxis = list(title = ""),
      showlegend = FALSE, paper_bgcolor = PDRB_COLORS$surface, plot_bgcolor = PDRB_COLORS$surface,
      font = list(color = PDRB_COLORS$ink), margin = list(l = 250, r = 35, b = 80, t = 95)
    ) %>% config(displaylogo = FALSE)
  }

  output$plot_structure <- renderPlotly({
    req(identical(as.character(input$tabs)[1], "struktur"), cancelOutput = TRUE)
    req(input$wilayah, structure_active_indicator(), input$tahun_struktur, input$triwulan_struktur, cancelOutput = TRUE)
    display_type <- structure_display_type()
    if (identical(display_type, "subkategori")) {
      req(input$kategori_utama_struktur, cancelOutput = TRUE)
    }
    if (identical(display_type, "rincian")) {
      req(input$kategori_utama_struktur, input$subkategori_struktur, cancelOutput = TRUE)
    }
    make_structure_plot()
  })
  
  observeEvent(
    list(input$tahun_struktur, input$triwulan_struktur, structure_active_indicator(), input$tampilan_komposisi_struktur, input$kategori_utama_struktur, input$subkategori_struktur),
    {
      session$sendCustomMessage("resizePlotly", "plot_structure")
    },
    ignoreInit = TRUE
  )
  
  write_export_xlsx <- function(data, file, sheet_name = "Data") {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    sheet_name <- substr(gsub("[\\/:*?\\[\\]]", " ", sheet_name), 1, 31)
    if (!nzchar(trimws(sheet_name))) sheet_name <- "Data"

    workbook <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(workbook, sheet_name, gridLines = FALSE)

    header_style <- openxlsx::createStyle(
      fontColour = "#FFFFFF",
      fgFill = "#326A92",
      textDecoration = "bold",
      halign = "center",
      valign = "center",
      border = "Bottom",
      borderColour = "#D9E2E8"
    )
    body_style <- openxlsx::createStyle(
      border = "TopBottomLeftRight",
      borderColour = "#E1E6EA",
      valign = "top",
      wrapText = TRUE
    )
    numeric_style <- openxlsx::createStyle(
      numFmt = "#,##0.###############",
      border = "TopBottomLeftRight",
      borderColour = "#E1E6EA",
      valign = "top"
    )

    openxlsx::writeData(
      workbook,
      sheet = sheet_name,
      x = data,
      startRow = 1,
      startCol = 1,
      headerStyle = header_style,
      withFilter = TRUE,
      keepNA = FALSE
    )

    if (nrow(data) > 0 && ncol(data) > 0) {
      openxlsx::addStyle(
        workbook,
        sheet = sheet_name,
        style = body_style,
        rows = 2:(nrow(data) + 1),
        cols = seq_len(ncol(data)),
        gridExpand = TRUE,
        stack = TRUE
      )
      numeric_cols <- which(vapply(data, is.numeric, logical(1)))
      if (length(numeric_cols) > 0) {
        openxlsx::addStyle(
          workbook,
          sheet = sheet_name,
          style = numeric_style,
          rows = 2:(nrow(data) + 1),
          cols = numeric_cols,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }

    openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
    openxlsx::setColWidths(workbook, sheet_name, cols = seq_len(max(1, ncol(data))), widths = "auto")
    openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  }

  write_export_csv <- function(data, file) {
    old_digits <- getOption("digits")
    on.exit(options(digits = old_digits), add = TRUE)
    options(digits = 15)

    utils::write.csv(
      as.data.frame(data, stringsAsFactors = FALSE),
      file,
      row.names = FALSE,
      na = "",
      fileEncoding = "UTF-8"
    )
  }

  # Data reactive tidak dibulatkan agar tombol unduh menerima nilai presisi penuh.
  # Empat desimal hanya diterapkan pada renderDT di bawah ini.
  structure_table_data <- reactive({
    req(input$wilayah, structure_active_indicator(), input$tahun_struktur, input$triwulan_struktur, cancelOutput = TRUE)
    display_type <- structure_display_type()
    if (identical(display_type, "subkategori")) {
      req(input$kategori_utama_struktur, cancelOutput = TRUE)
    }
    if (identical(display_type, "rincian")) {
      req(input$kategori_utama_struktur, input$subkategori_struktur, cancelOutput = TRUE)
    }

    data <- structure_display_data() %>%
      mutate(Peringkat = row_number())

    label_column <- switch(
      display_type,
      "subkategori" = "Subkategori",
      "rincian" = "Rincian Subkategori",
      "Kategori"
    )
    contribution_column <- switch(
      display_type,
      "subkategori" = "Kontribusi dalam kategori (%)",
      "rincian" = "Kontribusi dalam subkategori (%)",
      "Kontribusi terhadap total (%)"
    )

    data_out <- data.frame(
      Peringkat = data$Peringkat,
      Kode = data$label_kode,
      Label = data$label_uraian,
      Nilai = as.numeric(data$nilai),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    names(data_out)[3] <- label_column
    if (any(!is.na(data$kontribusi))) {
      data_out[[contribution_column]] <- as.numeric(data$kontribusi)
    }
    data_out
  })

  output$structure_table <- renderDT({
    data_out <- structure_table_data()
    table_output <- datatable(
      data_out,
      rownames = FALSE,
      options = pdrb_dt_options(pageLength = 10, buttons = FALSE, dom = "lfrtip")
    )
    if ("Nilai" %in% names(data_out)) {
      table_output <- table_output %>%
        formatRound("Nilai", digits = 4, mark = ",", dec.mark = ".")
    }
    contribution_cols <- grep("Kontribusi", names(data_out), value = TRUE)
    if (length(contribution_cols) > 0) {
      table_output <- table_output %>%
        formatRound(contribution_cols, digits = 4, mark = ",", dec.mark = ".")
    }
    table_output
  }, server = FALSE)
  
  comparison_is_total_distribution <- reactive({
    identical(as.character(input$komparasi_jenis_nilai)[1], "Distribusi") &&
      identical(
        as.character(input$komparasi_jenis_sektor)[1],
        "Total PDRB__PDRB__PRODUK DOMESTIK REGIONAL BRUTO"
      )
  })

  comparison_candidates <- reactive({
    req(input$kelompok, input$komparasi_jenis_sektor, input$komparasi_jenis_nilai)

    selected_item_id <- as.character(input$komparasi_jenis_sektor)[1]
    indicator_value <- comparison_indicator()

    req(
      !is.na(selected_item_id), nzchar(selected_item_id),
      !is.na(indicator_value), nzchar(indicator_value)
    )

    if (isTRUE(comparison_is_total_distribution())) {
      basis_value <- as.character(input$komparasi_dasar_harga)[1]
      if (is.na(basis_value) || !basis_value %in% c("ADHB", "ADHK")) basis_value <- "ADHB"

      data <- pdrb_data() %>%
        filter(
          kode_kelompok == input$kelompok,
          .data$item_id == .env$selected_item_id,
          indikator == paste("PDRB", basis_value),
          !is.na(tahun),
          !is.na(periode),
          !is.na(nilai),
          is.finite(nilai)
        ) %>%
        mutate(
          indikator = "Distribusi PDRB",
          dasar_harga = basis_value,
          nilai = 100,
          satuan = "Persen"
        )
    } else {
      data <- pdrb_data() %>%
        filter(
          kode_kelompok == input$kelompok,
          .data$item_id == .env$selected_item_id,
          indikator == indicator_value,
          !is.na(tahun),
          !is.na(periode),
          !is.na(nilai),
          is.finite(nilai)
        )

      if (identical(as.character(input$komparasi_jenis_nilai)[1], "Distribusi") &&
          "dasar_harga" %in% names(data)) {
        basis_value <- as.character(input$komparasi_dasar_harga)[1]
        if (is.na(basis_value) || !basis_value %in% c("ADHB", "ADHK")) basis_value <- "ADHB"
        data <- data %>% filter(dasar_harga == basis_value)
      }
    }

    data %>%
      mutate(
        periode = as.character(periode),
        jenis_wilayah = normalize_region_type(jenis_wilayah, kode_wilayah, kode_kelompok),
        is_agregat = is_aggregate_region(kode_wilayah, kode_kelompok, jenis_wilayah)
      )
  })

  observe({
    data <- comparison_candidates()
    years <- sort(unique(as.integer(data$tahun)), decreasing = FALSE)
    years <- years[!is.na(years)]
    current_year <- isolate(input$tahun_perbandingan)
    
    selected_year <- if (!is.null(current_year) && current_year %in% as.character(years)) {
      current_year
    } else if (length(years) > 0) {
      as.character(min(years, na.rm = TRUE))
    } else {
      NULL
    }
    
    updateSelectizeInput(
      session,
      "tahun_perbandingan",
      choices = stats::setNames(as.character(years), as.character(years)),
      selected = selected_year,
      server = FALSE
    )
  })
  
  observe({
    data <- comparison_candidates()
    req(input$tahun_perbandingan)
    selected_year <- suppressWarnings(as.integer(input$tahun_perbandingan))
    
    periods <- data %>%
      filter(tahun == selected_year) %>%
      pull(periode) %>%
      as.character() %>%
      unique()
    periods <- periods[order(period_rank(periods), na.last = NA)]
    current_period <- isolate(input$periode_perbandingan)
    
    if (!is.null(current_period) && current_period %in% periods) {
      selected_period <- current_period
    } else {
      selected_period <- if ("Total" %in% periods) {
        "Total"
      } else if (length(periods) > 0) {
        tail(periods, 1)
      } else {
        NULL
      }
    }
    
    updateSelectizeInput(
      session,
      "periode_perbandingan",
      choices = stats::setNames(periods, period_label(periods)),
      selected = selected_period,
      server = FALSE
    )
  })
  
  
  observe({
    data <- comparison_candidates()
    region_choices <- data %>%
      filter(!is_agregat) %>%
      distinct(kode_wilayah, wilayah) %>%
      arrange(wilayah)
    updateSelectizeInput(
      session,
      "wilayah_banding",
      choices = stats::setNames(region_choices$kode_wilayah, region_choices$wilayah),
      selected = character(0),
      server = TRUE
    )
  })
  
  comparison_period <- reactive({
    req(input$tahun_perbandingan, input$periode_perbandingan)
    selected_year <- suppressWarnings(as.integer(input$tahun_perbandingan))
    selected_period <- as.character(input$periode_perbandingan)
    req(!is.na(selected_year), nzchar(selected_period))
    
    list(
      tahun = selected_year,
      periode = selected_period,
      label = paste(period_label(selected_period), selected_year)
    )
  })
  
  comparison_axis_info <- function(indikator, satuan = NULL) {
    indikator <- as.character(indikator)[1]
    if (indikator %in% c("PDRB ADHB", "PDRB ADHK")) {
      return(list(scale = 1e6, title = "Triliun Rupiah"))
    }
    
    satuan <- as.character(satuan)[1]
    if (is.na(satuan) || !nzchar(satuan)) satuan <- "Nilai"
    list(scale = 1, title = satuan)
  }
  
  comparison_data <- reactive({
    selected_period <- comparison_period()
    
    data <- comparison_candidates() %>%
      filter(
        tahun == selected_period$tahun,
        periode == selected_period$periode
      )
    
    aggregate_data <- data %>% filter(is_agregat)
    detail_data <- data %>% filter(!is_agregat)
    
    selected_regions <- input$wilayah_banding
    has_selected_regions <- !is.null(selected_regions) &&
      length(selected_regions) > 0 &&
      any(nzchar(as.character(selected_regions)))
    
    if (has_selected_regions) {
      data_out <- detail_data %>% filter(kode_wilayah %in% selected_regions)
    } else if (nrow(detail_data) > 0) {
      data_out <- detail_data
    } else {
      data_out <- data
    }
    
    if (isTRUE(input$include_agregat_perbandingan) && nrow(aggregate_data) > 0) {
      data_out <- bind_rows(data_out, aggregate_data)
    }
    
    data_out %>%
      distinct(kode_wilayah, indikator, tahun, periode, kode_kategori, .keep_all = TRUE) %>%
      arrange(desc(nilai), wilayah)
  })
  
  comparison_summary <- reactive({
    data <- comparison_data() %>%
      filter(!is.na(nilai), is.finite(nilai))
    
    if (nrow(data) == 0) {
      return(list(high = NULL, low = NULL, ratio = NA_real_, all_equal = FALSE))
    }
    
    all_equal <- dplyr::n_distinct(signif(data$nilai, 12)) <= 1L
    high <- data %>% arrange(desc(nilai), wilayah) %>% slice(1)
    low <- data %>% arrange(nilai, wilayah) %>% slice(1)
    ratio <- if (
      nrow(high) > 0 && nrow(low) > 0 &&
      is.finite(high$nilai[[1]]) && is.finite(low$nilai[[1]]) &&
      low$nilai[[1]] > 0
    ) {
      high$nilai[[1]] / low$nilai[[1]]
    } else {
      NA_real_
    }
    
    list(high = high, low = low, ratio = ratio, all_equal = all_equal)
  })
  
  output$vb_compare_high <- renderValueBox({
    summary <- comparison_summary()
    if (is.null(summary$high)) {
      return(valueBox("–", "Wilayah tertinggi", icon = icon("arrow-up"), color = "green"))
    }
    if (isTRUE(summary$all_equal)) {
      return(valueBox(
        "Semua wilayah",
        paste0("Nilai sama · ", format_indicator_value_card(summary$high$nilai[[1]], comparison_indicator())),
        icon = icon("balance-scale"),
        color = "green"
      ))
    }
    valueBox(
      summary$high$wilayah[[1]],
      paste0("Wilayah tertinggi · ", format_indicator_value_card(summary$high$nilai[[1]], comparison_indicator())),
      icon = icon("arrow-up"),
      color = "green"
    )
  })

  output$vb_compare_low <- renderValueBox({
    summary <- comparison_summary()
    if (is.null(summary$low)) {
      return(valueBox("–", "Wilayah terendah", icon = icon("arrow-down"), color = "yellow"))
    }
    if (isTRUE(summary$all_equal)) {
      return(valueBox(
        "Semua wilayah",
        paste0("Nilai sama · ", format_indicator_value_card(summary$low$nilai[[1]], comparison_indicator())),
        icon = icon("balance-scale"),
        color = "yellow"
      ))
    }
    valueBox(
      summary$low$wilayah[[1]],
      paste0("Wilayah terendah · ", format_indicator_value_card(summary$low$nilai[[1]], comparison_indicator())),
      icon = icon("arrow-down"),
      color = "yellow"
    )
  })

  output$vb_compare_ratio <- renderValueBox({
    summary <- comparison_summary()
    ratio_label <- if (is.na(summary$ratio) || !is.finite(summary$ratio)) {
      "–"
    } else {
      paste0(scales::number(summary$ratio, accuracy = 0.01, big.mark = ",", decimal.mark = "."), " kali")
    }
    ratio_subtitle <- if (isTRUE(summary$all_equal)) "Semua wilayah bernilai sama" else "Rasio tertinggi/terendah"
    valueBox(
      ratio_label,
      ratio_subtitle,
      icon = icon("balance-scale"),
      color = "orange"
    )
  })

  make_comparison_plot <- function() {
    data <- comparison_data()
    validate(need(nrow(data) > 0, "Data perbandingan belum tersedia."))
    
    axis_info <- if (comparison_indicator() %in% c("PDRB ADHB", "PDRB ADHK")) pdrb_value_scale(data$nilai, force_triliun = any(as.character(data$level) == "Total PDRB", na.rm = TRUE)) else comparison_axis_info(comparison_indicator(), unique(data$satuan)[1])
    data_plot <- data %>%
      mutate(
        nilai_plot = nilai / axis_info$scale,
        nilai_label = format_indicator_value_plot(nilai, comparison_indicator(), axis_info),
        wilayah_plot = factor(wilayah, levels = rev(unique(wilayah)))
      )
    
    finite_values <- data_plot$nilai_plot[is.finite(data_plot$nilai_plot)]
    xaxis_config <- list(
      title = axis_info$title,
      separatethousands = TRUE,
      gridcolor = PDRB_COLORS$grid,
      zeroline = FALSE
    )
    if (length(finite_values) > 0) {
      if (all(finite_values >= 0)) {
        xaxis_config$range <- c(0, max(finite_values, na.rm = TRUE) * 1.16)
      } else if (all(finite_values <= 0)) {
        xaxis_config$range <- c(min(finite_values, na.rm = TRUE) * 1.16, 0)
      } else {
        xaxis_config$range <- c(
          min(finite_values, na.rm = TRUE) * 1.16,
          max(finite_values, na.rm = TRUE) * 1.16
        )
      }
    }
    
    plot_ly(
      data_plot,
      x = ~nilai_plot,
      y = ~wilayah_plot,
      type = "bar", orientation = "h",
      marker = list(color = indicator_color(comparison_indicator())),
      text = ~nilai_label,
      textposition = "outside",
      hovertemplate = "%{y}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(text = paste(comparison_indicator(), "-", analytics_context_for(input$komparasi_jenis_sektor)$category, "•", comparison_period()$label), x = 0.02),
        xaxis = xaxis_config,
        yaxis = list(title = ""),
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        margin = list(l = 190, r = 95, b = 55, t = 70),
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE)
  }
  
  output$plot_compare <- renderPlotly({
    req(identical(as.character(input$tabs)[1], "perbandingan"), cancelOutput = TRUE)
    make_comparison_plot()
  })
  
  comparison_table_data <- reactive({
    data <- comparison_data()
    validate(need(nrow(data) > 0, "Data peringkat wilayah belum tersedia."))

    n_data <- nrow(data)
    all_equal <- dplyr::n_distinct(signif(data$nilai, 12)) <= 1L
    max_value <- max(data$nilai, na.rm = TRUE)
    min_value <- min(data$nilai, na.rm = TRUE)

    data %>%
      arrange(desc(nilai), wilayah) %>%
      mutate(
        Peringkat = dense_rank(desc(nilai)),
        Status = case_when(
          all_equal ~ "Nilai sama",
          n_data == 1 ~ "Tertinggi/Terendah",
          nilai == max_value ~ "Tertinggi",
          nilai == min_value ~ "Terendah",
          TRUE ~ "Menengah"
        ),
        Nilai = as.numeric(nilai)
      ) %>%
      transmute(
        Peringkat,
        Wilayah = wilayah,
        Nilai,
        Status
      )
  })

  output$table_compare_rank <- renderDT({
    data <- comparison_table_data()
    table_output <- datatable(
      data,
      rownames = FALSE,
      options = pdrb_dt_options(pageLength = 10, buttons = FALSE, dom = "lfrtip")
    )
    if ("Nilai" %in% names(data)) {
      table_output <- table_output %>%
        formatRound("Nilai", digits = 4, mark = ",", dec.mark = ".")
    }
    table_output
  }, server = FALSE)



  structure_download_filename_v919 <- function(extension) {
    indicator_parts <- analytics_filename_parts_v919(
      group = input$struktur_jenis_nilai,
      basis = input$struktur_dasar_harga,
      index = input$struktur_jenis_indeks,
      growth = input$struktur_jenis_pertumbuhan
    )
    build_download_filename_v919(
      c(
        "Struktur Ekonomi",
        indicator_parts,
        download_region_name_v919(input$wilayah),
        download_year_part_v919(input$tahun_struktur),
        download_period_part_v919(input$triwulan_struktur)
      ),
      extension
    )
  }

  comparison_download_filename_v919 <- function(extension) {
    indicator_parts <- analytics_filename_parts_v919(
      group = input$komparasi_jenis_nilai,
      basis = input$komparasi_dasar_harga,
      index = input$komparasi_jenis_indeks,
      growth = input$komparasi_jenis_pertumbuhan
    )
    sector_label <- tryCatch(
      analytics_context_for(input$komparasi_jenis_sektor)$category,
      error = function(e) NULL
    )
    build_download_filename_v919(
      c(
        "Komparasi Wilayah",
        indicator_parts,
        download_group_name_v919(input$kelompok),
        sector_label,
        download_year_part_v919(input$tahun_perbandingan),
        download_period_part_v919(input$periode_perbandingan)
      ),
      extension
    )
  }

  output$download_structure_excel <- downloadHandler(
    filename = function() structure_download_filename_v919("xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      write_export_xlsx(structure_table_data(), file, "Struktur Ekonomi")
    }
  )

  output$download_structure_csv <- downloadHandler(
    filename = function() structure_download_filename_v919("csv"),
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      write_export_csv(structure_table_data(), file)
    }
  )

  output$download_compare_excel <- downloadHandler(
    filename = function() comparison_download_filename_v919("xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      write_export_xlsx(comparison_table_data(), file, "Komparasi Wilayah")
    }
  )

  output$download_compare_csv <- downloadHandler(
    filename = function() comparison_download_filename_v919("csv"),
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      write_export_csv(comparison_table_data(), file)
    }
  )
  
  make_top_category_plot <- function(indikator_value) {
    req(input$wilayah)
    top_n <- suppressWarnings(as.integer(input$jumlah_top_ringkasan))
    if (is.na(top_n) || top_n < 1L) top_n <- 5L
    
    candidates <- pdrb_data() %>%
      filter(
        kode_wilayah == input$wilayah,
        indikator == indikator_value,
        as.character(level) == "Kategori Utama",
        !is.na(nilai)
      )
    selected_period <- overview_snapshot_period()
    req(!is.na(selected_period$tahun), !is.na(selected_period$periode))
    
    data <- candidates %>%
      filter(
        tahun == selected_period$tahun,
        as.character(periode) == selected_period$periode
      ) %>%
      slice_max(nilai, n = top_n, with_ties = FALSE) %>%
      arrange(nilai) %>%
      mutate(
        label = paste0(kode_kategori, " - ", stringr::str_trunc(uraian, 34))
      )
    
    validate(need(nrow(data) > 0, paste("Data kategori", indikator_value, "belum tersedia.")))
    scale_info <- pdrb_value_scale(data$nilai)
    data <- data %>% mutate(nilai_plot = nilai / scale_info$scale)
    top_text <- format_pdrb_plot(data$nilai, scale_info)
    
    plot_ly(
      data,
      x = ~nilai_plot, y = ~reorder(label, nilai_plot),
      type = "bar", orientation = "h",
      marker = list(color = indicator_color(indikator_value)),
      text = top_text,
      hovertemplate = "%{y}<br>%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = paste0("Top ", top_n, " Lapangan Usaha ", indikator_value, " • ", selected_period$label),
          x = 0.02
        ),
        xaxis = list(title = scale_info$title, separatethousands = TRUE, gridcolor = PDRB_COLORS$grid, zeroline = FALSE),
        yaxis = list(title = ""),
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        margin = list(l = 190, r = 20, b = 50, t = 65),
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE)
  }
  
  output$plot_top_adhb <- renderPlotly(
    make_top_category_plot("PDRB ADHB")
  )
  
  output$plot_top_adhk <- renderPlotly(
    make_top_category_plot("PDRB ADHK")
  )
  
  calculate_lq_table <- function(data, kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga) {
    indikator_pilih <- paste("PDRB", dasar_harga)
    level_pilih <- normalize_lq_level(level_pilih)
    level_dipakai <- if (identical(level_pilih, "Semua")) c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian") else level_pilih
    
    data_indikator <- data %>%
      mutate(level = as.character(level), periode = as.character(periode)) %>%
      filter(kode_kelompok == kode_kelompok_pilih, indikator == indikator_pilih) %>%
      canonicalize_pdrb_rows()
    if (nrow(data_indikator) == 0) return(tibble())
    
    kode_provinsi <- data_indikator %>%
      filter(kode_wilayah == kode_wilayah_pilih) %>%
      distinct(kode_kelompok) %>%
      pull(kode_kelompok)
    if (length(kode_provinsi) == 0 || is.na(kode_provinsi[1])) kode_provinsi <- kode_kelompok_pilih else kode_provinsi <- kode_provinsi[1]
    
    nilai_wilayah <- data_indikator %>%
      filter(kode_wilayah == kode_wilayah_pilih, level %in% level_dipakai) %>%
      select(kode_kelompok, kelompok, kode_wilayah, wilayah, level, kode_kategori, kategori_label, uraian, item_id, tahun, periode, nilai_wilayah = nilai, source_file, source_row) %>%
      collapse_for_join(c("kode_kelompok", "kode_wilayah", "level", "kode_kategori", "tahun", "periode"))
    
    total_wilayah <- data_indikator %>%
      filter(kode_wilayah == kode_wilayah_pilih, level == "Total PDRB", kode_kategori == "PDRB") %>%
      select(kode_wilayah, tahun, periode, total_wilayah = nilai, source_file, source_row) %>%
      collapse_for_join(c("kode_wilayah", "tahun", "periode"))
    
    nilai_provinsi <- data_indikator %>%
      filter(kode_wilayah == kode_provinsi, level %in% level_dipakai) %>%
      select(level, kode_kategori, tahun, periode, nilai_provinsi = nilai, source_file, source_row) %>%
      collapse_for_join(c("level", "kode_kategori", "tahun", "periode"))
    
    total_provinsi <- data_indikator %>%
      filter(kode_wilayah == kode_provinsi, level == "Total PDRB", kode_kategori == "PDRB") %>%
      select(tahun, periode, total_provinsi = nilai, source_file, source_row) %>%
      collapse_for_join(c("tahun", "periode"))
    
    wilayah_pembanding_lq <- data_indikator %>%
      filter(kode_wilayah == kode_provinsi) %>%
      distinct(wilayah) %>%
      pull(wilayah)
    wilayah_pembanding_lq <- if (length(wilayah_pembanding_lq) > 0) wilayah_pembanding_lq[[1]] else kode_provinsi
    
    nilai_wilayah %>%
      left_join(total_wilayah, by = c("kode_wilayah", "tahun", "periode")) %>%
      left_join(nilai_provinsi, by = c("level", "kode_kategori", "tahun", "periode")) %>%
      left_join(total_provinsi, by = c("tahun", "periode")) %>%
      mutate(
        kode_wilayah_pembanding = kode_provinsi,
        wilayah_pembanding = wilayah_pembanding_lq,
        share_wilayah = if_else(!is.na(total_wilayah) & total_wilayah != 0, nilai_wilayah / total_wilayah * 100, NA_real_),
        share_pembanding = if_else(!is.na(total_provinsi) & total_provinsi != 0, nilai_provinsi / total_provinsi * 100, NA_real_),
        Indikator = paste("LQ", dasar_harga),
        LQ = if_else(
          !is.na(nilai_wilayah) & !is.na(total_wilayah) & !is.na(nilai_provinsi) & !is.na(total_provinsi) &
            total_wilayah != 0 & total_provinsi != 0 & nilai_provinsi != 0,
          (nilai_wilayah / total_wilayah) / (nilai_provinsi / total_provinsi),
          NA_real_
        ),
        Keterangan = case_when(
          is.na(LQ) ~ "Tidak dihitung",
          LQ > 1 ~ "Basis",
          LQ == 1 ~ "Sama",
          LQ < 1 ~ "Nonbasis"
        )
      ) %>%
      collapse_for_join(c("kode_kelompok", "kode_wilayah", "level", "kode_kategori", "tahun", "periode")) %>%
      arrange(level, kode_kategori, tahun, match(periode, c("I", "II", "III", "IV", "Total")))
  }
  
  observe({
    data <- pdrb_data()
    req(nrow(data) > 0)
    
    tahun_tersedia <- data %>%
      filter(indikator %in% c("PDRB ADHB", "PDRB ADHK")) %>%
      distinct(tahun) %>%
      arrange(tahun) %>%
      pull(tahun)
    
    tahun_tersedia <- tahun_tersedia[!is.na(tahun_tersedia)]
    req(length(tahun_tersedia) > 0)
    
    year_choices_all <- c("Semua Tahun" = "__ALL__", setNames(as.character(tahun_tersedia), as.character(tahun_tersedia)))
    period_choices_all <- c(
      "Semua Periode" = "__ALL__",
      "Triwulan I" = "I",
      "Triwulan II" = "II",
      "Triwulan III" = "III",
      "Triwulan IV" = "IV",
      "Tahun" = "Total"
    )
    
    tahun_shift_tersedia <- tahun_tersedia[(tahun_tersedia - 1L) %in% tahun_tersedia]
    if (length(tahun_shift_tersedia) == 0) tahun_shift_tersedia <- tahun_tersedia
    year_choices_shift <- setNames(as.character(tahun_shift_tersedia), as.character(tahun_shift_tersedia))
    
    # Tahun LQ hanya berasal dari irisan tahun valid wilayah dan provinsi pembanding.
    # Jangan fallback ke seluruh tahun PDRB karena dapat membuat kolom LQ yang tidak dapat dihitung.
    lq_years_update <- tryCatch(valid_lq_years(), error = function(e) integer(0))
    dlq_years_update <- tryCatch(valid_dlq_years(), error = function(e) tahun_tersedia)
    if (length(dlq_years_update) == 0) dlq_years_update <- tahun_tersedia
    shift_start_update <- tryCatch(valid_shift_start_years(), error = function(e) integer(0))
    shift_end_update <- tryCatch(valid_shift_years(), error = function(e) integer(0))
    if (length(shift_start_update) == 0) shift_start_update <- if (length(tahun_tersedia) > 1) tahun_tersedia[-length(tahun_tersedia)] else tahun_tersedia
    if (length(shift_end_update) == 0) shift_end_update <- if (length(tahun_tersedia) > 1) tahun_tersedia[-1] else tahun_tersedia
    
    update_pdrb_selectize(session, "tahun_lq", choices = c("Semua Tahun" = "__ALL__", valid_year_choices(lq_years_update)), selected = "__ALL__")
    update_pdrb_selectize(session, "periode_lq_peringkat", choices = c("Semua Periode" = "__ALL__", "Semua Triwulan" = "__QUARTERS__", "Triwulan I" = "I", "Triwulan II" = "II", "Triwulan III" = "III", "Triwulan IV" = "IV", "Tahun" = "Total"), selected = "__QUARTERS__")
    dlq_start_update <- dlq_years_update[(dlq_years_update + 1L) %in% dlq_years_update]
    if (length(dlq_start_update) == 0 && length(dlq_years_update) > 1) dlq_start_update <- dlq_years_update[-length(dlq_years_update)]
    update_pdrb_selectize(
      session, "tahun_awal_analisis",
      choices = c("Semua Tahun" = "__ALL__", valid_year_choices(dlq_start_update)),
      selected = "__ALL__"
    )
    update_pdrb_selectize(
      session, "tahun_akhir_analisis",
      choices = c("Semua Tahun" = "__ALL__"),
      selected = "__ALL__"
    )
    update_pdrb_selectize(session, "tahun_awal_shift_analisis", choices = valid_year_choices(shift_start_update), selected = as.character(min(shift_start_update, na.rm = TRUE)))
    update_pdrb_selectize(session, "tahun_akhir_shift_analisis", choices = valid_year_choices(shift_end_update), selected = as.character(max(shift_end_update, na.rm = TRUE)))
    
  })
  
  
  
  observe({
    input$jenis_tabel_data
    data <- pdrb_data()
    req(nrow(data) > 0)
    
    tahun_tersedia <- sort(unique(as.integer(data$tahun)))
    tahun_tersedia <- tahun_tersedia[!is.na(tahun_tersedia)]
    req(length(tahun_tersedia) > 0)
    
    year_choices_all <- c("Semua Tahun" = "__ALL__", setNames(as.character(tahun_tersedia), as.character(tahun_tersedia)))
    period_choices_all <- c(
      "Semua Periode" = "__ALL__",
      "Triwulan I" = "I",
      "Triwulan II" = "II",
      "Triwulan III" = "III",
      "Triwulan IV" = "IV",
      "Tahun" = "Total"
    )
    year_choices <- setNames(as.character(tahun_tersedia), as.character(tahun_tersedia))
    tahun_shift_tersedia <- tahun_tersedia[(tahun_tersedia - 1L) %in% tahun_tersedia]
    if (length(tahun_shift_tersedia) == 0) tahun_shift_tersedia <- tahun_tersedia
    year_choices_shift <- setNames(as.character(tahun_shift_tersedia), as.character(tahun_shift_tersedia))
    
    # Tahun LQ hanya berasal dari irisan tahun valid wilayah dan provinsi pembanding.
    # Jangan fallback ke seluruh tahun PDRB karena dapat membuat kolom LQ yang tidak dapat dihitung.
    lq_years_update <- tryCatch(valid_lq_years(), error = function(e) integer(0))
    dlq_years_update <- tryCatch(valid_dlq_years(), error = function(e) tahun_tersedia)
    if (length(dlq_years_update) == 0) dlq_years_update <- tahun_tersedia
    shift_start_update <- tryCatch(valid_shift_start_years(), error = function(e) integer(0))
    shift_end_update <- tryCatch(valid_shift_years(), error = function(e) integer(0))
    if (length(shift_start_update) == 0) shift_start_update <- if (length(tahun_tersedia) > 1) tahun_tersedia[-length(tahun_tersedia)] else tahun_tersedia
    if (length(shift_end_update) == 0) shift_end_update <- if (length(tahun_tersedia) > 1) tahun_tersedia[-1] else tahun_tersedia
    
    update_pdrb_selectize(session, "tahun_lq", choices = c("Semua Tahun" = "__ALL__", valid_year_choices(lq_years_update)), selected = "__ALL__")
    update_pdrb_selectize(session, "periode_lq_peringkat", choices = c("Semua Periode" = "__ALL__", "Semua Triwulan" = "__QUARTERS__", "Triwulan I" = "I", "Triwulan II" = "II", "Triwulan III" = "III", "Triwulan IV" = "IV", "Tahun" = "Total"), selected = "__QUARTERS__")
    update_pdrb_selectize(session, "tahun_awal_shift_analisis", choices = valid_year_choices(shift_start_update), selected = as.character(min(shift_start_update, na.rm = TRUE)))
    update_pdrb_selectize(session, "tahun_akhir_shift_analisis", choices = valid_year_choices(shift_end_update), selected = as.character(max(shift_end_update, na.rm = TRUE)))
  })
  
  observeEvent(input$tahun_awal_analisis, {
    years <- tryCatch(valid_dlq_years(), error = function(e) integer(0))
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    awal_input <- as.character(input$tahun_awal_analisis)[1]

    if (is.na(awal_input) || !nzchar(awal_input) || identical(awal_input, "__ALL__")) {
      update_pdrb_selectize(
        session, "tahun_akhir_analisis",
        choices = c("Semua Tahun" = "__ALL__"),
        selected = "__ALL__"
      )
      return(NULL)
    }

    awal <- suppressWarnings(as.integer(awal_input))
    if (is.na(awal) || length(years) == 0) return(NULL)
    end_choices <- years[years == awal + 1L]
    if (length(end_choices) == 0) return(NULL)
    update_pdrb_selectize(
      session, "tahun_akhir_analisis",
      choices = valid_year_choices(end_choices),
      selected = as.character(end_choices[[1]])
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$tahun_awal_shift_analisis, {
    years <- tryCatch(valid_dlq_years(), error = function(e) integer(0))
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    awal <- suppressWarnings(as.integer(input$tahun_awal_shift_analisis))
    end_choices <- years[years > awal]
    if (length(end_choices) == 0) return(NULL)
    selected <- if (!is.null(input$tahun_akhir_shift_analisis) && suppressWarnings(as.integer(input$tahun_akhir_shift_analisis)) %in% end_choices) {
      as.character(input$tahun_akhir_shift_analisis)
    } else {
      as.character(max(end_choices, na.rm = TRUE))
    }
    update_pdrb_selectize(session, "tahun_akhir_shift_analisis", choices = valid_year_choices(end_choices), selected = selected)
  }, ignoreInit = TRUE)
  
  # LQ, DLQ, dan Extended Shift Share untuk menu Potensi Wilayah.
  # Dashboard ini membandingkan kabupaten/kota terhadap provinsi/agregat.
  # Kondisi "provinsi masih dipilih" dibedakan dari kondisi
  # "data provinsi pembanding belum diunggah".
  potential_region_state_v8 <- function(dasar_harga = "ADHK") {
    data <- pdrb_data()
    dasar_harga <- as.character(dasar_harga)[1]
    if (is.na(dasar_harga) || !dasar_harga %in% c("ADHB", "ADHK")) dasar_harga <- "ADHK"
    indikator_pilih <- paste("PDRB", dasar_harga)

    kode_kelompok <- as.character(input$kelompok)[1]
    kode_wilayah <- as.character(input$wilayah)[1]

    if (is.na(kode_kelompok) || !nzchar(kode_kelompok)) {
      return(list(ready = FALSE, code = "select_province", message = "Pilih provinsi pada Filter Global terlebih dahulu."))
    }
    if (is.na(kode_wilayah) || !nzchar(kode_wilayah)) {
      return(list(ready = FALSE, code = "select_region", message = "Pilih kabupaten/kota pada Filter Global sebagai wilayah analisis."))
    }

    data_filter <- data %>%
      mutate(
        kode_kelompok = as.character(kode_kelompok),
        kode_wilayah = as.character(kode_wilayah)
      ) %>%
      filter(indikator == indikator_pilih)

    provinsi_tersedia <- any(data_filter$kode_wilayah == kode_kelompok, na.rm = TRUE)
    kabkota_tersedia <- any(
      data_filter$kode_kelompok == kode_kelompok &
        data_filter$kode_wilayah != kode_kelompok,
      na.rm = TRUE
    )
    wilayah_tersedia <- any(data_filter$kode_wilayah == kode_wilayah, na.rm = TRUE)
    memilih_provinsi <- identical(kode_wilayah, kode_kelompok)

    if (!wilayah_tersedia) {
      return(list(ready = FALSE, code = "region_unavailable", message = "Wilayah yang dipilih belum tersedia pada dasar harga ini. Pilih wilayah lain atau periksa data yang diunggah."))
    }

    if (memilih_provinsi) {
      if (kabkota_tersedia) {
        return(list(
          ready = FALSE,
          code = "select_subregion",
          message = "Data provinsi/agregat sudah tersedia sebagai pembanding. Pilih kabupaten/kota pada Filter Global sebagai wilayah analisis."
        ))
      }
      return(list(
        ready = FALSE,
        code = "upload_subregion",
        message = "Data provinsi/agregat sudah tersedia, tetapi data kabupaten/kota belum tersedia. Unggah data kabupaten/kota untuk menjalankan LQ, DLQ, dan Extended Shift Share."
      ))
    }

    if (!provinsi_tersedia) {
      return(list(
        ready = FALSE,
        code = "upload_province",
        message = "Wilayah kabupaten/kota sudah dipilih, tetapi data provinsi/agregat pembanding belum tersedia. Unggah data provinsi/agregat."
      ))
    }

    list(ready = TRUE, code = "ready", message = "", reference_code = kode_kelompok)
  }

  potential_region_message_v8 <- function(dasar_harga = "ADHK") {
    potential_region_state_v8(dasar_harga)$message
  }

  reference_available_by_dasar_v5 <- function(dasar_harga) {
    isTRUE(potential_region_state_v8(dasar_harga)$ready)
  }
  
  reference_available_v5 <- reactive({
    # DLQ menggunakan ADHK agar perhitungan berbasis pertumbuhan riil.
    reference_available_by_dasar_v5("ADHK")
  })
  
  reference_available_lq_trend_v5 <- reactive({
    dasar <- if (is.null(input$dasar_harga_lq_tren)) "ADHK" else input$dasar_harga_lq_tren
    reference_available_by_dasar_v5(dasar)
  })
  
  output$reference_region_missing <- reactive({
    req(input$kelompok, input$wilayah)
    dasar_aktif <- if (identical(input$jenis_analisis_wilayah, "lq")) {
      if (is.null(input$dasar_harga_lq_tren)) "ADHK" else input$dasar_harga_lq_tren
    } else {
      "ADHK"
    }
    !isTRUE(reference_available_by_dasar_v5(dasar_aktif))
  })
  outputOptions(output, "reference_region_missing", suspendWhenHidden = FALSE)
  
  output$reference_region_status <- renderUI({
    req(input$kelompok, input$wilayah)
    dasar_aktif <- if (identical(input$jenis_analisis_wilayah, "lq")) {
      if (is.null(input$dasar_harga_lq_tren)) "ADHK" else input$dasar_harga_lq_tren
    } else {
      "ADHK"
    }
    state <- potential_region_state_v8(dasar_aktif)
    if (isTRUE(state$ready)) return(NULL)

    is_guidance <- state$code %in% c("select_subregion", "select_region", "select_province")
    tags$div(
      class = if (is_guidance) "status-warning" else "status-error",
      icon(if (is_guidance) "info-circle" else "warning"),
      paste0(" ", state$message)
    )
  })

  # Hindari pesan ganda; seluruh kondisi wilayah sudah dijelaskan pada card status.
  output$analysis_same_region_warning <- renderUI(NULL)
  
  calculate_dlq_v5 <- function(data, kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga, tahun_awal, tahun_akhir, periode_pilih = "Total") {
    if (!any(data$kode_wilayah == kode_kelompok_pilih)) return(tibble())
    indikator_pilih <- paste("PDRB", dasar_harga)
    level_pilih <- normalize_lq_level(level_pilih)
    level_dipakai <- if (identical(level_pilih, "Semua")) c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian") else level_pilih
    
    selang_tahun <- suppressWarnings(as.integer(tahun_akhir) - as.integer(tahun_awal))
    if (is.na(selang_tahun) || selang_tahun <= 0L) return(tibble())

    # Ikuti urutan operasi Excel manual secara eksplisit:
    # CAGR = (akhir / awal)^(1 / selang tahun) - 1.
    safe_cagr <- function(akhir, awal, selang) {
      rasio <- dplyr::if_else(
        !is.na(awal) & awal != 0 & !is.na(akhir),
        akhir / awal,
        NA_real_
      )
      dplyr::if_else(
        !is.na(rasio) & is.finite(rasio) & rasio > 0,
        rasio^(1 / selang) - 1,
        NA_real_
      )
    }
    
    raw <- data %>%
      mutate(level = as.character(level), periode = as.character(periode)) %>%
      filter(indikator == indikator_pilih, kode_kelompok == kode_kelompok_pilih) %>%
      canonicalize_pdrb_rows()
    
    sector_wil <- raw %>%
      filter(kode_wilayah == kode_wilayah_pilih, level %in% level_dipakai, periode == periode_pilih) %>%
      select(kode_kelompok, kelompok, kode_wilayah, wilayah, level, kode_kategori, kategori_label, item_id, tahun, nilai_wilayah = nilai, source_file, source_row) %>%
      collapse_for_join(c("kode_kelompok", "kode_wilayah", "level", "kode_kategori", "tahun"))
    
    sector_ref <- raw %>%
      filter(kode_wilayah == kode_kelompok_pilih, level %in% level_dipakai, periode == periode_pilih) %>%
      select(level, kode_kategori, tahun, nilai_acuan = nilai, source_file, source_row) %>%
      collapse_for_join(c("level", "kode_kategori", "tahun"))
    
    total_wil <- raw %>%
      filter(kode_wilayah == kode_wilayah_pilih, level == "Total PDRB", kode_kategori == "PDRB", periode == periode_pilih) %>%
      select(tahun, total_wilayah = nilai, source_file, source_row) %>%
      collapse_for_join(c("tahun"))
    
    total_ref <- raw %>%
      filter(kode_wilayah == kode_kelompok_pilih, level == "Total PDRB", kode_kategori == "PDRB", periode == periode_pilih) %>%
      select(tahun, total_acuan = nilai, source_file, source_row) %>%
      collapse_for_join(c("tahun"))
    
    awal <- sector_wil %>%
      filter(tahun == tahun_awal) %>%
      rename(nilai_wilayah_awal = nilai_wilayah) %>%
      left_join(sector_ref %>% filter(tahun == tahun_awal) %>% select(level, kode_kategori, nilai_acuan_awal = nilai_acuan), by = c("level", "kode_kategori")) %>%
      mutate(
        total_wilayah_awal = total_wil %>% filter(tahun == tahun_awal) %>% pull(total_wilayah) %>% dplyr::first(default = NA_real_),
        total_acuan_awal = total_ref %>% filter(tahun == tahun_awal) %>% pull(total_acuan) %>% dplyr::first(default = NA_real_)
      ) %>%
      collapse_for_join(c("level", "kode_kategori"))
    
    akhir <- sector_wil %>%
      filter(tahun == tahun_akhir) %>%
      select(level, kode_kategori, nilai_wilayah_akhir = nilai_wilayah) %>%
      left_join(sector_ref %>% filter(tahun == tahun_akhir) %>% select(level, kode_kategori, nilai_acuan_akhir = nilai_acuan), by = c("level", "kode_kategori")) %>%
      mutate(
        total_wilayah_akhir = total_wil %>% filter(tahun == tahun_akhir) %>% pull(total_wilayah) %>% dplyr::first(default = NA_real_),
        total_acuan_akhir = total_ref %>% filter(tahun == tahun_akhir) %>% pull(total_acuan) %>% dplyr::first(default = NA_real_)
      ) %>%
      collapse_for_join(c("level", "kode_kategori"))
    
    if (nrow(awal) == 0 || nrow(akhir) == 0) return(tibble())
    
    lq_all <- shared_lq_data_v90(data, kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga) %>%
      mutate(periode = as.character(periode)) %>%
      filter(periode == periode_pilih)
    
    lq_awal <- lq_all %>%
      filter(tahun == tahun_awal) %>%
      select(level, kode_kategori, LQ_awal = LQ) %>%
      collapse_for_join(c("level", "kode_kategori"))
    lq_akhir <- lq_all %>%
      filter(tahun == tahun_akhir) %>%
      select(level, kode_kategori, LQ_akhir = LQ) %>%
      collapse_for_join(c("level", "kode_kategori"))
    
    wilayah_pembanding_dlq <- raw %>%
      filter(kode_wilayah == kode_kelompok_pilih) %>%
      distinct(wilayah) %>%
      pull(wilayah)
    wilayah_pembanding_dlq <- if (length(wilayah_pembanding_dlq) > 0) wilayah_pembanding_dlq[[1]] else kode_kelompok_pilih
    
    awal %>%
      inner_join(akhir, by = c("level", "kode_kategori")) %>%
      left_join(lq_awal, by = c("level", "kode_kategori")) %>%
      left_join(lq_akhir, by = c("level", "kode_kategori")) %>%
      mutate(
        wilayah_pembanding = wilayah_pembanding_dlq,
        selang_tahun = as.integer(selang_tahun),
        `Pertumbuhan Sektor Wilayah` = safe_cagr(
          nilai_wilayah_akhir, nilai_wilayah_awal, selang_tahun
        ),
        `Pertumbuhan Total Wilayah` = safe_cagr(
          total_wilayah_akhir, total_wilayah_awal, selang_tahun
        ),
        `Pertumbuhan Sektor Acuan` = safe_cagr(
          nilai_acuan_akhir, nilai_acuan_awal, selang_tahun
        ),
        `Pertumbuhan Total Acuan` = safe_cagr(
          total_acuan_akhir, total_acuan_awal, selang_tahun
        ),
        DLQ = if_else(
          !is.na(`Pertumbuhan Sektor Wilayah`) & !is.na(`Pertumbuhan Total Wilayah`) &
            !is.na(`Pertumbuhan Sektor Acuan`) & !is.na(`Pertumbuhan Total Acuan`) &
            (1 + `Pertumbuhan Total Wilayah`) != 0 &
            (1 + `Pertumbuhan Sektor Acuan`) != 0 &
            (1 + `Pertumbuhan Total Acuan`) != 0,
          (
            ((1 + `Pertumbuhan Sektor Wilayah`) /
               (1 + `Pertumbuhan Total Wilayah`)) /
              ((1 + `Pertumbuhan Sektor Acuan`) /
                 (1 + `Pertumbuhan Total Acuan`))
          )^selang_tahun,
          NA_real_
        ),
        `Status LQ` = case_when(is.na(LQ_akhir) ~ "Tidak dihitung", LQ_akhir > 1 ~ "Basis", LQ_akhir == 1 ~ "Sama", LQ_akhir < 1 ~ "Nonbasis"),
        `Status DLQ` = case_when(is.na(DLQ) ~ "Tidak dihitung", DLQ > 1 ~ "Prospektif", DLQ == 1 ~ "Tetap", DLQ < 1 ~ "Kurang Prospektif"),
        `Klasifikasi Sektor` = case_when(LQ_akhir > 1 & DLQ > 1 ~ "Unggulan", LQ_akhir > 1 & DLQ < 1 ~ "Prospektif", LQ_akhir < 1 & DLQ > 1 ~ "Andalan", LQ_akhir < 1 & DLQ < 1 ~ "Kurang Prospektif", TRUE ~ "Tidak Terklasifikasi")
      ) %>%
      collapse_for_join(c("level", "kode_kategori")) %>%
      arrange(level, kode_kategori)
  }
  # Extended Shift Share: helper klasifikasi.
  # Tipe T1-T8 dibuat dari tanda CE, RIE, dan RSE.
  # Nilai nol diperlakukan sebagai non-negatif agar baris tetap terklasifikasi.
  shift_share_type_v5 <- function(CE, RIE, RSE) {
    dplyr::case_when(
      is.na(CE) | is.na(RIE) | is.na(RSE) ~ NA_character_,
      CE >= 0 & RIE >= 0 & RSE >= 0 ~ "T1",
      CE >= 0 & RIE >= 0 & RSE <  0 ~ "T2",
      CE >= 0 & RIE <  0 & RSE >= 0 ~ "T3",
      CE >= 0 & RIE <  0 & RSE <  0 ~ "T4",
      CE <  0 & RIE >= 0 & RSE >= 0 ~ "T5",
      CE <  0 & RIE >= 0 & RSE <  0 ~ "T6",
      CE <  0 & RIE <  0 & RSE >= 0 ~ "T7",
      CE <  0 & RIE <  0 & RSE <  0 ~ "T8",
      TRUE ~ NA_character_
    )
  }
  
  shift_share_diagnosis_v5 <- function(tipe) {
    dplyr::case_when(
      tipe == "T1" ~ "Sektor unggul terhadap pembanding, kuat di dalam wilayah sendiri, dan wilayahnya lebih dinamis dari pembanding.",
      tipe == "T2" ~ "Sektor unggul dan kuat secara internal, tetapi ekonomi wilayah kurang dinamis dari pembanding.",
      tipe == "T3" ~ "Sektor unggul terhadap pembanding dan wilayahnya dinamis, tetapi sektor belum kuat secara internal.",
      tipe == "T4" ~ "Sektor unggul terhadap pembanding, tetapi lemah secara internal dan wilayah kurang dinamis.",
      tipe == "T5" ~ "Sektor kuat secara internal dan wilayah dinamis, tetapi belum unggul terhadap sektor pembanding.",
      tipe == "T6" ~ "Sektor kuat secara internal, tetapi belum kompetitif terhadap pembanding dan wilayah kurang dinamis.",
      tipe == "T7" ~ "Sektor lemah terhadap pembanding dan lemah secara internal, tetapi wilayah masih dinamis.",
      tipe == "T8" ~ "Sektor lemah terhadap pembanding, lemah secara internal, dan wilayah kurang dinamis.",
      TRUE ~ "Tidak dapat didiagnosis karena komponen CE, RIE, atau RSE tidak lengkap."
    )
  }
  
  shift_share_diagnosis_en_v5 <- function(tipe) {
    dplyr::case_when(
      tipe == "T1" ~ "The region shows competitive advantages in the sector at both the national and regional levels, in addition to a regional economy that performs better than the national economy.",
      tipe == "T2" ~ "The region shows competitive advantages in the sector at both the national and regional levels but has a less dynamic regional economy than the national one.",
      tipe == "T3" ~ "The region shows competitive advantages in the sector at the national level and is more dynamic than the national economy, but presents competitive disadvantages at the regional level.",
      tipe == "T4" ~ "The region shows competitive advantages in the sector only at the national level, while the regional level shows disadvantages within a regional economy that grows at a slower rate than the national economy.",
      tipe == "T5" ~ "The region shows competitive advantages in the sector at the regional level in a strong regional economy compared to the national one, but this is not enough to achieve a sectoral competitive advantage at the national level.",
      tipe == "T6" ~ "The region shows competitive advantages in the sector within a regional economy that grows at a slower rate than the national economy and presents comparative disadvantages at the national level.",
      tipe == "T7" ~ "The region shows competitive disadvantages in the sector at the national and regional levels, despite the regional economy being stronger than the national one.",
      tipe == "T8" ~ "The region shows competitive disadvantages in the sector at the national and regional levels. Also, the regional economy is less dynamic compared to the national one.",
      TRUE ~ "Diagnosis cannot be determined because CE, RIE, or RSE components are incomplete."
    )
  }
  
  shift_share_translation_id_v5 <- function(tipe) {
    dplyr::case_when(
      tipe == "T1" ~ "Wilayah menunjukkan keunggulan kompetitif pada sektor tersebut, baik pada tingkat nasional maupun regional, serta berada dalam perekonomian regional yang berkinerja lebih baik daripada perekonomian nasional.",
      tipe == "T2" ~ "Wilayah menunjukkan keunggulan kompetitif pada sektor tersebut, baik pada tingkat nasional maupun regional, tetapi memiliki perekonomian regional yang kurang dinamis dibandingkan perekonomian nasional.",
      tipe == "T3" ~ "Wilayah menunjukkan keunggulan kompetitif pada sektor tersebut pada tingkat nasional dan berada dalam perekonomian regional yang lebih dinamis daripada perekonomian nasional. Namun, sektor tersebut menunjukkan kelemahan kompetitif pada tingkat regional.",
      tipe == "T4" ~ "Wilayah menunjukkan keunggulan kompetitif pada sektor tersebut hanya pada tingkat nasional, sedangkan pada tingkat regional sektor tersebut menunjukkan kelemahan dalam perekonomian regional yang tumbuh lebih lambat daripada perekonomian nasional.",
      tipe == "T5" ~ "Wilayah menunjukkan keunggulan kompetitif pada sektor tersebut pada tingkat regional dalam perekonomian regional yang kuat dibandingkan perekonomian nasional. Namun, kondisi tersebut belum cukup untuk mencapai keunggulan kompetitif sektoral pada tingkat nasional.",
      tipe == "T6" ~ "Wilayah menunjukkan keunggulan kompetitif pada sektor tersebut dalam perekonomian regional yang tumbuh lebih lambat daripada perekonomian nasional. Selain itu, sektor regional tersebut menunjukkan kelemahan kompetitif pada tingkat nasional.",
      tipe == "T7" ~ "Wilayah menunjukkan kelemahan kompetitif pada sektor tersebut, baik pada tingkat nasional maupun regional. Meskipun demikian, perekonomian regional lebih kuat daripada perekonomian nasional.",
      tipe == "T8" ~ "Wilayah menunjukkan kelemahan kompetitif pada sektor tersebut, baik pada tingkat nasional maupun regional. Selain itu, perekonomian regional kurang dinamis dibandingkan perekonomian nasional.",
      TRUE ~ "Diagnosis tidak dapat diterjemahkan karena komponen CE, RIE, atau RSE tidak lengkap."
    )
  }
  
  shift_share_note_v5 <- function(tipe) {
    dplyr::case_when(
      tipe == "T1" ~ "Sektor menunjukkan posisi kuat secara kompetitif dan berada pada wilayah yang relatif dinamis.",
      tipe == "T2" ~ "Sektor memiliki keunggulan kompetitif, tetapi dinamika wilayah masih perlu diperhatikan.",
      tipe == "T3" ~ "Sektor unggul terhadap pembanding dan wilayah relatif dinamis, tetapi kekuatan internal sektor masih perlu dicermati.",
      tipe == "T4" ~ "Sektor memiliki keunggulan terhadap pembanding, tetapi lemah secara internal dan berada pada wilayah yang kurang dinamis.",
      tipe == "T5" ~ "Sektor kuat secara internal dan wilayah relatif dinamis, tetapi belum unggul dibanding sektor pembanding.",
      tipe == "T6" ~ "Sektor kuat secara internal, tetapi belum kompetitif terhadap pembanding dan dinamika wilayah kurang kuat.",
      tipe == "T7" ~ "Sektor lemah terhadap pembanding dan lemah secara internal, meskipun wilayah masih relatif dinamis.",
      tipe == "T8" ~ "Sektor menunjukkan kelemahan kompetitif dan berada pada wilayah yang kurang dinamis.",
      TRUE ~ "Catatan tidak tersedia karena tipe belum dapat ditentukan."
    )
  }
  
  # Menentukan wilayah pembanding:
  # - Kabupaten/kota memakai provinsi/agregat pada kode_kelompok.
  # - Provinsi/agregat memakai nasional jika data nasional tersedia.
  resolve_reference_code_v5 <- function(data, kode_kelompok_pilih, kode_wilayah_pilih, indikator_pilih = NULL) {
    kode_kelompok_pilih <- as.character(kode_kelompok_pilih)[1]
    kode_wilayah_pilih <- as.character(kode_wilayah_pilih)[1]
    
    if (!identical(kode_wilayah_pilih, kode_kelompok_pilih)) {
      return(kode_kelompok_pilih)
    }
    
    kandidat <- data %>%
      mutate(
        kode_wilayah = as.character(kode_wilayah),
        wilayah_upper = stringr::str_to_upper(dplyr::coalesce(wilayah, "")),
        jenis_upper = stringr::str_to_upper(dplyr::coalesce(jenis_wilayah, ""))
      ) %>%
      filter(
        kode_wilayah %in% c("0000", "00", "0", "1000", "9999") |
          stringr::str_detect(wilayah_upper, "INDONESIA|NASIONAL") |
          stringr::str_detect(jenis_upper, "NASIONAL")
      )
    
    if (!is.null(indikator_pilih) && !is.na(indikator_pilih)) {
      kandidat <- kandidat %>% filter(indikator == indikator_pilih)
    }
    
    kode <- kandidat %>%
      filter(kode_wilayah != kode_wilayah_pilih) %>%
      distinct(kode_wilayah) %>%
      pull(kode_wilayah)
    
    if (length(kode) > 0) kode[[1]] else NA_character_
  }
  
  
  empty_shift_share_result_v5 <- function() {
    # Struktur kolom kosong agar render tabel/grafik tidak error ketika data acuan,
    # tahun, periode, atau kategori belum tersedia.
    tibble::tibble(
      kode_kelompok = character(), kelompok = character(),
      kode_wilayah = character(), wilayah = character(),
      kode_wilayah_acuan = character(), wilayah_acuan = character(),
      level = character(), kode_kategori = character(), kategori_label = character(), item_id = character(),
      tahun = integer(), tahun_awal = integer(), periode_awal = character(), tahun_akhir = integer(), periode_akhir = character(),
      v_ij0 = numeric(), v_ij1 = numeric(), v_i0 = numeric(), v_i1 = numeric(),
      G = numeric(), Gi = numeric(), gj = numeric(), gij = numeric(),
      NE = numeric(), IM = numeric(), CE = numeric(), RIE = numeric(), RSE = numeric(), RCCE = numeric(),
      `Total Perubahan` = numeric(), `Total Klasik` = numeric(), `Total Extended` = numeric(),
      Tipe = character(), Diagnosa = character(), Interpretasi = character()
    )
  }
  
  # Mengubah data triwulanan menjadi data tahunan untuk Extended Shift Share.
  # Jika baris Tahunan/Total tersedia, baris itu hanya dipakai ketika tahun
  # lengkap. Jika tidak ada, nilai tahunan dibentuk hanya dari Triwulan I-IV
  # yang lengkap; tahun parsial dikeluarkan dari Extended Shift Share tahunan.
  shift_share_annual_data_v5 <- function(data, indikator_pilih) {
    quarters <- c("I", "II", "III", "IV")

    data %>%
      mutate(
        tahun = as.integer(tahun),
        periode = as.character(periode),
        kode_wilayah = as.character(kode_wilayah),
        kode_kelompok = as.character(kode_kelompok),
        nilai = suppressWarnings(as.numeric(nilai))
      ) %>%
      filter(
        indikator == indikator_pilih,
        periode %in% c(quarters, "Total"),
        !is.na(nilai),
        is.finite(nilai)
      ) %>%
      canonicalize_pdrb_rows() %>%
      group_by(
        kode_kelompok, kelompok, kode_wilayah, wilayah, jenis_wilayah,
        indikator, satuan, level, kode_kategori, kategori_label, item_id, tahun
      ) %>%
      summarise(
        .has_total = any(periode == "Total"),
        .has_any_quarter = any(periode %in% quarters),
        .quarter_count = dplyr::n_distinct(periode[periode %in% quarters]),
        nilai = dplyr::case_when(
          .has_total & (!.has_any_quarter | .quarter_count == 4L) ~
            dplyr::first(nilai[periode == "Total"]),
          !.has_total & .quarter_count == 4L ~
            sum(nilai[periode %in% quarters], na.rm = TRUE),
          TRUE ~ NA_real_
        ),
        .groups = "drop"
      ) %>%
      filter(!is.na(nilai), is.finite(nilai)) %>%
      select(-.has_total, -.has_any_quarter, -.quarter_count) %>%
      mutate(periode = "Total")
  }
  
  # Extended Shift Share tahunan.
  # Tahun awal dan tahun akhir dipilih pengguna; tahun akhir wajib lebih besar.
  # Komponen klasik: NE, IM, CE.
  # Komponen extended: NE, IM, CE, RIE, RSE, RCCE.
  # Diagnosis utama memakai CE, RIE, dan RSE.
  calculate_shift_share_v5 <- function(data, kode_kelompok_pilih, kode_wilayah_pilih,
                                       level_pilih = "Kategori Utama", dasar_harga = "ADHK",
                                       tahun_akhir, tahun_awal = NULL, mode = "extended") {
    mode <- shift_mode_value(mode)
    dasar_harga <- as.character(dasar_harga)[1]
    if (is.na(dasar_harga) || !dasar_harga %in% c("ADHB", "ADHK")) dasar_harga <- "ADHK"
    
    tahun_akhir <- suppressWarnings(as.integer(tahun_akhir)[1])
    if (is.null(tahun_awal)) tahun_awal <- tahun_akhir - 1L
    tahun_awal <- suppressWarnings(as.integer(tahun_awal)[1])
    periode_awal <- "Total"
    periode_akhir <- "Total"
    if (is.na(tahun_akhir) || is.na(tahun_awal) || tahun_akhir <= tahun_awal) return(empty_shift_share_result_v5())
    
    kode_kelompok_pilih <- as.character(kode_kelompok_pilih)[1]
    kode_wilayah_pilih <- as.character(kode_wilayah_pilih)[1]
    if (is.na(kode_kelompok_pilih) || is.na(kode_wilayah_pilih)) return(empty_shift_share_result_v5())
    
    indikator_pilih <- paste("PDRB", dasar_harga)
    raw <- shift_share_annual_data_v5(data, indikator_pilih)
    if (nrow(raw) == 0) return(empty_shift_share_result_v5())
    
    kode_acuan <- tryCatch(
      resolve_reference_code_v5(raw, kode_kelompok_pilih, kode_wilayah_pilih, indikator_pilih),
      error = function(e) NA_character_
    )
    if (is.na(kode_acuan) || !any(raw$kode_wilayah == kode_acuan)) return(empty_shift_share_result_v5())
    
    wilayah_acuan <- raw %>%
      filter(kode_wilayah == kode_acuan) %>%
      distinct(wilayah) %>%
      pull(wilayah)
    wilayah_acuan <- if (length(wilayah_acuan) > 0) wilayah_acuan[[1]] else kode_acuan
    
    level_pilih <- normalize_lq_level(level_pilih)
    level_dipakai <- if (identical(level_pilih, "Semua")) c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian") else level_pilih
    
    sector_wil <- raw %>%
      filter(kode_wilayah == kode_wilayah_pilih, level %in% level_dipakai) %>%
      select(kode_kelompok, kelompok, kode_wilayah, wilayah, level, kode_kategori, kategori_label, item_id, tahun, nilai_wilayah = nilai) %>%
      collapse_for_join(c("kode_kelompok", "kode_wilayah", "level", "kode_kategori", "tahun"))
    
    sector_ref <- raw %>%
      filter(kode_wilayah == kode_acuan, level %in% level_dipakai) %>%
      select(level, kode_kategori, tahun, nilai_acuan = nilai) %>%
      collapse_for_join(c("level", "kode_kategori", "tahun"))
    
    total_wil <- raw %>%
      filter(kode_wilayah == kode_wilayah_pilih, level == "Total PDRB", kode_kategori == "PDRB") %>%
      select(tahun, total_wilayah = nilai) %>%
      collapse_for_join(c("tahun"))
    
    total_ref <- raw %>%
      filter(kode_wilayah == kode_acuan, level == "Total PDRB", kode_kategori == "PDRB") %>%
      select(tahun, total_acuan = nilai) %>%
      collapse_for_join(c("tahun"))
    
    awal <- sector_wil %>%
      filter(tahun == tahun_awal) %>%
      rename(v_ij0 = nilai_wilayah) %>%
      left_join(
        sector_ref %>% filter(tahun == tahun_awal) %>% select(level, kode_kategori, v_i0 = nilai_acuan),
        by = c("level", "kode_kategori")
      ) %>%
      collapse_for_join(c("level", "kode_kategori"))
    
    akhir <- sector_wil %>%
      filter(tahun == tahun_akhir) %>%
      select(level, kode_kategori, v_ij1 = nilai_wilayah) %>%
      left_join(
        sector_ref %>% filter(tahun == tahun_akhir) %>% select(level, kode_kategori, v_i1 = nilai_acuan),
        by = c("level", "kode_kategori")
      ) %>%
      collapse_for_join(c("level", "kode_kategori"))
    
    if (nrow(awal) == 0 || nrow(akhir) == 0) return(empty_shift_share_result_v5())
    
    V0 <- total_ref %>% filter(tahun == tahun_awal) %>% pull(total_acuan) %>% dplyr::first(default = NA_real_)
    V1 <- total_ref %>% filter(tahun == tahun_akhir) %>% pull(total_acuan) %>% dplyr::first(default = NA_real_)
    W0 <- total_wil %>% filter(tahun == tahun_awal) %>% pull(total_wilayah) %>% dplyr::first(default = NA_real_)
    W1 <- total_wil %>% filter(tahun == tahun_akhir) %>% pull(total_wilayah) %>% dplyr::first(default = NA_real_)
    if (is.na(V0) || is.na(V1) || is.na(W0) || is.na(W1) || V0 == 0 || W0 == 0) return(empty_shift_share_result_v5())
    
    G <- (V1 - V0) / V0       # pertumbuhan total ekonomi acuan
    gj <- (W1 - W0) / W0      # pertumbuhan total ekonomi wilayah analisis
    
    awal %>%
      inner_join(akhir, by = c("level", "kode_kategori")) %>%
      mutate(
        kode_wilayah_acuan = kode_acuan,
        wilayah_acuan = wilayah_acuan,
        tahun = as.integer(tahun_akhir),
        tahun_awal = as.integer(tahun_awal),
        tahun_akhir = as.integer(tahun_akhir),
        periode_awal = periode_awal,
        periode_akhir = periode_akhir,
        G = G,
        gj = gj,
        Gi = if_else(!is.na(v_i0) & v_i0 != 0, (v_i1 - v_i0) / v_i0, NA_real_),
        gij = if_else(!is.na(v_ij0) & v_ij0 != 0, (v_ij1 - v_ij0) / v_ij0, NA_real_),
        
        # Extended Shift Share klasik: ΔXij = NE + IM + CE
        NE = v_ij0 * G,
        IM = v_ij0 * (Gi - G),
        CE = v_ij0 * (gij - Gi),
        
        # Extended Shift Share extended/comprehensive: tambahan efek intrinsik regional.
        RIE = v_ij0 * (gij - gj),
        RSE = v_ij0 * (gj - G),
        RCCE = v_ij0 * (G - gij),
        
        `Total Perubahan` = v_ij1 - v_ij0,
        `Total Klasik` = NE + IM + CE,
        `Total Extended` = NE + IM + CE + RIE + RSE + RCCE,
        Tipe = shift_share_type_v5(CE, RIE, RSE),
        Diagnosa = shift_share_diagnosis_v5(Tipe),
        Interpretasi = Diagnosa
      ) %>%
      select(
        kode_kelompok, kelompok, kode_wilayah, wilayah,
        kode_wilayah_acuan, wilayah_acuan,
        level, kode_kategori, kategori_label, item_id,
        tahun, tahun_awal, periode_awal, tahun_akhir, periode_akhir,
        v_ij0, v_ij1, v_i0, v_i1,
        G, Gi, gj, gij,
        NE, IM, CE, RIE, RSE, RCCE,
        `Total Perubahan`, `Total Klasik`, `Total Extended`,
        Tipe, Diagnosa, Interpretasi
      ) %>%
      collapse_for_join(c("level", "kode_kategori")) %>%
      arrange(level, kode_kategori)
  }
  
  # Shared cached analytical results -----------------------------------------
  # LQ, DLQ, dan Extended Shift Share dihitung maksimal satu kali untuk setiap kombinasi
  # data/parameter. Grafik, tabel Potensi Wilayah, Tabel Data, dan laporan
  # menerima objek hasil yang sama.
  shared_lq_data_v90 <- function(data, kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga) {
    level_pilih <- normalize_lq_level(level_pilih)
    key <- cache_key_v90(
      "LQ", kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga
    )
    cache_get_or_compute_v90(lq_result_cache, key, function() {
      calculate_lq_table(
        data = data,
        kode_kelompok_pilih = kode_kelompok_pilih,
        kode_wilayah_pilih = kode_wilayah_pilih,
        level_pilih = level_pilih,
        dasar_harga = dasar_harga
      )
    })
  }

  shared_dlq_data_v90 <- function(data, kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga, tahun_awal, tahun_akhir, periode_pilih = "Total") {
    level_pilih <- normalize_lq_level(level_pilih)
    key <- cache_key_v90(
      "DLQ", kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga,
      as.integer(tahun_awal), as.integer(tahun_akhir), periode_pilih
    )
    cache_get_or_compute_v90(dlq_result_cache, key, function() {
      calculate_dlq_v5(
        data = data,
        kode_kelompok_pilih = kode_kelompok_pilih,
        kode_wilayah_pilih = kode_wilayah_pilih,
        level_pilih = level_pilih,
        dasar_harga = dasar_harga,
        tahun_awal = tahun_awal,
        tahun_akhir = tahun_akhir,
        periode_pilih = periode_pilih
      )
    })
  }

  shared_shift_data_v90 <- function(data, kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga, tahun_akhir, tahun_awal = NULL, mode = "extended") {
    level_pilih <- normalize_lq_level(level_pilih)
    key <- cache_key_v90(
      "SHIFT", kode_kelompok_pilih, kode_wilayah_pilih, level_pilih, dasar_harga,
      as.integer(tahun_awal), as.integer(tahun_akhir), shift_mode_value(mode)
    )
    cache_get_or_compute_v90(shift_result_cache, key, function() {
      calculate_shift_share_v5(
        data = data,
        kode_kelompok_pilih = kode_kelompok_pilih,
        kode_wilayah_pilih = kode_wilayah_pilih,
        level_pilih = level_pilih,
        dasar_harga = dasar_harga,
        tahun_akhir = tahun_akhir,
        tahun_awal = tahun_awal,
        mode = mode
      )
    })
  }

  analysis_lq_data_v5 <- reactive({
    req(input$kelompok, input$wilayah, input$tahun_akhir_analisis)
    validate(need(reference_available_v5(), potential_region_message_v8("ADHK")))
    shared_lq_data_v90(pdrb_data(), input$kelompok, input$wilayah, normalize_lq_level(input$potensi_level_kategori), "ADHK") %>%
      mutate(periode = as.character(periode)) %>%
      filter(tahun == as.integer(input$tahun_akhir_analisis), periode == "Total")
  })
  
  analysis_lq_rank_data_v5 <- reactive({
    req(input$kelompok, input$wilayah, input$dasar_harga_lq_tren, input$tahun_lq, input$periode_lq_peringkat)
    validate(need(reference_available_lq_trend_v5(), potential_region_message_v8(input$dasar_harga_lq_tren)))
    data <- shared_lq_data_v90(
      pdrb_data(),
      input$kelompok,
      input$wilayah,
      normalize_lq_level(input$potensi_level_kategori),
      input$dasar_harga_lq_tren
    ) %>%
      mutate(
        periode = as.character(periode),
        tahun = as.integer(tahun)
      ) %>%
      filter(!is.na(LQ))
    
    tahun_pilih <- as.character(input$tahun_lq)[1]
    periode_pilih <- as.character(input$periode_lq_peringkat)[1]
    if (!identical(tahun_pilih, "__ALL__")) {
      data <- data %>% filter(tahun == suppressWarnings(as.integer(tahun_pilih)))
    }
    if (identical(periode_pilih, "__QUARTERS__")) {
      data <- data %>% filter(periode %in% c("I", "II", "III", "IV"))
    } else if (!identical(periode_pilih, "__ALL__")) {
      data <- data %>% filter(periode == periode_pilih)
    }
    
    data %>% arrange(desc(LQ), kode_kategori, tahun, match(periode, c("I", "II", "III", "IV", "Total")))
  })
  
  analysis_lq_trend_data_v5 <- reactive({
    req(input$kelompok, input$wilayah, input$dasar_harga_lq_tren)
    validate(need(reference_available_lq_trend_v5(), potential_region_message_v8(input$dasar_harga_lq_tren)))
    
    # Level tren mengikuti pilihan Tingkat Analisis pada card Metode Analisis.
    level_tren <- normalize_lq_level(input$potensi_level_kategori)
    
    data <- shared_lq_data_v90(
      pdrb_data(),
      input$kelompok,
      input$wilayah,
      level_tren,
      input$dasar_harga_lq_tren
    ) %>%
      mutate(
        periode = as.character(periode),
        periode_order = match(periode, c("I", "II", "III", "IV", "Total")),
        tahun = as.integer(tahun)
      ) %>%
      filter(!is.na(LQ), !is.na(tahun), !is.na(periode_order))
    
    tahun_pilih <- as.character(input$tahun_lq)[1]
    periode_pilih <- as.character(input$periode_lq_peringkat)[1]
    if (!is.null(tahun_pilih) && !is.na(tahun_pilih) && !identical(tahun_pilih, "__ALL__")) {
      data <- data %>% filter(tahun == suppressWarnings(as.integer(tahun_pilih)))
    }
    if (identical(periode_pilih, "__QUARTERS__")) {
      data <- data %>% filter(periode %in% c("I", "II", "III", "IV"))
    } else if (!is.null(periode_pilih) && !is.na(periode_pilih) && !identical(periode_pilih, "__ALL__")) {
      data <- data %>% filter(periode == periode_pilih)
    }
    
    data %>%
      arrange(level, kode_kategori, tahun, periode_order) %>%
      mutate(
        waktu_urut = tahun * 10 + periode_order,
        waktu_label = if_else(periode == "Total", as.character(tahun), paste0(tahun, " TW ", periode)),
        label_garis = stringr::str_trunc(kategori_label, 62),
        hover_lq = paste0(
          kategori_label,
          "<br>Tahun/Periode: ", waktu_label,
          "<br>Dasar harga: ", input$dasar_harga_lq_tren,
          "<br>LQ: ", round(LQ, 4),
          "<br>Status: ", Keterangan
        )
      )
  })
  
  # V8.3 ---------------------------------------------------------------------
  # Pembaruan Jenis Sektor memakai binding resmi Shiny. Daftar pilihan hanya
  # dikirim ketika signature berubah, sehingga stabil tanpa label `undefined`.
  sanitize_sector_choices_v83 <- function(choices) {
    values <- unname(as.character(choices))
    labels <- names(choices)
    if (is.null(labels)) labels <- values
    labels <- as.character(labels)

    valid <- !is.na(values) & nzchar(values) & !is.na(labels) & nzchar(labels)
    values <- values[valid]
    labels <- labels[valid]

    if (length(values) == 0L) return(stats::setNames(character(0), character(0)))

    keep <- !duplicated(values)
    stats::setNames(values[keep], labels[keep])
  }

  send_stable_selectize_v82 <- function(id, choices, selected = NULL) {
    choices <- sanitize_sector_choices_v83(choices)
    values <- unname(as.character(choices))

    selected_value <- if (
      is.null(selected) || length(selected) == 0L || is.na(selected[[1]]) ||
      !as.character(selected[[1]]) %in% values
    ) {
      if (length(values) > 0L) values[[1]] else character(0)
    } else {
      as.character(selected[[1]])
    }

    shiny::updateSelectizeInput(
      session = session,
      inputId = id,
      choices = choices,
      selected = selected_value,
      options = list(dropdownParent = "body"),
      server = FALSE
    )
  }

  potential_sector_choices_v82 <- function(level_value, dasar_harga = "ADHK", include_all = FALSE) {
    data <- pdrb_data()
    if (is.null(data) || nrow(data) == 0L) {
      return(if (isTRUE(include_all)) c("Semua Sektor" = "__ALL__") else character(0))
    }

    level_value <- normalize_lq_level(level_value)
    dasar_harga <- as.character(dasar_harga)[1]
    if (is.na(dasar_harga) || !dasar_harga %in% c("ADHB", "ADHK")) dasar_harga <- "ADHK"
    indikator_value <- paste("PDRB", dasar_harga)
    kode_wilayah <- as.character(input$wilayah)[1]
    kode_pembanding <- as.character(input$kelompok)[1]

    target <- data %>%
      mutate(
        level = as.character(level),
        kode_kategori = as.character(kode_kategori),
        kategori_label = stringr::str_squish(as.character(kategori_label))
      ) %>%
      filter(
        kode_wilayah == .env$kode_wilayah,
        indikator == .env$indikator_value,
        !is.na(kode_kategori), nzchar(kode_kategori),
        !is.na(kategori_label), nzchar(kategori_label)
      )

    reference <- data %>%
      mutate(
        level = as.character(level),
        kode_kategori = as.character(kode_kategori)
      ) %>%
      filter(
        kode_wilayah == .env$kode_pembanding,
        indikator == .env$indikator_value,
        !is.na(kode_kategori), nzchar(kode_kategori)
      ) %>%
      distinct(level, kode_kategori)

    if (nrow(reference) > 0L) {
      target <- target %>% semi_join(reference, by = c("level", "kode_kategori"))
    }

    if (!identical(level_value, "Semua")) {
      target <- target %>% filter(level == .env$level_value)
    } else {
      target <- target %>% filter(level %in% c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian"))
    }

    pilihan <- target %>%
      distinct(level, kode_kategori, kategori_label) %>%
      arrange(factor(level, levels = c("Kategori Utama", "Subkategori", "Rincian", "Total PDRB")), kode_kategori) %>%
      mutate(
        value = paste(level, kode_kategori, sep = "||"),
        label = if_else(
          duplicated(kategori_label) | duplicated(kategori_label, fromLast = TRUE),
          paste0(kategori_label, " · ", level),
          kategori_label
        )
      )

    choices <- stats::setNames(as.character(pilihan$value), as.character(pilihan$label))
    if (isTRUE(include_all)) c("Semua Sektor" = "__ALL__", choices) else choices
  }

  analysis_dlq_data_v5 <- reactive({
    req(input$kelompok, input$wilayah, input$tahun_awal_analisis)
    validate(need(reference_available_v5(), potential_region_message_v8("ADHK")))

    tahun_awal_input <- as.character(input$tahun_awal_analisis)[1]
    tahun_akhir_input <- as.character(input$tahun_akhir_analisis)[1]
    semua_tahun_dlq <- is.na(tahun_awal_input) || !nzchar(tahun_awal_input) ||
      identical(tahun_awal_input, "__ALL__")

    tahun_tersedia <- tryCatch(valid_dlq_years(), error = function(e) integer(0))
    tahun_tersedia <- sort(unique(as.integer(tahun_tersedia)))
    tahun_tersedia <- tahun_tersedia[!is.na(tahun_tersedia)]
    tahun_awal_valid <- tahun_tersedia[(tahun_tersedia + 1L) %in% tahun_tersedia]
    validate(need(
      length(tahun_awal_valid) > 0,
      "DLQ memerlukan minimal dua tahun berurutan yang tersedia pada wilayah dan provinsi pembanding."
    ))

    if (semua_tahun_dlq) {
      pasangan_tahun <- tibble::tibble(
        tahun_awal = tahun_awal_valid,
        tahun_akhir = tahun_awal_valid + 1L
      )
    } else {
      tahun_awal_dlq <- suppressWarnings(as.integer(tahun_awal_input))
      tahun_akhir_dlq <- suppressWarnings(as.integer(tahun_akhir_input))
      validate(need(
        !is.na(tahun_awal_dlq) && !is.na(tahun_akhir_dlq),
        "Pilih Tahun Awal dan Tahun Akhir Dynamic Location Quotient (DLQ)."
      ))
      validate(need(
        tahun_akhir_dlq == tahun_awal_dlq + 1L,
        "Dynamic Location Quotient (DLQ) harus dihitung antar-tahun berurutan."
      ))
      validate(need(
        tahun_awal_dlq %in% tahun_awal_valid,
        "Pasangan tahun DLQ yang dipilih tidak tersedia pada wilayah dan provinsi pembanding."
      ))
      pasangan_tahun <- tibble::tibble(
        tahun_awal = tahun_awal_dlq,
        tahun_akhir = tahun_akhir_dlq
      )
    }

    bagian_dlq <- lapply(seq_len(nrow(pasangan_tahun)), function(i) {
      tahun_awal_i <- pasangan_tahun$tahun_awal[[i]]
      tahun_akhir_i <- pasangan_tahun$tahun_akhir[[i]]
      hasil_i <- shared_dlq_data_v90(
        pdrb_data(), input$kelompok, input$wilayah,
        normalize_lq_level(input$potensi_level_kategori),
        "ADHK", tahun_awal_i, tahun_akhir_i, "Total"
      )
      if (nrow(hasil_i) == 0L) return(hasil_i)
      hasil_i %>% mutate(
        .dlq_tahun_awal = as.integer(tahun_awal_i),
        .dlq_tahun_akhir = as.integer(tahun_akhir_i),
        .dlq_periode = paste0(tahun_awal_i, "–", tahun_akhir_i)
      )
    })

    hasil <- dplyr::bind_rows(bagian_dlq)
    validate(need(nrow(hasil) > 0, "Data DLQ tidak tersedia untuk pasangan tahun yang dipilih."))
    hasil %>% arrange(.dlq_tahun_awal, .dlq_tahun_akhir, level, kode_kategori)
  })

  # Pilihan Jenis Sektor DLQ memakai daftar statis berdasarkan wilayah,
  # dasar harga ADHK, dan tingkat analisis. Perubahan tahun DLQ tidak lagi
  # membangun ulang dropdown sehingga tampilan tetap stabil.
  dlq_sector_choice_state_v82 <- reactiveVal(NULL)

  observeEvent(
    list(
      processed_upload_version(),
      input$jenis_analisis_wilayah,
      input$kelompok,
      input$wilayah,
      input$potensi_level_kategori
    ),
    {
      req(
        input$jenis_analisis_wilayah, input$kelompok,
        input$wilayah, input$potensi_level_kategori
      )
      if (!identical(as.character(input$jenis_analisis_wilayah)[1], "dlq")) {
        return(invisible(NULL))
      }

      choices <- potential_sector_choices_v82(
        input$potensi_level_kategori,
        "ADHK",
        include_all = TRUE
      )
      values <- unname(as.character(choices))
      current <- isolate(as.character(input$kategori_dlq)[1])
      selected <- if (!is.na(current) && current %in% values) current else "__ALL__"

      signature <- paste(names(choices), values, sep = "=", collapse = "|")
      if (identical(dlq_sector_choice_state_v82(), signature)) {
        return(invisible(NULL))
      }
      dlq_sector_choice_state_v82(signature)
      freezeReactiveValue(input, "kategori_dlq")
      send_stable_selectize_v82("kategori_dlq", choices, selected)
    },
    ignoreInit = FALSE,
    ignoreNULL = FALSE
  )

  analysis_dlq_display_data_v8 <- reactive({
    data <- analysis_dlq_data_v5()
    selected <- as.character(input$kategori_dlq)[1]
    if (is.null(selected) || is.na(selected) || !nzchar(selected) || identical(selected, "__ALL__")) {
      return(data)
    }

    bagian <- strsplit(selected, "||", fixed = TRUE)[[1]]
    if (length(bagian) != 2) return(data)
    data %>% filter(level == bagian[[1]], kode_kategori == bagian[[2]])
  })
  
  analysis_shift_data_v5 <- reactive({
    req(input$kelompok, input$wilayah, input$tahun_awal_shift_analisis, input$tahun_akhir_shift_analisis)
    tahun_awal_shift <- suppressWarnings(as.integer(input$tahun_awal_shift_analisis))
    tahun_akhir_shift <- suppressWarnings(as.integer(input$tahun_akhir_shift_analisis))
    validate(need(!is.na(tahun_awal_shift), "Pilih tahun awal Extended Shift Share terlebih dahulu."))
    validate(need(!is.na(tahun_akhir_shift), "Pilih tahun akhir Extended Shift Share terlebih dahulu."))
    validate(need(tahun_akhir_shift > tahun_awal_shift, "Tahun akhir Extended Shift Share harus lebih besar dari tahun awal."))
    validate(need(reference_available_by_dasar_v5("ADHK"), potential_region_message_v8("ADHK")))
    
    shared_shift_data_v90(
      pdrb_data(),
      input$kelompok,
      input$wilayah,
      "Kategori Utama",
      "ADHK",
      tahun_akhir_shift,
      tahun_awal = tahun_awal_shift,
      mode = "extended"
    )
  })

  # Menyiapkan data khusus tabel Extended Shift Share. Grafik dan value box tetap memakai
  # 17 kategori utama, sedangkan tabel juga menampilkan Total PDRB pada baris
  # paling bawah agar pengguna dapat melihat perubahan ekonomi wilayah secara total.
  analysis_shift_table_data_v85 <- reactive({
    kategori_data <- analysis_shift_data_v5()
    req(input$kelompok, input$wilayah, input$tahun_awal_shift_analisis, input$tahun_akhir_shift_analisis)

    tahun_awal_shift <- suppressWarnings(as.integer(input$tahun_awal_shift_analisis))
    tahun_akhir_shift <- suppressWarnings(as.integer(input$tahun_akhir_shift_analisis))

    total_data <- shared_shift_data_v90(
      pdrb_data(),
      input$kelompok,
      input$wilayah,
      "Total PDRB",
      "ADHK",
      tahun_akhir_shift,
      tahun_awal = tahun_awal_shift,
      mode = "extended"
    )

    dplyr::bind_rows(kategori_data, total_data) %>%
      mutate(
        .urutan_level = case_when(
          level == "Kategori Utama" ~ 1L,
          level == "Total PDRB" ~ 2L,
          TRUE ~ 3L
        )
      ) %>%
      distinct(level, kode_kategori, .keep_all = TRUE) %>%
      arrange(.urutan_level, kode_kategori) %>%
      select(-.urutan_level)
  })

  # DT/JSON tidak dapat merender list-column, nilai Inf/NaN, atau nama kolom
  # ganda secara konsisten. Normalisasi ini menjaga tabel DLQ dan Extended Shift Share
  # menjadi data.frame atomik biasa sebelum dikirim ke browser.
  sanitize_dt_frame_v85 <- function(data) {
    if (is.null(data)) return(data.frame())

    data <- dplyr::ungroup(data)
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    if (ncol(data) == 0L) return(data)

    data[] <- lapply(data, function(column) {
      if (is.list(column) && !is.data.frame(column)) {
        return(vapply(column, function(value) {
          if (is.null(value) || length(value) == 0L || all(is.na(value))) return(NA_character_)
          paste(as.character(value), collapse = ", ")
        }, character(1)))
      }

      if (inherits(column, "factor")) column <- as.character(column)
      if (inherits(column, c("Date", "POSIXct", "POSIXlt"))) column <- as.character(column)

      if (is.numeric(column)) {
        column[!is.finite(column)] <- NA_real_
      } else if (!is.atomic(column)) {
        column <- as.character(column)
      }

      column
    })

    names(data) <- make.unique(names(data), sep = " ")
    data
  }
  
  # Data tabel Potensi Wilayah menggunakan hasil analisis bersama yang sama
  # dengan grafik dan menu Tabel Data. Renderer hanya memilih, mengurutkan,
  # memformat, dan menampilkan hasil; tidak menghitung ulang indikator.
  lq_table_data_v91 <- reactive({
    data <- analysis_lq_rank_data_v5() %>%
      group_by(tahun, periode) %>%
      arrange(desc(LQ), kode_kategori, .by_group = TRUE) %>%
      mutate(Peringkat = dplyr::row_number()) %>%
      ungroup() %>%
      transmute(
        Peringkat,
        Level = level,
        `Kode Kategori` = kode_kategori,
        Kategori = kategori_label,
        Tahun = as.integer(tahun),
        Periode = period_label(periode),
        `Location Quotient (LQ)` = as.numeric(LQ),
        Status = Keterangan,
        Interpretasi = case_when(
          is.na(LQ) ~ "Location Quotient (LQ) tidak dapat dihitung karena data pembanding tidak lengkap.",
          LQ > 1 ~ "Sektor basis: peran sektor lebih besar dibanding wilayah pembanding.",
          LQ < 1 ~ "Sektor nonbasis: peran sektor lebih kecil dibanding wilayah pembanding.",
          TRUE ~ "Sama dengan wilayah pembanding."
        )
      ) %>%
      sanitize_dt_frame_v85()

    validate(need(nrow(data) > 0, "Data Location Quotient (LQ) belum tersedia untuk filter ini."))
    data
  })

  # Nilai numerik DLQ dipertahankan sebagai numeric presisi penuh.
  # Pembulatan hanya diterapkan oleh DT::formatRound() saat tabel dirender.

  clean_text_dlq_v91 <- function(x) {
    x <- as.character(x)
    x[is.na(x) | !nzchar(trimws(x))] <- "–"
    x
  }

  dlq_table_data_v91 <- reactive({
    raw_dlq <- analysis_dlq_display_data_v8()
    validate(need(
      !is.null(raw_dlq) && nrow(raw_dlq) > 0,
      "Data Dynamic Location Quotient (DLQ) belum tersedia untuk filter ini."
    ))

    required_columns <- c(
      ".dlq_tahun_awal", ".dlq_tahun_akhir", ".dlq_periode",
      "level", "kode_kategori", "kategori_label", "LQ_awal", "LQ_akhir",
      "Pertumbuhan Sektor Wilayah", "Pertumbuhan Total Wilayah",
      "Pertumbuhan Sektor Acuan", "Pertumbuhan Total Acuan", "DLQ",
      "Status LQ", "Status DLQ", "Klasifikasi Sektor"
    )
    missing_columns <- setdiff(required_columns, names(raw_dlq))
    validate(need(
      length(missing_columns) == 0L,
      paste0(
        "Tabel DLQ belum dapat dibentuk karena kolom berikut tidak tersedia: ",
        paste(missing_columns, collapse = ", "), "."
      )
    ))

    klasifikasi <- clean_text_dlq_v91(raw_dlq[["Klasifikasi Sektor"]])
    interpretasi <- dplyr::case_when(
      klasifikasi == "Unggulan" ~ "Basis dan prospektif.",
      klasifikasi == "Prospektif" ~ "Basis tetapi prospeknya melemah.",
      klasifikasi == "Andalan" ~ "Nonbasis tetapi prospektif untuk dikembangkan.",
      klasifikasi == "Kurang Prospektif" ~ "Nonbasis dan kurang prospektif.",
      TRUE ~ "Tidak terklasifikasi."
    )

    data.frame(
      `Tahun Awal` = suppressWarnings(as.integer(raw_dlq[[".dlq_tahun_awal"]])),
      `Tahun Akhir` = suppressWarnings(as.integer(raw_dlq[[".dlq_tahun_akhir"]])),
      `Periode DLQ` = clean_text_dlq_v91(raw_dlq[[".dlq_periode"]]),
      Level = clean_text_dlq_v91(raw_dlq[["level"]]),
      `Kode Kategori` = clean_text_dlq_v91(raw_dlq[["kode_kategori"]]),
      Kategori = clean_text_dlq_v91(raw_dlq[["kategori_label"]]),
      `LQ Awal` = suppressWarnings(as.numeric(raw_dlq[["LQ_awal"]])),
      `LQ Akhir` = suppressWarnings(as.numeric(raw_dlq[["LQ_akhir"]])),
      `Pertumbuhan Sektor Wilayah (%)` = suppressWarnings(as.numeric(raw_dlq[["Pertumbuhan Sektor Wilayah"]])) * 100,
      `Pertumbuhan Total Wilayah (%)` = suppressWarnings(as.numeric(raw_dlq[["Pertumbuhan Total Wilayah"]])) * 100,
      `Pertumbuhan Sektor Pembanding (%)` = suppressWarnings(as.numeric(raw_dlq[["Pertumbuhan Sektor Acuan"]])) * 100,
      `Pertumbuhan Total Pembanding (%)` = suppressWarnings(as.numeric(raw_dlq[["Pertumbuhan Total Acuan"]])) * 100,
      DLQ = suppressWarnings(as.numeric(raw_dlq[["DLQ"]])),
      `Status LQ` = clean_text_dlq_v91(raw_dlq[["Status LQ"]]),
      `Status DLQ` = clean_text_dlq_v91(raw_dlq[["Status DLQ"]]),
      `Klasifikasi Sektor` = klasifikasi,
      Interpretasi = clean_text_dlq_v91(interpretasi),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ) %>%
      sanitize_dt_frame_v85()
  })

  shiftshare_table_data_v91 <- reactive({
    raw_shift <- analysis_shift_table_data_v85()
    validate(need(
      nrow(raw_shift) > 0 && all(c("CE", "RIE", "RSE", "Tipe", "Diagnosa") %in% names(raw_shift)),
      "Data Extended Shift Share belum tersedia untuk tahun ini. Pastikan tahun awal, tahun akhir, dan wilayah pembanding tersedia."
    ))

    raw_shift %>%
      mutate(.is_total = level == "Total PDRB") %>%
      arrange(.is_total, desc(abs(CE)), kode_kategori) %>%
      transmute(
        Tahun = as.integer(tahun_akhir),
        Daerah = as.character(wilayah),
        Level = as.character(level),
        `Kode Kategori` = as.character(kode_kategori),
        `Kategori/Lapangan Usaha` = as.character(kategori_label),
        CE = as.numeric(CE),
        RIE = as.numeric(RIE),
        RSE = as.numeric(RSE),
        Tipe = as.character(Tipe),
        `Diagnosa (Indonesia)` = as.character(Diagnosa),
        `Diagnosis (English)` = as.character(shift_share_diagnosis_en_v5(Tipe)),
        Catatan = as.character(shift_share_note_v5(Tipe))
      ) %>%
      sanitize_dt_frame_v85()
  })

  potential_dt_v91 <- function(data) {
    table_output <- DT::datatable(
      data,
      rownames = FALSE,
      escape = TRUE,
      selection = "none",
      class = "stripe hover compact nowrap",
      options = pdrb_dt_options(pageLength = 10, buttons = FALSE, dom = "lfrtip")
    )

    identifier_columns <- intersect(
      c("Peringkat", "Tahun", "Tahun Awal", "Tahun Akhir"),
      names(data)
    )
    numeric_columns <- setdiff(
      names(data)[vapply(data, is.numeric, logical(1))],
      identifier_columns
    )
    if (length(numeric_columns) > 0L) {
      table_output <- table_output %>%
        DT::formatRound(numeric_columns, digits = 4, mark = ",", dec.mark = ".")
    }
    table_output
  }

  output$lq_table_v5 <- DT::renderDT({
    potential_dt_v91(lq_table_data_v91())
  }, server = FALSE)

  output$dlq_table_v5 <- DT::renderDT({
    data <- dlq_table_data_v91()
    validate(need(nrow(data) > 0, "Data Dynamic Location Quotient (DLQ) belum tersedia untuk filter ini."))
    potential_dt_v91(data)
  }, server = FALSE)

  output$shiftshare_table_v5 <- DT::renderDT({
    potential_dt_v91(shiftshare_table_data_v91())
  }, server = FALSE)



  potential_lq_download_filename_v919 <- function(extension) {
    build_download_filename_v919(
      c(
        "LQ",
        download_scalar_v919(input$dasar_harga_lq_tren, "ADHK"),
        download_region_name_v919(input$wilayah),
        download_year_part_v919(input$tahun_lq),
        download_period_part_v919(input$periode_lq_peringkat)
      ),
      extension
    )
  }

  potential_dlq_download_filename_v919 <- function(extension) {
    start_year <- download_scalar_v919(input$tahun_awal_analisis, "__ALL__")
    end_year <- download_scalar_v919(input$tahun_akhir_analisis, "__ALL__")
    year_part <- if (identical(start_year, "__ALL__") || identical(end_year, "__ALL__")) {
      "Semua Tahun"
    } else {
      paste(start_year, end_year, sep = "-")
    }
    build_download_filename_v919(
      c("DLQ", "ADHK", download_region_name_v919(input$wilayah), year_part),
      extension
    )
  }

  potential_shift_download_filename_v919 <- function(extension) {
    shift_data <- tryCatch(analysis_shift_data_v5(), error = function(e) tibble::tibble())
    reference_label <- NULL
    if (nrow(shift_data) > 0L && "wilayah_acuan" %in% names(shift_data)) {
      reference_code <- if ("kode_wilayah_acuan" %in% names(shift_data)) shift_data$kode_wilayah_acuan[[1]] else NULL
      reference_label <- qualify_region_name_v919(shift_data$wilayah_acuan[[1]], reference_code)
    }
    if (is.null(reference_label)) reference_label <- download_group_name_v919(input$kelompok)

    year_part <- paste(
      download_scalar_v919(input$tahun_awal_shift_analisis, "TAHUN_AWAL"),
      download_scalar_v919(input$tahun_akhir_shift_analisis, "TAHUN_AKHIR"),
      sep = "-"
    )
    build_download_filename_v919(
      c(
        "Extended Shift Share", "ADHK", download_region_name_v919(input$wilayah),
        "Dengan", reference_label, year_part
      ),
      extension
    )
  }

  output$download_lq_excel <- downloadHandler(
    filename = function() potential_lq_download_filename_v919("xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      write_export_xlsx(lq_table_data_v91(), file, "Location Quotient")
    }
  )

  output$download_lq_csv <- downloadHandler(
    filename = function() potential_lq_download_filename_v919("csv"),
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      write_export_csv(lq_table_data_v91(), file)
    }
  )

  output$download_dlq_excel <- downloadHandler(
    filename = function() potential_dlq_download_filename_v919("xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      write_export_xlsx(dlq_table_data_v91(), file, "Dynamic LQ")
    }
  )

  output$download_dlq_csv <- downloadHandler(
    filename = function() potential_dlq_download_filename_v919("csv"),
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      write_export_csv(dlq_table_data_v91(), file)
    }
  )

  output$download_shiftshare_excel <- downloadHandler(
    filename = function() potential_shift_download_filename_v919("xlsx"),
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      write_export_xlsx(shiftshare_table_data_v91(), file, "Extended Shift Share")
    }
  )

  output$download_shiftshare_csv <- downloadHandler(
    filename = function() potential_shift_download_filename_v919("csv"),
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      write_export_csv(shiftshare_table_data_v91(), file)
    }
  )

  dlq_semua_tahun_v91 <- reactive({
    awal <- as.character(input$tahun_awal_analisis)[1]
    is.na(awal) || !nzchar(awal) || identical(awal, "__ALL__")
  })

  dlq_unit_label_v91 <- reactive({
    if (isTRUE(dlq_semua_tahun_v91())) "observasi" else "sektor"
  })

  output$vb_analysis_1 <- renderValueBox({
    jenis <- if (is.null(input$jenis_analisis_wilayah)) "lq" else input$jenis_analisis_wilayah
    if (identical(jenis, "dlq")) {
      data <- analysis_dlq_display_data_v8()
      jumlah <- sum(data$`Status DLQ` == "Prospektif", na.rm = TRUE)
      return(valueBox(paste0(jumlah, " ", dlq_unit_label_v91()), "Prospektif · Dynamic Location Quotient (DLQ) > 1", icon = icon("line-chart"), color = "green"))
    }
    if (identical(jenis, "shift_share")) {
      data <- analysis_shift_data_v5()
      validate(need(nrow(data) > 0 && "Tipe" %in% names(data), "Data Extended Shift Share belum tersedia."))
      jumlah_t1 <- sum(data$Tipe == "T1", na.rm = TRUE)
      valueBox(paste0(jumlah_t1, " sektor"), "Tipe T1 · CE, RIE, dan RSE positif", icon = icon("check-circle"), color = "blue")
    } else {
      data <- analysis_lq_rank_data_v5()
      jumlah <- sum(data$Keterangan == "Basis", na.rm = TRUE)
      valueBox(paste0(jumlah, " sektor"), "Jumlah sektor basis · Location Quotient (LQ) > 1", icon = icon("check-circle"), color = "green")
    }
  })
  
  output$vb_analysis_2 <- renderValueBox({
    jenis <- if (is.null(input$jenis_analisis_wilayah)) "lq" else input$jenis_analisis_wilayah
    if (identical(jenis, "dlq")) {
      data <- analysis_dlq_display_data_v8()
      jumlah <- sum(data$`Status LQ` == "Basis" & data$`Status DLQ` == "Prospektif", na.rm = TRUE)
      return(valueBox(paste0(jumlah, " ", dlq_unit_label_v91()), "Basis dan prospektif", icon = icon("star"), color = "blue"))
    }
    if (identical(jenis, "shift_share")) {
      data <- analysis_shift_data_v5()
      comp <- if (is.null(input$komponen_shift_plot)) "CE" else as.character(input$komponen_shift_plot)[1]
      if (is.na(comp) || !comp %in% names(data)) comp <- "CE"
      validate(need(nrow(data) > 0 && comp %in% names(data), "Data Extended Shift Share belum tersedia."))
      top <- data %>%
        filter(!is.na(.data[[comp]])) %>%
        arrange(desc(.data[[comp]])) %>%
        slice(1)
      valueBox(
        ifelse(nrow(top) > 0, stringr::str_trunc(top$kategori_label[[1]], 32), "–"),
        ifelse(nrow(top) > 0, paste0(comp, " tertinggi · ", format_pdrb_card(top[[comp]][[1]])), paste0(comp, " tertinggi")),
        icon = icon("rocket"),
        color = "green"
      )
    } else {
      data <- analysis_lq_rank_data_v5()
      top <- data %>% filter(!is.na(LQ)) %>% slice_max(LQ, n = 1, with_ties = FALSE)
      valueBox(ifelse(nrow(top) > 0, stringr::str_trunc(top$kategori_label[[1]], 32), "–"), ifelse(nrow(top) > 0, paste0("Location Quotient (LQ) tertinggi · ", scales::number(top$LQ[[1]], accuracy = 0.001, decimal.mark = ".")), "Location Quotient (LQ) tertinggi"), icon = icon("arrow-up"), color = "blue")
    }
  })
  
  output$vb_analysis_3 <- renderValueBox({
    jenis <- if (is.null(input$jenis_analisis_wilayah)) "lq" else input$jenis_analisis_wilayah
    if (identical(jenis, "dlq")) {
      data <- analysis_dlq_display_data_v8()
      jumlah <- sum(data$`Status LQ` == "Basis" & data$`Status DLQ` == "Kurang Prospektif", na.rm = TRUE)
      return(valueBox(paste0(jumlah, " ", dlq_unit_label_v91()), "Basis tetapi melemah", icon = icon("warning"), color = "yellow"))
    }
    if (identical(jenis, "shift_share")) {
      data <- analysis_shift_data_v5()
      validate(need(nrow(data) > 0 && "Tipe" %in% names(data), "Data Extended Shift Share belum tersedia."))
      jumlah_t8 <- sum(data$Tipe == "T8", na.rm = TRUE)
      valueBox(paste0(jumlah_t8, " sektor"), "Tipe T8 · CE, RIE, dan RSE negatif", icon = icon("warning"), color = "yellow")
    } else {
      data <- analysis_lq_rank_data_v5()
      low <- data %>% filter(!is.na(LQ)) %>% slice_min(LQ, n = 1, with_ties = FALSE)
      valueBox(ifelse(nrow(low) > 0, stringr::str_trunc(low$kategori_label[[1]], 32), "–"), ifelse(nrow(low) > 0, paste0("Location Quotient (LQ) terendah · ", scales::number(low$LQ[[1]], accuracy = 0.001, decimal.mark = ".")), "Location Quotient (LQ) terendah"), icon = icon("arrow-down"), color = "yellow")
    }
  })
  
  output$analysis_interpretation <- renderUI({
    jenis <- if (is.null(input$jenis_analisis_wilayah)) "lq" else input$jenis_analisis_wilayah
    if (identical(jenis, "dlq")) {
      data <- analysis_dlq_display_data_v8()
      unggulan <- sum(data$`Klasifikasi Sektor` == "Unggulan", na.rm = TRUE)
      andalan <- sum(data$`Klasifikasi Sektor` == "Andalan", na.rm = TRUE)
      unit_text <- if (isTRUE(dlq_semua_tahun_v91())) " observasi sektor–periode " else " sektor "
      return(tags$div(
        class = "distribution-note", icon("info-circle"),
        span(paste0(
          "Terdapat ", unggulan, unit_text, "unggulan (basis dan prospektif) serta ",
          andalan, unit_text, "andalan (nonbasis tetapi prospektif). ",
          if (isTRUE(dlq_semua_tahun_v91())) "Ringkasan mencakup seluruh pasangan tahun berurutan yang tersedia. " else "",
          "Sektor unggulan layak diprioritaskan, sedangkan sektor andalan dapat menjadi kandidat pengembangan."
        ))
      ))
    }
    if (identical(jenis, "shift_share")) {
      data <- analysis_shift_data_v5()
      comp <- if (is.null(input$komponen_shift_plot)) "CE" else input$komponen_shift_plot
      validate(need(nrow(data) > 0 && comp %in% names(data), "Data Extended Shift Share belum tersedia."))
      positif <- sum(data[[comp]] > 0, na.rm = TRUE)
      negatif <- sum(data[[comp]] < 0, na.rm = TRUE)
      tipe_terbanyak <- data %>% count(Tipe, sort = TRUE) %>% filter(!is.na(Tipe)) %>% slice(1)
      teks_tipe <- if (nrow(tipe_terbanyak) > 0) paste0(" Tipe terbanyak adalah ", tipe_terbanyak$Tipe[[1]], " sebanyak ", tipe_terbanyak$n[[1]], " sektor.") else ""
      return(tags$div(class = "distribution-note", icon("info-circle"), span(paste0("Extended Shift Share utama memakai pendekatan extended. Komponen ", comp, " bernilai positif pada ", positif, " sektor dan negatif pada ", negatif, " sektor.", teks_tipe))))
    }
    data <- analysis_lq_rank_data_v5()
    jumlah_basis <- sum(data$Keterangan == "Basis", na.rm = TRUE)
    top <- data %>% filter(!is.na(LQ)) %>% slice_max(LQ, n = 1, with_ties = FALSE)
    teks_top <- if (nrow(top) > 0) paste0(" Sektor dengan Location Quotient (LQ) tertinggi adalah ", top$kategori_label[[1]], " dengan nilai Location Quotient (LQ) ", scales::number(top$LQ[[1]], accuracy = 0.001, decimal.mark = "."), ".") else ""
    tags$div(class = "distribution-note", icon("info-circle"), span(paste0("Terdapat ", jumlah_basis, " sektor basis pada wilayah terpilih. Nilai LQ di atas 1 menunjukkan peran sektor lebih besar dibandingkan wilayah pembanding.", teks_top)))
  })
  
  lq_sector_choice_state_v82 <- reactiveVal(NULL)

  observeEvent(
    list(
      processed_upload_version(),
      input$jenis_analisis_wilayah,
      input$kelompok,
      input$wilayah,
      input$dasar_harga_lq_tren,
      input$potensi_level_kategori
    ),
    {
      req(
        input$jenis_analisis_wilayah, input$kelompok, input$wilayah,
        input$dasar_harga_lq_tren, input$potensi_level_kategori
      )
      if (!identical(as.character(input$jenis_analisis_wilayah)[1], "lq")) {
        return(invisible(NULL))
      }

      choices <- potential_sector_choices_v82(
        input$potensi_level_kategori,
        input$dasar_harga_lq_tren,
        include_all = FALSE
      )
      values <- unname(as.character(choices))
      current <- isolate(as.character(input$kategori_lq_tren)[1])
      selected <- if (length(values) == 0L) {
        NULL
      } else if (!is.na(current) && current %in% values) {
        current
      } else {
        values[[1]]
      }

      signature <- paste(names(choices), values, sep = "=", collapse = "|")
      if (identical(lq_sector_choice_state_v82(), signature)) {
        return(invisible(NULL))
      }
      lq_sector_choice_state_v82(signature)
      freezeReactiveValue(input, "kategori_lq_tren")
      send_stable_selectize_v82("kategori_lq_tren", choices, selected)
    },
    ignoreInit = FALSE,
    ignoreNULL = FALSE
  )

  output$plot_lq_v5 <- renderPlotly({
    tampilan <- if (is.null(input$tampilan_lq)) "tren_sektor" else as.character(input$tampilan_lq)[1]
    if (tampilan %in% c("tren", "tren_semua", "tren_sektor")) {
      data <- analysis_lq_trend_data_v5()
      if (identical(tampilan, "tren_sektor")) {
        sektor_pilih <- as.character(input$kategori_lq_tren)[1]
        if (!is.null(sektor_pilih) && !is.na(sektor_pilih) && nzchar(sektor_pilih)) {
          bagian <- strsplit(sektor_pilih, "||", fixed = TRUE)[[1]]
          if (length(bagian) == 2) {
            data <- data %>% filter(level == bagian[[1]], kode_kategori == bagian[[2]])
          }
        }
      }
      validate(need(nrow(data) > 0, "Data tren Location Quotient (LQ) belum tersedia."))
      x_order <- data %>% distinct(waktu_urut, waktu_label) %>% arrange(waktu_urut) %>% pull(waktu_label)
      judul_lq <- if (identical(tampilan, "tren_sektor")) {
        paste0("Tren Location Quotient (LQ) Sektor Terpilih ", input$dasar_harga_lq_tren)
      } else {
        paste0("Tren Location Quotient (LQ) Semua Sektor ", input$dasar_harga_lq_tren)
      }
      return(
        plot_ly(
          data,
          x = ~waktu_label,
          y = ~LQ,
          color = ~label_garis,
          colors = PDRB_CATEGORY_PALETTE,
          type = "scatter",
          mode = "lines+markers",
          hoverinfo = "text",
          hovertext = ~hover_lq,
          line = list(width = 2),
          marker = list(size = 6)
        ) %>%
          layout(
            title = list(text = judul_lq, x = 0.02),
            xaxis = list(title = "Tahun/Periode", type = "category", categoryorder = "array", categoryarray = x_order, tickangle = -35, gridcolor = PDRB_COLORS$grid, zeroline = FALSE),
            yaxis = list(title = "Location Quotient (LQ)", gridcolor = PDRB_COLORS$grid, zeroline = FALSE),
            shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = 1, y1 = 1, line = list(color = PDRB_COLORS$muted, dash = "dash", width = 1.2))),
            legend = list(orientation = "h", x = 0, y = -0.35, font = list(size = 10)),
            margin = list(l = 70, r = 25, b = 145, t = 70),
            paper_bgcolor = PDRB_COLORS$surface,
            plot_bgcolor = PDRB_COLORS$surface,
            font = list(color = PDRB_COLORS$ink),
            hovermode = "closest"
          ) %>%
          config(displaylogo = FALSE)
      )
    }
    
    tahun_lq_value <- as.character(input$tahun_lq)[1]
    periode_lq_value <- as.character(input$periode_lq_peringkat)[1]
    include_time_label <- identical(tahun_lq_value, "__ALL__") ||
      periode_lq_value %in% c("__ALL__", "__QUARTERS__")

    data <- analysis_lq_rank_data_v5() %>%
      mutate(
        LQ = suppressWarnings(as.numeric(LQ)),
        tahun = suppressWarnings(as.integer(tahun)),
        periode = as.character(periode)
      ) %>%
      filter(is.finite(LQ)) %>%
      arrange(desc(LQ), kode_kategori, tahun, match(periode, c("I", "II", "III", "IV", "Total"))) %>%
      slice_head(n = 15) %>%
      mutate(
        waktu_lq = paste0(tahun, " ", period_label(periode)),
        label_dasar = if (isTRUE(include_time_label)) {
          paste0(stringr::str_trunc(as.character(kategori_label), 42), " (", waktu_lq, ")")
        } else {
          stringr::str_trunc(as.character(kategori_label), 55)
        },
        # Label harus unik agar Plotly tidak menggabungkan beberapa kategori
        # waktu menjadi satu kategori sumbu dan gagal membentuk widget.
        label_plot = make.unique(as.character(label_dasar), sep = " · "),
        nilai_label = scales::number(LQ, accuracy = 0.001, big.mark = ",", decimal.mark = "."),
        hover_lq = paste0(
          as.character(kategori_label),
          "<br>Tahun/Periode: ", waktu_lq,
          "<br>Location Quotient (LQ): ", nilai_label,
          "<br>Status: ", as.character(Keterangan)
        )
      ) %>%
      arrange(LQ)

    validate(need(nrow(data) > 0, "Data Location Quotient (LQ) belum tersedia."))

    max_x <- max(c(1, data$LQ), na.rm = TRUE)
    periode_judul <- dplyr::case_when(
      identical(periode_lq_value, "__ALL__") ~ "Semua Periode",
      identical(periode_lq_value, "__QUARTERS__") ~ "Semua Triwulan",
      TRUE ~ period_label(periode_lq_value)
    )
    tahun_judul <- if (identical(tahun_lq_value, "__ALL__")) "Semua Tahun" else tahun_lq_value

    plotly::plot_ly(
      x = data$LQ,
      y = data$label_plot,
      type = "bar",
      orientation = "h",
      text = data$nilai_label,
      textposition = "outside",
      hoverinfo = "text",
      hovertext = data$hover_lq,
      marker = list(color = indicator_color(paste("PDRB", input$dasar_harga_lq_tren)))
    ) %>%
      plotly::layout(
        title = list(
          text = paste0(
            "Ranking Location Quotient (LQ) ", input$dasar_harga_lq_tren,
            " - ", tahun_judul, " ", periode_judul
          ),
          x = 0.02
        ),
        xaxis = list(
          title = "Location Quotient (LQ)",
          range = c(0, max_x * 1.18),
          gridcolor = PDRB_COLORS$grid,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          type = "category",
          categoryorder = "array",
          categoryarray = data$label_plot
        ),
        shapes = list(list(
          type = "line", xref = "x", x0 = 1, x1 = 1,
          yref = "paper", y0 = 0, y1 = 1,
          line = list(color = PDRB_COLORS$muted, dash = "dash", width = 1.2)
        )),
        margin = list(l = 300, r = 100, b = 60, t = 70),
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        showlegend = FALSE
      ) %>%
      plotly::config(displaylogo = FALSE)
  })
  output$plot_dlq_v5 <- renderPlotly({
    semua_tahun_plot <- isTRUE(dlq_semua_tahun_v91())
    mode_plot <- if (semua_tahun_plot) "markers" else "markers+text"
    data <- analysis_dlq_display_data_v8() %>%
      filter(!is.na(LQ_akhir), !is.na(DLQ)) %>%
      mutate(
        label_plot = if (semua_tahun_plot) "" else kode_kategori,
        hover_dlq = paste0(
          kategori_label,
          "<br>Periode DLQ: ", .dlq_periode,
          "<br>LQ akhir: ", round(LQ_akhir, 4),
          "<br>DLQ: ", round(DLQ, 4),
          "<br>Status LQ: ", `Status LQ`,
          "<br>Status DLQ: ", `Status DLQ`,
          "<br>Klasifikasi: ", `Klasifikasi Sektor`
        )
      )
    validate(need(nrow(data) > 0, "Data Dynamic Location Quotient (DLQ) belum tersedia."))
    max_x <- max(c(1, data$LQ_akhir), na.rm = TRUE)
    max_y <- max(c(1, data$DLQ), na.rm = TRUE)
    plot_ly(
      data,
      x = ~LQ_akhir,
      y = ~DLQ,
      type = "scatter",
      mode = mode_plot,
      text = ~label_plot,
      textposition = "top center",
      color = ~`Klasifikasi Sektor`,
      colors = PDRB_CATEGORY_PALETTE,
      hoverinfo = "text",
      hovertext = ~hover_dlq,
      marker = list(size = 11, opacity = 0.85)
    ) %>%
      layout(
        title = list(
          text = if (semua_tahun_plot) {
            "Kuadran Dynamic Location Quotient (DLQ): seluruh pasangan tahun"
          } else {
            "Kuadran Dynamic Location Quotient (DLQ): LQ akhir dan prospek sektor"
          },
          x = 0.02
        ),
        xaxis = list(title = "LQ Tahun Akhir", range = c(0, max_x * 1.12), gridcolor = PDRB_COLORS$grid, zeroline = FALSE),
        yaxis = list(title = "DLQ", range = c(0, max_y * 1.12), gridcolor = PDRB_COLORS$grid, zeroline = FALSE),
        shapes = list(
          list(type = "line", xref = "x", x0 = 1, x1 = 1, yref = "paper", y0 = 0, y1 = 1, line = list(color = PDRB_COLORS$muted, dash = "dash", width = 1.2)),
          list(type = "line", xref = "paper", x0 = 0, x1 = 1, yref = "y", y0 = 1, y1 = 1, line = list(color = PDRB_COLORS$muted, dash = "dash", width = 1.2))
        ),
        annotations = list(
          list(x = 0.98, y = 1.02, xref = "paper", yref = "paper", text = "Basis & prospektif", showarrow = FALSE, font = list(size = 11, color = PDRB_COLORS$muted)),
          list(x = 0.02, y = 1.02, xref = "paper", yref = "paper", text = "Nonbasis & prospektif", showarrow = FALSE, font = list(size = 11, color = PDRB_COLORS$muted))
        ),
        legend = list(orientation = "h", x = 0, y = -0.22, font = list(size = 10)),
        margin = list(l = 70, r = 40, b = 115, t = 75),
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        hovermode = "closest"
      ) %>%
      config(displaylogo = FALSE)
  })
  output$plot_shiftshare_v5 <- renderPlotly({
    komponen_tersedia <- c("NE", "IM", "CE", "RIE", "RSE", "RCCE")
    comp <- if (is.null(input$komponen_shift_plot)) "CE" else input$komponen_shift_plot
    if (!comp %in% komponen_tersedia) comp <- "CE"
    raw_shift <- analysis_shift_data_v5()
    validate(need(
      nrow(raw_shift) > 0 && comp %in% names(raw_shift),
      "Data Extended Shift Share belum tersedia untuk grafik ini. Pastikan tahun sebelumnya, tahun terpilih, dan wilayah pembanding tersedia."
    ))
    data <- raw_shift %>%
      mutate(
        nilai_plot = .data[[comp]],
        nilai_plot_triliun = nilai_plot / 1e6,
        nilai_label = format_pdrb_plot(nilai_plot, list(scale = 1e6, title = "Triliun Rupiah")),
        label_plot = stringr::str_trunc(kategori_label, 55)
      ) %>%
      filter(!is.na(nilai_plot)) %>%
      arrange(desc(abs(nilai_plot))) %>%
      slice_head(n = 17) %>%
      arrange(nilai_plot)
    validate(need(nrow(data) > 0, "Data Extended Shift Share belum tersedia."))
    finite_values <- data$nilai_plot_triliun[is.finite(data$nilai_plot_triliun)]
    x_range <- if (length(finite_values) > 0) {
      max_abs <- max(abs(finite_values), na.rm = TRUE)
      c(-max_abs * 1.18, max_abs * 1.18)
    } else {
      NULL
    }
    plot_ly(
      data,
      x = ~nilai_plot_triliun,
      y = ~reorder(label_plot, nilai_plot_triliun),
      type = "bar",
      orientation = "h",
      text = ~nilai_label,
      textposition = "outside",
      hoverinfo = "text",
      hovertext = ~paste0(
        kategori_label,
        "<br>Komponen: ", comp,
        "<br>Nilai: ", nilai_label,
        "<br>Tipe: ", Tipe,
        "<br>Diagnosa: ", Diagnosa
      ),
      marker = list(color = indicator_color("PDRB ADHK"))
    ) %>%
      layout(
        title = list(text = paste0("17 Kategori Extended Shift Share Tahunan - ", comp, " · ", paste0(input$tahun_awal_shift_analisis, "–", input$tahun_akhir_shift_analisis)), x = 0.02),
        xaxis = list(title = "Triliun Rupiah", range = x_range, zeroline = TRUE, gridcolor = PDRB_COLORS$grid),
        yaxis = list(title = ""),
        margin = list(l = 260, r = 110, b = 65, t = 70),
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        showlegend = FALSE
      ) %>%
      config(displaylogo = FALSE)
  })
  
  
  observeEvent(input$tahun_awal_dlq_tabel, {
    years <- tryCatch(valid_dlq_years(), error = function(e) integer(0))
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    awal_input <- as.character(input$tahun_awal_dlq_tabel)[1]

    # Pilihan Semua Tahun menghitung seluruh pasangan tahun berurutan yang valid.
    # Tahun Akhir dikunci ke Semua Tahun agar pengguna tidak perlu memilih pasangan satu per satu.
    if (identical(awal_input, "__ALL__")) {
      update_pdrb_selectize(
        session,
        "tahun_akhir_dlq_tabel",
        choices = c("Semua Tahun" = "__ALL__"),
        selected = "__ALL__"
      )
      return(NULL)
    }

    awal <- suppressWarnings(as.integer(awal_input))
    if (is.na(awal) || length(years) == 0) return(NULL)
    end_choices <- years[years == awal + 1L]
    if (length(end_choices) == 0) return(NULL)
    update_pdrb_selectize(
      session,
      "tahun_akhir_dlq_tabel",
      choices = valid_year_choices(end_choices),
      selected = as.character(end_choices[[1]])
    )
  }, ignoreInit = TRUE)
  
  observeEvent(input$tahun_awal_shift_tabel, {
    years <- tryCatch(valid_dlq_years(), error = function(e) integer(0))
    years <- sort(unique(as.integer(years)))
    years <- years[!is.na(years)]
    awal <- suppressWarnings(as.integer(input$tahun_awal_shift_tabel))
    if (is.na(awal) || length(years) == 0) return(NULL)
    end_choices <- years[years > awal]
    if (length(end_choices) == 0) return(NULL)
    selected <- if (!is.null(input$tahun_akhir_shift_tabel) && suppressWarnings(as.integer(input$tahun_akhir_shift_tabel)) %in% end_choices) {
      as.character(input$tahun_akhir_shift_tabel)
    } else {
      as.character(max(end_choices, na.rm = TRUE))
    }
    update_pdrb_selectize(session, "tahun_akhir_shift_tabel", choices = valid_year_choices(end_choices), selected = selected)
  }, ignoreInit = TRUE)
  
  
  output$tabel_pilihan_utama_ui <- renderUI({
    jenis_choices <- c(
      "Data PDRB" = "pdrb",
      "Distribusi PDRB" = "distribusi",
      "Pertumbuhan PDRB" = "pertumbuhan",
      "Indeks Implisit" = "indeks",
      "Laju Indeks Implisit" = "laju_indeks",
      "Sumber Pertumbuhan" = "sumber_pertumbuhan",
      "Location Quotient (LQ)" = "lq",
      "Dynamic Location Quotient (DLQ)" = "dlq",
      "Extended Shift Share" = "shift_share"
    )
    
    jenis <- if (is.null(input$jenis_tabel_data)) "pdrb" else as.character(input$jenis_tabel_data)[1]
    if (is.na(jenis) || !jenis %in% unname(jenis_choices)) jenis <- "pdrb"
    
    level_choices <- c(
      "Semua" = "Semua",
      "Total PDRB" = "Total PDRB",
      "Kategori Utama" = "Kategori Utama",
      "Subkategori" = "Subkategori",
      "Rincian Subkategori" = "Rincian"
    )
    selected_level <- if (is.null(input$level_tabel)) "Semua" else as.character(input$level_tabel)[1]
    if (is.na(selected_level) || !selected_level %in% unname(level_choices)) selected_level <- "Semua"
    
    selected_format <- if (is.null(input$format_tabel_data)) "lebar" else as.character(input$format_tabel_data)[1]
    if (is.na(selected_format) || !selected_format %in% c("lebar", "panjang")) selected_format <- "lebar"
    
    selected_dasar <- if (is.null(input$dasar_harga_tabel)) "ADHK" else as.character(input$dasar_harga_tabel)[1]
    if (is.na(selected_dasar) || !selected_dasar %in% c("ADHB", "ADHK")) selected_dasar <- "ADHK"
    
    selected_lq_dasar <- if (is.null(input$dasar_harga_lq_tabel)) "ADHK" else as.character(input$dasar_harga_lq_tabel)[1]
    if (is.na(selected_lq_dasar) || !selected_lq_dasar %in% c("ADHB", "ADHK")) selected_lq_dasar <- "ADHK"
    
    selected_growth <- if (is.null(input$jenis_pertumbuhan_tabel)) "Y-on-Y" else as.character(input$jenis_pertumbuhan_tabel)[1]
    if (is.na(selected_growth) || !selected_growth %in% c("Q-to-Q", "Y-on-Y", "C-to-C")) selected_growth <- "Y-on-Y"
    
    selected_index <- "Indeks Implisit"
    indeks_choices <- c("Indeks Implisit" = "Indeks Implisit")
    selected_laju <- if (is.null(input$jenis_laju_tabel)) "Y-on-Y" else as.character(input$jenis_laju_tabel)[1]
    laju_choices <- c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C")
    if (is.na(selected_laju) || !selected_laju %in% unname(laju_choices)) selected_laju <- "Y-on-Y"
    
    data_ui_tabel <- tryCatch(pdrb_data(), error = function(e) tibble::tibble())
    if (!is.null(data_ui_tabel) && nrow(data_ui_tabel) > 0) {
      data_ui_tabel <- data_ui_tabel %>%
        mutate(
          tahun = suppressWarnings(as.integer(tahun)),
          periode = as.character(periode),
          indikator = as.character(indikator)
        )
      if (!is.null(input$kelompok) && !is.na(as.character(input$kelompok)[1])) {
        data_ui_tabel <- data_ui_tabel %>% filter(kode_kelompok == input$kelompok)
      }
      if (!is.null(input$wilayah) && !is.na(as.character(input$wilayah)[1])) {
        data_ui_tabel <- data_ui_tabel %>% filter(kode_wilayah == input$wilayah)
      }
    }
    
    all_years <- if (!is.null(data_ui_tabel) && nrow(data_ui_tabel) > 0 && "tahun" %in% names(data_ui_tabel)) {
      sort(unique(stats::na.omit(as.integer(data_ui_tabel$tahun))))
    } else integer(0)
    
    period_choices_fixed <- c(
      "Triwulan I" = "I",
      "Triwulan II" = "II",
      "Triwulan III" = "III",
      "Triwulan IV" = "IV",
      "Tahun" = "Total"
    )
    
    keep_selected <- function(current, choices, default = NULL) {
      current <- as.character(current)[1]
      valid_values <- unname(choices)
      if (!is.null(current) && length(current) > 0 && !is.na(current) && current %in% valid_values) current else default
    }
    
    indicator_for_table <- function(table_type, dasar = selected_dasar, growth = selected_growth, index = selected_index, laju = selected_laju) {
      switch(
        table_type,
        "pdrb" = paste("PDRB", dasar),
        "distribusi" = paste("PDRB", dasar),
        "pertumbuhan" = paste("Pertumbuhan", dasar, growth),
        "indeks" = "Indeks Implisit",
        "laju_indeks" = paste("Laju Indeks Implisit", laju),
        "sumber_pertumbuhan" = paste("Sumber Pertumbuhan", dasar, growth),
        paste("PDRB", dasar)
      )
    }
    
    years_for_indicator <- function(indikator_pilih) {
      if (is.null(data_ui_tabel) || nrow(data_ui_tabel) == 0 || is.na(indikator_pilih)) return(all_years)
      years <- data_ui_tabel %>%
        filter(indikator == indikator_pilih, !is.na(nilai)) %>%
        distinct(tahun) %>%
        arrange(tahun) %>%
        pull(tahun)
      years <- sort(unique(as.integer(years)))
      years <- years[!is.na(years)]
      if (length(years) == 0) all_years else years
    }
    
    periods_for_indicator <- function(indikator_pilih, tahun_input_id = "tahun_tabel") {
      if (is.null(data_ui_tabel) || nrow(data_ui_tabel) == 0 || is.na(indikator_pilih)) return(period_choices_fixed)
      period_data <- data_ui_tabel %>% filter(indikator == indikator_pilih, !is.na(nilai))
      tahun_pilih <- as.character(input[[tahun_input_id]])[1]
      if (!is.null(tahun_pilih) && !is.na(tahun_pilih) && nzchar(tahun_pilih) && !identical(tahun_pilih, "__ALL__")) {
        period_data <- period_data %>% filter(tahun == suppressWarnings(as.integer(tahun_pilih)))
      }
      periods <- period_data %>% distinct(periode) %>% pull(periode) %>% as.character()
      periods <- periods[periods %in% unname(period_choices_fixed)]
      if (length(periods) == 0) return(period_choices_fixed)
      period_choices_fixed[unname(period_choices_fixed) %in% periods]
    }
    
    ordinary_indicator <- indicator_for_table(jenis)
    ordinary_year_choices <- c("Semua Tahun" = "__ALL__", valid_year_choices(years_for_indicator(ordinary_indicator)))
    ordinary_period_base <- periods_for_indicator(ordinary_indicator, "tahun_tabel")
    ordinary_has_quarters <- any(unname(ordinary_period_base) %in% c("I", "II", "III", "IV"))
    ordinary_period_choices <- c(
      "Semua Periode" = "__ALL__",
      if (ordinary_has_quarters) c("Semua Triwulan" = "__QUARTERS__") else character(0),
      ordinary_period_base
    )
    
    # Pilihan tahun LQ harus berupa irisan tahun yang benar-benar dapat dihitung.
    lq_years_ui <- tryCatch(valid_lq_years(), error = function(e) integer(0))
    lq_year_choices <- c("Semua Tahun" = "__ALL__", valid_year_choices(lq_years_ui))
    lq_period_choices <- c("Semua Periode" = "__ALL__", "Semua Triwulan" = "__QUARTERS__", period_choices_fixed)
    
    dlq_years_ui <- tryCatch(valid_dlq_years(), error = function(e) all_years)
    dlq_years_ui <- sort(unique(as.integer(dlq_years_ui)))
    dlq_years_ui <- dlq_years_ui[!is.na(dlq_years_ui)]
    if (length(dlq_years_ui) == 0) dlq_years_ui <- all_years
    dlq_start_years_ui <- dlq_years_ui[(dlq_years_ui + 1L) %in% dlq_years_ui]
    if (length(dlq_start_years_ui) == 0 && length(dlq_years_ui) > 1) dlq_start_years_ui <- dlq_years_ui[-length(dlq_years_ui)]

    # Default DLQ pada Tabel Data adalah Semua Tahun agar tabel dan unduhan
    # langsung memuat seluruh pasangan tahun berurutan yang tersedia.
    dlq_year_choices <- c("Semua Tahun" = "__ALL__", valid_year_choices(dlq_start_years_ui))
    selected_dlq_year_min <- "__ALL__"
    current_dlq_start_input <- as.character(input$tahun_awal_dlq_tabel)[1]
    current_dlq_all <- is.na(current_dlq_start_input) || !nzchar(current_dlq_start_input) || identical(current_dlq_start_input, "__ALL__")
    current_dlq_start <- suppressWarnings(as.integer(current_dlq_start_input))

    if (current_dlq_all) {
      dlq_end_choices <- c("Semua Tahun" = "__ALL__")
      selected_dlq_year_max <- "__ALL__"
    } else {
      if (is.na(current_dlq_start) || !current_dlq_start %in% dlq_start_years_ui) {
        current_dlq_start <- if (length(dlq_start_years_ui) > 0) dlq_start_years_ui[[1]] else NA_integer_
      }
      dlq_end_years_ui <- dlq_years_ui[dlq_years_ui == current_dlq_start + 1L]
      dlq_end_choices <- valid_year_choices(dlq_end_years_ui)
      selected_dlq_year_max <- if (length(dlq_end_years_ui) > 0) as.character(dlq_end_years_ui[[1]]) else NULL
    }
    
    shift_start_years_ui <- tryCatch(valid_shift_start_years(), error = function(e) integer(0))
    shift_end_years_ui_all <- tryCatch(valid_shift_years(), error = function(e) integer(0))
    shift_start_years_ui <- sort(unique(as.integer(shift_start_years_ui)))
    shift_end_years_ui_all <- sort(unique(as.integer(shift_end_years_ui_all)))
    shift_start_years_ui <- shift_start_years_ui[!is.na(shift_start_years_ui)]
    shift_end_years_ui_all <- shift_end_years_ui_all[!is.na(shift_end_years_ui_all)]
    if (length(shift_start_years_ui) == 0 && length(all_years) > 1) shift_start_years_ui <- all_years[-length(all_years)]
    if (length(shift_end_years_ui_all) == 0 && length(all_years) > 1) shift_end_years_ui_all <- all_years[-1]
    current_shift_start <- suppressWarnings(as.integer(as.character(input$tahun_awal_shift_tabel)[1]))
    if (is.na(current_shift_start) || !current_shift_start %in% shift_start_years_ui) current_shift_start <- if (length(shift_start_years_ui) > 0) min(shift_start_years_ui, na.rm = TRUE) else NA_integer_
    shift_end_years_ui <- shift_end_years_ui_all[shift_end_years_ui_all > current_shift_start]
    if (length(shift_end_years_ui) == 0) shift_end_years_ui <- shift_end_years_ui_all
    shift_start_choices <- valid_year_choices(shift_start_years_ui)
    shift_end_choices <- valid_year_choices(shift_end_years_ui)
    selected_shift_start <- if (!is.na(current_shift_start)) as.character(current_shift_start) else NULL
    selected_shift_end <- if (length(shift_end_years_ui) > 0) as.character(max(shift_end_years_ui, na.rm = TRUE)) else NULL
    
    filter_col <- function(width, control) {
      column(width, class = "tabel-filter-item", control)
    }
    
    control_jenis <- function(width) filter_col(width, pdrb_selectize("jenis_tabel_data", "Jenis Tabel", choices = jenis_choices, selected = jenis, placeholder = "Pilih jenis tabel"))
    control_format <- function(width) filter_col(width, pdrb_selectize("format_tabel_data", "Format Tabel", choices = c("Wide" = "lebar", "Long" = "panjang"), selected = selected_format, placeholder = "Pilih format tabel"))
    control_level <- function(width) filter_col(width, pdrb_selectize("level_tabel", "Tingkat Analisis", choices = level_choices, selected = selected_level, placeholder = "Pilih tingkat analisis"))
    control_dasar <- function(width) filter_col(width, pdrb_selectize("dasar_harga_tabel", "Dasar Harga", choices = c("ADHK" = "ADHK", "ADHB" = "ADHB"), selected = selected_dasar, placeholder = "Pilih dasar harga"))
    control_tahun <- function(width) filter_col(width, pdrb_selectize("tahun_tabel", "Tahun", choices = ordinary_year_choices, selected = keep_selected(input$tahun_tabel, ordinary_year_choices, "__ALL__"), placeholder = "Pilih tahun"))
    control_periode <- function(width) filter_col(width, pdrb_selectize("periode_tabel", "Periode", choices = ordinary_period_choices, selected = keep_selected(input$periode_tabel, ordinary_period_choices, "__ALL__"), placeholder = "Pilih periode"))
    control_growth <- function(width) filter_col(width, pdrb_selectize("jenis_pertumbuhan_tabel", "Jenis Pertumbuhan", choices = c("Q-to-Q" = "Q-to-Q", "Y-on-Y" = "Y-on-Y", "C-to-C" = "C-to-C"), selected = selected_growth, placeholder = "Pilih jenis pertumbuhan"))
    control_laju <- function(width) filter_col(width, pdrb_selectize("jenis_laju_tabel", "Jenis Laju", choices = laju_choices, selected = selected_laju, placeholder = "Pilih jenis laju"))
    control_index <- function(width) filter_col(width, pdrb_selectize("jenis_indeks_tabel", "Jenis Indeks", choices = indeks_choices, selected = selected_index, placeholder = "Pilih jenis indeks"))
    
    if (identical(jenis, "pdrb")) {
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row",
        control_jenis(2),
        control_dasar(2),
        control_tahun(2),
        control_periode(2),
        control_format(2),
        control_level(2)
      ))
    }
    
    if (identical(jenis, "distribusi")) {
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row",
        control_jenis(2),
        control_dasar(2),
        control_tahun(2),
        control_periode(2),
        control_format(2),
        control_level(2)
      ))
    }
    
    if (jenis %in% c("pertumbuhan", "sumber_pertumbuhan")) {
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row",
        control_jenis(2),
        control_dasar(2),
        control_level(2),
        control_growth(2),
        control_format(2),
        control_tahun(2),
        control_periode(2)
      ))
    }
    
    if (identical(jenis, "indeks")) {
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row",
        control_jenis(2),
        control_level(2),
        control_format(2),
        control_tahun(2),
        control_periode(2)
      ))
    }
    
    if (identical(jenis, "laju_indeks")) {
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row",
        control_jenis(2),
        control_laju(2),
        control_level(2),
        control_format(2),
        control_tahun(2),
        control_periode(2)
      ))
    }
    
    if (identical(jenis, "audit")) {
      audit_choices <- c(
        "Data dasar ADHB/ADHK" = "validasi_data_dasar",
        "Distribusi PDRB" = "distribusi",
        "Pertumbuhan PDRB" = "pertumbuhan",
        "Indeks Implisit" = "indeks_implisit",
        "Laju Indeks Implisit" = "laju_implisit",
        "Sumber Pertumbuhan" = "sumber_pertumbuhan"
      )
      audit_selected <- if (!is.null(input$jenis_audit_tabel) && input$jenis_audit_tabel %in% unname(audit_choices)) input$jenis_audit_tabel else "validasi_data_dasar"
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row",
        control_jenis(2),
        filter_col(2, pdrb_selectize("jenis_audit_tabel", "Indikator yang Diaudit", choices = audit_choices, selected = audit_selected, placeholder = "Pilih indikator audit")),
        control_level(2),
        control_format(2),
        control_tahun(2),
        control_periode(2)
      ))
    }
    
    if (jenis %in% c("lq", "lq_adhb", "lq_adhk")) {
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row",
        control_jenis(2),
        filter_col(2, pdrb_selectize("dasar_harga_lq_tabel", "Dasar Harga", choices = c("ADHK" = "ADHK", "ADHB" = "ADHB"), selected = selected_lq_dasar, placeholder = "Pilih dasar harga")),
        control_level(2),
        control_format(2),
        filter_col(2, pdrb_selectize("tahun_lq_tabel", "Tahun", choices = lq_year_choices, selected = keep_selected(input$tahun_lq_tabel, lq_year_choices, "__ALL__"), placeholder = "Pilih tahun")),
        filter_col(2, pdrb_selectize("periode_lq_tabel", "Periode", choices = lq_period_choices, selected = keep_selected(input$periode_lq_tabel, lq_period_choices, "__ALL__"), placeholder = "Pilih periode")),
        filter_col(2, pdrb_selectize("status_lq_tabel", "Status LQ", choices = c("Semua" = "semua", "Basis" = "Basis", "Sama" = "Sama", "Nonbasis" = "Nonbasis"), selected = keep_selected(input$status_lq_tabel, c("Semua" = "semua", "Basis" = "Basis", "Sama" = "Sama", "Nonbasis" = "Nonbasis"), "semua"), placeholder = "Pilih status"))
      ))
    }
    
    if (identical(jenis, "dlq")) {
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row",
        control_jenis(2),
        control_format(2),
        control_level(2),
        filter_col(2, pdrb_selectize("tahun_awal_dlq_tabel", "Tahun Awal", choices = dlq_year_choices, selected = keep_selected(input$tahun_awal_dlq_tabel, dlq_year_choices, selected_dlq_year_min), placeholder = "Pilih tahun awal")),
        filter_col(2, pdrb_selectize("tahun_akhir_dlq_tabel", "Tahun Akhir", choices = dlq_end_choices, selected = keep_selected(input$tahun_akhir_dlq_tabel, dlq_end_choices, selected_dlq_year_max), placeholder = "Pilih tahun akhir"))
      ))
    }
    
    if (jenis %in% c("shift_share", "shift_share_biasa", "shift_share_extended")) {
      return(fluidRow(
        class = "tabel-filter-grid tabel-filter-one-row tabel-filter-shift",
        control_jenis(2),
        control_format(2),
        control_level(2),
        filter_col(3, pdrb_selectize("tahun_awal_shift_tabel", "Tahun Awal", choices = shift_start_choices, selected = keep_selected(input$tahun_awal_shift_tabel, shift_start_choices, selected_shift_start), placeholder = "Pilih tahun awal")),
        filter_col(3, pdrb_selectize("tahun_akhir_shift_tabel", "Tahun Akhir", choices = shift_end_choices, selected = keep_selected(input$tahun_akhir_shift_tabel, shift_end_choices, selected_shift_end), placeholder = "Pilih tahun akhir"))
      ))
    }
    
    fluidRow(
      class = "tabel-filter-grid tabel-filter-one-row",
      control_jenis(2),
      control_dasar(2),
      control_tahun(2),
      control_periode(2),
      control_format(2),
      control_level(2)
    )
  })
  
  output$tabel_filter_dinamis_ui <- renderUI({
    NULL
  })
  
  # Sumber tunggal tabel dan unduhan: simpan nilai numerik tanpa round().
  # Tampilan browser dibatasi empat desimal melalui DT::formatRound().
  table_data_result <- reactive({
    req(input$jenis_tabel_data, input$kelompok, input$wilayah)
    jenis_tabel <- input$jenis_tabel_data
    if (identical(jenis_tabel, "audit")) jenis_tabel <- "pdrb"
    if (identical(jenis_tabel, "audit")) {
      audit_type <- if (is.null(input$jenis_audit_tabel)) "validasi_data_dasar" else as.character(input$jenis_audit_tabel)[1]
      jenis_tabel <- if (identical(audit_type, "validasi_data_dasar")) "validasi_data_dasar" else "validasi_turunan"
    }
    level_pilih <- if (is.null(input$level_tabel)) "Semua" else input$level_tabel
    cakupan <- "semua"
    dasar_harga_input <- ifelse(is.null(input$dasar_harga_tabel), "ADHK", input$dasar_harga_tabel)
    jenis_pertumbuhan_input <- ifelse(is.null(input$jenis_pertumbuhan_tabel), "Y-on-Y", input$jenis_pertumbuhan_tabel)
    format_tabel <- ifelse(is.null(input$format_tabel_data), "lebar", input$format_tabel_data)
    
    level_dipakai <- if (identical(level_pilih, "Semua")) {
      c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian")
    } else {
      level_pilih
    }
    
    current_region_info <- function() {
      info <- pdrb_data() %>%
        filter(kode_kelompok == input$kelompok, kode_wilayah == input$wilayah) %>%
        distinct(kode_kelompok, kelompok, kode_wilayah, wilayah) %>%
        slice(1)
      if (nrow(info) == 0) {
        tibble::tibble(
          kode_kelompok = input$kelompok,
          kelompok = input$kelompok,
          kode_wilayah = input$wilayah,
          wilayah = input$wilayah
        )
      } else {
        info
      }
    }
    
    add_tabel_sort_columns <- function(data) {
      if (is.null(data) || nrow(data) == 0) return(data)
      if (!"level" %in% names(data)) data$level <- NA_character_
      if (!"kode_kategori" %in% names(data)) data$kode_kategori <- NA_character_
      if (!"kategori_label" %in% names(data)) data$kategori_label <- NA_character_
      if (!"source_row" %in% names(data)) data$source_row <- NA_integer_
      if (!"kode_utama" %in% names(data)) data$kode_utama <- NA_character_
      
      main_rank <- function(code_value) {
        first_code <- stringr::str_extract(stringr::str_to_upper(as.character(code_value)), "[A-U]")
        match(first_code, LETTERS[1:21])
      }
      
      data %>%
        mutate(
          .tabel_level_chr = as.character(level),
          .tabel_code_chr = stringr::str_to_upper(stringr::str_squish(as.character(kode_kategori))),
          .tabel_label_chr = stringr::str_to_upper(stringr::str_squish(as.character(kategori_label))),
          .tabel_is_total = .tabel_level_chr == "Total PDRB" |
            .tabel_code_chr %in% c("PDRB", "PDRB_TANPA_MIGAS", "PDRB_NON_MIGAS") |
            stringr::str_detect(.tabel_label_chr, "PRODUK DOMESTIK REGIONAL BRUTO|(^|[^A-Z])PDRB([^A-Z]|$)"),
          .tabel_total_rank = dplyr::case_when(
            .tabel_code_chr == "PDRB" | stringr::str_detect(.tabel_label_chr, "PRODUK DOMESTIK REGIONAL BRUTO$") ~ 9900000,
            stringr::str_detect(.tabel_code_chr, "TANPA_MIGAS|NON_MIGAS") | stringr::str_detect(.tabel_label_chr, "TANPA MIGAS|NON MIGAS") ~ 9900001,
            TRUE ~ 9900099
          ),
          .tabel_main_code = dplyr::coalesce(as.character(kode_utama), stringr::str_extract(.tabel_code_chr, "^[A-U](?:,[A-U])?")),
          .tabel_main_rank = main_rank(.tabel_main_code),
          .tabel_sub_rank = suppressWarnings(as.integer(stringr::str_match(.tabel_code_chr, "\\.([0-9]+)")[, 2])),
          .tabel_detail_code = stringr::str_match(stringr::str_to_lower(as.character(kode_kategori)), "\\.[0-9]+\\.([a-z])")[, 2],
          .tabel_detail_rank = match(.tabel_detail_code, letters),
          .tabel_level_rank = dplyr::case_when(
            .tabel_level_chr == "Kategori Utama" ~ 0L,
            .tabel_level_chr == "Subkategori" ~ 1L,
            .tabel_level_chr == "Rincian" ~ 2L,
            TRUE ~ 9L
          ),
          .tabel_code_order = dplyr::coalesce(.tabel_main_rank, 99L) * 100000 +
            dplyr::coalesce(.tabel_sub_rank, 0L) * 1000 +
            .tabel_level_rank * 100 +
            dplyr::coalesce(.tabel_detail_rank, 0L),
          .tabel_sort_order = dplyr::case_when(
            .tabel_is_total ~ as.numeric(.tabel_total_rank),
            !is.na(.tabel_main_rank) ~ as.numeric(.tabel_code_order),
            !is.na(source_row) ~ as.numeric(source_row),
            TRUE ~ as.numeric(dplyr::row_number())
          )
        )
    }
    
    arrange_tabel_rows <- function(data, time_columns = TRUE) {
      if (is.null(data) || nrow(data) == 0) return(data)
      data <- add_tabel_sort_columns(data)
      if (time_columns && all(c("tahun", "periode") %in% names(data))) {
        data <- data %>%
          mutate(.tabel_periode_sort = match(as.character(periode), c("I", "II", "III", "IV", "Total"))) %>%
          arrange(.tabel_sort_order, tahun, .tabel_periode_sort)
      } else {
        data <- data %>% arrange(.tabel_sort_order)
      }
      data
    }
    
    
    arrange_output_table_rows <- function(data) {
      if (is.null(data) || nrow(data) == 0) return(data)
      if (!"Kode Kategori" %in% names(data)) return(data)
      
      main_rank_output <- function(code_value) {
        first_code <- stringr::str_extract(stringr::str_to_upper(as.character(code_value)), "[A-U]")
        match(first_code, LETTERS[1:21])
      }
      
      data %>%
        mutate(
          .out_code_chr = stringr::str_to_upper(stringr::str_squish(as.character(`Kode Kategori`))),
          .out_label_chr = if ("Kategori" %in% names(.)) stringr::str_to_upper(stringr::str_squish(as.character(Kategori))) else "",
          .out_level_chr = if ("Level" %in% names(.)) as.character(Level) else NA_character_,
          .out_is_total = .out_level_chr == "Total PDRB" |
            .out_code_chr %in% c("PDRB", "PDRB_TANPA_MIGAS", "PDRB_NON_MIGAS") |
            stringr::str_detect(.out_label_chr, "^PRODUK DOMESTIK REGIONAL BRUTO( TANPA MIGAS| NON MIGAS)?$"),
          .out_total_rank = dplyr::case_when(
            .out_code_chr == "PDRB" |
              stringr::str_detect(.out_label_chr, "^PRODUK DOMESTIK REGIONAL BRUTO$") ~ 9900000,
            stringr::str_detect(.out_code_chr, "TANPA_MIGAS|NON_MIGAS") |
              stringr::str_detect(.out_label_chr, "TANPA MIGAS|NON MIGAS") ~ 9900001,
            TRUE ~ 9900099
          ),
          .out_main_rank = main_rank_output(.out_code_chr),
          .out_sub_rank = suppressWarnings(as.integer(stringr::str_match(.out_code_chr, "\\.([0-9]+)")[, 2])),
          .out_detail_code = stringr::str_match(stringr::str_to_lower(.out_code_chr), "\\.[0-9]+\\.([a-z])")[, 2],
          .out_detail_rank = match(.out_detail_code, letters),
          .out_level_rank = dplyr::case_when(
            .out_level_chr == "Kategori Utama" ~ 0L,
            .out_level_chr == "Subkategori" ~ 1L,
            .out_level_chr == "Rincian" ~ 2L,
            TRUE ~ 9L
          ),
          .out_sort_order = dplyr::case_when(
            .out_is_total ~ as.numeric(.out_total_rank),
            !is.na(.out_main_rank) ~ as.numeric(
              dplyr::coalesce(.out_main_rank, 99L) * 100000 +
                dplyr::coalesce(.out_sub_rank, 0L) * 1000 +
                .out_level_rank * 100 +
                dplyr::coalesce(.out_detail_rank, 0L)
            ),
            TRUE ~ as.numeric(dplyr::row_number()) + 9800000
          )
        ) %>%
        arrange(.out_sort_order) %>%
        select(-dplyr::starts_with(".out_"))
    }
    
    apply_time_filter <- function(data) {
      tahun_pilih <- as.character(input$tahun_tabel)[1]
      periode_pilih <- as.character(input$periode_tabel)[1]
      
      if (!is.null(tahun_pilih) && length(tahun_pilih) > 0 && !is.na(tahun_pilih) && nzchar(tahun_pilih) && !identical(tahun_pilih, "__ALL__")) {
        data <- data %>% filter(tahun == as.integer(tahun_pilih))
      }
      if (!is.null(periode_pilih) && length(periode_pilih) > 0 && !is.na(periode_pilih) && nzchar(periode_pilih)) {
        if (identical(periode_pilih, "__QUARTERS__")) {
          data <- data %>% filter(as.character(periode) %in% c("I", "II", "III", "IV"))
        } else if (!identical(periode_pilih, "__ALL__")) {
          data <- data %>% filter(as.character(periode) == periode_pilih)
        }
      }
      data
    }
    
    apply_category_filter <- function(data) {
      data <- data %>% mutate(level = as.character(level))
      data %>% filter(level %in% level_dipakai)
    }
    
    # Fallback input untuk tabel LQ/DLQ/Extended Shift Share.
    # Saat jenis tabel diganti, Shiny sempat menghapus dan membuat ulang input dinamis.
    # Fungsi ini menjaga tabel tetap menggunakan tahun/periode default yang valid.
    available_table_years <- function() {
      data_year <- pdrb_data()
      if (is.null(data_year) || nrow(data_year) == 0 || !"tahun" %in% names(data_year)) return(integer(0))
      years <- sort(unique(suppressWarnings(as.integer(data_year$tahun))))
      years[!is.na(years)]
    }
    
    pick_year_input <- function(value, default = c("max", "min"), shift_ready = FALSE) {
      default <- match.arg(default)
      years <- available_table_years()
      if (shift_ready) {
        years_shift <- years[(years - 1L) %in% years]
        if (length(years_shift) > 0) years <- years_shift
      }
      if (length(years) == 0) return(NA_integer_)
      value <- suppressWarnings(as.integer(as.character(value)[1]))
      if (!is.na(value) && value %in% years) return(value)
      if (identical(default, "min")) min(years, na.rm = TRUE) else max(years, na.rm = TRUE)
    }
    
    pick_period_input <- function(value, default = "Total") {
      value <- as.character(value)[1]
      allowed <- c("I", "II", "III", "IV", "Total")
      if (!is.null(value) && length(value) > 0 && !is.na(value) && value %in% allowed) value else default
    }
    
    make_time_column <- function(tahun, periode_label_value) {
      tahun <- as.character(tahun)
      periode_label_value <- as.character(periode_label_value)
      dplyr::case_when(
        periode_label_value %in% c("Triwulan I", "I") ~ paste(tahun, "I"),
        periode_label_value %in% c("Triwulan II", "II") ~ paste(tahun, "II"),
        periode_label_value %in% c("Triwulan III", "III") ~ paste(tahun, "III"),
        periode_label_value %in% c("Triwulan IV", "IV") ~ paste(tahun, "IV"),
        periode_label_value %in% c("Total/Kumulatif", "Tahun", "Total", "Tahunan") ~ paste(tahun, "Total"),
        TRUE ~ paste(tahun, periode_label_value)
      )
    }
    
    order_time_columns <- function(cols) {
      if (length(cols) == 0) return(cols)
      sort_key <- function(x) {
        tahun <- suppressWarnings(as.integer(stringr::str_extract(x, "(19|20)[0-9]{2}")))
        periode_urut <- dplyr::case_when(
          stringr::str_detect(x, "\bI$") ~ 1L,
          stringr::str_detect(x, "\bII$") ~ 2L,
          stringr::str_detect(x, "\bIII$") ~ 3L,
          stringr::str_detect(x, "\bIV$") ~ 4L,
          stringr::str_detect(x, "\bTotal$") ~ 5L,
          stringr::str_detect(x, "^Q1-") ~ 1L,
          stringr::str_detect(x, "^Q2-") ~ 2L,
          stringr::str_detect(x, "^Q3-") ~ 3L,
          stringr::str_detect(x, "^Q4-") ~ 4L,
          stringr::str_detect(x, "^(19|20)[0-9]{2}$") ~ 5L,
          TRUE ~ 9L
        )
        sprintf("%04d_%02d_%s", ifelse(is.na(tahun), 9999L, tahun), periode_urut, x)
      }
      cols[order(vapply(cols, sort_key, character(1)))]
    }
    
    reorder_table_columns <- function(data) {
      preferred <- c(
        "Kategori", "Kode Kategori", "Level", "Indikator", "Dasar Harga", "Satuan",
        "Provinsi", "Wilayah", "Wilayah Pembanding", "Tahun", "Periode", "Waktu",
        "Periode Perhitungan", "Periode Pembanding", "Tahun Awal", "Periode Awal", "Tahun Akhir", "Periode Akhir",
        "Jenis Validasi", "Variabel", "Komponen", "Komponen 1", "Nilai Komponen 1", "Komponen 2", "Nilai Komponen 2", "Denominator", "Rumus", "Nilai", "Hasil Hitung Ulang", "Hasil Dashboard", "Selisih"
      )
      data %>% select(any_of(preferred), everything())
    }
    
    make_metric_long_table <- function(data, metric_cols, variable_name = "Variabel", value_name = "Nilai") {
      metric_cols <- intersect(metric_cols, names(data))
      if (!identical(format_tabel, "panjang") || length(metric_cols) == 0L) return(data)
      data %>%
        tidyr::pivot_longer(
          cols = all_of(metric_cols),
          names_to = variable_name,
          values_to = value_name
        ) %>%
        reorder_table_columns()
    }
    
    order_metric_time_columns <- function(cols, metrics) {
      if (length(cols) == 0L) return(cols)
      strip_metric_prefix <- function(col_name) {
        for (metric in metrics) {
          prefix <- paste0(metric, " ")
          if (startsWith(col_name, prefix)) {
            return(substr(col_name, nchar(prefix) + 1L, nchar(col_name)))
          }
        }
        col_name
      }
      time_labels <- unique(vapply(cols, strip_metric_prefix, character(1)))
      ordered_times <- order_time_columns(time_labels)
      unlist(lapply(ordered_times, function(time_label) {
        wanted <- paste(metrics, time_label)
        wanted[wanted %in% cols]
      }), use.names = FALSE)
    }
    
    make_wide_table <- function(data, jenis_tabel) {
      if (!identical(format_tabel, "lebar")) {
        if (identical(jenis_tabel, "dlq")) {
          return(make_metric_long_table(
            data,
            c(
              "Nilai Wilayah Awal", "Nilai Wilayah Akhir", "Nilai Pembanding Awal", "Nilai Pembanding Akhir",
              "Location Quotient (LQ) Awal", "Location Quotient (LQ) Akhir",
              "Pertumbuhan Sektor Wilayah (%)", "Pertumbuhan Total Wilayah (%)",
              "Pertumbuhan Sektor Pembanding (%)", "Pertumbuhan Total Pembanding (%)", "Dynamic Location Quotient (DLQ)"
            ),
            variable_name = "Variabel",
            value_name = "Nilai"
          ))
        }
        if (jenis_tabel %in% c("shift_share_biasa", "shift_share_extended", "shift_share")) {
          return(make_metric_long_table(
            data,
            c("NE", "IM", "CE", "RIE", "RSE", "RCCE", "Total Perubahan"),
            variable_name = "Komponen",
            value_name = "Nilai"
          ))
        }
        return(reorder_table_columns(data))
      }
      
      if (jenis_tabel %in% c("pdrb", "distribusi", "pertumbuhan", "indeks", "laju_indeks", "sumber_pertumbuhan")) {
        if (!all(c("Tahun", "Periode", "Nilai") %in% names(data))) return(reorder_table_columns(data))
        
        base_cols <- intersect(
          c("Kategori", "Kode Kategori", "Level", "Indikator", "Dasar Harga", "Satuan", "Provinsi", "Wilayah", "Wilayah Pembanding"),
          names(data)
        )
        
        wide_data <- data %>%
          mutate(`Kolom Waktu` = make_time_column(Tahun, Periode)) %>%
          select(all_of(base_cols), `Kolom Waktu`, .nilai_tabel = Nilai) %>%
          group_by(across(all_of(c(base_cols, "Kolom Waktu")))) %>%
          summarise(.nilai_tabel = dplyr::first(.nilai_tabel), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = `Kolom Waktu`, values_from = .nilai_tabel)
        
        time_cols <- setdiff(names(wide_data), base_cols)
        return(wide_data %>% select(any_of(base_cols), any_of(order_time_columns(time_cols))))
      }
      
      if (jenis_tabel %in% c("lq", "lq_adhb", "lq_adhk")) {
        if (!all(c("Tahun", "Periode", "LQ") %in% names(data))) return(reorder_table_columns(data))
        
        base_cols <- intersect(
          c("Kategori", "Kode Kategori", "Level", "Indikator", "Dasar Harga", "Satuan", "Provinsi", "Wilayah", "Wilayah Pembanding"),
          names(data)
        )
        value_cols <- intersect(c("LQ", "Location Quotient (LQ)", "Status Location Quotient (LQ)"), names(data))
        
        wide_data <- data %>%
          mutate(.kolom_waktu = make_time_column(Tahun, Periode)) %>%
          select(all_of(base_cols), .kolom_waktu, all_of(value_cols)) %>%
          group_by(across(all_of(c(base_cols, ".kolom_waktu")))) %>%
          summarise(across(all_of(value_cols), dplyr::first), .groups = "drop") %>%
          tidyr::pivot_wider(
            names_from = .kolom_waktu,
            values_from = all_of(value_cols),
            names_glue = "{.value} {.kolom_waktu}"
          )
        
        time_cols <- setdiff(names(wide_data), base_cols)
        ordered_cols <- order_metric_time_columns(time_cols, value_cols)
        return(wide_data %>% select(any_of(base_cols), any_of(ordered_cols), any_of(setdiff(time_cols, ordered_cols))))
      }
      
      # DLQ dan Extended Shift Share secara natural sudah berbentuk lebar: kategori sebagai baris,
      # sedangkan variabel/komponen analisis menjadi kolom.
      reorder_table_columns(data)
    }
    
    if (jenis_tabel == "validasi_data_dasar") {
      data <- pdrb_data() %>%
        mutate(level = as.character(level), periode = as.character(periode)) %>%
        filter(
          kode_kelompok == input$kelompok,
          kode_wilayah == input$wilayah,
          indikator %in% c("PDRB ADHB", "PDRB ADHK")
        ) %>%
        apply_category_filter() %>%
        apply_time_filter() %>%
        add_time_columns() %>%
        arrange(level, source_row, indikator, waktu_index) %>%
        transmute(
          Provinsi = kelompok,
          Wilayah = wilayah,
          `Jenis Validasi` = "Data dasar ADHB/ADHK",
          Indikator = indikator,
          `Dasar Harga` = if_else(indikator == "PDRB ADHB", "ADHB", "ADHK"),
          Satuan = satuan,
          Level = level,
          `Kode Kategori` = kode_kategori,
          Kategori = kategori_label,
          Tahun = as.character(tahun),
          Periode = period_label(periode),
          Waktu = waktu,
          `Nilai Mentah Dashboard` = nilai,
          `Satuan Mentah` = "Juta Rupiah",
          `Sumber Baris` = source_file,
          `Baris Sumber` = source_row,
          `Catatan Validasi` = "Samakan nilai ini dengan angka PDRB ADHB/ADHK di Excel terlebih dahulu. Jika nilai mentah sudah berbeda, semua indikator turunan akan ikut berbeda."
        )
      
    } else if (jenis_tabel == "validasi_turunan") {
      jenis_validasi <- if (!is.null(input$jenis_audit_tabel) && !identical(input$jenis_audit_tabel, "validasi_data_dasar")) input$jenis_audit_tabel else ifelse(is.null(input$jenis_validasi_turunan), "pertumbuhan", input$jenis_validasi_turunan)
      dasar_harga_validasi <- ifelse(is.null(input$dasar_harga_tabel), "ADHK", input$dasar_harga_tabel)
      metode_validasi <- ifelse(is.null(input$jenis_pertumbuhan_tabel), "Y-on-Y", input$jenis_pertumbuhan_tabel)
      
      raw_validasi <- pdrb_data() %>%
        mutate(level = as.character(level), periode = as.character(periode)) %>%
        filter(
          kode_kelompok == input$kelompok,
          kode_wilayah == input$wilayah,
          indikator %in% c("PDRB ADHB", "PDRB ADHK"),
          !is.na(nilai)
        )
      
      stored_result <- function(indikator_name) {
        pdrb_data() %>%
          mutate(periode = as.character(periode)) %>%
          filter(
            kode_kelompok == input$kelompok,
            kode_wilayah == input$wilayah,
            indikator == indikator_name
          ) %>%
          group_by(item_id, tahun, periode) %>%
          summarise(`Hasil Dashboard` = dplyr::first(nilai), .groups = "drop")
      }
      
      period_for_join <- function(x) {
        x %>% mutate(periode = as.character(periode))
      }
      
      if (identical(jenis_validasi, "distribusi")) {
        indikator_validasi <- "Distribusi PDRB"
        total_adhb <- raw_validasi %>%
          filter(indikator == "PDRB ADHB", level == "Total PDRB", kode_kategori == "PDRB") %>%
          transmute(kode_wilayah, tahun, periode, total_pdrb_adhb = nilai)
        
        data <- raw_validasi %>%
          filter(indikator == "PDRB ADHB", level != "Total PDRB") %>%
          left_join(total_adhb, by = c("kode_wilayah", "tahun", "periode")) %>%
          left_join(stored_result(indikator_validasi), by = c("item_id", "tahun", "periode")) %>%
          mutate(
            `Jenis Validasi` = "Distribusi PDRB",
            `Dasar Harga Validasi` = "ADHB",
            `Periode Pembanding` = "Total PDRB wilayah pada periode yang sama",
            `Komponen 1` = "Nilai sektor ADHB",
            `Nilai Komponen 1` = nilai,
            `Komponen 2` = "Total PDRB ADHB wilayah",
            `Nilai Komponen 2` = total_pdrb_adhb,
            Denominator = total_pdrb_adhb,
            Rumus = "Nilai sektor ADHB / Total PDRB ADHB wilayah × 100",
            `Hasil Hitung Ulang` = if_else(!is.na(total_pdrb_adhb) & total_pdrb_adhb != 0, nilai / total_pdrb_adhb * 100, NA_real_),
            Satuan = "Persen"
          )
        
      } else if (identical(jenis_validasi, "indeks_implisit")) {
        indikator_validasi <- "Indeks Implisit"
        key_cols <- intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id", "tahun", "periode"), names(raw_validasi))
        wide <- raw_validasi %>%
          select(all_of(key_cols), indikator, nilai) %>%
          group_by(across(all_of(c(key_cols, "indikator")))) %>%
          summarise(nilai = mean(nilai, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = indikator, values_from = nilai)
        
        data <- wide %>%
          left_join(stored_result(indikator_validasi), by = c("item_id", "tahun", "periode")) %>%
          mutate(
            `Jenis Validasi` = "Indeks Implisit",
            `Dasar Harga Validasi` = "ADHB dan ADHK",
            `Periode Pembanding` = "ADHB dan ADHK pada periode yang sama",
            `Komponen 1` = "PDRB ADHB",
            `Nilai Komponen 1` = `PDRB ADHB`,
            `Komponen 2` = "PDRB ADHK",
            `Nilai Komponen 2` = `PDRB ADHK`,
            Denominator = `PDRB ADHK`,
            Rumus = "PDRB ADHB / PDRB ADHK × 100",
            `Hasil Hitung Ulang` = if_else(!is.na(`PDRB ADHB`) & !is.na(`PDRB ADHK`) & `PDRB ADHK` != 0, `PDRB ADHB` / `PDRB ADHK` * 100, NA_real_),
            Satuan = "Indeks"
          )
        
      } else if (identical(jenis_validasi, "laju_implisit")) {
        indikator_validasi <- "Laju Indeks Implisit Q-to-Q"
        key_cols <- intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id", "tahun", "periode"), names(raw_validasi))
        group_cols <- setdiff(key_cols, c("tahun", "periode"))
        idx <- raw_validasi %>%
          select(all_of(key_cols), indikator, nilai) %>%
          group_by(across(all_of(c(key_cols, "indikator")))) %>%
          summarise(nilai = mean(nilai, na.rm = TRUE), .groups = "drop") %>%
          tidyr::pivot_wider(names_from = indikator, values_from = nilai) %>%
          mutate(
            indeks_implisit = if_else(!is.na(`PDRB ADHB`) & !is.na(`PDRB ADHK`) & `PDRB ADHK` != 0, `PDRB ADHB` / `PDRB ADHK` * 100, NA_real_),
            periode_urut = match(periode, c("I", "II", "III", "IV", "Total")),
            idx_waktu = tahun * 4L + periode_urut
          ) %>%
          filter(periode %in% c("I", "II", "III", "IV")) %>%
          group_by(across(all_of(group_cols))) %>%
          arrange(idx_waktu, .by_group = TRUE) %>%
          mutate(
            indeks_pembanding = lag(indeks_implisit),
            idx_pembanding = lag(idx_waktu),
            tahun_pembanding = lag(tahun),
            periode_pembanding = lag(periode)
          ) %>%
          ungroup()
        
        data <- idx %>%
          left_join(stored_result(indikator_validasi), by = c("item_id", "tahun", "periode")) %>%
          mutate(
            `Jenis Validasi` = "Laju Indeks Implisit Q-to-Q",
            `Dasar Harga Validasi` = "ADHB dan ADHK",
            `Periode Pembanding` = paste0(period_label(periode_pembanding), " ", tahun_pembanding),
            `Komponen 1` = "Indeks implisit periode sekarang",
            `Nilai Komponen 1` = indeks_implisit,
            `Komponen 2` = "Indeks implisit periode sebelumnya",
            `Nilai Komponen 2` = indeks_pembanding,
            Denominator = indeks_pembanding,
            Rumus = "(Indeks implisit sekarang / Indeks implisit sebelumnya - 1) × 100",
            `Hasil Hitung Ulang` = if_else(!is.na(indeks_pembanding) & idx_waktu - idx_pembanding == 1L & indeks_pembanding != 0, (indeks_implisit / indeks_pembanding - 1) * 100, NA_real_),
            Satuan = "Persen"
          )
        
      } else if (identical(jenis_validasi, "pertumbuhan")) {
        indikator_validasi <- paste("Pertumbuhan", dasar_harga_validasi, metode_validasi)
        raw_dasar <- raw_validasi %>%
          filter(indikator == paste("PDRB", dasar_harga_validasi)) %>%
          mutate(periode_urut = match(periode, c("I", "II", "III", "IV", "Total")))
        group_cols <- intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id"), names(raw_dasar))
        quarters <- raw_dasar %>% filter(periode %in% c("I", "II", "III", "IV")) %>% mutate(idx_waktu = tahun * 4L + periode_urut)
        totals <- raw_dasar %>% filter(periode == "Total")
        
        if (identical(metode_validasi, "Q-to-Q")) {
          data <- quarters %>%
            group_by(across(all_of(group_cols))) %>%
            arrange(idx_waktu, .by_group = TRUE) %>%
            mutate(
              nilai_pembanding = lag(nilai),
              idx_pembanding = lag(idx_waktu),
              tahun_pembanding = lag(tahun),
              periode_pembanding = lag(periode),
              nilai_sekarang_validasi = nilai
            ) %>%
            ungroup()
        } else if (identical(metode_validasi, "Y-on-Y")) {
          data <- bind_rows(
            quarters %>%
              group_by(across(all_of(group_cols)), periode) %>%
              arrange(tahun, .by_group = TRUE) %>%
              mutate(nilai_pembanding = lag(nilai), tahun_pembanding = lag(tahun), periode_pembanding = lag(periode), nilai_sekarang_validasi = nilai) %>%
              ungroup(),
            totals %>%
              group_by(across(all_of(group_cols))) %>%
              arrange(tahun, .by_group = TRUE) %>%
              mutate(nilai_pembanding = lag(nilai), tahun_pembanding = lag(tahun), periode_pembanding = lag(periode), nilai_sekarang_validasi = nilai) %>%
              ungroup()
          )
        } else {
          data <- bind_rows(
            quarters %>%
              group_by(across(all_of(group_cols)), tahun) %>%
              arrange(periode_urut, .by_group = TRUE) %>%
              mutate(nilai_kumulatif = cumsum(nilai)) %>%
              ungroup() %>%
              group_by(across(all_of(group_cols)), periode) %>%
              arrange(tahun, .by_group = TRUE) %>%
              mutate(nilai_pembanding = lag(nilai_kumulatif), tahun_pembanding = lag(tahun), periode_pembanding = lag(periode), nilai_sekarang_validasi = nilai_kumulatif) %>%
              ungroup(),
            totals %>%
              group_by(across(all_of(group_cols))) %>%
              arrange(tahun, .by_group = TRUE) %>%
              mutate(nilai_pembanding = lag(nilai), tahun_pembanding = lag(tahun), periode_pembanding = lag(periode), nilai_sekarang_validasi = nilai) %>%
              ungroup()
          )
        }
        
        data <- data %>%
          left_join(stored_result(indikator_validasi), by = c("item_id", "tahun", "periode")) %>%
          mutate(
            `Jenis Validasi` = paste("Pertumbuhan", metode_validasi),
            `Dasar Harga Validasi` = dasar_harga_validasi,
            `Periode Pembanding` = paste0(period_label(periode_pembanding), " ", tahun_pembanding),
            `Komponen 1` = if_else(metode_validasi == "C-to-C", "Nilai kumulatif sekarang", "Nilai periode sekarang"),
            `Nilai Komponen 1` = nilai_sekarang_validasi,
            `Komponen 2` = if_else(metode_validasi == "C-to-C", "Nilai kumulatif pembanding", "Nilai periode pembanding"),
            `Nilai Komponen 2` = nilai_pembanding,
            Denominator = nilai_pembanding,
            Rumus = if_else(metode_validasi == "C-to-C", "(Kumulatif sekarang / kumulatif pembanding - 1) × 100", "(Nilai sekarang / nilai pembanding - 1) × 100"),
            `Hasil Hitung Ulang` = if_else(!is.na(nilai_pembanding) & nilai_pembanding != 0, (nilai_sekarang_validasi / nilai_pembanding - 1) * 100, NA_real_),
            Satuan = "Persen"
          )
        
      } else {
        indikator_validasi <- paste("Sumber Pertumbuhan", dasar_harga_validasi, metode_validasi)
        raw_dasar <- raw_validasi %>%
          filter(indikator == paste("PDRB", dasar_harga_validasi)) %>%
          mutate(periode_urut = match(periode, c("I", "II", "III", "IV", "Total")))
        sectors <- raw_dasar %>% filter(level != "Total PDRB")
        total <- raw_dasar %>% filter(level == "Total PDRB", kode_kategori == "PDRB") %>% select(kode_wilayah, tahun, periode, total_pdrb = nilai)
        group_cols <- intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id"), names(sectors))
        quarters <- sectors %>% filter(periode %in% c("I", "II", "III", "IV")) %>% mutate(idx_waktu = tahun * 4L + periode_urut)
        total_q <- total %>% filter(periode %in% c("I", "II", "III", "IV")) %>% mutate(idx_waktu = tahun * 4L + match(periode, c("I", "II", "III", "IV")))
        
        if (identical(metode_validasi, "Q-to-Q")) {
          data <- quarters %>%
            group_by(across(all_of(group_cols))) %>%
            arrange(idx_waktu, .by_group = TRUE) %>%
            mutate(nilai_pembanding = lag(nilai), idx_pembanding = lag(idx_waktu), tahun_pembanding = lag(tahun), periode_pembanding = lag(periode), nilai_sekarang_validasi = nilai) %>%
            ungroup() %>%
            left_join(total_q %>% select(kode_wilayah, idx_waktu, total_pdrb_pembanding = total_pdrb) %>% mutate(idx_waktu = idx_waktu + 1L), by = c("kode_wilayah", "idx_waktu"))
        } else if (identical(metode_validasi, "Y-on-Y")) {
          data <- quarters %>%
            group_by(across(all_of(group_cols)), periode) %>%
            arrange(tahun, .by_group = TRUE) %>%
            mutate(nilai_pembanding = lag(nilai), tahun_pembanding = lag(tahun), periode_pembanding = lag(periode), nilai_sekarang_validasi = nilai) %>%
            ungroup() %>%
            left_join(total %>% select(kode_wilayah, tahun, periode, total_pdrb_pembanding = total_pdrb) %>% mutate(tahun = tahun + 1L), by = c("kode_wilayah", "tahun", "periode"))
        } else {
          data <- quarters %>%
            group_by(across(all_of(group_cols)), tahun) %>%
            arrange(periode_urut, .by_group = TRUE) %>%
            mutate(nilai_kumulatif = cumsum(nilai)) %>%
            ungroup() %>%
            group_by(across(all_of(group_cols)), periode) %>%
            arrange(tahun, .by_group = TRUE) %>%
            mutate(nilai_pembanding = lag(nilai_kumulatif), tahun_pembanding = lag(tahun), periode_pembanding = lag(periode), nilai_sekarang_validasi = nilai_kumulatif) %>%
            ungroup() %>%
            left_join(
              total %>%
                filter(periode %in% c("I", "II", "III", "IV")) %>%
                group_by(kode_wilayah, tahun) %>%
                arrange(match(periode, c("I", "II", "III", "IV")), .by_group = TRUE) %>%
                mutate(total_kumulatif = cumsum(total_pdrb)) %>%
                ungroup() %>%
                select(kode_wilayah, tahun, periode, total_pdrb_pembanding = total_kumulatif) %>%
                mutate(tahun = tahun + 1L),
              by = c("kode_wilayah", "tahun", "periode")
            )
        }
        
        data <- data %>%
          left_join(stored_result(indikator_validasi), by = c("item_id", "tahun", "periode")) %>%
          mutate(
            `Jenis Validasi` = paste("Sumber Pertumbuhan", metode_validasi),
            `Dasar Harga Validasi` = dasar_harga_validasi,
            `Periode Pembanding` = paste0(period_label(periode_pembanding), " ", tahun_pembanding),
            `Komponen 1` = if_else(metode_validasi == "C-to-C", "Kumulatif sektor sekarang", "Nilai sektor sekarang"),
            `Nilai Komponen 1` = nilai_sekarang_validasi,
            `Komponen 2` = if_else(metode_validasi == "C-to-C", "Kumulatif sektor pembanding", "Nilai sektor pembanding"),
            `Nilai Komponen 2` = nilai_pembanding,
            Denominator = total_pdrb_pembanding,
            Rumus = "(Nilai sektor sekarang - nilai sektor pembanding) / total PDRB pembanding × 100",
            `Hasil Hitung Ulang` = if_else(!is.na(nilai_pembanding) & !is.na(total_pdrb_pembanding) & total_pdrb_pembanding != 0, (nilai_sekarang_validasi - nilai_pembanding) / total_pdrb_pembanding * 100, NA_real_),
            Satuan = "Persen Poin"
          )
      }
      
      data <- data %>%
        apply_category_filter() %>%
        apply_time_filter() %>%
        filter(!is.na(`Hasil Hitung Ulang`), is.finite(`Hasil Hitung Ulang`)) %>%
        arrange(level, kode_kategori, tahun, match(periode, c("I", "II", "III", "IV", "Total"))) %>%
        transmute(
          Provinsi = kelompok,
          Wilayah = wilayah,
          `Jenis Validasi` = `Jenis Validasi`,
          Indikator = indikator_validasi,
          `Dasar Harga` = `Dasar Harga Validasi`,
          Satuan = Satuan,
          Level = level,
          `Kode Kategori` = kode_kategori,
          Kategori = kategori_label,
          Tahun = as.character(tahun),
          Periode = period_label(periode),
          `Periode Pembanding` = `Periode Pembanding`,
          `Komponen 1` = `Komponen 1`,
          `Nilai Komponen 1` = `Nilai Komponen 1`,
          `Komponen 2` = `Komponen 2`,
          `Nilai Komponen 2` = `Nilai Komponen 2`,
          Denominator = Denominator,
          Rumus = Rumus,
          `Hasil Hitung Ulang` = `Hasil Hitung Ulang`,
          `Hasil Dashboard` = `Hasil Dashboard`,
          Selisih = `Hasil Hitung Ulang` - `Hasil Dashboard`,
          `Catatan Satuan` = if_else(Satuan %in% c("Persen", "Persen Poin", "Indeks"), Satuan, "Nilai komponen PDRB dalam Juta Rupiah")
        )
      
    } else if (jenis_tabel %in% c("pdrb", "distribusi", "pertumbuhan", "indeks", "laju_indeks", "sumber_pertumbuhan")) {
      jenis_laju_input <- ifelse(is.null(input$jenis_laju_tabel), "Y-on-Y", input$jenis_laju_tabel)
      indikator_pilih <- switch(
        jenis_tabel,
        "pdrb" = paste("PDRB", dasar_harga_input),
        "distribusi" = paste("PDRB", dasar_harga_input),
        "pertumbuhan" = paste("Pertumbuhan", dasar_harga_input, jenis_pertumbuhan_input),
        "indeks" = "Indeks Implisit",
        "laju_indeks" = paste("Laju Indeks Implisit", jenis_laju_input),
        "sumber_pertumbuhan" = paste("Sumber Pertumbuhan", dasar_harga_input, jenis_pertumbuhan_input)
      )
      
      if (identical(jenis_tabel, "distribusi")) {
        raw_distribusi <- pdrb_data() %>%
          mutate(level = as.character(level), periode = as.character(periode)) %>%
          filter(
            kode_kelompok == input$kelompok,
            kode_wilayah == input$wilayah,
            indikator == indikator_pilih,
            !is.na(nilai)
          )
        
        total_distribusi <- raw_distribusi %>%
          filter(level == "Total PDRB", kode_kategori == "PDRB") %>%
          select(kode_wilayah, tahun, periode, total_pdrb = nilai)
        
        data <- raw_distribusi %>%
          left_join(total_distribusi, by = c("kode_wilayah", "tahun", "periode")) %>%
          mutate(
            indikator = "Distribusi PDRB",
            satuan = "Persen",
            nilai = if_else(!is.na(total_pdrb) & total_pdrb != 0, nilai / total_pdrb * 100, NA_real_)
          ) %>%
          filter(!is.na(nilai), is.finite(nilai))
      } else {
        data <- pdrb_data() %>%
          mutate(level = as.character(level), periode = as.character(periode)) %>%
          filter(
            kode_kelompok == input$kelompok,
            kode_wilayah == input$wilayah,
            indikator == indikator_pilih
          )
      }
      
      data <- data %>%
        apply_category_filter() %>%
        apply_time_filter() %>%
        add_time_columns() %>%
        arrange_tabel_rows() %>%
        transmute(
          Provinsi = kelompok,
          Wilayah = wilayah,
          Indikator = indikator,
          `Dasar Harga` = case_when(
            identical(jenis_tabel, "distribusi") ~ dasar_harga_input,
            jenis_tabel %in% c("indeks", "laju_indeks") ~ "ADHB/ADHK",
            TRUE ~ dasar_harga_input
          ),
          Satuan = satuan,
          Level = level,
          `Kode Kategori` = kode_kategori,
          Kategori = kategori_label,
          Tahun = as.character(tahun),
          Periode = period_label(periode),
          Waktu = waktu,
          Nilai = nilai
        )
      
      data <- make_wide_table(data, jenis_tabel)
      
    } else if (jenis_tabel %in% c("lq", "lq_adhb", "lq_adhk")) {
      dasar_harga <- dplyr::case_when(
        identical(jenis_tabel, "lq_adhb") ~ "ADHB",
        identical(jenis_tabel, "lq_adhk") ~ "ADHK",
        TRUE ~ ifelse(is.null(input$dasar_harga_lq_tabel), "ADHK", as.character(input$dasar_harga_lq_tabel))
      )
      tahun_lq_input <- as.character(input$tahun_lq_tabel)[1]
      periode_lq_input <- as.character(input$periode_lq_tabel)[1]
      tahun_lq_valid <- pick_year_input(input$tahun_lq_tabel, default = "max")
      validate(need(!is.na(tahun_lq_valid) || identical(tahun_lq_input, "__ALL__"), "Data tahun LQ belum tersedia."))
      validate(need(reference_available_by_dasar_v5(dasar_harga), potential_region_message_v8(dasar_harga)))
      
      tahun_lq_tersedia <- tryCatch(valid_lq_years(), error = function(e) integer(0))
      validate(need(length(tahun_lq_tersedia) > 0,
                    "Data LQ tidak tersedia karena tahun data wilayah dan provinsi pembanding tidak beririsan atau komponennya belum lengkap."))

      data <- shared_lq_data_v90(
        data = pdrb_data(),
        kode_kelompok_pilih = input$kelompok,
        kode_wilayah_pilih = input$wilayah,
        level_pilih = normalize_lq_level(level_pilih),
        dasar_harga = dasar_harga
      ) %>%
        mutate(
          tahun = as.integer(tahun),
          periode = as.character(periode)
        ) %>%
        # Hilangkan seluruh tahun yang tidak mempunyai LQ valid pada pasangan wilayah-pembanding.
        filter(tahun %in% tahun_lq_tersedia)
      
      if (!identical(tahun_lq_input, "__ALL__")) {
        data <- data %>% filter(tahun == tahun_lq_valid)
      }
      if (identical(periode_lq_input, "__QUARTERS__")) {
        data <- data %>% filter(periode %in% c("I", "II", "III", "IV"))
      } else if (!identical(periode_lq_input, "__ALL__")) {
        periode_lq <- pick_period_input(input$periode_lq_tabel, default = "Total")
        data <- data %>% filter(periode == periode_lq)
      }
      
      status_lq <- if (is.null(input$status_lq_tabel)) "semua" else as.character(input$status_lq_tabel)
      if (!identical(status_lq, "semua")) {
        data <- data %>% filter(Keterangan == status_lq)
      }
      
      data <- data %>%
        add_time_columns() %>%
        arrange_tabel_rows() %>%
        transmute(
          Provinsi = kelompok,
          Wilayah = wilayah,
          `Wilayah Pembanding` = wilayah_pembanding,
          Indikator = paste("LQ", dasar_harga),
          Satuan = "Rasio",
          Level = level,
          `Kode Kategori` = kode_kategori,
          Kategori = kategori_label,
          `Dasar Harga` = dasar_harga,
          Tahun = as.character(tahun),
          Periode = period_label(periode),
          `Nilai Sektor Wilayah` = nilai_wilayah,
          `Total PDRB Wilayah` = total_wilayah,
          `Share Sektor Wilayah (%)` = share_wilayah,
          `Nilai Sektor Pembanding` = nilai_provinsi,
          `Total PDRB Pembanding` = total_provinsi,
          `Share Sektor Pembanding (%)` = share_pembanding,
          LQ = LQ,
          `Location Quotient (LQ)` = LQ,
          `Status Location Quotient (LQ)` = Keterangan,
          Interpretasi = case_when(
            is.na(LQ) ~ "LQ tidak dapat dihitung karena data pembanding tidak lengkap.",
            LQ > 1 ~ "Sektor basis: peran sektor lebih besar dibanding wilayah pembanding.",
            LQ < 1 ~ "Sektor nonbasis: peran sektor lebih kecil dibanding wilayah pembanding.",
            TRUE ~ "Sama dengan wilayah pembanding."
          )
        ) %>%
        make_wide_table("lq")
      
    } else if (jenis_tabel == "dlq") {
      dasar_harga_dlq <- "ADHK"
      periode_dlq <- "Total"
      tahun_awal_input <- as.character(input$tahun_awal_dlq_tabel)[1]
      tahun_akhir_input <- as.character(input$tahun_akhir_dlq_tabel)[1]
      semua_tahun_dlq <- is.na(tahun_awal_input) || !nzchar(tahun_awal_input) ||
        identical(tahun_awal_input, "__ALL__") || identical(tahun_akhir_input, "__ALL__")

      validate(need(reference_available_by_dasar_v5(dasar_harga_dlq), potential_region_message_v8(dasar_harga_dlq)))

      tahun_dlq_tersedia <- tryCatch(valid_dlq_years(), error = function(e) integer(0))
      tahun_dlq_tersedia <- sort(unique(as.integer(tahun_dlq_tersedia)))
      tahun_dlq_tersedia <- tahun_dlq_tersedia[!is.na(tahun_dlq_tersedia)]
      tahun_awal_valid <- tahun_dlq_tersedia[(tahun_dlq_tersedia + 1L) %in% tahun_dlq_tersedia]
      validate(need(length(tahun_awal_valid) > 0, "DLQ memerlukan minimal dua tahun berurutan yang tersedia pada wilayah dan provinsi pembanding."))

      if (semua_tahun_dlq) {
        pasangan_tahun_dlq <- tibble::tibble(
          tahun_awal = tahun_awal_valid,
          tahun_akhir = tahun_awal_valid + 1L
        )
      } else {
        tahun_awal <- suppressWarnings(as.integer(tahun_awal_input))
        tahun_akhir <- suppressWarnings(as.integer(tahun_akhir_input))
        validate(need(!is.na(tahun_awal) && !is.na(tahun_akhir), "Data tahun DLQ belum tersedia."))
        validate(need(tahun_akhir == tahun_awal + 1L, "Dynamic Location Quotient (DLQ) harus dihitung antar-tahun berurutan."))
        validate(need(tahun_awal %in% tahun_awal_valid, "Pasangan tahun DLQ yang dipilih tidak tersedia pada wilayah dan provinsi pembanding."))
        pasangan_tahun_dlq <- tibble::tibble(tahun_awal = tahun_awal, tahun_akhir = tahun_akhir)
      }

      bagian_dlq <- lapply(seq_len(nrow(pasangan_tahun_dlq)), function(i) {
        tahun_awal_i <- pasangan_tahun_dlq$tahun_awal[[i]]
        tahun_akhir_i <- pasangan_tahun_dlq$tahun_akhir[[i]]
        hasil_i <- shared_dlq_data_v90(
          data = pdrb_data(),
          kode_kelompok_pilih = input$kelompok,
          kode_wilayah_pilih = input$wilayah,
          level_pilih = normalize_lq_level(level_pilih),
          dasar_harga = dasar_harga_dlq,
          tahun_awal = tahun_awal_i,
          tahun_akhir = tahun_akhir_i,
          periode_pilih = periode_dlq
        )
        if (is.null(hasil_i) || nrow(hasil_i) == 0) return(tibble::tibble())
        hasil_i %>% mutate(.dlq_tahun_awal = tahun_awal_i, .dlq_tahun_akhir = tahun_akhir_i)
      })
      data <- dplyr::bind_rows(bagian_dlq)
      validate(need(nrow(data) > 0, "Data DLQ tidak tersedia untuk pasangan tahun yang dipilih."))

      if (!"wilayah_pembanding" %in% names(data)) {
        if ("wilayah_acuan" %in% names(data)) {
          data$wilayah_pembanding <- data$wilayah_acuan
        } else {
          data$wilayah_pembanding <- rep(NA_character_, nrow(data))
        }
      }
      klasifikasi_tabel <- "semua"

      data <- data %>%
        group_by(.dlq_tahun_awal, .dlq_tahun_akhir) %>%
        arrange_tabel_rows(time_columns = FALSE) %>%
        ungroup() %>%
        transmute(
          Provinsi = kelompok,
          Wilayah = wilayah,
          `Wilayah Pembanding` = wilayah_pembanding,
          Indikator = "DLQ",
          `Dasar Harga` = dasar_harga_dlq,
          Level = level,
          `Kode Kategori` = kode_kategori,
          Kategori = kategori_label,
          `Tahun Awal` = as.character(.dlq_tahun_awal),
          `Tahun Akhir` = as.character(.dlq_tahun_akhir),
          Periode = period_label(periode_dlq),
          `Nilai Wilayah Awal` = nilai_wilayah_awal,
          `Nilai Wilayah Akhir` = nilai_wilayah_akhir,
          `Nilai Pembanding Awal` = nilai_acuan_awal,
          `Nilai Pembanding Akhir` = nilai_acuan_akhir,
          `Location Quotient (LQ) Awal` = LQ_awal,
          `Location Quotient (LQ) Akhir` = LQ_akhir,
          `Pertumbuhan Sektor Wilayah (%)` = `Pertumbuhan Sektor Wilayah` * 100,
          `Pertumbuhan Total Wilayah (%)` = `Pertumbuhan Total Wilayah` * 100,
          `Pertumbuhan Sektor Pembanding (%)` = `Pertumbuhan Sektor Acuan` * 100,
          `Pertumbuhan Total Pembanding (%)` = `Pertumbuhan Total Acuan` * 100,
          DLQ = DLQ,
          `Dynamic Location Quotient (DLQ)` = DLQ,
          `Status Location Quotient (LQ)` = `Status LQ`,
          `Status Dynamic Location Quotient (DLQ)` = `Status DLQ`,
          `Klasifikasi Sektor` = `Klasifikasi Sektor`,
          Interpretasi = case_when(
            `Klasifikasi Sektor` == "Unggulan" ~ "Basis dan prospektif.",
            `Klasifikasi Sektor` == "Prospektif" ~ "Basis tetapi perlu diwaspadai karena prospeknya melemah.",
            `Klasifikasi Sektor` == "Andalan" ~ "Nonbasis tetapi prospektif untuk dikembangkan.",
            `Klasifikasi Sektor` == "Kurang Prospektif" ~ "Nonbasis dan kurang prospektif.",
            TRUE ~ "Tidak terklasifikasi."
          )
        ) %>%
        make_wide_table("dlq")

    } else if (jenis_tabel %in% c("shift_share", "shift_share_biasa", "shift_share_extended")) {
      dasar_harga_shift <- "ADHK"
      tahun_awal_shift <- suppressWarnings(as.integer(as.character(input$tahun_awal_shift_tabel)[1]))
      tahun_akhir_shift <- suppressWarnings(as.integer(as.character(input$tahun_akhir_shift_tabel)[1]))
      mode_shift_tabel <- "extended"
      
      validate(need(!is.na(tahun_awal_shift) && !is.na(tahun_akhir_shift), "Tahun awal dan tahun akhir Extended Shift Share belum tersedia."))
      validate(need(tahun_akhir_shift > tahun_awal_shift, "Tahun Akhir Extended Shift Share harus lebih besar dari Tahun Awal."))
      validate(need(reference_available_by_dasar_v5(dasar_harga_shift), potential_region_message_v8(dasar_harga_shift)))
      
      raw_shift <- shared_shift_data_v90(
        pdrb_data(),
        input$kelompok,
        input$wilayah,
        normalize_lq_level(level_pilih),
        dasar_harga_shift,
        tahun_akhir_shift,
        tahun_awal = tahun_awal_shift,
        mode = mode_shift_tabel
      )
      validate(need(
        nrow(raw_shift) > 0 && all(c("CE", "RIE", "RSE", "Tipe", "Diagnosa") %in% names(raw_shift)),
        "Data Extended Shift Share belum tersedia. Pastikan tahun sebelumnya, tahun terpilih, dan wilayah pembanding tersedia."
      ))
      
      data <- raw_shift %>%
        arrange_tabel_rows(time_columns = FALSE) %>%
        transmute(
          Tahun = as.character(tahun_akhir),
          Daerah = wilayah,
          `Wilayah Pembanding` = wilayah_acuan,
          Level = level,
          `Kode Kategori` = kode_kategori,
          Kategori = kategori_label,
          `Tahun Awal` = as.character(tahun_awal),
          `Tahun Akhir` = as.character(tahun_akhir),
          Periode = "Tahun",
          `Dasar Harga` = dasar_harga_shift,
          `Nilai Awal Wilayah` = v_ij0,
          `Nilai Akhir Wilayah` = v_ij1,
          `Nilai Awal Pembanding` = v_i0,
          `Nilai Akhir Pembanding` = v_i1,
          `Pertumbuhan Sektor Wilayah` = gij,
          `Pertumbuhan Sektor Pembanding` = Gi,
          `Pertumbuhan Total Wilayah` = gj,
          `Pertumbuhan Total Pembanding` = G,
          NE = NE,
          IM = IM,
          CE = CE,
          RIE = RIE,
          RSE = RSE,
          RCCE = RCCE,
          `Total Perubahan` = `Total Perubahan`,
          `Perubahan Aktual` = v_ij1 - v_ij0,
          `Selisih Audit` = `Total Perubahan` - (NE + IM + CE + RIE + RSE + RCCE),
          Tipe = Tipe,
          Diagnosis = shift_share_diagnosis_en_v5(Tipe),
          `Terjemahan Diagnosis` = shift_share_translation_id_v5(Tipe)
        ) %>%
        make_wide_table("shift_share")
    }    
    validate(need(nrow(data) > 0, "Data tidak tersedia untuk kombinasi filter yang dipilih."))
    if (!(jenis_tabel %in% c("lq", "lq_adhb", "lq_adhk", "dlq", "shift_share", "shift_share_biasa", "shift_share_extended"))) {
      data <- reorder_table_columns(data)
    }
    # Pengaman akhir: urutan tampilan dikunci lagi setelah tabel dibuat lebar/panjang.
    # Ini mencegah pivot_wider/DataTable mengembalikan urutan alfabetis sehingga PDRB tetap berada setelah R,S,T,U.
    data <- arrange_output_table_rows(data)

    # Untuk Semua Tahun DLQ, kelompokkan hasil per pasangan tahun agar tabel dan
    # file unduhan mudah dibaca: 2020-2021, 2021-2022, dan seterusnya.
    if (identical(jenis_tabel, "dlq") && all(c("Tahun Awal", "Tahun Akhir") %in% names(data))) {
      data <- data %>%
        mutate(
          .dlq_sort_awal = suppressWarnings(as.integer(`Tahun Awal`)),
          .dlq_sort_akhir = suppressWarnings(as.integer(`Tahun Akhir`))
        ) %>%
        arrange(.dlq_sort_awal, .dlq_sort_akhir) %>%
        select(-.dlq_sort_awal, -.dlq_sort_akhir)
    }
    
    # Pengaman akhir: hanya menghapus baris tampilan yang benar-benar identik.
    # Penyebab utama duplikasi sudah ditangani di level data dan join, bukan hanya ditutup dengan distinct().
    data <- remove_exact_display_duplicates(data)
    data
  })
  
  output$data_table <- renderDT({
    data <- table_data_result()
    
    table_output <- datatable(
      data,
      rownames = FALSE,
                  options = list(
        pageLength = 15,
        lengthMenu = list(
          c(10, 15, 25, 50, 100, -1),
          c("10", "15", "25", "50", "100", "Semua")
        ),
        scrollX = TRUE,
        order = list(),
        destroy = TRUE,
        stateSave = FALSE,
        deferRender = TRUE,
        dom = "lfrtip",
                language = list(decimal = ".", thousands = ",")
      )
    )
    
    numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]
    if (length(numeric_columns) > 0) {
      table_output <- table_output %>%
        formatRound(numeric_columns, digits = 4, mark = ",", dec.mark = ".")
    }
    
    table_output
  }, server = FALSE)
  


  table_data_download_filename_v919 <- function(extension) {
    jenis <- download_scalar_v919(input$jenis_tabel_data, "pdrb")
    region_label <- download_region_name_v919(input$wilayah)
    parts <- switch(
      jenis,
      "pdrb" = c("PDRB", download_scalar_v919(input$dasar_harga_tabel, "ADHK"), region_label),
      "distribusi" = c("Distribusi PDRB", download_scalar_v919(input$dasar_harga_tabel, "ADHK"), region_label),
      "pertumbuhan" = c(
        "Pertumbuhan PDRB", download_scalar_v919(input$dasar_harga_tabel, "ADHK"),
        download_scalar_v919(input$jenis_pertumbuhan_tabel, "Y-on-Y"), region_label
      ),
      "indeks" = c("Indeks Implisit", region_label),
      "laju_indeks" = c(
        "Laju Indeks Implisit", download_scalar_v919(input$jenis_laju_tabel, "Y-on-Y"), region_label
      ),
      "sumber_pertumbuhan" = c(
        "Sumber Pertumbuhan", download_scalar_v919(input$dasar_harga_tabel, "ADHK"),
        download_scalar_v919(input$jenis_pertumbuhan_tabel, "Y-on-Y"), region_label
      ),
      "lq" = c("LQ", download_scalar_v919(input$dasar_harga_lq_tabel, "ADHK"), region_label),
      "lq_adhb" = c("LQ", "ADHB", region_label),
      "lq_adhk" = c("LQ", "ADHK", region_label),
      "dlq" = c("DLQ", "ADHK", region_label),
      "shift_share" = c("Extended Shift Share", "ADHK", region_label),
      "shift_share_biasa" = c("Extended Shift Share Klasik", "ADHK", region_label),
      "shift_share_extended" = c("Extended Shift Share Extended", "ADHK", region_label),
      c("Data PDRB", region_label)
    )

    if (jenis %in% c("pdrb", "distribusi", "pertumbuhan", "indeks", "laju_indeks", "sumber_pertumbuhan")) {
      parts <- c(
        parts,
        download_year_part_v919(input$tahun_tabel),
        download_period_part_v919(input$periode_tabel)
      )
    } else if (jenis %in% c("lq", "lq_adhb", "lq_adhk")) {
      parts <- c(
        parts,
        download_year_part_v919(input$tahun_lq_tabel),
        download_period_part_v919(input$periode_lq_tabel)
      )
    } else if (identical(jenis, "dlq")) {
      start_year <- download_scalar_v919(input$tahun_awal_dlq_tabel, "__ALL__")
      end_year <- download_scalar_v919(input$tahun_akhir_dlq_tabel, "__ALL__")
      year_part <- if (identical(start_year, "__ALL__") || identical(end_year, "__ALL__")) {
        "Semua Tahun"
      } else {
        paste(start_year, end_year, sep = "-")
      }
      parts <- c(parts, year_part)
    } else if (jenis %in% c("shift_share", "shift_share_biasa", "shift_share_extended")) {
      export_data <- tryCatch(table_data_result(), error = function(e) tibble::tibble())
      reference_label <- NULL
      if (nrow(export_data) > 0L && "Wilayah Pembanding" %in% names(export_data)) {
        candidates <- unique(as.character(export_data[["Wilayah Pembanding"]]))
        candidates <- candidates[!is.na(candidates) & nzchar(trimws(candidates))]
        if (length(candidates) > 0L) reference_label <- candidates[[1]]
      }
      if (is.null(reference_label)) reference_label <- download_group_name_v919(input$kelompok)
      year_part <- paste(
        download_scalar_v919(input$tahun_awal_shift_tabel, "TAHUN_AWAL"),
        download_scalar_v919(input$tahun_akhir_shift_tabel, "TAHUN_AKHIR"),
        sep = "-"
      )
      parts <- c(parts, "Dengan", reference_label, year_part)
    }

    build_download_filename_v919(parts, extension)
  }

  output$download_data_csv <- downloadHandler(
    filename = function() {
      table_data_download_filename_v919("csv")
    },
    content = function(file) {
      write_export_csv(table_data_result(), file)
    }
  )

  output$download_data_excel <- downloadHandler(
    filename = function() {
      table_data_download_filename_v919("xlsx")
    },
    content = function(file) {
      data <- as.data.frame(table_data_result(), stringsAsFactors = FALSE)

      # Excel mengikuti seluruh filter aktif, termasuk pilihan Wide/Long.
      workbook <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(workbook, "Data PDRB", gridLines = FALSE)

      header_style <- openxlsx::createStyle(
        fontColour = "#FFFFFF",
        fgFill = "#326A92",
        textDecoration = "bold",
        halign = "center",
        valign = "center",
        border = "Bottom",
        borderColour = "#D9E2E8"
      )
      body_border <- openxlsx::createStyle(
        border = "TopBottomLeftRight",
        borderColour = "#E1E6EA",
        valign = "top"
      )
      numeric_style <- openxlsx::createStyle(
        numFmt = "#,##0.###############",
        border = "TopBottomLeftRight",
        borderColour = "#E1E6EA",
        valign = "top"
      )

      openxlsx::writeData(
        workbook,
        sheet = "Data PDRB",
        x = data,
        startRow = 1,
        startCol = 1,
        headerStyle = header_style,
        withFilter = TRUE,
        keepNA = FALSE
      )

      if (nrow(data) > 0 && ncol(data) > 0) {
        openxlsx::addStyle(
          workbook,
          sheet = "Data PDRB",
          style = body_border,
          rows = 2:(nrow(data) + 1),
          cols = seq_len(ncol(data)),
          gridExpand = TRUE,
          stack = TRUE
        )

        numeric_cols <- which(vapply(data, is.numeric, logical(1)))
        if (length(numeric_cols) > 0) {
          openxlsx::addStyle(
            workbook,
            sheet = "Data PDRB",
            style = numeric_style,
            rows = 2:(nrow(data) + 1),
            cols = numeric_cols,
            gridExpand = TRUE,
            stack = TRUE
          )
        }
      }

      openxlsx::freezePane(workbook, "Data PDRB", firstRow = TRUE)
      openxlsx::setColWidths(workbook, "Data PDRB", cols = seq_len(max(1, ncol(data))), widths = "auto")
      openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
    }
  )

  output$validation_status <- renderUI({
    result <- workbook_result()
    data <- result$data

    if (!isTRUE(result$uploaded)) {
      return(tags$div(
        tags$p(class = "status-idle", icon("info-circle"), " Belum ada file."),
        tags$div(class = "status-data-note", icon("cloud-upload"), span("Pilih minimal satu file provinsi/agregat atau kabupaten/kota, lalu klik Proses Data untuk menampilkan ringkasan."))
      ))
    }

    validation <- validation_with_context()
    failed_rows <- failed_file_rows()
    total_files <- length(unique(as.character(result$file_names)))
    successful_files <- validation %>%
      filter(status == "Berhasil Dibaca") %>%
      distinct(source_file) %>%
      nrow()
    unsuccessful_files <- max(0L, total_files - successful_files)
    sheets_read <- sum(validation$status == "Berhasil Dibaca", na.rm = TRUE)
    ignored_rows <- sum(stringr::str_detect(as.character(validation$status), "^Diabaikan:"), na.rm = TRUE)
    problem_rows <- sum(
      validation$status != "Berhasil Dibaca" &
        !stringr::str_detect(as.character(validation$status), "^Diabaikan:"),
      na.rm = TRUE
    ) + nrow(failed_rows)
    sheets_unread <- ignored_rows + problem_rows

    if (is.null(data) || nrow(data) == 0) {
      region_count <- 0L
      year_text <- "-"
      period_text <- "-"
      type_text <- "-"
    } else {
      region_count <- dplyr::n_distinct(data$kode_wilayah)
      years <- sort(unique(as.integer(data$tahun)))
      years <- years[!is.na(years)]
      year_text <- if (length(years) == 0) "-" else if (length(years) == 1) as.character(years) else paste0(min(years), "–", max(years))
      period_values <- c("I", "II", "III", "IV", "Total")
      present_periods <- period_values[period_values %in% unique(as.character(data$periode))]
      period_labels <- c(I = "Triwulan I", II = "Triwulan II", III = "Triwulan III", IV = "Triwulan IV", Total = "Tahun")
      period_text <- if (length(present_periods) == 0) "-" else paste(unname(period_labels[present_periods]), collapse = ", ")
      types <- c("PDRB ADHB" = "ADHB", "PDRB ADHK" = "ADHK")
      available_types <- unname(types[names(types) %in% unique(as.character(data$indikator))])
      type_text <- if (length(available_types) == 0) "-" else paste(available_types, collapse = ", ")
    }

    has_problem <- unsuccessful_files > 0 || problem_rows > 0 || !is.null(result$error)
    note_class <- if (has_problem) "status-data-note warning" else "status-data-note"
    note_icon <- if (has_problem) icon("warning") else icon("check-circle")
    note_text <- if (has_problem) {
      paste0(
        "Ada ", problem_rows, " sheet/tabel yang perlu diperiksa",
        if (ignored_rows > 0) paste0(" dan ", ignored_rows, " sheet/tabel yang sengaja diabaikan") else "",
        ". Buka tab ‘Tabel Tidak Dibaca’ untuk melihat penyebab dan saran."
      )
    } else if (ignored_rows > 0) {
      paste0(
        "Data mentah PDRB berhasil dibaca. Ada ", ignored_rows,
        " sheet/tabel yang sengaja diabaikan karena merupakan tabel turunan, sheet pendukung, atau sheet kosong."
      )
    } else {
      "Semua tabel yang teridentifikasi sebagai data mentah PDRB berhasil dibaca."
    }

    tags$div(
      tags$p(class = if (successful_files > 0) "status-ok" else "status-error",
             if (successful_files > 0) icon("check-circle") else icon("warning"),
             if (successful_files > 0) " Proses pembacaan data selesai." else " Data belum berhasil dibaca."),
      tags$div(
        class = "status-data-summary-grid",
        tags$div(class = "status-data-metric", tags$span("File berhasil dibaca"), tags$strong(paste0(successful_files, " dari ", total_files))),
        tags$div(class = "status-data-metric", tags$span("File belum berhasil dibaca"), tags$strong(unsuccessful_files)),
        tags$div(class = "status-data-metric", tags$span("Sheet/tabel berhasil dibaca"), tags$strong(sheets_read)),
        tags$div(class = "status-data-metric", tags$span("Sheet/tabel diabaikan/tidak dibaca"), tags$strong(sheets_unread)),
        tags$div(class = "status-data-metric", tags$span("Wilayah terdeteksi"), tags$strong(region_count)),
        tags$div(class = "status-data-metric", tags$span("Tahun tersedia"), tags$strong(year_text)),
        tags$div(class = "status-data-metric", tags$span("Periode tersedia"), tags$strong(period_text)),
        tags$div(class = "status-data-metric", tags$span("Jenis data tersedia"), tags$strong(type_text))
      ),
      tags$div(class = note_class, note_icon, span(note_text))
    )
  })

  output$validation_read_table <- renderDT({
    validation <- validation_with_context() %>%
      filter(status == "Berhasil Dibaca")
    validate(need(nrow(validation) > 0, "Belum ada tabel yang berhasil dibaca."))
    out <- validation %>%
      arrange(source_file, sheet_name) %>%
      transmute(
        No. = dplyr::row_number(),
        `Nama File` = source_file,
        `Nama Sheet` = sheet_name,
        Wilayah = wilayah,
        `Kode Wilayah` = kode_wilayah,
        `Level Wilayah` = level_wilayah,
        `Jenis Tabel` = jenis_data,
        `Tahun Tersedia` = tahun_tersedia,
        `Periode Tersedia` = periode_tersedia,
        `Jumlah Baris` = jumlah_baris_data,
        Status = status,
        Keterangan = reason
      )
    datatable(out, rownames = FALSE, options = pdrb_dt_options(pageLength = 10, buttons = FALSE, dom = "lfrtip"))
  }, server = FALSE)

  output$validation_unread_table <- renderDT({
    validation <- validation_with_context() %>%
      filter(status != "Berhasil Dibaca")
    validation <- dplyr::bind_rows(validation, failed_file_rows())
    validate(need(
      nrow(validation) > 0,
      "Tidak ada tabel yang diabaikan atau gagal dibaca. Dashboard tetap hanya menggunakan data mentah PDRB ADHB/ADHK sebagai input."
    ))

    suggestion <- function(status, reason) {
      dplyr::case_when(
        status == "Diabaikan: Tabel Turunan" ~ "Tidak perlu diperbaiki. Dashboard menghitung ulang indikator turunan dari data mentah ADHB/ADHK.",
        status == "Diabaikan: Sheet Pendukung" ~ "Tidak perlu diperbaiki. Sheet petunjuk atau metadata tidak dijadikan data analisis.",
        status == "Diabaikan: Sheet Kosong" ~ "Tidak perlu diperbaiki. Sheet kosong boleh dihapus agar workbook lebih ringkas.",
        status == "Diabaikan karena Slot Tidak Sesuai" ~ "Unggah file pada slot yang sesuai dengan level wilayahnya.",
        status == "Wilayah Tidak Dikenali" ~ "Periksa kode wilayah empat digit atau tambahkan baris ‘Wilayah : Nama Wilayah’.",
        status == "Tidak Ada Tabel ADHB/ADHK" ~ "Pastikan terdapat judul PDRB ADHB atau PDRB ADHK beserta header tahun dan periode.",
        status == "Tidak Ada Nilai Valid" ~ "Periksa nilai numerik; data kosong, NA, 0.0, dan #REF! tidak digunakan.",
        status == "File Gagal Dibaca" ~ "Periksa format file Excel, struktur sheet, dan pesan kesalahan yang ditampilkan.",
        TRUE ~ "Periksa kembali struktur tabel sesuai template data PDRB."
      )
    }

    out <- validation %>%
      mutate(`Saran Perbaikan` = suggestion(status, reason)) %>%
      arrange(source_file, sheet_name) %>%
      transmute(
        No. = dplyr::row_number(),
        `Nama File` = source_file,
        `Nama Sheet` = sheet_name,
        `Perkiraan Jenis Tabel` = jenis_data,
        `Perkiraan Wilayah` = wilayah,
        Status = status,
        Penyebab = reason,
        `Saran Perbaikan` = `Saran Perbaikan`
      )
    datatable(out, rownames = FALSE, options = pdrb_dt_options(pageLength = 10, buttons = FALSE, dom = "lfrtip"))
  }, server = FALSE)


  output$validation_detail_table <- renderDT({
    validation <- validation_reference()
    validate(need(nrow(validation) > 0, "Belum ada ringkasan validasi. Pilih minimal satu file lalu klik Proses Data."))
    out <- validation %>%
      transmute(
        `File` = source_file,
        Sheet = sheet_name,
        `Kode Wilayah` = kode_wilayah,
        Status = status,
        Alasan = reason,
        `Jumlah Baris Dibaca` = jumlah_baris
      )
    datatable(out, rownames = FALSE, options = pdrb_dt_options(pageLength = 10, buttons = FALSE, dom = "lfrtip"))
  }, server = FALSE)
  output$region_map_table <- renderDT({
    region_data <- region_reference()
    validate(need(nrow(region_data) > 0, "Unggah file untuk menampilkan wilayah."))
    
    regions <- region_data %>%
      transmute(
        `Kode Kelompok` = kode_kelompok,
        `Provinsi` = kelompok,
        `Kode Wilayah` = kode_wilayah,
        Wilayah = wilayah,
        Jenis = jenis_wilayah,
        `File Sumber` = source_file
      ) %>%
      arrange(`Provinsi`, `Kode Wilayah`)
    
    datatable(
      regions,
      rownames = FALSE,
            options = pdrb_dt_options(pageLength = 10, buttons = FALSE, dom = "lfrtip")
    )
  }, server = FALSE)
  
  output$input_preview <- renderDT({
    req(input$preview_indikator)
    
    data <- pdrb_data() %>%
      filter(indikator == input$preview_indikator) %>%
      add_time_columns() %>%
      mutate(level = as.character(level)) %>%
      arrange(kelompok, wilayah, item_id, waktu_index, source_row) %>%
      transmute(
        `Provinsi` = kelompok,
        `Kode Wilayah` = kode_wilayah,
        Wilayah = wilayah,
        `Jenis Data` = indikator,
        Level = level,
        `Kode Kategori` = kode_kategori,
        Kategori = kategori_label,
        Waktu = waktu,
        Tahun = tahun,
        Periode = period_label(periode),
        Satuan = satuan,
        Nilai = round(nilai, 4)
      )
    
    validate(need(nrow(data) > 0, "Data untuk pilihan ini belum tersedia."))
    
    datatable(
      data,
      rownames = FALSE,
                  options = pdrb_dt_options(pageLength = 10, buttons = FALSE)
    )
  }, server = FALSE)
  
  output$sheet_structure <- renderDT({
    example <- tibble::tribble(
      ~Bagian, ~Contoh, ~Ketentuan,
      "Nama sheet", "3600 - Banten atau 3601 - Kabupaten Pandeglang", "Memuat kode wilayah empat digit. Kode berakhiran 00 dibaca sebagai Provinsi/Agregat.",
      "Indikator", "PDRB ADHB dan PDRB ADHK", "Hanya dua data mentah ini yang dibaca sebagai input; tabel turunan diabaikan dan dihitung ulang.",
      "Waktu", "Tahun + Triwulan atau Q1-2020", "Header waktu sejajar dengan kolom nilai.",
      "Kategori", "A | Pertanian atau A. Pertanian", "Kode dan uraian boleh dipisah atau digabung.",
      "Posisi", "Bebas", "Judul indikator, waktu, kategori, dan nilai harus ada.",
      "Nilai", "Angka atau kosong", "Tanda hubung dibaca sebagai data kosong."
    )
    
    datatable(
      example,
      rownames = FALSE,
      options = pdrb_dt_options(pageLength = 6, buttons = FALSE, dom = "lfrtip")
    )
  }, server = FALSE)
  output$download_example_xlsx <- downloadHandler(
    filename = function() {
      paste0("contoh_data_pdrb_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      write_embedded_xlsx(file)
    }
  )
  
  output$download_example_template_xlsx <- downloadHandler(
    filename = function() "Template_PDRB_17_Lapangan_Usaha.xlsx",
    content = function(file) {
      template_path <- file.path("www", "Template_PDRB_17_Lapangan_Usaha.xlsx")
      if (!file.exists(template_path)) {
        stop("File Template_PDRB_17_Lapangan_Usaha.xlsx tidak ditemukan di folder www.")
      }
      file.copy(template_path, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$download_example_template_xlsx_alt <- downloadHandler(
    filename = function() "Template_PDRB_Lengkap.xlsx",
    content = function(file) {
      template_path <- file.path("www", "Template_PDRB_Lengkap.xlsx")
      if (!file.exists(template_path)) {
        stop("File Template_PDRB_Lengkap.xlsx tidak ditemukan di folder www.")
      }
      file.copy(template_path, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )


  output$help_download_template_provinsi <- downloadHandler(
    filename = function() "Template_PDRB_17_Lapangan_Usaha.xlsx",
    content = function(file) {
      template_path <- file.path("www", "Template_PDRB_17_Lapangan_Usaha.xlsx")
      if (!file.exists(template_path)) {
        stop("File Template_PDRB_17_Lapangan_Usaha.xlsx tidak ditemukan di folder www.")
      }
      file.copy(template_path, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  output$help_download_template_kabkota <- downloadHandler(
    filename = function() "Template_PDRB_Lengkap.xlsx",
    content = function(file) {
      template_path <- file.path("www", "Template_PDRB_Lengkap.xlsx")
      if (!file.exists(template_path)) {
        stop("File Template_PDRB_Lengkap.xlsx tidak ditemukan di folder www.")
      }
      file.copy(template_path, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$download_example_csv <- downloadHandler(
    filename = function() {
      paste0("contoh_data_pdrb_tidy_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    contentType = "text/csv; charset=UTF-8",
    content = function(file) {
      example_data <- make_synthetic_tidy_data()
      utils::write.csv(
        example_data,
        file = file,
        row.names = FALSE,
        fileEncoding = "UTF-8",
        na = ""
      )
    }
  )
  
  
  output$plot_structure_bar <- renderPlotly({
    req(input$wilayah, structure_active_indicator(), input$tahun_struktur, input$triwulan_struktur, cancelOutput = TRUE)
    display_type <- structure_display_type()
    if (identical(display_type, "subkategori")) req(input$kategori_utama_struktur, cancelOutput = TRUE)
    if (identical(display_type, "rincian")) req(input$kategori_utama_struktur, input$subkategori_struktur, cancelOutput = TRUE)
    data <- structure_display_data() %>%
      arrange(dplyr::desc(kontribusi)) %>%
      mutate(label_rank = paste0(label_kode, " - ", stringr::str_trunc(label_uraian, 48)))
    validate(need(nrow(data) > 0, "Data ranking kontribusi sektor belum tersedia."))
    plot_ly(
      data,
      x = ~kontribusi,
      y = ~reorder(label_rank, kontribusi),
      type = "bar",
      orientation = "h",
      text = ~paste0(scales::number(kontribusi, accuracy = 0.01, big.mark = ",", decimal.mark = "."), "%"),
      textposition = "auto",
      hovertext = ~paste0(
        "<b>", label_kode, " - ", label_uraian, "</b>",
        "<br>Distribusi: ", scales::number(kontribusi, accuracy = 0.01, big.mark = ",", decimal.mark = "."), "%",
        "<br>Nilai: ", nilai_label
      ),
      hoverinfo = "text",
      marker = list(color = PDRB_COLORS$blue)
    ) %>%
      layout(
        title = list(text = "Ranking Kontribusi Sektor", x = 0.02),
        xaxis = list(title = "Kontribusi (%)", zeroline = FALSE),
        yaxis = list(title = "", automargin = TRUE),
        paper_bgcolor = PDRB_COLORS$surface,
        plot_bgcolor = PDRB_COLORS$surface,
        font = list(color = PDRB_COLORS$ink),
        margin = list(l = 220, r = 40, b = 55, t = 85)
      ) %>%
      config(displaylogo = FALSE)
  })
  
  # V9.3 — Filter laporan stabil --------------------------------------------
  # Dropdown laporan sebelumnya selalu di-update oleh observe() biasa. Karena
  # updateSelectizeInput memakai server = TRUE, pilihan sempat dikosongkan saat
  # setiap refresh. Observer juga membaca input yang sama dengan input yang
  # diperbarui, sehingga terbentuk feedback loop visual (dropdown berkedip).
  #
  # Perbaikan:
  # - update hanya ketika daftar pilihan benar-benar berubah;
  # - pilihan kecil laporan dikirim client-side (server = FALSE);
  # - freezeReactiveValue mencegah nilai NULL sementara mereset preview;
  # - nilai aktif dipertahankan selama masih valid.
  report_filter_cache <- reactiveValues(
    wilayah = NULL,
    tahun = NULL,
    tahun_awal = NULL,
    tahun_akhir = NULL
  )

  report_choice_signature <- function(choices) {
    if (is.null(choices) || length(choices) == 0L) return("<EMPTY>")
    paste0(names(choices), "=", unname(choices), collapse = "||")
  }

  update_report_selectize <- function(id, choices, selected = NULL, cache_name, placeholder = NULL) {
    if (is.null(choices)) choices <- character(0)
    choices <- choices[!is.na(unname(choices)) & nzchar(as.character(unname(choices)))]
    values <- as.character(unname(choices))
    current <- isolate(input[[id]])
    current <- if (is.null(current) || length(current) == 0L) NULL else as.character(current)[1]
    selected <- if (is.null(selected) || length(selected) == 0L) NULL else as.character(selected)[1]

    # Pertahankan pilihan pengguna jika masih valid.
    target <- if (!is.null(current) && current %in% values) {
      current
    } else if (!is.null(selected) && selected %in% values) {
      selected
    } else if (length(values) > 0L) {
      values[[1]]
    } else {
      character(0)
    }

    signature <- report_choice_signature(choices)
    previous_signature <- isolate(report_filter_cache[[cache_name]])
    current_valid <- !is.null(current) && current %in% values

    # Jangan menyentuh Selectize jika pilihan dan nilai aktif sudah valid.
    if (identical(previous_signature, signature) && current_valid) {
      return(invisible(FALSE))
    }

    freezeReactiveValue(input, id)
    update_pdrb_selectize(
      session,
      id,
      choices = choices,
      selected = target,
      placeholder = placeholder,
      server_side = FALSE
    )
    report_filter_cache[[cache_name]] <- signature
    invisible(TRUE)
  }

  observeEvent(shared_data_store$data_version, {
    data <- pdrb_data()
    req(shared_data_store$data_version > 0L, nrow(data) > 0)

    wilayah_choices <- data %>%
      distinct(kode_wilayah, wilayah) %>%
      arrange(wilayah) %>%
      { stats::setNames(as.character(.$kode_wilayah), as.character(.$wilayah)) }

    current_report <- isolate(input$laporan_wilayah)
    global_wilayah <- isolate(input$wilayah)
    selected_wil <- if (!is.null(current_report) && current_report %in% unname(wilayah_choices)) {
      current_report
    } else if (!is.null(global_wilayah) && global_wilayah %in% unname(wilayah_choices)) {
      global_wilayah
    } else {
      unname(wilayah_choices)[1]
    }

    update_report_selectize(
      "laporan_wilayah",
      choices = wilayah_choices,
      selected = selected_wil,
      cache_name = "wilayah",
      placeholder = "Pilih wilayah"
    )
  }, ignoreInit = FALSE)

  observeEvent(input$laporan_wilayah, {
    data <- pdrb_data()
    wilayah_id <- as.character(input$laporan_wilayah)[1]
    req(nrow(data) > 0, !is.na(wilayah_id), nzchar(wilayah_id))

    years <- data %>%
      filter(kode_wilayah == wilayah_id) %>%
      distinct(tahun) %>%
      arrange(tahun) %>%
      pull(tahun)
    years <- sort(unique(suppressWarnings(as.integer(years))))
    years <- years[!is.na(years)]
    req(length(years) > 0L)

    year_choices <- stats::setNames(as.character(years), as.character(years))
    selected_year <- isolate(input$laporan_tahun)
    if (is.null(selected_year) || !selected_year %in% unname(year_choices)) {
      selected_year <- tail(unname(year_choices), 1)
    }

    start_years <- if (length(years) > 1L) years[-length(years)] else years
    start_choices <- valid_year_choices(start_years)
    selected_start <- isolate(input$laporan_tahun_awal)
    if (is.null(selected_start) || !selected_start %in% unname(start_choices)) {
      selected_start <- as.character(min(start_years, na.rm = TRUE))
    }

    selected_start_num <- suppressWarnings(as.integer(selected_start))
    end_years <- years[years > selected_start_num]
    if (length(end_years) == 0L) end_years <- max(years, na.rm = TRUE)
    end_choices <- valid_year_choices(end_years)
    selected_end <- isolate(input$laporan_tahun_akhir)
    if (is.null(selected_end) || !selected_end %in% unname(end_choices)) {
      selected_end <- as.character(max(end_years, na.rm = TRUE))
    }

    update_report_selectize(
      "laporan_tahun",
      choices = year_choices,
      selected = selected_year,
      cache_name = "tahun",
      placeholder = "Pilih tahun fokus"
    )
    update_report_selectize(
      "laporan_tahun_awal",
      choices = start_choices,
      selected = selected_start,
      cache_name = "tahun_awal",
      placeholder = "Pilih tahun awal"
    )
    update_report_selectize(
      "laporan_tahun_akhir",
      choices = end_choices,
      selected = selected_end,
      cache_name = "tahun_akhir",
      placeholder = "Pilih tahun akhir"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$laporan_tahun_awal, {
    data <- pdrb_data()
    wilayah_id <- as.character(input$laporan_wilayah)[1]
    awal <- suppressWarnings(as.integer(input$laporan_tahun_awal)[1])
    req(nrow(data) > 0, !is.na(wilayah_id), nzchar(wilayah_id), !is.na(awal))

    years <- data %>%
      filter(kode_wilayah == wilayah_id) %>%
      distinct(tahun) %>%
      pull(tahun)
    years <- sort(unique(suppressWarnings(as.integer(years))))
    years <- years[!is.na(years)]

    end_years <- years[years > awal]
    if (length(end_years) == 0L) end_years <- max(years, na.rm = TRUE)
    end_choices <- valid_year_choices(end_years)
    selected_end <- isolate(input$laporan_tahun_akhir)
    if (is.null(selected_end) || !selected_end %in% unname(end_choices)) {
      selected_end <- as.character(max(end_years, na.rm = TRUE))
    }

    update_report_selectize(
      "laporan_tahun_akhir",
      choices = end_choices,
      selected = selected_end,
      cache_name = "tahun_akhir",
      placeholder = "Pilih tahun akhir"
    )
  }, ignoreInit = TRUE)

  # V9.4 — Laporan profesional dan konsisten -------------------------------
  # Laporan Ringkas tetap merangkum seluruh menu. Perbedaannya dengan laporan
  # Lengkap hanya pada jumlah baris tabel dan kedalaman penjelasan, bukan pada
  # jumlah menu yang disertakan.
  report_period_values <- function(x) {
    value <- as.character(x)[1]
    if (is.null(value) || is.na(value) || !nzchar(value)) "__LATEST__" else value
  }

  report_date_id <- function(date = Sys.Date()) {
    months_id <- c(
      "Januari", "Februari", "Maret", "April", "Mei", "Juni",
      "Juli", "Agustus", "September", "Oktober", "November", "Desember"
    )
    paste(format(date, "%d"), months_id[as.integer(format(date, "%m"))], format(date, "%Y"))
  }

  report_number_id <- function(x, digits = 2L) {
    if (length(x) == 0L || is.na(x[[1]]) || !is.finite(as.numeric(x[[1]]))) return("–")
    formatC(as.numeric(x[[1]]), format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  }

  report_percent_id <- function(x, digits = 2L) {
    if (length(x) == 0L || is.na(x[[1]]) || !is.finite(as.numeric(x[[1]]))) return("–")
    paste0(report_number_id(x, digits), " persen")
  }

  report_pdrb_card_id <- function(x) {
    if (length(x) == 0L || is.na(x[[1]]) || !is.finite(as.numeric(x[[1]]))) return("–")
    value <- as.numeric(x[[1]])
    abs_value <- abs(value)
    if (abs_value >= 1e6) {
      paste0("Rp", report_number_id(value / 1e6, 2L), " triliun")
    } else if (abs_value >= 1e3) {
      paste0("Rp", report_number_id(value / 1e3, 2L), " miliar")
    } else {
      paste0("Rp", report_number_id(value, 2L), " juta")
    }
  }

  # Membersihkan kode kategori khusus untuk narasi dan tabel laporan.
  # Kode tetap dipertahankan pada backend serta dapat dipakai sebagai label
  # singkat pada grafik kuadran DLQ.
  report_clean_sector_label <- function(x) {
    value <- trimws(as.character(x))
    value[is.na(value)] <- ""

    # Hapus seluruh prefix kode yang mungkin sudah tertulis berulang,
    # misalnya "A - A - Pertanian" atau "M,N - M,N - Jasa Perusahaan".
    value <- sub(
      "^(?:(?:M,N|R,S,T,U|[A-U])\\s*[-–]\\s*)+",
      "",
      value,
      perl = TRUE
    )

    trimws(value)
  }

  report_period_sentence_id <- function(period_label_value, year_value) {
    period_value <- as.character(period_label_value)[1]
    year_value <- suppressWarnings(as.integer(year_value)[1])
    if (is.na(period_value) || !nzchar(period_value) || identical(period_value, "Belum tersedia")) {
      return(paste0("tahun ", year_value))
    }
    if (identical(period_value, "Tahun")) paste0("tahun ", year_value) else paste0(period_value, " tahun ", year_value)
  }

  report_period_cover_id <- function(period_label_value, year_value) {
    period_value <- as.character(period_label_value)[1]
    year_value <- suppressWarnings(as.integer(year_value)[1])
    if (identical(period_value, "Tahun")) paste0("Tahun ", year_value) else paste(period_value, year_value)
  }

  report_shift_diagnosis_short <- function(type_value) {
    dplyr::case_when(
      type_value == "T1" ~ "Kompetitif dan tumbuh pada wilayah yang dinamis.",
      type_value == "T2" ~ "Kompetitif, tetapi dinamika regional relatif lambat.",
      type_value == "T3" ~ "Kuat secara regional, tetapi lemah terhadap wilayah pembanding.",
      type_value == "T4" ~ "Kompetitif terhadap pembanding, tetapi lemah secara internal.",
      type_value == "T5" ~ "Belum kompetitif, tetapi didukung dinamika wilayah yang baik.",
      type_value == "T6" ~ "Kuat secara internal, tetapi belum kompetitif terhadap pembanding.",
      type_value == "T7" ~ "Lemah secara internal dan terhadap pembanding, tetapi wilayah dinamis.",
      type_value == "T8" ~ "Belum kompetitif dan berada pada wilayah yang kurang dinamis.",
      TRUE ~ "Interpretasi belum tersedia."
    )
  }

  report_list_id <- function(x, max_n = 3L) {
    values <- trimws(as.character(x))
    values <- unique(values[!is.na(values) & nzchar(values)])
    if (length(values) == 0L) return("belum tersedia")
    values <- head(values, max(1L, as.integer(max_n)))
    if (length(values) == 1L) return(values[[1]])
    if (length(values) == 2L) return(paste(values, collapse = " dan "))
    paste0(paste(values[-length(values)], collapse = ", "), ", dan ", values[[length(values)]])
  }

  report_cv_interpretation_id <- function(cv) {
    if (length(cv) == 0L || is.na(cv[[1]]) || !is.finite(as.numeric(cv[[1]]))) {
      return("tingkat keragaman belum dapat dinilai")
    }
    value <- as.numeric(cv[[1]])
    if (value < 10) return("keragaman relatif rendah")
    if (value <= 20) return("keragaman relatif moderat")
    "keragaman relatif tinggi"
  }

  report_period_label <- function(x) {
    value <- as.character(x)[1]
    if (identical(value, "__LATEST__")) return("Periode terbaru")
    period_label(value)
  }

  report_focus_period <- function(data, wilayah_id, tahun_fokus, periode_request) {
    available <- data %>%
      filter(
        kode_wilayah == .env$wilayah_id,
        tahun == .env$tahun_fokus,
        indikator %in% c("PDRB ADHB", "PDRB ADHK"),
        as.character(level) == "Total PDRB",
        kode_kategori == "PDRB",
        periode %in% c("I", "II", "III", "IV", "Total"),
        !is.na(nilai), is.finite(nilai)
      ) %>%
      distinct(periode) %>%
      pull(periode) %>%
      as.character()

    if (length(available) == 0L) return(NA_character_)
    if (!identical(periode_request, "__LATEST__") && periode_request %in% available) {
      return(periode_request)
    }
    order_period <- c("I", "II", "III", "IV", "Total")
    available <- available[order(match(available, order_period), na.last = NA)]
    tail(available, 1)
  }

  report_safe_first <- function(x, default = NA) {
    if (length(x) == 0L) default else x[[1]]
  }

  make_report_bundle <- function() {
    data <- pdrb_data()
    req(nrow(data) > 0, input$laporan_wilayah, input$laporan_tahun, input$laporan_periode)

    wilayah_id <- as.character(input$laporan_wilayah)[1]
    tahun_fokus <- suppressWarnings(as.integer(input$laporan_tahun)[1])
    periode_request <- report_period_values(input$laporan_periode)
    tahun_awal <- suppressWarnings(as.integer(input$laporan_tahun_awal)[1])
    tahun_akhir <- suppressWarnings(as.integer(input$laporan_tahun_akhir)[1])
    report_type <- as.character(input$laporan_jenis)[1]
    if (is.na(report_type) || !report_type %in% c("ringkas", "lengkap")) report_type <- "ringkas"

    region_meta <- data %>%
      filter(kode_wilayah == .env$wilayah_id) %>%
      distinct(kode_wilayah, wilayah, kode_kelompok, kelompok, jenis_wilayah) %>%
      slice(1)

    validate(need(nrow(region_meta) > 0L, "Wilayah laporan tidak ditemukan pada data yang telah diproses."))

    wilayah_label <- report_safe_first(region_meta$wilayah, wilayah_id)
    kode_kelompok <- as.character(report_safe_first(region_meta$kode_kelompok, NA_character_))
    provinsi_label <- report_safe_first(region_meta$kelompok, kode_kelompok)
    jenis_wilayah <- report_safe_first(region_meta$jenis_wilayah, "Wilayah")
    is_subregion <- !is.na(kode_kelompok) && nzchar(kode_kelompok) && !identical(wilayah_id, kode_kelompok)

    years_region <- data %>%
      filter(kode_wilayah == .env$wilayah_id, !is.na(tahun)) %>%
      distinct(tahun) %>%
      pull(tahun) %>%
      as.integer() %>%
      sort()

    if (is.na(tahun_awal) || !tahun_awal %in% years_region) tahun_awal <- report_safe_first(years_region, tahun_fokus)
    if (is.na(tahun_akhir) || !tahun_akhir %in% years_region) tahun_akhir <- report_safe_first(tail(years_region, 1), tahun_fokus)
    if (tahun_akhir <= tahun_awal && length(years_region) > 1L) {
      tahun_awal <- years_region[[1]]
      tahun_akhir <- tail(years_region, 1)
    }

    focus_period <- report_focus_period(data, wilayah_id, tahun_fokus, periode_request)
    focus_period_label <- if (is.na(focus_period)) "Belum tersedia" else period_label(focus_period)
    focus <- if (is.na(focus_period)) tibble::tibble() else data %>%
      filter(kode_wilayah == .env$wilayah_id, tahun == .env$tahun_fokus, periode == .env$focus_period)

    get_total <- function(indicator) {
      value <- focus %>%
        filter(indikator == .env$indicator, as.character(level) == "Total PDRB", kode_kategori == "PDRB") %>%
        pull(nilai)
      report_safe_first(value, NA_real_)
    }

    pdrb_adhb <- get_total("PDRB ADHB")
    pdrb_adhk <- get_total("PDRB ADHK")
    growth <- get_total("Pertumbuhan ADHK Y-on-Y")

    top_sector_values <- if (is.na(focus_period)) tibble::tibble() else data %>%
      filter(
        kode_wilayah == .env$wilayah_id,
        tahun == .env$tahun_fokus,
        periode == .env$focus_period,
        indikator == "PDRB ADHB",
        as.character(level) == "Kategori Utama",
        !is.na(nilai), is.finite(nilai)
      ) %>%
      distinct(kode_kategori, kategori_label, .keep_all = TRUE) %>%
      arrange(desc(nilai)) %>%
      transmute(Kode = kode_kategori, Sektor = report_clean_sector_label(kategori_label), Nilai = nilai)

    struktur <- if (is.na(focus_period)) tibble::tibble() else data %>%
      filter(
        kode_wilayah == .env$wilayah_id,
        tahun == .env$tahun_fokus,
        periode == .env$focus_period,
        indikator == "Distribusi PDRB",
        as.character(level) == "Kategori Utama",
        !is.na(nilai), is.finite(nilai)
      ) %>%
      distinct(kode_kategori, kategori_label, .keep_all = TRUE) %>%
      arrange(desc(nilai)) %>%
      transmute(Kode = kode_kategori, Sektor = report_clean_sector_label(kategori_label), Distribusi = nilai)

    top_sector <- report_safe_first(top_sector_values$Sektor, NA_character_)
    top_sector_value <- report_safe_first(top_sector_values$Nilai, NA_real_)
    top_share <- report_safe_first(struktur$Distribusi, NA_real_)

    trend_quarterly <- data %>%
      filter(
        kode_wilayah == .env$wilayah_id,
        tahun >= .env$tahun_awal,
        tahun <= .env$tahun_akhir,
        periode %in% c("I", "II", "III", "IV"),
        indikator %in% c("PDRB ADHB", "PDRB ADHK"),
        as.character(level) == "Total PDRB",
        kode_kategori == "PDRB",
        !is.na(nilai), is.finite(nilai)
      ) %>%
      mutate(
        Periode = as.character(periode),
        Waktu = paste0(tahun, "-", Periode),
        Urutan = tahun * 10L + match(Periode, c("I", "II", "III", "IV")),
        Indikator = indikator,
        Nilai = nilai
      ) %>%
      distinct(Indikator, tahun, Periode, .keep_all = TRUE) %>%
      arrange(Urutan, Indikator) %>%
      select(Waktu, Urutan, Tahun = tahun, Periode, Indikator, Nilai)

    trend_annual <- data %>%
      filter(
        kode_wilayah == .env$wilayah_id,
        tahun >= .env$tahun_awal,
        tahun <= .env$tahun_akhir,
        periode == "Total",
        indikator %in% c("PDRB ADHB", "PDRB ADHK"),
        as.character(level) == "Total PDRB",
        kode_kategori == "PDRB",
        !is.na(nilai), is.finite(nilai)
      ) %>%
      mutate(Waktu = as.character(tahun), Urutan = as.integer(tahun), Indikator = indikator, Nilai = nilai) %>%
      distinct(Indikator, tahun, .keep_all = TRUE) %>%
      arrange(Urutan, Indikator) %>%
      select(Waktu, Urutan, Tahun = tahun, Indikator, Nilai)

    distribution_values <- data %>%
      filter(
        kode_wilayah == .env$wilayah_id,
        tahun >= .env$tahun_awal,
        tahun <= .env$tahun_akhir,
        periode %in% c("I", "II", "III", "IV"),
        indikator == "PDRB ADHB",
        as.character(level) == "Total PDRB",
        kode_kategori == "PDRB",
        !is.na(nilai), is.finite(nilai)
      ) %>%
      transmute(Tahun = as.integer(tahun), Periode = as.character(periode), Nilai = as.numeric(nilai)) %>%
      distinct(Tahun, Periode, .keep_all = TRUE) %>%
      arrange(Tahun, match(Periode, c("I", "II", "III", "IV")))

    distribution_stats <- if (nrow(distribution_values) > 0L) {
      values <- distribution_values$Nilai
      mean_value <- mean(values, na.rm = TRUE)
      sd_value <- stats::sd(values, na.rm = TRUE)
      tibble::tibble(
        Statistik = c("Jumlah observasi", "Rata-rata", "Median", "Simpangan baku", "Minimum", "Maksimum", "Koefisien variasi"),
        Nilai = c(
          as.character(length(values)),
          report_pdrb_card_id(mean_value),
          report_pdrb_card_id(stats::median(values, na.rm = TRUE)),
          report_pdrb_card_id(sd_value),
          report_pdrb_card_id(min(values, na.rm = TRUE)),
          report_pdrb_card_id(max(values, na.rm = TRUE)),
          ifelse(is.na(mean_value) || mean_value == 0 || is.na(sd_value), "–", paste0(report_number_id(sd_value / mean_value * 100, 2L), "%"))
        )
      )
    } else tibble::tibble(Statistik = character(), Nilai = character())

    comparison <- if (is.na(focus_period) || is.na(kode_kelompok)) {
      tibble::tibble(
        Kode = character(), Wilayah = character(), Jenis = character(),
        Nilai = numeric(), Dipilih = logical(), Peringkat = integer()
      )
    } else data %>%
      filter(
        (kode_kelompok == .env$kode_kelompok | kode_wilayah == .env$kode_kelompok),
        tahun == .env$tahun_fokus,
        periode == .env$focus_period,
        indikator == "PDRB ADHB",
        as.character(level) == "Total PDRB",
        kode_kategori == "PDRB",
        !is.na(nilai), is.finite(nilai)
      ) %>%
      distinct(kode_wilayah, .keep_all = TRUE) %>%
      transmute(
        Kode = kode_wilayah,
        Wilayah = wilayah,
        Jenis = jenis_wilayah,
        Nilai = nilai,
        Dipilih = kode_wilayah == .env$wilayah_id
      ) %>%
      arrange(desc(Nilai)) %>%
      mutate(Peringkat = min_rank(desc(Nilai)))

    comparison_subregions <- comparison %>%
      filter(Kode != .env$kode_kelompok) %>%
      arrange(desc(Nilai)) %>%
      mutate(Peringkat = min_rank(desc(Nilai)))
    comparison_has_province <- any(comparison$Kode == kode_kelompok)
    comparison_mode <- if (nrow(comparison_subregions) > 1L) {
      "ranking"
    } else if (nrow(comparison_subregions) == 1L && comparison_has_province) {
      "reference"
    } else {
      "limited"
    }

    comparison_selected_rank <- comparison_subregions %>% filter(Dipilih) %>% pull(Peringkat) %>% report_safe_first(NA_integer_)
    comparison_top <- report_safe_first(comparison_subregions$Wilayah, NA_character_)
    comparison_bottom <- report_safe_first(tail(comparison_subregions$Wilayah, 1), NA_character_)
    province_value <- comparison %>% filter(Kode == .env$kode_kelompok) %>% pull(Nilai) %>% report_safe_first(NA_real_)
    selected_value <- comparison %>% filter(Dipilih) %>% pull(Nilai) %>% report_safe_first(NA_real_)
    comparison_share <- if (!is.na(selected_value) && !is.na(province_value) && province_value != 0) selected_value / province_value * 100 else NA_real_
    subregion_average <- comparison_subregions %>% summarise(v = mean(Nilai, na.rm = TRUE)) %>% pull(v) %>% report_safe_first(NA_real_)
    comparison <- comparison %>%
      mutate(
        Proporsi = ifelse(!is.na(province_value) && province_value != 0, Nilai / province_value * 100, NA_real_),
        Peringkat = ifelse(Kode == kode_kelompok, NA_integer_, match(Kode, comparison_subregions$Kode))
      )

    reference_available <- is_subregion && !is.na(kode_kelompok) && any(data$kode_wilayah == kode_kelompok)
    empty_lq_report <- function() tibble::tibble(
      Kode = character(), Sektor = character(), LQ = numeric(),
      Status = character(), Interpretasi = character()
    )
    empty_dlq_report <- function() tibble::tibble(
      Kode = character(), Sektor = character(), `LQ Akhir` = numeric(),
      DLQ = numeric(), `Status LQ` = character(), `Status DLQ` = character(),
      Klasifikasi = character()
    )
    empty_shift_report <- function() tibble::tibble(
      Kode = character(), Sektor = character(), CE = numeric(), RIE = numeric(),
      RSE = numeric(), `Total Perubahan` = numeric(), Tipe = character(),
      Diagnosis = character(), `Diagnosis Ringkas` = character(),
      Rekomendasi = character()
    )

    lq_data <- empty_lq_report()
    dlq_data <- empty_dlq_report()
    shift_data <- empty_shift_report()

    # LQ boleh memakai periode fokus triwulanan. DLQ dan Extended Shift Share memakai
    # data tahunan ADHK yang benar-benar tersedia pada wilayah dan pembanding.
    dlq_tahun_awal <- NA_integer_
    dlq_tahun_akhir <- NA_integer_
    shift_tahun_awal <- NA_integer_
    shift_tahun_akhir <- NA_integer_
    annual_years_complete <- integer()

    if (reference_available) {
      annual_years_complete <- data %>%
        mutate(
          tahun = suppressWarnings(as.integer(tahun)),
          periode = as.character(periode),
          level = as.character(level),
          kode_wilayah = as.character(kode_wilayah)
        ) %>%
        filter(
          kode_wilayah %in% c(.env$wilayah_id, .env$kode_kelompok),
          indikator == "PDRB ADHK",
          periode %in% c("I", "II", "III", "IV"),
          level == "Total PDRB",
          kode_kategori == "PDRB",
          !is.na(tahun), !is.na(nilai), is.finite(nilai), nilai != 0
        ) %>%
        distinct(kode_wilayah, tahun, periode) %>%
        count(kode_wilayah, tahun, name = "jumlah_triwulan") %>%
        filter(jumlah_triwulan == 4L) %>%
        count(tahun, name = "jumlah_wilayah") %>%
        filter(
          jumlah_wilayah >= 2L,
          tahun >= .env$tahun_awal,
          tahun <= .env$tahun_akhir
        ) %>%
        arrange(tahun) %>%
        pull(tahun) %>%
        as.integer()

      if (length(annual_years_complete) >= 2L) {
        # DLQ laporan mengikuti Tahun Fokus. Tahun Fokus menjadi tahun akhir,
        # sedangkan tahun sebelumnya menjadi tahun awal. Contoh:
        # Tahun Fokus 2022 -> DLQ 2021-2022.
        kandidat_dlq_awal <- tahun_fokus - 1L
        kandidat_dlq_akhir <- tahun_fokus
        if (all(c(kandidat_dlq_awal, kandidat_dlq_akhir) %in% annual_years_complete)) {
          dlq_tahun_awal <- kandidat_dlq_awal
          dlq_tahun_akhir <- kandidat_dlq_akhir
        }

        # Extended Shift Share tetap menggunakan rentang Tahun Awal Analisis sampai
        # tahun tahunan lengkap terakhir di dalam rentang tersebut.
        shift_tahun_awal <- if (tahun_awal %in% annual_years_complete) {
          tahun_awal
        } else {
          annual_years_complete[[1L]]
        }
        shift_tahun_akhir <- tail(annual_years_complete, 1L)
        if (shift_tahun_akhir <= shift_tahun_awal) {
          shift_tahun_awal <- annual_years_complete[[length(annual_years_complete) - 1L]]
        }
      }
    }

    if (reference_available && !is.na(focus_period)) {
      lq_data <- tryCatch(
        shared_lq_data_v90(data, kode_kelompok, wilayah_id, "Kategori Utama", "ADHK") %>%
          mutate(periode = as.character(periode), tahun = as.integer(tahun)) %>%
          filter(tahun == .env$tahun_fokus, periode == .env$focus_period, !is.na(LQ), is.finite(LQ)) %>%
          distinct(kode_kategori, .keep_all = TRUE) %>%
          arrange(desc(LQ)) %>%
          transmute(
            Kode = kode_kategori,
            Sektor = report_clean_sector_label(kategori_label),
            LQ = as.numeric(LQ),
            Status = case_when(LQ > 1 ~ "Basis", LQ == 1 ~ "Sama", TRUE ~ "Nonbasis"),
            Interpretasi = Keterangan
          ),
        error = function(e) empty_lq_report()
      )
    }

    if (reference_available && !is.na(dlq_tahun_awal) && !is.na(dlq_tahun_akhir)) {
      dlq_data <- tryCatch(
        shared_dlq_data_v90(
          data, kode_kelompok, wilayah_id, "Kategori Utama", "ADHK",
          dlq_tahun_awal, dlq_tahun_akhir, "Total"
        ) %>%
          filter(!is.na(DLQ), is.finite(DLQ), !is.na(LQ_akhir), is.finite(LQ_akhir)) %>%
          distinct(kode_kategori, .keep_all = TRUE) %>%
          arrange(desc(DLQ)) %>%
          transmute(
            Kode = kode_kategori,
            Sektor = report_clean_sector_label(kategori_label),
            `LQ Akhir` = as.numeric(LQ_akhir),
            DLQ = as.numeric(DLQ),
            `Status LQ` = `Status LQ`,
            `Status DLQ` = `Status DLQ`,
            Klasifikasi = `Klasifikasi Sektor`
          ),
        error = function(e) empty_dlq_report()
      )
    }

    if (reference_available && !is.na(shift_tahun_awal) && !is.na(shift_tahun_akhir)) {
      shift_data <- tryCatch(
        shared_shift_data_v90(
          data, kode_kelompok, wilayah_id, "Kategori Utama", "ADHK",
          shift_tahun_akhir, tahun_awal = shift_tahun_awal, mode = "extended"
        ) %>%
          filter(!is.na(CE), is.finite(CE)) %>%
          distinct(kode_kategori, .keep_all = TRUE) %>%
          mutate(.tipe_urut = factor(Tipe, levels = paste0("T", 1:8), ordered = TRUE)) %>%
          arrange(.tipe_urut, desc(CE)) %>%
          transmute(
            Kode = kode_kategori,
            Sektor = report_clean_sector_label(kategori_label),
            CE = as.numeric(CE),
            RIE = as.numeric(RIE),
            RSE = as.numeric(RSE),
            `Total Perubahan` = as.numeric(`Total Perubahan`),
            Tipe = Tipe,
            Diagnosis = shift_share_translation_id_v5(Tipe),
            `Diagnosis Ringkas` = report_shift_diagnosis_short(Tipe),
            Rekomendasi = shift_share_note_v5(Tipe)
          ),
        error = function(e) empty_shift_report()
      )
    }

    lq_basis_count <- if (nrow(lq_data) > 0L) sum(lq_data$LQ > 1, na.rm = TRUE) else NA_integer_
    lq_top_sector <- report_safe_first(lq_data$Sektor, NA_character_)
    lq_top_value <- report_safe_first(lq_data$LQ, NA_real_)
    dlq_prospective_count <- if (nrow(dlq_data) > 0L) sum(dlq_data$DLQ > 1, na.rm = TRUE) else NA_integer_
    dlq_leading_count <- if (nrow(dlq_data) > 0L) sum(dlq_data$Klasifikasi == "Unggulan", na.rm = TRUE) else NA_integer_
    shift_top_row <- shift_data %>% arrange(desc(CE)) %>% slice(1)
    shift_top_sector <- report_safe_first(shift_top_row$Sektor, NA_character_)
    shift_top_ce <- report_safe_first(shift_top_row$CE, NA_real_)


    # Ringkasan analitis tambahan untuk memperkaya laporan tanpa mengubah
    # perhitungan indikator utama yang digunakan oleh dashboard.
    second_sector <- report_safe_first(top_sector_values$Sektor[2], NA_character_)
    second_sector_value <- report_safe_first(top_sector_values$Nilai[2], NA_real_)
    second_share <- report_safe_first(struktur$Distribusi[2], NA_real_)
    top5_share <- if (nrow(struktur) > 0L) sum(head(struktur$Distribusi, 5L), na.rm = TRUE) else NA_real_
    top_share_gap <- if (!is.na(top_share) && !is.na(second_share)) top_share - second_share else NA_real_
    adhb_adhk_ratio <- if (!is.na(pdrb_adhb) && !is.na(pdrb_adhk) && pdrb_adhk != 0) pdrb_adhb / pdrb_adhk else NA_real_

    report_trend_detail <- function(indicator_name) {
      quarterly <- trend_quarterly %>% filter(Indikator == .env$indicator_name) %>% arrange(Urutan)
      annual <- trend_annual %>% filter(Indikator == .env$indicator_name) %>% arrange(Tahun)
      result <- list(
        qoq = NA_real_, min_value = NA_real_, max_value = NA_real_,
        min_period = NA_character_, max_period = NA_character_, cagr = NA_real_
      )
      if (nrow(quarterly) > 0L) {
        min_row <- quarterly %>% slice_min(Nilai, n = 1L, with_ties = FALSE)
        max_row <- quarterly %>% slice_max(Nilai, n = 1L, with_ties = FALSE)
        result$min_value <- report_safe_first(min_row$Nilai, NA_real_)
        result$max_value <- report_safe_first(max_row$Nilai, NA_real_)
        result$min_period <- paste(period_label(report_safe_first(min_row$Periode, NA_character_)), report_safe_first(min_row$Tahun, NA_integer_))
        result$max_period <- paste(period_label(report_safe_first(max_row$Periode, NA_character_)), report_safe_first(max_row$Tahun, NA_integer_))
        if (nrow(quarterly) >= 2L) {
          previous_value <- quarterly$Nilai[[nrow(quarterly) - 1L]]
          latest_value <- quarterly$Nilai[[nrow(quarterly)]]
          if (!is.na(previous_value) && previous_value != 0 && !is.na(latest_value)) {
            result$qoq <- (latest_value - previous_value) / abs(previous_value) * 100
          }
        }
      }
      if (nrow(annual) >= 2L) {
        first_value <- annual$Nilai[[1L]]
        last_value <- annual$Nilai[[nrow(annual)]]
        year_span <- annual$Tahun[[nrow(annual)]] - annual$Tahun[[1L]]
        if (!is.na(first_value) && first_value > 0 && !is.na(last_value) && last_value > 0 && year_span > 0) {
          result$cagr <- ((last_value / first_value)^(1 / year_span) - 1) * 100
        }
      }
      result
    }

    trend_adhb_detail <- report_trend_detail("PDRB ADHB")
    trend_adhk_detail <- report_trend_detail("PDRB ADHK")

    distribution_mean <- if (nrow(distribution_values) > 0L) mean(distribution_values$Nilai, na.rm = TRUE) else NA_real_
    distribution_median <- if (nrow(distribution_values) > 0L) stats::median(distribution_values$Nilai, na.rm = TRUE) else NA_real_
    distribution_sd <- if (nrow(distribution_values) > 1L) stats::sd(distribution_values$Nilai, na.rm = TRUE) else NA_real_
    distribution_cv <- if (!is.na(distribution_mean) && distribution_mean != 0 && !is.na(distribution_sd)) distribution_sd / distribution_mean * 100 else NA_real_
    distribution_min_row <- if (nrow(distribution_values) > 0L) distribution_values %>% slice_min(Nilai, n = 1L, with_ties = FALSE) else tibble::tibble()
    distribution_max_row <- if (nrow(distribution_values) > 0L) distribution_values %>% slice_max(Nilai, n = 1L, with_ties = FALSE) else tibble::tibble()
    distribution_min_period <- if (nrow(distribution_min_row) > 0L) paste(period_label(distribution_min_row$Periode[[1L]]), distribution_min_row$Tahun[[1L]]) else NA_character_
    distribution_max_period <- if (nrow(distribution_max_row) > 0L) paste(period_label(distribution_max_row$Periode[[1L]]), distribution_max_row$Tahun[[1L]]) else NA_character_
    distribution_range <- if (nrow(distribution_values) > 0L) max(distribution_values$Nilai, na.rm = TRUE) - min(distribution_values$Nilai, na.rm = TRUE) else NA_real_

    comparison_gap <- if (!is.na(province_value) && !is.na(selected_value)) province_value - selected_value else NA_real_

    lq_nonbasis_count <- if (nrow(lq_data) > 0L) sum(lq_data$LQ < 1, na.rm = TRUE) else NA_integer_
    lq_basis_names <- if (nrow(lq_data) > 0L) report_list_id(lq_data %>% filter(LQ > 1) %>% arrange(desc(LQ)) %>% pull(Sektor), 3L) else "belum tersedia"

    dlq_unggulan_names <- if (nrow(dlq_data) > 0L) report_list_id(dlq_data %>% filter(Klasifikasi == "Unggulan") %>% arrange(desc(DLQ)) %>% pull(Sektor), 4L) else "belum tersedia"
    dlq_andalan_names <- if (nrow(dlq_data) > 0L) report_list_id(dlq_data %>% filter(Klasifikasi == "Andalan") %>% arrange(desc(DLQ)) %>% pull(Sektor), 4L) else "belum tersedia"
    dlq_prospektif_names <- if (nrow(dlq_data) > 0L) report_list_id(dlq_data %>% filter(Klasifikasi == "Prospektif") %>% arrange(desc(DLQ)) %>% pull(Sektor), 4L) else "belum tersedia"
    dlq_kurang_names <- if (nrow(dlq_data) > 0L) report_list_id(dlq_data %>% filter(Klasifikasi == "Kurang Prospektif") %>% arrange(desc(DLQ)) %>% pull(Sektor), 4L) else "belum tersedia"

    shift_positive_count <- if (nrow(shift_data) > 0L) sum(shift_data$CE > 0, na.rm = TRUE) else NA_integer_
    shift_negative_count <- if (nrow(shift_data) > 0L) sum(shift_data$CE < 0, na.rm = TRUE) else NA_integer_
    shift_bottom_row <- shift_data %>% arrange(CE) %>% slice(1)
    shift_bottom_sector <- report_safe_first(shift_bottom_row$Sektor, NA_character_)
    shift_bottom_ce <- report_safe_first(shift_bottom_row$CE, NA_real_)
    shift_type_summary <- if (nrow(shift_data) > 0L) shift_data %>% count(Tipe, name = "Jumlah") %>% arrange(desc(Jumlah), Tipe) %>% slice(1) else tibble::tibble()
    shift_dominant_type <- report_safe_first(shift_type_summary$Tipe, NA_character_)
    shift_dominant_count <- report_safe_first(shift_type_summary$Jumlah, NA_integer_)

    data_status <- tibble::tibble(
      Komponen = c("Data ADHB", "Data ADHK", "Wilayah pembanding", "Tahun fokus", "Periode fokus", "Rentang tren"),
      Status = c(
        ifelse(any(data$kode_wilayah == wilayah_id & data$indikator == "PDRB ADHB"), "Tersedia", "Tidak tersedia"),
        ifelse(any(data$kode_wilayah == wilayah_id & data$indikator == "PDRB ADHK"), "Tersedia", "Tidak tersedia"),
        ifelse(reference_available, paste0("Tersedia: ", provinsi_label), "Tidak tersedia/tidak diperlukan"),
        as.character(tahun_fokus),
        focus_period_label,
        paste0(tahun_awal, "–", tahun_akhir)
      )
    )

    overview_kpi <- tibble::tibble(
      Indikator = c("PDRB ADHB", "PDRB ADHK", "Pertumbuhan Y-on-Y", "Sektor terbesar", "Jumlah sektor basis", "Jumlah sektor prospektif"),
      Nilai = c(
        report_pdrb_card_id(pdrb_adhb),
        report_pdrb_card_id(pdrb_adhk),
        ifelse(is.na(growth), "Belum tersedia", report_percent_id(growth, 2L)),
        ifelse(is.na(top_sector), "Belum tersedia", top_sector),
        ifelse(is.na(lq_basis_count), "Belum tersedia", paste0(lq_basis_count, " sektor")),
        ifelse(is.na(dlq_prospective_count), "Belum tersedia", paste0(dlq_prospective_count, " sektor"))
      )
    )

    validation_notes <- character()
    if (is.na(focus_period)) {
      validation_notes <- c(validation_notes, paste0("Data tahun ", tahun_fokus, " belum memiliki periode bernilai untuk wilayah yang dipilih."))
    }
    if (!"Total" %in% (data %>% filter(kode_wilayah == wilayah_id, tahun == tahun_fokus, !is.na(nilai), is.finite(nilai)) %>% pull(periode) %>% as.character())) {
      validation_notes <- c(validation_notes, paste0("Nilai tahunan tahun ", tahun_fokus, " belum tersedia; analisis fokus menggunakan ", focus_period_label, "."))
    }
    if (nrow(trend_annual) == 0L) {
      validation_notes <- c(validation_notes, "Tren tahunan tidak ditampilkan karena nilai periode Tahun belum tersedia pada rentang yang dipilih.")
    }
    if (is_subregion && !reference_available) {
      validation_notes <- c(validation_notes, "Analisis LQ, DLQ, dan Extended Shift Share tidak dapat disusun karena data provinsi/agregat pembanding belum tersedia.")
    }
    if (!is_subregion) {
      validation_notes <- c(validation_notes, "Analisis potensi wilayah tidak ditampilkan karena wilayah laporan adalah provinsi/agregat. Metode LQ, DLQ, dan Extended Shift Share memerlukan wilayah analisis kabupaten/kota dan provinsi sebagai pembanding.")
    }
    if (reference_available && length(annual_years_complete) < 2L) {
      validation_notes <- c(validation_notes, "DLQ dan Extended Shift Share belum dapat dihitung karena belum tersedia sedikitnya dua tahun data tahunan ADHK yang lengkap pada wilayah analisis dan wilayah pembanding.")
    }
    if (reference_available) {
      kandidat_dlq_awal <- tahun_fokus - 1L
      kandidat_dlq_akhir <- tahun_fokus
      if (!is.na(dlq_tahun_awal) && !is.na(dlq_tahun_akhir)) {
        validation_notes <- c(validation_notes, paste0(
          "DLQ menggunakan periode ", dlq_tahun_awal, "–", dlq_tahun_akhir,
          " karena Tahun Fokus ", tahun_fokus,
          " diperlakukan sebagai tahun akhir analisis DLQ."
        ))
      } else if (length(annual_years_complete) >= 2L) {
        validation_notes <- c(validation_notes, paste0(
          "DLQ tidak dapat dihitung untuk periode ", kandidat_dlq_awal, "–", kandidat_dlq_akhir,
          " karena data tahunan ADHK lengkap belum tersedia pada wilayah analisis dan wilayah pembanding."
        ))
      }
    }
    if (reference_available && !is.na(shift_tahun_akhir) && shift_tahun_akhir < tahun_akhir) {
      validation_notes <- c(validation_notes, paste0(
        "Extended Shift Share menggunakan periode ", shift_tahun_awal, "–", shift_tahun_akhir,
        " karena tahun akhir laporan belum memiliki data tahunan ADHK yang lengkap."
      ))
    }
    negative_count <- data %>% filter(kode_wilayah == wilayah_id, indikator %in% c("PDRB ADHB", "PDRB ADHK"), !is.na(nilai), nilai < 0) %>% nrow()
    if (negative_count > 0L) validation_notes <- c(validation_notes, paste0("Ditemukan ", negative_count, " nilai PDRB negatif yang perlu diverifikasi pada sumber data."))
    if (length(validation_notes) == 0L) validation_notes <- "Data utama yang diperlukan untuk laporan tersedia pada rentang analisis yang dipilih."

    period_sentence <- report_period_sentence_id(focus_period_label, tahun_fokus)
    period_cover <- report_period_cover_id(focus_period_label, tahun_fokus)

    executive_paragraph_1 <- paste0(
      "Pada ", period_sentence, ", PDRB ", wilayah_label,
      " atas dasar harga berlaku tercatat sebesar ", report_pdrb_card_id(pdrb_adhb),
      ", sedangkan PDRB atas dasar harga konstan sebesar ", report_pdrb_card_id(pdrb_adhk), ". ",
      if (is.na(growth)) {
        "Pertumbuhan Y-on-Y belum dapat dihitung karena periode pembanding belum tersedia. "
      } else if (growth >= 0) {
        paste0("Dibandingkan dengan periode yang sama tahun sebelumnya, perekonomian tumbuh ", report_percent_id(growth, 2L), ". ")
      } else {
        paste0("Dibandingkan dengan periode yang sama tahun sebelumnya, perekonomian mengalami kontraksi sebesar ", report_percent_id(abs(growth), 2L), ". ")
      },
      if (is.na(top_sector)) "Lapangan usaha utama belum dapat diidentifikasi." else paste0("Lapangan usaha dengan nilai terbesar adalah ", top_sector, ".")
    )

    executive_paragraph_2 <- paste0(
      if (!is.na(top5_share)) paste0("Lima lapangan usaha terbesar membentuk ", report_percent_id(top5_share, 2L), " dari total PDRB, sehingga struktur ekonomi masih terkonsentrasi pada beberapa lapangan usaha utama. ") else "",
      if (reference_available && !is.na(lq_basis_count)) paste0("Analisis LQ mengidentifikasi ", lq_basis_count, " sektor basis", if (!is.na(lq_nonbasis_count)) paste0(" dan ", lq_nonbasis_count, " sektor nonbasis") else "", ". ") else "",
      if (nrow(dlq_data) > 0L) paste0("Analisis DLQ menunjukkan ", dlq_leading_count, " sektor unggulan dan ", dlq_prospective_count, " sektor dengan DLQ di atas satu. ") else "",
      if (nrow(shift_data) > 0L) paste0("Dari sisi daya saing, ", shift_top_sector, " mencatat efek kompetitif tertinggi sebesar ", report_pdrb_card_id(shift_top_ce), ".") else ""
    )
    executive_text <- paste(executive_paragraph_1, executive_paragraph_2, sep = "\n\n")

    overview_text <- if (nrow(top_sector_values) > 0L) {
      paste0(
        "Gambaran umum perekonomian menunjukkan bahwa ", top_sector,
        " merupakan lapangan usaha dengan nilai PDRB ADHB terbesar, yaitu ", report_pdrb_card_id(top_sector_value),
        if (!is.na(top_share)) paste0(" atau sekitar ", report_percent_id(top_share, 2L), " dari total PDRB. ") else ". ",
        if (!is.na(second_sector) && !is.na(second_sector_value)) paste0("Posisi berikutnya ditempati oleh ", second_sector, " dengan nilai ", report_pdrb_card_id(second_sector_value), ".") else ""
      )
    } else "Data sektoral pada tahun dan periode fokus belum tersedia."

    trend_series_sentence <- function(indicator_name) {
      series <- trend_quarterly %>% filter(Indikator == indicator_name) %>% arrange(Urutan)
      if (nrow(series) < 2L) return(character())
      first_row <- series[1, , drop = FALSE]
      last_row <- series[nrow(series), , drop = FALSE]
      if (is.na(first_row$Nilai[[1]]) || first_row$Nilai[[1]] == 0) return(character())
      change_value <- (last_row$Nilai[[1]] - first_row$Nilai[[1]]) / abs(first_row$Nilai[[1]]) * 100
      direction <- if (change_value > 0) "meningkat" else if (change_value < 0) "menurun" else "relatif tidak berubah"
      paste0(
        indicator_name, " triwulanan ", direction, " sebesar ", report_percent_id(abs(change_value), 2L),
        ", dari ", report_pdrb_card_id(first_row$Nilai[[1]]), " pada ", period_label(first_row$Periode[[1]]), " ", first_row$Tahun[[1]],
        " menjadi ", report_pdrb_card_id(last_row$Nilai[[1]]), " pada ", period_label(last_row$Periode[[1]]), " ", last_row$Tahun[[1]], "."
      )
    }
    trend_parts <- c(trend_series_sentence("PDRB ADHB"), trend_series_sentence("PDRB ADHK"))
    trend_text <- if (length(trend_parts) > 0L) {
      paste0(
        paste(trend_parts, collapse = " "),
        " Grafik triwulanan dan tahunan disajikan terpisah agar nilai total tahunan tidak menimbulkan lonjakan semu pada seri triwulanan."
      )
    } else "Data tren triwulanan belum tersedia dalam jumlah yang memadai pada rentang yang dipilih."

    distribution_text <- if (nrow(distribution_values) > 0L) {
      shape_text <- if (is.na(distribution_mean) || is.na(distribution_median)) {
        ""
      } else if (abs(distribution_mean - distribution_median) / max(abs(distribution_mean), 1) < 0.05) {
        " Kedekatan rata-rata dan median menunjukkan sebaran yang relatif seimbang."
      } else if (distribution_mean > distribution_median) {
        " Rata-rata yang lebih tinggi daripada median menunjukkan adanya beberapa observasi bernilai relatif tinggi."
      } else {
        " Median yang lebih tinggi daripada rata-rata menunjukkan adanya beberapa observasi bernilai relatif rendah."
      }
      paste0(
        "Sebaran PDRB ADHB triwulanan terdiri atas ", nrow(distribution_values), " observasi, dengan rata-rata ", report_pdrb_card_id(distribution_mean),
        " dan median ", report_pdrb_card_id(distribution_median), ". Koefisien variasi sebesar ",
        ifelse(is.na(distribution_cv), "–", paste0(report_number_id(distribution_cv, 2L), "%")),
        " menunjukkan ", report_cv_interpretation_id(distribution_cv), ".", shape_text
      )
    } else "Distribusi data belum dapat disusun karena observasi triwulanan tidak tersedia."

    structure_text <- if (nrow(struktur) > 0L) {
      paste0(
        "Struktur ekonomi ", wilayah_label, " pada ", period_sentence, " didominasi oleh ", top_sector,
        " dengan kontribusi ", report_percent_id(top_share, 2L), ". ",
        if (!is.na(top5_share)) paste0("Secara kumulatif, lima lapangan usaha terbesar menyumbang ", report_percent_id(top5_share, 2L), " dari total PDRB.") else ""
      )
    } else "Struktur ekonomi belum dapat disusun untuk tahun dan periode fokus."

    comparison_text <- if (identical(comparison_mode, "reference") && !is.na(selected_value) && !is.na(province_value)) {
      paste0(
        "Pada ", period_sentence, ", PDRB ADHB ", wilayah_label, " tercatat sebesar ", report_pdrb_card_id(selected_value),
        ", atau setara ", report_percent_id(comparison_share, 2L), " dari PDRB ADHB ", provinsi_label,
        " sebesar ", report_pdrb_card_id(province_value), ". Nilai provinsi digunakan sebagai agregat pembanding, sehingga perbandingan ini dibaca sebagai proporsi kontribusi dan bukan sebagai pemeringkatan langsung."
      )
    } else if (identical(comparison_mode, "ranking") && !is.na(comparison_selected_rank)) {
      paste0(
        wilayah_label, " berada pada peringkat ke-", comparison_selected_rank,
        " dari ", nrow(comparison_subregions), " kabupaten/kota yang memiliki data pada ", period_sentence, ". ",
        "Nilai tertinggi di antara kabupaten/kota dicatat oleh ", comparison_top, ", sedangkan nilai terendah dicatat oleh ", comparison_bottom, ". ",
        "Nilai provinsi digunakan sebagai agregat pembanding dan tidak termasuk dalam pemeringkatan."
      )
    } else if (identical(comparison_mode, "ranking")) {
      paste0(
        "Perbandingan mencakup ", nrow(comparison_subregions), " kabupaten/kota yang memiliki data pada ", period_sentence, ". ",
        "Nilai tertinggi dicatat oleh ", comparison_top, ", sedangkan nilai terendah dicatat oleh ", comparison_bottom, ". ",
        "Nilai provinsi digunakan sebagai agregat pembanding dan tidak termasuk dalam pemeringkatan kabupaten/kota."
      )
    } else "Perbandingan belum dapat dilakukan karena jumlah wilayah dengan data yang sebanding masih terbatas."

    lq_text <- if (nrow(lq_data) > 0L) {
      paste0(
        "Analisis Location Quotient menunjukkan terdapat ", lq_basis_count, " sektor basis. Nilai LQ tertinggi terdapat pada ", lq_top_sector,
        " sebesar ", report_number_id(lq_top_value, 4L), ", yang berarti peranan relatif sektor tersebut lebih besar dibandingkan wilayah pembanding. ",
        "Sektor basis utama meliputi ", lq_basis_names, "."
      )
    } else "Hasil Location Quotient belum tersedia untuk tahun dan periode fokus."

    dlq_text <- if (nrow(dlq_data) > 0L) {
      paste0(
        "Analisis Dynamic Location Quotient mengidentifikasi ", dlq_prospective_count, " sektor dengan DLQ di atas satu dan ", dlq_leading_count,
        " sektor unggulan pada periode tahunan ", dlq_tahun_awal, "–", dlq_tahun_akhir, ". ",
        if (!identical(dlq_unggulan_names, "belum tersedia")) paste0("Sektor yang masuk kelompok unggulan adalah ", dlq_unggulan_names, ".") else ""
      )
    } else "Hasil Dynamic Location Quotient belum tersedia pada rentang tahun yang dipilih."

    shift_text <- if (nrow(shift_data) > 0L) {
      paste0(
        "Hasil Extended Shift Share periode ", shift_tahun_awal, "–", shift_tahun_akhir, " menunjukkan bahwa efek kompetitif (Competitive Effect) tertinggi terdapat pada ",
        shift_top_sector, ", yaitu sebesar ", report_pdrb_card_id(shift_top_ce), ". ",
        "Sebanyak ", shift_positive_count, " sektor memiliki CE positif dan ", shift_negative_count, " sektor memiliki CE negatif."
      )
    } else "Hasil Extended Shift Share belum tersedia pada rentang tahun yang dipilih."

    overview_analysis <- paste0(
      if (!is.na(top5_share)) paste0("Kontribusi lima lapangan usaha terbesar mencapai ", report_percent_id(top5_share, 2L), ". ") else "",
      if (!is.na(top_share_gap) && !is.na(second_sector)) paste0("Selisih kontribusi antara ", top_sector, " dan ", second_sector, " mencapai ", report_number_id(top_share_gap, 2L), " poin persentase, yang menunjukkan dominasi sektor utama cukup kuat. ") else "",
      if (!is.na(adhb_adhk_ratio)) paste0("Nilai PDRB ADHB sekitar ", report_number_id(adhb_adhk_ratio, 2L), " kali PDRB ADHK. Perbedaan tersebut menunjukkan bahwa perkembangan nilai nominal tidak hanya dipengaruhi oleh perubahan volume produksi, tetapi juga oleh perubahan harga.") else ""
    )

    trend_analysis <- paste0(
      if (!is.na(trend_adhb_detail$min_value) && !is.na(trend_adhb_detail$max_value)) paste0("PDRB ADHB triwulanan terendah tercatat pada ", trend_adhb_detail$min_period, " sebesar ", report_pdrb_card_id(trend_adhb_detail$min_value), ", sedangkan nilai tertinggi terjadi pada ", trend_adhb_detail$max_period, " sebesar ", report_pdrb_card_id(trend_adhb_detail$max_value), ". ") else "",
      if (!is.na(trend_adhb_detail$cagr) && !is.na(trend_adhk_detail$cagr)) paste0("Dalam seri tahunan yang lengkap, PDRB ADHB tumbuh rata-rata ", report_percent_id(trend_adhb_detail$cagr, 2L), " per tahun dan PDRB ADHK tumbuh rata-rata ", report_percent_id(trend_adhk_detail$cagr, 2L), " per tahun. ") else "",
      if (!is.na(trend_adhb_detail$qoq) && !is.na(trend_adhk_detail$qoq)) paste0("Pada observasi triwulanan terakhir, perubahan terhadap triwulan sebelumnya sebesar ", report_percent_id(trend_adhb_detail$qoq, 2L), " untuk ADHB dan ", report_percent_id(trend_adhk_detail$qoq, 2L), " untuk ADHK. ") else "",
      if (!is.na(trend_adhb_detail$cagr) && !is.na(trend_adhk_detail$cagr) && trend_adhb_detail$cagr > trend_adhk_detail$cagr) "Pertumbuhan nominal yang lebih cepat daripada pertumbuhan riil menunjukkan adanya pengaruh perubahan harga dalam kenaikan PDRB." else ""
    )

    distribution_analysis <- paste0(
      if (!is.na(distribution_range)) paste0("Rentang antara nilai minimum dan maksimum mencapai ", report_pdrb_card_id(distribution_range), ", dengan nilai terendah pada ", distribution_min_period, " dan nilai tertinggi pada ", distribution_max_period, ". ") else "",
      if (!is.na(distribution_cv)) paste0("Koefisien variasi ", report_number_id(distribution_cv, 2L), "% menunjukkan bahwa fluktuasi triwulanan masih berada pada tingkat ", ifelse(distribution_cv <= 20, "yang relatif terkendali", "yang cukup tinggi"), ". ") else "",
      "Hasil distribusi bersifat deskriptif; pola sebaran tidak secara langsung menunjukkan penyebab perubahan ekonomi."
    )

    structure_analysis <- paste0(
      if (!is.na(top5_share)) paste0("Sebesar ", report_percent_id(top5_share, 2L), " PDRB dibentuk oleh lima lapangan usaha terbesar. ") else "",
      if (!is.na(top_share_gap) && !is.na(second_sector)) paste0("Kontribusi ", top_sector, " lebih tinggi ", report_number_id(top_share_gap, 2L), " poin persentase dibandingkan ", second_sector, ". ") else "",
      "Struktur yang terkonsentrasi memberi kekuatan pada sektor utama, tetapi juga menunjukkan perlunya penguatan sektor lain agar sumber pertumbuhan lebih beragam."
    )

    comparison_analysis <- if (identical(comparison_mode, "reference") && !is.na(comparison_share)) {
      paste0(
        "Kontribusi ", wilayah_label, " terhadap agregat ", provinsi_label, " sebesar ", report_percent_id(comparison_share, 2L), ". ",
        if (!is.na(comparison_gap)) paste0("Selisih nilai dengan agregat provinsi sebesar ", report_pdrb_card_id(comparison_gap), ". ") else "",
        "Karena provinsi merupakan agregasi seluruh kabupaten/kota, selisih ini tidak menunjukkan ketertinggalan secara langsung, melainkan perbedaan cakupan wilayah dan skala ekonomi."
      )
    } else if (identical(comparison_mode, "ranking")) {
      paste0("Posisi pemeringkatan menunjukkan skala PDRB relatif terhadap kabupaten/kota lain pada periode yang sama. Interpretasi tetap perlu mempertimbangkan perbedaan jumlah penduduk, struktur sektor, dan ukuran ekonomi tiap wilayah.")
    } else "Analisis komparatif belum cukup kuat karena jumlah wilayah pembanding masih terbatas."

    lq_analysis <- if (nrow(lq_data) > 0L) {
      paste0(
        "Nilai LQ yang tinggi menunjukkan spesialisasi relatif, bukan selalu menunjukkan nilai PDRB absolut terbesar. ",
        if (!is.na(top_sector) && !is.na(lq_top_sector) && !identical(top_sector, lq_top_sector)) paste0("Dalam laporan ini, ", lq_top_sector, " memiliki LQ tertinggi, sedangkan nilai PDRB absolut terbesar tetap berasal dari ", top_sector, ". ") else "",
        "Sektor basis dapat dipandang sebagai sektor yang memiliki peranan relatif lebih kuat dan berpotensi melayani kebutuhan di luar wilayah."
      )
    } else ""

    dlq_analysis <- if (nrow(dlq_data) > 0L) {
      paste0(
        if (!identical(dlq_unggulan_names, "belum tersedia")) paste0("Kelompok unggulan terdiri atas ", dlq_unggulan_names, "; sektor-sektor ini berstatus basis dan memiliki prospek pertumbuhan relatif yang baik. ") else "",
        if (!identical(dlq_andalan_names, "belum tersedia")) paste0("Kelompok andalan mencakup ", dlq_andalan_names, ", yaitu sektor yang belum basis tetapi menunjukkan arah perkembangan yang prospektif. ") else "",
        if (!identical(dlq_prospektif_names, "belum tersedia")) paste0("Kelompok prospektif mencakup ", dlq_prospektif_names, "; sektor ini masih basis, tetapi laju perkembangannya relatif perlu dijaga. ") else "",
        if (!identical(dlq_kurang_names, "belum tersedia")) paste0("Sektor kurang prospektif meliputi ", dlq_kurang_names, " dan memerlukan perhatian lebih lanjut sebelum dijadikan prioritas.") else ""
      )
    } else ""

    shift_analysis <- if (nrow(shift_data) > 0L) {
      paste0(
        "CE positif menunjukkan keunggulan kompetitif relatif, sedangkan CE negatif menunjukkan sektor tumbuh lebih lemah dibandingkan pola wilayah pembanding. ",
        if (!is.na(shift_bottom_sector) && !is.na(shift_bottom_ce) && shift_bottom_ce < 0) paste0("CE terendah terdapat pada ", shift_bottom_sector, " sebesar ", report_pdrb_card_id(shift_bottom_ce), ". ") else "",
        if (!is.na(shift_dominant_type) && !is.na(shift_dominant_count)) paste0("Tipe yang paling banyak muncul adalah ", shift_dominant_type, " pada ", shift_dominant_count, " sektor. ", report_shift_diagnosis_short(shift_dominant_type), " ") else "",
        "Hasil ini dapat digunakan untuk membedakan sektor yang perlu dipertahankan daya saingnya dari sektor yang memerlukan penguatan produktivitas dan keterkaitan ekonomi."
      )
    } else ""

    conclusion_paragraph_1 <- paste0(
      "Pada ", period_sentence, ", perekonomian ", wilayah_label,
      if (is.na(growth)) {
        " belum dapat dinilai berdasarkan pertumbuhan Y-on-Y karena periode pembanding belum tersedia. "
      } else if (growth >= 0) {
        paste0(" tumbuh sebesar ", report_percent_id(growth, 2L), ". ")
      } else {
        paste0(" mengalami kontraksi sebesar ", report_percent_id(abs(growth), 2L), ". ")
      },
      if (is.na(top_sector)) {
        "Struktur sektoral belum dapat disimpulkan."
      } else {
        paste0("Struktur ekonomi didominasi oleh ", top_sector, if (!is.na(top_share)) paste0(" dengan kontribusi ", report_percent_id(top_share, 2L), ". ") else ". ", if (!is.na(top5_share)) paste0("Lima sektor terbesar secara bersama-sama menyumbang ", report_percent_id(top5_share, 2L), ".") else "")
      }
    )

    conclusion_paragraph_2 <- paste0(
      if (reference_available && nrow(lq_data) > 0L) paste0("Analisis LQ mengidentifikasi ", lq_basis_count, " sektor basis. ") else "",
      if (nrow(dlq_data) > 0L) paste0("DLQ menunjukkan ", dlq_leading_count, " sektor unggulan, dengan sektor unggulan utama meliputi ", dlq_unggulan_names, ". ") else "",
      if (nrow(shift_data) > 0L) paste0("Extended Shift Share menempatkan ", shift_top_sector, " sebagai sektor dengan efek kompetitif tertinggi sebesar ", report_pdrb_card_id(shift_top_ce), ". ") else "",
      "Secara analitis, arah pengembangan dapat mempertimbangkan tiga fokus: menjaga produktivitas sektor dominan, memperkuat sektor unggulan dan berdaya saing, serta meningkatkan kapasitas sektor yang prospeknya belum stabil."
    )
    conclusion_text <- paste(conclusion_paragraph_1, conclusion_paragraph_2, sep = "\n\n")

    list(
      meta = list(
        wilayah = wilayah_label,
        wilayah_id = wilayah_id,
        provinsi = provinsi_label,
        jenis_wilayah = jenis_wilayah,
        tahun = tahun_fokus,
        periode = focus_period_label,
        periode_kode = focus_period,
        periode_narasi = period_sentence,
        periode_sampul = period_cover,
        tahun_awal = tahun_awal,
        tahun_akhir = tahun_akhir,
        dlq_tahun_awal = dlq_tahun_awal,
        dlq_tahun_akhir = dlq_tahun_akhir,
        shift_tahun_awal = shift_tahun_awal,
        shift_tahun_akhir = shift_tahun_akhir,
        tanggal = report_date_id(),
        jenis = report_type,
        jenis_label = ifelse(identical(report_type, "lengkap"), "Lengkap", "Ringkas"),
        potential_available = reference_available
      ),
      overview_kpi = overview_kpi,
      data_status = data_status,
      top_sector_values = top_sector_values,
      trend_quarterly = trend_quarterly,
      trend_annual = trend_annual,
      distribution_values = distribution_values,
      distribution_stats = distribution_stats,
      struktur = struktur,
      comparison = comparison,
      comparison_summary = list(
        selected_rank = comparison_selected_rank,
        top = comparison_top,
        bottom = comparison_bottom,
        province_value = province_value,
        subregion_average = subregion_average,
        selected_value = selected_value,
        share_of_province = comparison_share,
        mode = comparison_mode
      ),
      lq = lq_data,
      dlq = dlq_data,
      shift = shift_data,
      validation_notes = validation_notes,
      narratives = list(
        executive = executive_text,
        overview = overview_text,
        trend = trend_text,
        distribution = distribution_text,
        structure = structure_text,
        comparison = comparison_text,
        lq = lq_text,
        dlq = dlq_text,
        shift = shift_text,
        conclusion = conclusion_text
      ),
      analysis_notes = list(
        overview = overview_analysis,
        trend = trend_analysis,
        distribution = distribution_analysis,
        structure = structure_analysis,
        comparison = comparison_analysis,
        lq = lq_analysis,
        dlq = dlq_analysis,
        shift = shift_analysis
      )
    )
  }

  output$laporan_preview <- renderUI({
    req(input$laporan_wilayah, input$laporan_tahun, input$laporan_periode)
    bundle <- make_report_bundle()
    tags$div(
      class = "mini-guide-panel",
      tags$h4(tagList(icon("file-lines"), " Preview Laporan")),
      tags$p(strong("Wilayah: "), bundle$meta$wilayah),
      tags$p(strong("Tahun dan periode fokus: "), bundle$meta$tahun, " · ", bundle$meta$periode),
      tags$p(strong("Rentang analisis: "), bundle$meta$tahun_awal, "–", bundle$meta$tahun_akhir),
      tags$p(
        strong("Periode DLQ: "),
        if (!is.na(bundle$meta$dlq_tahun_awal) && !is.na(bundle$meta$dlq_tahun_akhir)) {
          paste0(bundle$meta$dlq_tahun_awal, "–", bundle$meta$dlq_tahun_akhir)
        } else {
          paste0(bundle$meta$tahun - 1L, "–", bundle$meta$tahun, " (data tahunan lengkap belum tersedia)")
        }
      ),
      tags$p(strong("Jenis laporan: "), bundle$meta$jenis_label),
      tags$hr(),
      tags$p(
        strong("Cakupan: "),
        "Ringkasan eksekutif, Overview, Tren PDRB triwulanan dan tahunan, Distribusi Data, Struktur Ekonomi, Komparasi Wilayah, LQ, DLQ, Extended Shift Share, kesimpulan, dan catatan validasi."
      ),
      if (!isTRUE(bundle$meta$potential_available)) {
        tags$p(class = "text-muted", "Bagian LQ, DLQ, dan Extended Shift Share akan berisi catatan ketersediaan karena wilayah pembanding belum tersedia atau wilayah laporan adalah provinsi/agregat.")
      }
    )
  })

  pdrb_find_quarto_cli <- function() {
    candidates <- c(
      Sys.getenv("QUARTO_PATH", unset = ""),
      unname(Sys.which("quarto")),
      file.path(Sys.getenv("LOCALAPPDATA", unset = ""), "Programs", "Quarto", "bin", "quarto.exe"),
      file.path(Sys.getenv("ProgramFiles", unset = ""), "Quarto", "bin", "quarto.exe"),
      file.path(Sys.getenv("ProgramFiles", unset = ""), "RStudio", "resources", "app", "bin", "quarto", "bin", "quarto.exe")
    )
    candidates <- unique(candidates[nzchar(candidates)])
    expanded <- unlist(lapply(candidates, function(x) {
      if (dir.exists(x)) c(file.path(x, "quarto.exe"), file.path(x, "quarto")) else x
    }), use.names = FALSE)
    valid <- expanded[file.exists(expanded)]
    if (length(valid) > 0L) normalizePath(valid[1], winslash = "/", mustWork = TRUE) else ""
  }

  pdrb_validate_rendered_report <- function(path, fmt) {
    if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size <= 0) {
      stop("Berkas laporan tidak terbentuk atau berukuran kosong.")
    }
    if (identical(fmt, "docx")) {
      signature <- readBin(path, what = "raw", n = 2L)
      if (length(signature) < 2L || !identical(as.integer(signature), c(80L, 75L))) {
        stop("Keluaran Word tidak valid. File DOCX seharusnya berformat ZIP/OpenXML.")
      }
    }
    if (identical(fmt, "pdf")) {
      con <- file(path, open = "rb")
      on.exit(close(con), add = TRUE)
      signature <- rawToChar(readBin(con, what = "raw", n = 5L))
      if (!identical(signature, "%PDF-")) {
        stop("Keluaran PDF tidak valid.")
      }
    }
    invisible(TRUE)
  }

  output$download_laporan_pdrb <- downloadHandler(
    filename = function() {
      fmt <- if (identical(input$laporan_format, "pdf")) "pdf" else "docx"
      wilayah_file <- if (is.null(input$laporan_wilayah)) "wilayah" else gsub("[^A-Za-z0-9]+", "_", as.character(input$laporan_wilayah)[1])
      paste0("laporan_pdrb_", wilayah_file, "_", format(Sys.Date(), "%Y%m%d"), ".", fmt)
    },
    contentType = "application/octet-stream",
    content = function(file) {
      req(
        input$laporan_wilayah,
        input$laporan_tahun,
        input$laporan_periode,
        input$laporan_tahun_awal,
        input$laporan_tahun_akhir,
        input$laporan_jenis,
        input$laporan_format
      )

      bundle <- make_report_bundle()
      fmt <- if (identical(input$laporan_format, "pdf")) "pdf" else "docx"
      tmpdir <- tempfile("laporan_pdrb_")
      dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

      saveRDS(bundle, file.path(tmpdir, "bundle.rds"))

      template_source <- normalizePath("reports/template_laporan.qmd", winslash = "/", mustWork = TRUE)
      qmd <- file.path(tmpdir, "laporan_pdrb.qmd")
      if (!isTRUE(file.copy(template_source, qmd, overwrite = TRUE))) {
        stop("Template laporan tidak dapat disalin ke folder kerja sementara.")
      }

      reference_source <- file.path("reports", "reference_laporan_pdrb.docx")
      if (file.exists(reference_source)) {
        file.copy(reference_source, file.path(tmpdir, "reference_laporan_pdrb.docx"), overwrite = TRUE)
      }
      out_name <- paste0("laporan_pdrb.", fmt)
      out <- file.path(tmpdir, out_name)
      render_messages <- character()
      render_error <- NULL

      # Jalur utama: package quarto lebih andal menemukan Quarto CLI pada Windows/RStudio.
      if (requireNamespace("quarto", quietly = TRUE)) {
        render_error <- tryCatch({
          quarto::quarto_render(
            input = qmd,
            output_format = fmt,
            output_file = out_name,
            execute_dir = tmpdir,
            quiet = TRUE,
            as_job = FALSE
          )
          NULL
        }, error = function(e) e)
      }

      # Fallback: panggil executable Quarto secara eksplisit bila package quarto belum tersedia
      # atau render melalui package quarto gagal.
      if (!file.exists(out)) {
        quarto_cli <- pdrb_find_quarto_cli()
        if (!nzchar(quarto_cli)) {
          package_detail <- if (inherits(render_error, "error")) paste0(" Detail package quarto: ", conditionMessage(render_error)) else ""
          stop(
            "Quarto tidak ditemukan oleh aplikasi. Instal package R 'quarto' dan pastikan Quarto CLI dapat dijalankan dari RStudio.",
            package_detail
          )
        }

        oldwd <- getwd()
        on.exit(setwd(oldwd), add = TRUE)
        setwd(tmpdir)
        cli_result <- tryCatch(
          system2(
            command = quarto_cli,
            args = c("render", "laporan_pdrb.qmd", "--to", fmt, "--output", out_name),
            stdout = TRUE,
            stderr = TRUE
          ),
          error = function(e) e
        )
        if (inherits(cli_result, "error")) {
          render_error <- cli_result
        } else {
          render_messages <- as.character(cli_result)
          cli_status <- attr(cli_result, "status")
          if (!is.null(cli_status) && !identical(as.integer(cli_status), 0L)) {
            render_error <- simpleError(paste(render_messages, collapse = "\n"))
          }
        }
      }

      if (!file.exists(out)) {
        detail <- if (inherits(render_error, "error")) conditionMessage(render_error) else paste(render_messages, collapse = "\n")
        stop(
          "Gagal membuat laporan ", toupper(fmt), ". ",
          "Periksa instalasi Quarto dan package yang diperlukan oleh template laporan. Detail: ",
          detail
        )
      }

      pdrb_validate_rendered_report(out, fmt)
      if (!isTRUE(file.copy(out, file, overwrite = TRUE))) {
        stop("Laporan berhasil dibuat, tetapi gagal disalin ke lokasi unduhan.")
      }
    }
  )


  # Output Plotly disuspensikan ketika tab tersembunyi agar tidak menghitung ulang
  # memakai input kanonik milik menu Analytics lain. Ketika tab dibuka kembali,
  # Shiny melanjutkan output dan JavaScript hanya memulihkan ukuran grafik terlihat.
  pdrb_plotly_outputs <- c(
    "plot_trend_pdrb_overview",
    "plot_trend_adhb_overview",
    "plot_trend_adhk_overview",
    "plot_top_adhb",
    "plot_top_adhk",
    "plot_trend",
    "plot_kernel_histogram",
    "plot_structure",
    "plot_structure_bar",
    "plot_compare",
    "plot_lq_v5",
    "plot_dlq_v5",
    "plot_shiftshare_v5"
  )
  invisible(lapply(pdrb_plotly_outputs, function(output_id) {
    outputOptions(output, output_id, suspendWhenHidden = TRUE)
  }))

  outputOptions(output, "structure_table", suspendWhenHidden = TRUE)
  
}
