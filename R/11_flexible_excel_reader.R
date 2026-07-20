# Reader fleksibel untuk enam pola workbook uji.
# File ini dimuat setelah 10_core_data_indikator.R dan hanya mengganti fungsi reader.

pdrb_role_matches_code <- function(kode_wilayah, role) {
  kode_wilayah <- as.character(kode_wilayah)
  role <- as.character(role)[1]
  is_agregat <- !is.na(kode_wilayah) & stringr::str_detect(kode_wilayah, "^[0-9]{2}00$")
  if (identical(role, "Provinsi/Agregat")) return(is_agregat)
  if (identical(role, "Kabupaten/Kota")) return(!is_agregat)
  rep(TRUE, length(kode_wilayah))
}

parse_sheet <- function(path, sheet_name, region_info) {
  raw <- readxl::read_excel(
    path, sheet = sheet_name, col_names = FALSE,
    col_types = "text", .name_repair = "minimal"
  )
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (nrow(raw) == 0 || ncol(raw) == 0 || all(is.na(unlist(raw, use.names = FALSE)))) {
    return(tibble::tibble())
  }

  blocks <- detect_table_blocks(raw, sheet_name)
  if (nrow(blocks) == 0) return(tibble::tibble())

  errors <- character()
  parsed_parts <- vector("list", nrow(blocks))
  for (i in seq_len(nrow(blocks))) {
    parsed_parts[[i]] <- tryCatch(
      parse_table_block(raw, region_info, blocks[i, , drop = FALSE]),
      error = function(e) {
        errors <<- c(errors, paste0(blocks$indikator[[i]], ": ", conditionMessage(e)))
        tibble::tibble()
      }
    )
  }
  parsed <- dplyr::bind_rows(parsed_parts)
  if (!has_required_columns(parsed, "indikator")) return(tibble::tibble())

  parsed %>%
    dplyr::filter(indikator %in% c("PDRB ADHB", "PDRB ADHK")) %>%
    dplyr::mutate(source_sheet = as.character(sheet_name))
}

pdrb_detected_table_label <- function(title_text, block_class = NA_character_, indikator = NA_character_) {
  text <- normalize_indicator_text(title_text)
  if (!is.na(indikator) && nzchar(indikator)) {
    return(dplyr::recode(as.character(indikator), "PDRB ADHB" = "ADHB", "PDRB ADHK" = "ADHK", .default = as.character(indikator)))
  }
  if (is.na(text) || !nzchar(text)) {
    if (identical(block_class, "ignore_derived")) return("Tabel turunan")
    return("Tidak dikenali")
  }
  dplyr::case_when(
    stringr::str_detect(text, "DYNAMIC LOCATION QUOTIENT|(^|[^A-Z])DLQ([^A-Z]|$)") ~ "Tabel turunan - DLQ",
    stringr::str_detect(text, "LOCATION QUOTIENT|(^|[^A-Z])LQ([^A-Z]|$)") ~ "Tabel turunan - LQ",
    stringr::str_detect(text, "Extended Shift Share|(^|[^A-Z])RIE([^A-Z]|$)|(^|[^A-Z])RSE([^A-Z]|$)|(^|[^A-Z])RCCE([^A-Z]|$)") ~ "Tabel turunan - Extended Shift Share",
    stringr::str_detect(text, "SUMBER PERTUMBUHAN") ~ "Tabel turunan - Sumber Pertumbuhan",
    stringr::str_detect(text, "INDEKS IMPLISIT|IMPLISIT") ~ "Tabel turunan - Indeks Implisit",
    stringr::str_detect(text, "PERTUMBUHAN|Q\\s*-?\\s*TO\\s*-?\\s*Q|Y\\s*-?\\s*ON\\s*-?\\s*Y|C\\s*-?\\s*TO\\s*-?\\s*C") ~ "Tabel turunan - Pertumbuhan",
    stringr::str_detect(text, "DISTRIBUSI|KONTRIBUSI|STRUKTUR EKONOMI") ~ "Tabel turunan - Distribusi",
    identical(block_class, "ignore_derived") ~ "Tabel turunan",
    TRUE ~ "Tidak dikenali"
  )
}

pdrb_sheet_validation_rows <- function(path, source_name, sheet_index, region_lookup, role = NA_character_) {
  all_sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
  if (length(all_sheets) == 0) return(tibble::tibble())

  helper_names <- c(
    "wilayah", "metadata_wilayah", "referensi_wilayah", "region",
    "petunjuk", "readme", "panduan", "keterangan"
  )

  purrr::map_dfr(all_sheets, function(sheet_name) {
    normalized_sheet <- stringr::str_to_lower(stringr::str_trim(sheet_name))
    if (normalized_sheet %in% helper_names) {
      helper_type <- if (normalized_sheet %in% c("wilayah", "metadata_wilayah", "referensi_wilayah", "region")) {
        "Sheet pendukung - Metadata wilayah"
      } else {
        "Sheet pendukung - Petunjuk"
      }
      return(tibble::tibble(
        source_file = source_name,
        sheet_name = as.character(sheet_name),
        kode_wilayah = NA_character_,
        status = "Diabaikan: Sheet Pendukung",
        reason = "Sheet pendukung tidak dijadikan baris data analisis PDRB.",
        jumlah_baris = 0L,
        detected_table_type = helper_type
      ))
    }

    index_row <- sheet_index %>%
      dplyr::filter(as.character(sheet_name) == as.character(.env$sheet_name)) %>%
      dplyr::slice(1)
    code <- if (nrow(index_row) == 0) NA_character_ else as.character(index_row$kode_wilayah[[1]])
    info <- if (is.na(code)) tibble::tibble() else {
      region_lookup %>% dplyr::filter(kode_wilayah == code) %>% dplyr::slice(1)
    }
    role_ok <- if (is.na(code)) TRUE else pdrb_role_matches_code(code, role)[[1]]

    raw <- tryCatch(
      readxl::read_excel(path, sheet = sheet_name, col_names = FALSE, col_types = "text", .name_repair = "minimal"),
      error = function(e) NULL
    )

    if (is.null(raw)) {
      return(tibble::tibble(
        source_file = source_name, sheet_name = as.character(sheet_name), kode_wilayah = code,
        status = "File Gagal Dibaca", reason = "Sheet tidak dapat dibuka oleh pembaca Excel.",
        jumlah_baris = 0L, detected_table_type = "Sheet Excel"
      ))
    }

    raw_df <- as.data.frame(raw, stringsAsFactors = FALSE)
    if (nrow(raw_df) == 0 || ncol(raw_df) == 0 || all(is.na(unlist(raw_df, use.names = FALSE)))) {
      return(tibble::tibble(
        source_file = source_name, sheet_name = as.character(sheet_name), kode_wilayah = code,
        status = "Diabaikan: Sheet Kosong", reason = "Sheet kosong sehingga tidak memiliki data yang perlu diproses.",
        jumlah_baris = 0L, detected_table_type = "Sheet kosong"
      ))
    }

    scan <- tryCatch(
      v5_scan_sheet_blocks(path, sheet_name, source_name),
      error = function(e) tibble::tibble()
    )

    derived_rows <- if (nrow(scan) == 0) {
      tibble::tibble()
    } else {
      scan %>%
        dplyr::filter(block_class == "ignore_derived") %>%
        dplyr::mutate(
          detected_table_type = purrr::pmap_chr(
            list(title_text, block_class, indikator),
            pdrb_detected_table_label
          )
        ) %>%
        dplyr::transmute(
          source_file = source_name,
          sheet_name = as.character(sheet_name),
          kode_wilayah = code,
          status = "Diabaikan: Tabel Turunan",
          reason = dplyr::coalesce(
            as.character(reason),
            "Tabel turunan diabaikan karena dashboard menghitung indikator tersebut dari data mentah ADHB/ADHK."
          ),
          jumlah_baris = 0L,
          detected_table_type = detected_table_type
        ) %>%
        dplyr::distinct()
    }

    raw_classes <- if (nrow(scan) == 0) character(0) else unique(as.character(scan$block_class))
    has_raw_block <- any(raw_classes %in% c("raw_adhb", "raw_adhk"))
    has_only_derived <- nrow(derived_rows) > 0 && !has_raw_block

    if (has_only_derived) return(derived_rows)

    blocks <- tryCatch(detect_table_blocks(raw_df, sheet_name), error = function(e) tibble::tibble())
    parsed <- if (nrow(blocks) == 0 || nrow(info) == 0 || !role_ok) {
      tibble::tibble()
    } else {
      tryCatch(parse_sheet(path, sheet_name, info), error = function(e) tibble::tibble())
    }

    parsed_types <- if (nrow(parsed) == 0 || !"indikator" %in% names(parsed)) {
      character(0)
    } else {
      sort(unique(dplyr::recode(as.character(parsed$indikator), "PDRB ADHB" = "ADHB", "PDRB ADHK" = "ADHK", .default = as.character(parsed$indikator))))
    }

    main_status <- dplyr::case_when(
      !role_ok ~ "Diabaikan karena Slot Tidak Sesuai",
      is.na(code) || nrow(info) == 0 ~ "Wilayah Tidak Dikenali",
      nrow(blocks) == 0 ~ "Tidak Ada Tabel ADHB/ADHK",
      nrow(parsed) == 0 ~ "Tidak Ada Nilai Valid",
      TRUE ~ "Berhasil Dibaca"
    )
    main_reason <- dplyr::case_when(
      !role_ok ~ paste0("Kode ", code, " tidak sesuai dengan slot ", role, "."),
      is.na(code) || nrow(info) == 0 ~ "Kode/nama wilayah tidak ditemukan pada referensi wilayah.",
      nrow(blocks) == 0 ~ "Judul ADHB/ADHK atau header tahun-periode tidak dapat dikenali.",
      nrow(parsed) == 0 ~ "Blok ditemukan, tetapi tidak memiliki nilai numerik valid.",
      TRUE ~ "Sheet berhasil dinormalisasi menjadi data long PDRB ADHB/ADHK."
    )
    main_type <- if (length(parsed_types) > 0) {
      paste(parsed_types, collapse = ", ")
    } else if (has_raw_block) {
      raw_indicators <- scan %>%
        dplyr::filter(block_class %in% c("raw_adhb", "raw_adhk")) %>%
        dplyr::pull(indikator) %>%
        unique()
      paste(vapply(
        raw_indicators,
        function(indicator_name) pdrb_detected_table_label(
          title_text = NA_character_,
          block_class = NA_character_,
          indikator = indicator_name
        ),
        character(1)
      ), collapse = ", ")
    } else {
      "Tidak dikenali"
    }

    main_row <- tibble::tibble(
      source_file = source_name,
      sheet_name = as.character(sheet_name),
      kode_wilayah = code,
      status = main_status,
      reason = main_reason,
      jumlah_baris = as.integer(nrow(parsed)),
      detected_table_type = main_type
    )

    dplyr::bind_rows(main_row, derived_rows)
  })
}


read_pdrb_workbook <- function(path, source_name = basename(path), role = NA_character_,
                               progress_callback = NULL, file_index = 1L, total_files = 1L) {
  sheet_index <- build_sheet_index(path)
  region_lookup <- build_region_lookup(path, sheet_index %>% dplyr::distinct(kode_wilayah, .keep_all = TRUE))
  validation <- pdrb_sheet_validation_rows(path, source_name, sheet_index, region_lookup, role)

  parsed_parts <- vector("list", nrow(sheet_index))
  for (sheet_no in seq_len(nrow(sheet_index))) {
    sheet_name <- sheet_index$sheet_name[[sheet_no]]
    sheet_code <- as.character(sheet_index$kode_wilayah[[sheet_no]])
    role_ok <- pdrb_role_matches_code(sheet_code, role)[[1]]

    if (role_ok) {
      region_info <- region_lookup %>% dplyr::filter(kode_wilayah == sheet_code) %>% dplyr::slice(1)
      parsed_parts[[sheet_no]] <- if (nrow(region_info) == 0) tibble::tibble() else {
        tryCatch(parse_sheet(path, sheet_name, region_info), error = function(e) tibble::tibble())
      }
    } else {
      parsed_parts[[sheet_no]] <- tibble::tibble()
    }

    if (is.function(progress_callback)) {
      progress_callback(
        file_name = source_name, sheet_name = sheet_name,
        sheet_no = sheet_no, sheet_total = nrow(sheet_index),
        file_index = file_index, file_total = total_files
      )
    }
  }

  all_data <- dplyr::bind_rows(parsed_parts)
  if (nrow(all_data) == 0) {
    status_text <- paste(unique(validation$status), collapse = ", ")
    stop(
      "Tidak ada sheet yang berhasil dibaca dari file `", source_name, "` pada slot `", role,
      "`. Status: ", status_text, ".",
      call. = FALSE
    )
  }

  all_data <- all_data %>% dplyr::mutate(source_file = source_name)
  active_raw_excel <- all_data %>%
    dplyr::filter(indikator %in% c("PDRB ADHB", "PDRB ADHK")) %>%
    dplyr::mutate(periode = as.character(periode), level = as.character(level)) %>%
    filter_available_pdrb_periods()

  active_sektor_input <- active_raw_excel %>% dplyr::filter(!is_excel_total_pdrb_row(.))
  active_raw <- prepare_pdrb_input_with_auto_totals(active_raw_excel) %>%
    filter_available_pdrb_periods() %>%
    filter_complete_pdrb_value_periods()

  duplicate_diagnostics <- dplyr::bind_rows(
    diagnose_pdrb_duplicates(active_sektor_input, paste0("File ", source_name, " sektor input sebelum deduplikasi")),
    diagnose_pdrb_duplicates(active_raw, paste0("File ", source_name, " setelah PDRB otomatis"))
  )

  active_data <- active_raw %>%
    canonicalize_pdrb_rows() %>%
    dplyr::mutate(
      periode = factor(as.character(periode), levels = c("I", "II", "III", "IV", "Total"), ordered = TRUE),
      level = factor(as.character(level), levels = c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian", "Lainnya"))
    ) %>%
    dplyr::arrange(kode_kelompok, kode_wilayah, indikator, level, source_row, tahun, periode)

  used_codes <- unique(as.character(active_data$kode_wilayah))
  list(
    data = active_data,
    regions = region_lookup %>%
      dplyr::filter(kode_wilayah %in% used_codes) %>%
      dplyr::mutate(source_file = source_name),
    validation = validation,
    duplicate_diagnostics = duplicate_diagnostics
  )
}

read_multiple_workbooks <- function(paths, source_names, roles = NULL, progress_callback = NULL) {
  if (length(paths) == 0) stop("Tidak ada file Excel yang dipilih.")
  if (length(source_names) != length(paths)) source_names <- basename(paths)
  if (is.null(roles) || length(roles) != length(paths)) roles <- rep(NA_character_, length(paths))

  results <- list()
  failed_files <- character()
  failed_validation <- list()

  for (file_index in seq_along(paths)) {
    result <- tryCatch(
      read_pdrb_workbook(
        path = paths[[file_index]], source_name = source_names[[file_index]],
        role = roles[[file_index]], progress_callback = progress_callback,
        file_index = file_index, total_files = length(paths)
      ),
      error = function(e) {
        failed_files <<- c(failed_files, paste0("`", source_names[[file_index]], "`: ", pdrb_deep_error_message(e)))
        NULL
      }
    )
    if (!is.null(result)) results[[length(results) + 1L]] <- result
  }

  if (length(results) == 0) {
    stop("Tidak ada file yang berhasil dibaca. ", paste(failed_files, collapse = " || "), call. = FALSE)
  }

  all_regions <- dplyr::bind_rows(lapply(results, function(x) x$regions))
  all_data_raw <- dplyr::bind_rows(lapply(results, function(x) x$data))
  validation <- dplyr::bind_rows(lapply(results, function(x) x$validation))
  duplicate_diagnostics <- dplyr::bind_rows(
    dplyr::bind_rows(lapply(results, function(x) x$duplicate_diagnostics)),
    diagnose_pdrb_duplicates(all_data_raw, "Gabungan semua file sebelum deduplikasi")
  )
  all_data <- canonicalize_pdrb_rows(all_data_raw)

  list(
    data = all_data,
    regions = all_regions %>% dplyr::distinct(kode_wilayah, source_file, .keep_all = TRUE),
    validation = validation,
    duplicate_diagnostics = duplicate_diagnostics,
    failed_files = failed_files
  )
}
