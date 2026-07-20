# REVISI BERDASARKAN DOKUMEN ALGORITMA 1-16 DAN 17
# - Logika indikator yang telah ada tidak diubah.
# - Label periode tahunan ditampilkan sebagai "Tahun" (nilai internal tetap "Total").
# - Distribusi Data memakai Semua Triwulan, histogram frekuensi rapat, dan kurva kernel.
# - Modularisasi penuh Algoritma 17 diterapkan setelah fungsi dashboard dinyatakan stabil.

# Paket utama
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(plotly)
library(DT)
library(scales)

# Komponen dropdown standar untuk UI dinamis di server.
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

# Komponen update dropdown standar.
update_pdrb_selectize <- function(session, id, choices = NULL, selected = NULL, placeholder = NULL, server_side = TRUE) {
  shiny::updateSelectizeInput(
    session = session,
    inputId = id,
    choices = choices,
    selected = selected,
    options = list(dropdownParent = "body", placeholder = placeholder),
    server = server_side
  )
}

normalize_lq_level <- function(level_input) {
  allowed <- c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian", "Semua")
  level_input <- as.character(level_input)[1]
  if (is.na(level_input) || !level_input %in% allowed) return("Kategori Utama")
  level_input
}

shift_mode_value <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x) || !x %in% c("biasa", "Extended")) "biasa" else x
}

shift_components_for_mode <- function(mode) {
  if (identical(shift_mode_value(mode), "Extended")) {
    c("NE", "IM", "CE", "RIE", "RSE", "RCCE", "Total Perubahan")
  } else {
    c("NE", "IM", "CE", "Total Perubahan")
  }
}


# Nama wilayah dibaca dari isi sheet (`Wilayah : ...`) atau kode wilayah. Nama sheet bebas.
# Metadata wilayah hanya menjadi opsi tambahan.
# Fallback teknis agar aplikasi tidak terkunci pada wilayah tertentu.
fallback_region_map <- tibble(
  kode_wilayah = character(),
  wilayah = character(),
  kode_kelompok = character(),
  kelompok = character(),
  jenis_wilayah = character()
)

region_name_reference <- tibble::tribble(
  ~kode_wilayah, ~nama_wilayah_referensi, ~kode_kelompok_referensi, ~jenis_wilayah_referensi,
  "3600", "Banten",                       "3600", "Provinsi/Agregat",
  "3601", "Kabupaten Pandeglang",         "3600", "Kabupaten/Kota",
  "3602", "Kabupaten Lebak",              "3600", "Kabupaten/Kota",
  "3603", "Kabupaten Tangerang",          "3600", "Kabupaten/Kota",
  "3604", "Kabupaten Serang",             "3600", "Kabupaten/Kota",
  "3671", "Kota Tangerang",               "3600", "Kabupaten/Kota",
  "3672", "Kota Cilegon",                 "3600", "Kabupaten/Kota",
  "3673", "Kota Serang",                  "3600", "Kabupaten/Kota",
  "3674", "Kota Tangerang Selatan",       "3600", "Kabupaten/Kota"
)

province_reference <- tibble::tribble(
  ~kode_kelompok, ~nama_provinsi,
  "1100", "Aceh",
  "1200", "Sumatera Utara",
  "1300", "Sumatera Barat",
  "1400", "Riau",
  "1500", "Jambi",
  "1600", "Sumatera Selatan",
  "1700", "Bengkulu",
  "1800", "Lampung",
  "1900", "Kepulauan Bangka Belitung",
  "2100", "Kepulauan Riau",
  "3100", "Daerah Khusus Ibukota Jakarta",
  "3200", "Jawa Barat",
  "3300", "Jawa Tengah",
  "3400", "Daerah Istimewa Yogyakarta",
  "3500", "Jawa Timur",
  "3600", "Banten",
  "5100", "Bali",
  "5200", "Nusa Tenggara Barat",
  "5300", "Nusa Tenggara Timur",
  "6100", "Kalimantan Barat",
  "6200", "Kalimantan Tengah",
  "6300", "Kalimantan Selatan",
  "6400", "Kalimantan Timur",
  "6500", "Kalimantan Utara",
  "7100", "Sulawesi Utara",
  "7200", "Sulawesi Tengah",
  "7300", "Sulawesi Selatan",
  "7400", "Sulawesi Tenggara",
  "7500", "Gorontalo",
  "7600", "Sulawesi Barat",
  "8100", "Maluku",
  "8200", "Maluku Utara",
  "9100", "Papua Barat",
  "9200", "Papua Barat Daya",
  "9400", "Papua",
  "9500", "Papua Selatan",
  "9600", "Papua Tengah",
  "9700", "Papua Pegunungan"
)


# Helper dasar harus tersedia sebelum referensi wilayah CSV dimuat.
clean_text <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[x %in% c("", " ", "NA", "N/A", "NULL", "-", "--", ":")] <- NA_character_
  x
}

normalize_column_names <- function(x) {
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9]+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

# Referensi wilayah utama dari CSV aplikasi.
# File CSV menjadi sumber utama; tabel bawaan di atas hanya fallback jika file tidak tersedia.
load_master_region_reference <- function() {
  candidates <- c(
    file.path("data", "master_wilayah_pdrb.csv"),
    file.path("www", "master_wilayah_pdrb.csv"),
    "master_wilayah_pdrb.csv"
  )
  csv_path <- candidates[file.exists(candidates)][1]
  if (length(csv_path) == 0 || is.na(csv_path)) return(NULL)

  ref <- tryCatch(
    utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(ref)) return(NULL)

  names(ref) <- normalize_column_names(names(ref))
  required <- c("kode_wilayah", "nama_wilayah", "level_wilayah", "kode_provinsi", "nama_provinsi")
  if (!all(required %in% names(ref))) return(NULL)

  ref %>%
    dplyr::transmute(
      kode_wilayah = stringr::str_pad(as.character(kode_wilayah), 4, side = "left", pad = "0"),
      nama_wilayah = clean_text(nama_wilayah),
      level_wilayah = clean_text(level_wilayah),
      kode_provinsi = stringr::str_pad(as.character(kode_provinsi), 4, side = "left", pad = "0"),
      nama_provinsi = clean_text(nama_provinsi)
    ) %>%
    dplyr::filter(!is.na(kode_wilayah), !is.na(nama_wilayah)) %>%
    dplyr::distinct(kode_wilayah, .keep_all = TRUE)
}

master_region_reference <- load_master_region_reference()
if (!is.null(master_region_reference) && nrow(master_region_reference) > 0) {
  region_name_reference <- master_region_reference %>%
    dplyr::transmute(
      kode_wilayah,
      nama_wilayah_referensi = stringr::str_remove(nama_wilayah, "^Provinsi\\s+"),
      kode_kelompok_referensi = kode_provinsi,
      jenis_wilayah_referensi = dplyr::case_when(
        stringr::str_detect(stringr::str_to_lower(level_wilayah), "provinsi|agregat") ~ "Provinsi/Agregat",
        TRUE ~ "Kabupaten/Kota"
      )
    )

  province_reference <- master_region_reference %>%
    dplyr::filter(kode_wilayah == kode_provinsi) %>%
    dplyr::transmute(
      kode_kelompok = kode_provinsi,
      nama_provinsi = stringr::str_remove(nama_provinsi, "^Provinsi\\s+")
    ) %>%
    dplyr::distinct(kode_kelompok, .keep_all = TRUE)
}

# Marker wilayah seperti ':' saja bukan nama wilayah yang valid.
clean_region_name <- function(x) {
  x <- clean_text(x)
  invalid <- is.na(x) | stringr::str_detect(x, "^[[:punct:]\\s]+$")
  x[invalid] <- NA_character_
  x
}

# Normalisasi singkatan provinsi menjadi nama lengkap pada label filter.
expand_province_name <- function(x) {
  x <- clean_text(x)
  if (length(x) == 0) return(character())
  
  normalized <- stringr::str_to_lower(x) %>%
    stringr::str_replace_all("[.]", "") %>%
    stringr::str_squish() %>%
    stringr::str_remove("^provinsi\\s+")
  
  dplyr::case_when(
    normalized %in% c("di yogyakarta", "d i yogyakarta", "diy", "daerah istimewa yogyakarta") ~
      "Daerah Istimewa Yogyakarta",
    normalized %in% c("dki jakarta", "d k i jakarta", "daerah khusus ibukota jakarta") ~
      "Daerah Khusus Ibukota Jakarta",
    TRUE ~ stringr::str_squish(x)
  )
}

is_code_only_label <- function(x) {
  x <- clean_text(x)
  is.na(x) | stringr::str_detect(
    stringr::str_to_lower(x),
    "^(wilayah|daerah|kabupaten|kota|provinsi|kelompok)?\\s*[-_:]?\\s*[0-9]{4}$"
  )
}



normalize_region_lookup_key <- function(x) {
  x <- clean_text(x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "[.]", "")
  x <- stringr::str_remove(x, "^\\s*provinsi\\s+")
  x <- stringr::str_replace_all(x, "\\bkab\\b", "kabupaten")
  x <- stringr::str_replace_all(x, "\\bkota\\b", "kota")
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  x <- stringr::str_squish(x)
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

build_region_alias_reference <- function() {
  base_reference <- dplyr::bind_rows(
    region_name_reference %>%
      dplyr::transmute(
        kode_wilayah,
        nama_wilayah_referensi,
        kode_kelompok_referensi,
        jenis_wilayah_referensi
      ),
    province_reference %>%
      dplyr::transmute(
        kode_wilayah = kode_kelompok,
        nama_wilayah_referensi = nama_provinsi,
        kode_kelompok_referensi = kode_kelompok,
        jenis_wilayah_referensi = "Provinsi/Agregat"
      )
  ) %>%
    dplyr::distinct(kode_wilayah, .keep_all = TRUE)
  
  full_alias <- base_reference %>%
    dplyr::mutate(
      alias_key = normalize_region_lookup_key(nama_wilayah_referensi),
      alias_priority = 1L
    )
  
  without_province_alias <- base_reference %>%
    dplyr::mutate(
      alias_key = normalize_region_lookup_key(stringr::str_remove(nama_wilayah_referensi, "^Provinsi\\s+")),
      alias_priority = 2L
    )
  
  short_alias <- base_reference %>%
    dplyr::mutate(
      alias_key = normalize_region_lookup_key(stringr::str_remove(nama_wilayah_referensi, "^(Kabupaten|Kota)\\s+")),
      alias_priority = 3L
    ) %>%
    dplyr::group_by(alias_key) %>%
    dplyr::filter(dplyr::n_distinct(kode_wilayah) == 1L) %>%
    dplyr::ungroup()
  
  dplyr::bind_rows(full_alias, without_province_alias, short_alias) %>%
    dplyr::filter(!is.na(alias_key), !is.na(kode_wilayah)) %>%
    dplyr::arrange(alias_priority, kode_wilayah) %>%
    dplyr::distinct(alias_key, kode_wilayah, .keep_all = TRUE)
}

lookup_region_reference_by_name <- function(region_name) {
  key <- normalize_region_lookup_key(region_name)
  empty_result <- tibble::tibble(
    kode_wilayah = character(),
    nama_wilayah_referensi = character(),
    kode_kelompok_referensi = character(),
    jenis_wilayah_referensi = character()
  )
  
  if (length(key) == 0 || is.na(key[[1]]) || !nzchar(key[[1]])) return(empty_result)
  
  matches <- build_region_alias_reference() %>%
    dplyr::filter(alias_key == key[[1]])
  
  if (nrow(matches) == 0) return(empty_result)
  
  best_priority <- min(matches$alias_priority, na.rm = TRUE)
  matches <- matches %>%
    dplyr::filter(alias_priority == best_priority) %>%
    dplyr::distinct(kode_wilayah, .keep_all = TRUE)
  
  if (nrow(matches) != 1L) return(empty_result)
  
  matches %>%
    dplyr::select(
      kode_wilayah,
      nama_wilayah_referensi,
      kode_kelompok_referensi,
      jenis_wilayah_referensi
    )
}

is_aggregate_region <- function(kode_wilayah, kode_kelompok = NULL, jenis_wilayah = NULL) {
  kode_wilayah <- as.character(kode_wilayah)
  n <- length(kode_wilayah)
  
  kode_kelompok <- if (is.null(kode_kelompok)) {
    rep(NA_character_, n)
  } else {
    as.character(kode_kelompok)
  }
  
  jenis_wilayah <- if (is.null(jenis_wilayah)) {
    rep(NA_character_, n)
  } else {
    clean_text(jenis_wilayah)
  }
  
  jenis_compact <- stringr::str_to_lower(jenis_wilayah) %>%
    stringr::str_replace_all("[^a-z0-9]+", "")
  
  by_code <- !is.na(kode_wilayah) & (
    stringr::str_detect(kode_wilayah, "^[0-9]{2}00$") |
      (!is.na(kode_kelompok) & kode_wilayah == kode_kelompok)
  )
  
  by_type <- !is.na(jenis_compact) &
    stringr::str_detect(jenis_compact, "provinsi|agregat|province|aggregate")
  
  by_code | by_type
}

normalize_region_type <- function(jenis_wilayah, kode_wilayah = NULL, kode_kelompok = NULL) {
  jenis_wilayah <- clean_text(jenis_wilayah)
  jenis_compact <- stringr::str_to_lower(jenis_wilayah) %>%
    stringr::str_replace_all("[^a-z0-9]+", "")
  
  dplyr::case_when(
    is_aggregate_region(kode_wilayah, kode_kelompok, jenis_wilayah) ~ "Provinsi/Agregat",
    !is.na(jenis_compact) & stringr::str_detect(jenis_compact, "kabupaten|kota|kabkot") ~ "Kabupaten/Kota",
    TRUE ~ dplyr::coalesce(
      as.character(jenis_wilayah),
      if_else(
        !is.na(as.character(kode_wilayah)) & stringr::str_detect(as.character(kode_wilayah), "^[0-9]{2}00$"),
        "Provinsi/Agregat",
        "Kabupaten/Kota"
      )
    )
  )
}

indicator_units <- c(
  "PDRB ADHB" = "Juta Rupiah",
  "PDRB ADHK" = "Juta Rupiah",
  "Distribusi PDRB" = "Persen",
  "Pertumbuhan Q-to-Q" = "Persen",
  "Pertumbuhan Y-on-Y" = "Persen",
  "Pertumbuhan C-to-C" = "Persen",
  "Pertumbuhan ADHB Q-to-Q" = "Persen",
  "Pertumbuhan ADHB Y-on-Y" = "Persen",
  "Pertumbuhan ADHB C-to-C" = "Persen",
  "Pertumbuhan ADHK Q-to-Q" = "Persen",
  "Pertumbuhan ADHK Y-on-Y" = "Persen",
  "Pertumbuhan ADHK C-to-C" = "Persen",
  "Indeks Implisit" = "Indeks",
  "Laju Indeks Implisit Q-to-Q" = "Persen",
  "Laju Indeks Implisit Y-on-Y" = "Persen",
  "Laju Indeks Implisit C-to-C" = "Persen",
  "Sumber Pertumbuhan ADHB Q-to-Q" = "Persen Poin",
  "Sumber Pertumbuhan ADHB Y-on-Y" = "Persen Poin",
  "Sumber Pertumbuhan ADHB C-to-C" = "Persen Poin",
  "Sumber Pertumbuhan ADHK Q-to-Q" = "Persen Poin",
  "Sumber Pertumbuhan ADHK Y-on-Y" = "Persen Poin",
  "Sumber Pertumbuhan ADHK C-to-C" = "Persen Poin",
  "LQ" = "Indeks",
  "LQ ADHB" = "Indeks",
  "LQ ADHK" = "Indeks"
)

growth_indicator_groups <- list(
  "ADHB" = c(
    "Q-to-Q" = "Pertumbuhan ADHB Q-to-Q",
    "Y-on-Y" = "Pertumbuhan ADHB Y-on-Y",
    "C-to-C" = "Pertumbuhan ADHB C-to-C"
  ),
  "ADHK" = c(
    "Q-to-Q" = "Pertumbuhan ADHK Q-to-Q",
    "Y-on-Y" = "Pertumbuhan ADHK Y-on-Y",
    "C-to-C" = "Pertumbuhan ADHK C-to-C"
  )
)

indicator_groups <- list(
  "PDRB" = c("ADHB" = "PDRB ADHB", "ADHK" = "PDRB ADHK"),
  "Distribusi" = c("Distribusi PDRB" = "Distribusi PDRB"),
  "Pertumbuhan ADHB" = growth_indicator_groups[["ADHB"]],
  "Pertumbuhan ADHK" = growth_indicator_groups[["ADHK"]],
  "Indeks Implisit" = c(
    "Indeks Implisit" = "Indeks Implisit",
    "Laju Indeks Implisit Q-to-Q" = "Laju Indeks Implisit Q-to-Q",
    "Laju Indeks Implisit Y-on-Y" = "Laju Indeks Implisit Y-on-Y",
    "Laju Indeks Implisit C-to-C" = "Laju Indeks Implisit C-to-C"
  ),
  "Sumber Pertumbuhan" = c(
    "Sumber Pertumbuhan ADHK Q-to-Q" = "Sumber Pertumbuhan ADHK Q-to-Q",
    "Sumber Pertumbuhan ADHK Y-on-Y" = "Sumber Pertumbuhan ADHK Y-on-Y",
    "Sumber Pertumbuhan ADHK C-to-C" = "Sumber Pertumbuhan ADHK C-to-C",
    "Sumber Pertumbuhan ADHB Q-to-Q" = "Sumber Pertumbuhan ADHB Q-to-Q",
    "Sumber Pertumbuhan ADHB Y-on-Y" = "Sumber Pertumbuhan ADHB Y-on-Y",
    "Sumber Pertumbuhan ADHB C-to-C" = "Sumber Pertumbuhan ADHB C-to-C"
  )
)


PDRB_COLORS <- list(
  navy = "#17324D",
  blue = "#2C5F8A",
  teal = "#2D7F78",
  green = "#4F806B",
  gold = "#C49A45",
  orange = "#C97845",
  red = "#B45151",
  ink = "#1F2937",
  muted = "#68737D",
  grid = "#E3E8E5",
  surface = "#FFFFFF"
)

PDRB_CATEGORY_PALETTE <- c(
  "#17324D", "#2C5F8A", "#3E7398", "#2D7F78", "#4F806B",
  "#6E927D", "#C49A45", "#D3AE64", "#C97845", "#A9684F",
  "#6B7785", "#738CA3", "#6A8F8A", "#82976C", "#A68B55",
  "#8A6E78", "#5D747E"
)

indicator_color <- function(indikator) {
  indikator <- as.character(indikator)[1]
  dplyr::case_when(
    indikator == "PDRB ADHB" ~ PDRB_COLORS$blue,
    indikator == "PDRB ADHK" ~ PDRB_COLORS$green,
    indikator == "Distribusi PDRB" ~ PDRB_COLORS$gold,
    stringr::str_detect(indikator, "Pertumbuhan ADHB") ~ PDRB_COLORS$blue,
    stringr::str_detect(indikator, "Pertumbuhan ADHK") ~ PDRB_COLORS$green,
    stringr::str_detect(indikator, "Sumber Pertumbuhan") ~ PDRB_COLORS$teal,
    stringr::str_detect(indikator, "Pertumbuhan|Laju") ~ PDRB_COLORS$orange,
    indikator == "Indeks Implisit" ~ PDRB_COLORS$teal,
    TRUE ~ PDRB_COLORS$blue
  )
}

get_indikator_values <- function(data) {
  if (is.null(data) || !is.data.frame(data) || !"indikator" %in% names(data)) {
    return(character(0))
  }
  unique(as.character(data[["indikator"]]))
}

has_required_columns <- function(data, cols) {
  is.data.frame(data) && all(cols %in% names(data))
}


first_existing_column <- function(data, candidates) {
  hit <- intersect(candidates, names(data))
  if (length(hit) == 0) NA_character_ else hit[1]
}

fill_right <- function(x) {
  x <- clean_text(x)
  out <- rep(NA_character_, length(x))
  last_value <- NA_character_
  
  for (i in seq_along(x)) {
    if (!is.na(x[i]) && nzchar(x[i])) last_value <- x[i]
    out[i] <- last_value
  }
  out
}

parse_numeric <- function(x) {
  x <- clean_text(x)
  
  convert_one <- function(z) {
    if (is.na(z)) return(NA_real_)
    z <- stringr::str_squish(as.character(z))
    if (!nzchar(z)) return(NA_real_)
    z <- gsub("%", "", z, fixed = TRUE)
    z <- gsub("\\s+", "", z)
    z <- gsub("[^0-9,.-]", "", z)
    if (!nzchar(z) || z %in% c("-", ".", ",")) return(NA_real_)
    
    # Standar keluaran dashboard: 1,530,863.2009
    # Parser tetap defensif: jika koma dan titik sama-sama ada, tanda yang muncul paling akhir
    # dianggap sebagai desimal, sedangkan tanda lainnya dianggap pemisah ribuan.
    last_comma <- max(gregexpr(",", z, fixed = TRUE)[[1]])
    last_dot <- max(gregexpr(".", z, fixed = TRUE)[[1]])
    if (last_comma < 0) last_comma <- NA_integer_
    if (last_dot < 0) last_dot <- NA_integer_
    
    if (!is.na(last_comma) && !is.na(last_dot)) {
      if (last_dot > last_comma) {
        # Format internasional: 1,530,863.2009
        z <- gsub(",", "", z, fixed = TRUE)
      } else {
        # Format lokal yang masih mungkin muncul: 1.530.863,2009
        z <- gsub(".", "", z, fixed = TRUE)
        z <- sub(",", ".", z, fixed = TRUE)
      }
    } else if (!is.na(last_comma)) {
      comma_count <- lengths(gregexpr(",", z, fixed = TRUE))
      if (comma_count > 1) {
        z <- gsub(",", "", z, fixed = TRUE)
      } else {
        parts <- strsplit(z, ",", fixed = TRUE)[[1]]
        if (length(parts) == 2 && nchar(parts[2]) == 3 && nchar(parts[1]) > 3) {
          z <- gsub(",", "", z, fixed = TRUE)
        } else {
          z <- sub(",", ".", z, fixed = TRUE)
        }
      }
    } else if (!is.na(last_dot)) {
      dot_count <- lengths(gregexpr(".", z, fixed = TRUE))
      if (dot_count > 1) {
        z <- gsub(".", "", z, fixed = TRUE)
      }
    }
    
    suppressWarnings(as.numeric(z))
  }
  
  vapply(x, convert_one, numeric(1))
}

extract_region_name_from_sheet <- function(path, sheet_name) {
  preview <- tryCatch(
    readxl::read_excel(
      path, sheet = sheet_name, range = "A1:Z50",
      col_names = FALSE, col_types = "text", .name_repair = "minimal"
    ),
    error = function(e) NULL
  )

  if (is.null(preview) || nrow(preview) == 0) return(NA_character_)
  preview <- as.data.frame(preview, stringsAsFactors = FALSE)

  valid_candidate <- function(x) {
    x <- clean_region_name(x)
    if (length(x) == 0 || is.na(x[[1]])) return(NA_character_)
    y <- stringr::str_to_lower(x[[1]])
    if (stringr::str_detect(y, "^(wilayah|nama wilayah|kode wilayah|provinsi|kabupaten/kota)\\s*:?$") ||
        stringr::str_detect(y, "^(kategori|uraian|tahun|triwulan|total)$")) return(NA_character_)
    x[[1]]
  }

  for (i in seq_len(nrow(preview))) {
    row_values <- clean_text(unlist(preview[i, , drop = FALSE], use.names = FALSE))
    marker <- which(!is.na(row_values) & stringr::str_detect(stringr::str_to_lower(row_values), "^\\s*(nama\\s+)?wilayah\\s*:?") )
    if (length(marker) == 0) next

    for (position in marker) {
      inline_value <- stringr::str_match(row_values[position], "(?i)^\\s*(?:nama\\s+)?wilayah\\s*:?\\s*(.+)$")[, 2]
      inline_value <- valid_candidate(inline_value)
      if (!is.na(inline_value)) return(inline_value)

      # Cari nama wilayah pada sel-sel di kanan marker pada baris yang sama.
      if (position < length(row_values)) {
        candidates <- row_values[seq.int(position + 1L, length(row_values))]
        for (candidate in candidates) {
          candidate <- valid_candidate(candidate)
          if (!is.na(candidate)) return(candidate)
        }
      }

      # Toleransi format vertikal: nama wilayah dapat berada sampai tiga baris di bawah marker.
      next_rows <- seq.int(i + 1L, min(nrow(preview), i + 3L))
      for (r in next_rows) {
        candidates <- clean_text(unlist(preview[r, , drop = FALSE], use.names = FALSE))
        for (candidate in candidates) {
          candidate <- valid_candidate(candidate)
          if (!is.na(candidate)) return(candidate)
        }
      }
    }
  }

  NA_character_
}

build_sheet_index <- function(path) {
  sheet_names <- readxl::excel_sheets(path)
  code_pattern <- "(?<![0-9])[0-9]{4}(?![0-9])"
  metadata_candidates <- c("wilayah", "metadata_wilayah", "referensi_wilayah", "region")
  normalized_sheet_names <- stringr::str_to_lower(stringr::str_trim(sheet_names))
  metadata_sheets <- sheet_names[normalized_sheet_names %in% metadata_candidates]
  data_sheet_names <- sheet_names[!(sheet_names %in% metadata_sheets)]
  
  extract_code_from_content <- function(sheet_name) {
    preview <- tryCatch(
      readxl::read_excel(path, sheet = sheet_name, range = "A1:Z50", col_names = FALSE, col_types = "text", .name_repair = "minimal"),
      error = function(e) NULL
    )
    if (is.null(preview) || nrow(preview) == 0) return(NA_character_)
    values <- clean_text(unlist(as.data.frame(preview, stringsAsFactors = FALSE), use.names = FALSE))
    values <- values[!is.na(values)]
    if (length(values) == 0) return(NA_character_)

    # Prioritas pertama: kode yang secara eksplisit diberi label kode wilayah/daerah.
    labeled <- stringr::str_match(values, "(?i)(?:kode\\s*(?:wilayah|daerah|kabupaten|kota|provinsi)\\s*:?\\s*)([0-9]{4})")[, 2]
    labeled <- labeled[!is.na(labeled)]
    if (length(labeled) > 0) return(labeled[[1]])

    # Kode bebas hanya diterima bila bukan tahun dan tercantum di referensi wilayah.
    direct <- stringr::str_extract(values, code_pattern)
    direct <- direct[!is.na(direct)]
    direct <- direct[!stringr::str_detect(direct, "^(19|20)[0-9]{2}$")]
    known_codes <- unique(as.character(region_name_reference$kode_wilayah))
    direct_known <- direct[direct %in% known_codes]
    if (length(direct_known) > 0) return(direct_known[[1]])
    NA_character_
  }
  
  if (length(data_sheet_names) == 0) {
    stop("Tidak ada sheet data PDRB yang dapat dibaca. Sheet metadata/petunjuk tidak diperlakukan sebagai sheet data.")
  }
  
  sheet_code_by_name <- stringr::str_extract(stringr::str_trim(data_sheet_names), code_pattern)
  sheet_code_by_content <- vapply(data_sheet_names, extract_code_from_content, character(1))
  embedded_region_name <- vapply(data_sheet_names, function(sheet_name) extract_region_name_from_sheet(path, sheet_name), character(1))
  file_region_hint <- basename(path) %>%
    stringr::str_remove("\\.[Xx][Ll][Ss][Xx]?$") %>%
    stringr::str_replace_all("\\([^)]*\\)", " ") %>%
    stringr::str_replace_all("(?i)\\b(PDRB|17\\s*LU|SERI|TRIWULAN|TW|DATA)\\b", " ") %>%
    stringr::str_replace_all("[0-9]{4}", " ") %>%
    stringr::str_squish()
  embedded_region_name <- dplyr::coalesce(
    clean_region_name(embedded_region_name),
    rep(clean_region_name(file_region_hint), length(data_sheet_names))
  )
  inferred_reference <- purrr::map(embedded_region_name, lookup_region_reference_by_name)
  sheet_code_by_region <- purrr::map_chr(inferred_reference, function(ref) {
    if (is.null(ref) || nrow(ref) == 0) NA_character_ else as.character(ref$kode_wilayah[[1]])
  })
  
  file_code <- stringr::str_extract(basename(path), code_pattern)
  if (!is.na(file_code) && stringr::str_detect(file_code, "^(19|20)[0-9]{2}$")) file_code <- NA_character_
  file_code <- rep(file_code, length(data_sheet_names))

  # Prioritas: kode nama sheet -> nama wilayah di isi sheet -> kode eksplisit isi -> kode nama file.
  # Tahun seperti 2010/2020 tidak boleh pernah dianggap sebagai kode wilayah.
  sheet_code <- dplyr::coalesce(sheet_code_by_name, sheet_code_by_region, sheet_code_by_content, file_code)
  
  index <- tibble::tibble(
    sheet_name = data_sheet_names,
    kode_wilayah = sheet_code,
    sheet_region_name = stringr::str_squish(stringr::str_remove(stringr::str_trim(data_sheet_names), code_pattern)),
    embedded_region_name = embedded_region_name
  ) %>%
    dplyr::filter(!is.na(kode_wilayah)) %>%
    dplyr::mutate(
      kode_wilayah = stringr::str_pad(kode_wilayah, width = 4, side = "left", pad = "0"),
      sheet_region_name = stringr::str_remove_all(sheet_region_name, "^[-_–—: ]+|[-_–—: ]+$"),
      sheet_region_name = clean_text(sheet_region_name),
      sheet_region_name = dplyr::if_else(
        is.na(sheet_region_name) | sheet_region_name == kode_wilayah | stringr::str_to_lower(sheet_region_name) %in% c("sheet1", "sheet 1", "data", "pdrb"),
        embedded_region_name,
        sheet_region_name
      ),
      sheet_region_name = dplyr::if_else(is.na(sheet_region_name) | sheet_region_name == kode_wilayah, NA_character_, sheet_region_name)
    ) %>%
    dplyr::select(sheet_name, kode_wilayah, sheet_region_name)
  
  if (nrow(index) == 0) {
    stop(
      "Wilayah sheet belum dapat dikenali. Sistem sudah mencoba nama sheet, nama file, kode wilayah berlabel, dan baris `Wilayah`. ",
      "Minimal setiap sheet perlu memiliki kode wilayah empat digit atau nama wilayah yang cocok dengan CSV referensi. ",
      "Tahun seperti 2010/2020 tidak dianggap sebagai kode wilayah."
    )
  }
  
  index
}

read_metadata_sheet <- function(path, available_sheets) {
  metadata_candidates <- c("wilayah", "metadata_wilayah", "referensi_wilayah", "region")
  normalized_sheet_names <- stringr::str_to_lower(stringr::str_trim(available_sheets))
  metadata_position <- match(metadata_candidates, normalized_sheet_names, nomatch = 0)
  metadata_position <- metadata_position[metadata_position > 0]
  
  if (length(metadata_position) == 0) return(NULL)
  metadata_sheet <- available_sheets[metadata_position[1]]
  
  metadata <- readxl::read_excel(path, sheet = metadata_sheet, .name_repair = "unique")
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  names(metadata) <- normalize_column_names(names(metadata))
  
  code_col <- first_existing_column(metadata, c("kode_wilayah", "kode", "kodewilayah", "sheet"))
  region_col <- first_existing_column(metadata, c("wilayah", "nama_wilayah", "nama", "daerah"))
  group_code_col <- first_existing_column(metadata, c("kode_kelompok", "kode_provinsi", "kode_cakupan"))
  group_col <- first_existing_column(metadata, c("kelompok", "provinsi", "cakupan", "nama_provinsi", "kelompok_wilayah"))
  type_col <- first_existing_column(metadata, c("jenis_wilayah", "jenis", "level_wilayah"))
  
  if (is.na(code_col) || is.na(region_col)) {
    stop("Sheet metadata harus memiliki minimal kolom kode_wilayah dan wilayah.")
  }
  
  result <- tibble(
    kode_wilayah = clean_text(metadata[[code_col]]),
    wilayah = clean_text(metadata[[region_col]]),
    kode_kelompok = if (!is.na(group_code_col)) clean_text(metadata[[group_code_col]]) else NA_character_,
    kelompok = if (!is.na(group_col)) clean_text(metadata[[group_col]]) else NA_character_,
    jenis_wilayah = if (!is.na(type_col)) clean_text(metadata[[type_col]]) else NA_character_
  ) %>%
    filter(!is.na(kode_wilayah), !is.na(wilayah)) %>%
    mutate(kode_wilayah = stringr::str_pad(kode_wilayah, width = 4, side = "left", pad = "0"))
  
  result
}

build_region_lookup <- function(path, sheet_index) {
  available_sheets <- readxl::excel_sheets(path)
  metadata <- read_metadata_sheet(path, available_sheets)
  
  embedded_names <- purrr::map_chr(
    sheet_index$sheet_name,
    ~extract_region_name_from_sheet(path, .x)
  )
  embedded_names <- clean_region_name(embedded_names)
  
  lookup <- sheet_index %>%
    transmute(kode_wilayah, sheet_region_name, embedded_name = embedded_names) %>%
    left_join(fallback_region_map, by = "kode_wilayah") %>%
    left_join(region_name_reference, by = "kode_wilayah")
  
  if (!is.null(metadata)) {
    metadata <- metadata %>% distinct(kode_wilayah, .keep_all = TRUE)
    lookup <- lookup %>%
      left_join(metadata, by = "kode_wilayah", suffix = c("_fallback", "_meta")) %>%
      transmute(
        kode_wilayah,
        wilayah = dplyr::coalesce(
          wilayah_meta, nama_wilayah_referensi, embedded_name,
          sheet_region_name, wilayah_fallback
        ),
        kode_kelompok = dplyr::coalesce(
          kode_kelompok_meta, kode_kelompok_referensi, kode_kelompok_fallback
        ),
        kelompok = dplyr::coalesce(kelompok_meta, kelompok_fallback),
        jenis_wilayah = dplyr::coalesce(
          jenis_wilayah_meta, jenis_wilayah_referensi, jenis_wilayah_fallback
        )
      )
  } else {
    lookup <- lookup %>%
      transmute(
        kode_wilayah,
        wilayah = dplyr::coalesce(
          nama_wilayah_referensi, embedded_name, sheet_region_name, wilayah
        ),
        kode_kelompok = dplyr::coalesce(kode_kelompok_referensi, kode_kelompok),
        kelompok,
        jenis_wilayah = dplyr::coalesce(jenis_wilayah_referensi, jenis_wilayah)
      )
  }
  
  lookup <- lookup %>%
    mutate(
      kode_kelompok = dplyr::coalesce(
        kode_kelompok,
        paste0(stringr::str_sub(kode_wilayah, 1, 2), "00")
      ),
      jenis_wilayah = normalize_region_type(jenis_wilayah, kode_wilayah, kode_kelompok)
    )
  
  group_names <- lookup %>%
    filter(kode_wilayah == kode_kelompok) %>%
    transmute(
      kode_kelompok,
      inferred_group = stringr::str_remove(wilayah, "^Provinsi\\s+")
    ) %>%
    distinct(kode_kelompok, .keep_all = TRUE)
  
  result <- lookup %>%
    left_join(group_names, by = "kode_kelompok") %>%
    left_join(province_reference, by = "kode_kelompok") %>%
    mutate(
      kelompok = clean_text(kelompok),
      inferred_group = clean_text(inferred_group),
      kelompok = if_else(is_code_only_label(kelompok), NA_character_, kelompok),
      inferred_group = if_else(is_code_only_label(inferred_group), NA_character_, inferred_group),
      kelompok = dplyr::coalesce(nama_provinsi, kelompok, inferred_group),
      kelompok = expand_province_name(kelompok),
      wilayah = clean_text(wilayah),
      wilayah = if_else(is_code_only_label(wilayah), NA_character_, wilayah),
      wilayah = if_else(
        kode_wilayah == kode_kelompok & !is.na(kelompok),
        kelompok,
        wilayah
      ),
      wilayah = as.character(wilayah),
      kelompok = as.character(kelompok)
    )
  
  unresolved_provinces <- result %>%
    filter(is.na(kelompok)) %>%
    distinct(kode_kelompok) %>%
    pull(kode_kelompok)
  
  unresolved_regions <- result %>%
    filter(is.na(wilayah)) %>%
    distinct(kode_wilayah) %>%
    pull(kode_wilayah)
  
  if (length(unresolved_provinces) > 0 || length(unresolved_regions) > 0) {
    details <- c(
      if (length(unresolved_provinces) > 0)
        paste0("nama provinsi untuk kode ", paste(unresolved_provinces, collapse = ", ")),
      if (length(unresolved_regions) > 0)
        paste0("nama wilayah untuk kode ", paste(unresolved_regions, collapse = ", "))
    )
    
    stop(
      "Nama wilayah belum lengkap (", paste(details, collapse = "; "), "). ",
      "Tambahkan sheet `wilayah` dengan kolom kode_wilayah, wilayah, kode_kelompok, kelompok, dan jenis_wilayah, ",
      "atau tuliskan baris `Wilayah : Nama Wilayah` di dalam sheet. Nama sheet boleh bebas; kode wilayah tetap dipakai jika tersedia."
    )
  }
  
  result %>%
    select(kode_wilayah, wilayah, kode_kelompok, kelompok, jenis_wilayah)
}

normalize_period_value <- function(x) {
  x <- clean_text(x)
  normalized <- stringr::str_to_upper(x)
  normalized <- stringr::str_replace_all(normalized, "[‐‑–—−]", "-")
  normalized <- stringr::str_replace_all(normalized, "[._:/]", " ")
  normalized <- stringr::str_squish(normalized)
  normalized <- stringr::str_remove(normalized, "^(TRIWULAN|TW|QUARTER|Q)\\s*")
  
  dplyr::case_when(
    normalized %in% c("I", "1", "01") ~ "I",
    normalized %in% c("II", "2", "02") ~ "II",
    normalized %in% c("III", "3", "03") ~ "III",
    normalized %in% c("IV", "4", "04") ~ "IV",
    normalized %in% c("TOTAL", "TAHUNAN", "ANNUAL", "JUMLAH", "KUMULATIF") ~ "Total",
    TRUE ~ NA_character_
  )
}

extract_year_value <- function(x) {
  x <- clean_text(x)
  value <- suppressWarnings(as.integer(x))
  ifelse(!is.na(value) & value >= 1900L & value <= 2100L, value, NA_integer_)
}

# Membaca header waktu dalam satu sel, misalnya Q1-2020,
# 2020-Q1, Triwulan I 2020, I/2020, atau hanya 2020 untuk nilai tahunan.
parse_combined_time_cell <- function(x) {
  x <- clean_text(x)
  if (length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(list(tahun = NA_integer_, periode = NA_character_))
  }
  
  normalized <- stringr::str_to_upper(x)
  normalized <- stringr::str_replace_all(normalized, "[‐‑–—−]", "-")
  normalized <- stringr::str_squish(normalized)
  
  year_pattern <- "(?<![0-9])(19|20)[0-9]{2}(?![0-9])"
  year_matches <- stringr::str_extract_all(normalized, year_pattern)[[1]]
  year_matches <- unique(year_matches[!is.na(year_matches)])
  
  # Rentang seperti 'TAHUN 2020-2024' tidak diperlakukan sebagai header satu kolom.
  if (length(year_matches) != 1L) {
    return(list(tahun = NA_integer_, periode = NA_character_))
  }
  
  tahun <- suppressWarnings(as.integer(year_matches[[1]]))
  if (is.na(tahun) || tahun < 1900L || tahun > 2100L) {
    return(list(tahun = NA_integer_, periode = NA_character_))
  }
  
  if (stringr::str_detect(normalized, "^\\s*(19|20)[0-9]{2}\\s*$")) {
    return(list(tahun = tahun, periode = "Total"))
  }
  
  remainder <- stringr::str_replace_all(normalized, year_pattern, " ")
  remainder <- stringr::str_replace_all(remainder, "[-()\\[\\]]", " ")
  remainder <- stringr::str_replace_all(remainder, "\\bTAHUN\\b", " ")
  remainder <- stringr::str_squish(remainder)
  periode <- normalize_period_value(remainder)
  
  list(tahun = tahun, periode = periode[[1]])
}

extract_combined_time_year <- function(x) {
  vapply(
    as.character(x),
    function(value) parse_combined_time_cell(value)$tahun,
    integer(1)
  )
}

extract_combined_time_period <- function(x) {
  vapply(
    as.character(x),
    function(value) parse_combined_time_cell(value)$periode,
    character(1)
  )
}

normalize_indicator_text <- function(x) {
  x <- clean_text(x)
  x <- stringr::str_to_upper(x)
  x <- stringr::str_replace_all(x, "[‐‑–—−]", "-")
  stringr::str_squish(x)
}

classify_indicator_title <- function(text) {
  text <- normalize_indicator_text(text)
  if (is.na(text) || !nzchar(text)) return(NA_character_)
  
  has_pdrb <- stringr::str_detect(text, "PDRB|PRODUK DOMESTIK REGIONAL BRUTO")
  has_adhb <- stringr::str_detect(text, "ADHB|HARGA BERLAKU")
  has_adhk <- stringr::str_detect(text, "ADHK|HARGA KONSTAN")
  derived_pattern <- paste(
    c(
      "DISTRIBUSI", "KONTRIBUSI", "STRUKTUR EKONOMI",
      "PERTUMBUHAN", "LAJU PERTUMBUHAN", "Q\\s*-?\\s*TO\\s*-?\\s*Q", "Y\\s*-?\\s*ON\\s*-?\\s*Y", "C\\s*-?\\s*TO\\s*-?\\s*C",
      "INDEKS IMPLISIT", "IMPLISIT", "SUMBER PERTUMBUHAN", "LAJU PERTUMBUHAN NOMINAL",
      "LOCATION QUOTIENT", "DYNAMIC LOCATION QUOTIENT", "Extended Shift Share",
      "(^|[^A-Z])LQ([^A-Z]|$)", "(^|[^A-Z])DLQ([^A-Z]|$)",
      "(^|[^A-Z])NE([^A-Z]|$)", "(^|[^A-Z])IM([^A-Z]|$)", "(^|[^A-Z])CE([^A-Z]|$)",
      "(^|[^A-Z])RIE([^A-Z]|$)", "(^|[^A-Z])RSE([^A-Z]|$)", "(^|[^A-Z])RCCE([^A-Z]|$)"
    ),
    collapse = "|"
  )
  has_derived <- stringr::str_detect(text, derived_pattern)
  
  # Pengecualian diproses lebih dulu. Jika judul memuat ADHB/ADHK
  # tetapi juga kata turunan, blok tidak dijadikan input mentah.
  if (has_derived) return(NA_character_)
  if (has_pdrb && has_adhb) return("PDRB ADHB")
  if (has_pdrb && has_adhk) return("PDRB ADHK")
  NA_character_
}



v5_block_classification <- function(text) {
  text_original <- clean_text(text)
  text <- normalize_indicator_text(text_original)
  if (is.na(text) || !nzchar(text)) return(list(class = "unknown", indikator = NA_character_, reason = "Judul/area blok kosong atau tidak terbaca."))
  
  has_pdrb <- stringr::str_detect(text, "PDRB|PRODUK DOMESTIK REGIONAL BRUTO")
  has_adhb <- stringr::str_detect(text, "ADHB|HARGA BERLAKU")
  has_adhk <- stringr::str_detect(text, "ADHK|HARGA KONSTAN")
  derived_pattern <- paste(
    c(
      "DISTRIBUSI", "KONTRIBUSI", "STRUKTUR EKONOMI", "PERTUMBUHAN", "LAJU PERTUMBUHAN",
      "Q\\s*-?\\s*TO\\s*-?\\s*Q", "Y\\s*-?\\s*ON\\s*-?\\s*Y", "C\\s*-?\\s*TO\\s*-?\\s*C",
      "INDEKS IMPLISIT", "IMPLISIT", "SUMBER PERTUMBUHAN", "LAJU PERTUMBUHAN NOMINAL",
      "LOCATION QUOTIENT", "DYNAMIC LOCATION QUOTIENT", "Extended Shift Share",
      "(^|[^A-Z])LQ([^A-Z]|$)", "(^|[^A-Z])DLQ([^A-Z]|$)",
      "(^|[^A-Z])NE([^A-Z]|$)", "(^|[^A-Z])IM([^A-Z]|$)", "(^|[^A-Z])CE([^A-Z]|$)",
      "(^|[^A-Z])RIE([^A-Z]|$)", "(^|[^A-Z])RSE([^A-Z]|$)", "(^|[^A-Z])RCCE([^A-Z]|$)"
    ),
    collapse = "|"
  )
  if (stringr::str_detect(text, derived_pattern)) {
    return(list(class = "ignore_derived", indikator = NA_character_, reason = "Diabaikan karena terdeteksi sebagai tabel turunan/hasil analisis, bukan data mentah ADHB/ADHK."))
  }
  if (has_pdrb && has_adhb) return(list(class = "raw_adhb", indikator = "PDRB ADHB", reason = "Dibaca sebagai data PDRB ADHB."))
  if (has_pdrb && has_adhk) return(list(class = "raw_adhk", indikator = "PDRB ADHK", reason = "Dibaca sebagai data   PDRB ADHK."))
  list(class = "unknown", indikator = NA_character_, reason = "Belum memenuhi pola data   PDRB ADHB/ADHK." )
}

v5_scan_sheet_blocks <- function(path, sheet_name, source_name = basename(path)) {
  raw <- tryCatch(
    readxl::read_excel(path, sheet = sheet_name, col_names = FALSE, col_types = "text", .name_repair = "minimal"),
    error = function(e) NULL
  )
  if (is.null(raw)) {
    return(tibble::tibble(source_file = source_name, sheet_name = sheet_name, block_class = "unknown", indikator = NA_character_, title_row = NA_integer_, title_text = NA_character_, reason = "Sheet tidak dapat dibaca."))
  }
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (nrow(raw) == 0 || ncol(raw) == 0 || all(is.na(unlist(raw, use.names = FALSE)))) {
    return(tibble::tibble(source_file = source_name, sheet_name = sheet_name, block_class = "unknown", indikator = NA_character_, title_row = NA_integer_, title_text = NA_character_, reason = "Sheet kosong."))
  }
  headers <- tryCatch(detect_time_headers(raw), error = function(e) tibble::tibble())
  if (nrow(headers) == 0) {
    text_sample <- paste(na.omit(clean_text(unlist(raw[seq_len(min(8, nrow(raw))), , drop = FALSE], use.names = FALSE))), collapse = " ")
    cls <- v5_block_classification(text_sample)
    return(tibble::tibble(source_file = source_name, sheet_name = sheet_name, block_class = cls$class, indikator = cls$indikator, title_row = NA_integer_, title_text = stringr::str_trunc(text_sample, 160), reason = ifelse(cls$class == "ignore_derived", cls$reason, "Header tahun/periode tidak ditemukan.")))
  }
  purrr::map_dfr(seq_len(nrow(headers)), function(i) {
    header <- headers[i, , drop = FALSE]
    previous_end <- if (i == 1L) 1L else headers$header_end[i - 1L] + 1L
    search_start <- max(1L, previous_end)
    search_end <- max(search_start, header$header_start[[1]] - 1L)
    search_rows <- seq.int(search_start, search_end)
    row_titles <- vapply(search_rows, function(r) row_text_value(raw, r), character(1))
    classes <- lapply(row_titles, v5_block_classification)
    block_classes <- vapply(classes, `[[`, character(1), "class")
    raw_hit <- which(block_classes %in% c("raw_adhb", "raw_adhk", "ignore_derived"))
    selected <- if (length(raw_hit) > 0) raw_hit[length(raw_hit)] else length(search_rows)
    cls <- classes[[selected]]
    tibble::tibble(
      source_file = source_name,
      sheet_name = sheet_name,
      block_class = cls$class,
      indikator = cls$indikator,
      title_row = as.integer(search_rows[selected]),
      title_text = stringr::str_trunc(row_titles[selected], 180),
      reason = cls$reason
    )
  })
  
}

v5_scan_workbook_blocks <- function(path, source_name = basename(path)) {
  sheets <- readxl::excel_sheets(path)
  helper_sheets <- c(
    "wilayah", "metadata_wilayah", "referensi_wilayah", "region",
    "petunjuk", "readme", "panduan", "keterangan"
  )
  sheets_to_scan <- sheets[!stringr::str_to_lower(stringr::str_trim(sheets)) %in% helper_sheets]
  if (length(sheets_to_scan) == 0) {
    return(tibble::tibble(
      source_file = source_name,
      sheet_name = NA_character_,
      block_class = "unknown",
      indikator = NA_character_,
      title_row = NA_integer_,
      title_text = NA_character_,
      reason = "Tidak ada sheet data PDRB yang dapat dipindai. Nama sheet data sebaiknya memuat kode wilayah empat digit."
    ))
  }
  purrr::map_dfr(sheets_to_scan, ~v5_scan_sheet_blocks(path, .x, source_name))
}

row_text_value <- function(raw, row_number) {
  values <- clean_text(unlist(raw[row_number, , drop = FALSE], use.names = FALSE))
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NA_character_)
  stringr::str_squish(paste(values, collapse = " "))
}

row_year_score <- function(raw, row_number) {
  values <- unlist(raw[row_number, , drop = FALSE], use.names = FALSE)
  sum(!is.na(extract_year_value(values)))
}

row_period_score <- function(raw, row_number) {
  values <- clean_text(unlist(raw[row_number, , drop = FALSE], use.names = FALSE))
  nonempty <- values[!is.na(values)]
  if (length(nonempty) == 0) return(0L)
  
  recognized <- normalize_period_value(nonempty)
  score <- sum(!is.na(recognized))
  
  # Mencegah nomor urut 1, 2, 3, 4, ... terbaca sebagai header periode.
  if (score < 2L || score / length(nonempty) < 0.5) return(0L)
  as.integer(score)
}

detect_time_headers <- function(raw) {
  row_numbers <- seq_len(nrow(raw))
  year_scores <- vapply(row_numbers, function(i) row_year_score(raw, i), integer(1))
  period_scores <- vapply(row_numbers, function(i) row_period_score(raw, i), integer(1))
  combined_scores <- vapply(row_numbers, function(i) {
    values <- unlist(raw[i, , drop = FALSE], use.names = FALSE)
    years <- extract_combined_time_year(values)
    periods <- extract_combined_time_period(values)
    valid <- !is.na(years) & !is.na(periods)
    if (!any(periods[valid] %in% c("I", "II", "III", "IV"))) return(0L)
    sum(valid)
  }, integer(1))
  
  year_rows <- row_numbers[year_scores >= 1L]
  period_rows <- row_numbers[period_scores >= 2L]
  
  split_pairs <- tibble::tibble()
  if (length(year_rows) > 0 && length(period_rows) > 0) {
    split_pairs <- purrr::map_dfr(period_rows, function(period_row) {
      candidates <- year_rows[abs(year_rows - period_row) <= 5L]
      if (length(candidates) == 0) return(tibble::tibble())
      
      candidate_score <- year_scores[candidates] * 20 - abs(candidates - period_row)
      candidate_score <- candidate_score + ifelse(candidates <= period_row, 4, 0)
      year_row <- candidates[which.max(candidate_score)]
      
      tibble::tibble(
        year_row = as.integer(year_row),
        period_row = as.integer(period_row),
        header_start = min(year_row, period_row),
        header_end = max(year_row, period_row),
        header_score = year_scores[year_row] + period_scores[period_row],
        header_mode = "split",
        mode_priority = 2L
      )
    })
  }
  
  combined_rows <- row_numbers[combined_scores >= 2L]
  combined_pairs <- if (length(combined_rows) > 0) {
    tibble::tibble(
      year_row = as.integer(combined_rows),
      period_row = as.integer(combined_rows),
      header_start = as.integer(combined_rows),
      header_end = as.integer(combined_rows),
      header_score = combined_scores[combined_rows],
      header_mode = "combined",
      mode_priority = 1L
    )
  } else {
    tibble::tibble()
  }
  
  pairs <- dplyr::bind_rows(split_pairs, combined_pairs)
  if (nrow(pairs) == 0) return(tibble::tibble())
  
  pairs %>%
    arrange(header_start, desc(mode_priority), desc(header_score)) %>%
    distinct(header_start, .keep_all = TRUE) %>%
    select(-mode_priority)
}

detect_table_blocks <- function(raw, sheet_name) {
  headers <- detect_time_headers(raw)
  if (nrow(headers) == 0) return(tibble::tibble())
  
  blocks <- purrr::map_dfr(seq_len(nrow(headers)), function(i) {
    header <- headers[i, , drop = FALSE]
    previous_end <- if (i == 1L) 1L else headers$header_end[i - 1L] + 1L
    search_start <- max(1L, previous_end)
    search_end <- max(search_start, header$header_start[[1]] - 1L)
    search_rows <- seq.int(search_start, search_end)
    
    classifications <- vapply(search_rows, function(row_number) classify_indicator_title(row_text_value(raw, row_number)), character(1))
    recognized <- which(!is.na(classifications))
    if (length(recognized) == 0) return(tibble::tibble())
    
    selected_position <- recognized[length(recognized)]
    title_row <- search_rows[selected_position]
    indikator <- classifications[selected_position]
    next_header <- if (i < nrow(headers)) headers$header_start[i + 1L] else nrow(raw) + 1L
    
    tibble::tibble(
      title_row = as.integer(title_row),
      year_row = as.integer(header$year_row[[1]]),
      period_row = as.integer(header$period_row[[1]]),
      header_mode = as.character(header$header_mode[[1]]),
      data_start = as.integer(header$header_end[[1]] + 1L),
      data_end = as.integer(next_header - 1L),
      indikator = indikator,
      block_class = if_else(indikator == "PDRB ADHB", "raw_adhb", "raw_adhk"),
      satuan = unname(indicator_units[[indikator]])
    )
  })
  
  if (nrow(blocks) == 0) return(tibble::tibble())
  blocks %>% filter(data_start <= data_end) %>% arrange(data_start)
}

extract_main_category_code <- function(x) {
  x <- clean_text(x)
  pattern <- "^\\s*([A-U](?:\\s*[,;/]\\s*[A-U])*)\\s*(?:[.]|\\)|-|\\s+|$)"
  matched <- stringr::str_match(x, pattern)[, 2]
  matched <- stringr::str_replace_all(matched, "\\s*[,;/]\\s*", ",")
  matched <- stringr::str_replace_all(matched, "\\s+", "")
  ifelse(!is.na(matched) & nzchar(matched), matched, NA_character_)
}

extract_subcategory_code <- function(x) {
  x <- clean_text(x)
  pattern <- "^\\s*([0-9]{1,3}(?:\\s*[.-]\\s*[0-9]{1,3})*)\\s*(?:[.]|\\)|-|\\s+|$)"
  matched <- stringr::str_match(x, pattern)[, 2]
  matched <- stringr::str_replace_all(matched, "\\s+", "")
  ifelse(!is.na(matched) & nzchar(matched), matched, NA_character_)
}

extract_detail_category_code <- function(x) {
  x <- clean_text(x)
  pattern <- "^\\s*([a-z])\\s*(?:[.]|\\)|-)\\s*(?:\\s+|$)"
  matched <- stringr::str_match(x, pattern)[, 2]
  ifelse(!is.na(matched) & nzchar(matched), matched, NA_character_)
}

strip_detail_category_prefix <- function(x) {
  x <- clean_text(x)
  pattern <- "^\\s*[a-z]\\s*(?:[.]|\\)|-)\\s*(?:\\s+|$)"
  result <- stringr::str_remove(x, pattern)
  clean_text(result)
}

strip_main_category_prefix <- function(x) {
  x <- clean_text(x)
  pattern <- "^\\s*[A-U](?:\\s*[,;/]\\s*[A-U])*\\s*(?:[.]|\\)|-|\\s+|$)\\s*"
  result <- stringr::str_remove(x, pattern)
  clean_text(result)
}

strip_subcategory_prefix <- function(x) {
  x <- clean_text(x)
  pattern <- "^\\s*[0-9]{1,3}(?:\\s*[.-]\\s*[0-9]{1,3})*\\s*(?:[.]|\\)|-|\\s+|$)\\s*"
  result <- stringr::str_remove(x, pattern)
  clean_text(result)
}

infer_code_column <- function(raw, rows, excluded_columns) {
  candidate_columns <- setdiff(seq_len(ncol(raw)), excluded_columns)
  if (length(candidate_columns) == 0) return(NA_integer_)
  
  scores <- vapply(candidate_columns, function(column_number) {
    values <- unlist(raw[rows, column_number, drop = FALSE], use.names = FALSE)
    sum(!is.na(extract_main_category_code(values)))
  }, integer(1))
  
  if (max(scores, na.rm = TRUE) <= 0L) return(NA_integer_)
  candidate_columns[which.max(scores)]
}

build_row_metadata <- function(raw, row_number, value_columns, code_column) {
  descriptor_columns <- setdiff(seq_len(ncol(raw)), value_columns)
  cells <- clean_text(unlist(raw[row_number, descriptor_columns, drop = FALSE], use.names = FALSE))
  
  main_codes <- extract_main_category_code(cells)
  main_candidates <- main_codes[!is.na(main_codes)]
  main_code <- if (length(main_candidates) > 0) main_candidates[[1]] else NA_character_
  
  sub_codes <- extract_subcategory_code(cells)
  sub_candidates <- sub_codes[!is.na(sub_codes)]
  sub_code <- if (length(sub_candidates) > 0) sub_candidates[[1]] else NA_character_
  
  detail_codes <- extract_detail_category_code(cells)
  detail_candidates <- detail_codes[!is.na(detail_codes)]
  detail_code <- if (length(detail_candidates) > 0) detail_candidates[[1]] else NA_character_
  
  cleaned_cells <- cells
  main_positions <- which(!is.na(main_codes))
  sub_positions <- which(!is.na(sub_codes))
  detail_positions <- which(!is.na(detail_codes))
  if (length(main_positions) > 0) {
    cleaned_cells[main_positions] <- strip_main_category_prefix(cleaned_cells[main_positions])
  }
  if (length(sub_positions) > 0) {
    cleaned_cells[sub_positions] <- strip_subcategory_prefix(cleaned_cells[sub_positions])
  }
  if (length(detail_positions) > 0) {
    cleaned_cells[detail_positions] <- strip_detail_category_prefix(cleaned_cells[detail_positions])
  }
  
  keep_text <- !is.na(cleaned_cells)
  keep_text <- keep_text & is.na(extract_year_value(cleaned_cells))
  keep_text <- keep_text & is.na(normalize_period_value(cleaned_cells))
  keep_text <- keep_text & !stringr::str_detect(cleaned_cells, "^[+-]?[0-9]+([.,][0-9]+)?%?$")
  
  description_values <- cleaned_cells[keep_text]
  description_values <- description_values[!is.na(description_values) & nzchar(description_values)]
  description <- if (length(description_values) > 0) {
    stringr::str_squish(paste(description_values, collapse = " "))
  } else {
    NA_character_
  }
  
  all_values <- cleaned_cells[!is.na(cleaned_cells) & nzchar(cleaned_cells)]
  combined_text <- if (length(all_values) > 0) {
    stringr::str_squish(paste(all_values, collapse = " "))
  } else {
    NA_character_
  }
  
  tibble::tibble(
    source_row = as.integer(row_number),
    kode_raw = main_code,
    subkode_raw = sub_code,
    detailkode_raw = detail_code,
    uraian_raw = description,
    combined_text = combined_text
  )
}

parse_table_block <- function(raw, region_info, block_info) {
  sheet_code <- region_info$kode_wilayah[[1]]
  max_col <- ncol(raw)
  if (max_col < 4L) stop("Jumlah kolom pada sheet ", sheet_code, " tidak mencukupi.")
  
  header_mode <- as.character(block_info$header_mode[[1]])
  
  if (identical(header_mode, "combined")) {
    header_cells <- unlist(raw[block_info$year_row[[1]], , drop = FALSE], use.names = FALSE)
    year_values <- extract_combined_time_year(header_cells)
    period_values <- extract_combined_time_period(header_cells)
  } else {
    year_cells <- unlist(raw[block_info$year_row[[1]], , drop = FALSE], use.names = FALSE)
    period_cells <- unlist(raw[block_info$period_row[[1]], , drop = FALSE], use.names = FALSE)
    
    year_markers <- extract_year_value(year_cells)
    year_values <- fill_right(ifelse(is.na(year_markers), NA_character_, as.character(year_markers)))
    year_values <- suppressWarnings(as.integer(year_values))
    period_values <- normalize_period_value(period_cells)
  }
  
  value_columns <- which(!is.na(year_values) & !is.na(period_values))
  if (length(value_columns) == 0L) {
    stop(
      "Kolom nilai pada indikator `", block_info$indikator[[1]], "` di sheet `", sheet_code,
      "` tidak dapat dikenali. Pastikan header tahun dan periode sejajar dengan kolom nilai."
    )
  }
  
  period_meta <- tibble::tibble(
    value_col = paste0("V", seq_along(value_columns)),
    tahun = year_values[value_columns],
    periode = period_values[value_columns]
  )
  
  row_range <- seq.int(block_info$data_start[[1]], block_info$data_end[[1]])
  if (length(row_range) == 0L) return(tibble::tibble())
  
  numeric_values <- lapply(value_columns, function(column_number) {
    parse_numeric(unlist(raw[row_range, column_number, drop = FALSE], use.names = FALSE))
  })
  numeric_matrix <- do.call(cbind, numeric_values)
  if (is.null(dim(numeric_matrix))) numeric_matrix <- matrix(numeric_matrix, ncol = 1L)
  data_rows <- row_range[rowSums(!is.na(numeric_matrix)) > 0L]
  if (length(data_rows) == 0L) return(tibble::tibble())
  
  code_column <- infer_code_column(raw, data_rows, value_columns)
  if (is.na(code_column)) {
    stop(
      "Kode kategori utama A sampai U pada indikator `", block_info$indikator[[1]],
      "` di sheet `", sheet_code, "` tidak ditemukan. Posisi kolom boleh berpindah, ",
      "tetapi kode kategori tetap harus tersedia."
    )
  }
  
  meta <- purrr::map_dfr(
    data_rows,
    ~build_row_metadata(raw, .x, value_columns, code_column)
  ) %>%
    mutate(
      is_main = !is.na(kode_raw),
      is_total = stringr::str_detect(
        stringr::str_to_upper(dplyr::coalesce(combined_text, "")),
        "PRODUK DOMESTIK REGIONAL BRUTO|(^|[^A-Z])PDRB([^A-Z]|$)"
      ),
      is_subcategory = !is.na(subkode_raw) & !is.na(uraian_raw),
      is_detail = !is.na(detailkode_raw) & !is.na(uraian_raw) & is.na(subkode_raw),
      kode_utama = if_else(is_main, kode_raw, NA_character_),
      nama_utama = if_else(is_main, uraian_raw, NA_character_)
    ) %>%
    tidyr::fill(kode_utama, nama_utama, .direction = "down") %>%
    group_by(kode_utama) %>%
    mutate(
      .subkode_down = if_else(is_subcategory, subkode_raw, NA_character_),
      .subkode_up = .subkode_down
    ) %>%
    tidyr::fill(.subkode_down, .direction = "down") %>%
    tidyr::fill(.subkode_up, .direction = "up") %>%
    ungroup() %>%
    mutate(
      parent_subkode = dplyr::coalesce(.subkode_down, .subkode_up),
      level = case_when(
        is_total ~ "Total PDRB",
        is_main ~ "Kategori Utama",
        is_subcategory ~ "Subkategori",
        is_detail | !is.na(uraian_raw) ~ "Rincian",
        TRUE ~ "Lainnya"
      ),
      uraian = case_when(
        is_total ~ combined_text,
        TRUE ~ uraian_raw
      ),
      kode_kategori = case_when(
        is_total & stringr::str_detect(stringr::str_to_upper(combined_text), "TANPA MIGAS") ~ "PDRB_TANPA_MIGAS",
        is_total ~ "PDRB",
        is_main ~ kode_raw,
        is_subcategory ~ paste0(kode_utama, ".", subkode_raw),
        level == "Rincian" & !is.na(parent_subkode) & !is.na(detailkode_raw) ~ paste0(kode_utama, ".", parent_subkode, ".", detailkode_raw),
        TRUE ~ paste0(kode_utama, ".R", source_row)
      ),
      kategori_label = case_when(
        level == "Total PDRB" ~ uraian,
        level %in% c("Kategori Utama", "Subkategori", "Rincian") ~ paste0(kode_kategori, " - ", uraian),
        TRUE ~ uraian
      ),
      item_id = paste(
        level,
        kode_kategori,
        stringr::str_to_upper(stringr::str_squish(uraian)),
        sep = "__"
      )
    ) %>%
    filter(!is.na(uraian), uraian != "")
  
  value_data <- as.data.frame(raw[data_rows, value_columns, drop = FALSE], stringsAsFactors = FALSE)
  names(value_data) <- period_meta$value_col
  value_data$source_row <- data_rows
  
  meta %>%
    left_join(value_data, by = "source_row") %>%
    pivot_longer(
      cols = all_of(period_meta$value_col),
      names_to = "value_col",
      values_to = "nilai_raw"
    ) %>%
    inner_join(period_meta, by = "value_col") %>%
    mutate(
      nilai = parse_numeric(nilai_raw),
      kode_wilayah = region_info$kode_wilayah[[1]],
      wilayah = region_info$wilayah[[1]],
      kode_kelompok = region_info$kode_kelompok[[1]],
      kelompok = region_info$kelompok[[1]],
      jenis_wilayah = region_info$jenis_wilayah[[1]],
      indikator = block_info$indikator[[1]],
      satuan = block_info$satuan[[1]]
    ) %>%
    filter(!is.na(nilai), is.finite(nilai), abs(nilai) > 1e-12) %>%
    select(
      kode_kelompok, kelompok, kode_wilayah, wilayah, jenis_wilayah,
      indikator, satuan, level, kode_kategori, kategori_label, uraian,
      kode_utama, nama_utama, item_id, tahun, periode, nilai, source_row
    )
}

parse_sheet <- function(path, sheet_name, region_info) {
  raw <- readxl::read_excel(path, sheet = sheet_name, col_names = FALSE, col_types = "text", .name_repair = "minimal")
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  if (nrow(raw) == 0 || ncol(raw) == 0 || all(is.na(unlist(raw, use.names = FALSE)))) return(tibble::tibble())
  
  blocks <- detect_table_blocks(raw, sheet_name)
  if (nrow(blocks) == 0) return(tibble::tibble())
  
  parsed <- purrr::map_dfr(seq_len(nrow(blocks)), function(i) {
    tryCatch(
      parse_table_block(raw, region_info, blocks[i, , drop = FALSE]),
      error = function(e) tibble::tibble()
    )
  })
  
  if (!has_required_columns(parsed, c("indikator"))) {
    return(tibble::tibble())
  }
  
  parsed %>% filter(indikator %in% c("PDRB ADHB", "PDRB ADHK"))
}
# Audit data: kunci unik, diagnosis duplikasi, dan ringkasan aman.
pdrb_unique_key <- function(data) {
  # item_id menjadi bagian kunci agar baris total resmi seperti PDRB dan PDRB Nonmigas
  # tidak terbaca sebagai duplikasi saat kode kategori kosong atau total.
  intersect(
    c("kode_kelompok", "kode_wilayah", "indikator", "level", "kode_kategori", "item_id", "tahun", "periode"),
    names(data)
  )
}

diagnose_pdrb_duplicates <- function(data, tahap = "Data") {
  if (is.null(data) || nrow(data) == 0 || !"nilai" %in% names(data)) return(tibble::tibble())
  data <- data %>%
    mutate(
      level = as.character(level),
      periode = as.character(periode),
      nilai = suppressWarnings(as.numeric(nilai))
    )
  if (!"source_file" %in% names(data)) data$source_file <- NA_character_
  if (!"source_row" %in% names(data)) data$source_row <- NA_integer_
  key_cols <- pdrb_unique_key(data)
  if (length(key_cols) == 0) return(tibble::tibble())
  data %>%
    mutate(.nilai_round = round(nilai, 8)) %>%
    group_by(across(all_of(key_cols))) %>%
    summarise(
      `Jumlah Baris` = dplyr::n(),
      `Jumlah Nilai Berbeda` = dplyr::n_distinct(.nilai_round, na.rm = TRUE),
      `Nilai Minimum` = if (all(is.na(nilai))) NA_real_ else min(nilai, na.rm = TRUE),
      `Nilai Maksimum` = if (all(is.na(nilai))) NA_real_ else max(nilai, na.rm = TRUE),
      `Sumber File` = paste(unique(stats::na.omit(as.character(source_file))), collapse = "; "),
      `Baris Sumber` = paste(unique(stats::na.omit(as.character(source_row))), collapse = ", "),
      .groups = "drop"
    ) %>%
    filter(`Jumlah Baris` > 1) %>%
    mutate(
      Tahap = tahap,
      `Jenis Duplikasi` = dplyr::case_when(
        `Jumlah Nilai Berbeda` <= 1 ~ "Duplikasi identik",
        TRUE ~ "Duplikasi konflik nilai"
      )
    ) %>%
    select(Tahap, `Jenis Duplikasi`, all_of(key_cols), `Jumlah Baris`, `Jumlah Nilai Berbeda`, `Nilai Minimum`, `Nilai Maksimum`, `Sumber File`, `Baris Sumber`)
}

canonicalize_pdrb_rows <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  original_names <- names(data)
  data <- data %>%
    mutate(
      level = as.character(level),
      periode = as.character(periode)
    )
  if (!"source_file" %in% names(data)) data$source_file <- NA_character_
  if (!"source_row" %in% names(data)) data$source_row <- NA_integer_
  key_cols <- pdrb_unique_key(data)
  if (length(key_cols) == 0) return(data %>% select(any_of(original_names)))
  non_key_cols <- setdiff(names(data), key_cols)
  data <- if (all(c("source_file", "source_row") %in% names(data))) {
    data %>% arrange(source_file, source_row)
  } else if ("source_file" %in% names(data)) {
    data %>% arrange(source_file)
  } else if ("source_row" %in% names(data)) {
    data %>% arrange(source_row)
  } else {
    data
  }
  data %>%
    group_by(across(all_of(key_cols))) %>%
    summarise(across(all_of(non_key_cols), ~ dplyr::first(.x)), .groups = "drop") %>%
    select(any_of(original_names))
}

# Filter ketersediaan data input.
# Dashboard tidak mengunci data pada tahun berjalan, karena file PDRB dapat memuat
# data proyeksi, revisi, atau data tahun depan yang sudah terisi.
# Periode yang seluruh nilainya kosong/nol tidak dibaca.
# Total tahunan hanya dipakai jika empat triwulan pada tahun tersebut sudah tersedia,
# kecuali file memang hanya menyediakan data tahunan tanpa triwulan.
filter_available_pdrb_periods <- function(data, max_year = NULL) {
  if (is.null(data) || nrow(data) == 0) return(data)
  if (!all(c("tahun", "periode", "nilai") %in% names(data))) return(data)
  
  original_names <- names(data)
  quarter_levels <- c("I", "II", "III", "IV")
  zero_tol <- 1e-12
  
  data <- data %>%
    mutate(
      tahun = suppressWarnings(as.integer(tahun)),
      periode = as.character(periode),
      nilai = suppressWarnings(as.numeric(nilai))
    ) %>%
    filter(!is.na(tahun), !is.na(nilai), is.finite(nilai))
  
  if (nrow(data) == 0) return(data %>% select(any_of(original_names)))
  
  period_group_cols <- intersect(
    c("source_file", "kode_kelompok", "kode_wilayah", "indikator", "tahun", "periode"),
    names(data)
  )
  
  # Buang periode yang seluruh isinya kosong/nol.
  # Angka 0 tidak dibuang per baris, tetapi dibuang jika satu blok waktu memang kosong.
  if (length(period_group_cols) > 0) {
    data <- data %>%
      group_by(across(all_of(period_group_cols))) %>%
      filter(!all(is.na(nilai) | !is.finite(nilai) | abs(nilai) <= zero_tol)) %>%
      ungroup()
  }
  
  if (nrow(data) == 0) return(data %>% select(any_of(original_names)))
  
  # Sinkronisasi titik waktu rilis berdasarkan baris Total PDRB.
  # Jika Total PDRB pada suatu triwulan masih 0, maka seluruh baris pada
  # triwulan tersebut dianggap belum rilis dan tidak dipakai dashboard.
  # Aturan ini dinamis: saat triwulan berikutnya sudah berisi nilai positif,
  # triwulan tersebut otomatis ikut terbaca tanpa perlu mengubah kode.
  release_key_cols <- intersect(
    c("source_file", "kode_kelompok", "kode_wilayah", "indikator"),
    names(data)
  )
  if (length(release_key_cols) > 0 && all(c("level", "kode_kategori") %in% names(data))) {
    data <- data %>%
      mutate(
        .level_release = stringr::str_to_upper(stringr::str_squish(as.character(level))),
        .kode_release = stringr::str_to_upper(stringr::str_squish(as.character(kode_kategori))),
        .periode_release = as.character(periode),
        .nilai_release = suppressWarnings(as.numeric(nilai))
      )
    
    total_release <- data %>%
      filter(
        .periode_release %in% quarter_levels,
        .level_release == "TOTAL PDRB",
        .kode_release == "PDRB"
      ) %>%
      mutate(.is_released_time = is.finite(.nilai_release) & .nilai_release > zero_tol)
    
    total_release_groups <- total_release %>%
      distinct(across(all_of(release_key_cols))) %>%
      mutate(.has_total_release_row = TRUE)
    
    valid_release_times <- total_release %>%
      filter(.is_released_time) %>%
      distinct(across(all_of(c(release_key_cols, "tahun", "periode")))) %>%
      mutate(.is_released_time = TRUE)
    
    if (nrow(total_release_groups) > 0) {
      data <- data %>%
        left_join(total_release_groups, by = release_key_cols) %>%
        left_join(valid_release_times, by = c(release_key_cols, "tahun", "periode")) %>%
        filter(
          !(.periode_release %in% quarter_levels &
              dplyr::coalesce(.has_total_release_row, FALSE) &
              !dplyr::coalesce(.is_released_time, FALSE))
        ) %>%
        select(-.has_total_release_row, -.is_released_time)
    }
    
    data <- data %>%
      select(-.level_release, -.kode_release, -.periode_release, -.nilai_release)
  }
  
  if (nrow(data) == 0) return(data %>% select(any_of(original_names)))
  
  # Cut-off periode berjalan berbasis data aktual.
  # Masalah yang ditangani: kolom 2026 Triwulan II-IV atau tahun setelahnya
  # sering terisi 0.0/#REF! karena data belum rilis. Dashboard hanya menyimpan
  # triwulan sampai titik waktu terakhir yang memiliki Total PDRB positif.
  cutoff_group_cols <- intersect(
    c("source_file", "kode_kelompok", "kode_wilayah", "indikator"),
    names(data)
  )
  
  if (length(cutoff_group_cols) > 0) {
    data <- data %>%
      mutate(
        .periode_rank = match(as.character(periode), quarter_levels),
        .waktu_index_available = tahun * 10L + .periode_rank
      )
    
    cutoff_base <- data %>%
      filter(periode %in% quarter_levels, !is.na(.periode_rank)) %>%
      mutate(
        .level_chr = if ("level" %in% names(.)) as.character(level) else NA_character_,
        .kode_chr = if ("kode_kategori" %in% names(.)) as.character(kode_kategori) else NA_character_,
        .nilai_abs = abs(as.numeric(nilai))
      )
    
    total_cutoff <- cutoff_base %>%
      filter(
        .level_chr == "Total PDRB",
        .kode_chr == "PDRB",
        .nilai_abs > zero_tol
      ) %>%
      group_by(across(all_of(cutoff_group_cols))) %>%
      summarise(.latest_valid_index_total = max(.waktu_index_available, na.rm = TRUE), .groups = "drop")
    
    # Fallback untuk data mentah sebelum Total PDRB otomatis terbentuk.
    # Dipakai hanya kalau baris Total PDRB belum tersedia pada grup tersebut.
    sector_cutoff <- cutoff_base %>%
      filter(
        .level_chr == "Kategori Utama",
        stringr::str_detect(stringr::str_to_upper(stringr::str_squish(.kode_chr)), "^[A-U](,[A-U])*$"),
        .nilai_abs > zero_tol
      ) %>%
      group_by(across(all_of(c(cutoff_group_cols, "tahun", "periode", ".waktu_index_available")))) %>%
      summarise(.period_signal = sum(.nilai_abs, na.rm = TRUE), .groups = "drop") %>%
      filter(.period_signal > zero_tol) %>%
      group_by(across(all_of(cutoff_group_cols))) %>%
      summarise(.latest_valid_index_sector = max(.waktu_index_available, na.rm = TRUE), .groups = "drop")
    
    cutoff <- full_join(total_cutoff, sector_cutoff, by = cutoff_group_cols) %>%
      mutate(
        .latest_valid_index = dplyr::coalesce(.latest_valid_index_total, .latest_valid_index_sector)
      ) %>%
      select(all_of(cutoff_group_cols), .latest_valid_index)
    
    if (nrow(cutoff) > 0) {
      data <- data %>%
        left_join(cutoff, by = cutoff_group_cols) %>%
        filter(
          !(periode %in% quarter_levels) |
            is.na(.latest_valid_index) |
            .waktu_index_available <= .latest_valid_index
        ) %>%
        select(-.latest_valid_index)
    }
    
    data <- data %>%
      select(-.periode_rank, -.waktu_index_available)
  }
  
  if (nrow(data) == 0) return(data %>% select(any_of(original_names)))
  
  # Total tahunan hanya dipakai jika empat triwulan pada tahun tersebut sudah tersedia.
  # Jika file memang hanya berisi tahunan tanpa triwulan, Total tetap dipakai.
  year_group_cols <- setdiff(period_group_cols, "periode")
  if (length(year_group_cols) > 0) {
    quarter_availability <- data %>%
      filter(periode %in% quarter_levels) %>%
      group_by(across(all_of(year_group_cols))) %>%
      summarise(
        .quarter_count = dplyr::n_distinct(as.character(periode)),
        .groups = "drop"
      ) %>%
      mutate(
        .has_any_quarter = .quarter_count > 0L,
        .complete_quarters = .quarter_count == 4L
      )
    
    data <- data %>%
      left_join(
        quarter_availability %>% select(all_of(year_group_cols), .has_any_quarter, .complete_quarters),
        by = year_group_cols
      ) %>%
      filter(
        periode != "Total" |
          !dplyr::coalesce(.has_any_quarter, FALSE) |
          dplyr::coalesce(.complete_quarters, FALSE)
      ) %>%
      select(-.has_any_quarter, -.complete_quarters)
  }
  
  data %>% select(any_of(original_names))
}

# Sinkronisasi periode valid berdasarkan Total PDRB ADHB/ADHK yang benar-benar terisi.
# Satu titik waktu dianggap tersedia jika minimal salah satu dari Total PDRB ADHB
# atau Total PDRB ADHK memiliki nilai numerik positif.
# Jika hanya ADHB yang tersedia, ADHB tetap dibaca. Jika hanya ADHK yang tersedia,
# ADHK tetap dibaca. Baris bernilai 0/kosong/#REF! tetap tidak dianggap valid.
filter_complete_pdrb_value_periods <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  required_cols <- c("tahun", "periode", "nilai", "indikator", "level", "kode_kategori")
  if (!all(required_cols %in% names(data))) return(data)
  original_names <- names(data)
  zero_tol <- 1e-12
  key_cols <- intersect(
    c("source_file", "kode_kelompok", "kode_wilayah", "tahun", "periode"),
    names(data)
  )
  if (length(key_cols) == 0) return(data)
  valid_time_keys <- data %>%
    mutate(
      .indikator_chr = as.character(indikator),
      .level_chr = stringr::str_to_upper(stringr::str_squish(as.character(level))),
      .kode_chr = stringr::str_to_upper(stringr::str_squish(as.character(kode_kategori))),
      .nilai_num = suppressWarnings(as.numeric(nilai))
    ) %>%
    filter(
      .indikator_chr %in% c("PDRB ADHB", "PDRB ADHK"),
      .level_chr == "TOTAL PDRB",
      .kode_chr == "PDRB",
      !is.na(.nilai_num),
      is.finite(.nilai_num),
      .nilai_num > zero_tol
    ) %>%
    distinct(across(all_of(c(key_cols, ".indikator_chr")))) %>%
    group_by(across(all_of(key_cols))) %>%
    summarise(.jumlah_indikator_pdrb_valid = dplyr::n_distinct(.indikator_chr), .groups = "drop") %>%
    filter(.jumlah_indikator_pdrb_valid >= 1L) %>%
    select(all_of(key_cols))
  if (nrow(valid_time_keys) == 0) return(data[0, original_names, drop = FALSE])
  data %>% semi_join(valid_time_keys, by = key_cols) %>% select(any_of(original_names))
}

collapse_for_join <- function(data, key_cols) {
  if (is.null(data) || nrow(data) == 0) return(data)
  key_cols <- intersect(key_cols, names(data))
  if (length(key_cols) == 0) return(data)
  non_key_cols <- setdiff(names(data), key_cols)
  data <- if (all(c("source_file", "source_row") %in% names(data))) {
    data %>% arrange(source_file, source_row)
  } else if ("source_file" %in% names(data)) {
    data %>% arrange(source_file)
  } else if ("source_row" %in% names(data)) {
    data %>% arrange(source_row)
  } else {
    data
  }
  data %>%
    group_by(across(all_of(key_cols))) %>%
    summarise(across(all_of(non_key_cols), ~ dplyr::first(.x)), .groups = "drop")
}

remove_exact_display_duplicates <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  data %>% distinct()
}
# Total PDRB otomatis.
# Baris total PDRB dan PDRB Nonmigas dari Excel tidak dipakai sebagai input sektor.
# Dashboard menghitung PDRB dari kategori utama A-U, lalu menghitung PDRB Nonmigas
# jika komponen migas tersedia.
is_excel_total_pdrb_row <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(logical(0))
  level_text <- if ("level" %in% names(data)) as.character(data$level) else rep(NA_character_, nrow(data))
  code_text <- if ("kode_kategori" %in% names(data)) as.character(data$kode_kategori) else rep(NA_character_, nrow(data))
  uraian_text <- if ("uraian" %in% names(data)) as.character(data$uraian) else rep(NA_character_, nrow(data))
  label_text <- if ("kategori_label" %in% names(data)) as.character(data$kategori_label) else rep(NA_character_, nrow(data))
  combined <- stringr::str_to_upper(stringr::str_squish(dplyr::coalesce(uraian_text, label_text, "")))
  code_norm <- stringr::str_to_upper(stringr::str_squish(dplyr::coalesce(code_text, "")))
  level_norm <- stringr::str_to_upper(stringr::str_squish(dplyr::coalesce(level_text, "")))
  level_norm == "TOTAL PDRB" |
    code_norm %in% c("PDRB", "PDRB_TANPA_MIGAS", "PDRB_NON_MIGAS") |
    stringr::str_detect(combined, "^\\s*(PDRB|PRODUK DOMESTIK REGIONAL BRUTO)(\\s+TANPA\\s+MIGAS|\\s+NON\\s+MIGAS)?\\s*$")
}

is_main_sector_code <- function(x) {
  x <- stringr::str_to_upper(stringr::str_squish(as.character(x)))
  stringr::str_detect(x, "^[A-U](,[A-U])*$")
}

# Deduplikasi khusus kategori utama sebelum Total PDRB dihitung.
# Kunci sengaja tidak memakai item_id/uraian karena satu kategori yang sama dapat
# terbaca dari lebih dari satu blok tabel dengan penulisan label sedikit berbeda.
# Nilai pertama menurut urutan file dan baris sumber dipertahankan; nilai duplikat
# tidak pernah dijumlahkan. Ini menjaga Total PDRB identik dengan 17 baris kategori
# yang akhirnya ditampilkan dan diekspor dashboard.
canonicalize_main_sector_rows <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  required_cols <- c("level", "kode_kategori", "tahun", "periode", "nilai")
  if (!all(required_cols %in% names(data))) return(data)

  original_names <- names(data)
  data_work <- data %>%
    mutate(
      level = as.character(level),
      kode_kategori = stringr::str_to_upper(
        stringr::str_squish(as.character(kode_kategori))
      ),
      periode = as.character(periode),
      tahun = suppressWarnings(as.integer(tahun)),
      nilai = suppressWarnings(as.numeric(nilai))
    )

  if (!"source_file" %in% names(data_work)) data_work$source_file <- NA_character_
  if (!"source_row" %in% names(data_work)) data_work$source_row <- NA_integer_

  is_main <- dplyr::coalesce(
    data_work$level == "Kategori Utama" &
      is_main_sector_code(data_work$kode_kategori),
    FALSE
  )
  if (!any(is_main, na.rm = TRUE)) {
    return(data_work %>% select(any_of(original_names)))
  }

  key_cols <- intersect(
    c(
      "kode_kelompok", "kode_wilayah", "indikator",
      "level", "kode_kategori", "tahun", "periode"
    ),
    names(data_work)
  )
  if (length(key_cols) == 0) {
    return(data_work %>% select(any_of(original_names)))
  }

  main_rows <- data_work[is_main, , drop = FALSE] %>%
    arrange(source_file, source_row) %>%
    group_by(across(all_of(key_cols))) %>%
    slice(1L) %>%
    ungroup()

  other_rows <- data_work[!is_main, , drop = FALSE]

  bind_rows(other_rows, main_rows) %>%
    select(any_of(original_names))
}

category_hierarchy_arrange <- function(data) {
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
      .cat_level_chr = as.character(level),
      .cat_code_chr = stringr::str_to_upper(stringr::str_squish(as.character(kode_kategori))),
      .cat_label_chr = stringr::str_to_upper(stringr::str_squish(as.character(kategori_label))),
      .cat_is_total = .cat_level_chr == "Total PDRB" |
        .cat_code_chr %in% c("PDRB", "PDRB_TANPA_MIGAS", "PDRB_NON_MIGAS") |
        stringr::str_detect(.cat_label_chr, "PRODUK DOMESTIK REGIONAL BRUTO|(^|[^A-Z])PDRB([^A-Z]|$)"),
      .cat_total_rank = dplyr::case_when(
        .cat_code_chr == "PDRB" | stringr::str_detect(.cat_label_chr, "^PRODUK DOMESTIK REGIONAL BRUTO$") ~ 9900000,
        stringr::str_detect(.cat_code_chr, "TANPA_MIGAS|NON_MIGAS") | stringr::str_detect(.cat_label_chr, "TANPA MIGAS|NON MIGAS") ~ 9900001,
        TRUE ~ 9900099
      ),
      .cat_main_code = dplyr::coalesce(as.character(kode_utama), stringr::str_extract(.cat_code_chr, "^[A-U](?:,[A-U])?")),
      .cat_main_rank = main_rank(.cat_main_code),
      .cat_sub_rank = suppressWarnings(as.integer(stringr::str_match(.cat_code_chr, "\\.([0-9]+)")[, 2])),
      .cat_detail_code = stringr::str_match(stringr::str_to_lower(as.character(kode_kategori)), "\\.[0-9]+\\.([a-z])")[, 2],
      .cat_detail_rank = match(.cat_detail_code, letters),
      .cat_level_rank = dplyr::case_when(
        .cat_level_chr == "Kategori Utama" ~ 0L,
        .cat_level_chr == "Subkategori" ~ 1L,
        .cat_level_chr == "Rincian" ~ 2L,
        TRUE ~ 9L
      ),
      .cat_code_order = dplyr::coalesce(.cat_main_rank, 99L) * 100000 +
        dplyr::coalesce(.cat_sub_rank, 0L) * 1000 +
        .cat_level_rank * 100 +
        dplyr::coalesce(.cat_detail_rank, 0L),
      .cat_sort_order = dplyr::case_when(
        .cat_is_total ~ as.numeric(.cat_total_rank),
        !is.na(.cat_main_rank) ~ as.numeric(.cat_code_order),
        !is.na(source_row) ~ as.numeric(source_row),
        TRUE ~ as.numeric(dplyr::row_number()) + 9800000
      )
    ) %>%
    arrange(.cat_sort_order, source_row) %>%
    select(-dplyr::starts_with(".cat_"))
}

ensure_output_columns <- function(data, template_names) {
  missing_cols <- setdiff(template_names, names(data))
  for (col in missing_cols) data[[col]] <- NA
  data %>% select(any_of(template_names))
}

build_auto_pdrb_total <- function(sector_data) {
  if (is.null(sector_data) || nrow(sector_data) == 0) return(tibble::tibble())
  template_names <- names(sector_data)
  group_cols <- intersect(
    c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "indikator", "satuan", "tahun", "periode"),
    names(sector_data)
  )
  if (length(group_cols) == 0) return(tibble::tibble())

  # Pengamanan ganda: kategori utama selalu dikanonisasi kembali tepat sebelum
  # penjumlahan agar duplikat lintas blok/sheet tidak menaikkan Total PDRB.
  main_sector <- sector_data %>%
    canonicalize_main_sector_rows() %>%
    mutate(
      level = as.character(level),
      kode_kategori = stringr::str_to_upper(
        stringr::str_squish(as.character(kode_kategori))
      ),
      nilai = suppressWarnings(as.numeric(nilai))
    ) %>%
    filter(
      level == "Kategori Utama",
      is_main_sector_code(kode_kategori),
      !is.na(nilai),
      is.finite(nilai)
    )
  if (nrow(main_sector) == 0) return(tibble::tibble())

  main_sector %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      .jumlah_kategori = dplyr::n_distinct(kode_kategori),
      .jumlah_baris = dplyr::n(),
      nilai = sum(nilai, na.rm = TRUE),
      source_file = if ("source_file" %in% names(main_sector)) paste(unique(stats::na.omit(as.character(source_file))), collapse = "; ") else NA_character_,
      source_row = NA_integer_,
      .groups = "drop"
    ) %>%
    # Dashboard PDRB ini memakai struktur resmi 17 kategori. Total tidak dibuat
    # dari periode yang kategorinya belum lengkap.
    filter(.jumlah_kategori == 17L, .jumlah_baris == 17L) %>%
    select(-.jumlah_kategori, -.jumlah_baris) %>%
    mutate(
      level = "Total PDRB",
      kode_kategori = "PDRB",
      kategori_label = "Produk Domestik Regional Bruto",
      uraian = "Produk Domestik Regional Bruto",
      kode_utama = NA_character_,
      nama_utama = NA_character_,
      item_id = "Total PDRB__PDRB__PRODUK DOMESTIK REGIONAL BRUTO"
    ) %>%
    ensure_output_columns(template_names)
}

build_auto_pdrb_nonmigas <- function(sector_data, pdrb_total) {
  if (is.null(sector_data) || nrow(sector_data) == 0 || is.null(pdrb_total) || nrow(pdrb_total) == 0) {
    return(tibble::tibble())
  }
  template_names <- names(sector_data)
  if (!"source_file" %in% names(sector_data)) sector_data$source_file <- NA_character_
  if (!"source_row" %in% names(sector_data)) sector_data$source_row <- NA_integer_
  join_cols <- intersect(
    c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "indikator", "satuan", "tahun", "periode"),
    names(sector_data)
  )
  if (length(join_cols) == 0) return(tibble::tibble())
  prepared <- sector_data %>%
    mutate(
      level = as.character(level),
      kode_kategori = as.character(kode_kategori),
      uraian_upper = stringr::str_to_upper(stringr::str_squish(dplyr::coalesce(as.character(uraian), as.character(kategori_label), "")))
    )
  pertambangan_migas <- prepared %>%
    filter(
      stringr::str_detect(uraian_upper, "PERTAMBANGAN\\s+MINYAK.*GAS.*PANAS\\s+BUMI")
    ) %>%
    mutate(.migas_group = "B_MIGAS", .migas_priority = 1L)
  pengilangan_detail <- prepared %>%
    filter(
      stringr::str_detect(uraian_upper, "INDUSTRI\\s+PENGILANGAN\\s+MIGAS") &
        !stringr::str_detect(uraian_upper, "BATU\\s*BARA\\s+DAN\\s+PENGILANGAN\\s+MIGAS")
    ) %>%
    mutate(.migas_group = "C_PENGILANGAN", .migas_priority = 1L)
  pengilangan_agregat <- prepared %>%
    filter(
      stringr::str_detect(uraian_upper, "INDUSTRI\\s+BATU\\s*BARA\\s+DAN\\s+PENGILANGAN\\s+MIGAS")
    ) %>%
    mutate(.migas_group = "C_PENGILANGAN", .migas_priority = 2L)
  migas_candidates <- bind_rows(pertambangan_migas, pengilangan_detail, pengilangan_agregat)
  if (nrow(migas_candidates) == 0) return(tibble::tibble())
  # Satu komponen dipilih untuk setiap kelompok migas dan titik waktu.
  # Duplikat komponen dari blok/sheet lain tidak boleh ikut dijumlahkan.
  migas_selected <- migas_candidates %>%
    mutate(nilai = suppressWarnings(as.numeric(nilai))) %>%
    filter(!is.na(nilai), is.finite(nilai)) %>%
    arrange(.migas_priority, source_file, source_row) %>%
    group_by(across(all_of(c(join_cols, ".migas_group")))) %>%
    slice(1L) %>%
    ungroup() %>%
    group_by(across(all_of(join_cols))) %>%
    summarise(nilai_migas = sum(nilai, na.rm = TRUE), .groups = "drop")
  pdrb_total %>%
    left_join(migas_selected, by = join_cols) %>%
    filter(!is.na(nilai_migas)) %>%
    mutate(
      nilai = as.numeric(nilai) - nilai_migas,
      level = "Total PDRB",
      kode_kategori = "PDRB_TANPA_MIGAS",
      kategori_label = "Produk Domestik Regional Bruto Tanpa Migas",
      uraian = "Produk Domestik Regional Bruto Tanpa Migas",
      kode_utama = NA_character_,
      nama_utama = NA_character_,
      item_id = "Total PDRB__PDRB_TANPA_MIGAS__PRODUK DOMESTIK REGIONAL BRUTO TANPA MIGAS",
      source_row = NA_integer_
    ) %>%
    select(-nilai_migas) %>%
    ensure_output_columns(template_names)
}

ensure_annual_period_totals <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  if (!all(c("tahun", "periode", "nilai") %in% names(data))) return(data)
  
  data_work <- data %>% mutate(periode = as.character(periode))
  quarters <- data_work %>%
    filter(periode %in% c("I", "II", "III", "IV"), !is.na(nilai))
  
  if (nrow(quarters) == 0) return(data_work)
  
  group_cols <- setdiff(names(data_work), c("periode", "nilai", "source_row"))
  
  annual_rows <- quarters %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      periode = "Total",
      nilai = if (all(c("I", "II", "III", "IV") %in% as.character(periode))) sum(nilai, na.rm = TRUE) else NA_real_,
      source_row = NA_integer_,
      .groups = "drop"
    ) %>%
    filter(!is.na(nilai), is.finite(nilai)) %>%
    select(all_of(names(data_work)))
  
  if (nrow(annual_rows) == 0) return(data_work)
  
  key_cols <- setdiff(names(data_work), c("nilai", "source_row"))
  data_without_old_total <- data_work %>% anti_join(annual_rows %>% select(all_of(key_cols)), by = key_cols)
  bind_rows(data_without_old_total, annual_rows)
}

prepare_pdrb_input_with_auto_totals <- function(raw_data) {
  if (is.null(raw_data) || nrow(raw_data) == 0) return(raw_data)

  # Urutan wajib untuk menjaga hasil identik dengan Excel manual:
  # normalisasi -> buang total bawaan -> deduplikasi -> bentuk tahunan ->
  # hitung total dari 17 kategori -> hitung PDRB Tanpa Migas.
  sektor_input <- raw_data %>%
    mutate(
      level = as.character(level),
      periode = as.character(periode),
      kode_kategori = stringr::str_to_upper(
        stringr::str_squish(as.character(kode_kategori))
      ),
      nilai = suppressWarnings(as.numeric(nilai))
    ) %>%
    filter(!is_excel_total_pdrb_row(.)) %>%
    canonicalize_pdrb_rows() %>%
    canonicalize_main_sector_rows() %>%
    ensure_annual_period_totals() %>%
    canonicalize_pdrb_rows() %>%
    canonicalize_main_sector_rows()

  pdrb_total <- build_auto_pdrb_total(sektor_input) %>%
    canonicalize_pdrb_rows()

  pdrb_nonmigas <- build_auto_pdrb_nonmigas(sektor_input, pdrb_total) %>%
    canonicalize_pdrb_rows()

  bind_rows(sektor_input, pdrb_total, pdrb_nonmigas) %>%
    canonicalize_pdrb_rows()
}

read_pdrb_workbook <- function(path, source_name = basename(path), progress_callback = NULL, file_index = 1L, total_files = 1L) {
  sheet_index <- build_sheet_index(path)
  region_lookup <- build_region_lookup(path, sheet_index %>% distinct(kode_wilayah, .keep_all = TRUE))
  validation <- v5_scan_workbook_blocks(path, source_name)
  
  all_data <- purrr::pmap_dfr(
    list(sheet_name = sheet_index$sheet_name, sheet_code = sheet_index$kode_wilayah, sheet_no = seq_len(nrow(sheet_index))),
    function(sheet_name, sheet_code, sheet_no) {
      region_info <- region_lookup %>% filter(kode_wilayah == sheet_code) %>% slice(1)
      parsed_sheet <- tryCatch(parse_sheet(path, sheet_name, region_info), error = function(e) tibble::tibble())
      if (is.function(progress_callback)) {
        progress_callback(file_name = source_name, sheet_name = sheet_name, sheet_no = sheet_no, sheet_total = nrow(sheet_index), file_index = file_index, file_total = total_files)
      }
      parsed_sheet
    }
  )
  
  if (nrow(all_data) == 0) {
    stop("Tidak ada blok mentah PDRB ADHB/ADHK yang berhasil dibaca pada file `", source_name, "`. Tabel turunan seperti distribusi, pertumbuhan, LQ, DLQ, dan Extended Shift Share sengaja diabaikan sebagai input.")
  }
  
  all_data <- all_data %>% mutate(source_file = source_name)
  active_raw_excel <- all_data %>%
    filter(indikator %in% c("PDRB ADHB", "PDRB ADHK")) %>%
    mutate(
      periode = as.character(periode),
      level = as.character(level)
    ) %>%
    filter_available_pdrb_periods()
  active_sektor_input <- active_raw_excel %>% filter(!is_excel_total_pdrb_row(.))
  active_raw <- prepare_pdrb_input_with_auto_totals(active_raw_excel) %>%
    filter_available_pdrb_periods() %>%
    filter_complete_pdrb_value_periods()
  duplicate_diagnostics <- dplyr::bind_rows(
    diagnose_pdrb_duplicates(active_sektor_input, paste0("File ", source_name, " sektor input sebelum deduplikasi")),
    diagnose_pdrb_duplicates(active_raw, paste0("File ", source_name, " setelah PDRB otomatis"))
  )
  active_data <- active_raw %>%
    canonicalize_pdrb_rows() %>%
    mutate(
      periode = factor(as.character(periode), levels = c("I", "II", "III", "IV", "Total"), ordered = TRUE),
      level = factor(as.character(level), levels = c("Total PDRB", "Kategori Utama", "Subkategori", "Rincian", "Lainnya"))
    ) %>%
    arrange(kode_kelompok, kode_wilayah, indikator, level, source_row, tahun, periode)
  
  list(
    data = active_data,
    regions = region_lookup %>% mutate(source_file = source_name),
    validation = validation,
    duplicate_diagnostics = duplicate_diagnostics
  )
}

# Mengambil pesan error terdalam. Error purrr sering hanya menampilkan "In index: 1".
pdrb_deep_error_message <- function(error) {
  if (is.null(error)) return("Kesalahan tidak diketahui.")
  messages <- character()
  current <- error
  for (i in seq_len(12L)) {
    msg <- tryCatch(conditionMessage(current), error = function(e) NA_character_)
    if (!is.na(msg) && nzchar(msg) && !msg %in% messages) messages <- c(messages, msg)
    parent <- current$parent
    if (is.null(parent) || identical(parent, current)) break
    current <- parent
  }
  messages <- messages[!stringr::str_detect(messages, "^In index: \\d+\\.?$")]
  if (length(messages) == 0) "Kesalahan tidak diketahui." else paste(messages, collapse = " | ")
}

read_multiple_workbooks <- function(paths, source_names, progress_callback = NULL) {
  if (length(paths) == 0) stop("Tidak ada file Excel yang dipilih.")
  if (length(source_names) != length(paths)) source_names <- basename(paths)

  # Jangan memakai purrr::pmap di lapisan file. Dengan loop eksplisit, satu file yang
  # bermasalah tidak menutup pesan asli dengan pesan umum `In index: 1`.
  results <- list()
  failed_files <- character()

  for (file_index in seq_along(paths)) {
    path <- paths[[file_index]]
    source_name <- source_names[[file_index]]

    result <- tryCatch(
      read_pdrb_workbook(
        path = path,
        source_name = source_name,
        progress_callback = progress_callback,
        file_index = file_index,
        total_files = length(paths)
      ),
      error = function(e) {
        failed_files <<- c(
          failed_files,
          paste0("`", source_name, "`: ", pdrb_deep_error_message(e))
        )
        NULL
      }
    )

    if (!is.null(result)) results[[length(results) + 1L]] <- result
  }

  if (length(results) == 0) {
    stop(
      "Tidak ada file yang berhasil dibaca. ",
      paste(failed_files, collapse = " || "),
      call. = FALSE
    )
  }

  all_regions <- dplyr::bind_rows(lapply(results, function(x) x$regions))
  all_data_raw <- dplyr::bind_rows(lapply(results, function(x) x$data))
  validation <- dplyr::bind_rows(lapply(results, function(x) x$validation))
  duplicate_diagnostics <- dplyr::bind_rows(
    dplyr::bind_rows(lapply(results, function(x) {
      if (is.null(x$duplicate_diagnostics)) tibble::tibble() else x$duplicate_diagnostics
    })),
    diagnose_pdrb_duplicates(all_data_raw, "Gabungan semua file sebelum deduplikasi")
  )
  all_data <- canonicalize_pdrb_rows(all_data_raw)

  # File yang gagal tetap dicatat pada Validasi File, sementara file valid tetap diproses.
  if (length(failed_files) > 0) {
    failed_validation <- tibble::tibble(
      source_file = sub("^`([^`]*)`.*$", "\1", failed_files),
      sheet_name = NA_character_,
      block_class = "error_file",
      indikator = NA_character_,
      title_row = NA_integer_,
      title_text = NA_character_,
      reason = sub("^`[^`]*`:\\s*", "", failed_files)
    )
    validation <- dplyr::bind_rows(validation, failed_validation)
  }

  list(
    data = all_data,
    regions = all_regions %>% distinct(kode_wilayah, .keep_all = TRUE),
    validation = validation,
    duplicate_diagnostics = duplicate_diagnostics,
    failed_files = failed_files
  )
}

format_pdrb_number <- function(x, accuracy = 0.0001) {
  if (length(x) == 0) return(character(0))
  vapply(as.numeric(x), function(value) {
    if (is.na(value) || !is.finite(value)) return("–")
    scales::number(value, accuracy = accuracy, big.mark = ",", decimal.mark = ".")
  }, character(1))
}

format_pdrb <- function(x) {
  if (length(x) == 0) return(character(0))
  vapply(as.numeric(x), function(value) {
    if (is.na(value) || !is.finite(value)) return("–")
    paste0("Rp ", scales::number(value, accuracy = 0.0001, big.mark = ",", decimal.mark = "."), " Juta")
  }, character(1))
}

format_pdrb_card <- function(x) {
  if (length(x) == 0) return(character(0))
  vapply(as.numeric(x), function(value) {
    if (is.na(value) || !is.finite(value)) return("–")
    abs_value <- abs(value)
    # Data PDRB dibaca dalam satuan Juta Rupiah. Untuk card/grafik ringkasan,
    # gunakan satuan paling ringkas agar tidak menampilkan angka terlalu panjang.
    if (abs_value >= 1e6) {
      paste0(scales::number(value / 1e6, accuracy = 0.01, big.mark = ",", decimal.mark = "."), " Triliun Rupiah")
    } else if (abs_value >= 1e3) {
      paste0(scales::number(value / 1e3, accuracy = 0.01, big.mark = ",", decimal.mark = "."), " Miliar Rupiah")
    } else {
      paste0(scales::number(value, accuracy = 0.01, big.mark = ",", decimal.mark = "."), " Juta Rupiah")
    }
  }, character(1))
}


pdrb_value_scale <- function(values, force_triliun = FALSE) {
  values <- suppressWarnings(as.numeric(values))
  finite_values <- values[is.finite(values)]
  max_abs <- if (length(finite_values) > 0) max(abs(finite_values), na.rm = TRUE) else 0
  
  if (isTRUE(force_triliun) || max_abs >= 1e6) {
    list(scale = 1e6, title = "Triliun Rupiah")
  } else if (max_abs >= 1e3) {
    list(scale = 1e3, title = "Miliar Rupiah")
  } else {
    list(scale = 1, title = "Juta Rupiah")
  }
}

pdrb_dt_options <- function(pageLength = 10, buttons = FALSE, dom = NULL) {
  if (is.null(dom)) {
    dom <- if (isTRUE(buttons)) "Blfrtip" else "lfrtip"
  }
  opts <- list(
    pageLength = pageLength,
    lengthMenu = list(
      c(10, 15, 25, 50, 100, -1),
      c("10", "15", "25", "50", "100", "Semua")
    ),
    scrollX = TRUE,
    dom = dom
  )

  if (isTRUE(buttons)) {
    opts$buttons <- list(
      list(extend = "csvHtml5", text = "Unduh CSV"),
      list(extend = "excelHtml5", text = "Unduh Excel")
    )
  }

  opts
}

format_indicator_value <- function(x, indikator) {
  if (length(x) == 0) return(character(0))
  indikator <- as.character(indikator)[1]
  
  vapply(as.numeric(x), function(value) {
    if (is.na(value)) return("–")
    if (indikator %in% c("PDRB ADHB", "PDRB ADHK")) return(format_pdrb(value))
    if (indikator == "Indeks Implisit") {
      return(scales::number(value, accuracy = 0.01, big.mark = ",", decimal.mark = "."))
    }
    if (stringr::str_detect(indikator, "Sumber Pertumbuhan")) {
      return(paste0(scales::number(value, accuracy = 0.01, big.mark = ",", decimal.mark = "."), " poin"))
    }
    if (indikator %in% c("LQ", "DLQ") || stringr::str_detect(indikator, "^(LQ|DLQ)")) {
      return(scales::number(value, accuracy = 0.01, big.mark = ",", decimal.mark = "."))
    }
    paste0(scales::number(value, accuracy = 0.01, big.mark = ",", decimal.mark = "."), "%")
  }, character(1))
}



format_pdrb_plot <- function(x, scale_info = NULL) {
  if (length(x) == 0) return(character(0))
  values <- suppressWarnings(as.numeric(x))
  if (is.null(scale_info)) scale_info <- pdrb_value_scale(values)
  scale <- if (!is.null(scale_info$scale)) scale_info$scale else 1
  title <- if (!is.null(scale_info$title)) scale_info$title else "Juta Rupiah"
  unit <- if (stringr::str_detect(title, "Triliun")) {
    "Triliun"
  } else if (stringr::str_detect(title, "Miliar")) {
    "Miliar"
  } else {
    "Juta"
  }
  accuracy <- if (identical(unit, "Triliun")) 0.01 else 1
  vapply(values, function(value) {
    if (is.na(value) || !is.finite(value)) return("–")
    paste0(scales::number(value / scale, accuracy = accuracy, big.mark = ",", decimal.mark = "."), " ", unit)
  }, character(1))
}

format_indicator_value_plot <- function(x, indikator, scale_info = NULL) {
  indikator <- as.character(indikator)[1]
  if (indikator %in% c("PDRB ADHB", "PDRB ADHK")) return(format_pdrb_plot(x, scale_info))
  format_indicator_value(x, indikator)
}

format_indicator_value_card <- function(x, indikator) {
  if (length(x) == 0) return(character(0))
  indikator <- as.character(indikator)[1]
  vapply(as.numeric(x), function(value) {
    if (is.na(value) || !is.finite(value)) return("–")
    if (indikator %in% c("PDRB ADHB", "PDRB ADHK")) return(format_pdrb_card(value))
    format_indicator_value(value, indikator)
  }, character(1))
}

safe_value <- function(data, indicator_name) {
  value <- data %>% filter(indikator == indicator_name) %>% pull(nilai)
  if (length(value) == 0) NA_real_ else value[1]
}

period_label <- function(x) {
  dplyr::recode(
    as.character(x),
    "I" = "Triwulan I", "II" = "Triwulan II",
    "III" = "Triwulan III", "IV" = "Triwulan IV",
    "Total" = "Tahun"
  )
}

period_rank <- function(x) {
  match(as.character(x), c("I", "II", "III", "IV", "Total"))
}

add_time_columns <- function(data) {
  data %>%
    mutate(
      periode = as.character(periode),
      periode_urut = period_rank(periode),
      waktu = case_when(
        periode == "Total" ~ paste0(tahun, " - Tahunan"),
        TRUE ~ paste0(tahun, " - Triwulan ", periode)
      ),
      waktu_index = tahun * 10L + periode_urut
    )
}

quarterly_time_series <- function(data) {
  data <- add_time_columns(data)
  quarterly <- data %>% filter(periode %in% c("I", "II", "III", "IV"))
  if (nrow(quarterly) > 0) quarterly else data
}

latest_period_from <- function(data, prefer_quarter = TRUE) {
  candidates <- data %>%
    add_time_columns() %>%
    filter(!is.na(tahun), !is.na(periode_urut)) %>%
    distinct(tahun, periode, periode_urut)
  
  if (nrow(candidates) == 0) {
    return(list(
      tahun = NA_integer_,
      periode = NA_character_,
      label = "Periode terbaru"
    ))
  }
  
  latest_year <- max(candidates$tahun, na.rm = TRUE)
  candidates <- candidates %>% filter(tahun == latest_year)
  
  if (prefer_quarter && any(candidates$periode %in% c("I", "II", "III", "IV"))) {
    candidates <- candidates %>% filter(periode %in% c("I", "II", "III", "IV"))
  }
  
  selected <- candidates %>%
    arrange(desc(periode_urut)) %>%
    slice(1)
  
  list(
    tahun = as.integer(selected$tahun[[1]]),
    periode = as.character(selected$periode[[1]]),
    label = paste(period_label(selected$periode[[1]]), selected$tahun[[1]])
  )
}

# Periode default berbasis data aktual.
# Jika tahun terakhir sudah memiliki Total, tampilkan Tahunan/Total.
# Jika tahun terakhir baru memiliki Triwulan I-III, tampilkan triwulan terakhir.
# Patokan nilai: Total PDRB ADHB atau ADHK yang numerik dan positif.
latest_overview_period_from_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(tahun = NA_integer_, periode = NA_character_, label = "Periode terbaru"))
  }
  quarter_levels <- c("I", "II", "III", "IV")
  zero_tol <- 1e-12
  data_valid <- data %>%
    mutate(
      tahun = suppressWarnings(as.integer(tahun)),
      periode = as.character(periode),
      nilai_num = suppressWarnings(as.numeric(nilai)),
      indikator_chr = as.character(indikator),
      level_chr = if ("level" %in% names(.)) stringr::str_to_upper(stringr::str_squish(as.character(level))) else NA_character_,
      kode_chr = if ("kode_kategori" %in% names(.)) stringr::str_to_upper(stringr::str_squish(as.character(kode_kategori))) else NA_character_
    ) %>%
    filter(
      !is.na(tahun),
      periode %in% c(quarter_levels, "Total"),
      indikator_chr %in% c("PDRB ADHB", "PDRB ADHK"),
      if (all(c("level", "kode_kategori") %in% names(data))) level_chr == "TOTAL PDRB" & kode_chr == "PDRB" else TRUE,
      is.finite(nilai_num),
      nilai_num > zero_tol
    ) %>%
    distinct(tahun, periode)
  
  if (nrow(data_valid) == 0) {
    return(list(tahun = NA_integer_, periode = NA_character_, label = "Periode terbaru"))
  }
  
  latest_year <- max(data_valid$tahun, na.rm = TRUE)
  year_periods <- data_valid %>% filter(tahun == latest_year) %>% pull(periode) %>% as.character()
  
  selected_period <- if ("Total" %in% year_periods || all(quarter_levels %in% year_periods)) {
    "Total"
  } else {
    q <- intersect(quarter_levels, year_periods)
    if (length(q) > 0) q[which.max(match(q, quarter_levels))] else year_periods[which.max(period_rank(year_periods))]
  }
  
  list(
    tahun = as.integer(latest_year),
    periode = as.character(selected_period),
    label = paste(period_label(selected_period), latest_year)
  )
}


# Menghitung indikator pertumbuhan dari deret PDRB ADHB dan ADHK.
# Q-to-Q membandingkan triwulan berurutan; Y-on-Y membandingkan triwulan yang
# sama dengan tahun sebelumnya; C-to-C membandingkan nilai kumulatif sampai
# triwulan berjalan terhadap kumulatif periode yang sama pada tahun sebelumnya.
derive_growth_indicators <- function(data) {
  if (nrow(data) == 0) return(data[0, , drop = FALSE])
  if (!all(c("indikator", "nilai", "tahun", "periode") %in% names(data))) {
    return(data[0, , drop = FALSE])
  }
  
  pdrb <- data %>%
    filter(
      indikator %in% c("PDRB ADHB", "PDRB ADHK"),
      !is.na(nilai), !is.na(tahun), !is.na(periode)
    ) %>%
    mutate(
      dasar_harga = if_else(indikator == "PDRB ADHB", "ADHB", "ADHK"),
      periode_teks = as.character(periode),
      periode_urut_hitung = match(periode_teks, c("I", "II", "III", "IV", "Total"))
    )
  
  if (nrow(pdrb) == 0) return(data[0, , drop = FALSE])
  
  group_columns <- intersect(
    c(
      "source_file", "kode_kelompok", "kelompok", "kode_wilayah", "wilayah",
      "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian",
      "kode_utama", "nama_utama", "item_id", "source_row", "dasar_harga"
    ),
    names(pdrb)
  )
  
  quarters <- pdrb %>%
    filter(periode_teks %in% c("I", "II", "III", "IV")) %>%
    mutate(indeks_triwulan = tahun * 4L + periode_urut_hitung)
  
  qoq <- quarters %>%
    group_by(across(all_of(group_columns))) %>%
    arrange(indeks_triwulan, .by_group = TRUE) %>%
    mutate(
      nilai_sebelumnya = lag(nilai),
      indeks_sebelumnya = lag(indeks_triwulan),
      nilai_pertumbuhan = if_else(
        !is.na(nilai_sebelumnya) & indeks_triwulan - indeks_sebelumnya == 1L & nilai_sebelumnya != 0,
        (nilai / nilai_sebelumnya - 1) * 100,
        NA_real_
      )
    ) %>%
    ungroup()
  
  yoy_quarter <- quarters %>%
    group_by(across(all_of(group_columns)), periode_teks) %>%
    arrange(tahun, .by_group = TRUE) %>%
    mutate(
      tahun_sebelumnya = lag(tahun),
      nilai_sebelumnya = lag(nilai),
      nilai_pertumbuhan = if_else(
        !is.na(nilai_sebelumnya) & tahun - tahun_sebelumnya == 1L & nilai_sebelumnya != 0,
        (nilai / nilai_sebelumnya - 1) * 100,
        NA_real_
      )
    ) %>%
    ungroup()
  
  totals <- pdrb %>%
    filter(periode_teks == "Total") %>%
    group_by(across(all_of(group_columns))) %>%
    arrange(tahun, .by_group = TRUE) %>%
    mutate(
      tahun_sebelumnya = lag(tahun),
      nilai_sebelumnya = lag(nilai),
      nilai_pertumbuhan = if_else(
        !is.na(nilai_sebelumnya) & tahun - tahun_sebelumnya == 1L & nilai_sebelumnya != 0,
        (nilai / nilai_sebelumnya - 1) * 100,
        NA_real_
      )
    ) %>%
    ungroup()
  
  ctc_quarter <- quarters %>%
    group_by(across(all_of(group_columns)), tahun) %>%
    arrange(periode_urut_hitung, .by_group = TRUE) %>%
    mutate(nilai_kumulatif = cumsum(nilai)) %>%
    ungroup() %>%
    group_by(across(all_of(group_columns)), periode_teks) %>%
    arrange(tahun, .by_group = TRUE) %>%
    mutate(
      tahun_sebelumnya = lag(tahun),
      kumulatif_sebelumnya = lag(nilai_kumulatif),
      nilai_pertumbuhan = if_else(
        !is.na(kumulatif_sebelumnya) & tahun - tahun_sebelumnya == 1L & kumulatif_sebelumnya != 0,
        (nilai_kumulatif / kumulatif_sebelumnya - 1) * 100,
        NA_real_
      )
    ) %>%
    ungroup()
  
  as_growth_rows <- function(x, method_name) {
    x %>%
      filter(!is.na(nilai_pertumbuhan), is.finite(nilai_pertumbuhan)) %>%
      mutate(
        indikator = paste("Pertumbuhan", dasar_harga, method_name),
        satuan = "Persen",
        nilai = nilai_pertumbuhan
      ) %>%
      select(all_of(names(data)))
  }
  
  bind_rows(
    as_growth_rows(qoq, "Q-to-Q"),
    as_growth_rows(bind_rows(yoy_quarter, totals), "Y-on-Y"),
    as_growth_rows(bind_rows(ctc_quarter, totals), "C-to-C")
  )
}
# Indikator turunan dihitung ulang dari data mentah ADHB dan ADHK.
derive_distribution_indicators_v5 <- function(data) {
  if (nrow(data) == 0) return(data[0, , drop = FALSE])
  if (!has_required_columns(data, c("indikator"))) return(data[0, , drop = FALSE])
  raw <- data %>% mutate(level = as.character(level), periode = as.character(periode)) %>% filter(indikator == "PDRB ADHB")
  if (nrow(raw) == 0) return(data[0, , drop = FALSE])
  total <- raw %>% filter(level == "Total PDRB", kode_kategori == "PDRB") %>% select(kode_wilayah, tahun, periode, total_pdrb = nilai)
  out <- raw %>%
    filter(level != "Total PDRB") %>%
    left_join(total, by = c("kode_wilayah", "tahun", "periode")) %>%
    mutate(
      indikator = "Distribusi PDRB",
      satuan = "Persen",
      nilai = if_else(!is.na(total_pdrb) & total_pdrb != 0, nilai / total_pdrb * 100, NA_real_),
      source_file = "Dihitung otomatis V5",
      source_row = NA_integer_
    ) %>%
    filter(!is.na(nilai), is.finite(nilai))
  out %>% select(all_of(names(data)))
}

derive_implicit_indicators_v5 <- function(data) {
  if (nrow(data) == 0) return(data[0, , drop = FALSE])
  if (!has_required_columns(data, c("indikator"))) return(data[0, , drop = FALSE])
  raw <- data %>% mutate(periode = as.character(periode), level = as.character(level)) %>% filter(indikator %in% c("PDRB ADHB", "PDRB ADHK"))
  if (!all(c("PDRB ADHB", "PDRB ADHK") %in% get_indikator_values(raw))) return(data[0, , drop = FALSE])
  key_cols <- intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id", "tahun", "periode"), names(raw))
  wide <- raw %>%
    select(all_of(key_cols), indikator, nilai) %>%
    group_by(across(all_of(c(key_cols, "indikator")))) %>%
    summarise(nilai = mean(nilai, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = indikator, values_from = nilai)
  idx <- wide %>%
    mutate(nilai = if_else(!is.na(`PDRB ADHB`) & !is.na(`PDRB ADHK`) & `PDRB ADHK` != 0, `PDRB ADHB` / `PDRB ADHK` * 100, NA_real_)) %>%
    filter(!is.na(nilai), is.finite(nilai)) %>%
    mutate(indikator = "Indeks Implisit", satuan = "Indeks", source_file = "Dihitung otomatis V5", source_row = NA_integer_)
  idx_full <- idx %>% select(any_of(names(data)))
  missing_cols <- setdiff(names(data), names(idx_full))
  for (col in missing_cols) idx_full[[col]] <- NA
  idx_full <- idx_full %>% select(all_of(names(data)))
  
  ensure_data_columns <- function(x) {
    missing_cols <- setdiff(names(data), names(x))
    for (col in missing_cols) x[[col]] <- NA
    x %>% select(all_of(names(data)))
  }
  finalize_laju <- function(x, label) {
    ensure_data_columns(
      x %>%
        filter(!is.na(nilai), is.finite(nilai)) %>%
        mutate(
          indikator = label,
          satuan = "Persen",
          source_file = "Dihitung otomatis V5",
          source_row = NA_integer_
        )
    )
  }
  implicit_group_cols <- setdiff(
    intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id"), names(idx_full)),
    character(0)
  )
  qoq <- idx_full %>%
    mutate(
      periode_teks = as.character(periode),
      periode_urut = match(periode_teks, c("I", "II", "III", "IV", "Total")),
      indeks_waktu = tahun * 4L + periode_urut
    ) %>%
    filter(periode_teks %in% c("I", "II", "III", "IV")) %>%
    group_by(across(all_of(implicit_group_cols))) %>%
    arrange(indeks_waktu, .by_group = TRUE) %>%
    mutate(
      nilai_lag = lag(nilai),
      indeks_lag = lag(indeks_waktu),
      nilai = if_else(!is.na(nilai_lag) & indeks_waktu - indeks_lag == 1L & nilai_lag != 0, (nilai / nilai_lag - 1) * 100, NA_real_)
    ) %>%
    ungroup()
  qoq <- finalize_laju(qoq, "Laju Indeks Implisit Q-to-Q")
  
  yoy_quarter <- idx_full %>%
    mutate(periode_teks = as.character(periode)) %>%
    filter(periode_teks %in% c("I", "II", "III", "IV")) %>%
    group_by(across(all_of(implicit_group_cols)), periode_teks) %>%
    arrange(tahun, .by_group = TRUE) %>%
    mutate(
      nilai_lag = lag(nilai),
      tahun_lag = lag(tahun),
      nilai = if_else(!is.na(nilai_lag) & tahun - tahun_lag == 1L & nilai_lag != 0, (nilai / nilai_lag - 1) * 100, NA_real_)
    ) %>%
    ungroup()
  yoy_total <- idx_full %>%
    mutate(periode_teks = as.character(periode)) %>%
    filter(periode_teks == "Total") %>%
    group_by(across(all_of(implicit_group_cols))) %>%
    arrange(tahun, .by_group = TRUE) %>%
    mutate(
      nilai_lag = lag(nilai),
      tahun_lag = lag(tahun),
      nilai = if_else(!is.na(nilai_lag) & tahun - tahun_lag == 1L & nilai_lag != 0, (nilai / nilai_lag - 1) * 100, NA_real_)
    ) %>%
    ungroup()
  yoy <- finalize_laju(bind_rows(yoy_quarter, yoy_total), "Laju Indeks Implisit Y-on-Y")
  
  wide_work <- wide %>%
    mutate(
      periode_teks = as.character(periode),
      periode_urut = match(periode_teks, c("I", "II", "III", "IV", "Total"))
    )
  wide_group_cols <- setdiff(
    intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id"), names(wide_work)),
    character(0)
  )
  ctc_quarter <- wide_work %>%
    filter(periode_teks %in% c("I", "II", "III", "IV")) %>%
    group_by(across(all_of(wide_group_cols)), tahun) %>%
    arrange(periode_urut, .by_group = TRUE) %>%
    mutate(
      adhb_kumulatif = cumsum(`PDRB ADHB`),
      adhk_kumulatif = cumsum(`PDRB ADHK`),
      indeks_kumulatif = if_else(!is.na(adhb_kumulatif) & !is.na(adhk_kumulatif) & adhk_kumulatif != 0, adhb_kumulatif / adhk_kumulatif * 100, NA_real_)
    ) %>%
    ungroup() %>%
    group_by(across(all_of(wide_group_cols)), periode_teks) %>%
    arrange(tahun, .by_group = TRUE) %>%
    mutate(
      indeks_kumulatif_lag = lag(indeks_kumulatif),
      tahun_lag = lag(tahun),
      nilai = if_else(!is.na(indeks_kumulatif_lag) & tahun - tahun_lag == 1L & indeks_kumulatif_lag != 0, (indeks_kumulatif / indeks_kumulatif_lag - 1) * 100, NA_real_)
    ) %>%
    ungroup()
  ctc_total <- idx_full %>%
    mutate(periode_teks = as.character(periode)) %>%
    filter(periode_teks == "Total") %>%
    group_by(across(all_of(implicit_group_cols))) %>%
    arrange(tahun, .by_group = TRUE) %>%
    mutate(
      nilai_lag = lag(nilai),
      tahun_lag = lag(tahun),
      nilai = if_else(!is.na(nilai_lag) & tahun - tahun_lag == 1L & nilai_lag != 0, (nilai / nilai_lag - 1) * 100, NA_real_)
    ) %>%
    ungroup()
  ctc <- finalize_laju(bind_rows(ctc_quarter, ctc_total), "Laju Indeks Implisit C-to-C")
  
  bind_rows(idx_full, qoq, yoy, ctc)
}

derive_growth_indicators_v5 <- function(data) {
  if (nrow(data) == 0) return(data[0, , drop = FALSE])
  if (!has_required_columns(data, c("indikator"))) return(data[0, , drop = FALSE])
  pdrb <- data %>%
    filter(indikator %in% c("PDRB ADHB", "PDRB ADHK"), !is.na(nilai), !is.na(tahun), !is.na(periode)) %>%
    mutate(dasar_harga = if_else(indikator == "PDRB ADHB", "ADHB", "ADHK"), periode_teks = as.character(periode), periode_urut_hitung = match(periode_teks, c("I", "II", "III", "IV", "Total")))
  if (nrow(pdrb) == 0) return(data[0, , drop = FALSE])
  group_columns <- intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id", "dasar_harga"), names(pdrb))
  quarters <- pdrb %>% filter(periode_teks %in% c("I", "II", "III", "IV")) %>% mutate(indeks_triwulan = tahun * 4L + periode_urut_hitung)
  qoq <- quarters %>% group_by(across(all_of(group_columns))) %>% arrange(indeks_triwulan, .by_group = TRUE) %>% mutate(nilai_sebelumnya = lag(nilai), indeks_sebelumnya = lag(indeks_triwulan), nilai_pertumbuhan = if_else(!is.na(nilai_sebelumnya) & indeks_triwulan - indeks_sebelumnya == 1L & nilai_sebelumnya != 0, (nilai / nilai_sebelumnya - 1) * 100, NA_real_)) %>% ungroup()
  yoy_quarter <- quarters %>% group_by(across(all_of(group_columns)), periode_teks) %>% arrange(tahun, .by_group = TRUE) %>% mutate(tahun_sebelumnya = lag(tahun), nilai_sebelumnya = lag(nilai), nilai_pertumbuhan = if_else(!is.na(nilai_sebelumnya) & tahun - tahun_sebelumnya == 1L & nilai_sebelumnya != 0, (nilai / nilai_sebelumnya - 1) * 100, NA_real_)) %>% ungroup()
  totals <- pdrb %>% filter(periode_teks == "Total") %>% group_by(across(all_of(group_columns))) %>% arrange(tahun, .by_group = TRUE) %>% mutate(tahun_sebelumnya = lag(tahun), nilai_sebelumnya = lag(nilai), nilai_pertumbuhan = if_else(!is.na(nilai_sebelumnya) & tahun - tahun_sebelumnya == 1L & nilai_sebelumnya != 0, (nilai / nilai_sebelumnya - 1) * 100, NA_real_)) %>% ungroup()
  ctc_quarter <- quarters %>% group_by(across(all_of(group_columns)), tahun) %>% arrange(periode_urut_hitung, .by_group = TRUE) %>% mutate(nilai_kumulatif = cumsum(nilai)) %>% ungroup() %>% group_by(across(all_of(group_columns)), periode_teks) %>% arrange(tahun, .by_group = TRUE) %>% mutate(tahun_sebelumnya = lag(tahun), kumulatif_sebelumnya = lag(nilai_kumulatif), nilai_pertumbuhan = if_else(!is.na(kumulatif_sebelumnya) & tahun - tahun_sebelumnya == 1L & kumulatif_sebelumnya != 0, (nilai_kumulatif / kumulatif_sebelumnya - 1) * 100, NA_real_)) %>% ungroup()
  as_growth_rows <- function(x, method_name) {
    x %>% filter(!is.na(nilai_pertumbuhan), is.finite(nilai_pertumbuhan)) %>% mutate(indikator = paste("Pertumbuhan", dasar_harga, method_name), satuan = "Persen", nilai = nilai_pertumbuhan, source_file = "Dihitung otomatis V5", source_row = NA_integer_) %>% select(all_of(names(data)))
  }
  bind_rows(as_growth_rows(qoq, "Q-to-Q"), as_growth_rows(bind_rows(yoy_quarter, totals), "Y-on-Y"), as_growth_rows(bind_rows(ctc_quarter, totals), "C-to-C"))
}

derive_source_growth_indicators_v5 <- function(data) {
  if (nrow(data) == 0) return(data[0, , drop = FALSE])
  if (!has_required_columns(data, c("indikator"))) return(data[0, , drop = FALSE])
  pdrb <- data %>% filter(indikator %in% c("PDRB ADHB", "PDRB ADHK")) %>% mutate(dasar_harga = if_else(indikator == "PDRB ADHB", "ADHB", "ADHK"), periode = as.character(periode), periode_urut = match(periode, c("I", "II", "III", "IV", "Total")))
  if (nrow(pdrb) == 0) return(data[0, , drop = FALSE])
  total <- pdrb %>% filter(level == "Total PDRB", kode_kategori == "PDRB") %>% select(kode_wilayah, dasar_harga, tahun, periode, total_pdrb = nilai)
  components <- pdrb %>%
    filter(!is.na(nilai), is.finite(nilai))
  group_cols <- intersect(c("kode_kelompok", "kelompok", "kode_wilayah", "wilayah", "jenis_wilayah", "level", "kode_kategori", "kategori_label", "uraian", "kode_utama", "nama_utama", "item_id", "dasar_harga"), names(components))
  quarters <- components %>% filter(periode %in% c("I", "II", "III", "IV")) %>% mutate(idx = tahun * 4L + periode_urut)
  total_q <- total %>% filter(periode %in% c("I", "II", "III", "IV")) %>% mutate(idx = tahun * 4L + match(periode, c("I", "II", "III", "IV")))
  qoq <- quarters %>% group_by(across(all_of(group_cols))) %>% arrange(idx, .by_group = TRUE) %>% mutate(nilai_lag = lag(nilai), idx_lag = lag(idx)) %>% ungroup() %>% left_join(total_q %>% select(kode_wilayah, dasar_harga, idx, total_pdrb_lag = total_pdrb) %>% mutate(idx = idx + 1L), by = c("kode_wilayah", "dasar_harga", "idx")) %>% mutate(nilai_sumber = if_else(!is.na(nilai_lag) & !is.na(total_pdrb_lag) & total_pdrb_lag != 0 & idx - idx_lag == 1L, (nilai - nilai_lag) / total_pdrb_lag * 100, NA_real_))
  yoy <- quarters %>% group_by(across(all_of(group_cols)), periode) %>% arrange(tahun, .by_group = TRUE) %>% mutate(nilai_lag = lag(nilai), tahun_lag = lag(tahun)) %>% ungroup() %>% left_join(total %>% select(kode_wilayah, dasar_harga, tahun, periode, total_pdrb_lag = total_pdrb) %>% mutate(tahun = tahun + 1L), by = c("kode_wilayah", "dasar_harga", "tahun", "periode")) %>% mutate(nilai_sumber = if_else(!is.na(nilai_lag) & !is.na(total_pdrb_lag) & total_pdrb_lag != 0 & tahun - tahun_lag == 1L, (nilai - nilai_lag) / total_pdrb_lag * 100, NA_real_))
  ctc <- quarters %>% group_by(across(all_of(group_cols)), tahun) %>% arrange(periode_urut, .by_group = TRUE) %>% mutate(kumulatif = cumsum(nilai)) %>% ungroup() %>% group_by(across(all_of(group_cols)), periode) %>% arrange(tahun, .by_group = TRUE) %>% mutate(kum_lag = lag(kumulatif), tahun_lag = lag(tahun)) %>% ungroup() %>% left_join(total %>% filter(periode %in% c("I", "II", "III", "IV")) %>% group_by(kode_wilayah, dasar_harga, tahun) %>% arrange(match(periode, c("I", "II", "III", "IV")), .by_group = TRUE) %>% mutate(total_kum = cumsum(total_pdrb)) %>% ungroup() %>% select(kode_wilayah, dasar_harga, tahun, periode, total_pdrb_lag = total_kum) %>% mutate(tahun = tahun + 1L), by = c("kode_wilayah", "dasar_harga", "tahun", "periode")) %>% mutate(nilai_sumber = if_else(!is.na(kum_lag) & !is.na(total_pdrb_lag) & total_pdrb_lag != 0 & tahun - tahun_lag == 1L, (kumulatif - kum_lag) / total_pdrb_lag * 100, NA_real_))
  as_rows <- function(x, method) {
    x %>% filter(!is.na(nilai_sumber), is.finite(nilai_sumber)) %>% mutate(indikator = paste("Sumber Pertumbuhan", dasar_harga, method), satuan = "Persen Poin", nilai = nilai_sumber, source_file = "Dihitung otomatis V5", source_row = NA_integer_) %>% select(all_of(names(data)))
  }
  bind_rows(as_rows(qoq, "Q-to-Q"), as_rows(yoy, "Y-on-Y"), as_rows(ctc, "C-to-C"))
}


# Workbook contoh bawaan disimpan di server.R sebagai cadangan teknis.
# File template utama tetap diambil dari folder www.
SYNTHETIC_XLSX_BASE64 <- paste0(
  "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",
  "kztITJx5xREBdEUCI5UcBhYXMuRQ7pKQBhMBzZTJRPACgmSmHICY+gu98gy5KRXOrT45f0Usg4ZOXtIlEhSKsAwFIRdy4+/vk2p3",
  "jNf6LIFthFQyZNUXykOJwT0zckPYVCXzrtomC4Xb4lTNuxq+JmBLw3punS0n/9te1D20Fz1G86OZ4B6zh3OberjCRaz/WNYe+TLf",
  "OXDbOt4DXuYTLEOkfsF9ioqAEativrqvT/klnDu0e/GBIJv81tuk9t3gDHzUq1qlZCsRP0sHfB+SBmOMW/Q0X48UYq2msa3G2jEM",
  "eYBY8wyhZjjfh0WaGjPVi6w5jQpvQdVA5T/b1A1o9g00HJEFXjGZtjaj5E4KPNz+7w2wwsSO4e2LvwFQSwMEFAAAAAgA4xvRXIM+",
  "w+ZUTQAAfH8CABgAAAB4bC93b3Jrc2hlZXRzL3NoZWV0MS54bWytvVt35MbRpf1XeunahnA+eNlay1azTgCqcKgC1lzSblriqLup",
  "6Wa/fv39+i+yyGoWIjeATYieGQ2lfBK1M6sikBmIDPz1Pw9ffvv6693d47v//fTx89e//fDr4+Pvf/nxx6//+vXu0+1X5+H3u8/S",
  "8u+HL59uH+Vfv/zy49ffv9zdfjh3+vTxR9914x8/3d5//uGnv57/W/Xlp78+fHv8eP/5rvry7uu3T59uv/z3H3cfH/7ztx+8Hy7/",
  "obn/5ddH8x9+/Omvv9/+ctfePZ5+r77Iv/34/Sof7j/dff56//D53Ze7f//th797f+mjODI9zkh3f/efr1d/v/v668N/1l/uPxTy",
  "0TIS94d3ZnT/fHj4zTRvP5j/ZD7t8927/21//3gvnx/88O6/z3+mP7x7fPi9uPv34893Hz/+7Yf32Q/vbv/1eP8/d5X0+NsP/3x4",
  "fHz4dNYto3i8fZT/9u8vD//f3eezoruPdwKL1t/PtFzqCQVtT1cynzTe+vw5TwKeBP3dzN7/e56KH77PlBnb9d+XKVmdvzL5Cv55",
  "+/Xu54eP/f2Hx1/Pw/xw9+/bbx8fm4f/bO6ev4bzrP7r4ePX8z/f/eeJ9WR6/vXtq8h57iwKPt1/fvr/b//3+eu76hD6Ix385w6+",
  "6hCEIx2C5w6B6uCNdQifO4Rsh+i5Q8R2iJ87xGyH5LlDwnZInzukbIfsuUPGdvDcyzfn0l2+f9n62x7vcvm6Pf19j3e5fOEe/Y17",
  "l6/co79z7/Kle/S37l2+do/+3r3LF+/R37x3+eo9+rv3Ll++R3/7/uXb9+lv3798+z797fvfjZ3+9v3Lt++fv/0fnzzR2Y29v328",
  "/emvXx7+8+6L4eV65o+zLzz3F+91/9ncbNrHL9J6L/0ef6reN/949/f3m3/89cdHuZz5jz/+S/6fXOb7tfzv1/LP1/JHrtXff7z9",
  "7+2vfxle6tzzH2N9qi8P/3Mvt653Pz98fnz4dUJF8F1FcFYRjFwxf/hwhxRM9ypu5Z7yy+3nd6evt7/egv4/T/dv7j//6/72M+j4",
  "/rqjufd/b7kZbVmNtqxHWzajLdvRlt1oSz7aUoy2lKMt+9GWw2hLNdpSj7Y0oy3taMtxtOU02tKNtvSoZfA7Di+/4/fhGQ2/o//z",
  "k+/6/l9//J/r7w5CwRDaQygcQi2Eou/QQGT0XWR07hWN/Oy34Ad/M9MF9VnN9UGd1jOdOtBn89QnHulzfHi8/Qi6bV8/DbsF05Av",
  "mYZiwTSUy6Zh//ppOCyYhmrJNNQLpqFZNg3t66fhuGAaTkumoVswDf2rp2HgLLLvd+bsfJ1kYq2B7sxPvdKxXs3h/Sl/9/5Q3rTH",
  "bf6uuVlvD/u/F+/+0ZyOB3SnvlYxcM7vn1qyaxfoB37kuu7QV94AMPATG1whMAtdC1wDMApjG9zYYGY+1wK36KPj1I0jz8kS1xvS",
  "O0CHcZoGQeb4aZIN6RypDbIg8V0n9NxoSBeATlw3doX2PHXp0oY9103iNAk8J0kTde09UuJ5qe9FTpR46m55AHTspYmbRE7qpuq2",
  "WSE6CyI/SWQGQ0XXgE7jMJL/4wRREA/pBg1T5lsmJXBEj745IylxKOI9J07VtY9ovpMkNt9lGvrpkD4h3VGcZWHmRJGvrt3ZdOAG",
  "aZB5oRO7epQ9GKUXyM8kla8nTkcWF577sjlxn3Yn7ojx/x35i+dOo27m7svj7WdZjP/pXX736zf52/z5Qdb20nL/m/lX5DUGVx26",
  "jeem4aR4gfVjvUGgF/qer8AVAv3Ayyy3gcAwSDzLbQDQ81Pg27ZQpJhSdP49KKU7RAdu7Luxk8R6AnIoV1xBLL9MT/0wCwTHXhyl",
  "kRPEyrJLNL4gzMLE9xztM6DmIA4S8V5JqBzSAWpOUplmY3rqF18hOhLr80MnC0I1xBrRqRumqe+4ieUz0BhDL0sCGaOXKI8BhbhZ",
  "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",
  "uNFUS0ZTc6NpiNG03GiO3GhOS0bTcaPpp0ejjD68MvqZM0GwvOCl17LygsPe2rTtsw1olwwp+56MMLBLRtjsLhl0siybGMyOG0zO",
  "DaZYMpiSGMyeGsyBG0zFDaZeMpiGGExLDebIDebEDaZbMph+ejDKrKMrs5455APLBl56vVXZwOH1tKHbBxjgdhlhYLuMMLBdhpht",
  "2wCzbJvRv+P055z+gtNfEvr3nP4Dp7/i9Nec/obQ33L6j5z+E6e/4/T30/qV0cZXRjtzKgeW9rv0eqvSfsPraaO1jx2gHTOk7Lsz",
  "wsCOGRx1QDtmcDXLZgn5O05+zskvOPklIX9PyT9w8itOfs3Jbwj5LSX/yMk/cfI7Tn4/LV8ZbHJlsDPnaGDpvUuvty69N7yuNlz7",
  "dADcMSMM7JgRNrtjhp3sey/ALDtmRrPjRpMvGU3BjaYkRrPnRnPgRlMtGU3NjaYhRtNyozlyozktGU3HjaafHo0y+qtssmjmqAys",
  "rXfp9Ra19YbX0oYOk3YsO4eUfYeGWTr2HRphs/tnIomMGcyOG0zODaZYMpiSGMyeGsyBG0zFDaZeMpiGGExLDebIDebEDaZbMph+",
  "ejDKyK8yyOTvSSOHhfMuvd6mcN7watrMM273jDCwe0YY2D1DzLZsgFmWzejfcfpzTn/B6S8J/XtO/4HTX3H6a05/Q+hvOf1HTv+J",
  "099x+vtp/UOTja8SwOKZBDBY3O7Sa1lxu2FvZaIxzJ3RFoop604MMXuvDDC0V0ZX0xbKyN9x8nNOfsHJLwn5e0r+gZNfcfJrTn5D",
  "yG8p+UdO/omT33Hy+2n5yjyv8rbimbytkeJzl35/oPjc8BLaSlECjL0xhpi9MYbY3MYYd7JuqwizjJYZzY4bTb5kNAU3mpIYzZ4b",
  "zYEbTbVkNDU3moYYTcuN5siN5rRkNB03mn56NMrCr7K94plsL1hY7tLrTQrLDS+mLR3l2Fg7Y0zZ92OY12Pfj2Fez8zOGHWyDJ0Y",
  "zI4bTM4NplgymJIYzJ4azIEbTMUNpl4ymIYYTEsN5sgN5sQNplsymH56MMrKr7LE4pksMVg67tLrD5SOG15C2zbMv7Hv4jCVxr6L",
  "I8zeDmPMNmciBYzSv+P055z+gtNfEvr3nP4Dp7/i9Nec/obQ33L6j5z+E6e/4/T30/qVnV4ldsUziV2wxtul19vWeBteVZsuTJ6x",
  "LBdS9m0ZptjYt2VQQBdtk4kUL0b+jpOfc/ILTn5JyN9T8g+c/IqTX3PyG0J+S8k/cvJPnPyOk99Py1dme5W4Fc8kbk0UYrv0XVqI",
  "bdhfmynKfAH7ZJiVY99hYVbO3D4ZdrLvt0TyFjWaHTeafMloCm40JTGaPTeaAzeaasloam40DTGalhvNkRvNacloOm40/fRonkz8",
  "x6+/3t09vpcb5E9/lb3rL3c/3338+PXdvx6+fX40lUWu/uu7L3f/Fg/g/aX3fvjR/u9BGv+ll3+gtiT9S5+kqMUP/L/08g/UFsbB",
  "X3r5B2qLQvcvvSkHB9q8KBKNUQRVupmodDPT9uPLgH/66++3v9yVt19+uf/89d3Hu3/L4M0C64d3X+5/+fX7vzw+/C6u74d3/3x4",
  "fHz4dP7z17vbD3dfDCDt/354eLz8i/mA/zx8+e08wT/9/1BLAwQUAAAACADjG9FcwEbP7EpOAABlgAIAGAAAAHhsL3dvcmtzaGVl",
  "dHMvc2hlZXQyLnhtbK292ZLkxrGt/SptvJZAzANNkhnFrpwAZGLIBOxcltQlso964Olha+t/+t8jq7ML8FgAVoFFs83dYnyBXhEJ",
  "d0Q4PBx/+c/HT//+/NvDw5dX//v+3YfPf/3hty9ffv/pxx8///O3h/f3n52Pvz98kJZ/ffz0/v6L/M9Pv/74+fdPD/dvrp3ev/vR",
  "d934x/f3bz/88Le/XP9b9elvf/n49cu7tx8eqk+vPn99//7+03///vDu43/++oP3w+0/NG9//e2L+Q8//u0vv9//+tA+fLn8Xn2S",
  "//Xj96u8efv+4cPntx8/vPr08K+//vCz91MfxZHpcUW6tw//+Tz486vPv338z/bT2zeF/NUyEveHV2Z0//j48d+mef/G/Cfzt314",
  "ePW/7e/v3srfH/zw6r/f/pj+8OrLx9+Lh399+eXh3bu//vA6++HV/T+/vP2fh0p6/PWHf3z88uXj+6tuGcWX+y/y3/716eP/9/Dh",
  "qujh3YPAovX3Ky2XekRB2+OVzN803frt73kU8CjoZzN7/+/bVPzwfabM2IZ/vk3J5vqTyU/wj/vPD798fNe/ffPlt+sw3zz86/7r",
  "uy/Nx//sHr79DNdZ/efHd5+v/371n0fWk+n559fPIudbZ1Hw/u2Hx/9//7/ffr5Bh9Cf6OB/6+CrDkE40SH41iFQHbypDuG3DiHb",
  "IfrWIWI7xN86xGyH5FuHhO2QfuuQsh2ybx0ytoPn3n45l+7y/cfWv/Z0l9vP7enfe7rL7Qf36F/cu/3kHv2be7cf3aN/de/2s3v0",
  "7+7dfniP/uW920/v0b+9d/vxPfrX92+/vk//+v7t1/fpX9//buz0r+/ffn3/+uv/+OiJrm7s9f2X+7/95dPH/7z6ZHi5nvnD1Rde",
  "+4v3evvBPGzaL5+k9a30+/K36nXz91c/v979/S8/fpHLmf/44z/l/+Qy36/lf7+Wf72WP3Gt/u27+//e//bT+FLXnn+f6pPf/+Pr",
  "7/KE+PDq509fP7z99/2MjuC7juCqI5i65sc3D0jDfK/iXp4qv95/eHX5fP/bPej/y3z/5u2Hf769/wA6vh52NE//7y13ky2byZbt",
  "ZMtusmU/2XKYbMknW4rJlnKy5TjZcppsqSZb6smWZrKlnWw5T7ZcJlu6yZYetYzu4/B2H78Or2j4Hf2fv/mu7//lx/8Z/nYQCsbQ",
  "EULhGGohFH2HRiKj7yKja69o4rbfgxv+bqEL6rNZ6oM6bRc6daDP7rFPPNHn/PHL/TvQbf/8aTismIZ8zTQUK6ahXDcNx+dPw2nF",
  "NFRrpqFeMQ3Numlonz8N5xXTcFkzDd2KaeifPQ0jZ5F9fzJn1+skM6sN9GR+7JVO9WpOry/5q9en8q497/NXzd12fzr+XLz6e3M5",
  "n9CTeqhi5JxfP7ZkAxfoiZ9Ms9gb+8o7BMZp5KbJGNwgMMvcIIkc5cm3APXiNPWS2EnH6M5GwyD2/Nh11dMB/fVp4vl+5Jj/N6YP",
  "SEEQBGngOb6fRGM6h3qjJI5CJ0tiJbkAtB9kSeoHThRqugQDjGV4URA5sa+fgUekJApkpIETeJHSfUJKXF+mOXESz8vGdAV1x5Ef",
  "xk6SZuoZWgNaFLhB6Dky0nhMN2CUmdxGYZA5cejq5zNS4gduksmvk6VqTs6ITiLPj1wnsp79F6hbpi7xnCyM1HKiQ3QWpoEbO3Gi",
  "hPQ2HPluGGW+64RekuH1hec+7VDcxy2KO2H/PyOX8a3TpKd5+PTl/oOsx//0Kn/47av82fzxjSzvpUW2GR/gSv2X0VXHnuNb03hO",
  "0iB2MycJ1B14h+AwFEcj91So4A2E0yBxXUduQ+VDEBwliTgcJ9VGtgNwlKZJ4ipvt4cSoij1YyfItA0coAYvScSPyc+unAiEY88T",
  "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",
  "eYO/+zEy/tySWkv9lktqLV1hpqTWuOvIc93NtG1m2rYzbbuZtv1M22GmLZ9pK2baypm240zbaaatmmmrZ9qambZ2pu0803aZaetm",
  "2nrcpu777692Xps/G3yhpNYEpktqTWC6pNYENlFSK/CCgdzHmNuzSmotdoIltZZ7wZJai91gSa1br2eX1FozIYdVE5Kvm5Bi1YSU",
  "ayfkuGZCTqsmpFo3IfWqCWnWTki7ZkLOqybksm5CulUT0q+YEOVWnrYf5s9zW4ipklq3fi9XUmusZLwZubUNF2zmc9pqM8JhGw7b",
  "ctiOw/YYsz7CwWE5hxUcVnLYEWOZ/qIOh1UcVnNYw2EtwiIr+H7msAuHdRzWL2LKiNOBES+cvYDltW69Xri81viy2o7tNPXA+kAW",
  "RW0oaktRO4raIyq0AkEHDss5rOCwksOOGNP1+CmqoqiaohqKajFlhf847MJhHYf1i5iy3kFYwcvmrXfiAZwtWy9fYGt8OW21GRia",
  "/tzDHYdtOGzLYTsO22PMqsLPYTmHFRxWctgRY/qjiicOqzis5rCGw1qERY7nW/bLYBcO6zisX8TG9usPgnf+wjkMWGzr1usPFtsa",
  "X0bZrW+nqaNVM4VtOGzLYTsO22PMXjVTWM5hBYeVHHbEmL1qprCKw2oOazisRRhaNVPYhcM6DusXMWW3g8C3v3AcAxbeuvV6kcJb",
  "44tp67Uz1kMrF++OwzYctuWwHYftMZbElvUyWM5hBYeVHHZEWGSnkXJYxWE1hzUc1mIs1B+Z5LALh3Uc1i9iynr9gfUunMyARbhu",
  "vV66CNf4utqQ7bx2eU7oT05y2IbDthy247A9xjL9Mp3Dcg4rOKzksCPC5DmRWobMYBWH1RzWcFiLsVi/UuewC4d1HNYvYsqQg4Eh",
  "L5zfgAW5br3WFeQa99bmaie3y4NB56hx2IbDthy247A9wiL9rckDReUUVVBUSVFHTIU6SY3DKg6rOazhsBZj+kuRFHWhqI6i+iVK",
  "GWk4MNKFoxuwJNet10uV5BpfT5utneEOQssMtaGoLUXtKGqPKBRaprCcwwoOKznsiDErtMxQFUXVFNVQVIspO7RMYRcO6zisX8SU",
  "2UYDs104vwGLct16vVRRrvH1tNnaCe8otkxhGw7bctiOw/YYs2PLFJZzWMFhJYcdMWbHlims4rCawxoOaxGGYssUduGwjsP6RUwZ",
  "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",
  "Vd68ff/w4fPbjx9efXr4919++FvwU5+kie1xQbq3D//5fPPnV59//fif9ae3b0r5q2Uk/g+v7Oj++fHjb7Z5+8b+J/u3fXh49b/t",
  "7+/eyt8f/fDqv9/+mP/w6vHj7+XDvx//8fDu3V9+eG1+eHX/r8e3//NwlB5/+eGfHx8fP76/6JZRPN4/yn/796eP/9/Dh4uih3cP",
  "AovW3y+0XOorCtq+Xsn+TeOt3/6erwK+Cvqbnb3/920qfvg+U3Zst3++Tsnq8pPJT/DP+88P//j4rn/75vHXyzDfPPz7/su7x+bj",
  "fzYP336Gy6z+6+O7z5f/ffWfr2wg0/OvL59FzrfOouD92w9f///7//328910iMORDuG3DqHqEMUjHaJvHSLVIRjrEH/rELMdkm8d",
  "ErZD+q1DynbIvnXI2A75tw4528F862DYDoF//eV8usv3H1v/2uNdrj93oH/v8S7XHzygf/Hg+pMH9G8eXH/0gP7Vg+vPHtC/e3D9",
  "4QP6lw+uP31A//bB9ccP6F8/vP76If3rh9dfP6R//fC7sdO/fnj99cPLr//jV090cWOv7x/v//rzp4//efXJ8nI9+4eLL7z0F+/1",
  "9oN92LSPn6T1rfR7/OvxdfP3V397vfn7zz8+yuXsf/zxX/J/cpnv1wq/Xyu8XCscuVb/9t39f+9//Wl4qUvPv4/1KT4+3r9q799/",
  "efPpfkJC9F1CdJEQjV7uzQP666d7lffyQPnl/sOr8+f7X+9B/39M92/efvjX2/sPoOPr2472wf+95W60ZTXash5t2Yy2bEdbdqMt",
  "xWhLOdpSjbbsR1sOoy3H0ZZ6tKUZbWlHW06jLefRlm60pUctg/s4vt7Hr+MLGn9H/+evoR+GP//4P7e/HYSiIbSHUDyEWggl36GB",
  "yOS7yOTSKxm57bfghr+b6YL6rOb6oE7rmU4d6LP52icd6XMSX/QOdNs+fxp2C6ahWDIN5YJpqJZNw/7503BYMA3HJdNQL5iGZtk0",
  "tM+fhtOCaTgvmYZuwTT0z56GgbMw35/M5nKdbGKhgZ7MX3vlY72aw+tz8er1obprT9viVXO33h72fytf/b05nw7oSX2rYuCcX39t",
  "MTcuMAh9E6Rp6iVDb3mH0NRPQz/R6AqhJk2jPPTyIboGaJTlJo0TTzn+jYsmQZxExvfVEwL99bnVarwoT9WjYocURFmQJ8ZLMgUX",
  "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"
)

write_embedded_xlsx <- function(destination) {
  template_path <- file.path("www", "contoh_template_pdrb.xlsx")
  if (file.exists(template_path)) {
    ok <- file.copy(template_path, destination, overwrite = TRUE)
    if (isTRUE(ok) && file.exists(destination) && file.info(destination)$size > 0) {
      return(invisible(destination))
    }
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(
      "Paket 'jsonlite' diperlukan untuk menyiapkan unduhan XLSX.",
      call. = FALSE
    )
  }
  
  encoded_workbook <- paste(as.character(SYNTHETIC_XLSX_BASE64), collapse = "")
  workbook_raw <- jsonlite::base64_dec(encoded_workbook)
  
  connection <- base::file(destination, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(workbook_raw, connection)
  close(connection)
  on.exit(NULL, add = FALSE)
  
  file_size <- file.info(destination)$size
  if (!file.exists(destination) || is.na(file_size) || file_size <= 0) {
    stop("File XLSX contoh gagal dibuat dari data sintetis di server.R.", call. = FALSE)
  }
  
  signature_connection <- base::file(destination, open = "rb")
  on.exit(close(signature_connection), add = TRUE)
  signature <- readBin(signature_connection, what = "raw", n = 4)
  close(signature_connection)
  on.exit(NULL, add = FALSE)
  
  expected_signature <- as.raw(c(0x50, 0x4B, 0x03, 0x04))
  if (length(signature) < 4 || !identical(signature, expected_signature)) {
    stop("Hasil unduhan bukan workbook XLSX yang valid.", call. = FALSE)
  }
  
  invisible(destination)
}

make_synthetic_tidy_data <- function() {
  regions <- tibble::tribble(
    ~kode_kelompok, ~kelompok, ~kode_wilayah, ~wilayah, ~jenis_wilayah, ~region_factor,
    "9900", "Provinsi Contoh", "9900", "Provinsi Contoh", "Provinsi/Agregat", 1.00,
    "9900", "Provinsi Contoh", "9901", "Kabupaten Arunika", "Kabupaten/Kota", 0.46,
    "9900", "Provinsi Contoh", "9971", "Kota Samudra", "Kabupaten/Kota", 0.39
  )
  
  categories <- tibble::tribble(
    ~kode_kategori, ~kategori, ~category_factor,
    "A", "Pertanian, Kehutanan, dan Perikanan", 0.18,
    "C", "Industri Pengolahan", 0.27,
    "F", "Konstruksi", 0.12,
    "G", "Perdagangan Besar dan Eceran", 0.16,
    "J", "Informasi dan Komunikasi", 0.09,
    "O", "Administrasi Pemerintahan", 0.08,
    "RSTU", "Jasa Lainnya", 0.10
  )
  
  periods <- tibble::tribble(
    ~periode, ~quarter_factor,
    "I", 0.235,
    "II", 0.245,
    "III", 0.252,
    "IV", 0.268
  )
  
  tidyr::crossing(
    regions,
    categories,
    tahun = 2022:2024,
    periods,
    indikator = c("PDRB ADHB", "PDRB ADHK")
  ) %>%
    mutate(
      satuan = "Juta Rupiah",
      level = "Kategori Utama",
      year_factor = 1 + 0.065 * (tahun - 2022),
      price_factor = if_else(indikator == "PDRB ADHB", 1, 0.79),
      nilai = round(
        50000000 * region_factor * category_factor * quarter_factor *
          year_factor * price_factor,
        2
      )
    ) %>%
    select(
      kode_kelompok, kelompok, kode_wilayah, wilayah, jenis_wilayah,
      indikator, satuan, level, kode_kategori, kategori,
      tahun, periode, nilai
    ) %>%
    arrange(kode_wilayah, indikator, kode_kategori, tahun, periode)
}

