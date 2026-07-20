# Algoritma 14 — Quality Assurance dan pemeriksaan struktur aplikasi
pdrb_required_assets <- c(
  "www/logo_eksplorasi_pdrb.png",
  "www/tanda_lokasi_sidebar_tebal.png",
  "www/Template_PDRB_17_Lapangan_Usaha.xlsx",
  "www/Template_PDRB_Lengkap.xlsx"
)

pdrb_required_columns <- c(
  "kode_wilayah", "wilayah", "kode_kelompok", "kelompok",
  "jenis_wilayah", "indikator", "level_analisis", "kode_kategori",
  "nama_kategori", "tahun", "periode", "nilai"
)

pdrb_runtime_check <- function() {
  missing_assets <- pdrb_required_assets[!file.exists(pdrb_required_assets)]
  list(
    status = if (length(missing_assets) == 0) "Siap Digunakan" else "Perlu Revisi",
    missing_assets = missing_assets,
    checked_at = Sys.time()
  )
}

pdrb_validate_long_schema <- function(data) {
  if (!is.data.frame(data)) {
    return(list(valid = FALSE, missing = pdrb_required_columns, message = "Objek bukan data frame."))
  }
  missing <- setdiff(pdrb_required_columns, names(data))
  list(
    valid = length(missing) == 0,
    missing = missing,
    message = if (length(missing) == 0) "Struktur data long valid." else
      paste("Kolom belum tersedia:", paste(missing, collapse = ", "))
  )
}
