# Algoritma 15–16 — konfigurasi keamanan dan deployment
# File pengguna diproses melalui datapath sementara milik sesi Shiny.
# Aplikasi tidak menyalin file upload ke penyimpanan permanen.
PDRB_DEPLOYMENT <- list(
  version = "1.0.0-algoritma-1-17",
  max_upload_mb = 200,
  allowed_extensions = c("xls", "xlsx"),
  persistent_user_storage = FALSE,
  session_isolation = TRUE
)

pdrb_is_allowed_excel <- function(filename) {
  ext <- tolower(tools::file_ext(as.character(filename)))
  nzchar(ext) && ext %in% PDRB_DEPLOYMENT$allowed_extensions
}
