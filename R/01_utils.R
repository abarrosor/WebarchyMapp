# Small shared helpers ------------------------------------------------------------

PACKAGES <- c(
  "httr2",        # API calls and downloads
  "readr",        # CSV reading
  "dplyr",        # data transformation
  "digest",       # SHA-256 fingerprint of every downloaded file
  "DBI",          # common database interface
  "duckdb",       # single-file database
  "sf",           # geographic data: boundaries
  "readxl",       # Excel files (Madrid police activity, London population, Mexico, Colombia)
  "shiny",        # the app
  "bslib",        # app layout
  "leaflet",      # interactive map (OpenStreetMap tiles)
  "shinyWidgets", # period slider
  "jsonlite"      # JSON answers of statistical APIs (Eurostat, CBS, London Datastore...)
)
# Needed by one module only: missing, that module reports it and the rest of the update runs
PACKAGES_OPTIONAL <- c(readODS = "England and Wales crime tables (ODS format, R/74_gb.R)", testthat = "the automated tests (menu option 16, tests/run_tests.R)", writexl = "the button that downloads the figures of a page as an Excel file (without it the app has no such button), and the automated tests")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# A step of an update: a title line, and the place the run journal (R/05_journal.R) files the next lines under.
# `place` is a short stable id (a module's code) and `label` the name shown in the summary; both default to the title.
step <- function(title, place = NULL, label = NULL) {
  message("\n-- ", title, " ", strrep("-", max(3, 64 - nchar(title))))
  journal_place(place %||% title, label %||% title)
  invisible(NULL)
}

check_packages <- function() {
  missing <- setdiff(PACKAGES, rownames(utils::installed.packages()))
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         ".\nRun scripts/maintenance/install_packages.R (or option 5 in the WEBARCHYMAPP.R menu).",
         call. = FALSE)
  }
  invisible(TRUE)
}

install_packages <- function() {
  wanted <- c(PACKAGES, names(PACKAGES_OPTIONAL))
  missing <- setdiff(wanted, rownames(utils::installed.packages()))
  if (length(missing) > 0) utils::install.packages(missing) else message("All packages are already installed.")
  for (p in wanted) message(sprintf("%-13s %s", p, if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else "not installed"))
}

# Upper case, no accents, single spaces; empty-like values become NA.
# Independent of the system locale.
normalize_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- toupper(trimws(x))
  x <- chartr("ÁÉÍÓÚÀÈÌÒÙÄËÏÖÜÂÊÎÔÛ",
              "AEIOUAEIOUAEIOUAEIOU", x)
  x <- gsub("\\s+", " ", x)
  x[x %in% c("", "#NULL", "NULL", "DESCONOCIDO", "NA")] <- NA_character_
  x
}

sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

# "77.705" -> 77705 ; "-1,3" -> -1.3 ; ".." or empty -> NA (Spanish number format)
parse_es_number <- function(x) {
  x <- trimws(x)
  x[x %in% c("", ".", "..", "...", "-")] <- NA
  suppressWarnings(as.numeric(gsub(",", ".", gsub(".", "", x, fixed = TRUE), fixed = TRUE)))
}

fmt_int <- function(x) format(x %||% 0, big.mark = ",", scientific = FALSE, trim = TRUE)

# Files ordered from newest to oldest (by modification time)
newest_first <- function(f) f[order(file.mtime(f), decreasing = TRUE)]

# Most recent file of a source, or an error
latest_file <- function(dir, pattern) {
  f <- newest_first(list.files(dir, pattern = pattern, full.names = TRUE))
  if (length(f) == 0) stop("No file matching '", pattern, "' in ", dir)
  f[1]
}

# Append a row to the ingest log
log_ingest <- function(con, source, file, sha256, periods = NA_character_, rows = NA_integer_,
                       events = NA_integer_, header = NA_character_, action = "added") {
  DBI::dbAppendTable(con, "ingest_log", data.frame(
    source = source, file = file, sha256 = sha256, periods = periods, rows = rows,
    events = events, header = header, action = action, loaded_at = Sys.time()))
}

logged_hashes <- function(con, source) {
  DBI::dbGetQuery(con, paste0("SELECT sha256 FROM ingest_log WHERE source = '", source, "'"))$sha256
}

# Writes a quality report CSV and returns its path
write_quality_report <- function(source, df) {
  dir.create(CONFIG$dir_reports, showWarnings = FALSE, recursive = TRUE)
  path <- file.path(CONFIG$dir_reports, paste0("quality_", source, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  readr::write_csv(df, path)
  path
}
