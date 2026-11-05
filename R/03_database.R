# DuckDB database ---------------------------------------------------------------
# A single file: data/webarchymapp.duckdb. Backup = copy the file.
#
# Layers: raw files (data/raw) -> staging tables (stg_*, one per source, as
# published) -> views (v_*, joined to the territory and reference tables) ->
# marts (mart_*, the only tables the app reads, with the privacy rules applied).

db_connect <- function(path = CONFIG$db_path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(duckdb::duckdb(dbdir = path))
}

# Read-only connection for the app. Recent duckdb versions reuse a database
# already open in the same R session and refuse to switch it to read-only:
# in that case a normal connection is used (the app only reads).
db_connect_readonly <- function(path = CONFIG$db_path) {
  con <- tryCatch(DBI::dbConnect(duckdb::duckdb(dbdir = path, read_only = TRUE)), error = function(e) NULL)
  if (is.null(con)) con <- DBI::dbConnect(duckdb::duckdb(dbdir = path))
  con
}

db_disconnect <- function(con) {
  drv <- tryCatch(con@driver, error = function(e) NULL)
  try(DBI::dbDisconnect(con), silent = TRUE)
  if (!is.null(drv)) try(duckdb::duckdb_shutdown(drv), silent = TRUE)
  invisible(TRUE)
}

# Open the database, run f(con) and always close it (even on error)
with_db <- function(f) {
  prepare_folders()
  con <- db_connect()
  on.exit(db_disconnect(con), add = TRUE)
  db_init(con)
  f(con)
}

# Create the shared tables and let every source create its own
db_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ingest_log (
      source VARCHAR, file VARCHAR, sha256 VARCHAR, periods VARCHAR, rows INTEGER,
      events INTEGER, header VARCHAR, action VARCHAR, loaded_at TIMESTAMP DEFAULT current_timestamp)")

  # Official territories (see R/territory.R). Geometries live in GeoPackages.
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS dim_territory (
      territory_id VARCHAR PRIMARY KEY, level VARCHAR, parent_id VARCHAR, country VARCHAR,
      official_code VARCHAR, code_scheme VARCHAR, name VARCHAR, name_norm VARCHAR,
      area_m2 DOUBLE, valid_from DATE, valid_to DATE, file VARCHAR, loaded_at TIMESTAMP)")

  # Residents by neighbourhood and reference date (Madrid and Barcelona registers)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_population (
      territory_id VARCHAR, ref_date DATE, population INTEGER, file VARCHAR,
      PRIMARY KEY (territory_id, ref_date))")

  for (f in c("madrid_calls_init", "madrid_activity_init", "crime_balance_init", "ine_population_init",
              "mossos_init", "bcn_incidents_init", "city_init", "country_init")) {
    if (exists(f, mode = "function")) do.call(f, list(con))
  }
  invisible(TRUE)
}

# Load a reference CSV (all columns as text), with "yes"/"si" booleans
read_reference <- function(name, bool_cols = character()) {
  df <- readr::read_csv(file.path(CONFIG$dir_ref, name), col_types = readr::cols(.default = "c"), progress = FALSE)
  for (b in intersect(bool_cols, names(df))) df[[b]] <- tolower(trimws(df[[b]])) %in% c("yes", "y", "si", "sí", "true", "1")
  df
}
