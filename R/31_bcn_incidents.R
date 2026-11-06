# F5 · Barcelona City Police (Guardia Urbana): incidents handled ---------------------
# Open Data BCN (CKAN), dataset incidents-gestionats-gub, CC BY 4.0.
# One CSV per year with one row per incident type x neighbourhood x month:
#   Codi_Incident, Descripcio_Incident, Codi_districte, Nom_districte,
#   Codi_barri, Nom_barri, NK_Any, Mes_any, Nom_mes, Numero_incidents_GUB
#
# What it measures: incidents handled by the Barcelona local police according to
# its internal coding. NOTE: until 2023 the file included offences against
# property, assaults, drugs, domestic violence...; since 2024 those codes (5xx
# except fights, 6xx, 7xx...) are no longer published. The public-safety series
# breaks in 2024. It is the closest thing to the Madrid police calls (F1), but the
# categories are NOT equivalent: figures are never compared between cities. Crimes
# in Barcelona (thefts, robberies...) are recorded by the Mossos d'Esquadra (F4).
#
# Published once a year (the previous year, around February) and sometimes an old
# year is re-published corrected: downloaded again if the portal reports a
# modification later than the saved copy (and kept only if it changed).

bcn_incidents_download <- function(from = CONFIG$sources$bcn_incidents$from) {
  src <- CONFIG$sources$bcn_incidents
  dir.create(src$dir_raw, recursive = TRUE, showWarnings = FALSE)
  res <- ckan_resources(src$ckan_id, base = CONFIG$ckan_barcelona)
  res$year <- suppressWarnings(as.integer(sub("^([0-9]{4})_.*$", "\\1", res$name)))
  res <- res[res$format == "csv" & !is.na(res$year) & res$year >= from, ]
  n <- 0
  for (i in order(res$year)) {
    name <- sprintf("bcn_incidents_%d.csv", res$year[i])
    previous <- newest_first(list.files(src$dir_raw, paste0("_(gub|bcn_incidents)_", res$year[i], "\\.csv$"), full.names = TRUE))
    modified <- suppressWarnings(as.POSIXct(substr(res$modified[i] %||% res$created[i], 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    if (length(previous) > 0 && (is.na(modified) || modified <= file.mtime(previous[1]))) next
    tmp <- file.path(tempdir(), name)
    if (file.exists(tmp)) file.remove(tmp)
    ok <- tryCatch({ download_safely(res$url[i], tmp, timeout = 600); TRUE },
                   error = function(e) { message("x ", name, ": ", conditionMessage(e)); FALSE })
    if (ok) { keep_version(tmp, src$dir_raw, name); n <- n + 1 }
    if (file.exists(tmp)) file.remove(tmp)
    Sys.sleep(1)
  }
  message(n, " year(s) of Barcelona incidents downloaded or checked.")
  invisible(n)
}

# Columns by name (with variants): the portal changed case and types over the years
bcn_incidents_read <- function(path) {
  df <- bcn_read_csv(path)
  col <- function(pattern) {
    x <- grep(pattern, names(df), value = TRUE)
    if (length(x) == 0) stop(basename(path), ": column '", pattern, "' not found (columns: ", toString(names(df)), ")")
    df[[x[1]]]
  }
  code <- toupper(trimws(col("^codi_incident")))
  code <- ifelse(grepl("^[0-9]+$", code), sprintf("%03d", suppressWarnings(as.integer(code))), code)
  int <- function(x) suppressWarnings(as.integer(trimws(x)))
  year <- int(col("^(nk_)?any$")); mon <- int(col("^mes"))
  dis <- int(col("^codi_districte")); bar <- int(col("^codi_barri"))
  ok <- grepl("^-?[0-9A-Z]{1,3}$", code)       # drops broken rows (e.g. a date in the code column)
  out <- data.frame(
    month         = as.Date(ifelse(is.na(year) | is.na(mon), NA, sprintf("%d-%02d-01", year, mon))),
    district_code = ifelse(is.na(dis) | dis < 1, NA, sprintf("%02d", dis)),
    neighbourhood_code = ifelse(is.na(bar) | bar < 1, NA, sprintf("%03d", bar)),
    code          = code,
    description   = trimws(col("^descripcio")),
    incidents     = int(col("^numero_incidents")),
    stringsAsFactors = FALSE)
  out[ok, ]
}

bcn_incidents_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_bcn_incidents (
      month DATE, district_code VARCHAR, neighbourhood_code VARCHAR, code VARCHAR, description VARCHAR,
      incidents INTEGER, file VARCHAR)")
  types <- read_reference("bcn_incident_types.csv", bool_cols = "sensitive")
  types$code <- toupper(trimws(types$code))
  if (anyDuplicated(types$code)) stop("reference/bcn_incident_types.csv has repeated codes: ", toString(unique(types$code[duplicated(types$code)])))
  DBI::dbWriteTable(con, "ref_bcn_incident_types", as.data.frame(types[, c("code", "block", "sensitive", "name_es", "name_en")]), overwrite = TRUE)
  # Neighbourhood and district with the common identifier; the district comes from the official neighbourhood
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE VIEW v_bcn_incidents AS
    SELECT s.month, s.code, s.description,
           coalesce(c.block, 'unmapped') AS block, coalesce(c.sensitive, TRUE) AS sensitive, s.incidents, s.file,
           b.territory_id AS neighbourhood_id,
           coalesce(b.parent_id, CASE WHEN s.district_code IS NOT NULL THEN '", ID_BARCELONA, "-D' || s.district_code END) AS district_id
    FROM stg_bcn_incidents s
    LEFT JOIN ref_bcn_incident_types c USING (code)
    LEFT JOIN dim_territory b
      ON b.level = 'neighbourhood' AND b.territory_id LIKE '", ID_BARCELONA, "-%' AND b.official_code = s.neighbourhood_code"))
  invisible(TRUE)
}

bcn_incidents_ingest <- function(con) {
  src <- CONFIG$sources$bcn_incidents
  f <- newest_first(list.files(src$dir_raw, "(gub|bcn_incidents)_[0-9]{4}\\.csv$", full.names = TRUE))
  if (length(f) == 0) { message("No Barcelona incident files in ", src$dir_raw); return(invisible(NULL)) }
  year <- sub("^.*_([0-9]{4})\\.csv$", "\\1", basename(f))
  f <- f[!duplicated(year)]; year <- year[!duplicated(year)]
  done <- logged_hashes(con, "bcn_incidents")
  for (i in order(year)) {
    hash <- sha256_file(f[i])
    if (hash %in% done) { message("= ", basename(f[i]), " (already loaded)"); next }
    d <- bcn_incidents_read(f[i])
    bad <- sum(is.na(d$month) | is.na(d$incidents))
    d <- d[!is.na(d$month) & !is.na(d$incidents), ]
    d$file <- basename(f[i])
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM stg_bcn_incidents WHERE year(month) = ", as.integer(year[i])))
      DBI::dbAppendTable(con, "stg_bcn_incidents", d)
      log_ingest(con, "bcn_incidents", basename(f[i]), hash, paste(format(range(d$month), "%Y-%m"), collapse = " to "),
                 nrow(d), sum(d$incidents))
    })
    message(sprintf("+ %s: %s incidents, %s%% without neighbourhood%s", basename(f[i]), fmt_int(sum(d$incidents)),
                    round(100 * sum(d$incidents[is.na(d$neighbourhood_code)]) / sum(d$incidents), 1),
                    if (bad > 0) paste0(" (", bad, " rows without month or figure, dropped)") else ""))
  }
  unmapped <- DBI::dbGetQuery(con, "SELECT DISTINCT code, description FROM v_bcn_incidents WHERE block = 'unmapped'")
  if (nrow(unmapped) > 0) message("Incident types without equivalence (reference/bcn_incident_types.csv): ",
                                  paste(unmapped$code, unmapped$description, collapse = " | "))
  invisible(TRUE)
}

# Marts: same rules as the Madrid calls (month x neighbourhood x type, no
# sensitive types, only the public-safety and coexistence blocks)
