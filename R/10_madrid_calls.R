# F1 · Calls received by the Madrid Municipal Police central dispatch ---------------
# Monthly CSV from datos.madrid.es. Structure (official PDF, April 2026):
# date, hour, district, neighbourhood, origin, number of events, call type.
# Columns are read BY POSITION because the name of column 7 changes between months.
#
# What it measures: calls received (092, 112, patrols, cameras), not confirmed
# crimes, and only by the Municipal Police (not the National Police 091).
#
# Privacy: the staging table keeps day and hour as published (personal
# analysis only). Anything shown in the app is aggregated to month x
# neighbourhood, without sensitive types, by R/50_marts.R.

MADRID_CALLS_COLUMNS <- c("date", "hour", "district_txt", "neighbourhood_txt", "origin", "events", "call_type")

madrid_calls_read <- function(path) {
  header <- readr::read_lines(path, n_max = 1, locale = readr::locale(encoding = "UTF-8"))
  header <- sub("^﻿", "", header)
  n_col <- length(strsplit(header, ";", fixed = TRUE)[[1]])
  if (n_col != length(MADRID_CALLS_COLUMNS)) {
    stop(basename(path), ": expected 7 columns, found ", n_col, ". Check whether the City Council changed the file structure.")
  }
  df <- readr::read_delim(path, delim = ";", skip = 1, col_names = MADRID_CALLS_COLUMNS,
                          col_types = readr::cols(.default = readr::col_character()),
                          locale = readr::locale(encoding = "UTF-8"), progress = FALSE)
  attr(df, "header") <- header
  df
}

# Clean and normalise one monthly file into the staging structure
madrid_calls_clean <- function(df, file, ref_districts, ref_types) {
  df |>
    dplyr::mutate(
      date         = as.Date(date),
      hour         = suppressWarnings(as.integer(hour)),
      district_txt = normalize_text(district_txt),
      neighbourhood = normalize_text(neighbourhood_txt),
      origin       = dplyr::case_when(
        trimws(origin) %in% c("92", "092") ~ "092",
        toupper(trimws(origin)) %in% c("#NULL", "SIN ORIG", "") ~ NA_character_,
        TRUE ~ toupper(trimws(origin))),
      events       = suppressWarnings(as.integer(events)),
      call_type    = toupper(trimws(call_type)),
      month        = as.Date(format(date, "%Y-%m-01")),
      file         = file) |>
    dplyr::left_join(ref_districts, by = c("district_txt" = "district_name")) |>
    dplyr::left_join(ref_types[, c("call_type", "block", "sensitive")], by = "call_type") |>
    dplyr::mutate(block = dplyr::coalesce(block, "unmapped"), sensitive = dplyr::coalesce(sensitive, FALSE)) |>
    dplyr::select(date, month, hour, district_code, district_txt, neighbourhood, origin, call_type, block, sensitive, events, file)
}

# Quality checks of one file; returns a table of findings
madrid_calls_validate <- function(stg) {
  chk <- function(check, n, detail = "") dplyr::tibble(check = check, rows_affected = n, detail = detail)
  unmapped <- unique(stg$call_type[stg$block == "unmapped"])
  unknown_districts <- unique(stg$district_txt[is.na(stg$district_code) & !is.na(stg$district_txt)])
  months <- unique(format(stg$month, "%Y-%m"))
  dplyr::bind_rows(
    chk("invalid date",              sum(is.na(stg$date))),
    chk("hour outside 0-23",         sum(is.na(stg$hour) | stg$hour < 0 | stg$hour > 23)),
    chk("non-numeric events",        sum(is.na(stg$events))),
    chk("empty district",            sum(is.na(stg$district_txt))),
    chk("unknown district",          sum(is.na(stg$district_code) & !is.na(stg$district_txt)), paste(unknown_districts, collapse = " | ")),
    chk("empty neighbourhood",       sum(is.na(stg$neighbourhood))),
    chk("unmapped call type",        sum(stg$block == "unmapped"), paste(unmapped, collapse = " | ")),
    chk("file spans several months", as.integer(length(months) > 1), paste(months, collapse = ", "))
  )
}

madrid_calls_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_madrid_calls (
      date DATE, month DATE, hour INTEGER, district_code VARCHAR, district_txt VARCHAR, neighbourhood VARCHAR,
      origin VARCHAR, call_type VARCHAR, block VARCHAR, sensitive BOOLEAN, events INTEGER, file VARCHAR)")

  # Call-type reference: reloaded from the CSV on EVERY run, so a change in
  # reference/madrid_call_types.csv also applies to months already loaded.
  types <- read_reference("madrid_call_types.csv", bool_cols = "sensitive")
  types$call_type <- toupper(trimws(types$call_type))
  if (anyDuplicated(types$call_type)) stop("reference/madrid_call_types.csv has duplicated call types: ",
                                           toString(unique(types$call_type[duplicated(types$call_type)])))
  for (col in c("name_es", "name_en")) if (!col %in% names(types)) types[[col]] <- NA_character_
  DBI::dbWriteTable(con, "ref_madrid_call_types", as.data.frame(types[, c("call_type", "block", "sensitive", "name_es", "name_en")]), overwrite = TRUE)

  # View joined to the official territory. The district is ALWAYS taken from the
  # official neighbourhood (about 3.5% of rows carry a district that does not
  # match their neighbourhood). Unmapped types are treated as sensitive, so
  # they are never shown by mistake.
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE VIEW v_madrid_calls AS
    SELECT s.date, s.month, s.hour, s.district_code, s.district_txt, s.neighbourhood, s.origin, s.call_type,
           coalesce(c.block, 'unmapped') AS block, coalesce(c.sensitive, TRUE) AS sensitive, s.events, s.file,
           b.territory_id AS neighbourhood_id,
           coalesce(b.parent_id, '", ID_MADRID, "-D' || s.district_code) AS district_id
    FROM stg_madrid_calls s
    LEFT JOIN ref_madrid_call_types c USING (call_type)
    LEFT JOIN dim_territory b ON b.level = 'neighbourhood' AND b.territory_id LIKE '", ID_MADRID, "-%' AND b.name_norm = s.neighbourhood"))
  invisible(TRUE)
}

madrid_calls_download <- function() {
  src <- CONFIG$sources$madrid_calls
  ckan_download_new(src$ckan_id, src$dir_raw, src$format)
}

# Incremental load: a file already loaded (same SHA-256) is skipped; a
# DIFFERENT file for a month already loaded replaces that month (and is logged).
madrid_calls_ingest <- function(con) {
  src <- CONFIG$sources$madrid_calls
  files <- list.files(src$dir_raw, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("No CSV files in ", src$dir_raw)
  ref_districts <- read_reference("madrid_districts.csv")
  ref_types <- read_reference("madrid_call_types.csv", bool_cols = "sensitive")
  ref_types$call_type <- toupper(trimws(ref_types$call_type))
  done <- logged_hashes(con, "madrid_calls")
  reports <- list()
  for (path in files) {
    name <- basename(path); hash <- sha256_file(path)
    if (hash %in% done) { message("= ", name, " (already loaded)"); next }
    raw <- madrid_calls_read(path)
    stg <- madrid_calls_clean(raw, name, ref_districts, ref_types)
    val <- madrid_calls_validate(stg)
    months <- sort(unique(format(stg$month, "%Y-%m")))
    month_filter <- paste0("strftime(month, '%Y-%m') IN (", paste(DBI::dbQuoteString(con, months), collapse = ", "), ")")
    DBI::dbWithTransaction(con, {
      previous <- DBI::dbGetQuery(con, paste("SELECT count(*) AS n FROM stg_madrid_calls WHERE", month_filter))$n
      if (previous > 0) DBI::dbExecute(con, paste("DELETE FROM stg_madrid_calls WHERE", month_filter))
      DBI::dbAppendTable(con, "stg_madrid_calls", as.data.frame(stg))
      log_ingest(con, "madrid_calls", name, hash, paste(months, collapse = ","), nrow(stg),
                 sum(stg$events, na.rm = TRUE), attr(raw, "header"), if (previous > 0) "replaced" else "added")
    })
    message(if (previous > 0) "~ " else "+ ", name, ": ", fmt_int(nrow(stg)), " rows, month ", paste(months, collapse = ","),
            if (previous > 0) " (REPLACES data previously loaded for that month)" else "")
    reports[[name]] <- dplyr::mutate(val, file = name, .before = 1)
  }
  report <- dplyr::bind_rows(reports)
  if (nrow(report) > 0) {
    path <- write_quality_report("madrid_calls", report)
    problems <- report[report$rows_affected > 0, ]
    message("Quality report: ", path)
    if (nrow(problems) > 0) {
      problems$file <- sub("-incidencias.*$", "", problems$file)
      print(as.data.frame(problems[, c("file", "check", "rows_affected")]), row.names = FALSE)
    }
  }
  invisible(report)
}

# --- Madrid municipal register: residents by neighbourhood --------------------------
# One row per census section x single age with 4 counts (Spanish/foreign x
# men/women). DATA MINIMISATION: only the TOTAL by neighbourhood and date is kept.
CENSUS_REQUIRED <- c("COD_DISTRITO", "COD_BARRIO", "COD_DIST_BARRIO",
                     "ESPANOLESHOMBRES", "ESPANOLESMUJERES", "EXTRANJEROSHOMBRES", "EXTRANJEROSMUJERES")

# Reference date from the file name (200076_YYYYMMDD_hhmmss.csv), moved to day 1
census_date_from_name <- function(path) {
  m <- regmatches(basename(path), regexpr("_(20[0-9]{6})_", basename(path)))
  if (length(m) == 0) stop("Cannot read the date from the file name: ", basename(path))
  as.Date(format(as.Date(gsub("_", "", m), "%Y%m%d"), "%Y-%m-01"))
}

madrid_census_read <- function(path) {
  header_raw <- readLines(path, n = 1, warn = FALSE)
  enc <- if (validUTF8(header_raw)) "UTF-8" else "latin1"
  header <- sub("^﻿", "", iconv(header_raw, enc, "UTF-8"))
  sep <- if (grepl(";", header, fixed = TRUE)) ";" else ","
  df <- readr::read_delim(path, delim = sep, col_types = readr::cols(.default = readr::col_character()),
                          locale = readr::locale(encoding = enc), trim_ws = TRUE, progress = FALSE)
  names(df) <- chartr("ÁÉÍÓÚÑ", "AEIOUN", toupper(trimws(sub("^﻿", "", names(df)))))
  missing <- setdiff(CENSUS_REQUIRED, names(df))
  if (length(missing) > 0) stop(basename(path), ": missing columns ", toString(missing), ". Check whether the City Council changed the structure.")
  num <- function(x) { x <- suppressWarnings(as.numeric(trimws(x))); x[is.na(x)] <- 0; x }
  df$people <- num(df$ESPANOLESHOMBRES) + num(df$ESPANOLESMUJERES) + num(df$EXTRANJEROSHOMBRES) + num(df$EXTRANJEROSMUJERES)
  dates <- if ("FX_DATOS_INI" %in% names(df)) unique(stats::na.omit(as.Date(substr(df$FX_DATOS_INI, 1, 10)))) else NULL
  attr(df, "ref_date") <- if (length(dates) == 1) dates else census_date_from_name(path)
  df
}

madrid_census_by_neighbourhood <- function(df) {
  dis <- sprintf("%02d", suppressWarnings(as.integer(df$COD_DISTRITO)))
  bar <- suppressWarnings(as.integer(df$COD_BARRIO))
  no_nb <- is.na(bar) | is.na(suppressWarnings(as.integer(df$COD_DISTRITO)))
  if (any(no_nb)) {
    message(sprintf("  (%s residents with no neighbourhood in the source: excluded)", fmt_int(sum(df$people[no_nb]))))
    df <- df[!no_nb, ]; dis <- dis[!no_nb]; bar <- bar[!no_nb]
  }
  code <- sprintf("%s%d", dis, bar)              # "01" + 1 -> "011"
  out <- stats::aggregate(df$people, by = list(district = dis, neighbourhood = code), FUN = sum)
  names(out)[3] <- "population"
  out$territory_id <- paste0(ID_MADRID, "-D", out$district, "-B", out$neighbourhood)
  out
}

madrid_census_download <- function() {
  src <- CONFIG$sources$madrid_census
  ckan_download_new(src$ckan_id, src$dir_raw, src$format)
}

madrid_census_ingest <- function(con) {
  src <- CONFIG$sources$madrid_census
  files <- list.files(src$dir_raw, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("No register files in ", src$dir_raw)
  ok_ids <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE '",
                                        ID_MADRID, "-%'"))$territory_id
  if (length(ok_ids) == 0) stop("Load the Madrid territory first")
  done <- DBI::dbGetQuery(con, "SELECT DISTINCT file FROM ref_population")$file
  # Two files with the same reference date (a correction): keep the newest by name
  dates <- vapply(files, function(f) tryCatch(as.character(census_date_from_name(f)), error = function(e) NA_character_), "")
  o <- order(basename(files)); files <- files[o]; dates <- dates[o]
  keep <- !duplicated(dates, fromLast = TRUE) | is.na(dates)
  for (f in files[!keep]) message(". ", basename(f), " (a newer file has the same reference date: skipped)")
  for (path in files[keep]) {
    if (basename(path) %in% done) { message("= ", basename(path), " (already loaded)"); next }
    raw <- madrid_census_read(path); ref_date <- attr(raw, "ref_date")
    pop <- madrid_census_by_neighbourhood(raw)
    unknown <- setdiff(pop$territory_id, ok_ids)
    if (length(unknown) > 0) warning("Register neighbourhoods not in dim_territory: ", toString(unknown))
    absent <- setdiff(ok_ids, pop$territory_id)
    if (length(absent) > 0) warning("Official neighbourhoods with no population: ", toString(absent))
    pop <- pop[pop$territory_id %in% ok_ids, ]
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE ref_date = DATE '", ref_date, "' AND territory_id LIKE '", ID_MADRID, "-%'"))
      DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = pop$territory_id, ref_date = ref_date,
                                                           population = as.integer(pop$population), file = basename(path)))
    })
    message(sprintf("+ %s: population at %s = %s residents in %d neighbourhoods", basename(path),
                    format(ref_date, "%Y-%m-%d"), fmt_int(sum(pop$population)), nrow(pop)))
  }
  invisible(TRUE)
}
