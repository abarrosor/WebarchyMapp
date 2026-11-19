# F5 · Barcelona City Police (Guardia Urbana): incidents handled ---------------------
# Open Data BCN (CKAN), dataset incidents-gestionats-gub, CC BY 4.0.
# One CSV per year with one row per incident type x neighbourhood x month:
#   Codi_Incident, Descripcio_Incident, Codi_districte, Nom_districte,
#   Codi_barri, Nom_barri, NK_Any, Mes_any, Nom_mes, Numero_incidents_GUB
#
# What it measures: incidents handled by the Barcelona local police according to
# its internal coding. It is the closest thing to the Madrid police calls (F1), but the
# categories are NOT equivalent: figures are never compared between cities. Crimes
# in Barcelona (thefts, robberies...) are recorded by the Mossos d'Esquadra (F4).
#
# THE FILES OF 2024 AND 2025 WERE CUT BY THE PUBLISHER, AND ARE COMPLETE SINCE 19 NOVEMBER 2026 (decisions P-056 and
# P-076). As found on 16 November 2026, each held exactly 30,000 lines (29,999 rows), sorted by incident code,
# district, neighbourhood and month, and ended in the middle of code 510 (fights): 2024 after district 09, 2025
# after district 03. The 27 codes after 510 (offences against property, assaults, drugs, domestic violence,
# police patrols...: 41 % of the incidents of 2023) were missing, and until the cut was seen the module said that
# those codes "are no longer published since 2024" and showed the fights of 2025 as zero in districts 04 to 10:
# both were wrong. Open Data BCN was told, answered that its extraction tool limited the files to 30,000 lines,
# and published them again complete (40,005 and 40,717 lines).
# The rule stays, for the next time (bcn_incidents_cut): a file whose line count is a round thousand and whose
# rows come grouped by code is cut; its last code is not shown for that year (the rows stay in stg_bcn_incidents)
# and the app is told which types that year lacks (stg_city_gaps), so that missing is never read as zero. With
# complete files the rule does not apply.
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

# The reading rules: a change here makes every yearly file be read again once (the load log keeps the hash of
# the file together with this mark)
BCN_INCIDENTS_RULES <- "r2"

# Is a yearly file cut by the publisher? `code`: the incident codes in the order of the file; `n_rows`: its rows.
# Cut = the lines (rows + header) are a round thousand AND each code comes in one block of rows (a file written
# code by code and stopped at a limit). Returns the last code, the one left incomplete, or NA. A complete file
# that happens to fit both (one chance in a thousand, and only if it is grouped) would lose its last code for that
# year, which the load says in a line.
bcn_incidents_cut <- function(code, n_rows = length(code)) {
  code <- code[!is.na(code)]
  if (length(code) < 2 || (n_rows + 1) %% 1000 != 0) return(NA_character_)
  if (length(rle(code)$values) != length(unique(code))) return(NA_character_)
  code[length(code)]
}

# Columns by name (with variants): the portal changed case and types over the years. The result carries two
# attributes when the file is cut: cut_code (its last, incomplete code) and cut_sorted (whether the codes come
# in ascending order, so that the codes after the last one are the ones the file lacks).
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
  out <- out[ok, ]
  cut <- bcn_incidents_cut(code[ok], nrow(df))
  if (!is.na(cut)) { attr(out, "cut_code") <- cut; attr(out, "cut_sorted") <- identical(code[ok], sort(code[ok], method = "radix")); attr(out, "cut_lines") <- nrow(df) + 1L }
  out
}

bcn_incidents_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_bcn_incidents (
      month DATE, district_code VARCHAR, neighbourhood_code VARCHAR, code VARCHAR, description VARCHAR,
      incidents INTEGER, file VARCHAR)")
  # Years whose file is cut by the publisher: its last code (incomplete) and whether the codes after it are the missing ones
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_bcn_incidents_cut (year INTEGER, last_code VARCHAR, sorted BOOLEAN, lines INTEGER, file VARCHAR)")
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
      ON b.level = 'neighbourhood' AND b.territory_id LIKE '", ID_BARCELONA, "-%' AND b.official_code = s.neighbourhood_code
    LEFT JOIN stg_bcn_incidents_cut k ON k.year = year(s.month)
    WHERE k.last_code IS NULL OR s.code <> k.last_code"))     # the incomplete last code of a cut file is not shown
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
    hash <- paste0(sha256_file(f[i]), ":", BCN_INCIDENTS_RULES)                # the file as it is, read with the rules of today
    if (hash %in% done) { message("= ", basename(f[i]), " (already loaded)"); next }
    d <- bcn_incidents_read(f[i])
    cut <- attr(d, "cut_code"); cut_row <- if (is.null(cut)) NULL else
      data.frame(year = as.integer(year[i]), last_code = cut, sorted = isTRUE(attr(d, "cut_sorted")), lines = as.integer(attr(d, "cut_lines")), file = basename(f[i]), stringsAsFactors = FALSE)
    bad <- sum(is.na(d$month) | is.na(d$incidents))
    d <- d[!is.na(d$month) & !is.na(d$incidents), ]
    d$file <- basename(f[i])
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM stg_bcn_incidents WHERE year(month) = ", as.integer(year[i])))
      DBI::dbAppendTable(con, "stg_bcn_incidents", d)
      DBI::dbExecute(con, paste0("DELETE FROM stg_bcn_incidents_cut WHERE year = ", as.integer(year[i])))
      if (!is.null(cut_row)) DBI::dbAppendTable(con, "stg_bcn_incidents_cut", cut_row)
      log_ingest(con, "bcn_incidents", basename(f[i]), hash, paste(format(range(d$month), "%Y-%m"), collapse = " to "),
                 nrow(d), sum(d$incidents))
    })
    message(sprintf("+ %s: %s incidents, %s%% without neighbourhood%s", basename(f[i]), fmt_int(sum(d$incidents)),
                    round(100 * sum(d$incidents[is.na(d$neighbourhood_code)]) / sum(d$incidents), 1),
                    if (bad > 0) paste0(" (", bad, " rows without month or figure, dropped)") else ""))
    if (!is.null(cut_row)) message(sprintf("  [!!] %s is cut by the publisher: %s lines exactly, written code by code and ending inside code %s. Code %s is not shown for %s (it reaches only some districts)%s",
                                           basename(f[i]), fmt_int(cut_row$lines), cut, cut, year[i], if (cut_row$sorted) " and the codes after it are missing from the file, not zero." else "; the codes that the file does not hold are missing, not zero."))
  }
  unmapped <- DBI::dbGetQuery(con, "SELECT DISTINCT code, description FROM v_bcn_incidents WHERE block = 'unmapped'")
  if (nrow(unmapped) > 0) message("Incident types without equivalence (reference/bcn_incident_types.csv): ",
                                  paste(unmapped$code, unmapped$description, collapse = " | "))
  invisible(TRUE)
}

# What the cut files lack, type by type, for the app (stg_city_gaps, R/60_cities.R). In a cut year the missing
# types are the incomplete last code and, if the file is in ascending order of code, the codes after it; if the
# order is another one, every known code the file does not hold (zero or cut: it cannot be told).
bcn_incidents_gaps <- function(con) {
  cut <- DBI::dbGetQuery(con, "SELECT year, last_code, sorted, file FROM stg_bcn_incidents_cut ORDER BY year")
  empty <- data.frame(event_type = character(), first_month = as.Date(character()), last_month = as.Date(character()), reason = character(), file = character(), stringsAsFactors = FALSE)
  if (nrow(cut) == 0) return(empty)
  known <- DBI::dbGetQuery(con, "SELECT code FROM ref_bcn_incident_types")$code
  rows <- lapply(seq_len(nrow(cut)), function(i) {
    held <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT code FROM stg_bcn_incidents WHERE year(month) = ", cut$year[i]))$code
    ord <- sort(unique(c(known, cut$last_code[i])), method = "radix")                    # the same order as the test of `sorted`
    lacking <- if (isTRUE(cut$sorted[i])) ord[seq_along(ord) > match(cut$last_code[i], ord)] else setdiff(known, held)
    lacking <- sort(unique(c(cut$last_code[i], lacking)), method = "radix")
    data.frame(event_type = lacking, first_month = as.Date(sprintf("%d-01-01", cut$year[i])), last_month = as.Date(sprintf("%d-12-01", cut$year[i])), reason = "cut_file", file = cut$file[i], stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Marts: same rules as the Madrid calls (month x neighbourhood x type, no
# sensitive types, only the public-safety and coexistence blocks)
