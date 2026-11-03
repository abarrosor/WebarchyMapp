# F4 · Mossos d'Esquadra: known and solved offences, arrests ------------------------
# Open-data portal of the Government of Catalonia (Socrata), dataset qnyt-emjc.
# One row per month x basic police area (ABP) x Criminal Code offence type, with
# KNOWN offences, solved offences and arrests. Monthly since 2011.
#
# What it measures: criminal offences known to the Catalan police. Same family of
# measure as the Ministry Crime Balance (F3) but with another classification and
# by ABP: never added to each other nor to calls (F1, F5).
#
# Territory: in Barcelona city every district is an ABP (reference/mossos_abp.csv).
# Outside Barcelona an ABP groups several municipalities: kept as is, never split.
# The "ABP Virtual" (online reports) is not attributed to any place.
#
# Download: one CSV per year (SoQL query "any = YYYY"). The current and previous
# year are requested again on every update (they get revised); if unchanged no
# copy is kept. Other years are downloaded once.

MOSSOS_COLUMNS <- c("mes", "any", "regi_policial_rp", "rea_b_sica_policial_abp",
                    "t_tol_codi_penal", "tipus_de_fet", "coneguts", "resolts", "detencions")

mossos_download <- function(from = CONFIG$sources$mossos$from, today = Sys.Date()) {
  src <- CONFIG$sources$mossos
  dir.create(src$dir_raw, recursive = TRUE, showWarnings = FALSE)
  this_year <- as.integer(format(today, "%Y"))
  for (year in from:this_year) {
    name <- sprintf("mossos_%d.csv", year)
    exists <- length(list.files(src$dir_raw, paste0("_", name, "$"))) > 0
    if (exists && year < this_year - 1) next
    tmp <- file.path(tempdir(), name)
    if (file.exists(tmp)) file.remove(tmp)
    ok <- tryCatch({ mossos_download_year(year, tmp); TRUE },
                   error = function(e) { message("x ", name, ": ", conditionMessage(e)); FALSE })
    if (ok) keep_version(tmp, src$dir_raw, name)
    if (file.exists(tmp)) file.remove(tmp)
    Sys.sleep(0.5)
  }
  invisible(TRUE)
}

# Download a whole year in pages of 50,000 rows
mossos_download_year <- function(year, dest, page = 50000L) {
  src <- CONFIG$sources$mossos
  lines <- character(0); offset <- 0L
  repeat {
    resp <- httr2::request(src$url) |>
      httr2::req_url_query(`$where` = paste0("any=", year), `$limit` = page, `$offset` = offset, `$order` = ":id") |>
      httr2::req_user_agent(USER_AGENT) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_timeout(120) |>
      httr2::req_perform()
    l <- strsplit(httr2::resp_body_string(resp, encoding = "UTF-8"), "\r?\n")[[1]]
    l <- l[nzchar(l)]
    if (length(l) <= 1) break
    lines <- c(lines, if (offset == 0) l else l[-1])
    if (length(l) - 1 < page) break
    offset <- offset + page
  }
  if (length(lines) <= 1) stop("no data for ", year)
  if (!grepl("coneguts", lines[1], fixed = TRUE)) stop("unexpected answer (not the Mossos CSV)")
  writeLines(lines, dest, useBytes = TRUE)
  invisible(dest)
}

mossos_read <- function(path) {
  df <- readr::read_csv(path, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = "UTF-8"),
                        trim_ws = TRUE, progress = FALSE)
  names(df) <- tolower(names(df))
  missing <- setdiff(MOSSOS_COLUMNS, names(df))
  if (length(missing) > 0) stop(basename(path), ": missing columns ", toString(missing), " (has the Mossos dataset changed?)")
  num <- function(x) suppressWarnings(as.integer(round(as.numeric(x))))
  data.frame(
    month      = as.Date(sprintf("%s-%02d-01", df$any, num(df$mes))),
    region     = trimws(gsub("\\s+", " ", df$regi_policial_rp)),
    abp        = trimws(gsub("\\s+", " ", df$rea_b_sica_policial_abp)),
    title      = trimws(df$t_tol_codi_penal),
    offence    = trimws(df$tipus_de_fet),
    offence_key = normalize_text(df$tipus_de_fet),
    known      = num(df$coneguts),
    solved     = num(df$resolts),
    arrests    = num(df$detencions),
    stringsAsFactors = FALSE)
}

mossos_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_mossos (
      month DATE, region VARCHAR, abp VARCHAR, title VARCHAR, offence VARCHAR, offence_key VARCHAR,
      known INTEGER, solved INTEGER, arrests INTEGER, file VARCHAR)")
  types <- read_reference("mossos_types.csv", bool_cols = "sensitive")
  types$offence_key <- normalize_text(types$source_text)
  if (anyDuplicated(types$offence_key)) stop("reference/mossos_types.csv has repeated types: ",
                                            toString(unique(types$source_text[duplicated(types$offence_key)])))
  groups <- read_reference("mossos_groups.csv"); groups$sort_order <- as.integer(groups$sort_order)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ref_abp_municipality (abp_code VARCHAR, ine_code VARCHAR, district_code VARCHAR, file VARCHAR)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS ref_abp (abp_code VARCHAR, name VARCHAR, region_code VARCHAR)")
  mossos_abp_map(con)
  DBI::dbWriteTable(con, "ref_mossos_types", as.data.frame(types[, c("offence_key", "group_id", "sensitive")]), overwrite = TRUE)
  DBI::dbWriteTable(con, "ref_mossos_groups", as.data.frame(groups), overwrite = TRUE)
  # Types without equivalence: group 'unmapped' and sensitive (never shown separately)
  DBI::dbExecute(con, "
    CREATE OR REPLACE VIEW v_mossos AS
    SELECT s.month, s.region, s.abp, a.territory_id, a.abp_code, s.title, s.offence,
           coalesce(t.group_id, 'unmapped') AS group_id, coalesce(t.sensitive, TRUE) AS sensitive,
           s.known, s.solved, s.arrests, s.file
    FROM stg_mossos s
    LEFT JOIN ref_mossos_abp_map a ON a.abp = s.abp
    LEFT JOIN ref_mossos_types t USING (offence_key)")
  invisible(TRUE)
}

# Tolerant key for ABP names: without "ABP", articles or punctuation
# ("ABP el Prat de Llobregat" = "Prat de Llobregat")
abp_key <- function(x) {
  x <- normalize_text(sub("^\\s*ABP\\s+", "", x, ignore.case = TRUE))
  x <- gsub("\\bL'", "", x)
  x <- gsub("[^A-Z0-9Ñ]+", " ", x)
  vapply(strsplit(trimws(x), " +"), function(p)
    paste(sort(unique(setdiff(p, c("EL", "LA", "LES", "ELS", "L", "DE", "D")))), collapse = " "), "")
}

# ABP name (as it comes in the data) -> ABP code and territory (Barcelona district).
# First reference/mossos_abp.csv; a name not found there is matched with the
# tolerant key in that file and in the official ABP list (ref_abp). Rebuilt on every load.
mossos_abp_map <- function(con) {
  ref <- read_reference("mossos_abp.csv")
  ref$territory_id[!is.na(ref$territory_id) & !nzchar(ref$territory_id)] <- NA
  ref$abp_code[!is.na(ref$abp_code) & !nzchar(ref$abp_code)] <- NA
  names_in_data <- if (DBI::dbExistsTable(con, "stg_mossos")) DBI::dbGetQuery(con, "SELECT DISTINCT abp FROM stg_mossos")$abp else character()
  official <- tryCatch(DBI::dbGetQuery(con, "SELECT abp_code, name FROM ref_abp"),
                       error = function(e) data.frame(abp_code = character(), name = character()))
  i1 <- match(normalize_text(names_in_data), normalize_text(ref$abp_name))
  i2 <- match(abp_key(names_in_data), abp_key(ref$abp_name))
  i <- ifelse(is.na(i1), i2, i1)
  code <- ref$abp_code[i]; terr <- ref$territory_id[i]
  missing <- is.na(i) & nrow(official) > 0
  if (any(missing)) code[missing] <- official$abp_code[match(abp_key(names_in_data[missing]), abp_key(official$name))]
  DBI::dbWriteTable(con, "ref_mossos_abp_map",
                    data.frame(abp = names_in_data, abp_code = code, territory_id = terr, stringsAsFactors = FALSE), overwrite = TRUE)
  invisible(TRUE)
}

# --- Basic police areas: municipalities of each one and their boundaries -------------
# Datasets of the Government of Catalonia: dtpq-6fvw (municipality -> ABP;
# Barcelona by district) and i2dd-kfpa (polygons of the 60 ABP). Checked every 90 days.
abp_download <- function(days = 90) {
  src <- CONFIG$sources$mossos
  previous <- newest_first(list.files(src$dir_abp, "abp_municip(ios|alities)\\.csv$", full.names = TRUE))
  if (length(previous) > 0 && difftime(Sys.time(), file.mtime(previous[1]), units = "days") < days) {
    message("= Police areas (ABP) downloaded less than ", days, " days ago")
    return(invisible(NULL))
  }
  download_versioned(src$abp_municipalities_url, src$dir_abp, name = "abp_municipalities.csv")
  download_versioned(src$abp_boundaries_url, src$dir_abp, name = "abp.geojson")
}

GPKG_ABP <- "data/territory_abp.gpkg"

abp_save <- function(con) {
  src <- CONFIG$sources$mossos
  f_mun <- newest_first(list.files(src$dir_abp, "abp_municip(ios|alities)\\.csv$", full.names = TRUE))
  f_geo <- newest_first(list.files(src$dir_abp, "abp\\.geojson$", full.names = TRUE))
  if (length(f_mun) == 0) { message("No municipality-to-ABP table (", src$dir_abp, ")"); return(invisible(FALSE)) }
  m <- readr::read_csv(f_mun[1], col_types = readr::cols(.default = "c"), progress = FALSE)
  names(m) <- tolower(names(m))
  missing <- setdiff(c("codi_abp_mossos", "codi_municipi"), names(m))
  if (length(missing) > 0) stop(basename(f_mun[1]), ": missing columns ", toString(missing))
  dis <- if ("codi_districte" %in% names(m)) suppressWarnings(as.integer(m$codi_districte)) else NA
  tab <- unique(data.frame(
    abp_code      = sprintf("%02d", as.integer(m$codi_abp_mossos)),
    ine_code      = substr(sprintf("%06d", as.integer(m$codi_municipi)), 1, 5),   # 6 digits = INE + check digit
    district_code = ifelse(is.na(dis) | dis < 1 | dis > 90, NA, sprintf("%02d", dis)),
    file          = basename(f_mun[1])))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM ref_abp_municipality")
    DBI::dbAppendTable(con, "ref_abp_municipality", tab)
  })
  if (length(f_geo) > 0) {
    g <- sf::st_read(f_geo[1], quiet = TRUE)
    names(g)[names(g) != attr(g, "sf_column")] <- tolower(names(g)[names(g) != attr(g, "sf_column")])
    g <- sf::st_make_valid(g)
    g <- sf::st_transform(sf::st_simplify(sf::st_transform(g, 25831), dTolerance = 40), 4326)
    abp <- sf::st_sf(territory_id = paste0("ES-09-ABP-", sprintf("%02d", as.integer(g$codi_abp_mossos))),
                     name = g$nom_abp_mossos, region_id = "ES-09", geometry = sf::st_geometry(g))
    if (file.exists(GPKG_ABP)) file.remove(GPKG_ABP)
    sf::st_write(abp, GPKG_ABP, layer = "abp", quiet = TRUE)
    DBI::dbWriteTable(con, "ref_abp", data.frame(abp_code = sprintf("%02d", as.integer(g$codi_abp_mossos)),
                                                 name = g$nom_abp_mossos, region_code = g$codi_rp_mossos %||% NA_character_),
                      overwrite = TRUE)
  }
  mossos_abp_map(con)
  message(sprintf("Police areas: %d ABP with %d municipalities%s", length(unique(tab$abp_code)), length(unique(tab$ine_code)),
                  if (length(f_geo) > 0) paste0("; boundaries in ", GPKG_ABP) else " (no boundaries)"))
  invisible(TRUE)
}

# Load the latest file of every year if it changed (replaces that year)
mossos_ingest <- function(con) {
  src <- CONFIG$sources$mossos
  f <- newest_first(list.files(src$dir_raw, "mossos_[0-9]{4}\\.csv$", full.names = TRUE))
  if (length(f) == 0) { message("No Mossos files in ", src$dir_raw); return(invisible(NULL)) }
  year <- sub("^.*mossos_([0-9]{4})\\.csv$", "\\1", basename(f))
  f <- f[!duplicated(year)]; year <- year[!duplicated(year)]    # the newest of each year
  done <- logged_hashes(con, "mossos")
  for (i in order(year)) {
    hash <- sha256_file(f[i])
    if (hash %in% done) { message("= ", basename(f[i]), " (already loaded)"); next }
    d <- mossos_read(f[i])
    d <- d[!is.na(d$month) & format(d$month, "%Y") == year[i], ]
    d$file <- basename(f[i])
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM stg_mossos WHERE year(month) = ", as.integer(year[i])))
      DBI::dbAppendTable(con, "stg_mossos", d)
      log_ingest(con, "mossos", basename(f[i]), hash, paste(format(range(d$month), "%Y-%m"), collapse = " to "),
                 nrow(d), sum(d$known, na.rm = TRUE))
    })
    message(sprintf("+ %s: %s rows, %s known offences (%s to %s)", basename(f[i]), fmt_int(nrow(d)),
                    fmt_int(sum(d$known, na.rm = TRUE)), format(min(d$month), "%m/%Y"), format(max(d$month), "%m/%Y")))
  }
  mossos_abp_map(con)
  unmapped <- DBI::dbGetQuery(con, "SELECT DISTINCT offence FROM v_mossos WHERE group_id = 'unmapped'")$offence
  if (length(unmapped) > 0) message("Offence types without equivalence (reference/mossos_types.csv): ", paste(unmapped, collapse = " | "))
  invisible(TRUE)
}

# Marts: Barcelona by district and month per offence group ('total' includes ALL
# offences, sensitive ones too); Catalonia by ABP and year; ABP population and info
mossos_marts <- function(con) {
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE mart_mossos_bcn_month AS
    SELECT month, territory_id, group_id, sum(known)::INTEGER AS known
    FROM v_mossos WHERE territory_id LIKE '", ID_BARCELONA, "%' AND NOT sensitive GROUP BY ALL
    UNION ALL
    SELECT month, territory_id, 'total', sum(known)::INTEGER
    FROM v_mossos WHERE territory_id LIKE '", ID_BARCELONA, "%' GROUP BY ALL
    ORDER BY ALL"))
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE mart_mossos_groups AS SELECT * FROM ref_mossos_groups ORDER BY sort_order")

  # Whole of Catalonia by ABP and year (January to the last month with data of that
  # year). 'ES-09' = Catalonia WITHOUT the online reports with no place (ABP Virtual),
  # so it can be compared with the ABPs. 'total' includes sensitive types too.
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_mossos_abp_year AS
    WITH b AS (SELECT abp_code, abp, year(month) AS year, group_id, sensitive, known FROM v_mossos),
    last AS (SELECT year(month) AS year, max(month(month)) AS to_month FROM stg_mossos GROUP BY 1),
    x AS (
      SELECT 'ES-09-ABP-' || abp_code AS territory_id, 'abp' AS level, year, group_id, sum(known) AS cumulative
        FROM b WHERE abp_code IS NOT NULL AND NOT sensitive GROUP BY ALL
      UNION ALL
      SELECT 'ES-09-ABP-' || abp_code, 'abp', year, 'total', sum(known) FROM b WHERE abp_code IS NOT NULL GROUP BY ALL
      UNION ALL
      SELECT 'ES-09', 'region', year, group_id, sum(known) FROM b WHERE upper(abp) <> 'ABP VIRTUAL' AND NOT sensitive GROUP BY ALL
      UNION ALL
      SELECT 'ES-09', 'region', year, 'total', sum(known) FROM b WHERE upper(abp) <> 'ABP VIRTUAL' GROUP BY ALL)
    SELECT x.territory_id, x.level, x.year, last.to_month, x.group_id, x.cumulative::INTEGER AS cumulative
    FROM x JOIN last USING (year) ORDER BY ALL")

  # Population of each ABP by year (INE). Barcelona districts share the municipal
  # population according to the most recent city register.
  if (DBI::dbExistsTable(con, "ref_population_municipality") && DBI::dbExistsTable(con, "mart_population") &&
      DBI::dbGetQuery(con, "SELECT count(*) AS n FROM ref_abp_municipality")$n > 0) {
    DBI::dbExecute(con, paste0("
      CREATE OR REPLACE TABLE mart_population_abp AS
      WITH m AS (SELECT abp_code, ine_code, district_code FROM ref_abp_municipality),
      ine AS (SELECT right(territory_id, 5) AS ine_code, year, population FROM ref_population_municipality WHERE territory_id LIKE 'ES-09-%'),
      dis AS (SELECT right(territory_id, 2) AS district_code, population * 1.0 / sum(population) OVER () AS share
              FROM mart_population WHERE level = 'district' AND territory_id LIKE '", ID_BARCELONA, "-D%'),
      p AS (
        SELECT m.abp_code, i.year, i.population * 1.0 AS pop FROM m JOIN ine i USING (ine_code) WHERE m.district_code IS NULL
        UNION ALL
        SELECT m.abp_code, i.year, i.population * d.share FROM m JOIN ine i USING (ine_code) JOIN dis d USING (district_code)
        WHERE m.district_code IS NOT NULL)
      SELECT 'ES-09-ABP-' || abp_code AS territory_id, year, round(sum(pop))::INTEGER AS population
      FROM p GROUP BY 1, 2 ORDER BY 1, 2"))
    # Name of each ABP and the municipalities it covers (most populated first)
    DBI::dbExecute(con, "
      CREATE OR REPLACE TABLE mart_abp_info AS
      WITH u AS (SELECT max(year) AS y FROM ref_population_municipality),
      mu AS (SELECT DISTINCT m.abp_code, m.ine_code, m.district_code, i.name, i.population
             FROM ref_abp_municipality m
             LEFT JOIN ref_population_municipality i ON right(i.territory_id, 5) = m.ine_code AND i.year = (SELECT y FROM u))
      SELECT 'ES-09-ABP-' || mu.abp_code AS territory_id, any_value(r.name) AS name,
             count(DISTINCT mu.ine_code) AS n_municipalities,
             string_agg(mu.name, ', ' ORDER BY mu.population DESC) AS municipalities,
             bool_or(mu.district_code IS NOT NULL) AS barcelona_district
      FROM mu LEFT JOIN ref_abp r USING (abp_code) GROUP BY mu.abp_code")
  }
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE mart_mossos_meta AS
    SELECT 'Known and solved criminal offences and arrests (Mossos d''Esquadra)' AS source,
           'Government of Catalonia - Department of the Interior and Public Safety' AS publisher,
           '", CONFIG$sources$mossos$page, "' AS url, 'See the terms of use of the portal' AS licence,
           (SELECT min(month) FROM mart_mossos_bcn_month) AS first_month,
           (SELECT max(month) FROM mart_mossos_bcn_month) AS last_month,
           (SELECT max(loaded_at) FROM ingest_log WHERE source = 'mossos') AS loaded_at,
           (SELECT sum(known) FROM v_mossos WHERE upper(abp) = 'ABP VIRTUAL'
              AND year(month) = (SELECT max(year(month)) FROM stg_mossos)) AS virtual_last_year"))
  invisible(TRUE)
}
