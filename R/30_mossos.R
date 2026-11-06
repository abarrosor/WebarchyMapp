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
  # Offence types and their 13 groups from the shared file of the Mossos source (R/72_es_mossos.R):
  # source_code = normalised offence, offence_type_id = group, sort_order = order of the group in the app
  tf <- read_reference("es_mossos_offence_types.csv", bool_cols = "sensitive"); tf <- tf[tf$offence_type_id != "total", ]
  types <- data.frame(offence_key = tf$source_code, group_id = tf$offence_type_id, sensitive = tf$sensitive, stringsAsFactors = FALSE)
  if (anyDuplicated(types$offence_key)) stop("reference/es_mossos_offence_types.csv has repeated types: ", toString(unique(tf$source_text[duplicated(types$offence_key)])))
  groups <- unique(data.frame(group_id = tf$offence_type_id, sort_order = as.integer(tf$sort_order), name_es = tf$name_local, name_en = tf$name_en, stringsAsFactors = FALSE))
  groups <- groups[order(groups$sort_order), ]
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


# Municipality -> ABP by geometry: the point on the surface of each Catalan municipality (IGN boundaries,
# data/territory_es.gpkg) inside the ABP polygons. Barcelona itself is left out (its districts are ABPs).
abp_assign_spatially <- function(f_geo) {
  gpkg <- if (exists("COUNTRY_MODULES") && !is.null(COUNTRY_MODULES$ES)) COUNTRY_MODULES$ES$gpkg else "data/territory_es.gpkg"
  if (length(f_geo) == 0 || !file.exists(gpkg) || !"municipality" %in% tryCatch(sf::st_layers(gpkg)$name, error = function(e) character())) return(NULL)
  g <- sf::st_make_valid(sf::st_read(f_geo[1], quiet = TRUE)); names(g)[names(g) != attr(g, "sf_column")] <- tolower(names(g)[names(g) != attr(g, "sf_column")])
  mun <- sf::st_read(gpkg, layer = "municipality", quiet = TRUE); mun <- mun[!is.na(mun$region_id) & mun$region_id == "ES-09" & mun$territory_id != ID_BARCELONA, ]
  if (nrow(mun) == 0) return(NULL)
  pts <- suppressWarnings(sf::st_point_on_surface(sf::st_transform(sf::st_make_valid(mun), 25831))); g <- sf::st_transform(g, 25831)
  hit <- suppressMessages(sf::st_intersects(pts, g)); i <- vapply(hit, function(h) if (length(h)) h[1] else NA_integer_, 1L)
  # a municipality whose point falls in no polygon (coast, file edge): the nearest ABP
  if (anyNA(i)) i[is.na(i)] <- suppressMessages(sf::st_nearest_feature(pts[is.na(i), ], g))
  data.frame(abp_code = sprintf("%02d", as.integer(g$codi_abp_mossos[i])), ine_code = sub("^.*-", "", mun$territory_id), district_code = NA_character_, file = "spatial (IGN municipalities in ABP polygons)", stringsAsFactors = FALSE)
}

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
  # Outside Barcelona the municipalities are assigned to the ABP polygon that contains them (IGN municipal
  # boundaries of the Spain module), which cannot disagree with the map; the CSV codes are compared with that
  # assignment and the agreement printed (100 % with the real files, 2026-11-06; the sandbox copies were
  # synthetic). The Barcelona districts (34-43) come from the CSV. The CSV is the fallback when a file is missing.
  spatial <- abp_assign_spatially(f_geo)
  if (!is.null(spatial)) spatial <- spatial[!spatial$abp_code %in% tab$abp_code[!is.na(tab$district_code)], ]   # a district ABP holds its district only
  if (!is.null(spatial)) {
    csv <- tab[is.na(tab$district_code), ]; k <- match(spatial$ine_code, csv$ine_code); agree <- mean(spatial$abp_code == csv$abp_code[k], na.rm = TRUE)
    message(sprintf("  municipality -> ABP: %d assigned by geometry; the CSV agrees for %.0f%% of them%s", nrow(spatial), 100 * agree,
                    if (is.finite(agree) && agree < 0.9) " (the CSV codes do not follow the boundaries file: geometry wins)" else ""))
    tab <- rbind(tab[!is.na(tab$district_code), ], spatial)
  }
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
                     name = g$nom_abp_mossos, region_id = "ES-09", province_id = NA_character_, geometry = sf::st_geometry(g))
    # The police areas are a layer of the Spain GeoPackage (written by the ES territory step before this one)
    gpkg_es <- if (exists("COUNTRY_MODULES") && !is.null(COUNTRY_MODULES$ES)) COUNTRY_MODULES$ES$gpkg else "data/territory_es.gpkg"
    if (file.exists(gpkg_es)) { sf::st_write(abp, gpkg_es, layer = "police_area", quiet = TRUE, delete_layer = TRUE); message(sprintf("  police_area layer: %d polygons in %s", nrow(abp), gpkg_es)) }
    else message("  (", gpkg_es, " not built yet: the police_area layer is written at the next run)")
    DBI::dbWriteTable(con, "ref_abp", data.frame(abp_code = sprintf("%02d", as.integer(g$codi_abp_mossos)),
                                                 name = g$nom_abp_mossos, region_code = g$codi_rp_mossos %||% NA_character_),
                      overwrite = TRUE)
  }
  mossos_abp_map(con)
  message(sprintf("Police areas: %d ABP with %d municipalities%s%s", length(unique(tab$abp_code)), length(unique(tab$ine_code)),
                  if (!is.null(spatial)) " (assigned by geometry)" else " (CSV assignment: unreliable outside Barcelona)",
                  if (length(f_geo) > 0) "" else " (no boundaries)"))
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
  if (length(unmapped) > 0) message("Offence types without equivalence (reference/es_mossos_offence_types.csv): ", paste(unmapped, collapse = " | "))
  invisible(TRUE)
}

# Marts of the Barcelona city panel: by district and month per offence group ('total' includes ALL
# offences, sensitive ones too). Catalonia by police area lives in the shared country tables (R/72_es_mossos.R).
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
