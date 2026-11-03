# Mexico: carpetas de investigación by municipio and entidad federativa -------------------
# Source: SESNSP (Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública),
# "Incidencia delictiva del fuero común, nueva metodología", municipal file: investigation
# files (carpetas de investigación) opened by the 32 state prosecutors' offices, by month,
# municipio and Bien jurídico > Tipo > Subtipo > Modalidad, January 2015 onwards, Libre Uso MX.
# The file is a zip with one wide CSV (12 month columns, Latin-1) whose URL changes every
# month: reference/mx_sources.csv holds the open-data page (scraped for the current link) and
# a URL the user can set by hand. Leaf category = "Tipo de delito", except Homicidio, Lesiones
# and Robo, split by "Subtipo" (doloso/culposo; kinds of robbery): reference/mx_offence_types.csv.
# Levels: region = entidad federativa (Clave_Ent), municipality = municipio (Cve. Municipio,
# 5 digits = entidad 2 + municipio 3). Population: CONAPO municipal projections (URL in
# mx_sources.csv). Boundaries: geoBoundaries ADM1/ADM2 (CC BY 4.0) unless an INEGI Marco
# Geoestadístico zip is configured.
#
# Written against the published documentation (no gob.mx page could be opened from the sandbox
# where this module was written): the reader detects columns by name and `mx_inspect()` prints
# the header of each downloaded file so that MX_COLUMNS can be adjusted.

MX_STATE <- new.env()   # parsed municipal table, shared by mx_territory and mx_ingest
MX_MONTHS <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
# Accepted names for every field (first match wins, case-insensitive, accents ignored)
MX_COLUMNS <- list(
  year     = c("^a[ñn]o$", "^anio$", "^year$"),
  ent_code = c("^clave_ent$", "^cve_ent$", "^clave ?entidad$", "^cve\\.? ?ent"),
  ent_name = c("^entidad$", "^nom_ent$"),
  mun_code = c("^cve\\.? ?municipio$", "^cve_mun", "^clave_mun", "^clave ?municipio$"),
  mun_name = c("^municipio$", "^nom_mun$"),
  tipo     = c("^tipo de delito$", "^tipo$"),
  subtipo  = c("^subtipo de delito$", "^subtipo$"),
  modalidad = c("^modalidad$")
)
MX_SPLIT_BY_SUBTIPO <- c("Homicidio", "Lesiones", "Robo")   # tipos whose subtipo is the leaf category

mx_norm <- function(x) { x <- normalize_text(x); x[is.na(x)] <- ""; x }
mx_pick <- function(nm, patterns) { n <- mx_norm(nm); for (p in patterns) { i <- grep(toupper(p), n, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }

# --- Sources (reference/mx_sources.csv: key, url, note) --------------------------------------
mx_sources <- function() read_reference("mx_sources.csv")
mx_source_url <- function(key) { s <- mx_sources(); u <- trimws(s$url[s$key == key]); u <- u[!is.na(u) & nzchar(u)]; if (length(u)) u[1] else NA_character_ }

# Google Drive share links -> direct download; anything else unchanged
mx_direct_url <- function(url) {
  # SharePoint / OneDrive share links (used by the SESNSP since 2026) answer with a viewer page; "download=1" asks for the file itself
  if (grepl("sharepoint\\.com|1drv\\.ms|onedrive\\.live\\.com", url) && !grepl("download=1", url)) return(paste0(url, if (grepl("\\?", url)) "&" else "?", "download=1"))
  if (!grepl("drive\\.google\\.com|docs\\.google\\.com", url)) return(url)
  id <- regmatches(url, regexpr("(?<=/d/)[A-Za-z0-9_-]{10,}|(?<=[?&]id=)[A-Za-z0-9_-]{10,}", url, perl = TRUE))
  if (!length(id)) return(url)
  sprintf("https://drive.usercontent.google.com/download?id=%s&export=download&confirm=t", id)
}

# Best effort: the current "Municipal-Delitos" link on the SESNSP open-data page
mx_find_link <- function(page_url) {
  html <- get_text(page_url); if (is.null(html)) return(NA_character_)
  anchors <- regmatches(html, gregexpr("<a[^>]+href=\"[^\"]+\"[^>]*>.*?</a>", html, perl = TRUE))[[1]]
  hrefs <- sub(".*href=\"([^\"]+)\".*", "\\1", anchors)
  hit <- hrefs[grepl("Municipal[-_ ]?Delitos", hrefs, ignore.case = TRUE)]
  if (!length(hit)) hit <- hrefs[grepl("municipal", anchors, ignore.case = TRUE) & grepl("delitos|drive\\.google", anchors, ignore.case = TRUE)]
  if (!length(hit)) return(NA_character_)
  u <- gsub("&amp;", "&", hit[1])
  if (grepl("^/", u)) u <- paste0("https://www.gob.mx", u)
  u
}

mx_file <- function(what = "municipal") {
  f <- newest_first(list.files(COUNTRY_MODULES$MX$dir_raw, paste0("_mx_", what, "_delitos\\.(zip|csv)$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}

# Download (never kept when the server answered with an HTML page instead of the file)
mx_fetch <- function(url, name) {
  m <- COUNTRY_MODULES$MX
  tmp <- file.path(tempdir(), paste0("download_", name)); if (file.exists(tmp)) file.remove(tmp)
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  download_safely(mx_direct_url(url), tmp)
  head <- readBin(tmp, "raw", 256)
  if (!identical(head[1:2], as.raw(c(0x50, 0x4B))) && grepl("<html|<!doctype", tolower(rawToChar(head[head != 0])), fixed = FALSE))
    stop("The answer for ", name, " is a web page, not a file (Google Drive confirmation or moved link).\n  Download it by hand from the SESNSP page and copy it to ",
         m$dir_raw, "/", format(Sys.Date(), "%Y%m%d"), "_", name, ", or set the 'municipal' url in reference/mx_sources.csv")
  keep_version(tmp, m$dir_raw, name)
}

mx_download <- function(days = 25) {
  for (what in c("municipal", "estatal")) {
    prev <- mx_file(what)
    if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < days) { message("= MX ", what, " file downloaded less than ", days, " days ago"); next }
    url <- NA_character_
    if (what == "municipal" && !is.na(page <- mx_source_url("page"))) {
      url <- mx_find_link(page)
      if (!is.na(url)) message("  SESNSP page: current municipal link ", url) else message("  no 'Municipal-Delitos' link found on ", page)
    }
    if (is.na(url)) url <- mx_source_url(what)
    if (is.na(url)) { if (what == "municipal") stop("No URL for the SESNSP municipal file: set 'municipal' in reference/mx_sources.csv"); next }
    ext <- if (grepl("\\.csv($|\\?)", url, ignore.case = TRUE)) "csv" else "zip"
    mx_fetch(url, paste0("mx_", what, "_delitos.", ext))
  }
}

# --- Reading the SESNSP files ----------------------------------------------------------------

# The CSV inside a zip (the largest one), or the CSV itself
mx_csv_path <- function(f) {
  if (grepl("\\.csv$", f, ignore.case = TRUE)) return(f)
  tmp <- file.path(tempdir(), tools::file_path_sans_ext(basename(f))); unlink(tmp, recursive = TRUE)
  utils::unzip(f, exdir = tmp)
  csv <- list.files(tmp, "\\.csv$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  if (!length(csv)) stop("No CSV inside ", basename(f))
  csv[which.max(file.size(csv))]
}
mx_encoding <- function(csv) { l <- readLines(csv, n = 200, warn = FALSE); if (all(validUTF8(l))) "UTF-8" else "latin1" }
mx_header <- function(csv, enc = mx_encoding(csv)) names(readr::read_csv(csv, n_max = 0, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = enc), show_col_types = FALSE, name_repair = "minimal"))

# Read one SESNSP file keeping only the needed columns: id columns as text, months as numbers
mx_read <- function(f, level = "municipality") {
  csv <- mx_csv_path(f); enc <- mx_encoding(csv); nm <- mx_header(csv, enc)
  col <- lapply(MX_COLUMNS, mx_pick, nm = nm)
  months <- nm[match(MX_MONTHS, mx_norm(nm))]; months <- months[!is.na(months)]
  if (is.na(col$year) || is.na(col$ent_code) || is.na(col$tipo) || length(months) < 3 || (level == "municipality" && is.na(col$mun_code)))
    stop("Unexpected columns in the SESNSP ", level, " file ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun mx_inspect() and adjust MX_COLUMNS in R/77_mx.R")
  ids <- unlist(col[!is.na(col)])
  types <- c(setNames(lapply(ids, function(x) readr::col_character()), ids), setNames(lapply(months, function(x) readr::col_double()), months))
  d <- readr::read_csv(csv, col_types = do.call(readr::cols_only, types), locale = readr::locale(encoding = enc), progress = FALSE, show_col_types = FALSE, name_repair = "minimal")
  tipo <- trimws(d[[col$tipo]]); sub <- if (!is.na(col$subtipo)) trimws(d[[col$subtipo]]) else tipo
  out <- data.frame(year = as.integer(d[[col$year]]), ent = sprintf("%02d", as.integer(d[[col$ent_code]])),
                    code = ifelse(tipo %in% MX_SPLIT_BY_SUBTIPO & !is.na(sub) & nzchar(sub), sub, tipo), stringsAsFactors = FALSE)
  out$ent_name <- if (!is.na(col$ent_name)) trimws(d[[col$ent_name]]) else NA_character_
  if (level == "municipality") {
    out$mun <- sprintf("%05d", as.integer(d[[col$mun_code]]))
    out$mun_name <- if (!is.na(col$mun_name)) trimws(d[[col$mun_name]]) else NA_character_
  }
  # Months not yet reported are empty for every row: they are dropped (the quarter stays incomplete)
  for (i in seq_along(MX_MONTHS)) { mc <- months[match(MX_MONTHS[i], mx_norm(months))]; if (!is.na(mc) && any(!is.na(d[[mc]]))) out[[paste0("m", i)]] <- d[[mc]] }
  out[!is.na(out$year) & !is.na(out$code) & nzchar(out$code), ]
}

# Print the header and first rows of the downloaded files (to adjust MX_COLUMNS)
mx_inspect <- function() {
  for (what in c("municipal", "estatal")) {
    f <- mx_file(what); if (is.na(f)) { message(what, ": no file yet"); next }
    csv <- mx_csv_path(f); enc <- mx_encoding(csv)
    message(what, ": ", basename(f), " (", enc, ")\n  ", paste(mx_header(csv, enc), collapse = " | "))
    print(as.data.frame(readr::read_csv(csv, n_max = 3, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = enc), show_col_types = FALSE, name_repair = "minimal")))
  }
  for (f in list.files(COUNTRY_MODULES$MX$dir_raw, "_mx_population_", full.names = TRUE)) message("population: ", basename(f), "\n  ", paste(names(mx_read_table(f, 0)), collapse = " | "))
}

# Sum the month columns by `keys`; a month that is empty for a whole year in the file (not yet
# reported) stays NA after the sum, so that the quarter is left incomplete
mx_aggregate <- function(d, keys, ...) {
  mcols <- grep("^m\\d+$", names(d), value = TRUE)
  reported <- d |> dplyr::group_by(year) |> dplyr::summarise(dplyr::across(dplyr::all_of(mcols), ~ any(!is.na(.x))), .groups = "drop")
  agg <- d |> dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(..., dplyr::across(dplyr::all_of(mcols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |> as.data.frame()
  for (mc in mcols) agg[[mc]][!reported[[mc]][match(agg$year, reported$year)]] <- NA_real_
  agg
}

# Municipal table aggregated to the leaf category (one row per year x municipio x code), cached
mx_load_municipal <- function(f) {
  if (!is.null(MX_STATE$muni) && identical(MX_STATE$file, f)) return(MX_STATE$muni)
  message("  reading ", basename(f), " (a few minutes and ~3 GB of memory the first time)")
  d <- mx_read(f, "municipality")
  agg <- mx_aggregate(d, c("year", "ent", "mun", "code"), mun_name = dplyr::first(mun_name))
  # "No especificado" / "Otros municipios" pseudo-municipios count in the state but are not places
  agg$special <- as.integer(substr(agg$mun, 3, 5)) >= 990 | grepl("NO ESPECIFICADO|OTROS MUNICIPIOS", mx_norm(agg$mun_name))
  MX_STATE$muni <- agg; MX_STATE$file <- f
  agg
}

# Wide months (m1..m12) -> long (year, month, n), then cumulative at the end of complete quarters
mx_cumulative <- function(w, level, id) {
  mcols <- grep("^m\\d+$", names(w), value = TRUE)
  long <- do.call(rbind, lapply(mcols, function(mc) data.frame(country = "MX", territory_id = id, level = level, offence_code = w$code, year = w$year,
                                                                month = as.integer(sub("m", "", mc)), n = w[[mc]], stringsAsFactors = FALSE)))
  monthly_to_cumulative(long[!is.na(long$n), ])   # unreported months are absent: their quarter is not complete
}
# Add the synthetic total (sum of every tipo, as SESNSP's own "total" does) to a wide table
mx_with_total <- function(w, keys) {
  mcols <- grep("^m\\d+$", names(w), value = TRUE)
  tot <- w |> dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "year")))) |> dplyr::summarise(dplyr::across(dplyr::all_of(mcols), sum), .groups = "drop") |> as.data.frame()
  tot$code <- "TOTAL_TIPOS"
  rbind(w[, c(keys, "year", "code", mcols)], tot[, c(keys, "year", "code", mcols)])
}

# Labels in the data -> the spelling of reference/mx_offence_types.csv (accents/case tolerant); unknown labels are reported
mx_align_codes <- function(code, ref) {
  m <- match(mx_norm(code), mx_norm(ref)); out <- ifelse(is.na(m), code, ref[m])
  unknown <- sort(unique(code[is.na(m)]))
  if (length(unknown)) message("  [!!] ", length(unknown), " categories absent from reference/mx_offence_types.csv (counted in the total, not listed): ", paste(unknown, collapse = "; "))
  out
}

mx_ingest <- function(con) {
  m <- COUNTRY_MODULES$MX
  f <- mx_file("municipal")
  if (is.na(f)) stop("No SESNSP municipal file in ", m$dir_raw, ": run the download, or copy the zip there as YYYYMMDD_mx_municipal_delitos.zip")
  d <- mx_load_municipal(f)
  d$code <- mx_align_codes(d$code, read_reference("mx_offence_types.csv")$source_code)
  rows <- list()
  for (e in sort(unique(d$ent))) {                      # one state at a time to keep memory low
    x <- d[d$ent == e, ]
    muni <- mx_with_total(x[!x$special, ], c("ent", "mun"))
    reg <- mx_with_total(x |> dplyr::group_by(ent, year, code) |> dplyr::summarise(dplyr::across(dplyr::matches("^m\\d+$"), sum), .groups = "drop") |> as.data.frame(), "ent")   # NA months stay NA
    rows[[e]] <- rbind(mx_cumulative(muni, "municipality", paste("MX", muni$ent, muni$mun, sep = "-")), mx_cumulative(reg, "region", paste0("MX-", reg$ent)))
  }
  crime <- do.call(rbind, rows); crime$file <- basename(f)
  files <- basename(f)
  # Official state file, when present: its rows replace the sums of the municipios
  fe <- mx_file("estatal")
  if (!is.na(fe)) {
    s <- mx_aggregate(mx_read(fe, "region"), c("year", "ent", "code"))
    s$code <- mx_align_codes(s$code, read_reference("mx_offence_types.csv")$source_code)
    reg <- mx_cumulative(mx_with_total(s, "ent"), "region", paste0("MX-", s$ent)); reg$file <- basename(fe)
    crime <- rbind(crime[crime$level != "region", ], reg); files <- paste(files, basename(fe), sep = ";")
    message("  state rows taken from ", basename(fe))
  }
  # National figures: sum of the 32 entidades
  nat <- crime[crime$level == "region", ] |> dplyr::group_by(country, offence_code, year, quarter, file) |> dplyr::summarise(cumulative = sum(cumulative), .groups = "drop")
  nat$territory_id <- "MX"; nat$level <- "country"
  crime <- rbind(crime, nat[, names(crime)])
  munis <- unique(d[!d$special, c("ent", "mun", "mun_name")])
  ensure_territories(con, "MX", paste("MX", munis$ent, munis$mun, sep = "-"), "municipality", paste0("MX-", munis$ent), munis$mun_name)
  replace_country_rows(con, "stg_country_crime", "MX", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_MX", files, NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  last <- max(crime$year * 10 + crime$quarter)
  message(sprintf("  %s rows, %s municipios, 32 entidades, %d-%dQ%d", fmt_int(nrow(crime)), fmt_int(nrow(munis)), min(crime$year), last %/% 10, last %% 10))
  invisible(crime)
}

# --- Territories and boundaries ------------------------------------------------------------------

# geoBoundaries ADM1 names/ISO codes -> Clave_Ent (names first: "Distrito Federal" carries a wrong ISO code)
mx_match_state <- function(names, iso, states) {
  alias <- lapply(seq_len(nrow(states)), function(i) mx_norm(c(states$name[i], strsplit(states$aliases[i] %||% "", "\\|")[[1]])))
  key <- rep(states$clave_ent, lengths(alias)); alias <- unlist(alias)
  ent <- key[match(mx_norm(names), alias)]
  by_iso <- states$clave_ent[match(iso, states$iso_3166_2)]; dup <- iso %in% iso[duplicated(iso)]
  ifelse(is.na(ent) & !dup, by_iso, ent)
}

# Municipal polygons (no code in geoBoundaries ADM2): state by location, then municipio by name within the state
mx_boundaries_geoboundaries <- function(states, munis) {
  adm1 <- geoboundaries("MEX", 1)
  adm1$ent <- mx_match_state(adm1$shapeName, adm1$shapeISO, states); adm1 <- adm1[!is.na(adm1$ent), ]
  if (nrow(adm1) != 32) warning("geoBoundaries ADM1: ", nrow(adm1), " states matched instead of 32")
  adm1$territory_id <- paste0("MX-", adm1$ent); adm1$name <- states$name[match(adm1$ent, states$clave_ent)]
  layers <- list(region = adm1[, c("territory_id", "name")])
  if (is.null(munis)) { message("  no SESNSP file yet: municipal polygons are matched after the download (run again)"); return(layers) }
  adm2 <- geoboundaries("MEX", 2)
  pts <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(adm2)))
  inside <- sf::st_within(pts, adm1); i <- vapply(inside, function(k) if (length(k)) k[1] else NA_integer_, 1L)
  i[is.na(i)] <- sf::st_nearest_feature(pts[is.na(i)], adm1)
  adm2$ent <- adm1$ent[i]
  simplify <- function(x) trimws(gsub("\\s+", " ", gsub("\\b(DE|DEL|LA|LAS|LOS|EL|Y)\\b|[^A-Z0-9 ]", "", mx_norm(x))))
  m <- match(paste(adm2$ent, mx_norm(adm2$shapeName)), paste(munis$ent, mx_norm(munis$mun_name)))
  m2 <- match(paste(adm2$ent, simplify(adm2$shapeName)), paste(munis$ent, simplify(munis$mun_name))); m[is.na(m)] <- m2[is.na(m)]
  ok <- !is.na(m)
  message(sprintf("  geoBoundaries ADM2 (2012): %d of %d polygons matched to SESNSP municipios; %d municipios without polygon (table only)", sum(ok), nrow(adm2), nrow(munis) - length(unique(m[ok]))))
  g <- adm2[ok, ]; g$territory_id <- paste("MX", munis$ent[m[ok]], munis$mun[m[ok]], sep = "-"); g$name <- munis$mun_name[m[ok]]
  g <- g[, c("territory_id", "name")]
  if (anyDuplicated(g$territory_id)) g <- g |> dplyr::group_by(territory_id, name) |> dplyr::summarise(.groups = "drop")
  layers$municipality <- g
  layers
}

# INEGI Marco Geoestadístico zip (00ent.shp / 00mun.shp with CVEGEO, NOMGEO) when configured in mx_sources.csv
mx_boundaries_inegi <- function(url, states) {
  dir <- COUNTRY_MODULES$MX$dir_raw
  f <- newest_first(list.files(dir, "_mx_marco_geoestadistico\\.zip$", full.names = TRUE))
  if (!length(f)) f <- download_versioned(url, dir, name = "mx_marco_geoestadistico.zip")
  tmp <- file.path(tempdir(), "mx_mg"); unlink(tmp, recursive = TRUE); utils::unzip(f[1], exdir = tmp)
  shp <- function(p) { x <- list.files(tmp, p, full.names = TRUE, recursive = TRUE, ignore.case = TRUE); if (!length(x)) stop("No ", p, " inside the INEGI zip"); sf::st_read(x[1], quiet = TRUE) }
  ent <- shp("ent\\.shp$"); mun <- shp("mun\\.shp$")
  code <- function(g, w) { c <- mx_pick(names(g), c("^cvegeo$", "^cve_geo$")); if (is.na(c)) stop("INEGI ", w, " layer without CVEGEO: ", paste(names(g), collapse = " | ")); as.character(g[[c]]) }
  nm <- function(g) { c <- mx_pick(names(g), c("^nomgeo$", "^nom_", "^nombre")); if (is.na(c)) NA_character_ else as.character(g[[c]]) }
  ent$territory_id <- paste0("MX-", substr(code(ent, "ent"), 1, 2)); ent$name <- states$name[match(substr(code(ent, "ent"), 1, 2), states$clave_ent)]
  mun$territory_id <- paste("MX", substr(code(mun, "mun"), 1, 2), substr(code(mun, "mun"), 1, 5), sep = "-"); mun$name <- nm(mun)
  list(region = ent[, c("territory_id", "name")], municipality = mun[, c("territory_id", "name")])
}

mx_territory <- function(con) {
  states <- read_reference("mx_states.csv")
  tbl <- data.frame(territory_id = paste0("MX-", states$clave_ent), level = "region", parent_id = "MX", official_code = states$clave_ent, name = states$name, stringsAsFactors = FALSE)
  f <- mx_file("municipal"); munis <- NULL
  if (!is.na(f)) {
    d <- mx_load_municipal(f)
    munis <- d[!d$special, c("year", "ent", "mun", "mun_name")] |> dplyr::arrange(dplyr::desc(year)) |> dplyr::distinct(mun, .keep_all = TRUE) |> as.data.frame()   # latest name per municipio
    munis$mun_name[is.na(munis$mun_name) | munis$mun_name == ""] <- munis$mun[is.na(munis$mun_name) | munis$mun_name == ""]
    tbl <- rbind(tbl, data.frame(territory_id = paste("MX", munis$ent, munis$mun, sep = "-"), level = "municipality", parent_id = paste0("MX-", munis$ent),
                                 official_code = munis$mun, name = munis$mun_name, stringsAsFactors = FALSE))
  } else message("  no SESNSP file yet: municipios are created at the first ingest")
  save_country_territory(con, "MX", tbl, scheme = "INEGI")
  layers <- tryCatch({
    inegi <- mx_source_url("boundaries_inegi")
    if (!is.na(inegi)) mx_boundaries_inegi(inegi, states) else mx_boundaries_geoboundaries(states, munis)
  }, error = function(e) { message("  boundaries not available (tables only): ", conditionMessage(e)); NULL })
  if (length(layers)) save_country_gpkg(con, "MX", layers, tolerance_m = 150)
  invisible(tbl)
}

# --- Population (CONAPO municipal projections, or any file with a 5-digit municipal key) --------

# CSV (Latin-1/UTF-8) or XLSX as text; n = 0 reads the header only
mx_read_table <- function(f, n = Inf) {
  if (grepl("\\.xlsx?$", f, ignore.case = TRUE)) return(readxl::read_excel(f, n_max = if (is.finite(n)) n else Inf, col_types = "text"))
  readr::read_csv(f, n_max = n, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = mx_encoding(f)), progress = FALSE, show_col_types = FALSE, name_repair = "minimal")
}

# -> cve (5 digits), year, population (summed over sex / age rows)
mx_read_population <- function(f) {
  d <- mx_read_table(f); nm <- names(d)
  code <- mx_pick(nm, c("^clave$", "^cve_?geo$", "^clave_mun", "^cve_mun$", "^cvemun$", "^mun_id$"))
  year <- mx_pick(nm, c("^a[ñn]o$", "^anio$", "^year$", "^periodo$"))
  pop  <- mx_pick(nm, c("^pob$", "^pob_?tot(al)?$", "^poblaci[óo]n(_total)?$", "^pob_mit_a[ñn]o$", "^total$"))
  ent <- mx_pick(nm, c("^entidad$", "^clave_ent$", "^cve_ent$", "^ent$")); mun <- mx_pick(nm, c("^mun$", "^municipio$", "^cve_mun$"))
  if (is.na(code) && !is.na(ent) && !is.na(mun) && !is.na(suppressWarnings(as.integer(d[[mun]][1])))) {   # INEGI census: ENTIDAD + MUN
    d$cve_ <- sprintf("%02d%03d", as.integer(d[[ent]]), as.integer(d[[mun]])); code <- "cve_"
    loc <- mx_pick(nm, c("^loc$", "^cve_loc$")); if (!is.na(loc)) d <- d[as.integer(d[[loc]]) == 0, ]           # ITER files: municipal total rows only
  }
  if (is.na(code) || is.na(pop)) stop("Population file ", basename(f), ": no municipal key / population column in: ", paste(nm, collapse = " | "))
  out <- data.frame(cve = sprintf("%05d", suppressWarnings(as.integer(gsub("[^0-9]", "", d[[code]])))),
                    year = if (!is.na(year)) as.integer(substr(gsub("[^0-9]", "", d[[year]]), 1, 4)) else { y <- regmatches(basename(f), regexpr("20[0-9]{2}", basename(f))); as.integer(if (length(y)) y else 2020) },
                    population = suppressWarnings(as.numeric(gsub("[^0-9.]", "", d[[pop]]))), stringsAsFactors = FALSE)
  # state totals (xx000) and pseudo-municipios (xx990+) are left out
  out <- out[!is.na(out$cve) & !is.na(out$year) & !is.na(out$population) & substr(out$cve, 3, 5) != "000" & as.integer(substr(out$cve, 3, 5)) < 990, ]
  out |> dplyr::group_by(cve, year) |> dplyr::summarise(population = sum(population), .groups = "drop") |> as.data.frame()
}

mx_population <- function(con) {
  m <- COUNTRY_MODULES$MX; s <- mx_sources()
  s <- s[grepl("^population", s$key) & !is.na(s$url) & nzchar(trimws(s$url)), ]
  if (nrow(s) == 0) stop("No population URL in reference/mx_sources.csv (keys population_*)")
  files <- character()
  for (i in seq_len(nrow(s))) {
    ext <- if (grepl("\\.xlsx?($|\\?)", s$url[i], ignore.case = TRUE)) "xlsx" else "csv"
    name <- paste0("mx_", s$key[i], ".", ext)
    prev <- newest_first(list.files(m$dir_raw, paste0("_", name, "$"), full.names = TRUE))
    if (!length(prev) || difftime(Sys.time(), file.mtime(prev[1]), units = "days") > 365)
      prev <- tryCatch(download_versioned(s$url[i], m$dir_raw, name = name), error = function(e) { message("  population download failed (", s$key[i], "): ", conditionMessage(e)); prev })
    if (length(prev)) files <- c(files, prev[1])
  }
  if (!length(files)) stop("No population file in ", m$dir_raw)
  pop <- do.call(rbind, lapply(files, mx_read_population)) |> dplyr::group_by(cve, year) |> dplyr::summarise(population = sum(population), .groups = "drop") |> as.data.frame()
  muni <- data.frame(country = "MX", territory_id = paste("MX", substr(pop$cve, 1, 2), pop$cve, sep = "-"), level = "municipality", year = pop$year, population = pop$population)
  reg <- pop |> dplyr::group_by(ent = substr(cve, 1, 2), year) |> dplyr::summarise(population = sum(population), .groups = "drop")
  reg <- data.frame(country = "MX", territory_id = paste0("MX-", reg$ent), level = "region", year = reg$year, population = reg$population)
  nat <- reg |> dplyr::group_by(year) |> dplyr::summarise(population = sum(population), .groups = "drop")
  nat <- data.frame(country = "MX", territory_id = "MX", level = "country", year = nat$year, population = nat$population)
  out <- rbind(muni, reg, nat)
  replace_country_rows(con, "ref_country_population", "MX", out)
  message(sprintf("  population: %s municipios, %d-%d (%s)", fmt_int(length(unique(muni$territory_id))), min(out$year), max(out$year), paste(basename(files), collapse = ", ")))
}

register_country(list(
  code = "MX", iso3 = "MEX", name_en = "Mexico", name_local = "México", lang = "es",
  levels = c("region", "municipality"),
  level_labels = list(region = c("States", "Entidades federativas", "State", "Entidad federativa"),
                      municipality = c("Municipalities", "Municipios", "Municipality", "Municipio")),
  source = list(name = "Incidencia delictiva del fuero común, nueva metodología (SESNSP)", publisher = "Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)",
                url = "https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva",
                licence = "Términos de Libre Uso de la Información del Gobierno de México (Libre Uso MX)",
                measure = "Carpetas de investigación (investigation files) opened by the state prosecutors' offices for fuero común offences, counted by month of registration (offences, not victims), as reported to SESNSP; the total shown is the sum of all types, including negligent homicide and injuries."),
  population_source = "CONAPO, Proyecciones de la Población de los Municipios de México (Libre Uso MX); URL in reference/mx_sources.csv",
  boundaries_attribution = "geoBoundaries (www.geoboundaries.org), CC BY 4.0; ADM2 polygons from 2012 matched to current municipios by name",
  not_published = c("No suppression threshold: SESNSP publishes every figure, zeros included", "Sin umbral de supresión: el SESNSP publica todas las cifras, incluidos los ceros"),
  notes_en = "Prosecutor files, not police incidents: very high under-reporting (ENVIPE 'cifra negra' above 90%) and uneven recording practices between states; monthly files are revised retroactively; 'Tipo de delito' is the leaf category except Homicidio, Lesiones and Robo (split by subtipo); negligent homicide and injuries are counted in the total but not listed.",
  default_region = "MX-09",
  download = mx_download, ingest = mx_ingest, territory = mx_territory, population = mx_population
))
