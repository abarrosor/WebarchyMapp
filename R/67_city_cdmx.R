# Mexico City (CMX): investigation files of the FGJ by colonia, monthly ----------------------------------------
# City module of the shared city framework (R/60_cities.R). Source: "Carpetas de investigación FGJ de la Ciudad
# de México" on datos.cdmx.gob.mx (Fiscalía General de Justicia, open data of Mexico City): one cumulative file
# of every investigation file opened since 2016, one row per carpeta, with the offence (delito, categoria_delito),
# the colonia and alcaldía of the facts and coordinates. The module keeps only month x colonia x category (no day,
# hour or coordinates leave the raw file). Hierarchy: Mexico City (a node under the MX-09 region of the Mexico
# country module) > the 16 alcaldías as districts (INEGI municipio codes, reference/cdmx_alcaldias.csv) >
# colonias as neighbourhoods (the portal's colonias layer). Types: categoria_delito (reference/cdmx_event_types.csv;
# a category not in the file is kept under its own name; homicide, rape and kidnapping count in totals only).
# Population: alcaldías from the Mexico module (CONAPO, ref_country_population); colonias have no official
# population, so rates exist for the alcaldías and the city only.
# Counting: the month the investigation file was opened (fecha_inicio), as the SESNSP municipal data does;
# the location is where the facts happened (colonia_hecho / alcaldia_hecho). The file is big (hundreds of MB):
# every monthly copy is kept in data/raw/city_cdmx/carpetas/ (raw files are never deleted; move old ones aside).
#
# Written against the published columns, not run against the live files: `cdmx_inspect()` prints the headers.

ID_CDMX <- "MX-09-CDMX"
CDMX_DIR <- file.path(CONFIG$dir_raw, "city_cdmx")
cdmx_sources <- function() read_reference("cdmx_sources.csv")
cdmx_source <- function(key) { s <- cdmx_sources(); u <- s$url[s$key == key]; if (length(u) && !is.na(u) && nzchar(trimws(u))) trimws(u) else NA_character_ }
cdmx_dir <- function(sub) { d <- file.path(CDMX_DIR, sub); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
cdmx_newest <- function(sub, pattern) { f <- newest_first(list.files(file.path(CDMX_DIR, sub), pattern, full.names = TRUE)); if (length(f)) f[1] else NA_character_ }
cdmx_alcaldias <- function() read_reference("cdmx_alcaldias.csv")
cdmx_district_id <- function(mun) paste0(ID_CDMX, "-D", mun)

# alcaldía name (any spelling) -> INEGI municipio code
cdmx_mun_code <- function(name) {
  a <- cdmx_alcaldias(); alias <- lapply(seq_len(nrow(a)), function(i) c(a$name[i], if (!is.na(a$aliases[i]) && nzchar(a$aliases[i])) strsplit(a$aliases[i], "|", fixed = TRUE)[[1]]))
  rep(a$mun_code, lengths(alias))[match(normalize_text(name), normalize_text(unlist(alias)))]
}

# --- Download: the newest cumulative resource of the dataset, and the colonias layer ----------------------------
cdmx_resource <- function(dataset_key, manual_key, pattern) {
  manual <- cdmx_source(manual_key); if (!is.na(manual)) return(list(url = manual, name = basename(sub("\\?.*$", "", manual))))
  base <- cdmx_source("ckan_api"); id <- cdmx_source(dataset_key); if (is.na(base) || is.na(id)) stop("no ckan_api / ", dataset_key, " in reference/cdmx_sources.csv")
  res <- tryCatch(ckan_resources(id, base = base), error = function(e) stop("the datos.cdmx.gob.mx API could not be read (", conditionMessage(e), "): put a direct URL in reference/cdmx_sources.csv, key ", manual_key))
  res <- res[grepl(pattern, paste(res$format, res$url, res$name), ignore.case = TRUE), ]
  if (nrow(res) == 0) stop("no resource matching '", pattern, "' in dataset ", id, ": run cdmx_inspect()")
  res <- res[order(res$modified, decreasing = TRUE), ][1, ]
  list(url = res$url, name = ckan_file_name(res))
}

cdmx_download <- function(days = 20) {
  prev <- cdmx_newest("carpetas", "\\.(csv|zip|csv\\.gz)$")
  if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < days) message("= CDMX carpetas file downloaded less than ", days, " days ago") else {
    r <- cdmx_resource("carpetas_dataset", "carpetas_manual", "csv|zip")
    message("  CDMX carpetas: ", r$name, " (a large file; this can take several minutes)")
    download_versioned(r$url, cdmx_dir("carpetas"), name = r$name)
  }
  if (is.na(cdmx_newest("colonias", "\\.(geojson|json|zip|gpkg)$"))) {
    r <- cdmx_resource("colonias_dataset", "colonias_manual", "geojson|json|shp|zip|gpkg")
    download_versioned(r$url, cdmx_dir("colonias"), name = r$name)
  }
  invisible(TRUE)
}

# --- Territory: alcaldías and colonias from the portal's layer -------------------------------------------------------
cdmx_read_colonias <- function() {
  f <- cdmx_newest("colonias", "\\.(geojson|json|zip|gpkg)$"); if (is.na(f)) stop("no colonias layer in ", file.path(CDMX_DIR, "colonias"), " (option 14 with CMX)")
  src <- if (grepl("\\.zip$", f)) paste0("/vsizip/", f) else f
  g <- sf::st_read(src, quiet = TRUE); nm <- names(g)
  pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col <- list(code = pick("^cve_?col|^id_?col|^clave_?col|^cvegeo|^id$"), name = pick("^colonia$|^nom_?col|^nombre|^name"), alc = pick("^alcaldia$|^nom_?alc|^nomgeo|^delegacion|^municipio$"))
  if (is.na(col$name) || is.na(col$alc)) stop("colonias layer: no colonia / alcaldía name columns; fields are ", paste(nm, collapse = ", "), "\nRun cdmx_inspect()")
  g$col_name <- as.character(g[[col$name]]); g$alc_name <- as.character(g[[col$alc]])
  g$mun <- cdmx_mun_code(g$alc_name)
  lost <- unique(g$alc_name[is.na(g$mun)]); if (length(lost)) message("  alcaldías of the layer not in reference/cdmx_alcaldias.csv (left out): ", paste(lost, collapse = ", "))
  g <- g[!is.na(g$mun), ]
  code <- if (!is.na(col$code)) gsub("[^A-Za-z0-9]", "", as.character(g[[col$code]])) else NA_character_
  if (all(is.na(code)) || anyDuplicated(code)) code <- sprintf("%s%04d", g$mun, stats::ave(seq_len(nrow(g)), g$mun, FUN = seq_along))   # no usable code: a stable sequence inside each alcaldía
  g$col_code <- code; g$file <- basename(f)
  sf::st_make_valid(sf::st_transform(g, 4326))
}

cdmx_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_CDMX) > 0 && file.exists(CITY_MODULES$cdmx$gpkg)) { message("= Mexico City territory kept (", city_neighbourhood_count(con, ID_CDMX), " colonias)"); return(invisible(FALSE)) }
  g <- cdmx_read_colonias()
  district_id <- cdmx_district_id(g$mun)
  neighbourhoods <- sf::st_sf(territory_id = paste0(district_id, "-B", g$col_code), level = "neighbourhood", parent_id = district_id, country = "MX", official_code = g$col_code, code_scheme = "CDMX colonias layer",
                              name = g$col_name, area_m2 = as.numeric(sf::st_area(g)), file = g$file, geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, district_id)
  a <- cdmx_alcaldias(); dname <- a$name[match(sub("^.*-D", "", dis$territory_id), a$mun_code)]
  districts <- sf::st_sf(territory_id = dis$territory_id, level = "district", parent_id = ID_CDMX, country = "MX", official_code = sub("^.*-D", "", dis$territory_id), code_scheme = "INEGI municipio",
                         name = dname, area_m2 = as.numeric(sf::st_area(dis)), file = g$file[1], geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = ID_CDMX, level = "municipality", parent_id = "MX-09", country = "MX", official_code = "09", code_scheme = "WebarchyMapp (the whole entidad)", name = "Ciudad de México", stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES$cdmx$gpkg)
}

# --- Ingest: one row per carpeta -> month x colonia x category ------------------------------------------------------
cdmx_read_carpetas <- function(f) {
  src <- if (grepl("\\.zip$", f)) { inner <- utils::unzip(f, list = TRUE)$Name; inner <- inner[grepl("\\.csv$", inner, ignore.case = TRUE)][1]; if (is.na(inner)) stop("no CSV inside ", basename(f)); unz(f, inner) } else f
  cols <- readr::cols_only(fecha_inicio = "c", anio_inicio = "c", mes_inicio = "c", delito = "c", categoria_delito = "c", colonia_hecho = "c", colonia_catalogo = "c",
                           alcaldia_hecho = "c", alcaldia_catalogo = "c", latitud = "c", longitud = "c")
  d <- tryCatch(readr::read_csv(src, col_types = cols, progress = FALSE, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")),
                error = function(e) stop("could not read ", basename(f), ": ", conditionMessage(e), "\nRun cdmx_inspect() to see its columns"))
  if (nrow(d) == 0) stop(basename(f), " has no rows")
  d
}

cdmx_ingest <- function(con) {
  f <- cdmx_newest("carpetas", "\\.(csv|zip|csv\\.gz)$"); if (is.na(f)) stop("no carpetas file in ", file.path(CDMX_DIR, "carpetas"), " (option 14 with CMX)")
  d <- cdmx_read_carpetas(f)
  # month the file was opened
  ini <- suppressWarnings(as.Date(substr(d$fecha_inicio %||% NA, 1, 10)))
  if (all(is.na(ini)) && !is.null(d$anio_inicio)) ini <- suppressWarnings(as.Date(sprintf("%s-%02d-01", d$anio_inicio, as.integer(d$mes_inicio))))
  d$month <- as.Date(format(ini, "%Y-%m-01")); d <- d[!is.na(d$month) & d$month >= as.Date("2016-01-01") & d$month <= Sys.Date(), ]
  # alcaldía (where the facts happened), colonia from the catalogue name inside it
  alc <- d$alcaldia_hecho %||% d$alcaldia_catalogo; alc[is.na(alc) | !nzchar(alc)] <- (d$alcaldia_catalogo %||% NA)[is.na(alc) | !nzchar(alc)]
  d$mun <- cdmx_mun_code(alc)
  outside <- sum(is.na(d$mun)); d <- d[!is.na(d$mun), ]
  cols <- DBI::dbGetQuery(con, paste0("SELECT territory_id, parent_id, name FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_CDMX, "-%"))))
  key_layer <- paste(sub("^.*-D([0-9]{3})-.*$", "\\1", cols$territory_id), normalize_text(cols$name))
  colname <- d$colonia_catalogo %||% d$colonia_hecho; colname[is.na(colname) | !nzchar(colname)] <- (d$colonia_hecho %||% NA)[is.na(colname) | !nzchar(colname)]
  d$neighbourhood_id <- cols$territory_id[match(paste(d$mun, normalize_text(colname)), key_layer)]
  # rows whose colonia name is not in the layer: the coordinates decide
  todo <- which(is.na(d$neighbourhood_id) & !is.na(suppressWarnings(as.numeric(d$latitud))) & !is.na(suppressWarnings(as.numeric(d$longitud))))
  if (length(todo) && file.exists(CITY_MODULES$cdmx$gpkg)) {
    geo <- sf::st_read(CITY_MODULES$cdmx$gpkg, quiet = TRUE); geo <- geo[geo$level == "neighbourhood", ]
    pts <- sf::st_as_sf(data.frame(row = seq_along(todo), x = as.numeric(d$longitud[todo]), y = as.numeric(d$latitud[todo])), coords = c("x", "y"), crs = 4326)
    hit <- sf::st_drop_geometry(sf::st_join(pts, geo["territory_id"], join = sf::st_within, left = TRUE)); hit <- hit[!duplicated(hit$row), ]
    d$neighbourhood_id[todo] <- hit$territory_id[match(seq_along(todo), hit$row)]
  }
  d$district_id <- cdmx_district_id(d$mun)
  types <- read_reference("cdmx_event_types.csv", bool_cols = "sensitive")
  cat <- toupper(trimws(d$categoria_delito %||% "SIN CATEGORIA")); cat[is.na(cat) | !nzchar(cat)] <- "SIN CATEGORIA"
  d$event_type <- types$source_code[match(normalize_text(cat), normalize_text(types$source_code))]
  d$event_type[is.na(d$event_type)] <- cat[is.na(d$event_type)]           # a category the file does not know keeps its own name
  ev <- d |> dplyr::group_by(month, neighbourhood_id, district_id, event_type) |> dplyr::summarise(events = dplyr::n(), .groups = "drop") |> as.data.frame()
  ev$city <- "cdmx"; ev$file <- basename(f); ev$events <- as.integer(ev$events)
  replace_city_rows(con, "stg_city_events", "cdmx", ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  seen <- unique(ev$event_type); known <- types[types$source_code %in% seen, ]
  extra <- setdiff(seen, known$source_code)
  plain <- function(x) { x <- tolower(x); paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))) }
  t <- rbind(data.frame(city = "cdmx", event_type = known$source_code, block = known$block, name_local = known$name_local, name_en = known$name_en, sensitive = known$sensitive, stringsAsFactors = FALSE),
             if (length(extra)) data.frame(city = "cdmx", event_type = extra, block = "public_safety", name_local = plain(extra), name_en = plain(extra), sensitive = grepl("VIOLACION|SECUESTRO|HOMICIDIO|FEMINICIDIO|SEXUAL", extra), stringsAsFactors = FALSE))
  if (length(extra)) message("  categories not in cdmx_event_types.csv (kept under their own name): ", paste(extra, collapse = " / "))
  replace_city_rows(con, "ref_city_event_types", "cdmx", t)
  log_ingest(con, "city_cdmx", basename(f), NA, paste(format(range(ev$month), "%Y-%m"), collapse = " to "), nrow(ev), sum(ev$events))
  message(sprintf("  cdmx: %s carpetas in %s event rows, %d months (%s to %s), %d colonias with a polygon, %d%% without colonia (alcaldía totals), %s rows outside the city left out",
                  fmt_int(sum(ev$events)), fmt_int(nrow(ev)), length(unique(ev$month)), format(min(ev$month), "%m/%Y"), format(max(ev$month), "%m/%Y"),
                  length(unique(stats::na.omit(ev$neighbourhood_id))), round(100 * sum(ev$events[is.na(ev$neighbourhood_id)]) / sum(ev$events)), fmt_int(outside)))
  invisible(ev)
}

# --- Population: the alcaldías from the Mexico country module (CONAPO), as district rows ------------------------------
cdmx_population <- function(con) {
  pop <- DBI::dbGetQuery(con, "SELECT territory_id, year, population FROM ref_country_population WHERE country = 'MX' AND level = 'municipality' AND territory_id LIKE 'MX-09-%'")
  if (nrow(pop) == 0) stop("no Mexico population loaded yet (option 14 with MX): counts only, no rates")
  y <- max(pop$year[pop$year <= as.integer(format(Sys.Date(), "%Y"))]); pop <- pop[pop$year == y, ]
  pop$district_id <- cdmx_district_id(sub("^MX-09-", "", pop$territory_id))
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'district' AND parent_id = ", DBI::dbQuoteString(con, ID_CDMX)))$territory_id
  pop <- pop[pop$district_id %in% known, ]
  if (nrow(pop) == 0) stop("no alcaldía of Mexico City in dim_territory yet (the territory step failed): nothing to load")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_CDMX, "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = pop$district_id, ref_date = as.Date(sprintf("%d-07-01", y)), population = as.integer(round(pop$population)), file = "ref_country_population (MX, CONAPO)"))
  })
  message(sprintf("  population: %d alcaldías, CONAPO %d (%s residents); colonias have no official population", nrow(pop), y, fmt_int(sum(pop$population))))
  invisible(TRUE)
}

cdmx_inspect <- function() {
  for (sub in c("carpetas", "colonias")) { cat("\n", sub, ":\n", sep = ""); print(list.files(file.path(CDMX_DIR, sub))) }
  f <- cdmx_newest("carpetas", "\\.(csv|zip)$")
  if (!is.na(f)) { src <- if (grepl("\\.zip$", f)) unz(f, utils::unzip(f, list = TRUE)$Name[1]) else f; print(names(readr::read_csv(src, n_max = 3, col_types = readr::cols(.default = "c"), show_col_types = FALSE))) }
  base <- cdmx_source("ckan_api")
  for (k in c("carpetas_dataset", "colonias_dataset")) { id <- cdmx_source(k); res <- tryCatch(ckan_resources(id, base = base), error = function(e) NULL); cat("\n", k, ":\n", sep = ""); if (!is.null(res)) print(res[, c("name", "format", "modified", "url")]) else cat("(API not readable)\n") }
  invisible(TRUE)
}

register_city(list(
  code = "CMX", key = "cdmx", id = ID_CDMX, name_en = "Mexico City", name_local = "Ciudad de México", lang = "es", country = "MX", after_countries = TRUE,
  measure_kind = "cases", stats_level = "district",
  boundaries_attribution = "Gobierno de la Ciudad de México, Portal de Datos Abiertos (colonias)",
  method = list(where = "Colonia and alcaldía where the facts happened, as recorded in the investigation file", date = "Month the investigation file was opened (fecha de inicio)", revisions = "The cumulative file is republished every month and earlier months change"),
  source = list(name = "Investigation files of the FGJ (carpetas de investigación) by colonia", name_local = "Carpetas de investigación de la FGJ por colonia",
                publisher = "Fiscalía General de Justicia de la Ciudad de México, Portal de Datos Abiertos", url = "https://datos.cdmx.gob.mx/dataset/carpetas-de-investigacion-fgj-de-la-ciudad-de-mexico",
                licence = "Open data of Mexico City (see the terms of the portal)",
                measure = "Investigation files opened by the prosecution service (carpetas de investigación), one per file, by month of opening, colonia and category; they are complaints under investigation, not proven offences, and most are 'low-impact offences'. No official total: the total shown is the sum of the categories."),
  about_source_key = "about_src_cdmx", about_measures_key = "about_cdmx_measures", about_territory_key = "about_cdmx_territory",
  download = cdmx_download, territory = cdmx_territory, ingest = cdmx_ingest, population = cdmx_population
))
