# Switzerland: police-recorded offences by canton -------------------------------------------
# Source: Bundesamt für Statistik (BFS), Polizeiliche Kriminalstatistik (PKS), PX-Web cube
# px-x-1903020100_101 "Polizeilich registrierte Straftaten gemäss Strafgesetzbuch nach Straftat,
# Kanton, Ausführungsgrad, Aufklärungsgrad und Jahr" (2009 onwards, 267 items = StGB articles
# and the 20 title totals, Schweiz + 26 cantons). Licence OPEN-BY (BFS), no key, no registration.
# Only the Strafgesetzbuch cube is in the API: Betäubungsmittelgesetz / Ausländergesetz offences
# are not loaded. Nationwide the finest open level is the canton (municipal open data exist only
# for Kanton Zürich: see the notes). Population: BFS STATPOP cube px-x-0102010000_101 (ständige
# Wohnbevölkerung, 31 December) through the same API. Boundaries: swisstopo swissBOUNDARIES3D
# (open government data, free use with source statement), fallback Eurostat GISCO NUTS 3 (= cantons).
#
# PX-Web: GET <cube>.px gives the metadata (variables and their value codes); a POST with a JSON
# query returns the figures. We ask for JSON-stat 2 and parse it with jsonstat_to_df()
# (R/79_eurostat_rates.R). Written against the cube metadata read on 2026-11-03, not against a
# full download: `ch_inspect()` prints what was downloaded.

CH_STATE <- new.env()
CH_API <- "https://www.pxweb.bfs.admin.ch/api/v1/de"
CH_CUBES <- c(crime = "px-x-1903020100_101", population = "px-x-0102010000_101")
CH_YEARS_PER_CALL <- 5     # PX-Web limits the cells per query: ~25 codes x 27 cantons x 5 years < 5,000
CH_BOUNDARIES <- "https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/swissboundaries3d_2026-01/swissboundaries3d_2026-01_2056_5728.gpkg.zip"

ch_cube_url <- function(id) sprintf("%s/%s/%s.px", CH_API, id, id)
ch_cantons <- function() read_reference("ch_cantons.csv")
ch_file <- function(pattern) { f <- newest_first(list.files(COUNTRY_MODULES$CH$dir_raw, pattern, full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

# Metadata of a cube (variables, codes, texts) from its GET endpoint; versioned like any download
ch_meta <- function(cube) {
  f <- download_versioned(ch_cube_url(CH_CUBES[[cube]]), COUNTRY_MODULES$CH$dir_raw, name = paste0("ch_", cube, "_meta.json"))
  jsonlite::fromJSON(f, simplifyVector = FALSE)
}
ch_var <- function(meta, pattern) {
  i <- which(vapply(meta$variables, function(v) grepl(pattern, v$code, ignore.case = TRUE), logical(1)))
  if (length(i) == 0) stop("Variable '", pattern, "' not found in the BFS cube: ", paste(vapply(meta$variables, `[[`, "", "code"), collapse = " | "))
  meta$variables[[i[1]]]
}

# POST a PX-Web query and keep the JSON-stat 2 answer as a versioned file
ch_query <- function(cube, selections, name) {
  body <- list(query = lapply(names(selections), function(code) list(code = code, selection = list(filter = "item", values = I(as.character(selections[[code]]))))),   # I(): always a JSON array, even for one value
               response = list(format = "json-stat2"))
  tmp <- file.path(tempdir(), paste0("download_", name)); if (file.exists(tmp)) file.remove(tmp)
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  httr2::request(ch_cube_url(CH_CUBES[[cube]])) |> httr2::req_user_agent(USER_AGENT) |> httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_retry(max_tries = 3) |> httr2::req_timeout(300) |> httr2::req_perform(path = tmp)
  if (!file.exists(tmp) || file.size(tmp) == 0) stop("Empty answer from the BFS API for ", name)
  keep_version(tmp, COUNTRY_MODULES$CH$dir_raw, name)
}

ch_download <- function(days = 60) {
  m <- COUNTRY_MODULES$CH
  # A saved answer counts as recent only if it is a JSON-stat dataset (an error page saved by an earlier run is downloaded again)
  is_dataset <- function(f) tryCatch({ js <- jsonlite::fromJSON(f, simplifyVector = FALSE); !is.null(js$id) || !is.null(js$dataset) }, error = function(e) FALSE)
  recent <- function(pattern) { f <- ch_file(pattern); !is.na(f) && difftime(Sys.time(), file.mtime(f), units = "days") < days && is_dataset(f) }
  # 1. Crime: the codes of reference/ch_offence_types.csv, all cantons, totals of Ausführungs-/Aufklärungsgrad, years in chunks
  vals <- function(v) vapply(v$values, as.character, ""); txts <- function(v) vapply(v$valueTexts, as.character, "")
  total_of <- function(v) { x <- vals(v)[grepl("Total", txts(v))]; if (length(x)) x[1] else vals(v)[1] }   # the "Total" code of a variable
  if (recent("_ch_crime_.*\\.json$")) message("= CH crime cube downloaded less than ", days, " days ago")
  else {
    meta <- ch_meta("crime")
    straftat <- ch_var(meta, "^Straftat$"); kanton <- ch_var(meta, "^Kanton"); jahr <- ch_var(meta, "^Jahr$")
    ausf <- ch_var(meta, "^Ausf"); aufkl <- ch_var(meta, "^Aufkl")
    wanted <- read_reference("ch_offence_types.csv")$source_code
    if (length(miss <- setdiff(wanted, vals(straftat)))) message("  codes not in the cube (skipped): ", toString(miss))
    wanted <- intersect(wanted, vals(straftat))
    years <- vals(jahr)
    for (chunk in split(years, ceiling(seq_along(years) / CH_YEARS_PER_CALL))) {
      sel <- setNames(list(wanted, vals(kanton), total_of(ausf), total_of(aufkl), chunk), c(straftat$code, kanton$code, ausf$code, aufkl$code, jahr$code))
      ch_query("crime", sel, sprintf("ch_crime_%s_%s.json", chunk[1], chunk[length(chunk)]))
      Sys.sleep(1)
    }
  }
  # 2. Population: STATPOP, Schweiz (8100) + cantons (texts prefixed "- " in the hierarchical geo variable),
  #    ständige Wohnbevölkerung (Bevölkerungstyp 1), totals of nationality, sex and age
  if (recent("_ch_population\\.json$")) message("= CH population cube downloaded less than ", days, " days ago")
  else {
    meta <- ch_meta("population")
    geo <- ch_var(meta, "Kanton"); jahr <- ch_var(meta, "^Jahr$"); typ <- ch_var(meta, "^Bev"); nat <- ch_var(meta, "Staatsangeh"); sex <- ch_var(meta, "^Geschlecht"); age <- ch_var(meta, "^Alter")
    keep <- vals(geo)[vals(geo) == "8100" | grepl("^- ", txts(geo))]
    permanent <- vals(typ)[grepl("^St[äa]ndige", txts(typ))]; if (length(permanent) == 0) permanent <- vals(typ)[1]
    sel <- setNames(list(keep, vals(jahr), permanent[1], total_of(nat), total_of(sex), total_of(age)), c(geo$code, jahr$code, typ$code, nat$code, sex$code, age$code))
    ch_query("population", sel, "ch_population.json")
  }
  # 3. Boundaries (swisstopo, ~100 MB, once)
  if (is.na(ch_file("swissboundaries3d.*\\.gpkg\\.zip$"))) tryCatch(download_versioned(CH_BOUNDARIES, m$dir_raw), error = function(e) message("  swissBOUNDARIES3D not available (", conditionMessage(e), "); GISCO NUTS 3 will be used"))
}

# Print the files downloaded and the shape of each JSON-stat answer
ch_inspect <- function() {
  for (f in list.files(COUNTRY_MODULES$CH$dir_raw, "\\.json$", full.names = TRUE)) {
    js <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    if (!is.null(js$id)) message(basename(f), ": dimensions ", paste(unlist(js$id), unlist(js$size), sep = "=", collapse = ", "))
    else if (!is.null(js$dataset)) message(basename(f), ": JSON-stat 1.x dataset, dimensions ", paste(unlist(js$dataset$dimension$id), unlist(js$dataset$dimension$size), sep = "=", collapse = ", "))
    else if (!is.null(js$variables)) message(basename(f), ": metadata, variables ", paste(vapply(js$variables, `[[`, "", "code"), collapse = " | "))
    else message(basename(f), ": not recognised; top-level fields: ", paste(utils::head(names(js), 15), collapse = " | "), "\n  first 300 characters: ", substr(paste(readLines(f, n = 3, warn = FALSE), collapse = " "), 1, 300))
  }
}

# JSON-stat answer -> data frame with the dimension matched by `pattern` renamed to `as`
ch_read <- function(f, geo_pattern = "Kanton") {
  d <- jsonstat_to_df(jsonlite::fromJSON(f, simplifyVector = FALSE))
  geo <- grep(geo_pattern, names(d), ignore.case = TRUE, value = TRUE); geo <- geo[!grepl("_label$", geo)]
  if (length(geo) == 0) stop("No canton dimension in ", basename(f), ": ", paste(names(d), collapse = " | "))
  d$geo <- d[[geo[1]]]; d$geo_label <- d[[paste0(geo[1], "_label")]]
  d
}
# Territory id of a canton from the code or the text the cube uses (number, abbreviation or name)
ch_ids <- function(code, label, cantons) {
  by_num <- cantons$abbr[match(code, cantons$bfs_number)]
  by_abbr <- cantons$abbr[match(code, cantons$abbr)]
  by_name <- cantons$abbr[match(normalize_text(sub("^-\\s*", "", label)), normalize_text(cantons$name))]
  abbr <- dplyr::coalesce(by_num, by_abbr, by_name)
  ifelse(code == "8100" | grepl("^Schweiz|^Suisse|^Svizzera", label), "CH", ifelse(is.na(abbr), NA_character_, paste0("CH-", abbr)))
}

ch_ingest <- function(con) {
  files <- newest_first(list.files(COUNTRY_MODULES$CH$dir_raw, "_ch_crime_.*\\.json$", full.names = TRUE))
  files <- files[!duplicated(sub("^.*?_ch_crime_", "", basename(files)))]      # newest version of each year chunk
  if (length(files) == 0) stop("No BFS crime file yet in ", COUNTRY_MODULES$CH$dir_raw)
  cantons <- ch_cantons()
  rows <- lapply(files, function(f) {
    d <- ch_read(f)
    year <- grep("^Jahr$", names(d), ignore.case = TRUE, value = TRUE)[1]; code <- grep("^Straftat$", names(d), ignore.case = TRUE, value = TRUE)[1]
    if (is.na(year) || is.na(code)) stop("Unexpected dimensions in ", basename(f), ": ", paste(names(d), collapse = " | "), "\nRun ch_inspect()")
    data.frame(country = "CH", territory_id = ch_ids(d$geo, d$geo_label, cantons), geo_label = d$geo_label, offence_code = d[[code]], year = as.integer(d[[year]]), n = d$value, file = basename(f), stringsAsFactors = FALSE)
  })
  crime <- do.call(rbind, rows)
  if (any(is.na(crime$territory_id))) { message("  cantons not recognised (dropped): ", toString(unique(crime$geo_label[is.na(crime$territory_id)]))); crime <- crime[!is.na(crime$territory_id), ] }
  crime$geo_label <- NULL
  crime$level <- ifelse(crime$territory_id == "CH", "country", "region")
  crime <- yearly_to_cumulative(crime)
  # The cube has a Schweiz row; if it is missing, the national row is the sum of the cantons
  if (!"CH" %in% crime$territory_id) {
    nat <- crime[crime$level == "region", ] |> dplyr::group_by(country, offence_code, year, quarter, file) |> dplyr::summarise(cumulative = if (all(is.na(cumulative))) NA_real_ else sum(cumulative, na.rm = TRUE), .groups = "drop")
    nat$territory_id <- "CH"; nat$level <- "country"; crime <- rbind(crime, nat[, names(crime)])
  }
  crime <- crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")]
  replace_country_rows(con, "stg_country_crime", "CH", crime)
  log_ingest(con, "country_CH", paste(basename(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  crime: %s rows, %d territories, %d codes, %d-%d", fmt_int(nrow(crime)), length(unique(crime$territory_id)), length(unique(crime$offence_code)), min(crime$year), max(crime$year)))
  invisible(crime)
}

ch_population <- function(con) {
  f <- ch_file("_ch_population\\.json$"); if (is.na(f)) stop("No BFS population file yet")
  d <- ch_read(f, "Kanton")
  year <- grep("^Jahr$", names(d), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(year)) stop("No Jahr dimension in ", basename(f), ": ", paste(names(d), collapse = " | "))
  pop <- data.frame(country = "CH", territory_id = ch_ids(d$geo, d$geo_label, ch_cantons()), year = as.integer(d[[year]]), population = d$value, stringsAsFactors = FALSE)
  pop <- pop[!is.na(pop$territory_id) & !is.na(pop$population), ]
  pop$level <- ifelse(pop$territory_id == "CH", "country", "region")
  pop <- unique(pop[, c("country", "territory_id", "level", "year", "population")])
  replace_country_rows(con, "ref_country_population", "CH", pop)
  message(sprintf("  population: %d territories, %d-%d (BFS STATPOP, 31 December)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

# Cantons from reference/ch_cantons.csv; polygons from swissBOUNDARIES3D, else GISCO NUTS 3
ch_territory <- function(con) {
  cantons <- ch_cantons()
  tbl <- data.frame(territory_id = paste0("CH-", cantons$abbr), level = "region", parent_id = "CH", official_code = cantons$abbr, name = cantons$name)
  save_country_territory(con, "CH", tbl, scheme = "ISO 3166-2")
  g <- NULL
  zip <- ch_file("swissboundaries3d.*\\.gpkg\\.zip$")
  if (!is.na(zip)) g <- tryCatch({
    tmp <- file.path(tempdir(), "swissboundaries3d"); unlink(tmp, recursive = TRUE); utils::unzip(zip, exdir = tmp)
    gp <- list.files(tmp, "\\.gpkg$", full.names = TRUE, recursive = TRUE); if (length(gp) == 0) stop("no .gpkg inside ", basename(zip))
    layers <- sf::st_layers(gp[1])$name; lyr <- grep("kantonsgebiet", layers, ignore.case = TRUE, value = TRUE)
    if (length(lyr) == 0) stop("no Kantonsgebiet layer in ", basename(gp[1]), ": ", toString(layers))
    k <- sf::st_zm(sf::st_read(gp[1], layer = lyr[1], quiet = TRUE), drop = TRUE)
    num <- grep("^kantonsnummer$", names(k), ignore.case = TRUE, value = TRUE)[1]
    if (is.na(num)) stop("no KANTONSNUMMER column: ", paste(names(k), collapse = " | "))
    k$territory_id <- paste0("CH-", cantons$abbr[match(as.integer(k[[num]]), as.integer(cantons$bfs_number))])
    k <- k[!is.na(k$territory_id), ] |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop")
    k$name <- cantons$name[match(sub("^CH-", "", k$territory_id), cantons$abbr)]
    message("  boundaries: swissBOUNDARIES3D (", lyr[1], ", ", nrow(k), " cantons)"); k
  }, error = function(e) { message("  swissBOUNDARIES3D not usable (", conditionMessage(e), "); trying GISCO NUTS 3"); NULL })
  if (is.null(g)) g <- tryCatch({
    n <- gisco_nuts(3); n <- n[n$CNTR_CODE == "CH", ]
    n$territory_id <- paste0("CH-", cantons$abbr[match(n$NUTS_ID, cantons$nuts3)]); n <- n[!is.na(n$territory_id), ]
    n$name <- cantons$name[match(sub("^CH-", "", n$territory_id), cantons$abbr)]
    COUNTRY_MODULES$CH$boundaries_attribution <<- "© EuroGeographics for the administrative boundaries (Eurostat GISCO NUTS 3)"
    n[, c("territory_id", "name", attr(n, "sf_column"))]
  }, error = function(e) { message("  GISCO NUTS not available: cantons without boundaries (", conditionMessage(e), ")"); NULL })
  if (!is.null(g)) save_country_gpkg(con, "CH", list(region = g), tolerance_m = 100)
  invisible(tbl)
}

register_country(list(
  code = "CH", iso3 = "CHE", name_en = "Switzerland", name_local = "Schweiz / Suisse / Svizzera", lang = "de",
  levels = c("region"),
  level_labels = list(region = c("Cantons", "Kantone", "Canton", "Kanton")),
  source = list(name = "Polizeiliche Kriminalstatistik (PKS), BFS", publisher = "Bundesamt für Statistik (BFS)",
                url = "https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-1903020100_101/", licence = "OPEN-BY (BFS: open use, source must be stated)",
                measure = "Offences registered by the police under the Swiss Criminal Code (StGB), completed and attempted, by canton of the offence, yearly (official total: Straftat - Total). Narcotics and immigration-law offences are not in this cube."),
  population_source = "BFS STATPOP, ständige Wohnbevölkerung at 31 December (PX-Web px-x-0102010000_101)",
  boundaries_attribution = "swissBOUNDARIES3D © swisstopo (open government data)",
  not_published = c("Not published by the BFS", "Vom BFS nicht publiziert"),
  notes_en = "Canton level only nationwide (municipal open data exist only for Kanton Zürich). Series breaks from legal changes (e.g. Art. 260ter 2021, Art. 261bis 2020); the 2020 figures reflect the pandemic. Offences are counted where committed.",
  default_region = "CH-BE",
  download = ch_download, ingest = ch_ingest, territory = ch_territory, population = ch_population
))
