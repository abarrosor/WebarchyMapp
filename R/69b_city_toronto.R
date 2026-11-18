# Toronto (TOR): reported major crimes by neighbourhood, yearly ---------------------------------------------------
# City module of the shared city framework (R/60_cities.R). Source: the Toronto Police Service Public Safety Data
# Portal, layer "Neighbourhood Crime Rates Open Data" (ArcGIS FeatureServer): one polygon per neighbourhood (158,
# the City of Toronto's 2021 neighbourhood structure) with, for every year since 2014, the reported count and
# the rate per 100,000 residents of nine categories (assault, auto theft, bicycle theft, break and enter,
# homicide, robbery, shootings and firearm discharges, theft from motor vehicle, theft over $5,000) and the
# population estimate of the latest year. Hierarchy: Toronto (a node under Ontario, CA-35, of the Canada module)
# > one district (the city: the source has no level between the city and the neighbourhoods) > the 158
# neighbourhoods. A yearly source (time_unit = "year"); homicides count in the totals only (sensitive); the
# figures are reported offences as recorded by the TPS (an offence can be counted under one category only).
# Residents: the POPULATION_<year> field of the layer and, for the other years, the population implied by the
# rates (count x 100,000 / rate), so the rates of the app reproduce the portal's own figures.
#
# Written against the layer's published fields (checked 2026-11-10: AREA_NAME, HOOD_ID, <CATEGORY>_<year>,
# <CATEGORY>_RATE_<year>, POPULATION_2025, geometry) but not run against the live service: `toronto_inspect()`
# prints what was downloaded.

ID_TORONTO <- "CA-35-TORONTO"
TOR_DIR <- file.path(CONFIG$dir_raw, "city_toronto")
TOR_CATEGORIES <- c("ASSAULT", "AUTOTHEFT", "BIKETHEFT", "BREAKENTER", "HOMICIDE", "ROBBERY", "SHOOTING", "THEFTFROMMV", "THEFTOVER")
tor_sources <- function() read_reference("toronto_sources.csv")
tor_source <- function(key) { s <- tor_sources(); u <- s$url[s$key == key]; if (length(u) && !is.na(u) && nzchar(trimws(u))) trimws(u) else NA_character_ }
tor_dir <- function(sub) { d <- file.path(TOR_DIR, sub); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
tor_district_id <- function() paste0(ID_TORONTO, "-D00")

# --- Download: the layer as GeoJSON, in pages of 100 features (the service caps a query at 1,000-2,000) -----------
tor_pages <- function() {   # the newest copy of every page (a page whose content did not change keeps its older date)
  f <- list.files(file.path(TOR_DIR, "layer"), "_crime_rates_p[0-9]+\\.geojson$", full.names = TRUE); if (!length(f)) return(character())
  page <- sub("^.*_crime_rates_(p[0-9]+)\\.geojson$", "\\1", basename(f)); unname(vapply(split(f, page), function(x) newest_first(x)[1], ""))
}
tor_download <- function(days = 60, page = 100L) {
  prev <- tor_pages()
  if (length(prev) && file_age_days(prev[1]) < days) { message("= Toronto layer downloaded less than ", days, " days ago (", length(prev), " pages)"); return(invisible(prev)) }
  base <- tor_source("layer_query"); if (is.na(base)) stop("no layer_query in reference/toronto_sources.csv")
  got <- character(); offset <- 0L; first_ids <- character()
  repeat {
    url <- paste0(base, "&resultOffset=", offset, "&resultRecordCount=", page)
    tmp <- file.path(tempdir(), sprintf("download_toronto_p%02d.geojson", length(got) + 1L)); if (file.exists(tmp)) file.remove(tmp)
    download_safely(url, tmp)
    js <- tryCatch(jsonlite::fromJSON(tmp, simplifyVector = FALSE), error = function(e) NULL)
    feats <- js$features; n <- length(feats)
    if (n == 0) { if (!length(got)) stop("the service answered no feature (", url, "): run toronto_inspect() after checking reference/toronto_sources.csv"); break }
    fid <- as.character(feats[[1]]$properties$HOOD_ID %||% feats[[1]]$properties$OBJECTID_1 %||% feats[[1]]$id %||% offset)
    if (fid %in% first_ids) { message("  the service ignores resultOffset (same page again): keeping what was downloaded"); break }   # no pagination: one file holds all
    first_ids <- c(first_ids, fid)
    got <- c(got, keep_version(tmp, tor_dir("layer"), sprintf("crime_rates_p%02d.geojson", length(got) + 1L)))
    if (n < page) break
    offset <- offset + page; if (offset > 5000L) break
  }
  invisible(got)
}

tor_read_layer <- function() {
  files <- tor_pages(); if (!length(files)) stop("no Toronto layer in ", file.path(TOR_DIR, "layer"), " (option 14 with TOR)")
  parts <- lapply(files, function(f) { g <- sf::st_read(f, quiet = TRUE); if (nrow(g)) g else NULL })
  g <- do.call(rbind, Filter(Negate(is.null), parts))
  if (is.null(g) || nrow(g) == 0) stop("the Toronto layer pages hold no feature: run toronto_inspect()")
  nm <- names(g); id_col <- nm[grepl("^HOOD_ID$|^HOOD_158$", nm, ignore.case = TRUE)][1]; name_col <- nm[grepl("^AREA_NAME$|^NEIGHBOURHOOD", nm, ignore.case = TRUE)][1]
  if (is.na(id_col) || is.na(name_col)) stop("Toronto layer: no HOOD_ID / AREA_NAME fields; fields are ", paste(nm, collapse = ", "))
  g$hood_id <- sprintf("%03d", as.integer(g[[id_col]])); g$hood_name <- as.character(g[[name_col]])
  g <- g[!duplicated(g$hood_id), ]
  g
}

# --- Territory: the 158 neighbourhoods under one district -------------------------------------------------------
tor_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_TORONTO) > 1 && file.exists(CITY_MODULES$toronto$gpkg)) { message("= Toronto territory kept (", city_neighbourhood_count(con, ID_TORONTO), " neighbourhoods)"); return(invisible(FALSE)) }
  g <- tor_read_layer(); g <- sf::st_make_valid(sf::st_transform(g, 4326))
  district_id <- tor_district_id(); f <- basename(tor_pages()[1])
  neighbourhoods <- sf::st_sf(territory_id = paste0(district_id, "-B", g$hood_id), level = "neighbourhood", parent_id = district_id, country = "CA", official_code = g$hood_id, code_scheme = "City of Toronto neighbourhood (2021)",
                              name = g$hood_name, area_m2 = as.numeric(sf::st_area(g)), file = f, geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, rep(district_id, nrow(g)))
  districts <- sf::st_sf(territory_id = district_id, level = "district", parent_id = ID_TORONTO, country = "CA", official_code = "3520005", code_scheme = "Statistics Canada CSD",
                         name = "Toronto", area_m2 = as.numeric(sf::st_area(dis)), file = f, geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = ID_TORONTO, level = "municipality", parent_id = "CA-35", country = "CA", official_code = "3520005", code_scheme = "Statistics Canada CSD", name = "Toronto", stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES$toronto$gpkg)
  invisible(neighbourhoods)
}

# --- Ingest: the <CATEGORY>_<year> fields to year x neighbourhood x category --------------------------------------
tor_long <- function(g) {
  d <- sf::st_drop_geometry(g); nm <- names(d)
  cnt <- nm[grepl(paste0("^(", paste(TOR_CATEGORIES, collapse = "|"), ")_(20[0-9]{2})$"), nm)]
  if (!length(cnt)) stop("Toronto layer: no <CATEGORY>_<year> count fields; fields are ", paste(utils::head(nm, 40), collapse = ", "))
  rows <- lapply(cnt, function(k) { cat <- sub("_20[0-9]{2}$", "", k); year <- as.integer(sub("^.*_(20[0-9]{2})$", "\\1", k)); rate_col <- paste0(cat, "_RATE_", year)
    data.frame(hood_id = d$hood_id, year = year, event_type = cat, n = suppressWarnings(as.numeric(d[[k]])),
               rate = if (rate_col %in% nm) suppressWarnings(as.numeric(d[[rate_col]])) else NA_real_, stringsAsFactors = FALSE) })
  out <- do.call(rbind, rows)
  pop_cols <- nm[grepl("^POPULATION_(20[0-9]{2})$", nm)]
  out$pop_field <- NA_real_
  for (k in pop_cols) { y <- as.integer(sub("^POPULATION_", "", k)); out$pop_field[out$year == y] <- suppressWarnings(as.numeric(d[[k]]))[match(out$hood_id[out$year == y], d$hood_id)] }
  out
}

tor_ingest <- function(con) {
  g <- tor_read_layer(); d <- tor_long(g); d <- d[!is.na(d$n), ]
  types <- read_reference("toronto_event_types.csv", bool_cols = "sensitive")
  district_id <- tor_district_id()
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND parent_id = ", DBI::dbQuoteString(con, district_id)))$territory_id
  d$neighbourhood_id <- paste0(district_id, "-B", d$hood_id); d$neighbourhood_id[!d$neighbourhood_id %in% known] <- NA
  ev <- d |> dplyr::group_by(year, neighbourhood_id, event_type) |> dplyr::summarise(events = sum(n), .groups = "drop") |> as.data.frame()
  f <- basename(tor_pages()[1])
  ev$month <- as.Date(sprintf("%d-01-01", ev$year)); ev$district_id <- district_id; ev$city <- "toronto"; ev$file <- f; ev$events <- as.integer(round(ev$events))
  replace_city_rows(con, "stg_city_events", "toronto", ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  seen <- unique(ev$event_type); knownt <- types[types$source_code %in% seen, ]; extra <- setdiff(seen, knownt$source_code)
  t <- rbind(data.frame(city = "toronto", event_type = knownt$source_code, block = knownt$block, name_local = knownt$name_local, name_en = knownt$name_en, sensitive = knownt$sensitive, stringsAsFactors = FALSE),
             if (length(extra)) data.frame(city = "toronto", event_type = extra, block = "public_safety", name_local = extra, name_en = extra, sensitive = grepl("HOMICIDE|SEX", extra), stringsAsFactors = FALSE))
  if (length(extra)) message("  categories not in toronto_event_types.csv (kept under their own name): ", paste(extra, collapse = " / "))
  replace_city_rows(con, "ref_city_event_types", "toronto", t)
  # population: the POPULATION_<year> field where it exists, else implied by the assault rate (count x 100,000 / rate)
  p <- d[!is.na(d$neighbourhood_id), ]
  implied <- p[p$event_type == "ASSAULT" & !is.na(p$rate) & p$rate > 0 & p$n > 0, ]; implied$pop <- implied$n * 1e5 / implied$rate
  given <- unique(p[!is.na(p$pop_field), c("year", "neighbourhood_id", "pop_field")]); names(given)[3] <- "pop"
  pop <- rbind(given[, c("year", "neighbourhood_id", "pop")], implied[!paste(implied$year, implied$neighbourhood_id) %in% paste(given$year, given$neighbourhood_id), c("year", "neighbourhood_id", "pop")])
  TOR_STATE$pop <- pop; TOR_STATE$file <- f
  log_ingest(con, "city_toronto", f, NA, paste(range(ev$year), collapse = "-"), nrow(ev), sum(ev$events))
  message(sprintf("  toronto: %s rows, %d-%d, %d neighbourhoods, %d categories (yearly; %d shown by area)", fmt_int(nrow(ev)), min(ev$year), max(ev$year), length(unique(stats::na.omit(ev$neighbourhood_id))), nrow(t), sum(t$block %in% MARTS$blocks & !t$sensitive)))
  invisible(ev)
}
TOR_STATE <- new.env()

tor_population <- function(con) {
  pop <- TOR_STATE$pop
  if (is.null(pop) || nrow(pop) == 0) { g <- tor_read_layer(); d <- tor_long(g); district_id <- tor_district_id(); d$neighbourhood_id <- paste0(district_id, "-B", d$hood_id)
    given <- unique(d[!is.na(d$pop_field), c("year", "neighbourhood_id", "pop_field")]); names(given)[3] <- "pop"
    implied <- d[d$event_type == "ASSAULT" & !is.na(d$rate) & d$rate > 0 & !is.na(d$n) & d$n > 0, ]; implied$pop <- implied$n * 1e5 / implied$rate
    pop <- rbind(given, implied[!paste(implied$year, implied$neighbourhood_id) %in% paste(given$year, given$neighbourhood_id), c("year", "neighbourhood_id", "pop")]); TOR_STATE$file <- basename(tor_pages()[1]) }
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND parent_id = ", DBI::dbQuoteString(con, tor_district_id())))$territory_id
  pop <- pop[pop$neighbourhood_id %in% known & !is.na(pop$pop) & pop$pop > 0, ]; pop <- pop[!duplicated(pop[, c("year", "neighbourhood_id")]), ]
  if (nrow(pop) == 0) stop("no population in the Toronto layer (POPULATION_<year> fields or rates): counts only, no rates")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(tor_district_id(), "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = pop$neighbourhood_id, ref_date = as.Date(sprintf("%d-01-01", pop$year)), population = as.integer(round(pop$pop)), file = TOR_STATE$file %||% "layer"))
  })
  y <- max(pop$year)
  message(sprintf("  population: %d neighbourhoods, %d-%d (%s residents in %d)", length(unique(pop$neighbourhood_id)), min(pop$year), y, fmt_int(sum(pop$pop[pop$year == y])), y))
  invisible(TRUE)
}

toronto_inspect <- function() {
  cat("\nlayer pages:\n"); print(list.files(file.path(TOR_DIR, "layer")))
  f <- tor_pages()
  if (length(f)) { g <- tryCatch(sf::st_read(f[1], quiet = TRUE), error = function(e) NULL)
    if (!is.null(g)) { cat("\n", basename(f[1]), ": ", nrow(g), " features; fields: ", paste(utils::head(names(g), 60), collapse = ", "), if (ncol(g) > 60) " ..." else "", "\n", sep = "") } else cat("\nfirst page not readable; first line:", readLines(f[1], n = 1, warn = FALSE), "\n") }
  invisible(TRUE)
}

register_city(list(
  code = "TOR", key = "toronto", id = ID_TORONTO, name_en = "Toronto", name_local = "Toronto", lang = "en", country = "CA", after_countries = TRUE,
  time_unit = "year", measure_kind = "offences", stats_level = "neighbourhood",
  boundaries_attribution = "Toronto Police Service / City of Toronto neighbourhoods (Open Government Licence – Ontario)",
  method = list(where = "Neighbourhood where the offence occurred (City of Toronto 2021 neighbourhood structure), as geocoded by the Toronto Police Service", date = "Year the offence occurred (occurrence date)", revisions = "The layer is republished through the year; counts of recent years change as reports are updated"),
  source = list(name = "Reported major crimes by neighbourhood (Toronto Police Service, Neighbourhood Crime Rates)", name_local = "Neighbourhood Crime Rates Open Data (Toronto Police Service)",
                publisher = "Toronto Police Service, Public Safety Data Portal", url = "https://data.tps.ca/",
                licence = "Open Government Licence – Ontario",
                measure = "Offences reported to the Toronto Police Service, by year of occurrence, for nine categories (assault, auto theft, bicycle theft, break and enter, homicide, robbery, shootings and firearm discharges, theft from motor vehicle, theft over $5,000); yearly figures with the portal's own population estimates. Homicides count in the totals only."),
  about_source_key = "about_src_toronto", about_measures_key = "about_toronto_measures", about_territory_key = "about_toronto_territory",
  download = tor_download, territory = tor_territory, ingest = tor_ingest, population = tor_population
))
