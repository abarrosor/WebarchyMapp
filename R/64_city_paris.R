# Paris (PAR): recorded crime by arrondissement (SSMSI communal base), yearly -----------------------------
# City module of the shared city framework (R/60_cities.R), built on the France country module (R/71_fr.R):
# the SSMSI "base statistique communale de la délinquance enregistrée" lists the 20 arrondissements of Paris
# as communes (INSEE codes 75101 to 75120), with their INSEE population. Nothing new is downloaded for the
# figures; the arrondissement polygons come from Paris Open Data (dataset "arrondissements", ODbL).
#
# A yearly source (time_unit = "year"): stg_city_events.month is 1 January of each year and the app shows
# years. The hierarchy is city > one district (Paris itself: the source has no level between the city and the
# arrondissements) > the 20 arrondissements as neighbourhoods. The 15 SSMSI indicators are the event types;
# the counting unit differs by indicator (offences, victims, vehicles, persons implicated) and is stated in
# the type names; an indicator is published for an arrondissement only when it exceeds the SSMSI diffusion
# threshold ("est_diffuse"), which every arrondissement does. Rules as in every city: the threshold is applied
# in the app, never compared with another city.

ID_PARIS <- "FR-11-75-75056"
PARIS_DISTRICT <- paste0(ID_PARIS, "-D00")
PARIS_ARR <- sprintf("751%02d", 1:20)
paris_arr_id <- function(insee) paste0(PARIS_DISTRICT, "-B", insee)
PARIS_BOUNDARIES <- list(
  url = "https://opendata.paris.fr/api/explore/v2.1/catalog/datasets/arrondissements/exports/geojson",
  dir_raw = file.path(CONFIG$dir_raw, "city_paris", "boundaries"), name = "paris_arrondissements.geojson",
  page = "https://opendata.paris.fr/explore/dataset/arrondissements/")

paris_download <- function() {
  prev <- newest_first(list.files(PARIS_BOUNDARIES$dir_raw, "paris_arrondissements\\.geojson$", full.names = TRUE))
  if (length(prev)) { message("= Paris arrondissement boundaries already downloaded (", basename(prev[1]), ")"); return(invisible(prev[1])) }
  download_versioned(PARIS_BOUNDARIES$url, PARIS_BOUNDARIES$dir_raw, name = PARIS_BOUNDARIES$name)
}

paris_boundaries_file <- function() { f <- newest_first(list.files(PARIS_BOUNDARIES$dir_raw, "paris_arrondissements\\.geojson$", full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

# One district (Paris) and the 20 arrondissements, from the Paris Open Data GeoJSON (fields c_arinsee, l_ar, l_aroff)
paris_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_PARIS) > 0 && file.exists(CITY_MODULES$paris$gpkg)) { message("= Paris territory kept (", city_neighbourhood_count(con, ID_PARIS), " arrondissements)"); return(invisible(FALSE)) }
  f <- paris_boundaries_file(); if (is.na(f)) stop("no arrondissement boundaries in ", PARIS_BOUNDARIES$dir_raw, " (download: option 14 with PAR)")
  g <- sf::st_read(f, quiet = TRUE); nm <- names(g)
  pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col_insee <- pick("^c_arinsee$|insee"); col_num <- pick("^c_ar$"); col_name <- pick("^l_ar$|^l_aroff$|^nom")
  if (is.na(col_insee) && is.na(col_num)) stop("Paris boundaries: no arrondissement code column; fields are ", paste(nm, collapse = ", "))
  insee <- if (!is.na(col_insee)) sprintf("%05d", as.integer(g[[col_insee]])) else sprintf("751%02d", as.integer(g[[col_num]]))
  g <- g[insee %in% PARIS_ARR, ]; insee <- insee[insee %in% PARIS_ARR]
  if (nrow(g) == 0) stop("Paris boundaries: no arrondissement with an INSEE code 75101-75120")
  num <- as.integer(substr(insee, 4, 5))
  label <- if (!is.na(col_name)) as.character(g[[col_name]]) else NA_character_
  name <- ifelse(is.na(label) | !nzchar(label), ifelse(num == 1, "1er arrondissement", sprintf("%de arrondissement", num)), label)
  g <- sf::st_make_valid(sf::st_transform(g, 4326))
  neighbourhoods <- sf::st_sf(
    territory_id = paris_arr_id(insee), level = "neighbourhood", parent_id = PARIS_DISTRICT, country = "FR",
    official_code = insee, code_scheme = "INSEE", name = name, area_m2 = as.numeric(sf::st_area(g)), file = basename(f), geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, rep(PARIS_DISTRICT, nrow(g)))
  districts <- sf::st_sf(territory_id = PARIS_DISTRICT, level = "district", parent_id = ID_PARIS, country = "FR", official_code = "75056", code_scheme = "INSEE",
                         name = "Paris", area_m2 = as.numeric(sf::st_area(dis)), file = basename(f), geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = ID_PARIS, level = "municipality", parent_id = "FR-11-75", country = "FR", official_code = "75056", code_scheme = "INSEE", name = "Paris", stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES$paris$gpkg)
}

# The arrondissement rows of the SSMSI communal base (R/71_fr.R readers), standardised: code, year, indicator, value, pop
paris_rows <- function() {
  f <- fr_file("communal"); if (is.na(f)) stop("no SSMSI communal base in ", COUNTRY_MODULES$FR$dir_raw, " (the France module downloads it: option 14 with FR)")
  d <- fr_standardise(fr_read(f), "communal")
  d <- d[d$code %in% PARIS_ARR & d$indicator != "", ]
  if (nrow(d) == 0) stop("no arrondissement of Paris (75101-75120) in ", basename(f), ": the communal base may list Paris as 75056 only")
  d$file <- basename(f); d
}

paris_ingest <- function(con) {
  d <- paris_rows()
  types <- read_reference("fr_offence_types.csv", bool_cols = "sensitive"); types <- types[types$offence_type_id != "total", ]
  key <- normalize_text(types$source_code)
  d$event_type <- types$source_code[match(normalize_text(d$indicator), key)]
  unknown <- unique(d$indicator[is.na(d$event_type)]); if (length(unknown)) message("  indicators not in fr_offence_types.csv (left out): ", paste(unknown, collapse = " / "))
  d <- d[!is.na(d$event_type) & !is.na(d$value), ]
  ev <- d |> dplyr::group_by(year, code, event_type, file) |> dplyr::summarise(events = sum(value), .groups = "drop") |> as.data.frame()
  ev$month <- as.Date(sprintf("%d-01-01", ev$year)); ev$neighbourhood_id <- paris_arr_id(ev$code); ev$district_id <- PARIS_DISTRICT
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND parent_id = ", DBI::dbQuoteString(con, PARIS_DISTRICT)))$territory_id
  ev$neighbourhood_id[!ev$neighbourhood_id %in% known] <- NA
  ev$city <- "paris"; ev$events <- as.integer(round(ev$events))
  replace_city_rows(con, "stg_city_events", "paris", ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  seen <- types[types$source_code %in% ev$event_type, ]
  t <- data.frame(city = "paris", event_type = seen$source_code, block = "public_safety", name_local = seen$name_local, name_en = seen$name_en, sensitive = seen$sensitive, stringsAsFactors = FALSE)
  replace_city_rows(con, "ref_city_event_types", "paris", t)
  PARIS_STATE$pop <- unique(d[!is.na(d$pop), c("code", "year", "pop")])
  log_ingest(con, "city_paris", unique(ev$file)[1], NA, paste(range(ev$year), collapse = "-"), nrow(ev), sum(ev$events))
  message(sprintf("  paris: %s rows, %d-%d, %d arrondissements, %d indicators (yearly)", fmt_int(nrow(ev)), min(ev$year), max(ev$year), length(unique(stats::na.omit(ev$neighbourhood_id))), nrow(t)))
  invisible(ev)
}
PARIS_STATE <- new.env()

# INSEE population of each arrondissement, carried inside the SSMSI file (latest year)
paris_population <- function(con) {
  pop <- PARIS_STATE$pop; if (is.null(pop)) { d <- paris_rows(); pop <- unique(d[!is.na(d$pop), c("code", "year", "pop")]) }
  if (nrow(pop) == 0) stop("no population column in the SSMSI communal base")
  y <- max(pop$year); pop <- pop[pop$year == y, ]; pop <- pop[!duplicated(pop$code), ]
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND parent_id = ", DBI::dbQuoteString(con, PARIS_DISTRICT)))$territory_id
  ids <- paris_arr_id(pop$code); keep <- ids %in% known
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(PARIS_DISTRICT, "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = ids[keep], ref_date = as.Date(sprintf("%d-01-01", y)), population = as.integer(round(pop$pop[keep])), file = "SSMSI communal base (insee_pop)"))
  })
  message(sprintf("  population: %d arrondissements, INSEE %d (%s residents)", sum(keep), y, fmt_int(sum(pop$pop[keep]))))
  invisible(TRUE)
}

register_city(list(
  code = "PAR", key = "paris", id = ID_PARIS, name_en = "Paris", name_local = "Paris", lang = "fr", country = "FR", after_countries = TRUE,
  time_unit = "year", measure_kind = "offences", stats_level = "neighbourhood",
  boundaries_attribution = "Ville de Paris, Paris Open Data (ODbL)",
  method = list(where = "Arrondissement where the offence was recorded (commune of the SSMSI base)", date = "Year the offence was recorded by the police or gendarmerie", revisions = "The whole base is republished every year; an indicator is shown only above the SSMSI diffusion threshold"),
  source = list(name = "Recorded crime by arrondissement (SSMSI communal base)", name_local = "Délinquance enregistrée par arrondissement (base communale SSMSI)",
                publisher = "Ministère de l'Intérieur, SSMSI (data.gouv.fr)", url = "https://www.data.gouv.fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales",
                licence = "Licence Ouverte 2.0",
                measure = "Offences recorded by the police and gendarmerie, yearly, for 15 indicators whose counting unit differs (offences, victims for violence, vehicles for vehicle theft, persons implicated for drug use); no official total (the total shown is their sum). Yearly figures only."),
  about_source_key = "about_src_paris", about_measures_key = "about_paris_measures", about_territory_key = "about_paris_territory",
  download = paris_download, territory = paris_territory, ingest = paris_ingest, population = paris_population
))
