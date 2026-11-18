# French cities with arrondissements (Paris, Lyon, Marseille): recorded crime by arrondissement, yearly ------
# City modules of the shared city framework (R/60_cities.R), built on the France country module (R/71_fr.R):
# the SSMSI "base statistique communale de la délinquance enregistrée" lists the arrondissements of Paris
# (INSEE 75101-75120), Lyon (69381-69389) and Marseille (13201-13216) as communes, with their INSEE
# population. Nothing new is downloaded for the figures; the arrondissement polygons come from the French
# geo API (geo.api.gouv.fr, Etalab open licence), which also carries the legal population.
#
# `fr_city_module(code, key, insee, name, arrondissements, id)` describes one city; the three are registered at
# the end of the file. A yearly source (time_unit = "year"): stg_city_events.month is 1 January of each year
# and the app shows years. Hierarchy: city > one district (the city itself: the source has no level between
# the city and the arrondissements) > the arrondissements as neighbourhoods. The 15 SSMSI indicators are
# the event types; their counting unit differs (offences, victims, vehicles, persons implicated) and is
# stated in the type names; an indicator is published only above the SSMSI diffusion threshold.
# Rules as in every city: the threshold is applied in the app, never compared with another city.

FR_CITY_API <- "https://geo.api.gouv.fr/communes?codeParent=%s&type=arrondissement-municipal&format=geojson&geometry=contour"
FR_CITY_PAGE <- "https://geo.api.gouv.fr/decoupage-administratif/communes"

fr_city_dir <- function(key) file.path(CONFIG$dir_raw, paste0("city_", key), "boundaries")
fr_city_boundaries_file <- function(m) { f <- newest_first(list.files(fr_city_dir(m$key), paste0("_arrondissements_", m$insee, "\\.geojson$"), full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

fr_city_download <- function(m) {
  f <- fr_city_boundaries_file(m)
  if (!is.na(f) && file_age_days(f) < 365) { message("= ", m$name_en, " arrondissement boundaries already downloaded (", basename(f), ")"); return(invisible(f)) }
  download_versioned(sprintf(FR_CITY_API, m$insee), fr_city_dir(m$key), name = paste0("arrondissements_", m$insee, ".geojson"))
}

# One district (the city) and its arrondissements from the geo API (properties code, nom, population)
fr_city_territory <- function(m, con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, m$id) > 1 && file.exists(CITY_MODULES[[m$key]]$gpkg)) { message("= ", m$name_en, " territory kept (", city_neighbourhood_count(con, m$id), " arrondissements)"); return(invisible(FALSE)) }
  f <- fr_city_boundaries_file(m); if (is.na(f)) stop("no arrondissement boundaries in ", fr_city_dir(m$key), " (download: option 14 with ", m$code, ")")
  g <- sf::st_read(f, quiet = TRUE); nm <- names(g)
  pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col_code <- pick("^code$|^c_arinsee$|insee"); col_name <- pick("^nom$|^l_ar$|^name"); col_pop <- pick("^population$")
  if (is.na(col_code)) stop(m$name_en, " boundaries: no code column; fields are ", paste(nm, collapse = ", "))
  insee <- sprintf("%05d", as.integer(g[[col_code]]))
  g <- g[insee %in% m$arrondissements, ]; insee <- insee[insee %in% m$arrondissements]
  if (nrow(g) == 0) stop(m$name_en, " boundaries: none of the expected arrondissement codes (", m$arrondissements[1], "...) in ", basename(f))
  num <- as.integer(insee) - as.integer(m$arrondissements[1]) + 1L
  name <- if (!is.na(col_name)) as.character(g[[col_name]]) else ifelse(num == 1, "1er arrondissement", sprintf("%de arrondissement", num))
  name <- sub(paste0("^", m$name_local, "\\s+"), "", name)             # "Lyon 1er Arrondissement" -> "1er Arrondissement"
  g <- sf::st_make_valid(sf::st_transform(g, 4326))
  district_id <- paste0(m$id, "-D00")
  neighbourhoods <- sf::st_sf(territory_id = paste0(district_id, "-B", insee), level = "neighbourhood", parent_id = district_id, country = "FR",
                              official_code = insee, code_scheme = "INSEE", name = name, area_m2 = as.numeric(sf::st_area(g)), file = basename(f), geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, rep(district_id, nrow(g)))
  districts <- sf::st_sf(territory_id = district_id, level = "district", parent_id = m$id, country = "FR", official_code = m$insee, code_scheme = "INSEE",
                         name = m$name_local, area_m2 = as.numeric(sf::st_area(dis)), file = basename(f), geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = m$id, level = "municipality", parent_id = sub("-[^-]+$", "", m$id), country = "FR", official_code = m$insee, code_scheme = "INSEE", name = m$name_local, stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES[[m$key]]$gpkg)
  FR_CITY_STATE[[m$key]] <- list(pop = if (!is.na(col_pop)) data.frame(code = insee, population = suppressWarnings(as.numeric(g[[col_pop]])), stringsAsFactors = FALSE) else NULL, file = basename(f))
  invisible(neighbourhoods)
}
FR_CITY_STATE <- new.env()

# The arrondissement rows of the SSMSI communal base (R/71_fr.R readers): code, year, indicator, value, pop
fr_city_rows <- function(m) {
  f <- fr_file("communal"); if (is.na(f)) stop("no SSMSI communal base in ", COUNTRY_MODULES$FR$dir_raw, " (the France module downloads it: option 14 with FR)")
  d <- fr_standardise(fr_read(f), "communal")
  d <- d[d$code %in% m$arrondissements & d$indicator != "", ]
  if (nrow(d) == 0) stop("no arrondissement of ", m$name_en, " (", m$arrondissements[1], "...) in ", basename(f), ": the communal base may list the city as ", m$insee, " only")
  d$file <- basename(f); d
}

fr_city_ingest <- function(m, con) {
  d <- fr_city_rows(m)
  types <- read_reference("fr_offence_types.csv", bool_cols = "sensitive"); types <- types[types$offence_type_id != "total", ]
  d$event_type <- types$source_code[match(normalize_text(d$indicator), normalize_text(types$source_code))]
  unknown <- unique(d$indicator[is.na(d$event_type)]); if (length(unknown)) message("  indicators not in fr_offence_types.csv (left out): ", paste(unknown, collapse = " / "))
  d <- d[!is.na(d$event_type) & !is.na(d$value), ]
  ev <- d |> dplyr::group_by(year, code, event_type, file) |> dplyr::summarise(events = sum(value), .groups = "drop") |> as.data.frame()
  district_id <- paste0(m$id, "-D00")
  ev$month <- as.Date(sprintf("%d-01-01", ev$year)); ev$neighbourhood_id <- paste0(district_id, "-B", ev$code); ev$district_id <- district_id
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND parent_id = ", DBI::dbQuoteString(con, district_id)))$territory_id
  ev$neighbourhood_id[!ev$neighbourhood_id %in% known] <- NA
  ev$city <- m$key; ev$events <- as.integer(round(ev$events))
  replace_city_rows(con, "stg_city_events", m$key, ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  seen <- types[types$source_code %in% ev$event_type, ]
  t <- data.frame(city = m$key, event_type = seen$source_code, block = "public_safety", name_local = seen$name_local, name_en = seen$name_en, sensitive = seen$sensitive, stringsAsFactors = FALSE)
  replace_city_rows(con, "ref_city_event_types", m$key, t)
  FR_CITY_STATE[[paste0(m$key, "_pop")]] <- unique(d[!is.na(d$pop), c("code", "year", "pop")])
  log_ingest(con, paste0("city_", m$key), unique(ev$file)[1], NA, paste(range(ev$year), collapse = "-"), nrow(ev), sum(ev$events))
  message(sprintf("  %s: %s rows, %d-%d, %d arrondissements, %d indicators (yearly)", m$key, fmt_int(nrow(ev)), min(ev$year), max(ev$year), length(unique(stats::na.omit(ev$neighbourhood_id))), nrow(t)))
  invisible(ev)
}

# INSEE population of each arrondissement: the one the SSMSI file gives for every year (one register per year: the
# rate of a year stands on the population the source itself sets beside it); the geo API's legal population as the fallback
fr_city_population <- function(m, con) {
  district_id <- paste0(m$id, "-D00")
  pop <- FR_CITY_STATE[[paste0(m$key, "_pop")]]; if (is.null(pop)) { d <- fr_city_rows(m); pop <- unique(d[!is.na(d$pop), c("code", "year", "pop")]) }
  src <- "SSMSI communal base (insee_pop)"
  if (nrow(pop) == 0) { st <- FR_CITY_STATE[[m$key]]; if (!is.null(st$pop)) { pop <- data.frame(code = st$pop$code, year = as.integer(format(Sys.Date(), "%Y")), pop = st$pop$population); src <- paste0(st$file, " (geo.api.gouv.fr, population légale)") } }
  if (nrow(pop) == 0) stop("no population for the arrondissements of ", m$name_en)
  pop <- pop[!is.na(pop$pop) & !is.na(pop$year), ]; pop <- pop[!duplicated(pop[, c("code", "year")]), ]
  if (nrow(pop) == 0) stop("no population for the arrondissements of ", m$name_en)
  y <- max(pop$year)
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND parent_id = ", DBI::dbQuoteString(con, district_id)))$territory_id
  ids <- paste0(district_id, "-B", pop$code); keep <- ids %in% known
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(district_id, "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = ids[keep], ref_date = as.Date(sprintf("%d-01-01", pop$year[keep])), population = as.integer(round(pop$pop[keep])), file = src))
  })
  yrs <- sort(unique(pop$year[keep]))
  message(sprintf("  population: %d arrondissements, INSEE %s (%s residents in %d)", length(unique(ids[keep])), if (length(yrs) > 1) paste0(min(yrs), "-", y) else y, fmt_int(sum(pop$pop[keep & pop$year == y])), y))
  invisible(TRUE)
}

# code: menu code; key: folder and table key; insee: INSEE code of the commune; arrondissements: their INSEE codes;
# id: the municipality id of the France module (FR-<region>-<department>-<insee>)
fr_city_module <- function(code, key, insee, name, arrondissements, id, lang = "fr") {
  m <- list(code = code, key = key, insee = insee, id = id, name_en = name, name_local = name, lang = lang, country = "FR", after_countries = TRUE,
            arrondissements = arrondissements, time_unit = "year", measure_kind = "offences", stats_level = "neighbourhood",
            boundaries_attribution = "geo.api.gouv.fr (Etalab open licence)",
            method = list(where = "Arrondissement where the offence was recorded (commune of the SSMSI base)", date = "Year the offence was recorded by the police or gendarmerie", revisions = "The whole base is republished every year; an indicator is shown only above the SSMSI diffusion threshold"),
            source = list(name = "Recorded crime by arrondissement (SSMSI communal base)", name_local = "Délinquance enregistrée par arrondissement (base communale SSMSI)",
                          publisher = "Ministère de l'Intérieur, SSMSI (data.gouv.fr)", url = "https://www.data.gouv.fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales",
                          licence = "Licence Ouverte 2.0",
                          measure = "Offences recorded by the police and gendarmerie, yearly, for 15 indicators whose counting unit differs (offences, victims for violence, vehicles for vehicle theft, persons implicated for drug use); no official total (the total shown is their sum). Yearly figures only."),
            about_source_key = paste0("about_src_", key), about_measures_key = paste0("about_", key, "_measures"), about_territory_key = paste0("about_", key, "_territory"))
  m$download <- function() fr_city_download(m)
  m$territory <- function(con, force = FALSE) fr_city_territory(m, con, force)
  m$ingest <- function(con) fr_city_ingest(m, con)
  m$population <- function(con) fr_city_population(m, con)
  m
}

register_city(fr_city_module("PAR", "paris", "75056", "Paris", sprintf("751%02d", 1:20), "FR-11-75-75056"))
register_city(fr_city_module("LYO", "lyon", "69123", "Lyon", sprintf("6938%d", 1:9), "FR-84-69-69123"))
register_city(fr_city_module("MRS", "marseille", "13055", "Marseille", sprintf("132%02d", 1:16), "FR-93-13-13055"))
