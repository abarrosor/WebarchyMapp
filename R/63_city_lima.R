# Lima and Callao (LIM): police complaints (SIDPOL) by distrito -------------------------------------------
# City module of the shared city framework (R/60_cities.R), built on the Peru country module (R/93_pe.R):
# the same MININTER complaints file, read by month and distrito for the 43 distritos of Lima Metropolitana
# (provincia de Lima, 1501) and the 7 of the Callao (0701), with the district polygons and the INEI
# projections the country module already loads. Nothing new is downloaded.
#
# In the city hierarchy the metropolitan area is the "city" (a node of its own, PE-LIMACALLAO), Lima
# Metropolitana and Callao are its two "districts", and the distritos are the "neighbourhoods" (the finest
# level the source publishes). The same distritos stay in the Peru country view under their own ids.
# Rules as in every city: month x distrito at most, complaints are not verified offences, the threshold
# is applied in the app, never compared with another city.

ID_LIMA <- "PE-LIMACALLAO"
LIMA_PROVINCES <- c("1501" = "Lima Metropolitana", "0701" = "Callao")
lima_district_id <- function(prov_code) paste0(ID_LIMA, "-D", prov_code)
lima_neighbourhood_id <- function(ubigeo) paste0(lima_district_id(substr(ubigeo, 1, 4)), "-B", ubigeo)

lima_download <- function() { message("= Lima and Callao use the Peru files (R/93_pe.R): nothing to download"); invisible(TRUE) }

# Districts (the two provinces) and neighbourhoods (the distritos) from the Peru boundaries and names
lima_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_LIMA) > 0 && file.exists(CITY_MODULES$lima$gpkg)) { message("= Lima and Callao territory kept (", city_neighbourhood_count(con, ID_LIMA), " distritos)"); return(invisible(FALSE)) }
  gpkg_pe <- COUNTRY_MODULES$PE$gpkg
  if (!file.exists(gpkg_pe) || !"municipality" %in% sf::st_layers(gpkg_pe)$name) stop("no Peru district polygons yet (data/territory_pe.gpkg, layer municipality): run option 14 with PE first")
  g <- sf::st_read(gpkg_pe, layer = "municipality", quiet = TRUE)
  g$prov <- sub("^PE-[0-9]+-", "", g$province_id %||% "")
  g <- g[!is.na(g$prov) & g$prov %in% names(LIMA_PROVINCES), ]
  if (nrow(g) == 0) stop("no distrito of Lima (1501) or Callao (0701) in the Peru boundaries")
  ubigeo <- sub("^.*-", "", g$territory_id)
  names_pe <- DBI::dbGetQuery(con, "SELECT territory_id, name FROM dim_territory WHERE country = 'PE' AND level = 'municipality'")
  nm <- names_pe$name[match(g$territory_id, names_pe$territory_id)]; nm[is.na(nm)] <- g$name[is.na(nm)]
  neighbourhoods <- sf::st_sf(
    territory_id = lima_neighbourhood_id(ubigeo), level = "neighbourhood", parent_id = lima_district_id(g$prov), country = "PE",
    official_code = ubigeo, code_scheme = "INEI ubigeo", name = pe_title(nm), area_m2 = as.numeric(sf::st_area(g)), file = basename(gpkg_pe),
    geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, lima_district_id(g$prov))
  districts <- sf::st_sf(
    territory_id = dis$territory_id, level = "district", parent_id = ID_LIMA, country = "PE",
    official_code = sub("^.*-D", "", dis$territory_id), code_scheme = "INEI ubigeo (provincia)", name = unname(LIMA_PROVINCES[sub("^.*-D", "", dis$territory_id)]),
    area_m2 = as.numeric(sf::st_area(dis)), file = basename(gpkg_pe), geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = ID_LIMA, level = "municipality", parent_id = "PE", country = "PE", official_code = "1501+0701",
                      code_scheme = "WebarchyMapp (metropolitan area)", name = "Lima y Callao", stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES$lima$gpkg)
}

# Monthly complaints by distrito and modalidad from the Peru file, into the shared city tables
lima_ingest <- function(con) {
  f <- pe_complaints_file()
  if (is.na(f)) stop("No complaints file in ", pe_dir(), " (the Peru module downloads it: option 14 with PE)")
  d <- PE_STATE$complaints %||% pe_read_complaints(f)
  d <- d[d$prov_code %in% names(LIMA_PROVINCES), ]
  if (nrow(d) == 0) stop("no rows of Lima or Callao in ", basename(f))
  types <- read_reference("pe_offence_types.csv", bool_cols = "sensitive")
  types <- types[types$offence_type_id != "total", ]
  d$event_type <- types$source_code[match(normalize_text(d$type), normalize_text(types$source_code))]
  unknown <- unique(d$type[is.na(d$event_type)]); if (length(unknown)) message("  modalidades not in pe_offence_types.csv (left out): ", paste(unknown, collapse = " / "))
  d <- d[!is.na(d$event_type), ]
  ev <- d |> dplyr::group_by(year, month, ubigeo, event_type) |> dplyr::summarise(events = sum(n, na.rm = TRUE), .groups = "drop") |> as.data.frame()
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id, parent_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_LIMA, "-%"))))
  ev$neighbourhood_id <- lima_neighbourhood_id(ev$ubigeo); ev$neighbourhood_id[!ev$neighbourhood_id %in% known$territory_id] <- NA   # a distrito without polygon: district total only
  ev$district_id <- lima_district_id(substr(ev$ubigeo, 1, 4))
  ev$month <- as.Date(sprintf("%d-%02d-01", ev$year, ev$month)); ev$city <- "lima"; ev$file <- basename(f); ev$events <- as.integer(round(ev$events))
  replace_city_rows(con, "stg_city_events", "lima", ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  # Every modalidad is a "public safety" type; the sensitive ones count in the totals and are never listed
  t <- data.frame(city = "lima", event_type = types$source_code, block = "public_safety", name_local = types$name_local, name_en = types$name_en, sensitive = types$sensitive, stringsAsFactors = FALSE)
  t <- t[!duplicated(t$event_type), ]
  replace_city_rows(con, "ref_city_event_types", "lima", t)
  log_ingest(con, "city_lima", basename(f), NA, paste(format(range(ev$month), "%Y-%m"), collapse = " to "), nrow(ev), sum(ev$events))
  message(sprintf("  lima: %s event rows, %d months (%s to %s), %d distritos, %d types", fmt_int(nrow(ev)), length(unique(ev$month)), format(min(ev$month), "%m/%Y"), format(max(ev$month), "%m/%Y"), length(unique(stats::na.omit(ev$neighbourhood_id))), nrow(t)))
  invisible(ev)
}

# Population of each distrito: the INEI projections the Peru module loads (latest year), as the city register
lima_population <- function(con) {
  pop <- DBI::dbGetQuery(con, "SELECT territory_id, year, population FROM ref_country_population WHERE country = 'PE' AND level = 'municipality'")
  if (nrow(pop) == 0) stop("no Peru population loaded yet (option 14 with PE)")
  pop$ubigeo <- sub("^.*-", "", pop$territory_id); pop <- pop[substr(pop$ubigeo, 1, 4) %in% names(LIMA_PROVINCES), ]
  y <- max(pop$year[pop$year <= as.integer(format(Sys.Date(), "%Y"))]); pop <- pop[pop$year == y, ]
  ids <- lima_neighbourhood_id(pop$ubigeo)
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_LIMA, "-%"))))$territory_id
  keep <- ids %in% known; ref_date <- as.Date(sprintf("%d-01-01", y))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_LIMA, "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = ids[keep], ref_date = ref_date, population = as.integer(round(pop$population[keep])), file = "ref_country_population (PE, INEI projections)"))
  })
  message(sprintf("  population: %d distritos, INEI projection %d (%s residents)", sum(keep), y, fmt_int(sum(pop$population[keep]))))
  invisible(TRUE)
}

register_city(list(
  code = "LIM", key = "lima", id = ID_LIMA, name_en = "Lima and Callao", name_local = "Lima y Callao", lang = "es", country = "PE", after_countries = TRUE,
  stats_level = "neighbourhood",   # the statistics rank the 50 distritos, not the two provinces
  source = list(name = "Police complaints (SIDPOL) by distrito", name_local = "Denuncias policiales (SIDPOL) por distrito",
                publisher = "Ministerio del Interior (MININTER) / Policía Nacional del Perú, Plataforma Nacional de Datos Abiertos",
                url = "https://www.datosabiertos.gob.pe/dataset/denuncias-policiales", licence = "Open Data Commons Attribution License (ODC-BY)",
                measure = "Complaints filed at police stations, by month, distrito and modalidad (robo, hurto, estafa, extorsión, otros; secuestro and violence against women count in the totals only). Counted where they are filed, not verified offences."),
  download = lima_download, territory = lima_territory, ingest = lima_ingest, population = lima_population
))
