# Barcelona (BCN): incidents handled by the Guardia Urbana (F5), by neighbourhood ------------------------
# City module of the shared city framework (R/60_cities.R). Readers: R/31_bcn_incidents.R (incidents),
# R/04_territory.R (districts, neighbourhoods and the city register). The Mossos by district panel of the
# Barcelona page reads the Mossos tables of the Spain module (R/30_mossos.R, R/72_es_mossos.R).

bcn_city_download <- function() {
  attempt("Barcelona boundaries download", bcn_territory_download())
  attempt("Barcelona register download", bcn_census_download())
  attempt("Barcelona incidents download", bcn_incidents_download())
  invisible(TRUE)
}

bcn_city_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_BARCELONA) > 0 && file.exists(GPKG_BARCELONA)) { message("= Barcelona territory kept (", city_neighbourhood_count(con, ID_BARCELONA), " neighbourhoods)"); return(invisible(FALSE)) }
  bcn_territory_save(con)
}

bcn_city_ingest <- function(con) {
  bcn_incidents_ingest(con)                                  # stg_bcn_incidents / v_bcn_incidents
  types <- DBI::dbGetQuery(con, "SELECT code, block, sensitive, name_es, name_en FROM ref_bcn_incident_types")
  ev <- publish_city_events(con, "barcelona", "v_bcn_incidents", "code", "incidents", types, types_type_col = "code", file = "stg_bcn_incidents")
  log_ingest(con, "city_barcelona", "stg_bcn_incidents", NA, paste(format(range(ev$month), "%Y-%m"), collapse = " to "), nrow(ev), sum(ev$events))
  invisible(ev)
}

bcn_city_population <- function(con) bcn_census_ingest(con)

register_city(list(
  code = "BCN", key = "barcelona", id = ID_BARCELONA, name_en = "Barcelona", name_local = "Barcelona", lang = "es", country = "ES",
  source = list(name = "Incidents handled by the Guardia Urbana in the city of Barcelona", name_local = "Incidentes gestionados por la Guardia Urbana de Barcelona",
                publisher = "Barcelona City Council - Guardia Urbana",
                url = "https://opendata-ajuntament.barcelona.cat/data/en/dataset/incidents-gestionats-gub", licence = "CC BY 4.0",
                measure = "Incidents handled by the city police according to its internal coding, by month, neighbourhood and type. They are neither offences nor comparable with the Madrid calls."),
  panels = "mossos_districts",
  note_break = list(block = "public_safety", date = "2024-01-01", key = "break_2024"),
  about_source_key = "about_src_bcn", about_measures_key = "about_bcn_measures", about_territory_key = "about_bcn_territory",
  download = bcn_city_download, territory = bcn_city_territory, ingest = bcn_city_ingest, population = bcn_city_population
))
