# Madrid (MAD): calls received by the Municipal Police central dispatch (F1), by neighbourhood -----------
# City module of the shared city framework (R/60_cities.R). Readers: R/10_madrid_calls.R (calls and the
# municipal register), R/04_territory.R (districts and neighbourhoods), R/11_madrid_activity.R (the
# police activity and arrests panels of the Madrid page, an extra source of this city).

madrid_city_download <- function() {
  have_zip <- length(list.files(CONFIG$sources$madrid_neighbourhoods$dir_raw, "Barrios\\.zip$")) > 0 &&
              length(list.files(CONFIG$sources$madrid_districts$dir_raw, "Distritos\\.zip$")) > 0
  if (!have_zip) attempt("Madrid boundaries download", madrid_territory_download())   # boundaries change rarely: fetched once
  attempt("Madrid calls download", madrid_calls_download())
  attempt("Madrid register download", madrid_census_download())
  attempt("Madrid activity download", madrid_activity_download())
  invisible(TRUE)
}

# Districts and neighbourhoods: built once (131 neighbourhoods); option 7 rebuilds them
madrid_city_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_MADRID) > 0 && file.exists(GPKG_MADRID)) { message("= Madrid territory kept (", city_neighbourhood_count(con, ID_MADRID), " neighbourhoods)"); return(invisible(FALSE)) }
  madrid_territory_save(con)
}

madrid_city_ingest <- function(con) {
  madrid_calls_ingest(con)                                   # stg_madrid_calls / v_madrid_calls (incremental)
  types <- DBI::dbGetQuery(con, "SELECT call_type, block, sensitive, name_es, name_en FROM ref_madrid_call_types")
  ev <- publish_city_events(con, "madrid", "v_madrid_calls", "call_type", "events", types, file = "stg_madrid_calls")
  log_ingest(con, "city_madrid", "stg_madrid_calls", NA, paste(format(range(ev$month), "%Y-%m"), collapse = " to "), nrow(ev), sum(ev$events))
  attempt("Madrid activity not loaded (the activity panel stays as it was)", madrid_activity_ingest(con))
  invisible(ev)
}

madrid_city_population <- function(con) madrid_census_ingest(con)

register_city(list(
  code = "MAD", key = "madrid", id = ID_MADRID, name_en = "Madrid", name_local = "Madrid", lang = "es", country = "ES",
  source = list(name = "Calls received by the Municipal Police central dispatch", name_local = "Incidencias recibidas en la Emisora Central de la Policía Municipal",
                publisher = "Madrid City Council - Municipal Police",
                url = "https://datos.madrid.es/dataset/837676-0-incidencias-recibidas-en-la-emisora-central-de-policia-municipal", licence = "CC BY 4.0",
                measure = "Calls received by the central dispatch, by opening type (what was reported, not what was verified), month, district and neighbourhood. They are not offences."),
  panels = c("madrid_activity", "madrid_arrests"),
  note_territory = list(pattern = "Guindalera", key = "guindalera"),
  about_source_key = "about_src_calls", about_measures_key = "about_calls_measures", about_territory_key = "about_madrid_territory",
  download = madrid_city_download, territory = madrid_city_territory, ingest = madrid_city_ingest, population = madrid_city_population
))
