# Madrid (MAD): calls received by the Municipal Police central dispatch (F1), by neighbourhood -----------
# City module of the shared city framework (R/60_cities.R). Readers: R/10_madrid_calls.R (calls and the
# municipal register), R/04_territory.R (districts and neighbourhoods), R/11_madrid_activity.R (the
# police activity statistics, published as two panels of the shared panel contract, below).

madrid_city_download <- function() {
  have_zip <- length(list.files(CONFIG$sources$madrid_neighbourhoods$dir_raw, "Barrios\\.zip$")) > 0 &&
              length(list.files(CONFIG$sources$madrid_districts$dir_raw, "Distritos\\.zip$")) > 0
  if (!have_zip) attempt("Madrid boundaries download", madrid_territory_download())   # boundaries change rarely: fetched once
  attempt("Madrid calls download", madrid_calls_download())
  attempt("Madrid register download", madrid_census_download())
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
  invisible(ev)
}

madrid_city_population <- function(con) madrid_census_ingest(con)

register_city(list(
  code = "MAD", key = "madrid", id = ID_MADRID, name_en = "Madrid", name_local = "Madrid", lang = "es", country = "ES",
  source = list(name = "Calls received by the Municipal Police central dispatch", name_local = "Incidencias recibidas en la Emisora Central de la Policía Municipal",
                publisher = "Madrid City Council - Municipal Police",
                url = "https://datos.madrid.es/dataset/837676-0-incidencias-recibidas-en-la-emisora-central-de-policia-municipal", licence = "CC BY 4.0",
                measure = "Calls received by the central dispatch, by opening type (what was reported, not what was verified), month, district and neighbourhood. They are not offences."),
  method = list(where = "Neighbourhood of the address the caller gave to the central dispatch", date = "Date the call was received", revisions = "Monthly files, not revised; the source publishes the previous month"),
  note_territory = list(pattern = "Guindalera", key = "guindalera"),
  about_source_key = "about_src_calls", about_measures_key = "about_calls_measures", about_territory_key = "about_madrid_territory",
  download = madrid_city_download, territory = madrid_city_territory, ingest = madrid_city_ingest, population = madrid_city_population
))

# --- Panels: Municipal Police activity statistics (F2), two panels of the shared contract (R/60_cities.R) ---
# The same yearly Excel files (R/11_madrid_activity.R, stg_madrid_activity) give actions by district and
# month (panel "madrid_activity", shown in the profile) and people arrested or investigated by motive for
# the whole city (panel "madrid_arrests", shown in the statistics tab). Police activity, not crime.
MADRID_ACTIVITY_INDICATORS <- c("arrested_investigated", "losc_people", "losc_property", "losc_drug_possession", "losc_drug_use", "losc_weapons",
                                "drinking_in_public_adults", "venue_reports", "street_vending_counterfeit", "street_vending_food", "accidents_with_casualties")
MADRID_ACTIVITY_SOURCE <- list(name = "Municipal Police activity statistics", name_local = "Datos estadísticos de actuaciones de Policía Municipal", publisher = "Madrid City Council - Municipal Police",
                               url = "https://datos.madrid.es/dataset/212616-0-policia-estadisticas", licence = "CC BY 4.0", label = "datos.madrid.es")

madrid_activity_panel_ingest <- function(con) {
  madrid_activity_ingest(con)                                 # stg_madrid_activity (incremental, yearly files)
  v <- DBI::dbGetQuery(con, paste0("
    SELECT month, '", ID_MADRID, "-D' || district_code AS territory_id, indicator, sum(value) AS value
    FROM stg_madrid_activity WHERE district_code IS NOT NULL AND value IS NOT NULL AND block <> 'arrest_motives' GROUP BY ALL
    UNION ALL
    SELECT month, '", ID_MADRID, "', indicator, sum(value)                 -- actions without a district: the city row (the city total is the sum of every row)
    FROM stg_madrid_activity WHERE district_code IS NULL AND value IS NOT NULL AND block <> 'arrest_motives' GROUP BY ALL"))
  v <- v[v$indicator %in% MADRID_ACTIVITY_INDICATORS, ]
  if (nrow(v) == 0) stop("no activity rows in stg_madrid_activity")
  plain <- function(x) { x <- gsub("_", " ", x); paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))) }   # fallback names; the app uses the act_* keys
  ind <- data.frame(indicator = MADRID_ACTIVITY_INDICATORS, name_en = plain(MADRID_ACTIVITY_INDICATORS), name_local = plain(MADRID_ACTIVITY_INDICATORS),
                    name_key = paste0("act_", MADRID_ACTIVITY_INDICATORS), sort_order = seq_along(MADRID_ACTIVITY_INDICATORS), is_total = FALSE, stringsAsFactors = FALSE)
  publish_city_panel(con, "madrid", "madrid_activity", v, ind, "stg_madrid_activity")
}

madrid_arrests_panel_ingest <- function(con) {
  if (!DBI::dbExistsTable(con, "stg_madrid_activity")) stop("stg_madrid_activity missing (the activity panel loads it)")
  v <- DBI::dbGetQuery(con, paste0("SELECT month, '", ID_MADRID, "' AS territory_id, indicator, sum(value) AS value FROM stg_madrid_activity WHERE block = 'arrest_motives' AND value IS NOT NULL GROUP BY ALL"))
  if (nrow(v) == 0) stop("no arrest-motive rows in stg_madrid_activity")
  motives <- read_reference("madrid_arrest_motives.csv"); motives <- motives[!duplicated(motives$motive_id), ]
  ind <- data.frame(indicator = motives$motive_id, name_en = motives$name_en, name_local = motives$name_es, name_key = "", sort_order = seq_len(nrow(motives)), is_total = motives$motive_id == "total", stringsAsFactors = FALSE)
  publish_city_panel(con, "madrid", "madrid_arrests", v, ind, "stg_madrid_activity")
}

register_city_panel("madrid", list(
  id = "madrid_activity", kind = "district", name = MADRID_ACTIVITY_SOURCE$name, name_local = MADRID_ACTIVITY_SOURCE$name_local, publisher = MADRID_ACTIVITY_SOURCE$publisher,
  url = MADRID_ACTIVITY_SOURCE$url, licence = MADRID_ACTIVITY_SOURCE$licence,
  measure = "Actions of the Municipal Police by district and month (people arrested or investigated, Public Safety Act reports, drinking in public, venue reports, street vending, traffic accidents with casualties). Police activity, not crime.",
  title_key = "activity_title", note_key = "activity_note", col_key = "activity_col", other_period_key = "activity_other_period", source_key = "activity_source", source_label = MADRID_ACTIVITY_SOURCE$label,
  about_source_key = "about_src_act", about_measures_key = "about_act_measures", about_territory_key = "about_act_territory",
  download = madrid_activity_download, ingest = madrid_activity_panel_ingest
))

register_city_panel("madrid", list(
  id = "madrid_arrests", kind = "city", name = "Municipal Police arrests by motive", name_local = "Detenidos e investigados por la Policía Municipal, por motivo", publisher = MADRID_ACTIVITY_SOURCE$publisher,
  url = MADRID_ACTIVITY_SOURCE$url, licence = MADRID_ACTIVITY_SOURCE$licence,
  measure = "People arrested or investigated by the Municipal Police by motive and month, whole city only (not offences, not victims; National Police arrests excluded).",
  title_key = "arrests_title", col_key = "arrests_col_motive", value_key = "arrests_col_people", note_key = "arrests_note", note2_key = "arrests_year_note", default_indicator = "domestic_gender_violence",
  ingest = madrid_arrests_panel_ingest   # no About row of its own: the activity row covers both panels
))
