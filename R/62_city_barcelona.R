# Barcelona (BCN): incidents handled by the Guardia Urbana (F5), by neighbourhood ------------------------
# City module of the shared city framework (R/60_cities.R). Readers: R/31_bcn_incidents.R (incidents),
# R/04_territory.R (districts, neighbourhoods and the city register). The Mossos by district panel of the
# Barcelona page (below) reads the Mossos tables of the Spain module (R/30_mossos.R, R/72_es_mossos.R).

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
  method = list(where = "Neighbourhood where the Guardia Urbana handled the incident", date = "Date of the incident", revisions = "Yearly files; the public-safety block was recoded in 2024 (break in the series)"),
  note_break = list(block = "public_safety", date = "2024-01-01", key = "break_2024"),
  about_source_key = "about_src_bcn", about_measures_key = "about_bcn_measures", about_territory_key = "about_bcn_territory",
  download = bcn_city_download, territory = bcn_city_territory, ingest = bcn_city_ingest, population = bcn_city_population
))

# --- Panel: crime known to the Mossos d'Esquadra by district (shared panel contract, R/60_cities.R) -------
# The Mossos files (R/30_mossos.R, also the second source of Spain in R/72_es_mossos.R) give the offences
# known by basic police area; in Barcelona one ABP is one district. Shown in the profile of a neighbourhood
# as its district against the city, by offence group; the total counts every offence, sensitive ones too.
bcn_mossos_panel_ingest <- function(con) {
  mossos_ingest(con)                                         # stg_mossos / v_mossos (incremental; the Spain source runs it as well)
  if (!"v_mossos" %in% DBI::dbListTables(con)) stop("v_mossos missing: no Mossos file loaded yet")
  like <- DBI::dbQuoteString(con, paste0(ID_BARCELONA, "%"))
  v <- DBI::dbGetQuery(con, paste0("
    SELECT month, territory_id, group_id AS indicator, sum(known) AS value FROM v_mossos WHERE territory_id LIKE ", like, " AND NOT sensitive GROUP BY ALL
    UNION ALL
    SELECT month, territory_id, 'total', sum(known) FROM v_mossos WHERE territory_id LIKE ", like, " GROUP BY ALL"))
  if (nrow(v) == 0) stop("no Mossos rows for Barcelona (ref_mossos_abp_map: are the Barcelona ABP mapped to districts?)")
  g <- DBI::dbGetQuery(con, "SELECT group_id, name_en, name_es, sort_order FROM ref_mossos_groups WHERE group_id NOT IN ('sensitive', 'unmapped') ORDER BY sort_order")
  ind <- rbind(data.frame(indicator = g$group_id, name_en = g$name_en, name_local = g$name_es, name_key = "", sort_order = g$sort_order, is_total = FALSE, stringsAsFactors = FALSE),
               data.frame(indicator = "total", name_en = "Total (all offences)", name_local = "Total (todos los delitos)", name_key = "mossos_total", sort_order = 999L, is_total = TRUE, stringsAsFactors = FALSE))
  publish_city_panel(con, "barcelona", "mossos_districts", v, ind, "stg_mossos")
}

register_city_panel("barcelona", list(
  id = "mossos_districts", kind = "district", name = "Mossos d'Esquadra: offences known by district", name_local = "Mossos d'Esquadra: fets penals coneguts per districte",
  publisher = "Government of Catalonia, Department of the Interior (open data)", url = CONFIG$sources$mossos$page, licence = "Open data of the Government of Catalonia (see the terms of use of the portal)",
  measure = "Criminal offences known to the Mossos d'Esquadra by month and district of Barcelona (one basic police area per district), in 13 groups; sensitive offences count in the total only.",
  top_n = 6, title_key = "mossos_title", note_key = "mossos_note", col_key = "mossos_col_type", other_period_key = "mossos_other_period", source_key = "mossos_source", source_label = "analisi.transparenciacatalunya.cat",
  about_source_key = "about_src_mossos", about_measures_key = "about_mossos_measures", about_territory_key = "about_mossos_territory", about_licence_key = "about_mossos_licence", about_unless_country_source = "ES:mossos",
  download = function() { attempt("Mossos download", mossos_download()); invisible(TRUE) }, ingest = bcn_mossos_panel_ingest
))

# --- Survey: Enquesta de Victimització de Barcelona (EVB), by district ---------------------------------
# A measure of what the records miss: the share of residents who say they were victims of a fact they
# considered criminal during the year (yearly, since 1984 for the city; by district in the report tables).
# Read from data/raw/city_barcelona/survey/*.csv (long format: indicator, district_code 00 = city / 01-10,
# district_name, year, value, sample, source). The first file was transcribed from the 2024 report
# (P-033); exports of the Portal de Dades go in the same folder with the same columns.
BCN_SURVEY_DIR <- file.path(CONFIG$dir_raw, "city_barcelona", "survey")

bcn_survey_ingest <- function(con) {
  files <- list.files(BCN_SURVEY_DIR, "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("no survey file in ", BCN_SURVEY_DIR)
  d <- do.call(rbind, lapply(files, function(f) { x <- readr::read_csv(f, show_col_types = FALSE, col_types = readr::cols(.default = "c")); x$file <- basename(f); x }))
  d <- d[!is.na(d$value) & nzchar(d$value), ]
  code <- sprintf("%02d", as.integer(d$district_code))
  d$territory_id <- ifelse(code == "00", ID_BARCELONA, paste0(ID_BARCELONA, "-D", code))
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE territory_id = ", DBI::dbQuoteString(con, ID_BARCELONA), " OR (level = 'district' AND parent_id = ", DBI::dbQuoteString(con, ID_BARCELONA), ")"))$territory_id
  bad <- unique(d$district_name[!d$territory_id %in% known]); if (length(bad)) message("  survey rows for unknown districts left out: ", paste(bad, collapse = ", "))
  d <- d[d$territory_id %in% known, ]
  # the most recent file wins when two files give the same indicator, territory and year
  d <- d[order(d$file, decreasing = TRUE), ]; d <- d[!duplicated(d[, c("indicator", "territory_id", "year")]), ]
  values <- data.frame(year = as.integer(d$year), territory_id = d$territory_id, indicator = d$indicator, value = as.numeric(sub(",", ".", d$value)), sample = suppressWarnings(as.integer(d$sample)))
  indicators <- data.frame(
    indicator = "victimisation_index",
    name_local = "Índice de victimización", name_en = "Victimisation index", unit = "% of residents",
    definition_en = "Share of interviewees (residents aged 16 and over) who say they were victims of one or more facts they considered criminal during the year, whatever they reported to the police. Enquesta de Victimització de Barcelona, about 4,000 interviews a year.",
    definition_local = "Porcentaje de entrevistados (residentes de 16 años o más) que declaran haber sido víctimas de uno o más hechos que consideraron delictivos durante el año, los denunciaran o no. Enquesta de Victimització de Barcelona, unas 4.000 entrevistas al año.",
    higher_is = "worse", stringsAsFactors = FALSE)
  indicators <- indicators[indicators$indicator %in% values$indicator, ]
  publish_city_survey(con, "barcelona", values, indicators, paste(unique(d$file), collapse = "; "))
  log_ingest(con, "city_barcelona_survey", paste(unique(d$file), collapse = "; "), NA, paste(range(values$year), collapse = "-"), nrow(values))
  invisible(values)
}

register_city_survey("barcelona", list(
  id = "evb", name = "Enquesta de Victimització de Barcelona (victimisation survey)", name_local = "Enquesta de Victimització de Barcelona (encuesta de victimización)",
  publisher = "Barcelona City Council, Oficina Municipal de Dades (Departament d'Estudis d'Opinió)",
  url = "https://ajuntament.barcelona.cat/seguretatiprevencio/ca/enquesta-de-victimitzacio", licence = "Public report of Barcelona City Council, quoted with attribution; machine-readable series in the Portal de Dades de Barcelona",
  measure = "Yearly survey of residents (about 4,000 interviews): share who were victims of a fact they considered criminal during the year, by district. Measures victimisation as experienced and told, whether reported or not; it is not comparable with the recorded incidents.",
  ingest = bcn_survey_ingest
))
