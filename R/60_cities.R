# Cities: shared framework (Q-005 phase 3, P-030) -------------------------------------------------------
# A city is a module (R/6x_city_<key>.R) that registers itself with `register_city()` and fills the
# shared city tables with its own readers, the same way a country does (R/70_countries.R):
#
#   stg_city_events          city, month, neighbourhood_id, district_id, event_type, events, file
#                            monthly events of a city source (police calls, city-police incidents...)
#                            by official neighbourhood (NULL when the source gives none: district or
#                            city totals only); every type, sensitive ones included
#   ref_city_event_types     city, event_type, block, name_local, name_en, sensitive
#   ref_population           territory_id, ref_date, population (neighbourhoods, from the city register)
#   dim_territory            country > region > province > municipality > district > neighbourhood rows
#   data/territory_<key>.gpkg   layer "territory" with the districts and neighbourhoods (WGS84)
#
# What the app reads (mart_city_*): events without sensitive types and only the blocks that can be
# read by area (MARTS$blocks), the type names, and one metadata row per city (mart_city_meta).
# Rules: never day or hour (month x neighbourhood at most); the threshold is applied in the app after
# adding up the chosen months; figures of different cities are never compared.
#
# Module fields: code ("MAD"), key ("madrid"), id (municipality id), name_en, name_local, lang, country,
# source = list(name, publisher, url, licence, measure), download(), territory(con), ingest(con),
# population(con). Optional: after_countries = TRUE (the city is built on a country module's data and runs
# after the countries), note_territory = list(pattern, key) (a warning on territories
# whose name matches), note_break = list(block, date, key) (a series break), about_*_key (About page texts),
# stats_level ("district" or "neighbourhood": what the statistics tab ranks by default), time_unit ("month",
# the default, or "year" for a source that only publishes yearly figures: stg_city_events.month = 1 January of
# the year and the app shows years), measure_kind (which set of texts the app uses when a key has no city
# variant: "calls" (default), "incidents", "complaints", "offences", "cases"...: keys "k@<kind>" in
# translations.csv), boundaries_attribution (credit under the map when no "boundaries_source@<key>" text
# exists). Extra datasets of a
# city go in through register_city_survey() (yearly surveys) and register_city_panel() (monthly panels), below.

CITY_MODULES <- list()

register_city <- function(m) {
  needed <- c("code", "key", "id", "name_en", "name_local", "lang", "country", "source", "download", "territory", "ingest", "population")
  missing <- setdiff(needed, names(m)); if (length(missing)) stop("City module ", m$key %||% "?", " lacks: ", toString(missing))
  m$gpkg <- m$gpkg %||% file.path("data", paste0("territory_", m$key, ".gpkg"))
  CITY_MODULES[[m$key]] <<- m
  invisible(m)
}
city_by_code <- function(code) { k <- names(CITY_MODULES)[vapply(CITY_MODULES, function(m) m$code == code, logical(1))]; if (length(k)) CITY_MODULES[[k[1]]] else NULL }

city_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_city_events (
      city VARCHAR, month DATE, neighbourhood_id VARCHAR, district_id VARCHAR, event_type VARCHAR, events INTEGER, file VARCHAR)")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_city_event_types (
      city VARCHAR, event_type VARCHAR, block VARCHAR, name_local VARCHAR, name_en VARCHAR, sensitive BOOLEAN)")
  # Surveys (victimisation, perception of security): yearly indicators by city or district, a measure of
  # what the records miss, never mixed with the events (P-033)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_city_survey (
      city VARCHAR, year INTEGER, territory_id VARCHAR, indicator VARCHAR, value DOUBLE, sample INTEGER, file VARCHAR)")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_city_survey_indicators (
      city VARCHAR, indicator VARCHAR, name_local VARCHAR, name_en VARCHAR, unit VARCHAR, definition_en VARCHAR, definition_local VARCHAR, higher_is VARCHAR)")
  # Panels: a second monthly dataset of a city by district or for the whole city (police activity, arrests
  # by motive, a regional police by district...), never mixed with the events (P-035)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_city_panel_values (
      city VARCHAR, panel_id VARCHAR, month DATE, territory_id VARCHAR, indicator VARCHAR, value INTEGER, file VARCHAR)")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_city_panel_indicators (
      city VARCHAR, panel_id VARCHAR, indicator VARCHAR, name_local VARCHAR, name_en VARCHAR, name_key VARCHAR, sort_order INTEGER, is_total BOOLEAN)")
  invisible(TRUE)
}

# A survey of a city (register_city_survey(key, list(id, name, name_local, publisher, url, licence, measure,
# download, ingest))): ingest(con) fills stg_city_survey (year, territory_id = the city or one of its
# districts, indicator, value, sample) and ref_city_survey_indicators (indicator, names, unit, definition,
# higher_is = "worse" / "better") through publish_city_survey(). Shown in the city profile and in About.
register_city_survey <- function(key, s) {
  if (is.null(CITY_MODULES[[key]])) stop("register_city_survey: city ", key, " is not registered")
  needed <- c("id", "name", "publisher", "url", "licence", "measure", "ingest")
  missing <- setdiff(needed, names(s)); if (length(missing)) stop("Survey ", s$id %||% "?", " of ", key, " lacks: ", toString(missing))
  CITY_MODULES[[key]]$survey <<- s
  invisible(s)
}

publish_city_survey <- function(con, key, values, indicators, file) {
  v <- data.frame(city = key, year = as.integer(values$year), territory_id = values$territory_id, indicator = values$indicator, value = as.numeric(values$value),
                  sample = if (!is.null(values$sample)) as.integer(values$sample) else NA_integer_, file = file, stringsAsFactors = FALSE)
  replace_city_rows(con, "stg_city_survey", key, v)
  i <- data.frame(city = key, indicator = indicators$indicator, name_local = indicators$name_local, name_en = indicators$name_en, unit = indicators$unit,
                  definition_en = indicators$definition_en, definition_local = indicators$definition_local %||% indicators$definition_en, higher_is = indicators$higher_is %||% "worse", stringsAsFactors = FALSE)
  replace_city_rows(con, "ref_city_survey_indicators", key, i)
  message(sprintf("  %s survey: %d values, %d indicators, %d-%d, %d territories", key, nrow(v), nrow(i), min(v$year), max(v$year), length(unique(v$territory_id))))
  invisible(v)
}

# --- Panels: a second monthly dataset of a city ---------------------------------------------------------
# register_city_panel(key, list(id, kind, name, name_local, publisher, url, licence, measure, ingest, ...)):
# ingest(con) fills stg_city_panel_values (month, territory_id = the city or one of its districts, indicator,
# value) and ref_city_panel_indicators through publish_city_panel(). Two kinds:
#   "district"  shown in the profile of a neighbourhood: its district against the city, per indicator (count,
#               per 1,000 residents, city rate), threshold applied; top_n keeps the indicators with most
#               events in the district (else every indicator with data, in sort order). Rows with the city's
#               own id hold what has no district; the city figure is the sum of every row of the panel
#   "city"      shown in the statistics tab, whole-city figures: table by indicator (count, share, average of
#               the 12 months before) and a yearly bar chart of the chosen indicator (default_indicator)
# Optional: download(), top_n, default_indicator, and translation keys the app uses when present: title_key,
# note_key (tooltip of the title; "city" kind: a note under the table), note2_key (second note), col_key (first
# column), value_key (count column, "city" kind), other_period_key, source_key (line of the source block,
# "%s to %s" months), source_label (the host shown as a link), about_source_key / about_measures_key /
# about_territory_key / about_licence_key (About page row), about_unless_country_source = "CC:source" (no
# About row when that country source is loaded: the same data already has one). Without keys the app uses
# generic texts and the panel's English / local names. Indicators: indicator, name_en, name_local, name_key
# (translation key, optional), sort_order, is_total (the total row, shown last and in bold).
register_city_panel <- function(key, p) {
  if (is.null(CITY_MODULES[[key]])) stop("register_city_panel: city ", key, " is not registered")
  needed <- c("id", "kind", "name", "publisher", "url", "licence", "measure", "ingest")
  missing <- setdiff(needed, names(p)); if (length(missing)) stop("Panel ", p$id %||% "?", " of ", key, " lacks: ", toString(missing))
  if (!p$kind %in% c("district", "city")) stop("Panel ", p$id, " of ", key, ": kind must be 'district' or 'city'")
  CITY_MODULES[[key]]$panel_defs[[p$id]] <<- p
  invisible(p)
}

publish_city_panel <- function(con, key, panel_id, values, indicators, file) {
  v <- data.frame(city = key, panel_id = panel_id, month = as.Date(values$month), territory_id = values$territory_id, indicator = values$indicator,
                  value = as.integer(round(values$value)), file = file, stringsAsFactors = FALSE)
  v <- v[!is.na(v$value) & !is.na(v$territory_id), ]
  i <- data.frame(city = key, panel_id = panel_id, indicator = indicators$indicator, name_local = indicators$name_local %||% indicators$name_en, name_en = indicators$name_en,
                  name_key = indicators$name_key %||% "", sort_order = as.integer(indicators$sort_order %||% seq_len(nrow(indicators))),
                  is_total = as.logical(indicators$is_total %||% FALSE), stringsAsFactors = FALSE)
  i <- i[!duplicated(i$indicator), ]
  where <- paste0(" WHERE city = ", DBI::dbQuoteString(con, key), " AND panel_id = ", DBI::dbQuoteString(con, panel_id))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM stg_city_panel_values", where)); if (nrow(v)) DBI::dbAppendTable(con, "stg_city_panel_values", v)
    DBI::dbExecute(con, paste0("DELETE FROM ref_city_panel_indicators", where)); if (nrow(i)) DBI::dbAppendTable(con, "ref_city_panel_indicators", i)
  })
  if (nrow(v)) log_ingest(con, paste0("panel_", key, "_", panel_id), file, NA, paste(format(range(v$month), "%Y-%m"), collapse = " to "), nrow(v), sum(v$value))
  message(sprintf("  %s panel %s: %s values, %d indicators, %s to %s, %d territories", key, panel_id, fmt_int(nrow(v)), nrow(i),
                  if (nrow(v)) format(min(v$month), "%m/%Y") else "-", if (nrow(v)) format(max(v$month), "%m/%Y") else "-", length(unique(v$territory_id))))
  invisible(v)
}

# Replace the rows of one city in a shared city table
replace_city_rows <- function(con, table, key, df) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ", table, " WHERE city = ", DBI::dbQuoteString(con, key)))
    if (nrow(df) > 0) DBI::dbAppendTable(con, table, as.data.frame(df))
  })
  invisible(nrow(df))
}

# Monthly events from a reader's view (columns month, neighbourhood_id, district_id, <type column>, <count column>)
# into stg_city_events, and the type table (type column, block, sensitive, name_es, name_en) into ref_city_event_types
publish_city_events <- function(con, key, view, type_col, count_col, types, types_type_col = type_col, file = view) {
  ev <- DBI::dbGetQuery(con, sprintf("SELECT month, neighbourhood_id, district_id, %s AS event_type, sum(%s)::INTEGER AS events FROM %s GROUP BY ALL", type_col, count_col, view))
  ev$city <- key; ev$file <- file
  replace_city_rows(con, "stg_city_events", key, ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  t <- data.frame(city = key, event_type = types[[types_type_col]], block = types$block, name_local = types$name_es %||% types$name_local, name_en = types$name_en,
                  sensitive = as.logical(types$sensitive %||% FALSE), stringsAsFactors = FALSE)
  replace_city_rows(con, "ref_city_event_types", key, t)
  message(sprintf("  %s: %s event rows, %d months (%s to %s), %d types", key, fmt_int(nrow(ev)), length(unique(ev$month)), format(min(ev$month), "%m/%Y"), format(max(ev$month), "%m/%Y"), nrow(t)))
  invisible(ev)
}

# --- Marts ---------------------------------------------------------------------------------------------
city_marts <- function(con, threshold = MARTS$threshold) {
  blocks_sql <- paste(DBI::dbQuoteString(con, MARTS$blocks), collapse = ", ")
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE mart_city_events AS
    SELECT e.city, e.month, e.neighbourhood_id, e.district_id, t.block, e.event_type, sum(e.events)::INTEGER AS events
    FROM stg_city_events e JOIN ref_city_event_types t ON t.city = e.city AND t.event_type = e.event_type
    WHERE NOT t.sensitive AND t.block IN (", blocks_sql, ") GROUP BY ALL ORDER BY ALL"))
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE mart_city_event_types AS
    SELECT city, event_type, block, name_local, name_en FROM ref_city_event_types WHERE NOT sensitive AND block IN (", blocks_sql, ") ORDER BY ALL"))
  rows <- lapply(CITY_MODULES, function(m) {
    q <- function(sql) DBI::dbGetQuery(con, sprintf(sql, DBI::dbQuoteString(con, m$key)))
    per <- q("SELECT min(month) AS first_month, max(month) AS last_month FROM stg_city_events WHERE city = %s")
    pct <- q(paste0("SELECT round(100.0 * sum(events) FILTER (neighbourhood_id IS NULL) / sum(events), 1) AS pct FROM mart_city_events WHERE city = %s"))$pct
    loaded <- DBI::dbGetQuery(con, paste0("SELECT max(loaded_at) AS t FROM ingest_log WHERE source = ", DBI::dbQuoteString(con, paste0("city_", m$key))))$t
    popd <- DBI::dbGetQuery(con, paste0("SELECT max(ref_date) AS d FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(m$id, "-%"))))$d
    data.frame(city = m$key, code = m$code, territory_id = m$id, country = m$country, name_en = m$name_en, name_local = m$name_local, lang = m$lang,
               source_name = m$source$name, source_local = m$source$name_local %||% m$source$name, publisher = m$source$publisher, url = m$source$url, licence = m$source$licence, measure = m$source$measure,
               first_month = per$first_month, last_month = per$last_month, loaded_at = loaded, pct_without_neighbourhood = pct, threshold = as.integer(threshold),
               population_date = popd, gpkg = m$gpkg, panels = paste(unique(c(m$panels %||% character(), names(m$panel_defs %||% list()))), collapse = ";"), stats_level = m$stats_level %||% "district",
               time_unit = m$time_unit %||% "month", measure_kind = m$measure_kind %||% "calls", boundaries_attribution = m$boundaries_attribution %||% "",
               note_territory_pattern = m$note_territory$pattern %||% "", note_territory_key = m$note_territory$key %||% "",
               note_break_block = m$note_break$block %||% "", note_break_date = m$note_break$date %||% "", note_break_key = m$note_break$key %||% "",
               about_source_key = m$about_source_key %||% "", about_measures_key = m$about_measures_key %||% "", about_territory_key = m$about_territory_key %||% "",
               method_where = m$method$where %||% "", method_date = m$method$date %||% "", method_revisions = m$method$revisions %||% "",
               stringsAsFactors = FALSE)
  })
  meta <- if (length(rows)) do.call(rbind, rows) else data.frame()
  meta <- meta[!is.na(meta$first_month), ]
  DBI::dbWriteTable(con, "mart_city_meta", meta, overwrite = TRUE)
  # Surveys: values with their indicator names, and one metadata row per city survey
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_city_survey AS
    SELECT v.city, v.year, v.territory_id, v.indicator, v.value, v.sample, i.name_local, i.name_en, i.unit, i.definition_en, i.definition_local, i.higher_is
    FROM stg_city_survey v JOIN ref_city_survey_indicators i ON i.city = v.city AND i.indicator = v.indicator ORDER BY ALL")
  srows <- lapply(CITY_MODULES, function(m) { s <- m$survey; if (is.null(s)) return(NULL)
    per <- DBI::dbGetQuery(con, paste0("SELECT min(year) AS y0, max(year) AS y1, count(*) AS n FROM stg_city_survey WHERE city = ", DBI::dbQuoteString(con, m$key)))
    if (is.na(per$y0)) return(NULL)
    data.frame(city = m$key, survey_id = s$id, name_en = s$name, name_local = s$name_local %||% s$name, publisher = s$publisher, url = s$url, licence = s$licence, measure = s$measure,
               first_year = per$y0, last_year = per$y1, values = per$n, stringsAsFactors = FALSE) })
  # Panels: values and indicators as published, and one metadata row per panel (texts, keys, period)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_city_panel_values AS
    SELECT v.city, v.panel_id, v.month, v.territory_id, v.indicator, sum(v.value)::INTEGER AS value
    FROM stg_city_panel_values v JOIN ref_city_panel_indicators i ON i.city = v.city AND i.panel_id = v.panel_id AND i.indicator = v.indicator GROUP BY ALL ORDER BY ALL")
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE mart_city_panel_indicators AS SELECT * FROM ref_city_panel_indicators ORDER BY city, panel_id, sort_order")
  prows <- unlist(lapply(CITY_MODULES, function(m) lapply(m$panel_defs %||% list(), function(p) {
    per <- DBI::dbGetQuery(con, paste0("SELECT min(month) AS m0, max(month) AS m1, count(*) AS n FROM stg_city_panel_values WHERE city = ", DBI::dbQuoteString(con, m$key), " AND panel_id = ", DBI::dbQuoteString(con, p$id)))
    if (is.na(per$m0)) return(NULL)
    loaded <- DBI::dbGetQuery(con, paste0("SELECT max(loaded_at) AS t FROM ingest_log WHERE source = ", DBI::dbQuoteString(con, paste0("panel_", m$key, "_", p$id))))$t
    k <- function(f) p[[f]] %||% ""
    data.frame(city = m$key, panel_id = p$id, kind = p$kind, name_en = p$name, name_local = p$name_local %||% p$name, publisher = p$publisher, url = p$url, licence = p$licence, measure = p$measure,
               title_key = k("title_key"), note_key = k("note_key"), note2_key = k("note2_key"), col_key = k("col_key"), value_key = k("value_key"), other_period_key = k("other_period_key"),
               source_key = k("source_key"), source_label = k("source_label"), about_source_key = k("about_source_key"), about_measures_key = k("about_measures_key"),
               about_territory_key = k("about_territory_key"), about_licence_key = k("about_licence_key"), about_unless_country_source = k("about_unless_country_source"),
               top_n = as.integer(p$top_n %||% NA), default_indicator = k("default_indicator"), first_month = per$m0, last_month = per$m1, loaded_at = loaded, stringsAsFactors = FALSE) })), recursive = FALSE)
  prows <- Filter(Negate(is.null), prows)
  if (length(prows)) DBI::dbWriteTable(con, "mart_city_panels", do.call(rbind, prows), overwrite = TRUE) else
    DBI::dbExecute(con, "CREATE OR REPLACE TABLE mart_city_panels (city VARCHAR, panel_id VARCHAR, kind VARCHAR, name_en VARCHAR, name_local VARCHAR, publisher VARCHAR, url VARCHAR, licence VARCHAR, measure VARCHAR, title_key VARCHAR, note_key VARCHAR, note2_key VARCHAR, col_key VARCHAR, value_key VARCHAR, other_period_key VARCHAR, source_key VARCHAR, source_label VARCHAR, about_source_key VARCHAR, about_measures_key VARCHAR, about_territory_key VARCHAR, about_licence_key VARCHAR, about_unless_country_source VARCHAR, top_n INTEGER, default_indicator VARCHAR, first_month DATE, last_month DATE, loaded_at TIMESTAMP)")
  # The per-panel marts of the old layout (P-035): superseded by the three tables above
  for (tb in c("mart_madrid_activity", "mart_madrid_arrests", "mart_madrid_arrest_motives", "mart_madrid_activity_meta", "mart_mossos_bcn_month", "mart_mossos_groups", "mart_mossos_meta"))
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tb))
  srows <- Filter(Negate(is.null), srows)
  DBI::dbWriteTable(con, "mart_city_survey_meta", if (length(srows)) do.call(rbind, srows) else data.frame(city = character(), survey_id = character(), name_en = character(), name_local = character(), publisher = character(), url = character(), licence = character(), measure = character(), first_year = integer(), last_year = integer(), values = integer()), overwrite = TRUE)
  invisible(TRUE)
}

# --- Pipeline ------------------------------------------------------------------------------------------
# `after_countries`: NULL = every city; TRUE / FALSE = only the cities built on a country module's data (a
# module with after_countries = TRUE, run after country_steps()) or only the others
city_steps <- function(con, download = TRUE, only = NULL, after_countries = NULL) {
  mods <- CITY_MODULES; if (!is.null(only)) mods <- mods[vapply(mods, function(m) m$code %in% only || m$key %in% only, logical(1))]
  if (!is.null(after_countries)) mods <- mods[vapply(mods, function(m) isTRUE(m$after_countries) == after_countries, logical(1))]
  if (length(mods) == 0) { if (is.null(after_countries)) message("No city modules chosen."); return(invisible(FALSE)) }
  fetch <- function(label, expr) if (download) tryCatch(expr, error = function(e) message("Download not available (", label, "): ", conditionMessage(e), "\nContinuing with the files already in data/raw."))
  for (m in mods) {
    step(sprintf("%s (%s): %s", m$name_en, m$code, m$source$name))
    fetch(m$code, m$download())
    attempt(paste(m$code, "territory not loaded"), m$territory(con))
    attempt(paste(m$code, "data not loaded"), m$ingest(con))
    attempt(paste(m$code, "population not loaded (no rates per 1,000 residents)"), m$population(con))
    if (!is.null(m$survey)) {
      if (!is.null(m$survey$download)) fetch(paste(m$code, "survey"), m$survey$download())
      attempt(paste(m$code, "survey not loaded"), m$survey$ingest(con))
    }
    for (p in m$panel_defs %||% list()) {
      if (!is.null(p$download)) fetch(paste(m$code, "panel", p$id), p$download())
      attempt(paste(m$code, "panel", p$id, "not loaded (the panel stays as it was)"), p$ingest(con))
    }
  }
  invisible(TRUE)
}

update_cities <- function(download = TRUE, only = NULL) {
  with_db(function(con) {
    city_steps(con, download, only)
    step("Tables for the app")
    build_marts(con)
    city_status(con); country_status(con)
  })
}

city_status <- function(con) {
  if (!"stg_city_events" %in% DBI::dbListTables(con)) return(invisible(NULL))
  x <- DBI::dbGetQuery(con, "
    SELECT e.city, count(DISTINCT e.month) AS months, min(e.month) AS m0, max(e.month) AS m1, count(DISTINCT e.neighbourhood_id) AS nb
    FROM stg_city_events e GROUP BY 1")
  message("Cities")
  for (m in CITY_MODULES) {
    r <- x[x$city == m$key, ]
    if (nrow(r) == 0) { message("[!!] ", m$name_en, ": no data loaded yet"); next }
    pop <- DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(m$id, "-%"))))$n
    pn <- if ("stg_city_panel_values" %in% DBI::dbListTables(con)) DBI::dbGetQuery(con, paste0("SELECT panel_id, max(month) AS m1 FROM stg_city_panel_values WHERE city = ", DBI::dbQuoteString(con, m$key), " GROUP BY 1 ORDER BY 1")) else NULL
    yearly <- identical(m$time_unit, "year")
    message(sprintf("[ok] %-18s %d %s (%s to %s), %d neighbourhoods%s%s%s", m$name_en, r$months, if (yearly) "years" else "months", format(r$m0, if (yearly) "%Y" else "%m/%Y"), format(r$m1, if (yearly) "%Y" else "%m/%Y"), r$nb,
                    if (pop > 0) "" else " (no register: no rates)", if (file.exists(m$gpkg)) "" else " (no boundaries)",
                    if (!is.null(pn) && nrow(pn)) paste0("; panels: ", paste(sprintf("%s to %s", pn$panel_id, format(pn$m1, "%m/%Y")), collapse = ", ")) else ""))
  }
  invisible(x)
}
