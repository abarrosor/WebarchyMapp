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
# after the countries), panels (ids of extra panels the app knows: "madrid_activity",
# "madrid_arrests", "mossos_districts"), note_territory = list(pattern, key) (a warning on territories
# whose name matches), note_break = list(block, date, key) (a series break), about_*_key (About page texts),
# stats_level ("district" or "neighbourhood": what the statistics tab ranks by default).

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
               population_date = popd, gpkg = m$gpkg, panels = paste(m$panels %||% character(), collapse = ";"), stats_level = m$stats_level %||% "district",
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
    message(sprintf("[ok] %-18s %d months (%s to %s), %d neighbourhoods%s%s", m$name_en, r$months, format(r$m0, "%m/%Y"), format(r$m1, "%m/%Y"), r$nb,
                    if (pop > 0) "" else " (no register: no rates)", if (file.exists(m$gpkg)) "" else " (no boundaries)"))
  }
  invisible(x)
}
