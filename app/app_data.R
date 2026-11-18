# Data: the small tables read at start-up, the readers of a place's figures and boundaries ------------
# Part of the app: sourced by app/app.R (same environment); nothing here runs on its own.

# --- Data ------------------------------------------------------------------------
# Small tables (names, metadata, population, panels, surveys) are read once at start-up.
# The big ones (a country's figures, a city's monthly events, the boundary layers) are read the first
# time a view is opened and kept in memory for the rest of the session: opening the app takes seconds,
# not minutes, whatever the number of countries and cities loaded. Every read opens the database in
# read-only mode and closes it at once, so the data process can still run while the app is open.
DB_PATH <- path("data/webarchymapp.duckdb")
fetch <- function(sql) {
  con <- tryCatch(db_connect_readonly(DB_PATH), error = function(e) NULL); if (is.null(con)) return(NULL)
  on.exit(db_disconnect(con), add = TRUE)
  tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
}
sql_str <- function(x) paste0("'", gsub("'", "''", x), "'")
has_rows <- function(x) !is.null(x) && nrow(x) > 0
con <- db_connect_readonly(DB_PATH)
q <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
TERRITORY <- q("SELECT territory_id, level, parent_id, name FROM mart_territory")
POP_ALL   <- q("SELECT * FROM mart_population") %||% data.frame(territory_id = character(), population = numeric())
# The registers of the cities by year (a rate stands on the residents of the years of its period). NULL in a database
# built before this table existed: each city then has its latest register only (city_registers(), app_helpers.R).
POP_YEARS <- q("SELECT territory_id, year, population, ref_date FROM mart_population_years")

# Cities: one metadata row per city (mart_city_meta, written by the city modules of R/60_cities.R) and
# their type names; the monthly events by neighbourhood are read per city on first use (city_events())
CITY_META  <- q("SELECT * FROM mart_city_meta")
CITY_TYPES <- q("SELECT * FROM mart_city_event_types")
# Types a city's source lacks for some months (a file cut by its publisher): shown as missing, never as zero (P-056).
# NULL in a database built before this table existed, and no rows for most cities.
CITY_GAPS  <- q("SELECT city, block, event_type, first_month, last_month, reason FROM mart_city_gaps")
CITY_SURVEY <- q("SELECT * FROM mart_city_survey")                       # victimisation / perception surveys by city and district (yearly)
CITY_SURVEY_META <- q("SELECT * FROM mart_city_survey_meta")
CITY_SOURCES <- if (has_rows(CITY_META)) setNames(lapply(seq_len(nrow(CITY_META)), function(i) { m <- CITY_META[i, ]; list(id = m$territory_id, code = m$code, gpkg = m$gpkg, meta = m) }), CITY_META$city) else list()
city_events <- function(key) {
  base <- fetch(paste0("SELECT month, neighbourhood_id, district_id, block, event_type AS call_type, events FROM mart_city_events WHERE city = ", sql_str(key)))
  if (!has_rows(base)) return(NULL)
  base$month <- as.Date(base$month); base
}
city_types <- function(key) {
  types <- if (has_rows(CITY_TYPES)) CITY_TYPES[CITY_TYPES$city == key, c("event_type", "block", "name_local", "name_en")] else NULL
  if (!is.null(types)) names(types) <- c("call_type", "block", "name_es", "name_en")
  types
}

CITY_PANELS <- q("SELECT * FROM mart_city_panels")                       # extra monthly panels of a city (police activity, arrests by motive, Mossos by district...), P-035
CITY_PANEL_VALUES <- q("SELECT * FROM mart_city_panel_values")
CITY_PANEL_IND <- q("SELECT * FROM mart_city_panel_indicators")
CMETA   <- q("SELECT * FROM mart_country_meta")                          # one row per country and source ('main' + second sources)
CSOURCES_WITH_DATA <- q("SELECT country, source, count(*) AS n FROM mart_country_crime GROUP BY 1, 2")   # which countries and sources hold figures
CINFO   <- q("SELECT * FROM mart_country_territory_info")                # a line of context about some territories (municipalities of a police area...)
db_disconnect(con)
country_crime <- function(cc) fetch(paste0("SELECT * FROM mart_country_crime WHERE country = ", sql_str(cc)))
country_population <- function(cc) fetch(paste0("SELECT * FROM mart_country_population WHERE country = ", sql_str(cc)))

# A layer that the GeoPackage does not hold (a country without municipalities, say) is simply NULL
read_layer <- function(file, layer) {
  f <- path(file); if (!file.exists(f)) return(NULL)
  has <- tryCatch(layer %in% sf::st_layers(f)$name, error = function(e) FALSE); if (!has) return(NULL)
  tryCatch(sf::st_read(f, layer = layer, quiet = TRUE), error = function(e) NULL)
}
# Bounding box of every polygon (lon/lat), to know which territories are inside the map view
polygon_boxes <- function(g) {
  if (is.null(g)) return(NULL)
  b <- t(vapply(sf::st_geometry(g), function(x) as.numeric(sf::st_bbox(x)), numeric(4)))
  data.frame(territory_id = g$territory_id, xmin = b[, 1], ymin = b[, 2], xmax = b[, 3], ymax = b[, 4])
}
name_of   <- setNames(TERRITORY$name, TERRITORY$territory_id)
parent_of <- setNames(TERRITORY$parent_id, TERRITORY$territory_id)
level_of  <- setNames(TERRITORY$level, TERRITORY$territory_id)
# A single-province region (Madrid, Asturias...) may have no province row in the source: the
# province takes the figures of its region (it is the same territory)
fill_single_provinces <- function(crime) {
  if (!"source" %in% names(crime)) crime$source <- "main"
  main <- crime[crime$source == "main", ]; crime <- crime[crime$source != "main", ]
  prov <- TERRITORY[TERRITORY$level == "province" & TERRITORY$territory_id %in% TERRITORY$territory_id[startsWith(TERRITORY$territory_id, paste0(crime$country[1], "-"))], ]
  n_prov <- table(prov$parent_id)
  single <- prov[prov$parent_id %in% names(n_prov)[n_prov == 1], ]
  missing_prov <- single[!single$territory_id %in% main$territory_id & single$parent_id %in% main$territory_id, ]
  extra <- do.call(rbind, lapply(seq_len(nrow(missing_prov)), function(i) {
    r <- main[main$territory_id == missing_prov$parent_id[i], ]
    r$territory_id <- missing_prov$territory_id[i]; r$level <- "province"; r$name <- missing_prov$name[i]; r$parent_id <- missing_prov$parent_id[i]
    r
  }))
  rbind(main, extra, crime)
}
# Text of a source's metadata row: a translation key when the row names one that the app knows, else the local / English text
meta_text <- function(m, what, lang, clang, default = NULL) {
  k <- m[[paste0(what, "_key")]] %||% ""
  if (nzchar(k) && !identical(TR(k, "en"), k)) return(TR(k, lang))
  x <- if (lang == clang) m[[paste0(what, "_local")]] else NULL
  if (is.null(x) || is.na(x) || !nzchar(x)) x <- m[[paste0(what, "_en")]]
  if (is.null(x) || is.na(x) || !nzchar(x)) default else x
}
