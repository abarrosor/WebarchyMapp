# WebarchyMapp - personal app ---------------------------------------------------------
# Opened from WEBARCHYMAPP.R (options 2 and 3) or scripts/2_open_app.R.
# Reads ONLY the mart_* tables (no day, hour or sensitive types) and the official
# geometries. The "fewer than N" threshold is applied AFTER adding up the chosen
# range of months, with the same function as the data process (R/50_marts.R).
# The small tables are read at start-up; a country's or a city's figures and boundaries are read the
# first time its view opens and kept for the session (see "Data" below).
#
# Views (?view=<country code> / <city key> / about, ?lang=xx):
#   Country   - one view per country with data (Spain included), from the shared mart_country_*
#               tables: recorded offences by region, province and municipality; a country may carry
#               a second source (Spain: the Mossos d'Esquadra by police area in Catalonia) shown as
#               a switch of source inside the territory it covers, never added to the first one
#   City      - one view per city module (mart_city_*): events by neighbourhood, district and month,
#               with the panels and surveys the module declares
# Figures of one country or city are never compared with another: they measure different things. (The table of
# national rates of Eurostat, the one place that put countries side by side, was retired on 2026-11-17: P-063.)
DEFAULT_COUNTRY <- "ES"   # the country the app opens with and the first one in the Country menu

missing <- setdiff(c("shiny", "bslib", "leaflet", "shinyWidgets", "dplyr", "sf", "DBI", "duckdb"), rownames(installed.packages()))
if (length(missing) > 0) stop("Missing packages: ", paste(missing, collapse = ", "), ". Run option 5 in WEBARCHYMAPP.R and reopen the app.", call. = FALSE)

library(shiny)
library(leaflet)
library(dplyr, warn.conflicts = FALSE)

root <- if (file.exists("../WebarchyMapp.Rproj")) ".." else "."
path <- function(...) file.path(root, ...)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
source(path("R/50_marts.R"),    encoding = "UTF-8", local = TRUE)   # MARTS, apply_threshold()
source(path("R/03_database.R"), encoding = "UTF-8", local = TRUE)   # db_connect_readonly(), db_disconnect()
source(path("app/i18n.R"),      encoding = "UTF-8", local = TRUE)   # tr(), LANGUAGES, lang_from_url(), view_from_url()
TR <- tr

# The app is split into files sourced here, in this order (all into this one environment):
#   app/app_data.R       Data: the small tables read at start-up, the readers of a place's figures and boundaries
#   app/app_helpers.R    Helpers: formats, the time-series block, names, ⓘ, KPIs, the comparison card, density, concentration
#   app/app_styles.R     Styles and scripts: CSS, JavaScript (loading notice, phone bar), the page header, the navbar
#   app/app_city.R       City view: filters, map, profile, statistics, panels; one environment per city (make_city)
#   app/app_country.R    Country view: controls, map, table, profile, series; one environment per country (make_country)
#   app/app_about.R      About the data
for (part in c("app_data.R", "app_helpers.R", "app_styles.R", "app_city.R", "app_country.R", "app_about.R"))
  source(path(file.path("app", part)), encoding = "UTF-8", local = TRUE)

# =============================================================================
# Countries: DEFAULT_COUNTRY first, then the others by English name (only those with figures loaded); cities
# in the order of their modules. The light parts are built now; the full environments on first use.
country_codes <- if (has_rows(CMETA)) c(intersect(DEFAULT_COUNTRY, CMETA$country), setdiff(CMETA$country[order(CMETA$name_en)], DEFAULT_COUNTRY)) else character()
COUNTRY_LIGHT <- Filter(function(e) !is.null(e) && e$HAS_DATA, setNames(lapply(country_codes, country_light), country_codes))
CITY_LIGHT <- Filter(function(x) !is.null(x) && file.exists(path(CITY_SOURCES[[x$CITY]]$gpkg)), setNames(lapply(names(CITY_SOURCES), city_light), names(CITY_SOURCES)))
VIEWS_AVAILABLE <- c(names(COUNTRY_LIGHT), names(CITY_LIGHT), "about")
if (length(VIEWS_AVAILABLE) == 1) stop("The app tables are missing. Run option 1 in WEBARCHYMAPP.R.")
VIEW_CACHE <- new.env()
get_view <- function(view) {                                     # the full environment of a country or a city, built once
  if (is.null(VIEW_CACHE[[view]])) {
    t0 <- Sys.time()
    e <- if (view %in% names(COUNTRY_LIGHT)) make_country(view) else if (view %in% names(CITY_LIGHT)) make_city(view) else NULL
    VIEW_CACHE[[view]] <- if (is.null(e)) FALSE else e
    if (!is.null(e)) message(sprintf("%s loaded in %.1f s", view, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  x <- VIEW_CACHE[[view]]; if (isFALSE(x)) NULL else x
}
invisible(get_view(VIEWS_AVAILABLE[1]))                           # the first view is ready before the first browser asks for it
fallback_view <- function(view) { if (identical(view, "about")) return(view); if (!is.null(get_view(view))) return(view); if (!is.null(get_view(VIEWS_AVAILABLE[1]))) VIEWS_AVAILABLE[1] else "about" }

ui <- function(req) {
  lang <- lang_from_url(req$QUERY_STRING); view <- fallback_view(view_from_url(req$QUERY_STRING, VIEWS_AVAILABLE))
  if (view == "about") ui_about(lang, CITY_LIGHT)
  else if (view %in% names(COUNTRY_LIGHT)) get_view(view)$ui_country(lang)
  else get_view(view)$ui_city(lang)
}
server <- function(input, output, session) {
  qs <- isolate(session$clientData$url_search)
  lang <- lang_from_url(qs); view <- fallback_view(view_from_url(qs, VIEWS_AVAILABLE))
  if (view %in% names(COUNTRY_LIGHT)) get_view(view)$server_country(input, output, session, lang)
  else if (view != "about") get_view(view)$server_city(input, output, session, lang)
}
shinyApp(ui, server)
