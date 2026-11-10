# WebarchyMapp - personal app ---------------------------------------------------------
# Opened from WEBARCHYMAPP.R (options 2 and 3) or scripts/2_open_app.R.
# Reads ONLY the mart_* tables (no day, hour or sensitive types) and the official
# geometries. The "fewer than N" threshold is applied AFTER adding up the chosen
# range of months, with the same function as the data process (R/50_marts.R).
# Everything is loaded into memory at start-up and the database is closed.
#
# Views (?view=<country code> / madrid / barcelona / countries / about, ?lang=xx):
#   Country   - one view per country with data (Spain included), from the shared mart_country_*
#               tables: recorded offences by region, province and municipality; a country may carry
#               a second source (Spain: the Mossos d'Esquadra by police area in Catalonia) shown as
#               a switch of source inside the territory it covers, never added to the first one
#   Madrid    - calls to the Municipal Police (F1) by neighbourhood + police activity (F2)
#   Barcelona - incidents handled by the City Police (F5) + Mossos crime by district (F4)
# Figures of one country or city are never compared with another: they measure different things.
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

# --- Data ------------------------------------------------------------------------
con <- db_connect_readonly(path("data/webarchymapp.duckdb"))
q <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
has_rows <- function(x) !is.null(x) && nrow(x) > 0
TERRITORY <- q("SELECT territory_id, level, parent_id, name FROM mart_territory")
POP_ALL   <- q("SELECT * FROM mart_population") %||% data.frame(territory_id = character(), population = numeric())

# Cities: one metadata row per city (mart_city_meta, written by the city modules of R/60_cities.R),
# their monthly events by neighbourhood and their type names, all from the shared city tables
CITY_META  <- q("SELECT * FROM mart_city_meta")
CITY_EV    <- q("SELECT * FROM mart_city_events")
CITY_TYPES <- q("SELECT * FROM mart_city_event_types")
CITY_SURVEY <- q("SELECT * FROM mart_city_survey")                       # victimisation / perception surveys by city and district (yearly)
CITY_SURVEY_META <- q("SELECT * FROM mart_city_survey_meta")
CITY_SOURCES <- if (has_rows(CITY_META)) setNames(lapply(seq_len(nrow(CITY_META)), function(i) { m <- CITY_META[i, ]; list(id = m$territory_id, code = m$code, gpkg = m$gpkg, meta = m) }), CITY_META$city) else list()
RAW <- lapply(CITY_SOURCES, function(x) {
  key <- x$meta$city
  base <- if (has_rows(CITY_EV)) CITY_EV[CITY_EV$city == key, c("month", "neighbourhood_id", "district_id", "block", "event_type", "events")] else NULL
  if (!is.null(base)) names(base)[names(base) == "event_type"] <- "call_type"
  types <- if (has_rows(CITY_TYPES)) CITY_TYPES[CITY_TYPES$city == key, c("event_type", "block", "name_local", "name_en")] else NULL
  if (!is.null(types)) names(types) <- c("call_type", "block", "name_es", "name_en")
  list(base = base, meta = x$meta, types = types)
})

CITY_PANELS <- q("SELECT * FROM mart_city_panels")                       # extra monthly panels of a city (police activity, arrests by motive, Mossos by district...), P-035
CITY_PANEL_VALUES <- q("SELECT * FROM mart_city_panel_values")
CITY_PANEL_IND <- q("SELECT * FROM mart_city_panel_indicators")
CCRIME  <- q("SELECT * FROM mart_country_crime")                         # every country, Spain included (R/70_countries.R)
CPOP    <- q("SELECT * FROM mart_country_population")
CMETA   <- q("SELECT * FROM mart_country_meta")                          # one row per country and source ('main' + second sources)
CINFO   <- q("SELECT * FROM mart_country_territory_info")                # a line of context about some territories (municipalities of a police area...)
CRATES  <- q("SELECT * FROM mart_country_rates")                         # national rates (Eurostat; UNODC excluded by its terms) for the Countries table
db_disconnect(con)

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

# --- Formats (depend on the language) ------------------------------------------------
fmt_n <- function(n, lang) format(n, big.mark = TR("big", lang), decimal.mark = TR("dec", lang), scientific = FALSE, trim = TRUE)
fmt_r <- function(x, lang) vapply(x, function(v) {             # 2 decimals below 1, otherwise 1
  if (is.na(v)) return(NA_character_)
  d <- if (abs(v) < 1) 2 else 1
  format(round(v, d), nsmall = d, decimal.mark = TR("dec", lang), big.mark = TR("big", lang), trim = TRUE)
}, character(1))
fmt_month <- function(d, lang) paste(month_names(lang)[as.integer(format(d, "%m"))], format(d, "%Y"))
fmt_date  <- function(d, lang) format(d, "%Y-%m-%d")
fmt_range <- function(r, lang) if (r[1] == r[2]) fmt_month(r[1], lang) else paste(fmt_month(r[1], lang), "–", fmt_month(r[2], lang))
block_name <- function(b, lang) TR(paste0("block_", b), lang)
readable_type <- function(x) { x <- tolower(sub("\\.\\.\\.$", "", x)); paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))) }
# "Rozas de Madrid (Las)" -> "Las Rozas de Madrid"; "Hospitalet de Llobregat (L')" -> "L'Hospitalet..."
pretty_name <- function(x) {
  x <- sub("^(.*), (.*)$", "\\2 \\1", x)                    # "Balears, Illes" -> "Illes Balears"
  x <- sub("^(.*) \\((L')\\)$", "\\2\\1", x)
  sub("^(.*) \\((El|La|Los|Las|Els|Les|Es|Sa|A|O|Os|As)\\)$", "\\2 \\1", x)
}
# Localised name of a reference row (name_en for every language except Spanish)
local_name <- function(df, id_col, lang) setNames(if (lang == "es") df$name_es else df$name_en, df[[id_col]])

info <- function(text) bslib::tooltip(span(class = "info", HTML("&#9432;")), text, placement = "right")
count_label <- function(text, n, lang, note = NA) {
  tagList(text, span(class = "cnt", paste0("(", fmt_n(n, lang), ")")), if (!is.na(note %||% NA)) info(note))
}
kpi <- function(v, l) div(class = "kpi", div(class = "v", v), div(class = "l", l))
lc <- function(x) paste0(tolower(substr(x, 1, 1)), substr(x, 2, nchar(x)))   # "Police areas (ABP)" -> "police areas (ABP)" inside a sentence

# Rate compared with a reference: a label (much lower ... much higher than the reference
# average), the position among the territories of the same level, and the % difference.
# It is a comparison of recorded figures, never a safety or danger score.
CMP_LEVELS <- c("cmp_much_lower", "cmp_lower", "cmp_similar", "cmp_higher", "cmp_much_higher")
CMP_COLOURS <- c("#dbe7f6", "#bdd7e7", "#8fbbdc", "#4f94cb", "#1d5fa6")
# Percentile band of a rank among n (highest rates first in the wording): top 10 %, upper quarter, middle half, lower quarter, bottom 10 %
percentile_band <- function(rank, n, lang) {
  if (n < 10) return(NULL)
  p <- (rank - 0.5) / n   # share of places with a lower rate
  TR(if (p >= 0.9) "band_top10" else if (p >= 0.75) "band_upper" else if (p > 0.25) "band_middle" else if (p > 0.1) "band_lower" else "band_bottom10", lang)
}
MIN_EVENTS_STABLE <- 30   # below this many events a rate or a change is marked as unstable (Poisson noise)

comparison_block <- function(rate, ref_rate, rates_all, ref_label, unit_label, group_label, lang, n_events = NA) {
  t <- function(k, ...) TR(k, lang, ...)
  if (is.na(rate) || is.na(ref_rate) || ref_rate <= 0) return(NULL)
  ratio <- rate / ref_rate
  lvl <- findInterval(ratio, c(0.6, 0.85, 1.15, 1.5)) + 1
  all <- sort(rates_all[!is.na(rates_all)]); n <- length(all)
  rank <- sum(all < rate) + 1
  pct <- round(100 * abs(ratio - 1))
  diff_text <- if (pct == 0) t("cmp_same", ref_label) else if (ratio < 1) t("cmp_below", pct, ref_label) else if (ratio >= 1.995) t("cmp_times", fmt_r(ratio, lang), ref_label) else t("cmp_above", pct, ref_label)
  band <- percentile_band(rank, n, lang)
  div(class = "cmp",
    div(class = "cmp-head", span(class = "cmp-title", t("cmp_title", ref_label), info(t("cmp_note"))),
        span(class = "cmp-pill", style = paste0("background:", CMP_COLOURS[lvl], ";color:", if (lvl >= 4) "#fff" else "#1f2933"), t(CMP_LEVELS[lvl]))),
    div(class = "cmp-bar", div(class = "cmp-fill", style = paste0("width:", if (n > 1) round(100 * (rank - 1) / (n - 1)) else 50, "%;background:", CMP_COLOURS[lvl]))),
    div(class = "cmp-rate", paste0(fmt_r(rate, lang), " ‰")),
    div(class = "cmp-sub", unit_label),
    div(class = "cmp-sub", diff_text, if (n > 1) paste0(" · ", t("cmp_rank", rank, n, group_label)), if (!is.null(band)) paste0(" · ", band)),
    if (!is.na(n_events) && n_events < MIN_EVENTS_STABLE) div(class = "cmp-sub text-warning-emphasis", t("cmp_unstable", n_events)))
}

# Area (km2) and density of one territory from its polygon, for the profile line
area_density <- function(geo, id, population, lang) {
  if (is.null(geo) || !"territory_id" %in% names(geo)) return(NULL)
  g <- geo[geo$territory_id == id, ]; if (nrow(g) == 0) return(NULL)
  km2 <- as.numeric(sf::st_area(sf::st_make_valid(g[1, ]))) / 1e6
  if (!is.finite(km2) || km2 <= 0) return(NULL)
  TR("area_density", lang, fmt_r(km2, lang), if (is.na(population) || population <= 0) "—" else fmt_n(round(population / km2), lang))
}

# "The 10 % of <units> with most <events> hold X % of them (a of b)": concentration of a vector of counts
concentration_text <- function(n, unit_label, noun, lang) {
  n <- n[!is.na(n)]; if (length(n) < 10 || sum(n) <= 0) return(NULL)
  n <- sort(n, decreasing = TRUE); k <- max(1, ceiling(0.1 * length(n)))
  TR("concentration", lang, 10, unit_label, noun, round(100 * sum(n[seq_len(k)]) / sum(n)), k, length(n))
}

# --- Styles -----------------------------------------------------------------------
CSS <- "
  .bslib-page-navbar .bslib-sidebar-layout > .main { padding: 0; }
  .tab-pane[data-value='stats'] { padding: 1rem 1.25rem !important; }
  #map { height: 100% !important; min-height: 420px; }
  /* The button that hides and shows the sidebar: a visible tab above the map (leaflet controls sit at z-index 1000) */
  .bslib-sidebar-layout > .collapse-toggle { z-index: 1100; background: #fff; border: 1px solid rgba(0,0,0,.3); border-radius: .5rem; box-shadow: 0 1px 5px rgba(0,0,0,.3); width: 1.9rem; height: 2.8rem; display: flex; align-items: center; justify-content: center; color: #222; }
  .bslib-sidebar-layout > .collapse-toggle:hover { background: #f1f3f5; }
  .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle { left: .35rem; }
  .bslib-sidebar-layout.sidebar-collapsed > .main > *:first-child { margin-left: 2.6rem; }   /* room for the button next to the first title */
  .bslib-sidebar-layout > .sidebar > .sidebar-content { padding: .75rem .85rem; gap: .5rem; }
  .sidebar .form-group, .sidebar .shiny-input-container { margin-bottom: .25rem; }
  .sidebar .control-label { font-weight: 600; font-size: .85rem; margin-bottom: .1rem; }
  .sidebar .accordion-button { padding: .55rem 1.6rem !important; font-weight: 600; font-size: .9rem; }
  .sidebar .accordion-body { padding: .3rem 1.6rem .8rem !important; }
  .sidebar .shiny-input-container:has(#neighbourhood) { width: calc(100% - 1.8rem) !important; }
  .info { cursor: help; color: #64748b; font-weight: 400; margin-left: .25rem; }
  .foot { font-size: .72rem; color: #64748b; margin-top: auto; }
  .facet { font-weight: 600; font-size: .85rem; margin: .5rem 0 .15rem; display: flex; justify-content: space-between; }
  .facet a { font-weight: 400; font-size: .78rem; }
  .cnt { color: #64748b; font-size: .78rem; margin-left: .2rem; }
  #types .checkbox, #block .radio { margin: 0 0 .1rem; }
  #types label, #block label { font-size: .85rem; }
  .chips a { font-size: .78rem; margin-right: .6rem; }
  .chips a.active { font-weight: 700; text-decoration: none; color: #1f2933; }
  .filter-summary { font-size: .75rem; color: #475569; }
  .profile-head { display: flex; justify-content: space-between; align-items: start; }
  .profile h4 { margin: 0; font-size: 1.15rem; }
  .profile .sub { color: #64748b; font-size: .8rem; margin-bottom: .5rem; }
  .kpis { display: flex; gap: .4rem; margin-bottom: .6rem; }
  .kpi { flex: 1; background: #f1f5f9; border-radius: .5rem; padding: .4rem .5rem; }
  .kpi .v { font-size: 1.2rem; font-weight: 650; line-height: 1.15; }
  .kpi .l { font-size: .7rem; color: #475569; }
  .profile h6 { font-size: .82rem; font-weight: 650; margin: .6rem 0 .2rem; }
  .profile .table { font-size: .8rem; margin-bottom: .3rem; }
  .profile .alert { font-size: .76rem; padding: .35rem .55rem; margin: .35rem 0; }
  .profile details { font-size: .78rem; margin-top: .35rem; }
  .profile details ul { padding-left: 1.1rem; margin: .3rem 0; }
  .note { font-size: .76rem; color: #64748b; }
  .cmp { background: #f1f5f9; border-radius: .5rem; padding: .5rem .65rem; margin: .4rem 0 .6rem; }
  .cmp-head { display: flex; justify-content: space-between; align-items: center; margin-bottom: .35rem; }
  .cmp-title { font-size: .72rem; color: #475569; text-transform: uppercase; letter-spacing: .04em; }
  .cmp-pill { font-size: .78rem; font-weight: 700; padding: .1rem .55rem; border-radius: 999px; }
  .cmp-bar { height: 8px; background: #e2e8f0; border-radius: 999px; overflow: hidden; }
  .cmp-fill { height: 8px; border-radius: 999px; }
  .cmp-rate { text-align: center; font-weight: 700; font-size: 1.15rem; margin-top: .35rem; }
  .cmp-sub { text-align: center; font-size: .72rem; color: #64748b; }
  .table-sp { max-height: 68vh; overflow-y: auto; }
  .table-sp table { font-size: .82rem; }
  .table-sp thead th { position: sticky; top: 0; background: #fff; }
  .table-sp tr.ref td { font-weight: 650; background: #f1f5f9; }
  .table-sp tr.sel td { background: #fff7ed; font-weight: 650; }
  .table-sp a { cursor: pointer; }
  #sp_types .checkbox { margin: 0 0 .1rem; }
  #sp_types label { font-size: .85rem; }
  body.mode-stats .map-only, body.mode-stats .accordion-item[data-value='profile'] { display: none !important; }
  body.mode-stats .bslib-sidebar-layout > .sidebar > .sidebar-content { padding-top: 2.4rem; }
  .navbar .form-select { width: auto; font-size: .85rem; padding: .2rem 1.8rem .2rem .5rem; }
  .navbar .dropdown-menu a.nav-link { padding: .3rem 1rem; }
  .about { max-width: 960px; margin: 0 auto; padding: 1rem 1rem 3rem; }
  .about h2 { font-size: 1.15rem; margin: 1.8rem 0 .6rem; }
  .about p, .about li { font-size: .92rem; line-height: 1.5; }
  .about li { margin-bottom: .4rem; }
  .about table { font-size: .82rem; }
  .about table td, .about table th { vertical-align: top; padding: .35rem .5rem; }
  .about .upd { color: #666; font-size: .8rem; margin-top: 2rem; }
"
JS <- "Shiny.addCustomMessageHandler('mode', function(m) { document.body.classList.toggle('mode-stats', m === 'stats'); });
Shiny.addCustomMessageHandler('chip', function(id) {
  document.querySelectorAll('.chips a').forEach(function(a) { a.classList.toggle('active', a.id === id); });
});"

# --- Navbar: view switcher (Spain | Cities) and language selector ---------------------------
nav_switcher <- function(view, lang, cities, countries = COUNTRIES) {
  t <- function(k, ...) TR(k, lang, ...)
  url <- function(v, l = lang) paste0("?view=", v, "&lang=", l)
  link <- function(href, text, active) tags$a(href = href, text, class = "nav-link", style = if (active) "font-weight:700" else NULL)
  select <- tags$select(class = "form-select form-select-sm", `aria-label` = t("language"),
                        onchange = sprintf("window.location.href='?view=%s&lang=' + this.value", view),
                        lapply(names(LANGUAGES), function(l) tags$option(value = l, selected = if (l == lang) NA else NULL, LANGUAGES[[l]])))
  cname <- function(cc) if (cc %in% names(countries)) countries[[cc]]$CNAME(lang) else cc
  city_label <- function(c) { cc <- sub("-.*$", "", cities[[c]]$CITY_ID); if (length(countries) > 1) paste0(cities[[c]]$CITY_NAME, " · ", cname(cc)) else cities[[c]]$CITY_NAME }
  list(
    bslib::nav_spacer(),
    if (length(countries) == 1) bslib::nav_item(link(url(names(countries)), cname(names(countries)), view == names(countries)))
    else if (length(countries) > 1) bslib::nav_menu(if (view %in% names(countries)) cname(view) else t("nav_country"), !!!lapply(names(countries), function(cc)
      bslib::nav_item(link(url(cc), cname(cc), view == cc)))),
    if (has_rows(CRATES)) bslib::nav_item(link(url("countries"), t("nav_countries"), view == "countries")),
    if (length(cities) > 0) bslib::nav_menu(t("nav_cities"), !!!lapply(names(cities), function(c)
      bslib::nav_item(link(url(c), city_label(c), view == c)))),
    bslib::nav_item(link(url("about"), t("nav_about"), view == "about")),
    bslib::nav_item(select)
  )
}

# =============================================================================
# City functions. Copied into one environment per city (make_city), so that
# `base`, `geo_nb`, `THRESHOLD`, `tr`... are that city's. Madrid and Barcelona
# share all the code.
# =============================================================================

figure <- function(n, lang) ifelse(is.na(n), tr("less_than", lang, THRESHOLD), fmt_n(n, lang))
type_name <- function(x, lang) {
  n <- unname(if (lang == (meta$lang %||% "es")) NAME_ES[x] else NAME_EN[x])
  if (length(n) != length(x)) n <- rep(NA_character_, length(x))
  ifelse(!is.na(n) & nzchar(ifelse(is.na(n), "", n)), n, readable_type(x))
}
month_labels <- function(lang) fmt_month(MONTHS, lang)

# Period-slider views: which months the slider spans in each one
#   year = the latest year with data; five = the last 5 calendar years; all = the whole series
VIEWS <- c("year", "five", "all")
view_months <- function(view) {
  y <- as.integer(format(MONTHS, "%Y")); last <- max(y)
  switch(view, year = which(y == last), five = which(y >= last - 4), seq_along(MONTHS))
}
view_label <- function(view, lang) {
  last <- format(max(MONTHS), "%Y")
  switch(view, year = if (last == format(Sys.Date(), "%Y")) tr("view_this_year", lang) else tr("view_latest_year", lang, last),
         five = tr("view_five", lang), tr("view_all", lang))
}

# Counts and rates by neighbourhood, district and city for a block and month
# range, with the threshold applied. `types`: ticked types (NULL = all)
compute <- function(block, range, types = NULL) {
  d <- base[base$block == block & base$month >= range[1] & base$month <= range[2], ]
  if (!is.null(types)) d <- d[d$call_type %in% types, ]
  rate <- function(n, id) if (HAS_RATES) ifelse(is.na(n), NA, 1000 * n / pop_of[id]) else NA
  nb <- d |> filter(!is.na(neighbourhood_id)) |> group_by(territory_id = neighbourhood_id) |> summarise(raw = sum(events), .groups = "drop")
  nb <- data.frame(territory_id = geo_nb$territory_id) |> left_join(nb, by = "territory_id") |>
    mutate(raw = coalesce(raw, 0L), n = apply_threshold(raw, THRESHOLD), rate = rate(n, territory_id))
  di <- d |> filter(!is.na(district_id)) |> group_by(territory_id = district_id) |> summarise(raw = sum(events), .groups = "drop")
  di <- data.frame(territory_id = geo_di$territory_id) |> left_join(di, by = "territory_id") |>
    mutate(raw = coalesce(raw, 0L), n = apply_threshold(raw, THRESHOLD), rate = rate(n, territory_id), name = unname(name_of[territory_id]))
  n_city <- sum(d$events)
  list(neighbourhoods = nb, districts = di, city = list(n = n_city, rate = rate(n_city, CITY_ID)))
}

# --- City interface ----------------------------------------------------------------
ui_city <- function(lang) {
  t <- function(k, ...) tr(k, lang, ...)
  labels <- month_labels(lang)
  start <- labels[view_months("year")]          # on opening: year view, last 3 months selected

  selector <- selectizeInput(
    "neighbourhood", NULL, width = "100%",
    choices = c(setNames("", paste("\U0001F50D", t("search_hint"))),
                setNames(geo_nb$territory_id, paste0(geo_nb$name, " (", geo_nb$district, ")"))[order(geo_nb$name)]),
    # With a neighbourhood chosen the box is "full": on opening it is set aside (the
    # profile stays) and, if closed without choosing, the previous one comes back.
    options = list(
      onDropdownOpen  = I("function() { if (this.getValue()) { this._prev = this.getValue(); this.clear(true); } }"),
      onDropdownClose = I("function() { var s = this; setTimeout(function() { if (!s.getValue() && s._prev) s.setValue(s._prev, true); }, 0); }")))

  filters <- tagList(
    div(class = "facet", span(t("category"), info(paste(vapply(BLOCKS, function(b) t(paste0("desc_", b)), ""), collapse = " ")))),
    radioButtons("block", NULL, choiceNames = lapply(BLOCKS, block_name, lang), choiceValues = BLOCKS),
    div(class = "facet", span(t("call_types")), span(actionLink("types_all", t("all")), " · ", actionLink("types_none", t("none")))),
    checkboxGroupInput("types", NULL, choices = character(0)),
    uiOutput("types_summary"),
    div(class = "facet", span(t("period"))),
    div(class = "chips", lapply(VIEWS, function(v) actionLink(paste0("view_", v), view_label(v, lang), class = if (v == "year") "active"))),
    shinyWidgets::sliderTextInput("period", NULL, choices = start, selected = start[c(max(1, length(start) - 2), length(start))],
                                  grid = FALSE, force_edges = TRUE, width = "100%"),
    div(class = "map-only",
        radioButtons("shade", tagList(t("shade_by"), info(HTML(t("shade_help")))), inline = TRUE,
                     choices = shade_options(lang, HAS_NB_RATES), selected = shade_default(HAS_NB_RATES))),
    if (!HAS_RATES) div(class = "alert alert-warning small py-2 mb-0", HTML(t("missing_population")))
  )

  bslib::page_navbar(
    title = paste(t("app_title"), "·", CITY_NAME), lang = lang, theme = bslib::bs_theme(version = 5), id = "nav",
    header = tags$head(tags$style(HTML(CSS)), tags$script(HTML(JS))),
    sidebar = bslib::sidebar(
      width = 340, open = "desktop",
      div(class = "map-only", selector),
      bslib::accordion(id = "acc", multiple = TRUE, open = "filters",
                       bslib::accordion_panel(t("filters"), value = "filters", filters),
                       bslib::accordion_panel(t("profile"), value = "profile", uiOutput("profile"))),
      div(class = "foot", t("what_shows"), tags$br(), t("footer"))
    ),
    bslib::nav_panel(t("nav_map"), value = "map", leafletOutput("map", height = "100%")),
    bslib::nav_panel(
      t("nav_stats"), value = "stats",
      bslib::layout_columns(col_widths = c(6, 6), fill = FALSE,
        radioButtons("stats_level", t("stats_level"), inline = TRUE, choices = setNames(c("district", "neighbourhood"), c(t("col_district"), t("col_neighbourhood"))), selected = STATS_LEVEL),
        radioButtons("sort", t("sort_by"), inline = TRUE,
                     choices = if (HAS_RATES) setNames(c("rate", "n"), c(t("sort_rate"), t("sort_number"))) else setNames("n", t("sort_number")))),
      uiOutput("stats_title"),
      bslib::layout_columns(col_widths = c(7, 5), fill = FALSE, plotOutput("stats_plot", height = "300px"), tableOutput("stats_table")),
      uiOutput("stats_notes"),
      tags$hr(), uiOutput("season_title"),
      bslib::layout_columns(col_widths = c(7, 5), fill = FALSE, plotOutput("season_plot", height = "260px"), uiOutput("season_notes")),
      lapply(panel_ids("city"), function(id) tagList(
        tags$hr(), uiOutput(paste0("pn_", id, "_title")),
        bslib::layout_columns(col_widths = c(5, 7), fill = FALSE, div(class = "table-sp", uiOutput(paste0("pn_", id, "_table"))),
                              div(uiOutput(paste0("pn_", id, "_evol_head")), plotOutput(paste0("pn_", id, "_evol"), height = "260px"))),
        uiOutput(paste0("pn_", id, "_notes"))))
    ),
    !!!nav_switcher(CITY, lang, CITIES)
  )
}

# --- City logic ----------------------------------------------------------------------
server_city <- function(input, output, session, lang) {
  t <- function(k, ...) tr(k, lang, ...)
  labels <- month_labels(lang)

  range <- debounce(reactive({
    r <- MONTHS[match(input$period, labels)]
    if (length(r) != 2 || anyNA(r)) r <- MONTHS[c(max(1, length(MONTHS) - 2), length(MONTHS))]
    sort(r)
  }), 300)

  observeEvent(input$nav, {
    session$sendCustomMessage("mode", input$nav)
    if (identical(input$nav, "stats")) bslib::accordion_panel_open("acc", "filters", session = session)
  })

  # Only types with at least one event in the chosen period are listed
  type_counts <- reactive({
    r <- range()
    d <- base[base$block == input$block & base$month >= r[1] & base$month <= r[2], ]
    n <- tapply(d$events, d$call_type, sum)
    n <- n[!is.na(n) & n > 0]
    sort(n, decreasing = TRUE)
  })
  block_types <- reactive(names(type_counts()))
  ready <- reactiveVal(FALSE)
  prev_block <- reactiveVal(NULL)
  prev_types <- reactiveVal(character(0))

  observeEvent(list(input$block, type_counts()), {
    n <- type_counts()
    block_changed <- !identical(prev_block(), input$block)
    ticked <- isolate(input$types) %||% character(0)
    all_before <- !ready() || setequal(ticked, prev_types())
    sel <- if (block_changed || all_before) names(n) else intersect(ticked, names(n))
    updateCheckboxGroupInput(session, "types", selected = sel,
      choiceNames = lapply(names(n), function(x) count_label(type_name(x, lang), n[[x]], lang, type_note(x, lang))),
      choiceValues = names(n))
    prev_types(names(n)); prev_block(input$block); ready(TRUE)
  })

  observeEvent(range(), {
    r <- range()
    d <- base[base$month >= r[1] & base$month <= r[2], ]
    n <- tapply(d$events, d$block, sum)[BLOCKS]; n[is.na(n)] <- 0
    updateRadioButtons(session, "block", selected = isolate(input$block),
      choiceNames = lapply(seq_along(BLOCKS), function(i) count_label(block_name(BLOCKS[i], lang), n[[i]], lang)), choiceValues = BLOCKS)
  })

  observeEvent(input$types_all,  updateCheckboxGroupInput(session, "types", selected = names(type_counts())))
  observeEvent(input$types_none, updateCheckboxGroupInput(session, "types", selected = character(0)))
  change_view <- function(v) {
    e <- labels[view_months(v)]
    shinyWidgets::updateSliderTextInput(session, "period", choices = e, selected = e[c(1, length(e))])
    session$sendCustomMessage("chip", paste0("view_", v))
  }
  lapply(VIEWS, function(v) observeEvent(input[[paste0("view_", v)]], change_view(v)))

  selected_types <- reactive({ if (!ready()) return(block_types()); intersect(input$types %||% character(0), block_types()) })
  all_ticked <- reactive(setequal(selected_types(), block_types()))
  output$types_summary <- renderUI({
    if (all_ticked()) return(NULL)
    div(class = "filter-summary", t("n_of_types", length(selected_types()), length(block_types())))
  })
  data <- reactive(compute(input$block, range(), selected_types()))
  selection_text <- reactive({
    n <- length(selected_types())
    if (all_ticked()) block_name(input$block, lang) else if (n == 1) type_name(selected_types(), lang)
    else paste0(block_name(input$block, lang), " (", t("n_of_types", n, length(block_types())), ")")
  })

  # --- Map ---
  output$map <- renderLeaflet({
    bb <- sf::st_bbox(geo_nb)
    leaflet(options = leafletOptions(minZoom = 10)) |>
      addTiles(attribution = paste(ATTRIBUTION_OSM, "·", t("boundaries_by", boundaries_credit(lang)))) |>
      layer_neighbourhoods(isolate(data()), isolate(input$shade) %||% shade_default(HAS_NB_RATES), isolate(selection_text()), isolate(range()), lang) |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })
  observeEvent(list(data(), input$shade), {
    leafletProxy("map") |>
      layer_neighbourhoods(data(), input$shade %||% shade_default(HAS_NB_RATES), selection_text(), range(), lang) |>
      layer_selection(isolate(input$neighbourhood))
  }, ignoreInit = TRUE)
  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    if (!is.null(id)) updateSelectizeInput(session, "neighbourhood", selected = id)
  })
  observeEvent(input$neighbourhood, {
    p <- layer_selection(leafletProxy("map"), input$neighbourhood)
    if (nzchar(input$neighbourhood)) {
      bslib::accordion_panel_open("acc", "profile", session = session)
      bslib::accordion_panel_close("acc", "filters", session = session)
      bb <- sf::st_bbox(geo_nb[geo_nb$territory_id == input$neighbourhood, ])
      p |> flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]], options = list(maxZoom = 14, padding = c(60, 60)))
    } else bslib::accordion_panel_open("acc", "filters", session = session)
  }, ignoreInit = TRUE)
  observeEvent(input$close_profile, updateSelectizeInput(session, "neighbourhood", selected = ""))

  # --- Neighbourhood profile ---
  output$profile <- renderUI({
    id <- input$neighbourhood
    if (!nzchar(id)) return(div(class = "profile", p(class = "note", t("profile_empty")), reading_block(lang, open = TRUE)))
    if (length(selected_types()) == 0) return(div(class = "profile", p(class = "note", t("no_type_selected"))))
    r <- range(); d <- data()
    row <- d$neighbourhoods[d$neighbourhoods$territory_id == id, ]
    dis_id <- parent_of[[id]]
    types <- base[base$neighbourhood_id %in% id & base$block == input$block & base$call_type %in% selected_types() &
                    base$month >= r[1] & base$month <= r[2], ] |>
      group_by(call_type) |> summarise(n = sum(events), .groups = "drop") |> mutate(n = apply_threshold(n, THRESHOLD)) |> arrange(is.na(n), desc(n))
    ratio <- if (HAS_RATES) row$rate / d$city$rate else NA
    div(class = "profile",
      div(class = "profile-head", h4(name_of[[id]]), actionLink("close_profile", HTML("&#10005;"), title = t("close_profile"), class = "text-muted")),
      div(class = "sub", if (!SINGLE_DISTRICT) tagList(t("district_of", name_of[[dis_id]]), " · "), selection_text(), " · ", fmt_range(r, lang)),
      div(class = "kpis",
          kpi(figure(row$n, lang), t("k_calls")),
          if (HAS_RATES) kpi(if (is.na(row$rate)) "—" else fmt_r(row$rate, lang), t("k_rate")),
          if (HAS_RATES) kpi(if (is.na(ratio)) "—" else sprintf(t("times"), fmt_r(ratio, lang)), tagList(t("k_ratio"), info(t("lq_note"))))),
      if (HAS_RATES) comparison_block(row$rate, d$city$rate, d$neighbourhoods$rate, t("city"), paste(t("k_calls"), t("k_rate")), t("cmp_neighbourhoods"), lang, n_events = row$n),
      { ad <- area_density(geo_nb, id, unname(pop_of[id]), lang); if (!is.null(ad)) div(class = "sub", ad) },
      if (!is.na(ratio) && ratio >= 2) div(class = "alert alert-info", HTML(t("ratio_note", fmt_r(ratio, lang)))),
      if (HAS_RATES && isTRUE(pop_of[id] < 5000)) div(class = "alert alert-secondary", t("few_residents")),
      if (!is.na(row$n) && row$n < MIN_EVENTS_STABLE) div(class = "alert alert-secondary", t("few_events", MIN_EVENTS_STABLE)),
      if (HAS_SURVEY) survey_block(dis_id, lang),
      if (nzchar(NOTE_BREAK$block) && identical(input$block, NOTE_BREAK$block) && r[1] < as.Date(NOTE_BREAK$date) && r[2] >= as.Date(NOTE_BREAK$date))
        div(class = "alert alert-warning", t(NOTE_BREAK$key)),
      if (nzchar(NOTE_TERRITORY$pattern) && grepl(NOTE_TERRITORY$pattern, name_of[[id]], ignore.case = TRUE)) div(class = "alert alert-warning", t(NOTE_TERRITORY$key)),
      rates_block(id, dis_id, d, lang),
      h6(t(if (TIME_UNIT == "year") "arrests_by_year" else "by_month"), info(t("noise_note"))),
      plotOutput("evolution", height = "120px"),
      h6(t("types_in_period")),
      if (nrow(types) == 0) p(class = "note", t("no_calls")) else
        tags$table(class = "table table-sm", tags$tbody(lapply(seq_len(nrow(types)), function(i)
          tags$tr(tags$td(type_name(types$call_type[i], lang),
                          if (!is.na(type_note(types$call_type[i], lang))) info(type_note(types$call_type[i], lang))),
                  tags$td(class = "text-end", figure(types$n[i], lang)))))),
      lapply(panel_ids("district"), function(id) panel_block(id, dis_id, r, lang)),
      reading_block(lang), source_block(lang)
    )
  })

  output$evolution <- renderPlot({
    req(nzchar(input$neighbourhood), length(selected_types()) > 0)
    r <- range(); months <- MONTHS[MONTHS >= r[1] & MONTHS <= r[2]]
    d <- base[base$neighbourhood_id %in% input$neighbourhood & base$block == input$block & base$call_type %in% selected_types() & base$month %in% months, ] |>
      group_by(month) |> summarise(n = sum(events), .groups = "drop")
    n <- d$n[match(months, d$month)]; n[is.na(n)] <- 0
    low <- n < THRESHOLD; height <- ifelse(low, THRESHOLD, n)
    op <- par(mar = c(1.8, 2.6, 0.3, 0.3), las = 1, cex.axis = 0.75, mgp = c(2, 0.4, 0)); on.exit(par(op))
    b <- barplot(height, names.arg = if (TIME_UNIT == "year") format(months, "%Y") else month_names(lang)[as.integer(format(months, "%m"))], col = ifelse(low, NA, "#64748b"),
                 border = ifelse(low, "#94a3b8", NA), density = ifelse(low, 15, -1), angle = 45, ylim = c(0, max(height, THRESHOLD) * 1.18))
    if (any(low)) text(b[low], height[low], paste0("<", THRESHOLD), pos = 3, cex = 0.65, col = "#475569")
  }, res = 96)

  # --- Statistics: the top districts (5) or neighbourhoods (10) by number or rate, same filters ---
  top5 <- reactive({
    d <- data(); by <- if (identical(input$sort, "rate") && HAS_RATES) "rate" else "n"
    lvl <- input$stats_level %||% STATS_LEVEL
    di <- if (lvl == "neighbourhood") { x <- d$neighbourhoods; x$name <- paste0(unname(name_of[x$territory_id]), " (", unname(name_of[unname(parent_of[x$territory_id])]), ")"); x } else d$districts
    di <- di[!is.na(di[[by]]), ]
    di <- di[order(-di[[by]]), ][seq_len(min(if (lvl == "neighbourhood") 10 else 5, nrow(di))), ]
    list(districts = di, city = d$city, by = by, level = lvl)
  })
  output$stats_title <- renderUI({
    if (length(selected_types()) == 0) return(p(class = "note", t("no_type_selected")))
    h5(class = "mt-2 mb-1", tags$b(t(if (identical(input$stats_level %||% STATS_LEVEL, "neighbourhood")) "top_neighbourhoods" else "top5")), " · ", selection_text(), " · ", fmt_range(range(), lang))
  })
  # --- Seasonal profile at city level: average events by calendar month over the complete years, for the chosen types ---
  season <- reactive({
    req(length(selected_types()) > 0)
    if (TIME_UNIT == "year") return(NULL)                     # no calendar months in a yearly source
    d <- base[base$block == input$block & base$call_type %in% selected_types(), ]
    if (nrow(d) == 0) return(NULL)
    m <- d |> group_by(month) |> summarise(n = sum(events), .groups = "drop")
    m$year <- as.integer(format(m$month, "%Y")); m$mo <- as.integer(format(m$month, "%m"))
    full <- names(which(table(m$year) == 12)); m <- m[m$year %in% as.integer(full), ]
    if (nrow(m) == 0) return(NULL)
    avg <- m |> group_by(mo) |> summarise(n = mean(n), .groups = "drop")
    list(avg = avg, years = sort(as.integer(full)), total = sum(m$n))
  })
  output$season_title <- renderUI({ s <- season(); if (is.null(s)) return(NULL); h5(class = "mt-2 mb-1", tags$b(t("seasonal_title")), " · ", selection_text(), " · ", if (length(s$years) == 1) s$years else paste(min(s$years), "–", max(s$years))) })
  output$season_plot <- renderPlot({
    s <- season(); req(!is.null(s))
    op <- par(mar = c(3, 4, 0.5, 0.5), las = 1); on.exit(par(op))
    v <- s$avg$n[match(1:12, s$avg$mo)]; v[is.na(v)] <- 0
    b <- barplot(v, names.arg = substr(month_names(lang, TRUE), 1, 3), col = "#4e79a7", border = NA, ylab = "")
    abline(h = mean(v), lty = 2, col = "#555")
  }, res = 96)
  output$season_notes <- renderUI({ s <- season(); if (is.null(s)) return(NULL)
    v <- s$avg$n[match(1:12, s$avg$mo)]; hi <- which.max(v); lo <- which.min(v)
    p(class = "note", t("seasonal_note", length(s$years), month_names(lang, FALSE)[hi], fmt_r(100 * (v[hi] / mean(v, na.rm = TRUE) - 1), lang), month_names(lang, FALSE)[lo], fmt_r(100 * (1 - v[lo] / mean(v, na.rm = TRUE)), lang)), " ", info(t("seasonal_info")))
  })
  output$stats_plot <- renderPlot({
    x <- top5(); req(nrow(x$districts) > 0, length(selected_types()) > 0)
    v <- rev(x$districts[[x$by]]); lbl <- rev(x$districts$name)
    ref <- if (x$by == "rate") x$city$rate else NA
    op <- par(mar = c(3, if (x$level == "neighbourhood") 13 else 9.5, 0.5, 3), las = 1, cex.axis = if (x$level == "neighbourhood") 0.8 else 1); on.exit(par(op))
    b <- barplot(v, names.arg = lbl, horiz = TRUE, col = "#3182bd", border = NA, xlim = c(0, max(v, ref, na.rm = TRUE) * 1.15))
    text(v, b, if (x$by == "rate") fmt_r(v, lang) else fmt_n(v, lang), pos = 4, cex = 0.85)
    if (!is.na(ref)) { abline(v = ref, lty = 2, col = "#475569"); mtext(t("city_average", fmt_r(ref, lang)), side = 1, line = 2, at = ref, cex = 0.8, col = "#475569") }
  }, res = 96)
  output$stats_table <- renderTable({
    x <- top5(); req(nrow(x$districts) > 0, length(selected_types()) > 0)
    out <- data.frame(seq_len(nrow(x$districts)), x$districts$name, figure(x$districts$n, lang))
    names(out) <- c(t("col_rank"), t(if (x$level == "neighbourhood") "col_neighbourhood" else "col_district"), t("col_calls"))
    if (HAS_RATES) {
      out[[t("col_rate")]]  <- ifelse(is.na(x$districts$rate), "—", fmt_r(x$districts$rate, lang))
      out[[t("col_ratio")]] <- ifelse(is.na(x$districts$rate), "—", sprintf(t("times"), fmt_r(x$districts$rate / x$city$rate, lang)))
    }
    out
  }, striped = TRUE, spacing = "s", align = "l")
  output$stats_notes <- renderUI({
    req(length(selected_types()) > 0); x <- top5()
    conc <- concentration_text(if (x$level == "neighbourhood") data()$neighbourhoods$n else data()$districts$n, t(if (x$level == "neighbourhood") "cmp_neighbourhoods" else "cmp_districts"), t("k_calls"), lang)
    tagList(
      if (!is.null(conc)) p(class = "note mt-2", conc, " ", info(t("concentration_note"))),
      if (HAS_RATES) p(class = "note", t("lq_table_note")),
      p(class = "note mt-2", t("stats_note", fmt_n(x$city$n, lang), if (HAS_RATES) t("stats_total_rate", fmt_r(x$city$rate, lang)) else "")))
  })

  # --- Panels of kind "city" (whole-city figures by indicator, e.g. Madrid arrests by motive) ---
  for (id in panel_ids("city")) local({
    pid <- id; p <- panel_meta(pid); out <- function(x) paste0("pn_", pid, "_", x)
    V <- panel_values(pid); IND <- panel_indicators(pid); V <- V[V$territory_id == CITY_ID, ]
    iname <- function(x) panel_names(IND, lang)[x]
    first <- if (nzchar(p$default_indicator) && p$default_indicator %in% IND$indicator) p$default_indicator else IND$indicator[!IND$is_total][1]
    sel_ind <- reactiveVal(first)
    observeEvent(input[[out("click")]], sel_ind(input[[out("click")]]))
    pmonths <- sort(unique(V$month))
    pperiod <- reactive({
      r <- range(); sel <- pmonths[pmonths >= r[1] & pmonths <= r[2]]
      if (length(sel) == 0) sel <- utils::tail(pmonths, max(1, sum(MONTHS >= r[1] & MONTHS <= r[2])))
      sel
    })
    output[[out("title")]] <- renderUI(h5(class = "mt-2 mb-1", tags$b(panel_text(p, "title_key", panel_name(p, lang), lang)), " · ", t("city"), " · ", fmt_range(base::range(pperiod()), lang)))
    output[[out("table")]] <- renderUI({
      sel <- pperiod(); d <- V[V$month %in% sel, ]
      n <- tapply(d$value, d$indicator, sum)
      totals <- IND$indicator[IND$is_total]
      tot <- if (length(totals) && totals[1] %in% names(n)) n[[totals[1]]] else sum(n[!names(n) %in% totals])
      n <- sort(n[!names(n) %in% totals], decreasing = TRUE)
      prev <- V[V$month < min(sel) & V$month >= min(sel) - 366, ]
      np <- if (nrow(prev) > 0) tapply(prev$value, prev$indicator, sum) * length(sel) / length(unique(prev$month)) else NULL
      row <- function(m) tags$tr(class = if (identical(m, sel_ind())) "sel",
        tags$td(tags$a(onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", out("click"), m), iname(m) %||% m)),
        tags$td(class = "text-end", fmt_n(n[[m]], lang)),
        tags$td(class = "text-end text-muted", style = "white-space:nowrap", if (tot > 0) paste0(fmt_r(100 * n[[m]] / tot, lang), " %") else "—"),
        tags$td(class = "text-end text-muted", if (is.null(np) || is.na(np[m])) "—" else fmt_n(round(np[[m]]), lang)))
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(panel_text(p, "col_key", t("panel_col_indicator"), lang)), tags$th(class = "text-end", panel_text(p, "value_key", t("panel_col_value"), lang)), tags$th(class = "text-end", "%"),
                           tags$th(class = "text-end", t("arrests_col_before"), info(t("arrests_before_help"))))),
        tags$tbody(lapply(names(n), row), tags$tr(class = "ref", tags$td(t("panel_total")), tags$td(class = "text-end", fmt_n(tot, lang)), tags$td(), tags$td())))
    })
    output[[out("evol_head")]] <- renderUI(h6(class = "mb-0", t("arrests_by_year"), " · ", iname(sel_ind()) %||% sel_ind()))
    output[[out("evol")]] <- renderPlot({
      d <- V[V$indicator == sel_ind(), ]; req(nrow(d) > 0); d <- d[order(d$month), ]
      year <- as.integer(format(d$month, "%Y")); v <- tapply(d$value, year, sum); nm <- tapply(d$month, year, length)
      op <- par(mar = c(2.4, 3.2, 0.5, 0.5), las = 1, cex.axis = 0.8, mgp = c(2, 0.5, 0)); on.exit(par(op))
      b <- barplot(v, names.arg = ifelse(nm < 12, paste0(names(v), "*"), names(v)), col = ifelse(nm < 12, "#9ecae1", "#3182bd"), border = NA, ylim = c(0, max(v) * 1.15))
      text(b, v, fmt_n(v, lang), pos = 3, cex = 0.7, col = "#475569")
    }, res = 96)
    output[[out("notes")]] <- renderUI(div(class = "note mt-1", if (nzchar(p$note_key)) p(HTML(t(p$note_key))) else p(panel_measure(p, lang)), if (nzchar(p$note2_key)) p(t(p$note2_key))))
  })
}

# --- Map layers --------------------------------------------------------------------
PALETTE <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")   # one hue, no reds
ATTRIBUTION_OSM <- "&copy; <a href=\"https://www.openstreetmap.org/copyright\">OpenStreetMap</a>"

shade_options <- function(lang, has_rates = TRUE) {
  op <- c(rate = TR("shade_rate", lang), n = TR("shade_number", lang))
  if (!has_rates) op <- op["n"]
  setNames(names(op), unname(op))
}
shade_default <- function(has_rates) if (has_rates) "rate" else "n"

# Quintile palette (5 groups with the same number of areas) and legend format
quintile_scale <- function(v, mode, lang) {
  cuts <- stats::quantile(v, probs = seq(0, 1, 0.2), na.rm = TRUE)
  if (mode == "n") cuts <- c(floor(cuts[1]), round(cuts[2:5]), ceiling(cuts[6]))
  cuts <- unique(cuts)
  if (length(cuts) < 2) cuts <- range(v, na.rm = TRUE) + c(0, 1e-9)
  pal <- colorBin(PALETTE[seq_len(length(cuts) - 1)], domain = v, bins = cuts, na.color = "#e5e7eb")
  f <- if (mode == "n") function(x) fmt_n(round(x), lang) else function(x) fmt_r(x, lang)
  list(pal = pal, format = function(type, cuts, p) { n <- length(cuts); paste0(f(cuts[-n]), " – ", f(cuts[-1])) })
}
LABEL_OPTIONS <- labelOptions(direction = "auto", textsize = "13px", style = list(padding = "6px 8px", "line-height" = "1.35"))

neighbourhood_labels <- function(d, selection, range, lang) {
  b <- d$neighbourhoods[match(geo_nb$territory_id, d$neighbourhoods$territory_id), ]
  lapply(seq_len(nrow(geo_nb)), function(i) HTML(paste0(
    "<b>", htmltools::htmlEscape(geo_nb$name[i]), "</b> · ", htmltools::htmlEscape(geo_nb$district[i]),
    "<br>", tr("hover_calls", lang, figure(b$n[i], lang)),
    if (HAS_RATES) paste0(" · ", if (is.na(b$rate[i])) "—" else tr("hover_rate", lang, fmt_r(b$rate[i], lang))) else "",
    "<br><span style='color:#64748b;font-size:11px'>", htmltools::htmlEscape(selection), " · ", fmt_range(range, lang), "</span>")))
}

layer_neighbourhoods <- function(map, d, shade, selection, range, lang) {
  map <- map |> clearGroup("neighbourhoods") |> clearGroup("districts") |> removeControl("legend")
  lbl <- neighbourhood_labels(d, selection, range, lang)
  k <- match(geo_nb$territory_id, d$neighbourhoods$territory_id)
  mode <- if (identical(shade, "rate") && !HAS_RATES) "n" else shade
  v <- switch(mode, rate = d$neighbourhoods$rate[k], n = d$neighbourhoods$n[k], NULL)
  if (!is.null(v) && sum(!is.na(v)) >= 2) {
    sc <- quintile_scale(v, mode, lang)
    map <- map |>
      addPolygons(data = geo_nb, layerId = ~territory_id, group = "neighbourhoods", color = "#4a4a4a", weight = 0.8, opacity = 0.8,
                  fillColor = sc$pal(v), fillOpacity = 0.7, label = lbl, labelOptions = LABEL_OPTIONS,
                  highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)) |>
      addLegend("bottomright", pal = sc$pal, values = v, layerId = "legend",
                title = HTML(paste0(tr(if (mode == "n") "legend_number" else "legend_rate", lang), "<br><small>", htmltools::htmlEscape(selection), "</small>")),
                labFormat = sc$format, na.label = tr("legend_na", lang, THRESHOLD), opacity = 0.8)
  } else {
    map <- map |> addPolygons(data = geo_nb, layerId = ~territory_id, group = "neighbourhoods", color = "#4a4a4a", weight = 1, opacity = 0.8,
                              fillColor = "#9aa5b1", fillOpacity = 0.12, label = lbl, labelOptions = LABEL_OPTIONS,
                              highlightOptions = highlightOptions(weight = 3, fillOpacity = 0.25, bringToFront = TRUE))
  }
  map |> addPolylines(data = geo_di, group = "districts", color = "#1f2933", weight = 2.5, opacity = 0.9, options = pathOptions(interactive = FALSE))
}

layer_selection <- function(map, id) {
  map <- map |> clearGroup("sel")
  if (is.null(id) || !nzchar(id)) return(map)
  map |> addPolygons(data = geo_nb[geo_nb$territory_id == id, ], group = "sel", color = "#c2410c", weight = 5, fill = FALSE,
                     dashArray = "6 4", options = pathOptions(interactive = FALSE))
}

# --- Profile blocks -----------------------------------------------------------------
rates_block <- function(id, dis_id, d, lang) {
  t <- function(k, ...) tr(k, lang, ...)
  if (!HAS_RATES) return(p(class = "note", t("no_population_short")))
  b <- d$neighbourhoods[d$neighbourhoods$territory_id == id, ]; di <- d$districts[d$districts$territory_id == dis_id, ]
  nb_pop <- unname(pop_of[id]); if (is.na(nb_pop)) b$rate <- NA     # no register for this neighbourhood: its row shows no rate
  maxv <- suppressWarnings(max(c(b$rate, di$rate, d$city$rate), na.rm = TRUE))
  bar <- function(lbl, v, strong = FALSE, na_text = NULL) tags$tr(
    tags$td(style = "width:38%", if (strong) tags$b(lbl) else lbl),
    tags$td(style = "width:44%;vertical-align:middle", if (is.na(v)) span(class = "note", na_text %||% t("less_than_calls", THRESHOLD)) else
      div(style = paste0("height:10px;border-radius:3px;background:", if (strong) "#3182bd" else "#94a3b8", ";width:", max(2, round(100 * v / maxv)), "%"))),
    tags$td(class = "text-end", style = "width:18%", if (is.na(v)) "—" else fmt_r(v, lang)))
  tagList(
    h6(t("rates_title"), info(if (is.na(nb_pop)) t("no_population_short") else t("residents", fmt_n(nb_pop, lang), fmt_date(meta$population_date, lang)))),
    tags$table(class = "table table-sm table-borderless", tags$tbody(
      bar(name_of[[id]], b$rate, TRUE, na_text = if (is.na(nb_pop)) t("no_population_nb") else NULL), if (!SINGLE_DISTRICT) bar(t("district_x", name_of[[dis_id]]), di$rate), bar(t("city"), d$city$rate))))
}

# Credit of the boundaries under the map: the city's own text when translations.csv has one, else the module's attribution
boundaries_credit <- function(lang) {
  k <- paste0("boundaries_source@", CITY)
  if (!identical(TR(k, "en"), k)) tr("boundaries_source", lang) else if (nzchar(meta$boundaries_attribution %||% "")) meta$boundaries_attribution else tr("boundaries_source", lang)
}

# --- Panels (P-035): a city's extra monthly datasets, declared by its module (mart_city_panels) ----------
panel_ids <- function(kind) if (has_rows(PANELS)) PANELS$panel_id[PANELS$kind == kind] else character()
panel_meta <- function(id) PANELS[PANELS$panel_id == id, ][1, ]
panel_values <- function(id) PANEL_VALUES[PANEL_VALUES$panel_id == id, ]
panel_indicators <- function(id) { x <- PANEL_IND[PANEL_IND$panel_id == id, ]; x[order(x$is_total, x$sort_order), ] }
# A text of the panel: its translation key when the module names one, else the default
panel_text <- function(p, field, default, lang) { k <- p[[field]] %||% ""; if (nzchar(k) && !identical(TR(k, "en"), k)) tr(k, lang) else default }
panel_name <- function(p, lang) if (lang == (meta$lang %||% "es")) p$name_local else p$name_en
panel_measure <- function(p, lang) p$measure
panel_names <- function(ind, lang) {
  n <- ifelse(nzchar(ind$name_key) & vapply(ind$name_key, function(k) nzchar(k) && !identical(TR(k, "en"), k), logical(1)),
              vapply(ind$name_key, function(k) if (nzchar(k)) TR(k, lang) else "", ""), if (lang == (meta$lang %||% "es")) ind$name_local else ind$name_en)
  setNames(n, ind$indicator)
}

# Kind "district": the neighbourhood's district against the city for the same period, by indicator
panel_block <- function(id, dis_id, r, lang) {
  t <- function(k, ...) tr(k, lang, ...)
  p <- panel_meta(id); V <- panel_values(id); IND <- panel_indicators(id)
  ptext <- function(field, default) panel_text(p, field, default, lang)
  mm <- sort(unique(V$month)); sel <- mm[mm >= r[1] & mm <= r[2]]; warn <- NULL
  if (length(sel) == 0) { sel <- utils::tail(mm, max(1, sum(MONTHS >= r[1] & MONTHS <= r[2]))); if (length(sel)) warn <- ptext("other_period_key", t("panel_other_period", fmt_range(range(sel), lang))) }
  if (length(sel) == 0) return(NULL)
  if (!is.null(warn) && nzchar(p$other_period_key)) warn <- t(p$other_period_key, fmt_range(range(sel), lang))
  d <- V[V$month %in% sel, ]
  totals <- IND$indicator[IND$is_total]; regular <- IND$indicator[!IND$is_total]
  sum_by <- function(x) { v <- tapply(x$value, x$indicator, sum); v[c(regular, totals)] }
  in_dis <- sum_by(d[d$territory_id == dis_id, ]); in_city <- sum_by(d)   # the city: every row (districts plus what has no district, under the city's id)
  p_dis <- pop_of[dis_id]; p_city <- pop_of[CITY_ID]
  rate <- function(n, p) if (is.na(p) || is.na(n)) NA else 1000 * n / p
  names_of <- panel_names(IND, lang)
  shown <- regular[!is.na(in_dis[regular])]
  if (!is.na(p$top_n) && length(shown) > p$top_n) { x <- in_dis[shown]; x[is.na(x)] <- 0; shown <- shown[order(-x)][seq_len(p$top_n)] }
  if (length(shown) == 0 && length(totals) == 0) return(NULL)
  row <- function(k, strong = FALSE) {
    n <- apply_threshold(in_dis[[k]], THRESHOLD); nc <- in_city[[k]]
    tags$tr(tags$td(if (strong) tags$b(names_of[[k]]) else names_of[[k]]), tags$td(class = "text-end", figure(n, lang)),
            tags$td(class = "text-end", if (is.na(n) || !HAS_RATES) "—" else fmt_r(rate(n, p_dis), lang)),
            tags$td(class = "text-end text-muted", if (!HAS_RATES || is.na(nc)) "—" else fmt_r(rate(nc, p_city), lang)))
  }
  tagList(
    h6(ptext("title_key", panel_name(p, lang)), info(ptext("note_key", p$measure))),
    div(class = "sub note", name_of[[dis_id]], " · ", fmt_range(range(sel), lang)),
    if (!is.null(warn)) p(class = "note", warn),
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th(ptext("col_key", t("panel_col_indicator"))), tags$th(class = "text-end", t("mossos_col_district")),
                         tags$th(class = "text-end", t("mossos_col_rate")), tags$th(class = "text-end", t("mossos_col_city")))),
      tags$tbody(lapply(shown, row), lapply(totals[!is.na(in_dis[totals])], row, strong = TRUE))))
}

reading_block <- function(lang, open = FALSE) {
  l <- c(tr("reading_1", lang), if (HAS_RATES) tr("reading_2", lang), tr("reading_3", lang), tr("reading_4", lang, THRESHOLD, THRESHOLD))
  tags$details(open = if (open) NA else NULL, tags$summary(tr("how_to_read", lang)), tags$ul(lapply(l, function(x) tags$li(HTML(x)))))
}

source_block <- function(lang) {
  t <- function(k, ...) tr(k, lang, ...)
  tags$details(tags$summary(t("source_title")), tags$ul(
    tags$li(t("source_name"), " · ", t("licence", meta$licence), " · ", tags$a(href = meta$url, target = "_blank", t("portal"))),
    tags$li(t("data_from", fmt_month(meta$first_month, lang), fmt_month(meta$last_month, lang), fmt_date(meta$loaded_at, lang))),
    tags$li(t("pct_without_neighbourhood", fmt_r(meta$pct_without_neighbourhood, lang))),
    if (HAS_RATES) tags$li(t("source_register", fmt_date(meta$population_date, lang))),
    lapply(panel_ids("district"), function(id) { p <- panel_meta(id); m0 <- fmt_month(as.Date(p$first_month), lang); m1 <- fmt_month(as.Date(p$last_month), lang)
      tags$li(if (nzchar(p$source_key) && !identical(TR(p$source_key, "en"), p$source_key)) t(p$source_key, m0, m1) else t("panel_source", panel_name(p, lang), p$publisher, m0, m1),
              " · ", tags$a(href = p$url, target = "_blank", if (nzchar(p$source_label)) p$source_label else sub("^https?://([^/]+).*$", "\\1", p$url))) })))
}

CITY_FUNCTIONS <- c("figure", "type_name", "month_labels", "view_months", "view_label", "compute", "ui_city", "server_city",
                    "neighbourhood_labels", "layer_neighbourhoods", "layer_selection", "rates_block", "panel_ids", "panel_meta", "panel_values",
                    "panel_indicators", "panel_text", "panel_name", "panel_measure", "panel_names", "panel_block", "reading_block", "source_block", "survey_block", "boundaries_credit")

# Prepare the environment of a city (NULL if data or boundaries are missing)
make_city <- function(key) {
  x <- RAW[[key]]; f <- CITY_SOURCES[[key]]
  if (!has_rows(x$base) || is.null(x$meta) || !file.exists(path(f$gpkg))) return(NULL)
  app_env <- environment(make_city)
  e <- new.env(parent = app_env)
  e$CITY <- key; e$CITY_ID <- f$id; e$CITY_NAME <- unname(name_of[f$id]) %||% x$meta$name_en %||% key
  e$base <- x$base; e$meta <- x$meta
  # Extra panels the module declares (mart_city_panels, P-035), and the notes it asks for
  e$PANELS <- if (has_rows(CITY_PANELS)) CITY_PANELS[CITY_PANELS$city == key, ] else NULL
  e$PANEL_VALUES <- if (has_rows(CITY_PANEL_VALUES)) CITY_PANEL_VALUES[CITY_PANEL_VALUES$city == key, ] else data.frame(panel_id = character(), month = as.Date(character()), territory_id = character(), indicator = character(), value = integer())
  e$PANEL_IND <- if (has_rows(CITY_PANEL_IND)) CITY_PANEL_IND[CITY_PANEL_IND$city == key, ] else data.frame(panel_id = character(), indicator = character(), name_local = character(), name_en = character(), name_key = character(), sort_order = integer(), is_total = logical())
  e$STATS_LEVEL <- if (identical(x$meta$stats_level, "neighbourhood")) "neighbourhood" else "district"   # level the statistics tab ranks by default
  e$TIME_UNIT <- if (identical(x$meta$time_unit, "year")) "year" else "month"                               # a yearly source: "months" are 1 January of each year
  e$MEASURE_KIND <- x$meta$measure_kind %||% "calls"                                                        # which set of texts (key@kind) the app falls back to
  if (e$TIME_UNIT == "year") {                                                                                # yearly cities print years wherever a month would appear
    e$fmt_month <- function(d, lang) format(as.Date(d), "%Y")
    e$fmt_range <- function(r, lang) if (format(r[1], "%Y") == format(r[2], "%Y")) format(r[1], "%Y") else paste(format(r[1], "%Y"), "–", format(r[2], "%Y"))
  }
  e$SURVEY <- if (has_rows(CITY_SURVEY)) CITY_SURVEY[CITY_SURVEY$city == key, ] else NULL
  e$SURVEY_META <- if (has_rows(CITY_SURVEY_META) && key %in% CITY_SURVEY_META$city) CITY_SURVEY_META[CITY_SURVEY_META$city == key, ][1, ] else NULL
  e$HAS_SURVEY <- has_rows(e$SURVEY)
  e$NOTE_TERRITORY <- list(pattern = x$meta$note_territory_pattern %||% "", key = x$meta$note_territory_key %||% "")
  e$NOTE_BREAK <- list(block = x$meta$note_break_block %||% "", date = x$meta$note_break_date %||% "", key = x$meta$note_break_key %||% "")
  types <- x$types %||% data.frame(call_type = character(), name_es = character(), name_en = character())
  e$NAME_EN <- setNames(types$name_en, types$call_type); e$NAME_ES <- setNames(types$name_es, types$call_type)
  geo <- sf::st_read(path(f$gpkg), quiet = TRUE)
  e$geo_nb <- geo[geo$level == "neighbourhood", ]; e$geo_di <- geo[geo$level == "district", ]
  e$geo_nb$district <- unname(name_of[e$geo_nb$parent_id])
  e$SINGLE_DISTRICT <- nrow(e$geo_di) <= 1          # a city whose source has no level between the city and the neighbourhoods (Paris): no district rows
  pop <- POP_ALL[POP_ALL$territory_id == f$id | startsWith(POP_ALL$territory_id, paste0(f$id, "-")), ]
  e$HAS_RATES <- nrow(pop) > 0 && f$id %in% pop$territory_id
  e$pop_of <- if (e$HAS_RATES) setNames(pop$population, pop$territory_id) else c()
  e$HAS_NB_RATES <- e$HAS_RATES && any(e$geo_nb$territory_id %in% names(e$pop_of))   # a register by district only (Mexico City): no neighbourhood rates, no shading by rate
  e$BLOCKS <- MARTS$blocks[MARTS$blocks %in% unique(x$base$block)]   # only the categories this city's source publishes
  e$THRESHOLD <- x$meta$threshold[1] %||% MARTS$threshold
  e$MONTHS <- sort(unique(x$base$month))
  e$tr <- function(key, lang, ...) TR(key, lang, ..., city = CITY, kind = MEASURE_KIND)
  environment(e$tr) <- e
  for (fn in CITY_FUNCTIONS) { g <- get(fn, envir = app_env); environment(g) <- e; assign(fn, g, envir = e) }
  e
}

# =============================================================================
# Country view: recorded crime by region, province and municipality from each
# country's official source. A country may carry a second source (Spain: the
# Mossos d'Esquadra by police area) shown as a switch of source inside the
# territory it covers. The functions below run inside one environment per
# country (make_country), where CC, CB, CB_ALL, POPM, GEO_*, SP_LEVELS... are that country's.
# =============================================================================

# --- City survey block: the latest value of each indicator for the district and the city, and the last years ---
survey_block <- function(dis_id, lang) {
  t <- function(k, ...) TR(k, lang, ..., city = CITY, kind = MEASURE_KIND)
  s <- SURVEY; if (!has_rows(s)) return(NULL)
  loc <- function(en, local) if (lang == "es" || lang == "ca") local else en
  blocks <- lapply(unique(s$indicator), function(ind) {
    si <- s[s$indicator == ind, ]; d <- si[si$territory_id == dis_id, ]; c0 <- si[si$territory_id == CITY_ID, ]
    if (nrow(d) == 0 && nrow(c0) == 0) return(NULL)
    years <- sort(unique(c(d$year, c0$year)), decreasing = TRUE)[1:min(5, length(unique(c(d$year, c0$year))))]
    val <- function(x, y) { v <- x$value[x$year == y]; if (length(v)) fmt_r(v[1], lang) else "—" }
    last <- years[1]
    u <- if (startsWith(si$unit[1] %||% "", "%")) " %" else paste0(" ", si$unit[1])
    tagList(
      h6(class = "mt-3 mb-1", loc(si$name_en[1], si$name_local[1]), " ", info(loc(si$definition_en[1], si$definition_local[1]))),
      div(class = "sub", t("survey_latest", last, val(d, last), u, val(c0, last))),
      tags$table(class = "table table-sm mt-1", tags$thead(tags$tr(tags$th(""), lapply(years, function(y) tags$th(class = "text-end", y)))),
        tags$tbody(tags$tr(tags$td(name_of[[dis_id]] %||% dis_id), lapply(years, function(y) tags$td(class = "text-end", val(d, y)))),
                   tags$tr(class = "ref", tags$td(CITY_NAME), lapply(years, function(y) tags$td(class = "text-end", val(c0, y)))))))
  })
  blocks <- Filter(Negate(is.null), blocks); if (!length(blocks)) return(NULL)
  div(class = "survey mt-2", h5(class = "mb-0", t("survey_title"), info(t("survey_note"))), blocks,
      if (!is.null(SURVEY_META)) p(class = "note", SURVEY_META$publisher, if (nzchar(SURVEY_META$url %||% "")) tagList(" · ", tags$a(href = SURVEY_META$url, target = "_blank", t("more_info")))))
}

# Offence types that can be chosen (no sensitive ones) of a type table, its aggregates first
cb_types <- function(lang, tt = CB_TYPE_TABLE, aggregates = AGGREGATES) {
  x <- tt
  if (nrow(x) == 0) return(character())
  x$name <- if (lang == CLANG) x$name_local else x$name_en
  pri <- match(x$offence_type_id, aggregates)
  x <- x[order(is.na(pri), pri, x$name), ]
  setNames(x$offence_type_id, x$name)
}

# Periods: full years (4th quarter) and the latest partial year, for a reference territory of a source's rows
cb_periods <- function(ref_id, lang, d = CB) {
  d <- d[d$territory_id == ref_id & d$offence_type_id == "total" & !is.na(d$quarter), c("year", "quarter")]
  if (nrow(d) == 0) return(character())
  last <- tapply(d$quarter, d$year, max); years <- as.integer(names(last))
  lbl <- ifelse(last == 4, sprintf(TR("sp_year", lang), years), sprintf(TR("sp_partial", lang), years, month_names(lang, TRUE)[last * 3]))
  rev(setNames(paste0(years, "|", last), lbl))
}

ui_country_controls <- function(lang) {
  t <- function(k, ...) TR(k, lang, ...)
  per <- cb_periods(CC, lang); full <- per[grepl("\\|4$", per)]
  tagList(
    if (HAS_MAP) checkboxInput("sp_follow", tagList(t("sp_follow"), info(t("sp_follow_help"))), value = TRUE),
    radioButtons("sp_level", t("sp_level"), choiceNames = lapply(SP_LEVELS, function(l) LEVEL_LABEL(l, lang)), choiceValues = SP_LEVELS, selected = SP_LEVELS[length(SP_LEVELS)]),
    conditionalPanel("input.sp_level == 'municipality' && !input.sp_follow",
      selectInput("sp_region", LEVEL_LABEL("region", lang, col = TRUE), choices = SP_REGIONS, selected = DEFAULT_REGION, width = "100%"),
      # Second sources: a switch shown inside the territory each one covers (a region, or the whole country)
      if (nrow(ALT_SOURCES) > 0) {
        alts <- ALT_SOURCES[ALT_SOURCES$covers == CC | ALT_SOURCES$covers %in% SP_REGIONS, ]
        if (nrow(alts) > 0) {
          cond <- paste(ifelse(alts$covers == CC, "true", sprintf("input.sp_region == '%s'", alts$covers)), collapse = " || ")
          help <- meta_text(alts[1, ], "help", lang, CLANG, default = t("sp_source_switch_help"))
          conditionalPanel(cond,
            radioButtons("sp_source", tagList(t("sp_source"), info(HTML(help))),
                         choiceNames = c(list(meta_text(META, "short_name", lang, CLANG, default = LOC("source_name", lang))),
                                         lapply(seq_len(nrow(alts)), function(i) meta_text(alts[i, ], "short_name", lang, CLANG, default = LOC("source_name", lang, alts[i, ])))),
                         choiceValues = c("main", alts$source), selected = "main"))
        }
      }),
    uiOutput("sp_types_head"),
    checkboxGroupInput("sp_types", NULL, choices = character(0)),
    uiOutput("sp_types_summary"),
    div(class = "facet", span(t("sp_period"), info(t("sp_period_help")))),
    div(class = "chips",
        actionLink("sp_view_latest", t("view_latest_year", sub("\\|.*$", "", if (length(full)) full[1] else per[1])), class = "active"),
        actionLink("sp_view_five", t("view_five")), actionLink("sp_view_all", t("view_all"))),
    shinyWidgets::sliderTextInput("sp_period", NULL, choices = rev(names(per)), selected = rep(names(if (length(full)) full[1] else per[1]), 2),
                                  grid = FALSE, force_edges = TRUE, width = "100%"),
    div(class = "facet", span(t("sp_territory"))),
    selectizeInput("sp_territory", NULL, choices = NULL, width = "100%"),
    uiOutput("sp_territory_info"),
    if (HAS_MAP)
      radioButtons("sp_shade", tagList(t("shade_by"), info(HTML(t("sp_shade_help")))), inline = TRUE,
                   choices = shade_options(lang, HAS_POPM), selected = shade_default(HAS_POPM)),
    radioButtons("sp_sort", t("sp_sort"), inline = TRUE, choices = setNames(c("name", "rate"), c(t("sp_sort_name"), t("sp_sort_rate")))),
    div(class = "foot", uiOutput("sp_foot", inline = TRUE), tags$br(), t("footer"))
  )
}

ui_country <- function(lang) {
  t <- function(k, ...) TR(k, lang, ...)
  bslib::page_navbar(
    title = paste(t("app_title"), "·", CNAME(lang)), lang = lang, theme = bslib::bs_theme(version = 5), id = "nav",
    header = tags$head(tags$style(HTML(CSS)), tags$script(HTML(JS))),
    sidebar = bslib::sidebar(width = 340, open = "desktop", ui_country_controls(lang)),
    bslib::nav_panel(CNAME(lang), value = CC,
      uiOutput("sp_title"),
      bslib::layout_columns(col_widths = c(5, 7), fill = FALSE,
        div(uiOutput("sp_profile"), uiOutput("sp_breakdown"), uiOutput("sp_evol_head"), plotOutput("sp_evol", height = "220px")),
        if (!HAS_MAP) div(class = "table-sp", uiOutput("sp_table")) else
          bslib::navset_underline(id = "sp_view",
            bslib::nav_panel(t("sp_map"), leafletOutput("sp_map", height = "66vh")),
            bslib::nav_panel(t("sp_table"), div(class = "table-sp", uiOutput("sp_table"))))),
      uiOutput("sp_notes")),
    !!!nav_switcher(CC, lang, CITIES)
  )
}

server_country <- function(input, output, session, lang) {
  t <- function(k, ...) TR(k, lang, ...)
  # The source shown: 'main', or a second source when the reader is at municipal level inside the territory it covers
  src <- reactive({
    s <- input$sp_source %||% "main"
    if (s == "main" || isTRUE(input$sp_follow) || !identical(input$sp_level, "municipality") || !s %in% ALT_SOURCES$source) return("main")
    cov <- ALT_SOURCES$covers[ALT_SOURCES$source == s][1]
    if (cov == CC || identical(input$sp_region, cov)) s else "main"
  })
  alt <- reactive(src() != "main")
  SM <- reactive(SRC_META[SRC_META$source == src(), ][1, ])                    # metadata row of the source shown
  CBS <- reactive(if (alt()) CB_ALL[CB_ALL$source == src(), ] else CB)         # its rows
  top_id <- reactive(if (alt()) SM()$covers else CC)                           # the territory everything is compared with
  alt_level <- reactive(if (alt()) strsplit(SM()$levels, ";")[[1]][1] else NULL)
  eff_level <- reactive(if (alt()) alt_level() else input$sp_level)            # the level listed and mapped
  tt <- function(k, ...) TRS(k, lang, SM(), ...)                               # texts that depend on what the source counts
  type_table <- reactive(if (alt()) TYPE_TABLE(src()) else CB_TYPE_TABLE)
  aggregates <- reactive(AGGREGATES_OF(type_table()))
  visible_ids <- reactiveVal(NULL)     # territories inside the map view (NULL = all)
  auto_sel <- reactiveVal(NULL)        # last territory selected by the app itself (not by the user): no flight to it
  # Types as in the cities: a category (radio) and its types (checkboxes with counts).
  # All ticked = the official aggregate of the category (it also includes the sensitive
  # types that are never listed); a subset = the sum of the ticked types.
  type_tree <- reactive({
    tt <- type_table(); nm <- if (lang == CLANG) tt$name_local else tt$name_en
    aggregates <- aggregates()
    # Leaf types: those directly inside an aggregate (a sub-type such as residential burglary,
    # already counted in burglary, is left out so that nothing is added twice)
    i <- which(tt$included_in %in% aggregates & !tt$offence_type_id %in% aggregates)
    list(blocks = c(total = nm[match("total", tt$offence_type_id)]), leaves = list(total = setNames(tt$offence_type_id[i], nm[i])))
  })
  block_leaves <- reactive({ tr <- type_tree(); tr$leaves$total %||% character() })
  types_ready <- reactiveVal(FALSE)
  pending_types <- reactiveVal(NULL)   # boxes to tick after a list change (click on a type not yet listed)
  listed_types <- reactiveVal(character(0))   # types currently listed (to tell "all ticked" from a partial choice)
  listed_src <- reactiveVal("main")           # the source the list was built for: a change of source ticks everything again
  # Cumulative figure of each type for the reference territory in the period (for the counts)
  type_counts <- reactive({
    per <- period(); lv <- block_leaves(); if (length(lv) == 0) return(numeric())
    d <- CBS(); d <- d[d$territory_id == top_id() & in_periods(d, per) & d$offence_type_id %in% lv, ]; n <- tapply(d$cumulative, d$offence_type_id, sum)
    n <- n[lv]; n[is.na(n)] <- 0; names(n) <- lv
    n
  })
  observeEvent(list(type_tree(), type_counts()), {
    lv <- block_leaves(); n <- type_counts()
    lv <- lv[n[lv] > 0]; n <- n[lv]                      # only the types with figures in the period (as in the cities)
    if (length(lv) == 0) { updateCheckboxGroupInput(session, "sp_types", choices = character(0), selected = character(0)); types_ready(FALSE); return() }
    ticked <- isolate(input$sp_types) %||% character(0); listed <- listed_types()
    # Everything ticked before (the official total) stays "everything" when the list changes with the period
    all_before <- length(listed) == 0 || all(listed %in% ticked) || !identical(listed_src(), src())
    sel <- unname(if (!is.null(pending_types()) && all(pending_types() %in% lv)) pending_types()
                  else if (!types_ready() || all_before || !any(ticked %in% lv)) lv else intersect(ticked, lv))
    pending_types(NULL); listed_types(lv); listed_src(src())
    o <- order(-n)
    updateCheckboxGroupInput(session, "sp_types", selected = sel,
      choiceNames = unname(lapply(lv[o], function(x) count_label(names(lv)[match(x, lv)], n[[x]], lang))), choiceValues = unname(lv[o]))
    types_ready(TRUE)
  })
  observeEvent(input$sp_types_all,  updateCheckboxGroupInput(session, "sp_types", selected = unname(block_leaves())))
  observeEvent(input$sp_types_none, updateCheckboxGroupInput(session, "sp_types", selected = character(0)))
  # What is selected: ids to add up, a key (the aggregate id or "custom") and a label
  sel_types <- reactive({
    tr <- type_tree(); b <- "total"; bl <- names(tr$blocks)[1]
    lv <- block_leaves(); n <- type_counts(); lv <- lv[n[lv] > 0]
    ticked <- if (types_ready()) intersect(input$sp_types %||% character(0), lv) else lv
    if (length(lv) == 0 || setequal(ticked, lv)) list(ids = b, key = b, label = bl, all = TRUE)
    else if (length(ticked) == 0) list(ids = character(0), key = "none", label = t("none"), all = FALSE)
    else if (length(ticked) == 1) list(ids = ticked, key = ticked, label = names(lv)[match(ticked, lv)], all = FALSE)
    else list(ids = ticked, key = "custom", label = paste0(bl, " (", t("n_of_types", length(ticked), length(lv)), ")"), all = FALSE)
  })
  output$sp_types_head <- renderUI({
    if (length(block_leaves()) == 0) return(NULL)
    div(class = "facet", span(t("sp_type"), info(t("sp_block_help"))), span(actionLink("sp_types_all", t("all")), " · ", actionLink("sp_types_none", t("none"))))
  })
  output$sp_types_summary <- renderUI({
    x <- sel_types(); lv <- block_leaves()
    if (x$all || length(lv) == 0) return(NULL)
    div(class = "filter-summary", t("n_of_types", length(x$ids), length(lv)))
  })
  period_map <- reactiveVal(NULL)      # slider label -> "year|quarter[|month]"
  # Clicking a type in the breakdown ticks it alone; clicking the total ticks everything
  observeEvent(input$sp_type_click, {
    tr <- type_tree(); id <- input$sp_type_click; lv <- block_leaves()
    if (id %in% lv) updateCheckboxGroupInput(session, "sp_types", selected = id)
    else if (id == "total") updateCheckboxGroupInput(session, "sp_types", selected = unname(lv))
    else { ttab <- type_table(); sub <- ttab$offence_type_id[!is.na(ttab$included_in) & ttab$included_in == id]; sub <- intersect(sub, lv); if (length(sub)) updateCheckboxGroupInput(session, "sp_types", selected = sub) }
  })
  # Follow mode at municipality level: every municipality of Spain in view, whatever its region
  all_munis <- reactive(isTRUE(input$sp_follow) && identical(input$sp_level, "municipality") && !is.null(GEO_MUNI))
  # The region of any territory, climbing the parents (works for every country's identifiers)
  region_of <- function(id) {
    r <- id
    for (i in 1:5) { lv <- unname(level_of[r]); done <- is.na(r) | is.na(lv) | lv == "region"; if (all(done)) break; r[!done] <- unname(parent_of[r[!done]]) }
    ifelse(!is.na(r) & unname(level_of[r]) == "region", r, NA)
  }
  ref_id <- reactive({
    if (alt()) top_id()
    else if (all_munis()) { id <- input$sp_territory; if (!is.null(id) && isTRUE(unname(level_of[id]) == "municipality")) region_of(id) %||% CC else CC }
    else if (identical(input$sp_level, "municipality")) input$sp_region %||% CC else CC
  })
  ref_name <- function(id) { nm <- unname(name_of[id]); nm[is.na(nm)] <- id[is.na(nm)]; ifelse(id == CC, CNAME(lang), pretty_name(nm)) }
  level_name <- function(level) LEVEL_LABEL(level, lang, col = TRUE)
  display_name <- function(ids, level) pretty_name(unname(name_of[ids]) %||% ids)

  # INE population of every territory for a year (the most recent available up to that year)
  pop_year <- function(year) {
    if (!HAS_POPM) return(NULL)
    y <- max(POPM$year[POPM$year <= year], min(POPM$year)); p <- POPM[POPM$year == y, ]
    list(year = y, pop = setNames(p$population, p$territory_id))
  }

  # Territories of the chosen level (or of the second source), types and periods
  observeEvent(list(input$sp_level, input$sp_region, input$sp_source, input$sp_follow), {
    req(input$sp_level)
    types_ready(FALSE)
    rows <- level_rows(eff_level(), input$sp_region)
    ids <- unique(rows$territory_id); nm <- display_name(ids, eff_level())
    o <- order(nm)
    # Default: the most populated territory with data
    p <- pop_year(max(CBS()$year)); best <- if (!is.null(p)) ids[which.max(unname(p$pop[ids]) %||% 0)] else ids[1]
    keep <- isolate(input$sp_territory)
    # When following the map, the list is written only by the map-view observer (what is in view)
    if (!isTRUE(input$sp_follow) || !HAS_MAP || alt()) {
      sel <- if (!is.null(keep) && keep %in% ids) keep else best %||% ids[1]; auto_sel(sel)
      updateSelectizeInput(session, "sp_territory", choices = setNames(ids[o], nm[o]), selected = sel, server = FALSE)
    }
    visible_ids(NULL)
    per <- cb_periods(top_id(), lang, CBS()); full <- per[grepl("\\|4$", per)]
    cur <- isolate(input$sp_period)
    sel <- if (!is.null(cur) && all(cur %in% names(per))) cur else rep(names(if (length(full)) full[1] else per[1]), 2)
    if (!identical(period_map(), per)) { period_map(per); shinyWidgets::updateSliderTextInput(session, "sp_period", choices = rev(names(per)), selected = sel) }
  })
  observeEvent(input$sp_click, updateSelectizeInput(session, "sp_territory", selected = input$sp_click))

  level_rows <- function(level, region) {
    d <- CBS()
    if (level == "municipality" && !all_munis()) d[d$level == "municipality" & startsWith(d$territory_id, paste0(region, "-")), ] else d[d$level == level, ]
  }
  # The slider selects a range of years (as the months in the cities). One year = the official
  # figure (January to the last quarter published); several years = their sum. A partial year is
  # only used on its own: inside a range it is left out.
  parse_period <- function(v) { p <- strsplit(v, "|", fixed = TRUE)[[1]]; list(year = as.integer(p[1]), quarter = as.integer(p[2]), to_month = if (length(p) >= 3) as.integer(p[3]) else as.integer(p[2]) * 3L) }
  period <- reactive({
    pm <- period_map() %||% cb_periods(top_id(), lang, CBS()); labels <- rev(names(pm))     # oldest first
    sel <- input$sp_period %||% character(); i <- match(sel, labels); req(length(i) >= 1, !anyNA(i))
    chosen <- labels[seq(min(i), max(i))]
    ps <- lapply(unname(pm[chosen]), parse_period)
    partial <- vapply(ps, function(q) q$to_month < 12, logical(1))
    dropped <- length(ps) > 1 && any(partial)
    if (dropped) ps <- ps[!partial]
    req(length(ps) >= 1)
    last <- ps[[length(ps)]]
    list(periods = data.frame(year = vapply(ps, `[[`, 1L, "year"), quarter = vapply(ps, `[[`, 1L, "quarter")),
         year = last$year, quarter = last$quarter, to_month = last$to_month, first_year = ps[[1]]$year, n = length(ps), dropped_partial = dropped)
  })
  # Rows of a Crime Balance (or ABP) table that fall in the chosen periods
  in_periods <- function(df, per) if ("quarter" %in% names(df)) paste(df$year, df$quarter) %in% paste(per$periods$year, per$periods$quarter) else df$year %in% per$periods$year
  period_text <- reactive({
    per <- period()
    if (per$n > 1) paste0(per$first_year, "–", per$year)
    else if (per$to_month >= 12) as.character(per$year) else paste0(month_names(lang, TRUE)[1], "–", month_names(lang, TRUE)[per$to_month], " ", per$year)
  })
  # Chips: latest full year, last five full years, every full year
  set_period <- function(n_years) {
    pm <- period_map() %||% cb_periods(top_id(), lang, CBS()); labels <- rev(names(pm))
    full <- labels[!vapply(lapply(unname(pm[labels]), parse_period), function(q) q$to_month < 12, logical(1))]
    if (length(full) == 0) full <- labels
    to <- full[length(full)]; from <- full[max(1, length(full) - n_years + 1)]
    shinyWidgets::updateSliderTextInput(session, "sp_period", selected = c(from, to))
  }
  observeEvent(input$sp_view_latest, { set_period(1); session$sendCustomMessage("chip", "sp_view_latest") })
  observeEvent(input$sp_view_five,   { set_period(5); session$sendCustomMessage("chip", "sp_view_five") })
  observeEvent(input$sp_view_all,    { set_period(99); session$sendCustomMessage("chip", "sp_view_all") })

  country_data <- reactive({
    req(input$sp_level); per <- period(); st <- sel_types(); type <- if (length(st$ids) == 0) "__none__" else st$ids
    sum_types <- function(d, keys) { if (length(type) == 1 && per$n == 1) return(d); d |> group_by(across(all_of(keys))) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame() }
    lvl <- eff_level(); d0 <- CBS()
    rows <- level_rows(lvl, input$sp_region)
    refs <- d0[d0$territory_id == top_id() | (d0$level == "region" & (all_munis() | d0$territory_id == ref_id())), ]
    d <- rbind(rows, refs); d <- d[d$offence_type_id %in% type & in_periods(d, per), ]
    d <- sum_types(d, c("territory_id", "level", "name", "parent_id"))
    p <- pop_year(per$year)
    d$population <- if (is.null(p)) NA else unname(p$pop[d$territory_id])
    d$n <- apply_threshold(d$cumulative, MARTS$threshold)
    d$name <- ifelse(d$territory_id == CC, CNAME(lang), ifelse(d$level == "region" & lvl != "region", ref_name(d$territory_id), display_name(d$territory_id, lvl)))
    d$rate <- ifelse(is.na(d$n) | is.na(d$population), NA, 1000 * d$n / d$population / per$n)   # yearly average over a range
    if (lvl == "municipality" && all_munis()) {          # each municipality against its own region
      ref <- d$rate[match(region_of(d$territory_id), d$territory_id)]
      d$ratio <- ifelse(d$level == "municipality", d$rate / ref, NA)
    } else {
      ref <- d$rate[d$territory_id == ref_id()]
      d$ratio <- if (length(ref) == 1 && !is.na(ref)) d$rate / ref else rep(NA_real_, nrow(d))
    }
    list(d = d, level = lvl, ref_id = ref_id(), ref_name = ref_name(ref_id()), per = per, pop_year = p$year, alt = alt(), src = SM(), types = st,
         x_ref = t("sp_x_country", if (ref_id() == CC) CNAME(lang) else lc(LEVEL_LABEL("region", lang, col = TRUE))),
         vs_ref = t("sp_vs_country", if (ref_id() == CC) CNAME(lang) else lc(LEVEL_LABEL("region", lang, col = TRUE))))
  })

  # A line of context about the chosen territory, when a module gave one (the municipalities of a police area...)
  output$sp_territory_info <- renderUI({
    req(input$sp_territory); if (!has_rows(CINFO)) return(NULL)
    i <- CINFO[CINFO$country == CC & CINFO$territory_id == input$sp_territory, ]; if (nrow(i) == 0) return(NULL)
    x <- if (lang == CLANG && !is.na(i$info_local[1]) && nzchar(i$info_local[1])) i$info_local[1] else i$info_en[1]
    div(class = "note mb-2", x)
  })
  output$sp_title <- renderUI({
    x <- country_data()
    h5(class = "mt-1 mb-2", tags$b(tt("sp_title")), " · ", x$types$label, " · ", period_text(), " · ",
       if (x$level %in% c("region", "province") || all_munis()) CNAME(lang) else x$ref_name)
  })
  output$sp_profile <- renderUI({
    x <- country_data(); id <- input$sp_territory; req(id)
    f <- x$d[x$d$territory_id == id, ]
    if (nrow(f) == 0) return(div(class = "profile", h4(display_name(id, x$level) %||% id), p(class = "note", t("sp_no_data"))))
    div(class = "profile",
      h4(f$name),
      div(class = "sub", x$types$label, " · ", period_text(), if (!is.na(f$population)) paste0(" · ", fmt_n(f$population, lang), " ", tolower(t("sp_col_pop")))),
      div(class = "kpis",
          kpi(if (is.na(f$n)) t("less_than", MARTS$threshold) else fmt_n(f$n, lang), tt("sp_k_n")),
          kpi(if (is.na(f$rate)) "—" else fmt_r(f$rate, lang), t("sp_k_rate")),
          if (id != x$ref_id) kpi(if (is.na(f$ratio)) "—" else sprintf(t("times"), fmt_r(f$ratio, lang)), tagList(x$vs_ref, info(t("lq_note"))))),
      if (id != x$ref_id) comparison_block(f$rate, x$d$rate[x$d$territory_id == x$ref_id][1], x$d$rate[x$d$level == x$level], x$ref_name,
                                           paste(tt("sp_k_n"), t("sp_k_rate")), lc(LEVEL_LABEL(x$level, lang)), lang, n_events = f$n),
      { ad <- area_density(GEO_LEVEL(x$level), id, f$population, lang); if (!is.null(ad)) div(class = "sub", ad) },
      if (!is.na(f$n) && f$n < MIN_EVENTS_STABLE) div(class = "alert alert-secondary small py-1", t("few_events", MIN_EVENTS_STABLE)),
      if (x$per$n == 1 && x$per$to_month < 12) div(class = "alert alert-secondary small py-1", t("sp_partial_note", month_names(lang, TRUE)[x$per$to_month])),
      if (x$per$n > 1) div(class = "alert alert-secondary small py-1", t("sp_range_note", x$per$n, x$per$year), if (x$per$dropped_partial) paste0(" ", t("sp_range_dropped"))))
  })

  # Breakdown of the chosen territory by offence type (same period), with the reference rate alongside
  output$sp_breakdown <- renderUI({
    x <- country_data(); id <- input$sp_territory; req(id); per <- x$per
    d <- CBS(); d <- d[d$territory_id %in% c(id, x$ref_id) & in_periods(d, per) & !d$sensitive, ]
    if (per$n > 1) d <- d |> group_by(territory_id, offence_type_id) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
    if (nrow(d[d$territory_id == id, ]) == 0) return(NULL)
    p <- pop_year(per$year); pop <- function(i) if (is.null(p)) NA else unname(p$pop[i])
    types <- cb_types(lang, type_table(), aggregates()); aggregates <- aggregates()
    m <- d[d$territory_id == id & !d$offence_type_id %in% aggregates & !is.na(d$cumulative), ]; m <- m[order(-m$cumulative), ]
    row <- function(type, strong = FALSE) {
      n_t <- apply_threshold(d$cumulative[d$territory_id == id & d$offence_type_id == type][1], MARTS$threshold)
      n_r <- d$cumulative[d$territory_id == x$ref_id & d$offence_type_id == type][1]
      rate_t <- if (is.na(n_t)) NA else 1000 * n_t / pop(id) / per$n; rate_r <- if (is.na(n_r)) NA else 1000 * n_r / pop(x$ref_id) / per$n
      tags$tr(class = if (type %in% x$types$ids) "sel" else if (strong) "ref",
              tags$td(tags$a(onclick = sprintf("Shiny.setInputValue('sp_type_click', '%s', {priority: 'event'})", type), names(types)[match(type, types)] %||% type)),
              tags$td(class = "text-end", if (is.na(n_t)) t("less_than", MARTS$threshold) else fmt_n(n_t, lang)),
              tags$td(class = "text-end", if (is.na(rate_t)) "—" else fmt_r(rate_t, lang)),
              tags$td(class = "text-end text-muted", if (is.na(rate_r)) "—" else fmt_r(rate_r, lang)))
    }
    note <- if (x$alt) meta_text(x$src, "breakdown_note", lang, CLANG, default = t("sp_breakdown_note")) else t("sp_breakdown_note")
    tagList(h6(class = "mt-2 mb-1", t("sp_breakdown"), info(note)),
      div(class = "table-sp", style = "max-height:none", tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(t("sp_col_type")), tags$th(class = "text-end", tt("sp_col_n")), tags$th(class = "text-end", t("sp_col_rate")), tags$th(class = "text-end", x$ref_name))),
        tags$tbody(lapply(m$offence_type_id, row), if (any(d$territory_id == id & d$offence_type_id == "total")) row("total", TRUE)))))
  })

  output$sp_evol_head <- renderUI({
    x <- country_data(); id <- input$sp_territory; req(id)
    nm <- x$d$name[x$d$territory_id == id][1] %||% display_name(id, x$level) %||% id
    tagList(h6(class = "mt-2 mb-0", t("sp_evolution"), " · ", x$types$label),
            div(class = "note", span(style = "display:inline-block;width:10px;height:10px;background:#3182bd;margin-right:4px"), nm,
                if (id != x$ref_id) tagList(span(style = "color:#475569;margin:0 4px 0 12px", HTML("- &#9679; -")), x$ref_name)))
  })
  output$sp_evol <- renderPlot({
    x <- country_data(); id <- input$sp_territory; req(id)
    ids <- unique(c(id, x$ref_id)); type <- x$types$ids
    d <- CBS(); d <- d[d$territory_id %in% ids & d$offence_type_id %in% type & d$quarter == 4, ]; req(nrow(d) > 0)
    d <- d |> group_by(territory_id, year) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
    d$pop <- vapply(seq_len(nrow(d)), function(i) { p <- pop_year(d$year[i]); if (is.null(p)) NA_real_ else unname(p$pop[d$territory_id[i]]) }, 1)
    d$rate <- 1000 * apply_threshold(d$cumulative, MARTS$threshold) / d$pop
    years <- sort(unique(d$year))
    m <- d$rate[d$territory_id == id][match(years, d$year[d$territory_id == id])]
    rr <- if (length(ids) > 1) d$rate[d$territory_id == ids[2]][match(years, d$year[d$territory_id == ids[2]])] else NA
    req(any(!is.na(c(m, rr))))
    op <- par(mar = c(2.2, 3, 0.6, 0.5), las = 1, cex.axis = 0.8, mgp = c(2, 0.5, 0)); on.exit(par(op))
    b <- barplot(ifelse(is.na(m), 0, m), names.arg = years, col = "#3182bd", border = NA, ylim = c(0, max(c(m, rr), na.rm = TRUE) * 1.2))
    if (length(ids) > 1) lines(b, rr, type = "b", pch = 19, lty = 2, col = "#475569")
  }, res = 96)

  output$sp_table <- renderUI({
    x <- country_data(); d <- x$d
    refs <- d[d$territory_id == CC | (d$level == "region" & x$level != "region" & d$territory_id == x$ref_id), ]
    refs <- refs[order(refs$territory_id != CC), ]
    if (x$level == "region") refs <- refs[refs$territory_id == CC, ]
    main <- d[d$level == x$level & !d$territory_id %in% refs$territory_id, ]
    if (isTRUE(input$sp_follow) && !is.null(visible_ids())) main <- main[main$territory_id %in% visible_ids(), ]
    main <- if (identical(input$sp_sort, "rate")) main[order(-main$rate, na.last = TRUE), ] else main[order(main$name), ]
    cell <- function(v) tags$td(class = "text-end", v)
    row <- function(r, cls = NULL, link = FALSE) tags$tr(class = cls,
      tags$td(if (link) tags$a(onclick = sprintf("Shiny.setInputValue('sp_click', '%s', {priority: 'event'})", r$territory_id), r$name) else r$name),
      cell(if (is.na(r$population)) "—" else fmt_n(r$population, lang)), cell(if (is.na(r$n)) t("less_than", MARTS$threshold) else fmt_n(r$n, lang)),
      cell(if (is.na(r$rate)) "—" else fmt_r(r$rate, lang)), cell(if (is.na(r$ratio) || r$territory_id == x$ref_id) "—" else sprintf(t("times"), fmt_r(r$ratio, lang))))
    if (nrow(main) == 0) return(p(class = "note", t("sp_no_data")))
    conc <- concentration_text(main$n, lc(LEVEL_LABEL(x$level, lang)), tt("sp_k_n"), lang)
    tagList(tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th(level_name(x$level)), tags$th(class = "text-end", t("sp_col_pop")), tags$th(class = "text-end", tt("sp_col_n")),
                         tags$th(class = "text-end", t("sp_col_rate")), tags$th(class = "text-end", x$x_ref, info(t("lq_note"))))),
      tags$tbody(lapply(seq_len(nrow(refs)), function(i) row(refs[i, ], "ref")),
                 lapply(seq_len(nrow(main)), function(i) row(main[i, ], if (identical(main$territory_id[i], input$sp_territory)) "sel", link = TRUE)))),
      if (!is.null(conc)) p(class = "note mt-1", conc, " ", info(t("concentration_note"))))
  })

  # --- Map ---
  if (HAS_MAP) {
    active_geo <- function(x) {
      # Following the map: every municipality of Spain with data (> 20,000 inh.), drawn once;
      # the smaller ones are not drawn (too many to redraw on every pan)
      if (x$level == "municipality" && all_munis()) GEO_MUNI_DATA else if (x$level == "municipality") GEO_MUNI[GEO_MUNI$region_id == x$ref_id, ] else GEO_LEVEL(x$level)
    }
    output$sp_map <- renderLeaflet({
      x <- isolate(country_data()); g <- active_geo(x); req(!is.null(g), nrow(g) > 0)
      # First view: the default region, even when every municipality of Spain is drawn
      rg <- if (x$ref_id != CC) x$ref_id else DEFAULT_REGION
      bb <- sf::st_bbox(if (x$level == "municipality" && all_munis()) GEO_MUNI[GEO_MUNI$region_id == rg, ] else g)
      m <- leaflet(options = leafletOptions(minZoom = 2, zoomSnap = 0.5, zoomDelta = 0.5)) |>
        addTiles(attribution = paste(ATTRIBUTION_OSM, "·", BOUNDARIES_TEXT_OF(lang, x$src))) |>
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      isolate(layer_country(m, x, input$sp_shade %||% shade_default(HAS_POPM), input$sp_territory))
    })
    # --- Follow the map (switch "sp_follow"): the level follows the zoom, the region
    #     follows the map centre, and the territory list and table only show what is in view ---
    # Level by the width of the view in degrees of longitude (depends on the zoom and the map size):
    # more than 11 degrees (all Spain) -> regions; 4-11 -> provinces; less -> municipalities
    level_from_zoom <- function(z) {
      px <- session$clientData[["output_sp_map_width"]] %||% 800
      w <- 360 / 2^z * px / 256
      lv <- if (w > ZOOM_WIDTHS[["region"]]) "region" else if (w > ZOOM_WIDTHS[["province"]]) "province" else "municipality"
      # Only the levels this country has (and has boundaries for)
      if (!lv %in% SP_LEVELS) { i <- match(lv, c("region", "province", "municipality")); cands <- c("region", "province", "municipality")[seq_len(i)]; lv <- rev(intersect(cands, SP_LEVELS))[1] %||% SP_LEVELS[1] }
      lv
    }
    region_at_centre <- function() {
      c <- input$sp_map_center; if (is.null(c) || is.null(GEO_REGION)) return(NULL)
      hit <- suppressMessages(sf::st_intersects(sf::st_sfc(sf::st_point(c(c$lng, c$lat)), crs = 4326), GEO_REGION))[[1]]
      if (length(hit) == 0) NULL else GEO_REGION$territory_id[hit[1]]
    }
    # A level or region change made with the controls (not by zooming or panning): fly to it
    manual_change <- function(x) {
      !isTRUE(input$sp_follow) || is.null(input$sp_map_zoom) ||
        !identical(x$level, level_from_zoom(input$sp_map_zoom)) ||
        (x$level == "municipality" && !all_munis() && !identical(region_at_centre(), x$ref_id))
    }
    prev_level <- reactiveVal(NULL)
    flown_at <- reactiveVal(Sys.time() - 100)   # when the app last moved the map itself: zoom events right after it are ignored
    observeEvent(list(country_data(), input$sp_shade, input$sp_territory), {
      x <- country_data(); g <- active_geo(x); req(!is.null(g))
      m <- layer_country(leafletProxy("sp_map"), x, input$sp_shade %||% shade_default(HAS_POPM), input$sp_territory)
      key <- paste(x$level, if (all_munis()) "all" else x$ref_id)
      if (!identical(prev_level(), key)) {
        if (!is.null(prev_level()) && manual_change(x)) { flown_at(Sys.time()); bb <- sf::st_bbox(g); m |> flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) }
        prev_level(key)
      }
    }, ignoreInit = TRUE)
    observeEvent(input$sp_map_zoom, {
      req(isTRUE(input$sp_follow), input$sp_level)
      if (as.numeric(Sys.time() - isolate(flown_at()), units = "secs") < 2.5) return()
      lvl <- level_from_zoom(input$sp_map_zoom)
      if (!identical(lvl, input$sp_level)) updateRadioButtons(session, "sp_level", selected = lvl)
    })
    bounds <- debounce(reactive(input$sp_map_bounds), 500)
    observeEvent(list(bounds(), country_data()), {
      b <- bounds(); x <- country_data(); req(b, isTRUE(input$sp_follow))
      bx <- BOXES[[x$level]]; req(bx)
      inside <- bx$territory_id[bx$xmax >= b$west & bx$xmin <= b$east & bx$ymax >= b$south & bx$ymin <= b$north]
      if (isTRUE(getOption("webarchymapp.debug"))) message("bounds ", paste(round(unlist(b[c("west","east","south","north")]), 2), collapse = " "), " level ", x$level, " inside ", length(inside))
      ids <- x$d$territory_id[x$d$level == x$level & x$d$territory_id %in% inside]
      if (length(ids) == 0) {                      # nothing with data in view: the territories of the regions in view (or the whole level)
        rb <- BOXES$region; rg <- rb$territory_id[rb$xmax >= b$west & rb$xmin <= b$east & rb$ymax >= b$south & rb$ymin <= b$north]
        ids <- x$d$territory_id[x$d$level == x$level]
        if (length(rg) > 0 && x$level != "region") ids <- ids[region_of(ids) %in% rg]
        if (length(ids) == 0) ids <- x$d$territory_id[x$d$level == x$level]
        visible_ids(NULL)
      } else visible_ids(ids)
      nm <- display_name(ids, x$level); o <- order(nm)
      # Keep the current territory if visible; after zooming out, its parent; otherwise the most populated in view
      current <- input$sp_territory; ancestors <- character()
      a <- current; while (!is.null(a) && !is.na(a) && nzchar(a)) { a <- unname(parent_of[a]); if (!is.na(a)) ancestors <- c(ancestors, a) }
      pick <- intersect(c(current, ancestors), ids)
      sel <- if (length(pick) > 0) pick[1] else ids[which.max(x$d$population[match(ids, x$d$territory_id)] %||% 0)]
      if (isTRUE(getOption("webarchymapp.debug"))) message("  -> list of ", length(ids), " (", x$level, "), selected ", sel)
      if (!identical(sel, current)) auto_sel(sel)
      updateSelectizeInput(session, "sp_territory", choices = setNames(ids[o], nm[o]), selected = sel, server = FALSE)
    })
    observeEvent(input$sp_map_shape_click, {
      id <- input$sp_map_shape_click$id
      if (!is.null(id) && id %in% CB_ALL$territory_id) updateSelectizeInput(session, "sp_territory", selected = id)
    })
    # Centre the map on the territory the user chose (map click, table link or dropdown);
    # selections made by the app itself (default, parent after zooming out...) do not move the map
    observeEvent(input$sp_territory, {
      id <- input$sp_territory; req(nzchar(id %||% ""))
      if (identical(id, auto_sel())) { auto_sel(NULL); return() }
      x <- country_data(); bx <- BOXES[[x$level]]; i <- match(id, bx$territory_id); req(!is.na(i))
      cap <- switch(x$level, region = 7, province = 8, 10)                 # do not zoom in further than this
      z <- max(input$sp_map_zoom %||% cap, cap)
      flown_at(Sys.time())
      leafletProxy("sp_map") |> flyToBounds(bx$xmin[i], bx$ymin[i], bx$xmax[i], bx$ymax[i], options = list(maxZoom = z, padding = c(40, 40)))
    }, ignoreInit = TRUE)
  }

  layer_country <- function(map, x, shade, sel) {
    d <- x$d; g <- active_geo(x)
    map <- map |> clearGroup("areas") |> clearGroup("sel_area") |> clearGroup("outline") |> clearGroup("bg") |> removeControl("sp_legend") |> removeControl("sp_legend_nodata")
    if (is.null(g) || nrow(g) == 0) return(map)
    NO_DATA <- "#cbd5e1"; SUPPRESSED <- "#e5e7eb"          # grey: not published / below the threshold
    # Following the map at municipal level only the municipalities with data are drawn: a grey
    # layer with the regions underneath marks everything else (smaller municipalities) as "no data"
    if (x$level == "municipality" && all_munis() && !is.null(GEO_REGION))
      map <- map |> addPolygons(data = GEO_REGION, layerId = ~paste0("bg-", territory_id), group = "bg", color = NO_DATA, weight = 0.5,
                                fillColor = NO_DATA, fillOpacity = 0.55, labelOptions = LABEL_OPTIONS,
                                label = lapply(seq_len(nrow(GEO_REGION)), function(i) HTML(paste0("<b>", htmltools::htmlEscape(pretty_name(GEO_REGION$name[i])), "</b><br><span style='color:#64748b'>", NOT_PUBLISHED(lang), "</span>"))))
    k <- match(g$territory_id, d$territory_id); with_data <- !is.na(k)
    title <- paste0(x$types$label, " · ", period_text())
    lbl <- lapply(seq_len(nrow(g)), function(i) HTML(if (!with_data[i])
      paste0("<b>", htmltools::htmlEscape(pretty_name(g$name[i])), "</b><br><span style='color:#64748b'>", if (x$level == "municipality") NOT_PUBLISHED(lang) else t("sp_no_data"), "</span>") else
      paste0("<b>", htmltools::htmlEscape(d$name[k[i]]), "</b><br>",
             t("sp_hover", if (is.na(d$n[k[i]])) t("less_than", MARTS$threshold) else fmt_n(d$n[k[i]], lang), if (is.na(d$rate[k[i]])) "—" else fmt_r(d$rate[k[i]], lang)),
             if (!is.na(d$ratio[k[i]])) paste0(" · ", sprintf(t("times"), fmt_r(d$ratio[k[i]], lang)), " ", x$vs_ref) else "",
             "<br><span style='color:#64748b;font-size:11px'>", htmltools::htmlEscape(title), "</span>")))
    fill <- ifelse(with_data, "#64748b", NO_DATA); opac <- ifelse(with_data, 0.25, 0.55)
    v <- switch(shade, rate = d$rate[k], n = d$n[k], NULL)
    if (!is.null(v) && sum(!is.na(v)) >= 2) {
      sc <- quintile_scale(v[!is.na(v)], shade, lang)
      fill <- ifelse(with_data, ifelse(is.na(v), SUPPRESSED, sc$pal(v)), NO_DATA); opac <- ifelse(with_data, 0.75, 0.55)
      map <- map |> addLegend("bottomright", pal = sc$pal, values = v[with_data], layerId = "sp_legend",
        title = HTML(paste0(tt(if (shade == "n") "sp_legend_number" else "sp_legend_rate"), "<br><small>", htmltools::htmlEscape(title), "</small>")),
        labFormat = sc$format, na.label = t("less_than", MARTS$threshold), opacity = 0.8)
    }
    if (any(!with_data) || (x$level == "municipality" && all_munis()))
      map <- map |> addLegend("bottomright", colors = NO_DATA, labels = if (x$level == "municipality") NOT_PUBLISHED_LEGEND(lang) else t("sp_legend_no_data"), layerId = "sp_legend_nodata", opacity = 0.8)
    map <- map |> addPolygons(data = g, layerId = ~territory_id, group = "areas", color = ifelse(with_data, "#475569", "#94a3b8"),
                              weight = ifelse(with_data, 0.9, 0.5), opacity = 0.9, fillColor = fill, fillOpacity = opac, label = lbl,
                              labelOptions = LABEL_OPTIONS, highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE))
    if (!x$level %in% c("region", "province") && !is.null(GEO_REGION))
      map <- map |> addPolylines(data = if (all_munis()) GEO_REGION else GEO_REGION[GEO_REGION$territory_id == x$ref_id, ],
                                 group = "outline", color = "#1f2933", weight = 2, options = pathOptions(interactive = FALSE))
    if (!is.null(sel) && nzchar(sel) && sel %in% g$territory_id)
      map <- map |> addPolygons(data = g[g$territory_id == sel, ], group = "sel_area", color = "#c2410c", weight = 4, fill = FALSE, dashArray = "6 4", options = pathOptions(interactive = FALSE))
    map
  }

  output$sp_foot <- renderUI(paste(SOURCE_TEXT(lang, SM()), if (HAS_MAP) BOUNDARIES_TEXT_OF(lang, SM()) else ""))
  output$sp_notes <- renderUI({
    x <- country_data()
    note <- if (x$alt) meta_text(x$src, "notes", lang, CLANG, default = t("sp_note")) else t("sp_note")
    div(class = "note mt-2", p(HTML(note)), p(t("sp_sensitive")), if (is.null(x$pop_year)) p(NO_POP_TEXT(lang)) else p(POP_NOTE(lang, x$pop_year, x$src)))
  })
}


COUNTRY_FUNCTIONS <- c("cb_types", "cb_periods", "ui_country_controls", "ui_country", "server_country")

# Prepare the environment of a country (NULL if it has no data)
make_country <- function(cc) {
  app_env <- environment(make_country)
  crime <- if (has_rows(CCRIME)) CCRIME[CCRIME$country == cc, ] else NULL
  pop <- if (has_rows(CPOP)) CPOP[CPOP$country == cc, ] else NULL
  if (!has_rows(crime)) return(NULL)
  crime <- fill_single_provinces(crime)
  if (!has_rows(CMETA) || !cc %in% CMETA$country) return(NULL)
  if (!"source" %in% names(CMETA)) CMETA$source <- "main"
  src_meta <- CMETA[CMETA$country == cc, ]; meta <- src_meta[src_meta$source == "main", ][1, ]
  if (nrow(src_meta[src_meta$source == "main", ]) == 0) return(NULL)
  e <- new.env(parent = app_env)
  e$CC <- cc; e$CB_ALL <- crime; e$CB <- crime[crime$source == "main", ]; e$POPM <- pop; e$HAS_CB <- nrow(e$CB) > 0; e$HAS_POPM <- has_rows(pop)
  if (!e$HAS_CB) return(NULL)
  e$META <- meta; e$SRC_META <- src_meta
  # Second sources with data (a regional police...): each one covers a territory (a region id or the country)
  e$ALT_SOURCES <- src_meta[src_meta$source != "main" & src_meta$source %in% crime$source, ]
  e$CLANG <- meta$lang %||% "en"
  # Country name: local name in the local language, a translated name (country_<cc> key) elsewhere, English as the fallback
  e$CNAME <- function(lang) {
    if (lang == e$CLANG && nzchar(meta$name_local %||% "")) return(meta$name_local)
    k <- paste0("country_", cc); tx <- TR(k, lang); if (!identical(tx, k)) tx else meta$name_en %||% cc }
  f <- paste0("data/territory_", tolower(cc), ".gpkg")
  e$GEO_MUNI <- read_layer(f, "municipality"); e$GEO_PROVINCE <- read_layer(f, "province"); e$GEO_REGION <- read_layer(f, "region")
  # Layers of the second sources' levels (police_area...), by level name
  alt_levels <- unique(unlist(strsplit(e$ALT_SOURCES$levels %||% character(), ";")))
  e$GEO_EXTRA <- setNames(lapply(alt_levels, function(l) read_layer(f, l)), alt_levels)
  e$GEO_LEVEL <- function(l) switch(l, region = e$GEO_REGION, province = e$GEO_PROVINCE, municipality = e$GEO_MUNI, e$GEO_EXTRA[[l]])
  e$HAS_MAP <- !is.null(e$GEO_REGION) || !is.null(e$GEO_MUNI)
  e$BOXES <- c(list(region = polygon_boxes(e$GEO_REGION), province = polygon_boxes(e$GEO_PROVINCE), municipality = polygon_boxes(e$GEO_MUNI)), lapply(e$GEO_EXTRA, polygon_boxes))
  e$GEO_MUNI_DATA <- if (!is.null(e$GEO_MUNI)) e$GEO_MUNI[e$GEO_MUNI$territory_id %in% e$CB$territory_id, ] else NULL
  # Levels with data (and, when there is a map, with boundaries)
  lv <- intersect(c("region", "province", "municipality"), unique(e$CB$level))
  if (e$HAS_MAP) lv <- lv[vapply(lv, function(l) !is.null(e[[switch(l, region = "GEO_REGION", province = "GEO_PROVINCE", "GEO_MUNI")]]), logical(1))]
  if (length(lv) == 0) lv <- intersect(c("region", "province", "municipality"), unique(e$CB$level))
  if (length(lv) == 0) return(NULL)
  e$SP_LEVELS <- lv
  r <- TERRITORY[TERRITORY$level == "region" & TERRITORY$territory_id %in% e$CB$territory_id, ]
  e$SP_REGIONS <- setNames(r$territory_id, pretty_name(r$name))[order(r$name)]
  e$DEFAULT_REGION <- if (nzchar(meta$default_region %||% "") && meta$default_region %in% e$SP_REGIONS) meta$default_region else e$SP_REGIONS[1]
  # Offence types of a source (no sensitive ones) and its aggregates (total first)
  e$TYPE_TABLE <- function(src = "main") { d <- e$CB_ALL[e$CB_ALL$source == src, ]; unique(d[!d$sensitive, c("offence_type_id", "included_in", "name_local", "name_en")]) }
  e$AGGREGATES_OF <- function(tt) { a <- unique(c("total", tt$included_in[!is.na(tt$included_in)])); a[a %in% tt$offence_type_id] }
  e$CB_TYPE_TABLE <- e$TYPE_TABLE("main"); e$AGGREGATES <- e$AGGREGATES_OF(e$CB_TYPE_TABLE)
  # Level labels: the English / local labels of every source of the country; other languages through the lvl_* keys
  split_labels <- function(col) { out <- c(); for (i in seq_len(nrow(src_meta))) { m <- src_meta[i, ]; if (!is.null(m[[col]]) && nzchar(m$levels %||% "")) out <- c(out, setNames(strsplit(paste0(m[[col]], ";"), ";")[[1]], strsplit(m$levels, ";")[[1]])) }; out[!duplicated(names(out))] }
  labels_en <- split_labels("level_labels_en"); labels_local <- split_labels("level_labels_local")
  single_en <- split_labels("level_singular_en"); single_local <- split_labels("level_singular_local")
  e$LEVEL_LABEL <- function(level, lang, col = FALSE) {
    pick <- function(v) { x <- v[level]; if (is.null(x) || is.na(x) || !nzchar(x)) NA else unname(x) }
    x <- if (col) { if (lang == e$CLANG) pick(single_local) %||% pick(single_en) else pick(single_en) } else NA
    if (is.na(x)) x <- if (lang == e$CLANG) pick(labels_local) else pick(labels_en)
    if (is.na(x)) x <- pick(labels_en)
    # Other languages: the English label translated when it is a common word (lvl_* keys), else English
    if (!is.na(x) && lang != "en" && lang != e$CLANG) { k <- paste0("lvl_", gsub("[^a-z]", "", tolower(sub("\\s*\\(.*$", "", x)))); tx <- TR(k, lang); if (!identical(tx, k)) x <- tx }
    if (is.na(x) || !nzchar(x)) TR(paste0(if (col) "sp_col_" else "sp_level_", level), lang) else unname(x)
  }
  # Width of the view (degrees of longitude) under which the level changes: the module's values when
  # it gives them (zoom_widths), otherwise proportional to the country's width
  g <- e$GEO_REGION %||% e$GEO_MUNI; w <- if (is.null(g)) 10 else { b <- sf::st_bbox(g); min(as.numeric(b["xmax"] - b["xmin"]), 60) }
  e$ZOOM_WIDTHS <- c(region = if (!is.na(meta$zoom_region %||% NA)) meta$zoom_region else 0.65 * w, province = if (!is.na(meta$zoom_province %||% NA)) meta$zoom_province else 0.4 * w)
  e$BOUNDARIES_ATTR <- if (nzchar(meta$boundaries_attribution %||% "")) meta$boundaries_attribution else "see About the data"
  e$BOUNDARIES_TEXT <- function(lang) TR("boundaries_by", lang, e$BOUNDARIES_ATTR)
  loc <- function(what, lang, m = meta) { x <- if (lang == e$CLANG) m[[paste0(what, "_local")]] else NULL; if (is.null(x) || is.na(x) || !nzchar(x)) m[[what]] %||% "" else x }
  e$LOC <- loc
  # Source line of the sidebar and the population note, for the main source or a second one (m = its metadata row)
  e$SOURCE_TEXT <- function(lang, m = meta) { k <- m$source_text_key %||% ""; if (nzchar(k) && !identical(TR(k, "en"), k)) return(TR(k, lang))
    TR("sp_source_generic", lang, loc("source_name", lang, m), loc("publisher", lang, m), if (nzchar(m$population_source %||% "")) loc("population_source", lang, m) else "—") }
  e$POP_NOTE <- function(lang, year, m = meta) TR("sp_note_pop_generic", lang, loc("population_source", lang, m), year)
  e$BOUNDARIES_TEXT_OF <- function(lang, m) TR("boundaries_by", lang, if (nzchar(m$boundaries_attribution %||% "")) m$boundaries_attribution else e$BOUNDARIES_ATTR)
  e$NO_POP_TEXT <- function(lang) TR("sp_no_pop_generic", lang)
  # "Not published" texts: a translation key when the module names one (every language), else its English / local text
  has_key <- function(k) nzchar(k %||% "") && !identical(TR(k, "en"), k)
  np <- function(lang) { if (has_key(meta$not_published_key)) return(TR(meta$not_published_key, lang))
    x <- if (lang == e$CLANG) meta$not_published_local else meta$not_published_en %||% ""; if (is.null(x) || is.na(x) || !nzchar(x)) TR("sp_not_published", lang) else x }
  e$NOT_PUBLISHED <- np
  e$NOT_PUBLISHED_LEGEND <- function(lang) if (has_key(meta$not_published_legend_key)) TR(meta$not_published_legend_key, lang) else TR("sp_legend_not_published", lang)
  # Countries that count something other than recorded offences (complaints, reported cases): the title,
  # the KPI noun and the legends use the module's words in English and in the local language
  mlab <- function(what, lang, m = meta) { x <- m[[paste0(what, if (lang == e$CLANG) "_local" else "_en")]]
    if (lang != "en" && lang != e$CLANG) {   # other languages: the translated measure kind (msr_<kind>_title / _noun), else English
      kind <- m$measure_kind %||% ""; k <- paste0("msr_", kind, if (what == "count_noun") "_noun" else "_title")
      tx <- if (nzchar(kind)) TR(k, lang) else k; x <- if (!identical(tx, k)) tx else m[[paste0(what, "_en")]] }
    if (is.null(x) || is.na(x) || !nzchar(x)) NULL else x }
  cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  # Texts that depend on what the source counts: the title, the KPI noun, the legends and the count column.
  # m = the metadata row of the source shown (the main one by default; the server passes the active one)
  e$TRS <- function(k, lang, m = meta, ...) {
    if (k == "sp_title") { tk <- m$title_key %||% ""; if (nzchar(tk) && !identical(TR(tk, "en"), tk)) return(TR(tk, lang)) }
    if (!is.null(mlab("count_noun", lang, m))) {
      noun <- mlab("count_noun", lang, m)
      if (k == "sp_k_n") return(noun)
      if (k == "sp_legend_rate") return(paste(cap1(noun), TR("sp_k_rate", lang)))
      if (k == "sp_legend_number" || k == "sp_col_n") return(cap1(noun))
      if (k == "sp_title" && !is.null(mlab("measure_title", lang, m))) return(mlab("measure_title", lang, m))
    }
    TR(k, lang, ...)
  }
  e$TR <- function(k, lang, ...) e$TRS(k, lang, meta, ...)
  for (fn in COUNTRY_FUNCTIONS) { g <- get(fn, envir = app_env); environment(g) <- e; assign(fn, g, envir = e) }
  e
}

# =============================================================================
# Countries table: national rates per 100,000 inhabitants as published by
# Eurostat (crim_off_cat); UNODC is excluded by its terms of use. Shown apart from the maps, with the warning
# that legal definitions and recording practices differ between countries.
# =============================================================================
ui_countries <- function(lang) {
  t <- function(k, ...) TR(k, lang, ...)
  cats <- unique(CRATES[, c("offence_id", "offence_en", "offence_local")])
  choices <- setNames(cats$offence_id, if (lang == "es" && "offence_local" %in% names(cats)) ifelse(is.na(cats$offence_local) | !nzchar(cats$offence_local), cats$offence_en, cats$offence_local) else cats$offence_en)
  default <- if ("intentional_homicide" %in% cats$offence_id) "intentional_homicide" else cats$offence_id[1]
  bslib::page_navbar(
    title = paste(t("app_title"), "·", t("nav_countries")), lang = lang, theme = bslib::bs_theme(version = 5), id = "nav",
    header = tags$head(tags$style(HTML(CSS))),
    bslib::nav_panel(t("nav_countries"), value = "countries", div(class = "about",
      h1(class = "h3", t("countries_title")),
      div(class = "alert alert-warning", HTML(t("countries_intro"))),
      bslib::layout_columns(col_widths = c(5, 7), fill = FALSE,
        selectInput("cr_offence", t("col_offence"), choices = choices, selected = default, width = "100%"),
        radioButtons("cr_source", t("countries_source"), inline = TRUE, choices = setNames(unique(CRATES$source), unique(CRATES$source)))),
      uiOutput("cr_table"),
      p(class = "upd", HTML(t("countries_note_src"))))),
    !!!nav_switcher("countries", lang, CITIES)
  )
}
server_countries <- function(input, output, session, lang) {
  t <- function(k, ...) TR(k, lang, ...)
  output$cr_table <- renderUI({
    req(input$cr_offence, input$cr_source)
    d <- CRATES[CRATES$offence_id == input$cr_offence & CRATES$source == input$cr_source, ]
    if (nrow(d) == 0) return(p(class = "note", t("sp_no_data")))
    # Latest year with a figure for each country, plus the previous one for context
    d <- d[order(d$country, -d$year), ]
    latest <- d[!duplicated(d$country), ]
    prev <- d[!d$year %in% latest$year[match(d$country, latest$country)], ]; prev <- prev[!duplicated(prev$country), ]
    latest$prev <- prev$rate[match(latest$country, prev$country)]; latest$prev_year <- prev$year[match(latest$country, prev$country)]
    # Country names in the reader's language when a country_<code> translation exists (Eurostat's English name otherwise)
    latest$country_name <- vapply(seq_len(nrow(latest)), function(i) { k <- paste0("country_", latest$country[i]); tx <- t(k); if (identical(tx, k)) latest$country_name[i] else tx }, "")
    latest <- latest[order(latest$country_name), ]
    cell <- function(v) tags$td(class = "text-end", v)
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th(t("col_country")), tags$th(class = "text-end", t("col_year")), tags$th(class = "text-end", t("col_rate100k")),
                         tags$th(class = "text-end", t("col_previous")), tags$th(t("col_note")))),
      tags$tbody(lapply(seq_len(nrow(latest)), function(i) { r <- latest[i, ]
        tags$tr(tags$td(r$country_name), cell(r$year), cell(fmt_r(r$rate, lang)),
                cell(if (is.na(r$prev)) "—" else paste0(fmt_r(r$prev, lang), " (", r$prev_year, ")")), tags$td(class = "text-muted small", r$note %||% "")) })))
  })
}

# =============================================================================
# About the data: what each source counts, how the figures are shown and why
# there is not more data. Static text (translations.csv) plus the periods and
# dates read from the tables, so that it never goes stale.
# =============================================================================
ui_about <- function(lang, cities) {
  t <- function(k, ...) TR(k, lang, ...)
  host <- function(u) sub("^https?://([^/]+).*$", "\\1", u)
  link <- function(u) if (is.null(u) || is.na(u) || !nzchar(u)) NULL else tags$a(href = u, target = "_blank", host(u))
  months <- function(m) if (is.null(m) || all(is.na(m))) "" else { d <- as.Date(m); paste(fmt_month(min(d), lang), "–", fmt_month(max(d), lang)) }
  # One row per city (source, what it counts, territory, months, licence: the module's About keys when it names them), then its extra panels
  key_or <- function(k, default) if (nzchar(k %||% "") && !identical(TR(k, "en"), k)) t(k) else default
  # Panels with an About row: those that name About keys, unless the same data is already a country source (about_unless_country_source)
  panel_rows <- function(city, cname) { pm <- cities[[city]]$PANELS; if (!has_rows(pm)) return(NULL)
    lapply(seq_len(nrow(pm)), function(i) { p <- pm[i, ]
      if (!nzchar(p$about_source_key)) return(NULL)
      if (nzchar(p$about_unless_country_source)) { cs <- strsplit(p$about_unless_country_source, ":")[[1]]; if (has_rows(CMETA) && any(CMETA$country == cs[1] & CMETA$source == cs[2])) return(NULL) }
      list(key_or(p$about_source_key, paste0(p$name_en, " (", cname, ")")), key_or(p$about_measures_key, p$measure), key_or(p$about_territory_key, cname), months(c(p$first_month, p$last_month)), key_or(p$about_licence_key, p$licence), p$url) }) }
  rows <- unlist(lapply(names(cities), function(city) { m <- RAW[[city]]$meta; cname <- cities[[city]]$CITY_NAME
    c(list(list(key_or(m$about_source_key, paste0(m$source_name, " (", cname, ")")), key_or(m$about_measures_key, m$measure), key_or(m$about_territory_key, cname), months(c(m$first_month, m$last_month)), m$licence, m$url)),
      Filter(Negate(is.null), panel_rows(city, cname))) }), recursive = FALSE)
  mc <- if (length(cities)) RAW[[names(cities)[1]]]$meta else NULL; mb <- if ("barcelona" %in% names(cities)) RAW$barcelona$meta else NULL
  # One row per country (the first one is DEFAULT_COUNTRY): source, measure and notes, levels, period, licence, population and boundaries
  # One row per country and source (main source first, then its second sources)
  rows <- c(unlist(lapply(names(COUNTRIES), function(cc) { e <- COUNTRIES[[cc]]; ms <- e$SRC_META[e$SRC_META$source == "main" | e$SRC_META$source %in% e$ALT_SOURCES$source, ]
    lapply(seq_len(nrow(ms)), function(i) { m <- ms[i, ]
      per <- if (!is.null(m$first_period) && !is.na(m$first_period)) sprintf("%d – %d (%s)", m$first_period %/% 10, m$last_period %/% 10, month_names(lang, TRUE)[(m$last_period %% 10) * 3]) else ""
      lv <- if (m$source == "main") e$SP_LEVELS else strsplit(m$levels, ";")[[1]]
      lvls <- paste(vapply(lv, function(l) e$LEVEL_LABEL(l, lang), ""), collapse = ", ")
      cname <- e$CNAME(lang)
      list(paste0(m$source_name, " (", cname, ")"), paste0(m$measure, if (nzchar(m$notes_en %||% "")) paste0(" ", m$notes_en) else ""), paste0(cname, ": ", lvls), per,
           paste0(m$licence, if (nzchar(m$population_source %||% "")) paste0(" · ", t("about_pop_from", m$population_source)) else "",
                  if (nzchar(m$boundaries_attribution %||% "")) paste0(" · ", t("boundaries_by", m$boundaries_attribution)) else ""), m$url) }) }), recursive = FALSE), rows)
  rows <- Filter(Negate(is.null), rows)
  table <- tags$table(class = "table table-sm",
    tags$thead(tags$tr(lapply(c("about_col_source", "about_col_measures", "about_col_territory", "about_col_period", "about_col_licence"), function(k) tags$th(t(k))))),
    tags$tbody(lapply(rows, function(r) tags$tr(
      tags$td(tags$b(r[[1]]), if (!is.null(link(r[[6]]))) tagList(tags$br(), link(r[[6]]))),
      tags$td(r[[2]]), tags$td(r[[3]]), tags$td(r[[4]]), tags$td(r[[5]])))))
  first_calls <- if (!is.null(mc)) fmt_month(as.Date(mc$first_month), lang) else "—"
  # How each source counts: unit, where, date, revisions, known breaks (from the modules' metadata; "—" when the module says nothing)
  dash <- function(x) if (is.null(x) || is.na(x) || !nzchar(x)) "—" else x
  mrows <- c(
    unlist(lapply(names(COUNTRIES), function(cc) { e <- COUNTRIES[[cc]]; ms <- e$SRC_META[e$SRC_META$source == "main" | e$SRC_META$source %in% e$ALT_SOURCES$source, ]
      lapply(seq_len(nrow(ms)), function(i) { m <- ms[i, ]
        list(paste0(m$source_name, " (", e$CNAME(lang), ")"), dash(if (nzchar(m$count_noun_en %||% "")) m$count_noun_en else t("gl_recorded_unit")), dash(m$method_where), dash(m$method_date), dash(m$method_revisions), dash(m$notes_en)) }) }), recursive = FALSE),
    lapply(names(cities), function(city) { m <- RAW[[city]]$meta
      list(paste0(m$source_name, " (", cities[[city]]$CITY_NAME, ")"), t("k_calls", city = city, kind = m$measure_kind %||% "calls"), dash(m$method_where), dash(m$method_date), dash(m$method_revisions), dash(if (nzchar(m$note_break_key %||% "")) t(m$note_break_key) else "")) }))
  method_table <- tags$table(class = "table table-sm small",
    tags$thead(tags$tr(lapply(c("about_col_source", "about_col_unit", "about_col_where", "about_col_date", "about_col_revisions", "about_col_breaks"), function(k) tags$th(t(k))))),
    tags$tbody(lapply(mrows, function(r) tags$tr(lapply(r, function(v) tags$td(v))))))
  bslib::page_navbar(
    title = paste(t("app_title"), "·", t("nav_about")), lang = lang, theme = bslib::bs_theme(version = 5), id = "nav",
    header = tags$head(tags$style(HTML(CSS))),
    bslib::nav_panel(t("nav_about"), value = "about", div(class = "about",
      h1(class = "h3", t("nav_about")),
      p(t("about_intro_1")), p(t("about_intro_2")),
      h2(t("about_h_sources")), table,
      if (has_rows(CITY_SURVEY_META)) tagList(h2(t("about_h_surveys")), p(t("about_surveys_intro")), tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(t("about_col_source")), tags$th(t("about_col_measures")), tags$th(t("about_col_period")), tags$th(t("about_col_licence")))),
        tags$tbody(lapply(seq_len(nrow(CITY_SURVEY_META)), function(i) { s <- CITY_SURVEY_META[i, ]; cname <- if (!is.null(cities[[s$city]])) cities[[s$city]]$CITY_NAME else s$city
          tags$tr(tags$td(tags$b(paste0(s$name_en, " (", cname, ")")), if (!is.null(link(s$url))) tagList(tags$br(), link(s$url))), tags$td(s$measure), tags$td(paste(s$first_year, "–", s$last_year)), tags$td(paste(s$licence, "·", s$publisher))) })))),
      h2(t("about_h_method")), p(t("about_method_intro")), method_table,
      h2(t("about_h_glossary")), tags$dl(class = "row", lapply(c("recorded", "dark_figure", "rate", "lq", "percentile", "cumulative", "revision", "threshold"), function(g) tagList(tags$dt(class = "col-sm-3", t(paste0("gl_", g, "_t"))), tags$dd(class = "col-sm-9", t(paste0("gl_", g, "_d")))))),
      h2(t("about_h_rules")), tags$ul(lapply(c(t("about_rule_1"), t("about_rule_2"), t("about_rule_3", MARTS$threshold, MARTS$threshold), t("about_rule_4"), t("about_rule_5"), t("about_rule_6")), function(x) tags$li(HTML(x)))),
      h2(t("about_h_why")), tags$ul(lapply(c(t("about_why_1"), t("about_why_2"), if (!is.null(mb)) t("about_why_bcn"), t("about_why_3"), t("about_why_4"), t("about_why_5"), t("about_why_6"), t("about_why_7", first_calls, if (!is.null(COUNTRIES[[DEFAULT_COUNTRY]])) min(COUNTRIES[[DEFAULT_COUNTRY]]$CB$year) else NA), t("about_why_8"), if (length(COUNTRIES) > 1 || has_rows(CRATES)) t("about_why_countries")), function(x) tags$li(HTML(x)))),
      h2(t("about_h_limits")), tags$ul(lapply(c(t("about_limit_1"), t("about_limit_2"), t("about_limit_3"), t("about_limit_4")), function(x) tags$li(x))),
      p(class = "upd", t("about_updated", MARTS$threshold)))),
    !!!nav_switcher("about", lang, cities)
  )
}

# =============================================================================
# Countries: DEFAULT_COUNTRY first, then the others by English name (only those with data loaded)
COUNTRIES <- list()
country_codes <- if (has_rows(CMETA)) c(intersect(DEFAULT_COUNTRY, CMETA$country), setdiff(CMETA$country[order(CMETA$name_en)], DEFAULT_COUNTRY)) else character()
for (cc in country_codes) { e <- make_country(cc); if (!is.null(e)) COUNTRIES[[cc]] <- e }
CITIES <- Filter(Negate(is.null), setNames(lapply(names(CITY_SOURCES), make_city), names(CITY_SOURCES)))
VIEWS_AVAILABLE <- c(names(COUNTRIES), names(CITIES), if (has_rows(CRATES)) "countries", "about")
if (length(VIEWS_AVAILABLE) == 1) stop("The app tables are missing. Run option 1 in WEBARCHYMAPP.R.")

ui <- function(req) {
  lang <- lang_from_url(req$QUERY_STRING); view <- view_from_url(req$QUERY_STRING, VIEWS_AVAILABLE)
  if (view %in% names(COUNTRIES)) COUNTRIES[[view]]$ui_country(lang)
  else if (view == "about") ui_about(lang, CITIES)
  else if (view == "countries") ui_countries(lang)
  else CITIES[[view]]$ui_city(lang)
}
server <- function(input, output, session) {
  qs <- isolate(session$clientData$url_search)
  lang <- lang_from_url(qs); view <- view_from_url(qs, VIEWS_AVAILABLE)
  if (view %in% names(COUNTRIES)) COUNTRIES[[view]]$server_country(input, output, session, lang)
  else if (view == "countries") server_countries(input, output, session, lang)
  else if (view != "about") CITIES[[view]]$server_city(input, output, session, lang)
}
shinyApp(ui, server)
