# WebarchyMapp - personal app ---------------------------------------------------------
# Opened from WEBARCHYMAPP.R (options 2 and 3) or scripts/2_open_app.R.
# Reads ONLY the mart_* tables (no day, hour or sensitive types) and the official
# geometries. The "fewer than N" threshold is applied AFTER adding up the chosen
# range of months, with the same function as the data process (R/50_marts.R).
# Everything is loaded into memory at start-up and the database is closed.
#
# Views (?view=<country code> / madrid / barcelona / countries / about, ?lang=xx):
#   Country   - one view per country with data (Spain included), from the shared mart_country_*
#               tables: recorded offences by region, province and municipality; in Catalonia
#               also Mossos by police area (until phase 2 of Q-005 moves it into the contract)
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

CITY_SOURCES <- list(
  madrid    = list(id = "ES-13-28-28079", base = "mart_madrid_calls_month", meta = "mart_madrid_calls_meta",
                   types = "mart_madrid_call_types", gpkg = "data/territory_madrid.gpkg"),
  barcelona = list(id = "ES-09-08-08019", base = "mart_bcn_incidents_month", meta = "mart_bcn_incidents_meta",
                   types = "mart_bcn_incident_types", gpkg = "data/territory_barcelona.gpkg")
)
RAW <- lapply(CITY_SOURCES, function(x) list(
  base = q(paste("SELECT * FROM", x$base)), meta = q(paste("SELECT * FROM", x$meta)), types = q(paste("SELECT * FROM", x$types))))

MOSSOS  <- q("SELECT * FROM mart_mossos_bcn_month")          # Mossos, Barcelona by district
MGROUPS <- q("SELECT * FROM mart_mossos_groups")
MMETA   <- q("SELECT * FROM mart_mossos_meta")
ABP     <- q("SELECT * FROM mart_mossos_abp_year")           # Mossos by police area, all Catalonia
ABPPOP  <- q("SELECT * FROM mart_population_abp")
ABPINFO <- q("SELECT * FROM mart_abp_info")
ACT     <- q("SELECT * FROM mart_madrid_activity"); ACTMETA <- q("SELECT * FROM mart_madrid_activity_meta")
ARR     <- q("SELECT * FROM mart_madrid_arrests");  ARRNAMES <- q("SELECT * FROM mart_madrid_arrest_motives")
CCRIME  <- q("SELECT * FROM mart_country_crime")                         # every country, Spain included (R/70_countries.R)
CPOP    <- q("SELECT * FROM mart_country_population")
CMETA   <- q("SELECT * FROM mart_country_meta")
CRATES  <- q("SELECT * FROM mart_country_rates")                         # national rates (Eurostat; UNODC excluded by its terms) for the Countries table
db_disconnect(con)

# A layer that the GeoPackage does not hold (a country without municipalities, say) is simply NULL
read_layer <- function(file, layer) {
  f <- path(file); if (!file.exists(f)) return(NULL)
  has <- tryCatch(layer %in% sf::st_layers(f)$name, error = function(e) FALSE); if (!has) return(NULL)
  tryCatch(sf::st_read(f, layer = layer, quiet = TRUE), error = function(e) NULL)
}
GEO_ABP      <- read_layer("data/territory_abp.gpkg", "abp")   # Mossos police areas (Catalonia)
# Bounding box of every polygon (lon/lat), to know which territories are inside the map view
polygon_boxes <- function(g) {
  if (is.null(g)) return(NULL)
  b <- t(vapply(sf::st_geometry(g), function(x) as.numeric(sf::st_bbox(x)), numeric(4)))
  data.frame(territory_id = g$territory_id, xmin = b[, 1], ymin = b[, 2], xmax = b[, 3], ymax = b[, 4])
}
name_of   <- setNames(TERRITORY$name, TERRITORY$territory_id)
parent_of <- setNames(TERRITORY$parent_id, TERRITORY$territory_id)
level_of  <- setNames(TERRITORY$level, TERRITORY$territory_id)
HAS_MOSSOS <- has_rows(MOSSOS) && !is.null(MGROUPS)
HAS_ABP    <- has_rows(ABP) && !is.null(ABPINFO)
HAS_ACT    <- has_rows(ACT)
HAS_ARR    <- has_rows(ARR) && !is.null(ARRNAMES)
# A single-province region (Madrid, Asturias...) may have no province row in the source: the
# province takes the figures of its region (it is the same territory)
fill_single_provinces <- function(crime) {
  prov <- TERRITORY[TERRITORY$level == "province" & TERRITORY$territory_id %in% TERRITORY$territory_id[startsWith(TERRITORY$territory_id, paste0(crime$country[1], "-"))], ]
  n_prov <- table(prov$parent_id)
  single <- prov[prov$parent_id %in% names(n_prov)[n_prov == 1], ]
  missing_prov <- single[!single$territory_id %in% crime$territory_id & single$parent_id %in% crime$territory_id, ]
  extra <- do.call(rbind, lapply(seq_len(nrow(missing_prov)), function(i) {
    r <- crime[crime$territory_id == missing_prov$parent_id[i], ]
    r$territory_id <- missing_prov$territory_id[i]; r$level <- "province"; r$name <- missing_prov$name[i]; r$parent_id <- missing_prov$parent_id[i]
    r
  }))
  if (!is.null(extra)) rbind(crime, extra) else crime
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

# Rate compared with a reference: a label (much lower ... much higher than the reference
# average), the position among the territories of the same level, and the % difference.
# It is a comparison of recorded figures, never a safety or danger score.
CMP_LEVELS <- c("cmp_much_lower", "cmp_lower", "cmp_similar", "cmp_higher", "cmp_much_higher")
CMP_COLOURS <- c("#dbe7f6", "#bdd7e7", "#8fbbdc", "#4f94cb", "#1d5fa6")
comparison_block <- function(rate, ref_rate, rates_all, ref_label, unit_label, group_label, lang) {
  t <- function(k, ...) TR(k, lang, ...)
  if (is.na(rate) || is.na(ref_rate) || ref_rate <= 0) return(NULL)
  ratio <- rate / ref_rate
  lvl <- findInterval(ratio, c(0.6, 0.85, 1.15, 1.5)) + 1
  all <- sort(rates_all[!is.na(rates_all)]); n <- length(all)
  rank <- sum(all < rate) + 1
  pct <- round(100 * abs(ratio - 1))
  diff_text <- if (pct == 0) t("cmp_same", ref_label) else if (ratio < 1) t("cmp_below", pct, ref_label) else t("cmp_above", pct, ref_label)
  div(class = "cmp",
    div(class = "cmp-head", span(class = "cmp-title", t("cmp_title", ref_label), info(t("cmp_note"))),
        span(class = "cmp-pill", style = paste0("background:", CMP_COLOURS[lvl], ";color:", if (lvl >= 4) "#fff" else "#1f2933"), t(CMP_LEVELS[lvl]))),
    div(class = "cmp-bar", div(class = "cmp-fill", style = paste0("width:", if (n > 1) round(100 * (rank - 1) / (n - 1)) else 50, "%;background:", CMP_COLOURS[lvl]))),
    div(class = "cmp-rate", paste0(fmt_r(rate, lang), " ‰")),
    div(class = "cmp-sub", unit_label),
    div(class = "cmp-sub", diff_text, if (n > 1) paste0(" · ", t("cmp_rank", rank, n, group_label))))
}

# --- Styles -----------------------------------------------------------------------
CSS <- "
  .bslib-page-navbar .bslib-sidebar-layout > .main { padding: 0; }
  .tab-pane[data-value='stats'] { padding: 1rem 1.25rem !important; }
  #map { height: 100% !important; min-height: 420px; }
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
  n <- unname(if (lang == "es") NAME_ES[x] else NAME_EN[x])
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
    div(class = "facet", span(t("category"), info(paste(t("desc_public_safety"), t("desc_coexistence"))))),
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
                     choices = shade_options(lang, HAS_RATES), selected = shade_default(HAS_RATES))),
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
      radioButtons("sort", t("sort_by"), inline = TRUE,
                   choices = if (HAS_RATES) setNames(c("rate", "n"), c(t("sort_rate"), t("sort_number"))) else setNames("n", t("sort_number"))),
      uiOutput("stats_title"),
      bslib::layout_columns(col_widths = c(7, 5), fill = FALSE, plotOutput("stats_plot", height = "300px"), tableOutput("stats_table")),
      uiOutput("stats_notes"),
      if (CITY == "madrid" && HAS_ARR) tagList(
        tags$hr(), uiOutput("arr_title"),
        bslib::layout_columns(col_widths = c(5, 7), fill = FALSE, div(class = "table-sp", uiOutput("arr_table")),
                              div(uiOutput("arr_evol_head"), plotOutput("arr_evol", height = "260px"))),
        uiOutput("arr_notes"))
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
      addTiles(attribution = paste(ATTRIBUTION_OSM, "·", t("boundaries_by", t("council")))) |>
      layer_neighbourhoods(isolate(data()), isolate(input$shade) %||% shade_default(HAS_RATES), isolate(selection_text()), isolate(range()), lang) |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })
  observeEvent(list(data(), input$shade), {
    leafletProxy("map") |>
      layer_neighbourhoods(data(), input$shade %||% shade_default(HAS_RATES), selection_text(), range(), lang) |>
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
      div(class = "sub", t("district_of", name_of[[dis_id]]), " · ", selection_text(), " · ", fmt_range(r, lang)),
      div(class = "kpis",
          kpi(figure(row$n, lang), t("k_calls")),
          if (HAS_RATES) kpi(if (is.na(row$rate)) "—" else fmt_r(row$rate, lang), t("k_rate")),
          if (HAS_RATES) kpi(if (is.na(ratio)) "—" else sprintf(t("times"), fmt_r(ratio, lang)), t("k_ratio"))),
      if (HAS_RATES) comparison_block(row$rate, d$city$rate, d$neighbourhoods$rate, t("city"), paste(t("k_calls"), t("k_rate")), t("cmp_neighbourhoods"), lang),
      if (!is.na(ratio) && ratio >= 2) div(class = "alert alert-info", HTML(t("ratio_note", fmt_r(ratio, lang)))),
      if (HAS_RATES && isTRUE(pop_of[id] < 5000)) div(class = "alert alert-secondary", t("few_residents")),
      if (CITY == "barcelona" && input$block == "public_safety" && r[1] < as.Date("2024-01-01") && r[2] >= as.Date("2024-01-01"))
        div(class = "alert alert-warning", t("break_2024")),
      if (CITY == "madrid" && grepl("Guindalera", name_of[[id]], ignore.case = TRUE)) div(class = "alert alert-warning", t("guindalera")),
      rates_block(id, dis_id, d, lang),
      h6(t("by_month"), info(t("noise_note"))),
      plotOutput("evolution", height = "120px"),
      h6(t("types_in_period")),
      if (nrow(types) == 0) p(class = "note", t("no_calls")) else
        tags$table(class = "table table-sm", tags$tbody(lapply(seq_len(nrow(types)), function(i)
          tags$tr(tags$td(type_name(types$call_type[i], lang),
                          if (!is.na(type_note(types$call_type[i], lang))) info(type_note(types$call_type[i], lang))),
                  tags$td(class = "text-end", figure(types$n[i], lang)))))),
      if (CITY == "barcelona" && HAS_MOSSOS) mossos_block(dis_id, r, lang),
      if (CITY == "madrid" && HAS_ACT) activity_block(dis_id, r, lang),
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
    b <- barplot(height, names.arg = month_names(lang)[as.integer(format(months, "%m"))], col = ifelse(low, NA, "#64748b"),
                 border = ifelse(low, "#94a3b8", NA), density = ifelse(low, 15, -1), angle = 45, ylim = c(0, max(height, THRESHOLD) * 1.18))
    if (any(low)) text(b[low], height[low], paste0("<", THRESHOLD), pos = 3, cex = 0.65, col = "#475569")
  }, res = 96)

  # --- Statistics: top 5 districts (same filters) ---
  top5 <- reactive({
    d <- data(); by <- if (identical(input$sort, "rate") && HAS_RATES) "rate" else "n"
    di <- d$districts[!is.na(d$districts[[by]]), ]
    di <- di[order(-di[[by]]), ][seq_len(min(5, nrow(di))), ]
    list(districts = di, city = d$city, by = by)
  })
  output$stats_title <- renderUI({
    if (length(selected_types()) == 0) return(p(class = "note", t("no_type_selected")))
    h5(class = "mt-2 mb-1", tags$b(t("top5")), " · ", selection_text(), " · ", fmt_range(range(), lang))
  })
  output$stats_plot <- renderPlot({
    x <- top5(); req(nrow(x$districts) > 0, length(selected_types()) > 0)
    v <- rev(x$districts[[x$by]]); lbl <- rev(x$districts$name)
    ref <- if (x$by == "rate") x$city$rate else NA
    op <- par(mar = c(3, 9.5, 0.5, 3), las = 1); on.exit(par(op))
    b <- barplot(v, names.arg = lbl, horiz = TRUE, col = "#3182bd", border = NA, xlim = c(0, max(v, ref, na.rm = TRUE) * 1.15))
    text(v, b, if (x$by == "rate") fmt_r(v, lang) else fmt_n(v, lang), pos = 4, cex = 0.85)
    if (!is.na(ref)) { abline(v = ref, lty = 2, col = "#475569"); mtext(t("city_average", fmt_r(ref, lang)), side = 1, line = 2, at = ref, cex = 0.8, col = "#475569") }
  }, res = 96)
  output$stats_table <- renderTable({
    x <- top5(); req(nrow(x$districts) > 0, length(selected_types()) > 0)
    out <- data.frame(seq_len(nrow(x$districts)), x$districts$name, figure(x$districts$n, lang))
    names(out) <- c(t("col_rank"), t("col_district"), t("col_calls"))
    if (HAS_RATES) {
      out[[t("col_rate")]]  <- ifelse(is.na(x$districts$rate), "—", fmt_r(x$districts$rate, lang))
      out[[t("col_ratio")]] <- ifelse(is.na(x$districts$rate), "—", sprintf(t("times"), fmt_r(x$districts$rate / x$city$rate, lang)))
    }
    out
  }, striped = TRUE, spacing = "s", align = "l")
  output$stats_notes <- renderUI({
    req(length(selected_types()) > 0); x <- top5()
    p(class = "note mt-2", t("stats_note", fmt_n(x$city$n, lang), if (HAS_RATES) t("stats_total_rate", fmt_r(x$city$rate, lang)) else ""))
  })

  # --- Madrid: people arrested or investigated by motive (whole city) ---
  if (CITY == "madrid" && HAS_ARR) {
    motive_name <- local_name(ARRNAMES, "motive_id", lang)
    motive_sel <- reactiveVal("domestic_gender_violence")
    observeEvent(input$arr_click, motive_sel(input$arr_click))
    arr_months <- sort(unique(ARR$month))
    arr_period <- reactive({
      r <- range(); sel <- arr_months[arr_months >= r[1] & arr_months <= r[2]]
      if (length(sel) == 0) sel <- utils::tail(arr_months, max(1, sum(MONTHS >= r[1] & MONTHS <= r[2])))
      sel
    })
    output$arr_title <- renderUI(h5(class = "mt-2 mb-1", tags$b(t("arrests_title")), " · ", t("city"), " · ", fmt_range(base::range(arr_period()), lang)))
    output$arr_table <- renderUI({
      sel <- arr_period(); d <- ARR[ARR$month %in% sel, ]
      n <- tapply(d$people, d$motive_id, sum)
      tot <- n[["total"]] %||% sum(n[names(n) != "total"])
      n <- sort(n[names(n) != "total"], decreasing = TRUE)
      prev <- ARR[ARR$month < min(sel) & ARR$month >= min(sel) - 366, ]
      np <- if (nrow(prev) > 0) tapply(prev$people, prev$motive_id, sum) * length(sel) / length(unique(prev$month)) else NULL
      row <- function(m) tags$tr(class = if (identical(m, motive_sel())) "sel",
        tags$td(tags$a(onclick = sprintf("Shiny.setInputValue('arr_click', '%s', {priority: 'event'})", m), motive_name[m] %||% m)),
        tags$td(class = "text-end", fmt_n(n[[m]], lang)),
        tags$td(class = "text-end text-muted", style = "white-space:nowrap", if (tot > 0) paste0(fmt_r(100 * n[[m]] / tot, lang), " %") else "—"),
        tags$td(class = "text-end text-muted", if (is.null(np) || is.na(np[m])) "—" else fmt_n(round(np[[m]]), lang)))
      tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(t("arrests_col_motive")), tags$th(class = "text-end", t("arrests_col_people")), tags$th(class = "text-end", "%"),
                           tags$th(class = "text-end", t("arrests_col_before"), info(t("arrests_before_help"))))),
        tags$tbody(lapply(names(n), row), tags$tr(class = "ref", tags$td(t("mossos_total")), tags$td(class = "text-end", fmt_n(tot, lang)), tags$td(), tags$td())))
    })
    output$arr_evol_head <- renderUI(h6(class = "mb-0", t("arrests_by_year"), " · ", motive_name[motive_sel()] %||% motive_sel()))
    output$arr_evol <- renderPlot({
      d <- ARR[ARR$motive_id == motive_sel(), ]; req(nrow(d) > 0); d <- d[order(d$month), ]
      year <- as.integer(format(d$month, "%Y")); v <- tapply(d$people, year, sum); nm <- tapply(d$month, year, length)
      op <- par(mar = c(2.4, 3.2, 0.5, 0.5), las = 1, cex.axis = 0.8, mgp = c(2, 0.5, 0)); on.exit(par(op))
      b <- barplot(v, names.arg = ifelse(nm < 12, paste0(names(v), "*"), names(v)), col = ifelse(nm < 12, "#9ecae1", "#3182bd"), border = NA, ylim = c(0, max(v) * 1.15))
      text(b, v, fmt_n(v, lang), pos = 3, cex = 0.7, col = "#475569")
    }, res = 96)
    output$arr_notes <- renderUI(div(class = "note mt-1", p(HTML(t("arrests_note"))), p(t("arrests_year_note"))))
  }
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
  maxv <- suppressWarnings(max(c(b$rate, di$rate, d$city$rate), na.rm = TRUE))
  bar <- function(lbl, v, strong = FALSE) tags$tr(
    tags$td(style = "width:38%", if (strong) tags$b(lbl) else lbl),
    tags$td(style = "width:44%;vertical-align:middle", if (is.na(v)) span(class = "note", t("less_than_calls", THRESHOLD)) else
      div(style = paste0("height:10px;border-radius:3px;background:", if (strong) "#3182bd" else "#94a3b8", ";width:", max(2, round(100 * v / maxv)), "%"))),
    tags$td(class = "text-end", style = "width:18%", if (is.na(v)) "—" else fmt_r(v, lang)))
  tagList(
    h6(t("rates_title"), info(t("residents", fmt_n(pop_of[[id]], lang), fmt_date(meta$population_date, lang)))),
    tags$table(class = "table table-sm table-borderless", tags$tbody(
      bar(name_of[[id]], b$rate, TRUE), bar(t("district_x", name_of[[dis_id]]), di$rate), bar(t("city"), d$city$rate))))
}

# Barcelona: crime known to the Mossos in the neighbourhood's district, same period
mossos_block <- function(dis_id, r, lang) {
  t <- function(k, ...) tr(k, lang, ...)
  mm <- sort(unique(MOSSOS$month)); sel <- mm[mm >= r[1] & mm <= r[2]]; warn <- NULL
  if (length(sel) == 0) { sel <- utils::tail(mm, max(1, sum(MONTHS >= r[1] & MONTHS <= r[2]))); warn <- t("mossos_other_period", fmt_range(range(sel), lang)) }
  if (length(sel) == 0) return(tagList(h6(t("mossos_title")), p(class = "note", t("mossos_none"))))
  d <- MOSSOS[MOSSOS$month %in% sel, ]
  total <- function(x, g) { v <- tapply(x$known, x$group_id, sum); v[g] }
  groups <- MGROUPS$group_id[!MGROUPS$group_id %in% c("sensitive", "unmapped")]
  in_dis <- total(d[d$territory_id == dis_id, ], groups); in_dis[is.na(in_dis)] <- 0
  in_city <- total(d, groups); in_city[is.na(in_city)] <- 0
  ord <- order(-in_dis)[seq_len(min(6, length(groups)))]
  tot_dis <- sum(d$known[d$territory_id == dis_id & d$group_id == "total"]); tot_city <- sum(d$known[d$group_id == "total"])
  p_dis <- pop_of[dis_id]; p_city <- pop_of[CITY_ID]
  rate <- function(n, p) if (is.na(p) || is.na(n)) NA else 1000 * n / p
  gname <- local_name(MGROUPS, "group_id", lang)
  row <- function(lbl, n, nc, strong = FALSE) {
    n <- apply_threshold(n, THRESHOLD)
    tags$tr(tags$td(if (strong) tags$b(lbl) else lbl), tags$td(class = "text-end", figure(n, lang)),
            tags$td(class = "text-end", if (is.na(n)) "—" else fmt_r(rate(n, p_dis), lang)),
            tags$td(class = "text-end text-muted", fmt_r(rate(nc, p_city), lang)))
  }
  tagList(
    h6(t("mossos_title"), info(t("mossos_note"))),
    div(class = "sub note", name_of[[dis_id]], " · ", fmt_range(range(sel), lang)),
    if (!is.null(warn)) p(class = "note", warn),
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th(t("mossos_col_type")), tags$th(class = "text-end", t("mossos_col_district")),
                         tags$th(class = "text-end", t("mossos_col_rate")), tags$th(class = "text-end", t("mossos_col_city")))),
      tags$tbody(lapply(ord, function(i) row(gname[[groups[i]]], in_dis[[i]], in_city[[i]])), row(t("mossos_total"), tot_dis, tot_city, TRUE))))
}

# Madrid: Municipal Police activity in the district (F2), same period
ACT_INDICATORS <- c("arrested_investigated", "losc_people", "losc_property", "losc_drug_possession", "losc_drug_use", "losc_weapons",
                    "drinking_in_public_adults", "venue_reports", "street_vending_counterfeit", "street_vending_food", "accidents_with_casualties")
activity_block <- function(dis_id, r, lang) {
  t <- function(k, ...) tr(k, lang, ...)
  mm <- sort(unique(ACT$month)); sel <- mm[mm >= r[1] & mm <= r[2]]; warn <- NULL
  if (length(sel) == 0) { sel <- utils::tail(mm, max(1, sum(MONTHS >= r[1] & MONTHS <= r[2]))); warn <- t("activity_other_period", fmt_range(range(sel), lang)) }
  if (length(sel) == 0) return(NULL)
  d <- ACT[ACT$month %in% sel & ACT$indicator %in% ACT_INDICATORS, ]
  total <- function(id) { v <- tapply(d$value[d$territory_id == id], d$indicator[d$territory_id == id], sum); v[ACT_INDICATORS] }
  in_dis <- total(dis_id); in_city <- total(CITY_ID)
  p_dis <- pop_of[dis_id]; p_city <- pop_of[CITY_ID]
  rate <- function(n, p) if (is.na(p) || is.na(n)) NA else 1000 * n / p
  row <- function(k) {
    n <- apply_threshold(in_dis[[k]], THRESHOLD)
    tags$tr(tags$td(t(paste0("act_", k))), tags$td(class = "text-end", figure(n, lang)),
            tags$td(class = "text-end", if (is.na(n) || !HAS_RATES) "—" else fmt_r(rate(n, p_dis), lang)),
            tags$td(class = "text-end text-muted", if (!HAS_RATES || is.na(in_city[[k]])) "—" else fmt_r(rate(in_city[[k]], p_city), lang)))
  }
  with_data <- ACT_INDICATORS[!is.na(in_dis)]
  if (length(with_data) == 0) return(NULL)
  tagList(
    h6(t("activity_title"), info(t("activity_note"))),
    div(class = "sub note", name_of[[dis_id]], " · ", fmt_range(range(sel), lang)),
    if (!is.null(warn)) p(class = "note", warn),
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th(t("activity_col")), tags$th(class = "text-end", t("mossos_col_district")),
                         tags$th(class = "text-end", t("mossos_col_rate")), tags$th(class = "text-end", t("mossos_col_city")))),
      tags$tbody(lapply(with_data, row))))
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
    if (CITY == "madrid" && HAS_ACT && !is.null(ACTMETA))
      tags$li(t("activity_source", fmt_month(ACTMETA$first_month, lang), fmt_month(ACTMETA$last_month, lang)), " · ", tags$a(href = ACTMETA$url, target = "_blank", "datos.madrid.es")),
    if (CITY == "barcelona" && HAS_MOSSOS && !is.null(MMETA))
      tags$li(t("mossos_source", fmt_month(MMETA$first_month, lang), fmt_month(MMETA$last_month, lang)), " · ", tags$a(href = MMETA$url, target = "_blank", "analisi.transparenciacatalunya.cat"))))
}

CITY_FUNCTIONS <- c("figure", "type_name", "month_labels", "view_months", "view_label", "compute", "ui_city", "server_city",
                    "neighbourhood_labels", "layer_neighbourhoods", "layer_selection", "rates_block", "mossos_block", "activity_block",
                    "reading_block", "source_block")

# Prepare the environment of a city (NULL if data or boundaries are missing)
make_city <- function(key) {
  x <- RAW[[key]]; f <- CITY_SOURCES[[key]]
  if (!has_rows(x$base) || is.null(x$meta) || !file.exists(path(f$gpkg))) return(NULL)
  app_env <- environment(make_city)
  e <- new.env(parent = app_env)
  e$CITY <- key; e$CITY_ID <- f$id; e$CITY_NAME <- unname(name_of[f$id]) %||% key
  e$base <- x$base; e$meta <- x$meta
  types <- x$types %||% data.frame(call_type = character(), name_es = character(), name_en = character())
  e$NAME_EN <- setNames(types$name_en, types$call_type); e$NAME_ES <- setNames(types$name_es, types$call_type)
  geo <- sf::st_read(path(f$gpkg), quiet = TRUE)
  e$geo_nb <- geo[geo$level == "neighbourhood", ]; e$geo_di <- geo[geo$level == "district", ]
  e$geo_nb$district <- unname(name_of[e$geo_nb$parent_id])
  pop <- POP_ALL[POP_ALL$territory_id == f$id | startsWith(POP_ALL$territory_id, paste0(f$id, "-")), ]
  e$HAS_RATES <- nrow(pop) > 0 && f$id %in% pop$territory_id
  e$pop_of <- if (e$HAS_RATES) setNames(pop$population, pop$territory_id) else c()
  e$BLOCKS <- MARTS$blocks
  e$THRESHOLD <- x$meta$threshold[1] %||% MARTS$threshold
  e$MONTHS <- sort(unique(x$base$month))
  e$tr <- function(key, lang, ...) TR(key, lang, ..., city = CITY)
  environment(e$tr) <- e
  for (fn in CITY_FUNCTIONS) { g <- get(fn, envir = app_env); environment(g) <- e; assign(fn, g, envir = e) }
  e
}

# =============================================================================
# Country view (Spain and the other countries): recorded crime by region,
# province and municipality from each country's official source; in Catalonia
# also Mossos by basic police area (F4). The functions below run inside one
# environment per country (make_country), where CC, CB, POPM, GEO_*, SP_LEVELS...
# are that country's. Spain keeps its own tables and texts.
# =============================================================================

# Offence types that can be chosen (no sensitive ones), totals first
cb_types <- function(lang) {
  if (!HAS_CB) return(character())
  x <- CB_TYPE_TABLE
  x$name <- if (lang == CLANG) x$name_local else x$name_en
  pri <- match(x$offence_type_id, AGGREGATES)
  x <- x[order(is.na(pri), pri, x$name), ]
  setNames(x$offence_type_id, x$name)
}

# Periods: full years (4th quarter) and the latest partial year, for a reference territory
cb_periods <- function(ref_id, lang) {
  d <- CB[CB$territory_id == ref_id & CB$offence_type_id == "total" & !is.na(CB$quarter), c("year", "quarter")]
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
      if (HAS_ABP && ABP_REGION %in% SP_REGIONS) conditionalPanel(sprintf("input.sp_region == '%s'", ABP_REGION),
        radioButtons("sp_source", tagList(t("sp_source"), info(HTML(t("sp_source_help")))),
                     choiceNames = list(t("sp_source_ministry"), t("sp_source_mossos")), choiceValues = c("ministry", "abp"), selected = "ministry"))),
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
    uiOutput("sp_abp_municipalities"),
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
  is_abp <- reactive(HAS_ABP && !isTRUE(input$sp_follow) && identical(input$sp_level, "municipality") && identical(input$sp_region, ABP_REGION) && identical(input$sp_source, "abp"))
  visible_ids <- reactiveVal(NULL)     # territories inside the map view (NULL = all)
  auto_sel <- reactiveVal(NULL)        # last territory selected by the app itself (not by the user): no flight to it
  gname <- if (HAS_ABP) local_name(MGROUPS, "group_id", lang) else c()
  abp_types <- function() { g <- MGROUPS$group_id[!MGROUPS$group_id %in% c("sensitive", "unmapped")]; c(setNames("total", t("mossos_total")), setNames(g, gname[g])) }
  # Types as in the cities: a category (radio) and its types (checkboxes with counts).
  # All ticked = the official aggregate of the category (it also includes the sensitive
  # types that are never listed); a subset = the sum of the ticked types.
  type_tree <- reactive({
    if (isTRUE(is_abp())) {
      g <- MGROUPS$group_id[!MGROUPS$group_id %in% c("sensitive", "unmapped")]
      return(list(blocks = c(total = t("mossos_total")), leaves = list(total = setNames(g, gname[g]))))
    }
    tt <- CB_TYPE_TABLE; nm <- if (lang == CLANG) tt$name_local else tt$name_en
    aggregates <- AGGREGATES
    # Leaf types: those directly inside an aggregate (a sub-type such as residential burglary,
    # already counted in burglary, is left out so that nothing is added twice)
    i <- which(tt$included_in %in% aggregates & !tt$offence_type_id %in% aggregates)
    list(blocks = c(total = nm[match("total", tt$offence_type_id)]), leaves = list(total = setNames(tt$offence_type_id[i], nm[i])))
  })
  block_leaves <- reactive({ tr <- type_tree(); tr$leaves$total %||% character() })
  types_ready <- reactiveVal(FALSE)
  pending_types <- reactiveVal(NULL)   # boxes to tick after a list change (click on a type not yet listed)
  listed_types <- reactiveVal(character(0))   # types currently listed (to tell "all ticked" from a partial choice)
  # Cumulative figure of each type for the reference territory in the period (for the counts)
  type_counts <- reactive({
    per <- period(); lv <- block_leaves(); if (length(lv) == 0) return(numeric())
    if (isTRUE(is_abp())) { d <- ABP[ABP$territory_id == ABP_REGION & in_periods(ABP, per) & ABP$group_id %in% lv, ]; n <- tapply(d$cumulative, d$group_id, sum) }
    else { d <- CB[CB$territory_id == CC & in_periods(CB, per) & CB$offence_type_id %in% lv, ]; n <- tapply(d$cumulative, d$offence_type_id, sum) }
    n <- n[lv]; n[is.na(n)] <- 0; names(n) <- lv
    n
  })
  observeEvent(list(type_tree(), type_counts()), {
    lv <- block_leaves(); n <- type_counts()
    lv <- lv[n[lv] > 0]; n <- n[lv]                      # only the types with figures in the period (as in the cities)
    if (length(lv) == 0) { updateCheckboxGroupInput(session, "sp_types", choices = character(0), selected = character(0)); types_ready(FALSE); return() }
    ticked <- isolate(input$sp_types) %||% character(0); listed <- listed_types()
    # Everything ticked before (the official total) stays "everything" when the list changes with the period
    all_before <- length(listed) == 0 || all(listed %in% ticked)
    sel <- unname(if (!is.null(pending_types()) && all(pending_types() %in% lv)) pending_types()
                  else if (!types_ready() || all_before || !any(ticked %in% lv)) lv else intersect(ticked, lv))
    pending_types(NULL); listed_types(lv)
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
    else { sub <- CB_TYPE_TABLE$offence_type_id[CB_TYPE_TABLE$included_in == id]; sub <- intersect(sub, lv); if (length(sub)) updateCheckboxGroupInput(session, "sp_types", selected = sub) }
  })
  abp_periods <- function() {
    d <- unique(ABP[ABP$level == "region" & ABP$group_id == "total", c("year", "to_month")]); d <- d[order(-d$year), ]
    lbl <- ifelse(d$to_month == 12, sprintf(t("sp_year"), d$year), sprintf(t("sp_partial"), d$year, month_names(lang, TRUE)[d$to_month]))
    setNames(paste0(d$year, "|", ifelse(d$to_month == 12, 4, ceiling(d$to_month / 3)), "|", d$to_month), lbl)
  }
  # Follow mode at municipality level: every municipality of Spain in view, whatever its region
  all_munis <- reactive(isTRUE(input$sp_follow) && identical(input$sp_level, "municipality") && !is.null(GEO_MUNI))
  # The region of any territory, climbing the parents (works for every country's identifiers)
  region_of <- function(id) {
    r <- id
    for (i in 1:5) { lv <- unname(level_of[r]); done <- is.na(r) | is.na(lv) | lv == "region"; if (all(done)) break; r[!done] <- unname(parent_of[r[!done]]) }
    ifelse(!is.na(r) & unname(level_of[r]) == "region", r, NA)
  }
  ref_id <- reactive({
    if (isTRUE(is_abp())) ABP_REGION
    else if (all_munis()) { id <- input$sp_territory; if (!is.null(id) && isTRUE(unname(level_of[id]) == "municipality")) region_of(id) %||% CC else CC }
    else if (identical(input$sp_level, "municipality")) input$sp_region %||% CC else CC
  })
  ref_name <- function(id) { nm <- unname(name_of[id]); nm[is.na(nm)] <- id[is.na(nm)]; ifelse(id == CC, CNAME(lang), pretty_name(nm)) }
  level_name <- function(level) if (level == "abp") t("sp_abp") else LEVEL_LABEL(level, lang, col = TRUE)
  display_name <- function(ids, level) if (level == "abp") ABPINFO$name[match(ids, ABPINFO$territory_id)] else pretty_name(unname(name_of[ids]) %||% ids)

  # INE population of every territory for a year (the most recent available up to that year)
  pop_year <- function(year) {
    if (!HAS_POPM) return(NULL)
    y <- max(POPM$year[POPM$year <= year], min(POPM$year)); p <- POPM[POPM$year == y, ]
    list(year = y, pop = setNames(p$population, p$territory_id))
  }
  pop_abp <- function(year) {
    if (!has_rows(ABPPOP)) return(NULL)
    y <- max(ABPPOP$year[ABPPOP$year <= year], min(ABPPOP$year)); p <- ABPPOP[ABPPOP$year == y, ]
    pc <- POPM$population[POPM$territory_id == ABP_REGION & POPM$year == y]
    list(year = y, pop = c(setNames(p$population, p$territory_id), if (length(pc)) setNames(pc, ABP_REGION)))
  }

  # Territories of the chosen level (or ABPs), types and periods
  observeEvent(list(input$sp_level, input$sp_region, input$sp_source, input$sp_follow), {
    req(input$sp_level)
    if (isTRUE(is_abp())) {
      a <- ABPINFO[order(ABPINFO$name), ]
      sel <- if ("ES-09-ABP-34" %in% a$territory_id) "ES-09-ABP-34" else a$territory_id[1]; auto_sel(sel)
      updateSelectizeInput(session, "sp_territory", choices = setNames(a$territory_id, a$name), selected = sel, server = FALSE)
      per <- abp_periods(); full <- per[grepl("\\|12$", per)]
      period_map(per); types_ready(FALSE)
      shinyWidgets::updateSliderTextInput(session, "sp_period", choices = rev(names(per)), selected = rep(names(if (length(full)) full[1] else per[1]), 2))
      return()
    }
    types_ready(FALSE)
    rows <- level_rows(input$sp_level, input$sp_region)
    ids <- unique(rows$territory_id); nm <- display_name(ids, input$sp_level)
    o <- order(nm)
    # Default: the most populated territory with data
    p <- pop_year(max(CB$year)); best <- if (!is.null(p)) ids[which.max(unname(p$pop[ids]) %||% 0)] else ids[1]
    keep <- isolate(input$sp_territory)
    # When following the map, the list is written only by the map-view observer (what is in view)
    if (!isTRUE(input$sp_follow) || !HAS_MAP) {
      sel <- if (!is.null(keep) && keep %in% ids) keep else best %||% ids[1]; auto_sel(sel)
      updateSelectizeInput(session, "sp_territory", choices = setNames(ids[o], nm[o]), selected = sel, server = FALSE)
    }
    visible_ids(NULL)
    per <- cb_periods(CC, lang); full <- per[grepl("\\|4$", per)]
    cur <- isolate(input$sp_period)
    sel <- if (!is.null(cur) && all(cur %in% names(per))) cur else rep(names(if (length(full)) full[1] else per[1]), 2)
    if (!identical(period_map(), per)) { period_map(per); shinyWidgets::updateSliderTextInput(session, "sp_period", choices = rev(names(per)), selected = sel) }
  })
  observeEvent(input$sp_click, updateSelectizeInput(session, "sp_territory", selected = input$sp_click))

  level_rows <- function(level, region) {
    if (level == "municipality" && !all_munis()) CB[CB$level == "municipality" & startsWith(CB$territory_id, paste0(region, "-")), ] else CB[CB$level == level, ]
  }
  # The slider selects a range of years (as the months in the cities). One year = the official
  # figure (January to the last quarter published); several years = their sum. A partial year is
  # only used on its own: inside a range it is left out.
  parse_period <- function(v) { p <- strsplit(v, "|", fixed = TRUE)[[1]]; list(year = as.integer(p[1]), quarter = as.integer(p[2]), to_month = if (length(p) >= 3) as.integer(p[3]) else as.integer(p[2]) * 3L) }
  period <- reactive({
    pm <- period_map() %||% cb_periods(CC, lang); labels <- rev(names(pm))     # oldest first
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
    pm <- period_map() %||% cb_periods(CC, lang); labels <- rev(names(pm))
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
    if (isTRUE(is_abp())) {
      d <- ABP[ABP$group_id %in% type & in_periods(ABP, per), ]
      d <- sum_types(d, c("territory_id", "level"))
      p <- pop_abp(per$year)
      d$population <- if (is.null(p)) NA else unname(p$pop[d$territory_id])
      d$n <- apply_threshold(d$cumulative, MARTS$threshold)
      d$name <- ifelse(d$level == "region", ref_name(ABP_REGION), display_name(d$territory_id, "abp"))
      d$level[d$level == "abp"] <- "abp"
      lvl <- "abp"
    } else {
      rows <- level_rows(input$sp_level, input$sp_region)
      refs <- CB[CB$territory_id == CC | (CB$level == "region" & (all_munis() | CB$territory_id == ref_id())), ]
      d <- rbind(rows, refs); d <- d[d$offence_type_id %in% type & in_periods(d, per), ]
      d <- sum_types(d, c("territory_id", "level", "name", "parent_id"))
      p <- pop_year(per$year)
      d$population <- if (is.null(p)) NA else unname(p$pop[d$territory_id])
      d$n <- apply_threshold(d$cumulative, MARTS$threshold)
      d$name <- ifelse(d$territory_id == CC, CNAME(lang), ifelse(d$level == "region" & input$sp_level != "region", ref_name(d$territory_id), display_name(d$territory_id, input$sp_level)))
      lvl <- input$sp_level
    }
    d$rate <- ifelse(is.na(d$n) | is.na(d$population), NA, 1000 * d$n / d$population / per$n)   # yearly average over a range
    if (lvl == "municipality" && all_munis()) {          # each municipality against its own region
      ref <- d$rate[match(region_of(d$territory_id), d$territory_id)]
      d$ratio <- ifelse(d$level == "municipality", d$rate / ref, NA)
    } else {
      ref <- d$rate[d$territory_id == ref_id()]
      d$ratio <- if (length(ref) == 1 && !is.na(ref)) d$rate / ref else rep(NA_real_, nrow(d))
    }
    list(d = d, level = lvl, ref_id = ref_id(), ref_name = ref_name(ref_id()), per = per, pop_year = p$year, abp = isTRUE(is_abp()), types = st,
         x_ref = t("sp_x_country", if (ref_id() == CC) CNAME(lang) else tolower(LEVEL_LABEL("region", lang, col = TRUE))),
         vs_ref = t("sp_vs_country", if (ref_id() == CC) CNAME(lang) else tolower(LEVEL_LABEL("region", lang, col = TRUE))))
  })

  output$sp_abp_municipalities <- renderUI({
    req(isTRUE(is_abp()), input$sp_territory)
    i <- ABPINFO[ABPINFO$territory_id == input$sp_territory, ]; if (nrow(i) == 0) return(NULL)
    div(class = "note mb-2", if (isTRUE(i$barcelona_district)) t("sp_abp_district") else tagList(tags$b(t("sp_abp_n", i$n_municipalities)), " ", i$municipalities))
  })
  output$sp_title <- renderUI({
    x <- country_data()
    h5(class = "mt-1 mb-2", tags$b(t(if (x$abp) "sp_title_mossos" else "sp_title")), " · ", x$types$label, " · ", period_text(), " · ",
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
          kpi(if (is.na(f$n)) t("less_than", MARTS$threshold) else fmt_n(f$n, lang), t("sp_k_n")),
          kpi(if (is.na(f$rate)) "—" else fmt_r(f$rate, lang), t("sp_k_rate")),
          if (id != x$ref_id) kpi(if (is.na(f$ratio)) "—" else sprintf(t("times"), fmt_r(f$ratio, lang)), x$vs_ref)),
      if (id != x$ref_id) comparison_block(f$rate, x$d$rate[x$d$territory_id == x$ref_id][1], x$d$rate[x$d$level == x$level], x$ref_name,
                                           paste(t("sp_k_n"), t("sp_k_rate")), tolower(if (x$level == "abp") t("sp_abp") else LEVEL_LABEL(x$level, lang)), lang),
      if (x$per$n == 1 && x$per$to_month < 12) div(class = "alert alert-secondary small py-1", t("sp_partial_note", month_names(lang, TRUE)[x$per$to_month])),
      if (x$per$n > 1) div(class = "alert alert-secondary small py-1", t("sp_range_note", x$per$n, x$per$year), if (x$per$dropped_partial) paste0(" ", t("sp_range_dropped"))))
  })

  # Breakdown of the chosen territory by offence type (same period), with the reference rate alongside
  output$sp_breakdown <- renderUI({
    x <- country_data(); id <- input$sp_territory; req(id); per <- x$per
    if (x$abp) return(breakdown_abp(id, per))
    d <- CB[CB$territory_id %in% c(id, x$ref_id) & in_periods(CB, per) & !CB$sensitive, ]
    if (per$n > 1) d <- d |> group_by(territory_id, offence_type_id) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
    if (nrow(d[d$territory_id == id, ]) == 0) return(NULL)
    p <- pop_year(per$year); pop <- function(i) if (is.null(p)) NA else unname(p$pop[i])
    types <- cb_types(lang); aggregates <- AGGREGATES
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
    tagList(h6(class = "mt-2 mb-1", t("sp_breakdown"), info(t("sp_breakdown_note"))),
      div(class = "table-sp", style = "max-height:none", tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(t("sp_col_type")), tags$th(class = "text-end", t("sp_col_n")), tags$th(class = "text-end", t("sp_col_rate")), tags$th(class = "text-end", x$ref_name))),
        tags$tbody(lapply(m$offence_type_id, row), if (any(d$territory_id == id & d$offence_type_id == "total")) row("total", TRUE)))))
  })
  breakdown_abp <- function(id, per) {
    d <- ABP[ABP$territory_id %in% c(id, ABP_REGION) & in_periods(ABP, per), ]
    if (per$n > 1) d <- d |> group_by(territory_id, group_id) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
    if (!any(d$territory_id == id)) return(NULL)
    p <- pop_abp(per$year); pop <- function(i) if (is.null(p)) NA else unname(p$pop[i])
    m <- d[d$territory_id == id & d$group_id != "total", ]; m <- m[order(-m$cumulative), ]
    row <- function(g, strong = FALSE) {
      n_t <- apply_threshold(d$cumulative[d$territory_id == id & d$group_id == g][1], MARTS$threshold); n_r <- d$cumulative[d$territory_id == ABP_REGION & d$group_id == g][1]
      rate_t <- if (is.na(n_t)) NA else 1000 * n_t / pop(id) / per$n; rate_r <- if (is.na(n_r)) NA else 1000 * n_r / pop(ABP_REGION) / per$n
      tags$tr(class = if (g %in% sel_types()$ids) "sel" else if (strong) "ref",
              tags$td(tags$a(onclick = sprintf("Shiny.setInputValue('sp_type_click', '%s', {priority: 'event'})", g), if (g == "total") t("mossos_total") else gname[[g]])),
              tags$td(class = "text-end", if (is.na(n_t)) t("less_than", MARTS$threshold) else fmt_n(n_t, lang)),
              tags$td(class = "text-end", if (is.na(rate_t)) "—" else fmt_r(rate_t, lang)),
              tags$td(class = "text-end text-muted", if (is.na(rate_r)) "—" else fmt_r(rate_r, lang)))
    }
    tagList(h6(class = "mt-2 mb-1", t("sp_breakdown"), info(t("sp_breakdown_mossos_note"))),
      div(class = "table-sp", style = "max-height:none", tags$table(class = "table table-sm",
        tags$thead(tags$tr(tags$th(t("sp_col_type")), tags$th(class = "text-end", t("sp_col_n")), tags$th(class = "text-end", t("sp_col_rate")), tags$th(class = "text-end", ref_name(ABP_REGION)))),
        tags$tbody(lapply(m$group_id, row), row("total", TRUE)))))
  }

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
    if (x$abp) {
      d <- ABP[ABP$territory_id %in% ids & ABP$group_id %in% type & ABP$to_month == 12, ]; req(nrow(d) > 0)
      d <- d |> group_by(territory_id, year) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
      d$pop <- vapply(seq_len(nrow(d)), function(i) { p <- pop_abp(d$year[i]); if (is.null(p)) NA_real_ else unname(p$pop[d$territory_id[i]]) }, 1)
    } else {
      d <- CB[CB$territory_id %in% ids & CB$offence_type_id %in% type & CB$quarter == 4, ]; req(nrow(d) > 0)
      d <- d |> group_by(territory_id, year) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
      d$pop <- vapply(seq_len(nrow(d)), function(i) { p <- pop_year(d$year[i]); if (is.null(p)) NA_real_ else unname(p$pop[d$territory_id[i]]) }, 1)
    }
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
    tags$table(class = "table table-sm",
      tags$thead(tags$tr(tags$th(level_name(x$level)), tags$th(class = "text-end", t("sp_col_pop")), tags$th(class = "text-end", t("sp_col_n")),
                         tags$th(class = "text-end", t("sp_col_rate")), tags$th(class = "text-end", x$x_ref))),
      tags$tbody(lapply(seq_len(nrow(refs)), function(i) row(refs[i, ], "ref")),
                 lapply(seq_len(nrow(main)), function(i) row(main[i, ], if (identical(main$territory_id[i], input$sp_territory)) "sel", link = TRUE))))
  })

  # --- Map ---
  if (HAS_MAP) {
    active_geo <- function(x) {
      # Following the map: every municipality of Spain with data (> 20,000 inh.), drawn once;
      # the smaller ones are not drawn (too many to redraw on every pan)
      if (x$level == "municipality" && all_munis()) GEO_MUNI_DATA else switch(x$level, abp = GEO_ABP, region = GEO_REGION, province = GEO_PROVINCE, GEO_MUNI[GEO_MUNI$region_id == x$ref_id, ])
    }
    output$sp_map <- renderLeaflet({
      x <- isolate(country_data()); g <- active_geo(x); req(!is.null(g), nrow(g) > 0)
      # First view: the default region, even when every municipality of Spain is drawn
      rg <- if (x$ref_id != CC) x$ref_id else DEFAULT_REGION
      bb <- sf::st_bbox(if (x$level == "municipality" && all_munis()) GEO_MUNI[GEO_MUNI$region_id == rg, ] else g)
      m <- leaflet(options = leafletOptions(minZoom = 2, zoomSnap = 0.5, zoomDelta = 0.5)) |>
        addTiles(attribution = paste(ATTRIBUTION_OSM, "·", t("boundaries_by", BOUNDARIES_ATTR))) |>
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
      if (!is.null(id) && id %in% c(CB$territory_id, ABP$territory_id)) updateSelectizeInput(session, "sp_territory", selected = id)
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
        title = HTML(paste0(t(if (shade == "n") "sp_legend_number" else "sp_legend_rate"), "<br><small>", htmltools::htmlEscape(title), "</small>")),
        labFormat = sc$format, na.label = t("less_than", MARTS$threshold), opacity = 0.8)
    }
    if (any(!with_data) || (x$level == "municipality" && all_munis()))
      map <- map |> addLegend("bottomright", colors = NO_DATA, labels = if (x$level == "municipality") NOT_PUBLISHED_LEGEND(lang) else t("sp_legend_no_data"), layerId = "sp_legend_nodata", opacity = 0.8)
    map <- map |> addPolygons(data = g, layerId = ~territory_id, group = "areas", color = ifelse(with_data, "#475569", "#94a3b8"),
                              weight = ifelse(with_data, 0.9, 0.5), opacity = 0.9, fillColor = fill, fillOpacity = opac, label = lbl,
                              labelOptions = LABEL_OPTIONS, highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE))
    if (x$level %in% c("municipality", "abp") && !is.null(GEO_REGION))
      map <- map |> addPolylines(data = if (all_munis()) GEO_REGION else GEO_REGION[GEO_REGION$territory_id == x$ref_id, ],
                                 group = "outline", color = "#1f2933", weight = 2, options = pathOptions(interactive = FALSE))
    if (!is.null(sel) && nzchar(sel) && sel %in% g$territory_id)
      map <- map |> addPolygons(data = g[g$territory_id == sel, ], group = "sel_area", color = "#c2410c", weight = 4, fill = FALSE, dashArray = "6 4", options = pathOptions(interactive = FALSE))
    map
  }

  output$sp_foot <- renderUI(if (isTRUE(is_abp())) t("sp_source_mossos_text") else paste(SOURCE_TEXT(lang), if (HAS_MAP) BOUNDARIES_TEXT(lang) else ""))
  output$sp_notes <- renderUI({
    x <- country_data()
    if (x$abp) return(div(class = "note mt-2", p(HTML(t("sp_mossos_note"))), p(t("sp_sensitive_mossos")),
                          if (!is.null(MMETA)) p(t("mossos_source", fmt_month(MMETA$first_month, lang), fmt_month(MMETA$last_month, lang))),
                          if (!is.null(x$pop_year)) p(t("sp_mossos_pop_note", x$pop_year))))
    div(class = "note mt-2", p(HTML(t("sp_note"))), p(t("sp_sensitive")), if (is.null(x$pop_year)) p(NO_POP_TEXT(lang)) else p(POP_NOTE(lang, x$pop_year)))
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
  meta <- if (has_rows(CMETA) && cc %in% CMETA$country) CMETA[CMETA$country == cc, ][1, ] else NULL
  if (is.null(meta)) return(NULL)
  e <- new.env(parent = app_env)
  e$CC <- cc; e$CB <- crime; e$POPM <- pop; e$HAS_CB <- TRUE; e$HAS_POPM <- has_rows(pop)
  e$CLANG <- meta$lang %||% "en"
  # Country name: local name in the local language, a translated name (country_<cc> key) elsewhere, English as the fallback
  e$CNAME <- function(lang) {
    if (lang == e$CLANG && nzchar(meta$name_local %||% "")) return(meta$name_local)
    k <- paste0("country_", cc); tx <- TR(k, lang); if (!identical(tx, k)) tx else meta$name_en %||% cc }
  # Mossos d'Esquadra by police area: a second source of one country, still read from its own tables
  # until phase 2 of Q-005 adds a `source` to the shared contract (the one remaining country-specific line)
  e$HAS_ABP <- cc == "ES" && HAS_ABP; e$ABP_REGION <- if (e$HAS_ABP) "ES-09" else "none"
  f <- paste0("data/territory_", tolower(cc), ".gpkg")
  e$GEO_MUNI <- read_layer(f, "municipality"); e$GEO_PROVINCE <- read_layer(f, "province"); e$GEO_REGION <- read_layer(f, "region"); e$GEO_ABP <- if (e$HAS_ABP) GEO_ABP else NULL
  e$HAS_MAP <- !is.null(e$GEO_REGION) || !is.null(e$GEO_MUNI)
  e$BOXES <- list(region = polygon_boxes(e$GEO_REGION), province = polygon_boxes(e$GEO_PROVINCE), municipality = polygon_boxes(e$GEO_MUNI), abp = polygon_boxes(e$GEO_ABP))
  e$GEO_MUNI_DATA <- if (!is.null(e$GEO_MUNI)) e$GEO_MUNI[e$GEO_MUNI$territory_id %in% crime$territory_id, ] else NULL
  # Levels with data (and, when there is a map, with boundaries)
  lv <- intersect(c("region", "province", "municipality"), unique(crime$level))
  if (e$HAS_MAP) lv <- lv[vapply(lv, function(l) !is.null(e[[switch(l, region = "GEO_REGION", province = "GEO_PROVINCE", "GEO_MUNI")]]), logical(1))]
  if (length(lv) == 0) lv <- intersect(c("region", "province", "municipality"), unique(crime$level))
  if (length(lv) == 0) return(NULL)
  e$SP_LEVELS <- lv
  r <- TERRITORY[TERRITORY$level == "region" & TERRITORY$territory_id %in% crime$territory_id, ]
  e$SP_REGIONS <- setNames(r$territory_id, pretty_name(r$name))[order(r$name)]
  e$DEFAULT_REGION <- if (nzchar(meta$default_region %||% "") && meta$default_region %in% e$SP_REGIONS) meta$default_region else e$SP_REGIONS[1]
  e$CB_TYPE_TABLE <- unique(crime[!crime$sensitive, c("offence_type_id", "included_in", "name_local", "name_en")])
  e$AGGREGATES <- unique(c("total", e$CB_TYPE_TABLE$included_in[!is.na(e$CB_TYPE_TABLE$included_in)]))
  e$AGGREGATES <- e$AGGREGATES[e$AGGREGATES %in% e$CB_TYPE_TABLE$offence_type_id]
  # Level labels: the module's English / local labels; other languages through the lvl_* keys
  split_labels <- function(col) if (!is.null(meta[[col]])) setNames(strsplit(paste0(meta[[col]], ";"), ";")[[1]], strsplit(meta$levels %||% "", ";")[[1]]) else c()
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
  loc <- function(what, lang) { x <- if (lang == e$CLANG) meta[[paste0(what, "_local")]] else NULL; if (is.null(x) || is.na(x) || !nzchar(x)) meta[[what]] %||% "" else x }
  e$SOURCE_TEXT <- function(lang) TR("sp_source_generic", lang, loc("source", lang), loc("publisher", lang), if (nzchar(meta$population_source %||% "")) loc("population_source", lang) else "—")
  e$POP_NOTE <- function(lang, year) TR("sp_note_pop_generic", lang, loc("population_source", lang), year)
  e$NO_POP_TEXT <- function(lang) TR("sp_no_pop_generic", lang)
  # "Not published" texts: a translation key when the module names one (every language), else its English / local text
  has_key <- function(k) nzchar(k %||% "") && !identical(TR(k, "en"), k)
  np <- function(lang) { if (has_key(meta$not_published_key)) return(TR(meta$not_published_key, lang))
    x <- if (lang == e$CLANG) meta$not_published_local else meta$not_published_en %||% ""; if (is.null(x) || is.na(x) || !nzchar(x)) TR("sp_not_published", lang) else x }
  e$NOT_PUBLISHED <- np
  e$NOT_PUBLISHED_LEGEND <- function(lang) if (has_key(meta$not_published_legend_key)) TR(meta$not_published_legend_key, lang) else TR("sp_legend_not_published", lang)
  # Countries that count something other than recorded offences (complaints, reported cases): the title,
  # the KPI noun and the legends use the module's words in English and in the local language
  mlab <- function(what, lang) { x <- meta[[paste0(what, if (lang == e$CLANG) "_local" else "_en")]]
    if (lang != "en" && lang != e$CLANG) {   # other languages: the translated measure kind (msr_<kind>_title / _noun), else English
      kind <- meta$measure_kind %||% ""; k <- paste0("msr_", kind, if (what == "count_noun") "_noun" else "_title")
      tx <- if (nzchar(kind)) TR(k, lang) else k; x <- if (!identical(tx, k)) tx else meta[[paste0(what, "_en")]] }
    if (is.null(x) || is.na(x) || !nzchar(x)) NULL else x }
  cap1 <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
  e$TR <- function(k, lang, ...) {
    if (!is.null(mlab("count_noun", lang))) {
      noun <- mlab("count_noun", lang)
      if (k == "sp_k_n") return(noun)
      if (k == "sp_legend_rate") return(paste(cap1(noun), TR("sp_k_rate", lang)))
      if (k == "sp_legend_number" || k == "sp_col_n") return(cap1(noun))
      if (k == "sp_title" && !is.null(mlab("measure_title", lang))) return(mlab("measure_title", lang))
    }
    TR(k, lang, ...)
  }
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
  meta_of <- function(city) if (!is.null(cities[[city]])) RAW[[city]]$meta else NULL
  mc <- meta_of("madrid"); mb <- meta_of("barcelona")
  rows <- list(
    if (HAS_ABP || HAS_MOSSOS) list(t("about_src_mossos"), t("about_mossos_measures"), t("about_mossos_territory"), if (!is.null(MMETA)) months(c(MMETA$first_month, MMETA$last_month)) else "", t("about_mossos_licence"), MMETA$url %||% NA),
    if (!is.null(mc)) list(t("about_src_calls"), t("about_calls_measures"), t("about_madrid_territory"), months(c(mc$first_month, mc$last_month)), mc$licence, mc$url),
    if (!is.null(mc) && HAS_ACT && !is.null(ACTMETA)) list(t("about_src_act"), t("about_act_measures"), t("about_act_territory"), months(c(ACTMETA$first_month, ACTMETA$last_month)), ACTMETA$licence, ACTMETA$url),
    if (!is.null(mb)) list(t("about_src_bcn"), t("about_bcn_measures"), t("about_bcn_territory"), months(c(mb$first_month, mb$last_month)), mb$licence, mb$url)
  )
  # One row per country (the first one is DEFAULT_COUNTRY): source, measure and notes, levels, period, licence, population and boundaries
  rows <- c(lapply(names(COUNTRIES), function(cc) { m <- CMETA[CMETA$country == cc, ][1, ]; e <- COUNTRIES[[cc]]
    per <- if (!is.null(m$first_period) && !is.na(m$first_period)) sprintf("%d – %d (%s)", m$first_period %/% 10, m$last_period %/% 10, month_names(lang, TRUE)[(m$last_period %% 10) * 3]) else ""
    lvls <- paste(vapply(e$SP_LEVELS, function(l) e$LEVEL_LABEL(l, lang), ""), collapse = ", ")
    cname <- e$CNAME(lang)
    list(paste0(m$source, " (", cname, ")"), paste0(m$measure, if (nzchar(m$notes_en %||% "")) paste0(" ", m$notes_en) else ""), paste0(cname, ": ", lvls), per,
         paste0(m$licence, if (nzchar(m$population_source %||% "")) paste0(" · ", t("about_pop_from", m$population_source)) else "",
                if (nzchar(m$boundaries_attribution %||% "")) paste0(" · ", t("boundaries_by", m$boundaries_attribution)) else ""), m$url) }), rows)
  rows <- Filter(Negate(is.null), rows)
  table <- tags$table(class = "table table-sm",
    tags$thead(tags$tr(lapply(c("about_col_source", "about_col_measures", "about_col_territory", "about_col_period", "about_col_licence"), function(k) tags$th(t(k))))),
    tags$tbody(lapply(rows, function(r) tags$tr(
      tags$td(tags$b(r[[1]]), if (!is.null(link(r[[6]]))) tagList(tags$br(), link(r[[6]]))),
      tags$td(r[[2]]), tags$td(r[[3]]), tags$td(r[[4]]), tags$td(r[[5]])))))
  first_calls <- if (!is.null(mc)) fmt_month(as.Date(mc$first_month), lang) else "—"
  bslib::page_navbar(
    title = paste(t("app_title"), "·", t("nav_about")), lang = lang, theme = bslib::bs_theme(version = 5), id = "nav",
    header = tags$head(tags$style(HTML(CSS))),
    bslib::nav_panel(t("nav_about"), value = "about", div(class = "about",
      h1(class = "h3", t("nav_about")),
      p(t("about_intro_1")), p(t("about_intro_2")),
      h2(t("about_h_sources")), table,
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
