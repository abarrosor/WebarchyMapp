# City view: filters, map, profile, statistics, panels; one environment per city (make_city) ----------
# Part of the app: sourced by app/app.R (same environment); nothing here runs on its own.

# =============================================================================
# City functions. Copied into one environment per city (make_city), so that
# `base`, `geo_nb`, `THRESHOLD`, `tr`... are that city's. Madrid and Barcelona
# share all the code.
# =============================================================================

figure <- function(n, lang) ifelse(is.na(n), tr("less_than", lang, THRESHOLD), fmt_n(n, lang))
type_name <- function(x, lang) {
  n <- unname(if (lang == (meta$lang %||% "es")) NAME_ES[x] else if (categories_known(NAME_EN, lang)) tr_category(NAME_EN[x], lang) else NAME_EN[x])
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
# The views worth a link: the latest year always; "5 years" only for a series longer than that (for a shorter one it
# is the whole series under another name); "all" when the series has more than its latest year. A series that fits in
# one year (Madrid's calls begin in January 2026) has a single view: the page then shows no links, only the slider.
views_offered <- function() {
  n <- vapply(VIEWS, function(v) length(view_months(v)), 1L)
  c("year", if (n[["five"]] < n[["all"]]) "five", if (n[["all"]] > n[["year"]]) "all")
}
# The length of the series in months, from its first period to its last (a yearly source: twelve for each year)
series_months <- function() {
  if (length(MONTHS) == 0) return(0)
  r <- range(MONTHS); y <- as.integer(format(r, "%Y")); m <- as.integer(format(r, "%m"))
  12 * (y[2] - y[1]) + (m[2] - m[1]) + if (TIME_UNIT == "year") 12 else 1
}
# Why the series begins where it does: the city's own sentence, when app/translations.csv has one (a text named
# "series_start_<city key>", with the first month loaded written in its %s), under the chart over time. Madrid: the
# calls are published from January 2026 on, and what exists for earlier years are other sources, shown elsewhere.
# A city without such a text shows nothing: most series are long, and where they begin is on the chart.
series_start_note <- function(lang) {
  k <- paste0("series_start_", CITY)
  if (identical(TR(k, "en"), k) || length(MONTHS) == 0) return(NULL)
  p(class = "note series-start", TR(k, lang, fmt_month(min(MONTHS), lang)))
}

# --- The rate of each complete year (under the chart over time of the statistics page, as in the countries) ---
# The complete years of the series: those with their twelve months loaded (every year of a yearly source)
full_years <- function() {
  if (length(MONTHS) == 0) return(integer())
  y <- as.integer(format(MONTHS, "%Y"))
  if (TIME_UNIT == "year") sort(unique(y)) else as.integer(names(which(table(y) == 12)))
}
# The years in which the source lacks one of these types (mart_city_gaps): no rate is given for them
gap_years <- function(block, types) {
  if (!has_rows(GAPS)) return(integer())
  g <- GAPS[GAPS$block == block & GAPS$event_type %in% types, ]; if (nrow(g) == 0) return(integer())
  sort(unique(unlist(Map(function(a, b) as.integer(format(a, "%Y")):as.integer(format(b, "%Y")), g$first_month, g$last_month))))
}
# One row per complete year for an area (`level`: city, district or neighbourhood; `id`) and some types of a
# category: the events of the year, with the threshold applied, per 1,000 of the residents of that year, and the
# city's rate as the reference of a district or a neighbourhood. The columns are those of the countries' chart by
# rate (ts_plot_rate, rate_head_ui): `pop` the residents the rate stands on and `pop_year` their year, `mark` "†"
# when that year is not the year of the figures. A year in which the source lacks one of the types has no rate:
# what is missing is not zero (`lacking`, an attribute, names those years). NULL: no register, no complete year, or
# no rate at all to show.
rate_by_year <- function(block, types, level, id) {
  years <- full_years()
  if (!HAS_RATES || length(years) == 0 || length(types) == 0) return(NULL)
  d <- base[base$block == block & base$call_type %in% types, ]
  d$year <- as.integer(format(d$month, "%Y")); d <- d[d$year %in% years, ]
  own <- switch(level, neighbourhood = d[d$neighbourhood_id %in% id, ], district = d[d$district_id %in% id, ], d)
  count <- function(x) { v <- as.numeric(tapply(x$events, factor(x$year, levels = years), sum)); v[is.na(v)] <- 0; v }
  lacking <- intersect(years, gap_years(block, types))
  n <- function(x) { v <- apply_threshold(count(x), THRESHOLD); v[years %in% lacking] <- NA; v }
  py <- lapply(years, function(y) pop_for_year(POPY, y))
  pop <- function(t) vapply(py, function(p) as.numeric(unname(p$pop[t])), 1)
  taken <- vapply(py, function(p) as.integer(unname(p$filled[id])), 1L)             # a year this area took for itself, where the others have a register
  exact <- vapply(py, function(p) isTRUE(p$exact), TRUE) & is.na(taken)
  s <- data.frame(year = years, start = as.Date(sprintf("%d-01-01", years)), label = as.character(years),
                  rate = 1000 * n(own) / pop(id), ref = if (identical(level, "city")) NA_real_ else 1000 * n(d) / pop(CITY_ID),
                  pop = pop(id), pop_wanted = years, pop_year = ifelse(is.na(taken), vapply(py, function(p) as.integer(p$year), 1L), taken),
                  mark = ifelse(exact, "", "†"), stringsAsFactors = FALSE)
  if (all(is.na(s$rate)) && all(is.na(s$ref)) && length(lacking) == 0) return(NULL)
  attr(s, "lacking") <- lacking
  s
}
# What stands under the chart over time: the place of the chart by rate (built by the server when there is a rate to
# show); nothing for a city without a register; and, for a series that has no complete year yet (Madrid's calls begin
# in January 2026), the title of the chart and a sentence that says so, so that nobody looks for a chart that cannot be.
rate_extra_ui <- function(lang) {
  if (!HAS_RATES) return(NULL)
  if (length(full_years()) == 0) return(div(class = "ts-rate", h5(class = "mt-3 mb-1", tags$b(tr("rate_years_title", lang))), p(class = "note", tr("rate_years_none", lang))))
  uiOutput("rate_block")
}

# The residents behind the rates of a period: by territory, the register of each year of the period, each year
# counted as many times as it has months of data in it (pop_for_period, app_helpers.R). A year without a register
# takes the nearest one: `exact` is then FALSE and the rates are marked † wherever they are shown.
range_months <- function(range) MONTHS[MONTHS >= range[1] & MONTHS <= range[2]]
period_pop <- function(months) if (HAS_RATES && length(months) > 0) pop_for_period(POPY, as.integer(format(as.Date(months), "%Y"))) else NULL
register_date <- function(id, year) { d <- POPY$ref_date[POPY$territory_id == id & POPY$year == year]; if (length(d)) as.Date(d[1]) else NA }
# The words for "which register": its date when the period has one, "average 2024–2026" when it has several
register_text <- function(pp, id, lang) {
  own <- if (id %in% names(pp$filled)) pp$filled[[id]] else NULL                 # this territory took a year of its own
  if (!is.null(own)) return(own)
  d <- if (length(pp$years) == 1) register_date(id, pp$years) else NA
  if (!is.na(d)) fmt_date(d, lang) else pop_years_text(pp$years, lang)
}
# Which way the residents of a territory went between the first and the last register of the period (NULL with one)
register_trend <- function(pp, id, lang) {
  if (is.null(pp) || length(pp$years) < 2) return(NULL)
  y <- range(pp$years); p1 <- unname(pop_for_year(POPY, y[1])$pop[id]); p2 <- unname(pop_for_year(POPY, y[2])$pop[id])
  if (is.na(p1) || is.na(p2)) NULL else pop_trend_text(p1, y[1], p2, y[2], lang)
}
pop_marked <- function(pp, id = NULL) !is.null(pp) && (!pp$exact || (!is.null(id) && id %in% names(pp$filled)))

# Counts and rates by neighbourhood, district and city for a block and month
# range, with the threshold applied. `types`: ticked types (NULL = all)
compute <- function(block, range, types = NULL) {
  d <- base[base$block == block & base$month >= range[1] & base$month <= range[2], ]
  if (!is.null(types)) d <- d[d$call_type %in% types, ]
  pp <- period_pop(range_months(range)); people <- if (is.null(pp)) setNames(numeric(0), character(0)) else pp$pop
  rate <- function(n, id) if (HAS_RATES) ifelse(is.na(n), NA, 1000 * n / unname(people[id])) else NA
  nb <- d |> filter(!is.na(neighbourhood_id)) |> group_by(territory_id = neighbourhood_id) |> summarise(raw = sum(events), .groups = "drop")
  nb <- data.frame(territory_id = geo_nb$territory_id) |> left_join(nb, by = "territory_id") |>
    mutate(raw = coalesce(raw, 0L), n = apply_threshold(raw, THRESHOLD), rate = rate(n, territory_id))
  di <- d |> filter(!is.na(district_id)) |> group_by(territory_id = district_id) |> summarise(raw = sum(events), .groups = "drop")
  di <- data.frame(territory_id = geo_di$territory_id) |> left_join(di, by = "territory_id") |>
    mutate(raw = coalesce(raw, 0L), n = apply_threshold(raw, THRESHOLD), rate = rate(n, territory_id), name = unname(name_of[territory_id]))
  n_city <- sum(d$events)
  list(neighbourhoods = nb, districts = di, city = list(n = n_city, rate = rate(n_city, CITY_ID)), pop = pp)
}

# Choices of the area selector of the statistics tab: the city, its districts (when there are several), its neighbourhoods
zone_choices <- function(lang) {
  cap <- function(x) upper_first(x, lang)
  nb <- geo_nb[order(geo_nb$name), ]
  ch <- list("city"); names(ch) <- paste0(tr("stats_zone_city", lang), " (", CITY_NAME, ")")
  if (!SINGLE_DISTRICT) { di <- geo_di[order(geo_di$name), ]; ch[[cap(tr("cmp_districts", lang))]] <- as.list(setNames(di$territory_id, di$name)) }
  ch[[cap(tr("cmp_neighbourhoods", lang))]] <- as.list(setNames(nb$territory_id, if (SINGLE_DISTRICT) nb$name else paste0(nb$name, " (", nb$district, ")")))
  ch
}

# --- City interface ----------------------------------------------------------------
ui_city <- function(lang) {
  t <- function(k, ...) tr(k, lang, ...)
  labels <- month_labels(lang)
  start <- labels[view_months("year")]          # on opening: year view, last 3 months selected
  # ... and the first category that has events in those months (a category the source lacks there would open empty)
  opening <- MONTHS[view_months("year")]; opening <- opening[seq(max(1, length(opening) - 2), length(opening))]
  with_events <- BLOCKS[BLOCKS %in% unique(base$block[base$month %in% opening & base$events > 0])]
  opening_block <- if (length(with_events)) with_events[1] else BLOCKS[1]

  # A single-choice box that already holds a value is "full" and takes no typing: on opening, the value is set aside
  # (nothing changes for the server) so that a name can be typed, and it comes back if the box is closed without choosing
  reopen <- list(
    onDropdownOpen  = I("function() { if (this.getValue()) { this._prev = this.getValue(); this.clear(true); } }"),
    onDropdownClose = I("function() { var s = this; setTimeout(function() { if (!s.getValue() && s._prev) s.setValue(s._prev, true); }, 0); }"))
  selector <- selectizeInput(
    "neighbourhood", NULL, width = "100%",
    choices = c(setNames("", paste("\U0001F50D", t("search_hint"))),
                setNames(geo_nb$territory_id, paste0(geo_nb$name, " (", geo_nb$district, ")"))[order(geo_nb$name)]),
    options = reopen)

  # The filters both tabs share (category, types, period). One set of controls: it sits in the sidebar of the map and
  # moves to the bar at the top of the statistics page when that tab opens (JS in app_styles.R), so that a choice
  # made in one tab is the choice in the other. The types are a <details>: an open list in the sidebar, a dropdown
  # with the boxes in columns on the statistics page.
  filters <- div(id = "wm-filters", class = "wm-filters",
    div(class = "f-group f-block",
        div(class = "facet", span(t("category"), tip(paste(vapply(BLOCKS, function(b) t(paste0("desc_", b)), ""), collapse = " ")))),
        radioButtons("block", NULL, choiceNames = lapply(BLOCKS, block_name, lang), choiceValues = BLOCKS, selected = opening_block),
        uiOutput("gap_note")),
    div(class = "f-group f-types-wrap",
        div(class = "facet", span(t("call_types"))),
        tags$details(class = "f-types", open = NA,
          tags$summary(uiOutput("types_count", inline = TRUE)),
          div(class = "f-types-body",
              div(class = "f-types-links", actionLink("types_all", t("all")), " · ", actionLink("types_none", t("none"))),
              checkboxGroupInput("types", NULL, choices = character(0)),
              uiOutput("types_summary")))),
    div(class = "f-group f-period",
        div(class = "facet", span(t("period"))),
        { vo <- views_offered()                                       # no link that would do what another one does (a series of one year: none)
          if (length(vo) > 1) div(class = "chips", lapply(vo, function(v) actionLink(paste0("view_", v), view_label(v, lang), class = if (v == "year") "active"))) },
        shinyWidgets::sliderTextInput("period", NULL, choices = start, selected = start[c(max(1, length(start) - 2), length(start))],
                                      grid = FALSE, force_edges = TRUE, width = "100%"),
        div(class = "period-pick",                                     # phones: two month selectors instead of the slider (U11)
            selectInput("period_from", NULL, choices = labels, selected = start[max(1, length(start) - 2)], selectize = FALSE, width = "100%"), span("–"),
            selectInput("period_to", NULL, choices = labels, selected = start[length(start)], selectize = FALSE, width = "100%"))),
    if (!HAS_RATES) div(class = "f-group f-alert alert alert-warning small py-2 mb-0", HTML(t("missing_population")))
  )
  foot <- function() div(class = "foot", t("what_shows"), tags$br(), t("footer"), " · ", cite_link(lang))

  bslib::page_navbar(
    title = page_title(lang, paste(t("app_title"), "·", CITY_NAME)), window_title = paste(t("app_title"), "·", CITY_NAME), lang = lang, theme = bslib::bs_theme(version = 5), id = "nav",
    header = page_header(lang),
    # The map and its sidebar: the search box, the filters, the profile of the neighbourhood (P-046: the sidebar belongs to the map only)
    bslib::nav_panel(t("nav_map"), value = "map",
      bslib::layout_sidebar(
        border = FALSE, border_radius = FALSE, padding = 0, fillable = TRUE,
        sidebar = bslib::sidebar(
          width = 340, open = "desktop",
          selector,
          bslib::accordion(id = "acc", multiple = TRUE, open = "filters",
                           bslib::accordion_panel(t("filters"), value = "filters",
                             div(id = "side-filters", filters),
                             radioButtons("shade", tagList(t("shade_by"), info(HTML(t("shade_help")), "rules", lang)), inline = TRUE,
                                          choices = shade_options(lang, HAS_NB_RATES), selected = shade_default(HAS_NB_RATES))),
                           # The profile of the neighbourhood: shown here, or in the card over the map (the reader's choice; the
                           # same element, moved by the browser: app_styles.R)
                           bslib::accordion_panel(t("profile"), value = "profile",
                             div(id = "profile-dock",
                                 div(class = "profile-dock-tools", tags$button(id = "profile-float-btn", type = "button", t("profile_to_map"))),
                                 uiOutput("profile")))),
          foot()),
        div(class = "map-wrap html-fill-item html-fill-container",
            leafletOutput("map", height = "100%"),
            div(class = "where-box", uiOutput("where")),          # the box is this div, not the output's own container (see OUTPUT CONTAINERS in app_styles.R)
            div(id = "profile-card", class = "profile-card", role = "dialog", `aria-label` = t("profile"),
                div(class = "profile-card-tools", tags$button(id = "profile-dock-btn", type = "button", t("profile_to_side"))),
                div(id = "profile-card-body"))))),
    # The statistics page: no sidebar; a bar with the area and the shared filters, then the charts
    bslib::nav_panel(
      t("nav_stats"), value = "stats",
      div(id = "stats-page",
        div(id = "stats-bar",
            # The area the charts by type and over time describe: the whole city, a district or a neighbourhood
            # (it follows the neighbourhood picked on the map, and the map follows it)
            div(class = "f-group stats-zone", div(class = "facet", span(t("stats_zone"))),
                selectizeInput("stats_zone", NULL, width = "100%", choices = zone_choices(lang), selected = "city",
                               options = c(reopen, list(placeholder = paste("\U0001F50D", t("zone_search")), maxOptions = 2000)))),
            div(id = "stats-filters")),
        # The figures of the page as an Excel file (P-065), for whoever would draw them another way
        if (HAS_XLSX) div(class = "stats-tools", downloadLink("xl_stats", t("xl_button"), class = "btn btn-sm btn-outline-secondary"), info(t("xl_help", THRESHOLD))),
        # By type and over time: side by side on a wide screen (alone, each stretched from one edge of the page to the
        # other; .stats-pair in app_styles.R). At the left the chart by type and, under it, the time of year; at the
        # right the series by number with the rate of each complete year under it and the period bar they share.
        # On a narrower screen: one under the other, in the order they are written here.
        div(class = "stats-pair",
          div(class = "stats-pair-types",
              uiOutput("types_title"),
              div(class = "ts-block", plotOutput("types_plot", height = "auto", click = "types_click")),
              uiOutput("types_notes")),
          div(class = "stats-pair-series",
              ts_ui(lang, if (TIME_UNIT == "year") "year" else c("month", "quarter", "year"), selected = TS_GRAN, span_months = series_months(), extra = rate_extra_ui(lang)),
              series_start_note(lang)),
          # The seasonal profile needs a complete calendar year of months: a city that has none (a yearly source, a
          # series that began this year) has no such block, not an empty one. The place in the pair is a plain div: the
          # conditional panel inside it has no box of its own in a recent shiny (OUTPUT CONTAINERS in app_styles.R)
          div(class = "stats-pair-season",
            conditionalPanel("output.has_season", tags$hr(), uiOutput("season_title"),
              div(class = "stats-split", plotOutput("season_plot", height = "260px"), div(uiOutput("season_notes")))))),
        tags$hr(),
        bslib::layout_columns(col_widths = c(6, 6), fill = FALSE,
          if (SINGLE_DISTRICT) div(class = "shiny-input-container", tags$label(class = "control-label", t("stats_level")), div(t("col_neighbourhood"))) else
            radioButtons("stats_level", t("stats_level"), inline = TRUE, choices = setNames(c("district", "neighbourhood"), c(t("col_district"), t("col_neighbourhood"))), selected = STATS_LEVEL),
          radioButtons("sort", t("sort_by"), inline = TRUE,
                       choices = if (HAS_RATES) setNames(c("rate", "n"), c(t("sort_rate"), t("sort_number"))) else setNames("n", t("sort_number")))),
        uiOutput("stats_title"),
        bslib::layout_columns(col_widths = c(7, 5), fill = FALSE, plotOutput("stats_plot", height = "300px"), div(uiOutput("stats_table"))),
        uiOutput("stats_notes"),
        lapply(panel_ids("city"), function(id) tagList(
          tags$hr(), uiOutput(paste0("pn_", id, "_title")),
          bslib::layout_columns(col_widths = c(5, 7), fill = FALSE, div(class = "table-sp", uiOutput(paste0("pn_", id, "_table"))),
                                div(uiOutput(paste0("pn_", id, "_evol_head")), plotOutput(paste0("pn_", id, "_evol"), height = "260px"))),
          uiOutput(paste0("pn_", id, "_notes")))),
        tags$hr(), foot())
    ),
    !!!nav_switcher(CITY, lang)
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

  observeEvent(input$nav, session$sendCustomMessage("mode", input$nav))   # the filters follow the tab (also done in the browser on the tab's own event)

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
    nm <- names(n) %||% character(0)                       # a category with no event in the period (a gap of the source): an empty list, not NULL
    sel <- if (block_changed || all_before) nm else intersect(ticked, nm)
    updateCheckboxGroupInput(session, "types", selected = sel,
      choiceNames = lapply(nm, function(x) count_label(type_name(x, lang), n[[x]], lang, type_note(x, lang), mark = tip)),
      choiceValues = nm)
    prev_types(nm); prev_block(input$block); ready(TRUE)
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
  # the phone month selectors and the slider follow each other (a month outside the slider's current choices widens them to every month)
  observeEvent(input$period, { p <- input$period; if (length(p) == 2 && !anyNA(p)) {
    if (!identical(input$period_from, p[1])) updateSelectInput(session, "period_from", selected = p[1])
    if (!identical(input$period_to, p[2])) updateSelectInput(session, "period_to", selected = p[2]) } })
  observeEvent(list(input$period_from, input$period_to), {
    i <- match(c(input$period_from, input$period_to), labels); req(length(i) == 2, !anyNA(i)); sel <- labels[sort(i)]
    if (identical(sel, input$period)) return()
    shinyWidgets::updateSliderTextInput(session, "period", choices = labels, selected = sel); session$sendCustomMessage("chip", "")
  }, ignoreInit = TRUE)

  selected_types <- reactive({ if (!ready()) return(block_types()); intersect(input$types %||% character(0), block_types()) })
  all_ticked <- reactive(setequal(selected_types(), block_types()))
  # The text of the types dropdown on the statistics page: "All (10)", "3 of 10 types", "None"
  output$types_count <- renderUI({
    k <- length(selected_types()); n <- length(block_types())
    span(if (n > 0 && all_ticked()) paste0(t("all"), " (", n, ")") else if (k == 0) t("none") else t("n_of_types", k, n))
  })
  outputOptions(output, "types_count", suspendWhenHidden = FALSE)
  output$types_summary <- renderUI({
    if (all_ticked()) return(NULL)
    div(class = "filter-summary", t("n_of_types", length(selected_types()), length(block_types())))
  })
  data <- reactive(compute(input$block, range(), selected_types()))
  # What the source lacks of this category in the period (a cut file): said under the filters, and in place of
  # "no type selected" when the category has no type left to tick for that reason
  gap <- reactive(gap_info(input$block, range(), lang))
  output$gap_note <- renderUI({ g <- gap(); if (is.null(g)) NULL else div(class = "alert alert-warning small py-2 mb-0 mt-2 gap-note", g$text) })
  outputOptions(output, "gap_note", suspendWhenHidden = FALSE)
  gap_empty <- function() length(block_types()) == 0 && !is.null(gap())     # no type to tick in the period because the source lacks them all
  none_note <- function() if (gap_empty()) NULL else p(class = "note", t("no_type_selected"))   # statistics page: the sentence already stands in the bar above
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
  # The profile of the neighbourhood opens in a card over the map (P-058), or in the side panel when the reader moved
  # it there (input$profile_place, kept by the browser). In the side panel the filters fold to give it room, as before;
  # over the map they stay open. The map keeps the neighbourhood clear of the card: room at the right, or below it
  # when the map is narrow and the card is a sheet at the bottom.
  on_map <- reactive(!identical(input$profile_place, "side"))
  side_panels <- function(profile_open) {
    if (profile_open) { bslib::accordion_panel_open("acc", "profile", session = session); bslib::accordion_panel_close("acc", "filters", session = session) }
    else bslib::accordion_panel_open("acc", "filters", session = session)
  }
  fly_options <- function() {
    if (!on_map()) list(maxZoom = 14, padding = c(60, 60))
    else if (isTRUE(input$map_narrow)) list(maxZoom = 14, paddingTopLeft = c(30, 30), paddingBottomRight = c(30, round(0.62 * (session$clientData[["output_map_height"]] %||% 500))))
    else list(maxZoom = 14, paddingTopLeft = c(60, 60), paddingBottomRight = c(430, 60))
  }
  observeEvent(input$neighbourhood, {
    p <- layer_selection(leafletProxy("map"), input$neighbourhood)
    open <- nzchar(input$neighbourhood)
    session$sendCustomMessage("profile", list(open = open, changed = TRUE))
    if (open) {                                                     # also when it is chosen on the statistics page: back on the map, its profile is open
      if (!on_map()) side_panels(TRUE)
      bb <- sf::st_bbox(geo_nb[geo_nb$territory_id == input$neighbourhood, ])
      p |> flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]], options = fly_options())
    } else if (!on_map()) side_panels(FALSE)
  }, ignoreInit = TRUE)
  observeEvent(input$profile_place, { if (nzchar(input$neighbourhood %||% "")) side_panels(!on_map()) }, ignoreInit = TRUE)   # the reader moved an open profile
  observeEvent(input$close_profile, updateSelectizeInput(session, "neighbourhood", selected = ""))

  # The box over the map that says where the map is: the city, the district and the neighbourhood chosen. The city is a
  # link back to the whole city while a neighbourhood is open.
  output$where <- renderUI({
    id <- input$neighbourhood %||% ""; sel <- nzchar(id) && id %in% geo_nb$territory_id
    rows <- list(list(level = t("city"), name = CITY_NAME, current = !sel, send = if (sel) "where_pick", value = "city"))
    if (!SINGLE_DISTRICT) rows <- c(rows, list(list(level = t("col_district"), name = if (sel) name_of[[parent_of[[id]]]] else "—", off = !sel)))
    rows <- c(rows, list(list(level = t("col_neighbourhood"), name = if (sel) name_of[[id]] else "—", current = sel, off = !sel)))
    where_box(rows, t("where_title"))
  })
  observeEvent(input$near_pick, { id <- input$near_pick$value; if (!is.null(id) && id %in% geo_nb$territory_id) updateSelectizeInput(session, "neighbourhood", selected = id) })   # a bordering neighbourhood, clicked in the profile
  observeEvent(input$where_pick, {
    updateSelectizeInput(session, "neighbourhood", selected = "")
    bb <- sf::st_bbox(geo_nb); leafletProxy("map") |> flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })
  # Cite this data: the source of the city's figures, the period on screen and the day the source was last read
  observeEvent(input$cite, showModal(cite_modal(list(publisher = meta$publisher, title = meta$source_local %||% meta$source_name, url = meta$url, licence = meta$licence,
                                                     period = fmt_range(range(), lang), consulted = fmt_date(as.Date(meta$loaded_at), lang), threshold = THRESHOLD), lang, CITY)))

  # --- The area of the statistics tab (city, district or neighbourhood), in step with the map's neighbourhood ---
  zone <- reactive({
    z <- input$stats_zone %||% "city"
    if (z %in% geo_nb$territory_id) list(level = "neighbourhood", id = z, name = name_of[[z]])
    else if (z %in% geo_di$territory_id) list(level = "district", id = z, name = name_of[[z]])
    else list(level = "city", id = CITY_ID, name = CITY_NAME)
  })
  zone_rows <- function(d) { z <- zone(); switch(z$level, neighbourhood = d[d$neighbourhood_id %in% z$id, ], district = d[d$district_id %in% z$id, ], d) }
  observeEvent(input$neighbourhood, {
    z <- input$stats_zone %||% "city"; nb <- input$neighbourhood
    if (nzchar(nb)) { if (!identical(z, nb)) updateSelectizeInput(session, "stats_zone", selected = nb) }
    else if (z %in% geo_nb$territory_id) updateSelectizeInput(session, "stats_zone", selected = "city")
  }, ignoreInit = TRUE)
  observeEvent(input$stats_zone, {
    z <- input$stats_zone; nb <- input$neighbourhood %||% ""
    if (z %in% geo_nb$territory_id) { if (!identical(z, nb)) updateSelectizeInput(session, "neighbourhood", selected = z) }
    else if (nzchar(nb)) updateSelectizeInput(session, "neighbourhood", selected = "")
  }, ignoreInit = TRUE)

  # --- Statistics: the ticked types of the area in the period, as bars; a click keeps one type for the chart over time ---
  TYPES_MAX <- 12
  focus <- reactiveVal(NULL); type_pos <- reactiveVal(NULL)
  type_bars <- reactive({
    if (length(selected_types()) == 0) return(NULL)
    r <- range()
    d <- zone_rows(base[base$block == input$block & base$call_type %in% selected_types() & base$month >= r[1] & base$month <= r[2], ])
    raw <- tapply(d$events, d$call_type, sum); raw <- raw[!is.na(raw) & raw > 0]
    if (!length(raw)) return(data.frame(type = character(0), n = numeric(0), label = character(0)))
    x <- data.frame(type = names(raw), n = apply_threshold(as.numeric(raw), THRESHOLD), stringsAsFactors = FALSE)
    x$label <- vapply(x$type, type_name, "", lang)
    x[order(is.na(x$n), -ifelse(is.na(x$n), 0, x$n), x$label), ]          # figures under the threshold last, by name (their order would tell their size)
  })
  observe({ f <- focus(); if (!is.null(f) && !(f %in% utils::head(type_bars()$type, TYPES_MAX))) focus(NULL) })   # the type left the chart (filters, area, period)
  observeEvent(input$types_click, {
    b <- type_pos(); if (is.null(b) || !nrow(b)) return()
    ty <- b$type[which.min(abs(b$y - input$types_click$y))]
    focus(if (identical(focus(), ty)) NULL else ty)
  })
  observeEvent(input$types_focus_clear, focus(NULL))
  output$types_title <- renderUI({
    if (length(selected_types()) == 0) return(none_note())
    h5(class = "mt-2 mb-1", tags$b(t("types_chart_title")), " ", info(t("types_chart_help", THRESHOLD)), " · ", zone()$name, " · ", selection_text(), " · ", fmt_range(range(), lang))
  })
  output$types_plot <- render_chart({
    x <- type_bars(); req(!is.null(x))
    if (nrow(x) == 0) { par(mar = c(0, 0, 0, 0)); plot.new(); text(0.5, 0.5, t("no_calls"), col = ink("#666666")); type_pos(NULL); return(invisible()) }
    x <- utils::head(x, TYPES_MAX); x <- x[rev(seq_len(nrow(x))), ]            # the largest on top
    v <- ifelse(is.na(x$n), 0, x$n); f <- focus()
    names <- fit_labels(x$label, 0.45 * par("din")[1])                       # the names: up to 45% of the chart (a wider one is cut), in a margin as wide as the widest of them
    col <- if (is.null(f)) rep("#3182bd", nrow(x)) else ifelse(x$type == f, ink("#08519c"), ink("#c6dbef"))
    op <- par(mar = fit_mar(c(2.2, (names$width + 0.2) / par("csi"), 0.3, 1)), las = 1, cex.axis = 0.8, mgp = c(2, 0.5, 0)); on.exit(par(op))
    b <- barplot(v, names.arg = names$labels, horiz = TRUE, col = col, border = NA, xlim = c(0, max(v, 1) * 1.22), xaxt = "n")
    axis(1, at = pretty(c(0, max(v, 1))), labels = fmt_n(pretty(c(0, max(v, 1))), lang), lwd = 0, lwd.ticks = 1)
    text(v, b, ifelse(is.na(x$n), t("less_than", THRESHOLD), fmt_n(v, lang)), pos = 4, cex = 0.8, xpd = TRUE, col = ink("#333333"))
    type_pos(data.frame(y = as.numeric(b), type = x$type, stringsAsFactors = FALSE))
  }, res = 96, height = function() { x <- type_bars(); k <- if (is.null(x)) 0 else min(nrow(x), TYPES_MAX); if (k == 0) 60 else 56 + 26 * k })
  output$types_notes <- renderUI({
    x <- type_bars(); if (is.null(x) || nrow(x) == 0) return(NULL); f <- focus()
    tagList(
      if (!is.null(f)) p(class = "note mb-1", tags$b(t("types_focus", type_name(f, lang))), " · ", actionLink("types_focus_clear", t("types_focus_clear"))),
      if (nrow(x) > TYPES_MAX) p(class = "note mb-1", t("types_more", nrow(x) - TYPES_MAX)),
      if (zone()$level != "city" && sum(x$n, na.rm = TRUE) < MIN_EVENTS_STABLE) p(class = "note mb-1", t("few_events", MIN_EVENTS_STABLE)))
  })

  # --- Neighbourhood profile ---
  output$profile <- renderUI({
    input$profile_moved                                              # sent again after the browser moved it between the card and the side panel (app_styles.R)
    id <- input$neighbourhood
    if (!nzchar(id)) return(div(class = "profile", p(class = "note", t("profile_empty")), reading_block(lang, open = TRUE)))
    if (length(selected_types()) == 0) return(div(class = "profile", if (gap_empty()) div(class = "alert alert-warning gap-note", gap()$text) else p(class = "note", t("no_type_selected"))))
    r <- range(); d <- data()
    row <- d$neighbourhoods[d$neighbourhoods$territory_id == id, ]
    dis_id <- parent_of[[id]]
    types <- base[base$neighbourhood_id %in% id & base$block == input$block & base$call_type %in% selected_types() &
                    base$month >= r[1] & base$month <= r[2], ] |>
      group_by(call_type) |> summarise(n = sum(events), .groups = "drop") |> mutate(n = apply_threshold(n, THRESHOLD)) |> as.data.frame()
    types$name <- type_name(types$call_type, lang); types <- order_rows(types, "n", desc = TRUE)
    ratio <- if (HAS_RATES) row$rate / d$city$rate else NA
    div(class = "profile",
      div(class = "profile-head", h4(name_of[[id]]), actionLink("close_profile", HTML("&#10005;"), title = t("close_profile"), class = "text-muted")),
      div(class = "sub", if (!SINGLE_DISTRICT) tagList(t("district_of", name_of[[dis_id]]), " · "), selection_text(), " · ", fmt_range(r, lang)),
      div(class = "kpis",
          kpi(figure(row$n, lang), t("k_calls")),
          if (HAS_RATES) kpi(if (is.na(row$rate)) "—" else fmt_r(row$rate, lang), t("k_rate")),
          if (HAS_RATES) kpi(if (is.na(ratio)) "—" else sprintf(t("times"), fmt_r(ratio, lang)), tagList(t("k_ratio"), info(t("lq_note"), "glossary", lang)))),
      if (HAS_RATES) comparison_block(row$rate, d$city$rate, d$neighbourhoods$rate, t("city"), t("noun_rate", t("k_calls"), t("k_rate")), t("cmp_neighbourhoods"), lang, n_events = row$n),
      { ad <- area_density(geo_nb, id, unname(pop_of[id]), lang); if (!is.null(ad)) div(class = "sub", ad) },
      if (!is.na(ratio) && ratio >= 2) div(class = "alert alert-info", HTML(t("ratio_note", fmt_r(ratio, lang)))),
      if (HAS_RATES && isTRUE(pop_of[id] < 5000)) div(class = "alert alert-secondary", t("few_residents")),
      if (!is.na(row$n) && row$n < MIN_EVENTS_STABLE) div(class = "alert alert-secondary", t("few_events", MIN_EVENTS_STABLE)),
      if (HAS_SURVEY) survey_block(dis_id, lang),
      if (nzchar(NOTE_BREAK$block) && identical(input$block, NOTE_BREAK$block) && r[1] < as.Date(NOTE_BREAK$date) && r[2] >= as.Date(NOTE_BREAK$date))
        div(class = "alert alert-warning", t(NOTE_BREAK$key)),
      if (!is.null(gap())) div(class = "alert alert-warning gap-note", gap()$text),       # also here: the filters are folded while a profile is open
      if (nzchar(NOTE_TERRITORY$pattern) && grepl(NOTE_TERRITORY$pattern, name_of[[id]], ignore.case = TRUE)) div(class = "alert alert-warning", t(NOTE_TERRITORY$key)),
      rates_block(id, dis_id, d, lang),
      near_block(id, d, lang),
      h6(t(if (TIME_UNIT == "year") "arrests_by_year" else "by_month"), info(t("noise_note"))),
      plotOutput("evolution", height = "120px"),
      h6(t("types_in_period")),
      if (nrow(types) == 0) p(class = "note", t("no_calls")) else
        sort_table(id = "types",
          tags$thead(tags$tr(sort_th(tr("sp_col_type", lang), "name", lang, "text"), sort_th(t("col_calls"), "n", lang, order = "descending"))),
          tags$tbody(lapply(seq_len(nrow(types)), function(i)   # the note of a type is a plain element (tip()): a row moves when the table is put in another order, and a bslib tooltip does not survive the move
            tags$tr(sort_td(tagList(types$name[i], if (!is.na(type_note(types$call_type[i], lang))) tip(type_note(types$call_type[i], lang))), types$name[i]),
                    sort_td(figure(types$n[i], lang), types$n[i], "text-end"))))),
      lapply(panel_ids("district"), function(id) panel_block(id, dis_id, r, lang)),
      reading_block(lang), source_block(lang)
    )
  })

  output$evolution <- render_chart({
    req(nzchar(input$neighbourhood), length(selected_types()) > 0)
    r <- range(); months <- MONTHS[MONTHS >= r[1] & MONTHS <= r[2]]
    d <- base[base$neighbourhood_id %in% input$neighbourhood & base$block == input$block & base$call_type %in% selected_types() & base$month %in% months, ] |>
      group_by(month) |> summarise(n = sum(events), .groups = "drop")
    n <- d$n[match(months, d$month)]; n[is.na(n)] <- 0
    low <- n < THRESHOLD; height <- ifelse(low, THRESHOLD, n)
    op <- par(mar = c(1.8, 2.6, 0.3, 0.3), las = 1, cex.axis = 0.75, mgp = c(2, 0.4, 0)); on.exit(par(op))
    b <- barplot(height, names.arg = if (TIME_UNIT == "year") format(months, "%Y") else month_names(lang)[as.integer(format(months, "%m"))], col = ifelse(low, NA, "#64748b"),
                 border = ifelse(low, "#94a3b8", NA), density = ifelse(low, 15, -1), angle = 45, ylim = c(0, max(height, THRESHOLD) * 1.18))
    if (any(low)) text(b[low], height[low], paste0("<", THRESHOLD), pos = 3, cex = 0.65, col = ink("#475569"))
  }, res = 96)

  # --- Statistics: the top districts (5) or neighbourhoods (10) by number or rate, same filters ---
  # What the ranking goes by: the control above it, or the title of the column of numbers or of rates of its table,
  # whichever was touched last (the founder, 18 November 2026; P-068). A click on such a title moves the control too.
  rank_by <- reactiveVal(NULL)
  observeEvent(input$sort, rank_by(input$sort))
  observeEvent(input$rank_by, {
    v <- input$rank_by$value; if (!isTRUE(v %in% c("n", if (HAS_RATES) "rate"))) return()
    rank_by(v); updateRadioButtons(session, "sort", selected = v)
  })
  # A level whose residents no register gives (a register by district only: Mexico City's neighbourhoods) has no
  # ranking by rate. Asked for one, the page showed a title and nothing under it; it goes by number there, and the
  # control says so.
  observe({
    if (HAS_RATES && !HAS_NB_RATES && identical(input$stats_level %||% STATS_LEVEL, "neighbourhood") && identical(rank_by() %||% input$sort, "rate")) {
      rank_by("n"); updateRadioButtons(session, "sort", selected = "n") }
  })
  top5 <- reactive({
    d <- data(); lvl <- input$stats_level %||% STATS_LEVEL
    di <- if (lvl == "neighbourhood") { x <- d$neighbourhoods; x$name <- paste0(unname(name_of[x$territory_id]), " (", unname(name_of[unname(parent_of[x$territory_id])]), ")"); x } else d$districts
    can_rate <- HAS_RATES && any(!is.na(di$rate))                            # rows without a single rate have no ranking by rate to offer: they go by number
    by <- if (identical(rank_by() %||% input$sort, "rate") && can_rate) "rate" else "n"
    di <- order_rows(di[!is.na(di[[by]]), ], by, desc = TRUE)                # the largest first, equal values by name: the order the table says it is in
    di <- di[seq_len(min(if (lvl == "neighbourhood") 10 else 5, nrow(di))), ]
    list(districts = di, city = d$city, by = by, level = lvl, can_rate = can_rate)
  })
  output$stats_title <- renderUI({
    if (length(selected_types()) == 0) return(none_note())
    h5(class = "mt-2 mb-1", tags$b(t(if (identical(input$stats_level %||% STATS_LEVEL, "neighbourhood")) "top_neighbourhoods" else "top5")), " · ", selection_text(), " · ", fmt_range(range(), lang))
  })
  # --- Seasonal profile at city level: average events by calendar month over the complete years, for the chosen types ---
  season <- reactive({
    req(length(selected_types()) > 0)
    if (TIME_UNIT == "year") return(NULL)                     # no calendar months in a yearly source
    d <- base[base$block == input$block & base$call_type %in% selected_types(), ]
    season_profile(d$month, d$events)
  })
  # Time series block (statistics tab): the ticked types (or the one picked on the chart by type) in the chosen area
  ts_native <- reactive({
    if (length(selected_types()) == 0) return(NULL)
    d <- zone_rows(base[base$block == input$block & base$call_type %in% (focus() %||% selected_types()), ])
    s <- d |> group_by(date = month) |> summarise(n = sum(events), .groups = "drop") |> as.data.frame()
    all <- data.frame(date = MONTHS); all$n <- s$n[match(all$date, s$date)]; all$n[is.na(all$n)] <- 0
    all
  })
  ts_what <- reactive(if (is.null(focus())) selection_text() else type_name(focus(), lang))
  ts_view <- ts_server(input, output, session, lang, ts_native, heading = reactive(paste0(zone()$name, " · ", ts_what())),
                       gran_default = TS_GRAN, native_unit = if (TIME_UNIT == "year") "year" else "month", noun = function() t("k_calls"))
  # Under it, the rate of each complete year for the same area and types, over the period the bar leaves in view (the
  # complete years that period touches), in the countries' way: the city is the reference of a district or a neighbourhood
  rate_years <- reactive({ if (length(selected_types()) == 0) return(NULL); z <- zone(); rate_by_year(input$block, focus() %||% selected_types(), z$level, z$id) })
  rate_shown <- reactive({ s <- rate_years(); if (is.null(s)) return(NULL); s[ts_touched(s$start, ts_view(), "year"), , drop = FALSE] })
  has_rate <- reactiveVal(FALSE); observe(has_rate(!is.null(rate_years())))      # the block is built once, not at every change of the selection
  output$rate_block <- renderUI({
    if (!has_rate()) return(NULL)
    div(class = "ts-rate", uiOutput("rate_head"), plotOutput("rate_plot", height = "250px", click = "rate_click"), div(class = "ts-line", uiOutput("rate_note")))
  })
  output$rate_head <- renderUI({
    s <- rate_years(); if (is.null(s)) return(NULL); sh <- rate_shown()
    lacking <- intersect(sh$year, attr(s, "lacking"))                           # years in view for which the source lacks some of these types: said, never drawn as a low rate
    g <- if (length(lacking)) gap_info(input$block, as.Date(c(sprintf("%d-01-01", min(lacking)), sprintf("%d-12-01", max(lacking)))), lang)
    tagList(rate_head_ui(s, sh, zone()$name, CITY_NAME, ts_what(), lang, t("rate_years_title"), t("col_residents")),
            if (!is.null(g)) div(class = "note", g$text))
  })
  rate_bars <- reactiveVal(NULL); rate_clicked <- reactiveVal(NULL)
  output$rate_plot <- render_chart({ b <- tryCatch(ts_plot_rate(rate_shown(), lang), error = ts_blank); rate_bars(b); rate_clicked(NULL) }, res = 96)
  observeEvent(input$rate_click, { b <- rate_bars(); if (is.null(b) || !nrow(b)) return(); rate_clicked(b[which.min(abs(b$x - input$rate_click$x)), ]) })
  output$rate_note <- renderUI(rate_click_ui(rate_clicked(), rate_years(), zone()$name, CITY_NAME, lang, t("col_residents")))   # the bar clicked
  has_season <- reactive(!is.null(tryCatch(season(), error = function(e) NULL)))               # FALSE as well while no type is ticked
  # The figures of the page as an Excel file (P-065): what the charts show, read when the button is pressed. A chart
  # that has nothing to show at that moment has no sheet. The function that writes the file is also left where a test
  # can call it (session$userData), whatever the version of shiny makes of a download in a test.
  xl_file <- function(file) {
    got <- function(expr) tryCatch(expr, error = function(e) NULL)
    xl_write(city_workbook(list(
      zone = zone(), block = input$block, types = selected_types(), all_ticked = all_ticked(), selection = selection_text(), range = range(), focus = focus(),
      bars = got(type_bars()), series = got(attr(ts_view, "shown")()), series_what = paste0(zone()$name, " · ", ts_what()),
      rate = got(rate_shown()), lacking = attr(got(rate_years()), "lacking"), season = got(season()), top = got(top5()), gap = got(gap()),
      cite = list(publisher = meta$publisher, title = meta$source_local %||% meta$source_name, url = meta$url, licence = meta$licence,
                  period = fmt_range(range(), lang), consulted = fmt_date(as.Date(meta$loaded_at), lang), threshold = THRESHOLD),
      made = Sys.time()), lang), file)
  }
  session$userData$xl_stats <- xl_file
  if (HAS_XLSX) output$xl_stats <- downloadHandler(
    filename = function() sprintf("webarchymapp_%s_%s.xlsx", CITY, format(Sys.time(), "%Y%m%d_%H%M")), content = xl_file,
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
  output$has_season <- reactive(has_season())                                                    # tells the page whether the seasonal block exists
  outputOptions(output, "has_season", suspendWhenHidden = FALSE)
  output$season_title <- renderUI({ s <- season(); if (is.null(s)) return(NULL); h5(class = "mt-2 mb-1", tags$b(t("seasonal_title")), " · ", selection_text(), " · ", if (length(s$years) == 1) s$years else paste(min(s$years), "–", max(s$years))) })
  output$season_plot <- render_chart({
    s <- season(); req(!is.null(s))
    op <- par(mar = fit_mar(c(2.6, 4.2, 0.6, 0.4)), las = 1, cex.axis = 0.8, mgp = c(2.8, 0.5, 0)); on.exit(par(op))   # the look of the charts over time
    v <- s$avg$n[match(1:12, s$avg$mo)]; v[is.na(v)] <- 0
    b <- barplot(v, col = "#4e79a7", border = NA, ylab = "", yaxt = "n", ylim = c(0, max(v, 1) * 1.08))
    # The names of the months: all twelve when they fit side by side (a bar and its gap are 1.2 wide along the axis),
    # else one in every two or three from January on; written here, since barplot() drops the ones that would touch
    nm <- month_names(lang); named <- rev(named_bars(12, label_step(1.15 * max(strwidth(nm, cex = 0.8)) / 1.2)))
    axis(1, at = b[named], labels = nm[named], tick = FALSE, gap.axis = 0)
    at <- pretty(c(0, max(v, 1))); axis(2, at = at, labels = fmt_n(at, lang), lwd = 0, lwd.ticks = 1)
    abline(h = mean(v), lty = 2, col = ink("#555555"))
  }, res = 96)
  output$season_notes <- renderUI({ s <- season(); if (is.null(s)) return(NULL)
    v <- s$avg$n[match(1:12, s$avg$mo)]; hi <- which.max(v); lo <- which.min(v)
    p(class = "note", t("seasonal_note", length(s$years), month_names(lang, FALSE)[hi], fmt_r(100 * (v[hi] / mean(v, na.rm = TRUE) - 1), lang), month_names(lang, FALSE)[lo], fmt_r(100 * (1 - v[lo] / mean(v, na.rm = TRUE)), lang)), " ", info(t("seasonal_info"), "glossary", lang))
  })
  output$stats_plot <- render_chart({
    x <- top5(); req(nrow(x$districts) > 0, length(selected_types()) > 0)
    v <- rev(x$districts[[x$by]]); lbl <- rev(x$districts$name)
    ref <- if (x$by == "rate") x$city$rate else NA
    cx <- if (x$level == "neighbourhood") 0.8 else 1
    names <- fit_labels(lbl, 0.45 * par("din")[1], cex = cx)                 # a long name is not cut by the left edge of the chart ("Barking and Dagenham"), and the margin is that of the widest
    op <- par(mar = fit_mar(c(3, (names$width + 0.3) / par("csi"), 0.5, 3)), las = 1, cex.axis = cx); on.exit(par(op))
    b <- barplot(v, names.arg = names$labels, horiz = TRUE, col = "#3182bd", border = NA, xlim = c(0, max(v, ref, na.rm = TRUE) * 1.28))
    text(v, b, if (x$by == "rate") fmt_r(v, lang) else fmt_n(v, lang), pos = 4, cex = 0.85, xpd = TRUE)   # the value never clipped on a narrow screen
    if (!is.na(ref)) { abline(v = ref, lty = 2, col = ink("#475569")); mtext(t("city_average", fmt_r(ref, lang)), side = 1, line = 2, at = ref, cex = 0.8, col = ink("#475569")) }
  }, res = 96)
  # The table of the ranking: the areas of the chart, in its order. Which areas they are (the first by rate or by
  # number) is decided by the control above or by a click on the title of the column of numbers or of rates (and of
  # the ratio to the city, which goes as the rate): such a title, when the ranking does not go by it yet, asks the
  # server for the ranking by it (P-068). Any other click on a title puts the rows on screen in the order of that
  # column (P-067). Drawn again (another period, another level), the table starts in the order of what the ranking
  # goes by: it keeps no order of its own.
  output$stats_table <- renderUI({
    x <- top5(); req(nrow(x$districts) > 0, length(selected_types()) > 0); di <- x$districts
    arrives <- function(key) if (identical(x$by, key)) "descending"
    asks <- function(by) if (!identical(x$by, by) && (by == "n" || isTRUE(x$can_rate))) c("rank_by", by)
    div(class = "rank-wrap", sort_table(class = "table shiny-table table-striped spacing-s", style = "width:auto;",
      tags$thead(tags$tr(sort_th(t("col_rank"), "rank", lang, class = NULL, first = "asc"),
                         sort_th(t(if (x$level == "neighbourhood") "col_neighbourhood" else "col_district"), "name", lang, "text"),
                         sort_th(t("col_calls"), "n", lang, class = NULL, order = arrives("n"), ask = asks("n")),
                         if (HAS_RATES) sort_th(t("col_rate"), "rate", lang, class = NULL, order = arrives("rate"), ask = asks("rate")),
                         if (HAS_RATES) sort_th(t("col_ratio"), "ratio", lang, class = NULL, ask = asks("rate")))),
      tags$tbody(lapply(seq_len(nrow(di)), function(i) { r <- di[i, ]; ratio <- if (HAS_RATES && !is.na(r$rate)) r$rate / x$city$rate else NA
        tags$tr(sort_td(i, i), sort_td(r$name, r$name), sort_td(figure(r$n, lang), r$n),
                if (HAS_RATES) sort_td(if (is.na(r$rate)) "—" else fmt_r(r$rate, lang), r$rate),
                if (HAS_RATES) sort_td(if (is.na(ratio)) "—" else sprintf(t("times"), fmt_r(ratio, lang)), ratio)) }))))
  })
  output$stats_notes <- renderUI({
    req(length(selected_types()) > 0); x <- top5()
    conc <- concentration_text(if (x$level == "neighbourhood") data()$neighbourhoods$n else data()$districts$n, t(if (x$level == "neighbourhood") "cmp_neighbourhoods" else "cmp_districts"), t("k_calls"), lang)
    tagList(
      if (!is.null(conc)) p(class = "note mt-2", conc, " ", info(t("concentration_note"), "glossary", lang)),
      if (HAS_RATES) p(class = "note", t("lq_table_note")),
      p(class = "note mt-2", t("stats_note", fmt_n(x$city$n, lang), if (HAS_RATES) t("stats_total_rate", fmt_r(x$city$rate, lang)) else "")),
      residents_note(data()$pop, CITY_ID, lang, named = TRUE))
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
      n <- n[!names(n) %in% totals]
      label <- function(m) { x <- iname(m); if (is.null(x) || is.na(x)) m else unname(x) }
      rows <- order_rows(data.frame(id = names(n), name = vapply(names(n), label, ""), n = as.numeric(n), stringsAsFactors = FALSE), "n", desc = TRUE)
      prev <- V[V$month < min(sel) & V$month >= min(sel) - 366, ]
      np <- if (nrow(prev) > 0) tapply(prev$value, prev$indicator, sum) * length(sel) / length(unique(prev$month)) else NULL
      row <- function(m, name, v) { share <- if (tot > 0) 100 * v / tot else NA; before <- if (is.null(np) || is.na(np[m])) NA else round(np[[m]])
        tags$tr(class = if (identical(m, sel_ind())) "sel",
          sort_td(tags$a(onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", out("click"), m), name), name),
          sort_td(fmt_n(v, lang), v, "text-end"),
          sort_td(if (is.na(share)) "—" else paste0(fmt_r(share, lang), " %"), share, "text-end text-muted", style = "white-space:nowrap"),
          sort_td(if (is.na(before)) "—" else fmt_n(before, lang), before, "text-end text-muted")) }
      # A click on a row draws the table again (the row chosen is marked): its id lets the browser give back the order the reader chose
      sort_table(id = out("table"),
        tags$thead(tags$tr(sort_th(panel_text(p, "col_key", t("panel_col_indicator"), lang), "name", lang, "text"),
                           sort_th(panel_text(p, "value_key", t("panel_col_value"), lang), "n", lang, order = "descending"), sort_th("%", "share", lang),
                           sort_th(t("arrests_col_before"), "before", lang, extra = info(t("arrests_before_help"))))),
        tags$tbody(Map(row, rows$id, rows$name, rows$n),
                   tags$tr(class = "ref", `data-pin` = "bottom", tags$td(t("panel_total")), tags$td(class = "text-end", fmt_n(tot, lang)), tags$td(), tags$td())))
    })
    output[[out("evol_head")]] <- renderUI(h6(class = "mb-0", t("arrests_by_year"), " · ", iname(sel_ind()) %||% sel_ind()))
    output[[out("evol")]] <- render_chart({
      d <- V[V$indicator == sel_ind(), ]; req(nrow(d) > 0); d <- d[order(d$month), ]
      year <- as.integer(format(d$month, "%Y")); v <- tapply(d$value, year, sum); nm <- tapply(d$month, year, length)
      op <- par(mar = c(2.4, 3.2, 0.5, 0.5), las = 1, cex.axis = 0.8, mgp = c(2, 0.5, 0)); on.exit(par(op))
      fit <- bar_fit(length(v))
      b <- barplot(v, width = fit$width, space = fit$space, xlim = fit$xlim, names.arg = ifelse(nm < 12, paste0(names(v), "*"), names(v)), col = ifelse(nm < 12, "#9ecae1", "#3182bd"), border = NA, ylim = c(0, max(v) * 1.15))
      text(b, v, fmt_n(v, lang), pos = 3, cex = 0.7, col = ink("#475569"))
    }, res = 96)
    output[[out("notes")]] <- renderUI(div(class = "note mt-1", if (nzchar(p$note_key)) p(HTML(t(p$note_key))) else p(panel_measure(p, lang)), if (nzchar(p$note2_key)) p(t(p$note2_key))))
  })
}

# --- Map layers --------------------------------------------------------------------
PALETTE <- c("#deebf7", "#9ecae1", "#6baed6", "#3182bd", "#08519c")   # one hue, no reds; the lightest class still reads against white and the greys
SUPPRESSED_FILL <- "#e5e7eb"                                             # under the threshold: grey fill and a dashed outline (not only a colour)
# A legend entry of its own for the areas without a figure: a dashed swatch like the outline on the map
legend_dashed <- function(label, fill = SUPPRESSED_FILL, escape = TRUE) HTML(paste0("<i style='background:", fill, ";border:2px dashed #475569;box-sizing:border-box;opacity:1'></i> ", if (escape) htmltools::htmlEscape(label) else label))
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
  map <- map |> clearGroup("neighbourhoods") |> clearGroup("districts") |> removeControl("legend") |> removeControl("legend_na")
  lbl <- neighbourhood_labels(d, selection, range, lang)
  k <- match(geo_nb$territory_id, d$neighbourhoods$territory_id)
  mode <- if (identical(shade, "rate") && !HAS_RATES) "n" else shade
  v <- switch(mode, rate = d$neighbourhoods$rate[k], n = d$neighbourhoods$n[k], NULL)
  if (!is.null(v) && sum(!is.na(v)) >= 2) {
    sc <- quintile_scale(v, mode, lang)
    map <- map |>
      addPolygons(data = geo_nb, layerId = ~territory_id, group = "neighbourhoods", color = "#4a4a4a", weight = ifelse(is.na(v), 1.4, 0.8), opacity = 0.8,
                  dashArray = ifelse(is.na(v), "4 3", "1 0"), fillColor = ifelse(is.na(v), SUPPRESSED_FILL, sc$pal(v)), fillOpacity = 0.7, label = lbl, labelOptions = LABEL_OPTIONS,
                  highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)) |>
      addLegend("bottomleft", pal = sc$pal, values = v[!is.na(v)], layerId = "legend",
                title = HTML(paste0(tr(if (mode == "n") "legend_number" else "legend_rate", lang), "<br><small>", htmltools::htmlEscape(selection), "</small>")),
                labFormat = sc$format, opacity = 0.8)
    if (any(is.na(v))) map <- map |> addControl(legend_dashed(tr("legend_na", lang, THRESHOLD), escape = FALSE), position = "bottomleft", layerId = "legend_na", className = "info legend")
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

# --- The statistics page as an Excel file (P-065) -------------------------------------------------------------------
# What the page shows for the area, the types and the period chosen, one sheet per chart, after a first sheet that
# says whose figures they are, how to cite them and what each sheet holds. Only what the page shows, and as it shows
# it: every count comes from the figures the charts draw, after the threshold, so one under it is an empty cell with
# its note beside it (never the count, and never the count before the threshold, which no sheet is given); a period
# the source lacks is empty as well, never a zero. A cell with a figure is a number, so that it can be drawn.
# `x` = what the server reads when the button is pressed: zone, block, types (the ticked ones), all_ticked,
# selection (their words on the page), range, focus (the one type the chart over time is kept to, or NULL), bars (the
# chart by type), series (the bars in view of the chart over time; its unit as the attribute "gran"), series_what,
# rate (the rows in view of the chart by rate), lacking (the years of it the source lacks), season, top (the ranking
# of the page: top5()), gap (gap_info() of the period), cite (the parts of the citation) and made (the moment).
city_workbook <- function(x, lang) {
  t <- function(k, ...) tr(k, lang, ...)
  low <- t("less_than", THRESHOLD); calls <- t("col_calls"); note <- t("xl_note"); per <- t("period")
  why <- function(...) { p <- unlist(list(...)); p <- p[!is.na(p) & nzchar(p)]; if (length(p)) paste(unique(p), collapse = " · ") else "" }
  line <- function(...) paste(unlist(list(...)), collapse = " · ")
  sheet <- function(names, ...) stats::setNames(data.frame(..., stringsAsFactors = FALSE, check.names = FALSE), names)
  period_text <- fmt_range(x$range, lang)
  sheets <- list(); titles <- character()
  add <- function(name, title, df) { sheets[[name]] <<- df; titles[name] <<- title }

  # By type: every ticked type with a figure in the period (the chart draws the first twelve), in the chart's order
  b <- x$bars
  if (!is.null(b) && nrow(b) > 0)
    add(t("types_chart_title"), line(t("types_chart_title"), x$zone$name, x$selection, period_text),
        sheet(c(t("call_types"), calls, note), b$label, as.numeric(b$n), ifelse(is.na(b$n), low, "")))

  # Over time: the bars in view, in the unit the chart is seen in
  s <- x$series
  if (!is.null(s) && nrow(s) > 0) {
    year <- function(d) as.integer(format(d, "%Y")); lack <- gap_years(x$block, x$focus %||% x$types)
    notes <- vapply(seq_len(nrow(s)), function(i) why(
      if (is.na(s$n[i])) t("less_than", MARTS$threshold), if (!isTRUE(s$complete[i])) t("ts_partial_one"),     # the chart over time holds back under the app's own threshold
      if (any(year(s$start[i]):year(s$end[i]) %in% lack)) gap_info(x$block, c(s$start[i], s$end[i]), lang)$text), "")
    add(t("ts_title"), line(t("ts_title"), x$series_what, t(paste0("ts_", attr(s, "gran") %||% "month")), ts_span_text(s$label[1], s$label[nrow(s)])),
        sheet(c(per, t("xl_from"), calls, note), s$label, as.Date(s$start), as.numeric(s$n), notes))
  }

  # The rate of each complete year in view, with the city's beside it for a district or a neighbourhood
  r <- x$rate
  if (!is.null(r) && nrow(r) > 0) {
    notes <- vapply(seq_len(nrow(r)), function(i) why(
      if (r$year[i] %in% x$lacking) gap_info(x$block, as.Date(sprintf(c("%d-01-01", "%d-12-01"), r$year[i])), lang)$text
      else if (is.na(r$rate[i]) && !is.na(r$pop[i])) low,
      if (nzchar(r$mark[i])) t("pop_nearest", years_text(r$pop_wanted[i]), years_text(r$pop_year[i]))), "")
    cols <- list(r$label, round(r$rate, 2)); names <- c(per, t("col_rate"))
    if (any(!is.na(r$ref))) { cols <- c(cols, list(round(r$ref, 2))); names <- c(names, paste0(CITY_NAME, " · ", t("col_rate"))) }
    add(t("col_rate"), line(t("rate_years_title"), x$series_what, ts_span_text(r$label[1], r$label[nrow(r)])),
        do.call(sheet, c(list(c(names, t("col_residents"), note)), cols, list(as.numeric(r$pop), notes))))
  }

  # The time of year: the average of each calendar month over the complete years, whole city. The chart draws a bar;
  # a sheet gives a number, so a month whose average stands on fewer events than the threshold is left empty
  se <- x$season
  if (!is.null(se)) {
    v <- se$avg$n[match(1:12, se$avg$mo)]; held <- is.na(v) | v * length(se$years) < THRESHOLD
    add(t("seasonal_title"), line(t("seasonal_title"), CITY_NAME, x$selection, if (length(se$years) == 1) se$years else paste(min(se$years), "–", max(se$years))),
        sheet(c(per, paste0(calls, " · ", t("xl_avg")), note), month_names(lang, TRUE), ifelse(held, NA_real_, round(v, 1)), ifelse(held, low, "")))
  }

  # The ranking of the page: the districts or the neighbourhoods it lists, with the columns of its table. Only those
  # rows: the page lists none under the threshold, and a sheet with every area of both levels would let the figure of
  # an area that is not shown be worked out by taking the others from the total of its district
  tp <- x$top
  if (!is.null(tp) && has_rows(tp$districts) && length(x$types) > 0) {
    di <- tp$districts; nb <- identical(tp$level, "neighbourhood"); head <- t(if (nb) "top_neighbourhoods" else "top5")
    cols <- list(seq_len(nrow(di)), di$name, as.numeric(di$n)); names <- c(t("col_rank"), t(if (nb) "col_neighbourhood" else "col_district"), calls)
    if (HAS_RATES) { cols <- c(cols, list(round(di$rate, 2), round(di$rate / tp$city$rate, 2))); names <- c(names, t("col_rate"), t("col_ratio")) }
    tab <- if (nchar(head) <= 31) head else upper_first(t(if (nb) "cmp_neighbourhoods" else "cmp_districts"), lang)      # a title too long for a sheet name (Greek): the name of the level
    add(tab, line(head, x$selection, period_text), do.call(sheet, c(list(names), cols)))
  }

  # The first sheet: the choices of the page, whose figures these are and how to cite them, and what each sheet holds.
  # One column of sentences: a spreadsheet opens with narrow columns, and a line of text runs over the empty cells at
  # its right, while a second column would cut the first.
  names(sheets) <- xl_sheet_names(c(t("xl_about"), names(sheets)))[-1]
  chosen <- if (length(x$types) == 0) t("no_type_selected") else if (isTRUE(x$all_ticked)) paste0(t("all"), " (", length(x$types), ")") else paste(vapply(x$types, type_name, "", lang), collapse = "; ")
  said <- function(label, value) paste0(label, ": ", value)
  lines <- c(
    said(t("stats_zone"), if (identical(x$zone$level, "city")) x$zone$name else paste0(x$zone$name, " (", t(paste0("col_", x$zone$level)), ")")),
    said(t("category"), block_name(x$block, lang)),
    said(t("call_types"), chosen),
    if (!is.null(x$focus)) t("types_focus", type_name(x$focus, lang)),
    said(per, period_text),
    t("xl_made", format(x$made, "%Y-%m-%d %H:%M")),                    # the clock of the computer (as.Date() of a moment would give the day in Greenwich)
    "",
    t("what_shows"),
    t("xl_empty", THRESHOLD),
    if (!is.null(x$gap)) x$gap$text,
    "",
    t("cite_intro"),
    cite_lines(x$cite, lang),
    if (length(sheets)) c("", said(names(sheets), unname(titles))))
  c(stats::setNames(list(sheet(line(TR("app_title", lang), CITY_NAME, t("nav_stats")), lines)), t("xl_about")), sheets)
}

# --- Gaps: types the source does not hold for part of the series (mart_city_gaps) -------------------------------------
# What the chosen category lacks in the chosen period: how many of its types (of those the source holds in other
# months), in which years, with the three largest as examples, and the sentence that says so. NULL when the period
# has no gap. Missing is not zero: the sentence stands under the filters of both tabs (P-056).
gap_info <- function(block, range, lang) {
  if (!has_rows(GAPS) || length(block) != 1 || is.na(block)) return(NULL)
  g <- GAPS[GAPS$block == block & GAPS$first_month <= range[2] & GAPS$last_month >= range[1], ]
  size <- tapply(base$events[base$block == block], base$call_type[base$block == block], sum)       # the types the source holds at some time
  g <- g[g$event_type %in% names(size), ]
  if (nrow(g) == 0) return(NULL)
  years <- sort(unique(unlist(lapply(seq_len(nrow(g)), function(i) {
    a <- as.integer(format(max(g$first_month[i], range[1]), "%Y")); b <- as.integer(format(min(g$last_month[i], range[2]), "%Y")); if (a <= b) a:b else integer() }))))
  types <- unique(g$event_type); top <- utils::head(names(sort(size[types], decreasing = TRUE)), 3)
  examples <- paste0(paste(vapply(top, type_name, "", lang), collapse = ", "), if (length(types) > length(top)) "…" else "")
  list(types = types, n = length(types), of = length(size), years = years, reason = g$reason[1],
       text = tr(paste0("gap_", g$reason[1]), lang, years_text(years), fmt_n(length(types), lang), fmt_n(length(size), lang), examples))
}

# --- Profile blocks -----------------------------------------------------------------
# The residents behind the rates of the period, written where the rates are: how many, of which register (its date,
# or "average 2024–2026" when the period spans several), which way they went between the first and the last one, and
# a † with its note when a year of the period has no register of its own and stands on the nearest one.
residents_note <- function(pp, id, lang, class = "note", named = FALSE) {
  if (!HAS_RATES || is.null(pp)) return(NULL)
  n <- unname(pp$pop[id]); if (is.na(n)) return(NULL)
  marked <- pop_marked(pp, id); trend <- register_trend(pp, id, lang)
  who <- if (named) paste0(if (identical(id, CITY_ID)) upper_first(tr("city", lang), lang) else name_of[[id]], ": ")       # whose residents, where the block is not about them alone
  tagList(
    div(class = class, who, paste0(tr("residents", lang, fmt_n(n, lang), register_text(pp, id, lang)), if (marked) " †"), if (!is.null(trend)) paste0(" · ", trend)),
    if (marked) p(class = "note", pop_nearest_text(pp, lang, id)))
}
rates_block <- function(id, dis_id, d, lang) {
  t <- function(k, ...) tr(k, lang, ...)
  if (!HAS_RATES) return(p(class = "note", t("no_population_short")))
  pp <- d$pop
  b <- d$neighbourhoods[d$neighbourhoods$territory_id == id, ]; di <- d$districts[d$districts$territory_id == dis_id, ]
  nb_pop <- if (is.null(pp)) NA else unname(pp$pop[id]); if (is.na(nb_pop)) b$rate <- NA     # no register for this neighbourhood: its row shows no rate
  maxv <- suppressWarnings(max(c(b$rate, di$rate, d$city$rate), na.rm = TRUE))
  bar <- function(lbl, v, strong = FALSE, na_text = NULL) tags$tr(
    tags$td(style = "width:38%", if (strong) tags$b(lbl) else lbl),
    tags$td(style = "width:44%;vertical-align:middle", if (is.na(v)) span(class = "note", na_text %||% t("less_than_calls", THRESHOLD)) else
      div(style = paste0("height:10px;border-radius:3px;background:", if (strong) "#3182bd" else "#94a3b8", ";width:", max(2, round(100 * v / maxv)), "%"))),
    tags$td(class = "text-end", style = "width:18%", if (is.na(v)) "—" else fmt_r(v, lang)))
  # A neighbourhood without a register (a city whose residents are published by district only): the residents shown
  # are those of the smallest area that has them, named, since the rates of the table are that area's and the city's
  has <- function(x) !is.null(pp) && !is.na(unname(pp$pop[x]))
  whose <- if (!is.na(nb_pop)) id else if (!SINGLE_DISTRICT && has(dis_id)) dis_id else if (has(CITY_ID)) CITY_ID else NA
  tagList(
    h6(class = "mb-0", t("rates_title")),
    if (is.na(whose)) div(class = "note mb-1", t("no_population_short")) else residents_note(pp, whose, lang, class = "note mb-1", named = !identical(whose, id)),
    tags$table(class = "table table-sm table-borderless", tags$tbody(
      bar(name_of[[id]], b$rate, TRUE, na_text = if (is.na(nb_pop)) t("no_population_nb") else NULL), if (!SINGLE_DISTRICT) bar(t("district_x", name_of[[dis_id]]), di$rate), bar(t("city"), d$city$rate))))
}

# The neighbourhoods that border one, with the figures of the same types and period as its profile (P-063): the
# neighbourhood among them, from the highest to the lowest. Of the same city only; a name opens that neighbourhood.
near_block <- function(id, d, lang) {
  ids <- bordering(geo_nb, NB_BOXES, id); if (length(ids) == 0) return(NULL)
  rows <- d$neighbourhoods[d$neighbourhoods$territory_id %in% c(id, ids), c("territory_id", "n", "rate")]
  rows$name <- unname(name_of[rows$territory_id])
  near_ui(rows, id, lang, n_col = tr("col_calls", lang), rate_col = if (HAS_NB_RATES) tr("col_rate", lang), n_text = function(n) figure(n, lang), click = "near_pick", name_col = tr("col_neighbourhood", lang))
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
  pp <- period_pop(sel); p_dis <- if (is.null(pp)) NA else unname(pp$pop[dis_id]); p_city <- if (is.null(pp)) NA else unname(pp$pop[CITY_ID])   # the residents of the months shown
  rate <- function(n, p) if (is.na(p) || is.na(n)) NA else 1000 * n / p
  names_of <- panel_names(IND, lang)
  shown <- regular[!is.na(in_dis[regular])]
  # The first `top_n` indicators of the district: by the figures as they are shown (one under the threshold has no
  # value: it comes last, among its like by name), so that neither the rows chosen nor their order say anything
  # about a figure that is not shown. Without `top_n`, the order of the source.
  by_value <- !is.na(p$top_n) && length(shown) > p$top_n
  if (by_value) shown <- order_rows(data.frame(id = shown, name = unname(names_of[shown]), n = apply_threshold(unname(in_dis[shown]), THRESHOLD), stringsAsFactors = FALSE), "n", desc = TRUE)$id[seq_len(p$top_n)]
  if (length(shown) == 0 && length(totals) == 0) return(NULL)
  row <- function(k, strong = FALSE) {
    n <- apply_threshold(in_dis[[k]], THRESHOLD); nc <- in_city[[k]]
    r_dis <- if (is.na(n) || !HAS_RATES) NA else rate(n, p_dis); r_city <- if (!HAS_RATES || is.na(nc)) NA else rate(nc, p_city)
    tags$tr(`data-pin` = if (strong) "bottom",
            sort_td(if (strong) tags$b(names_of[[k]]) else names_of[[k]], names_of[[k]]), sort_td(figure(n, lang), n, "text-end"),
            sort_td(if (is.na(r_dis)) "—" else fmt_r(r_dis, lang), r_dis, "text-end"),
            sort_td(if (is.na(r_city)) "—" else fmt_r(r_city, lang), r_city, "text-end text-muted"))
  }
  tagList(
    h6(ptext("title_key", panel_name(p, lang)), info(ptext("note_key", p$measure))),
    div(class = "sub note", name_of[[dis_id]], " · ", fmt_range(range(sel), lang)),
    if (!is.null(warn)) p(class = "note", warn),
    sort_table(id = paste0("pn_", id),
      tags$thead(tags$tr(sort_th(ptext("col_key", t("panel_col_indicator")), "name", lang, "text"), sort_th(t("mossos_col_district"), "n", lang, order = if (by_value) "descending"),
                         sort_th(paste0(t("mossos_col_rate"), if (pop_marked(pp, dis_id)) " †"), "rate", lang), sort_th(t("mossos_col_city"), "city", lang))),
      tags$tbody(lapply(shown, row), lapply(totals[!is.na(in_dis[totals])], row, strong = TRUE))),
    if (pop_marked(pp, dis_id)) p(class = "note", pop_nearest_text(pp, lang, dis_id)))
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
    if (HAS_RATES) tags$li(t("source_register", if (length(REGISTER_YEARS) > 1) paste0(min(REGISTER_YEARS), "–", max(REGISTER_YEARS)) else fmt_date(meta$population_date, lang))),
    lapply(panel_ids("district"), function(id) { p <- panel_meta(id); m0 <- fmt_month(as.Date(p$first_month), lang); m1 <- fmt_month(as.Date(p$last_month), lang)
      tags$li(if (nzchar(p$source_key) && !identical(TR(p$source_key, "en"), p$source_key)) t(p$source_key, m0, m1) else t("panel_source", panel_name(p, lang), p$publisher, m0, m1),
              " · ", tags$a(href = p$url, target = "_blank", if (nzchar(p$source_label)) p$source_label else sub("^https?://([^/]+).*$", "\\1", p$url))) })))
}

CITY_FUNCTIONS <- c("figure", "type_name", "month_labels", "view_months", "view_label", "views_offered", "series_months", "series_start_note", "full_years", "gap_years", "rate_by_year", "rate_extra_ui", "compute", "zone_choices", "ui_city", "server_city",
                    "neighbourhood_labels", "layer_neighbourhoods", "layer_selection", "rates_block", "near_block", "residents_note", "gap_info", "city_workbook", "range_months", "period_pop", "register_date",
                    "register_text", "register_trend", "pop_marked", "panel_ids", "panel_meta", "panel_values",
                    "panel_indicators", "panel_text", "panel_name", "panel_measure", "panel_names", "panel_block", "reading_block", "source_block", "survey_block", "boundaries_credit")

# The light part of a city (name, id, metadata, panels), for the navigation bar and the About page
city_light <- function(key) {
  f <- CITY_SOURCES[[key]]; if (is.null(f)) return(NULL)
  list(CITY = key, CITY_ID = f$id, CITY_NAME = unname(name_of[f$id]) %||% f$meta$name_en %||% key, META = f$meta, code = f$code,
       PANELS = if (has_rows(CITY_PANELS)) CITY_PANELS[CITY_PANELS$city == key, ] else NULL, GAPS = city_gaps(key))
}
# The types the city's source lacks for some months (mart_city_gaps); NULL when it has none
city_gaps <- function(key) {
  if (!has_rows(CITY_GAPS)) return(NULL)
  g <- CITY_GAPS[CITY_GAPS$city == key, ]; if (nrow(g) == 0) return(NULL)
  g$first_month <- as.Date(g$first_month); g$last_month <- as.Date(g$last_month); g
}

# Prepare the environment of a city (NULL if data or boundaries are missing)
make_city <- function(key) {
  f <- CITY_SOURCES[[key]]; if (is.null(f)) return(NULL)
  x <- list(base = city_events(key), meta = f$meta, types = city_types(key))
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
  e$GAPS <- city_gaps(key)
  types <- x$types %||% data.frame(call_type = character(), name_es = character(), name_en = character())
  e$NAME_EN <- setNames(types$name_en, types$call_type); e$NAME_ES <- setNames(types$name_es, types$call_type)
  geo <- sf::st_read(path(f$gpkg), quiet = TRUE)
  e$geo_nb <- geo[geo$level == "neighbourhood", ]; e$geo_di <- geo[geo$level == "district", ]
  e$geo_nb$district <- unname(name_of[e$geo_nb$parent_id])
  e$SINGLE_DISTRICT <- nrow(e$geo_di) <= 1          # a city whose source has no level between the city and the neighbourhoods (Paris): no district rows
  e$NB_BOXES <- polygon_boxes(e$geo_nb)             # the box of each neighbourhood: which ones border one (bordering(), app_helpers.R)
  if (e$SINGLE_DISTRICT) e$STATS_LEVEL <- "neighbourhood"
  pop <- POP_ALL[POP_ALL$territory_id == f$id | startsWith(POP_ALL$territory_id, paste0(f$id, "-")), ]
  e$HAS_RATES <- nrow(pop) > 0 && f$id %in% pop$territory_id
  e$pop_of <- if (e$HAS_RATES) setNames(pop$population, pop$territory_id) else c()
  e$HAS_NB_RATES <- e$HAS_RATES && any(e$geo_nb$territory_id %in% names(e$pop_of))   # a register by district only (Mexico City): no neighbourhood rates, no shading by rate
  # The registers by year (mart_population_years): a rate stands on the residents of the years of its period, and a
  # year without a register on the nearest one, marked †. The latest register (pop_of) stays for what describes the
  # place today (residents per km², "few residents"), and fills any territory and year the yearly table lacks.
  e$POPY <- city_registers(POP_YEARS, pop, f$id)
  e$REGISTER_YEARS <- sort(unique(e$POPY$year))
  e$BLOCKS <- MARTS$blocks[MARTS$blocks %in% unique(x$base$block)]   # only the categories this city's source publishes
  e$THRESHOLD <- x$meta$threshold[1] %||% MARTS$threshold
  e$MONTHS <- sort(unique(x$base$month))
  e$TS_GRAN <- ts_default_gran(e$TIME_UNIT, length(e$MONTHS))                                                # the unit the chart over time opens with
  e$tr <- function(key, lang, ...) TR(key, lang, ..., city = CITY, kind = MEASURE_KIND)
  environment(e$tr) <- e
  for (fn in CITY_FUNCTIONS) { g <- get(fn, envir = app_env); environment(g) <- e; assign(fn, g, envir = e) }
  e
}
