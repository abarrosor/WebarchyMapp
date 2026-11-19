# Country view: controls, map, table, profile, series; one environment per country (make_country) -----
# Part of the app: sourced by app/app.R (same environment); nothing here runs on its own.

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
  div(class = "survey mt-2", h5(class = "mb-0", t("survey_title"), info(t("survey_note"), "glossary", lang)), blocks,
      if (!is.null(SURVEY_META)) p(class = "note", SURVEY_META$publisher, if (nzchar(SURVEY_META$url %||% "")) tagList(" · ", tags$a(href = SURVEY_META$url, target = "_blank", t("more_info")))))
}

# Offence types that can be chosen (no sensitive ones) of a type table, its aggregates first
cb_types <- function(lang, tt = CB_TYPE_TABLE, aggregates = AGGREGATES) {
  x <- tt
  if (nrow(x) == 0) return(character())
  x$name <- category_name(x, lang)
  pri <- match(x$offence_type_id, aggregates)
  x <- x[order(is.na(pri), pri, x$name), ]
  setNames(x$offence_type_id, x$name)
}

# Periods: full years (4th quarter) and the latest partial year, for a reference territory of a source's rows
cb_periods <- function(ref_id, lang, d = CB) {
  d <- d[d$territory_id == ref_id & d$offence_type_id == "total" & !is.na(d$quarter), c("year", "quarter")]
  if (nrow(d) == 0) return(character())
  last <- tapply(d$quarter, d$year, max); years <- as.integer(names(last))
  lbl <- ifelse(last == 4, sprintf(TR(if (FY) "sp_year_fy" else "sp_year", lang), YEAR_TEXT(years)),
                sprintf(TR("sp_partial", lang), YEAR_TEXT(years), month_names(lang, TRUE)[Q_START], month_names(lang, TRUE)[Q_END(last)]))
  rev(setNames(paste0(years, "|", last), lbl))
}

ui_country_controls <- function(lang) {
  t <- function(k, ...) TR(k, lang, ...)
  per <- cb_periods(CC, lang); full <- per[grepl("\\|4$", per)]
  tagList(
    if (HAS_MAP) checkboxInput("sp_follow", tagList(t("sp_follow"), info(t("sp_follow_help"))), value = TRUE),
    radioButtons("sp_level", t("sp_level"), choiceNames = lapply(SP_LEVELS, function(l) LEVEL_LABEL(l, lang)), choiceValues = SP_LEVELS, selected = SP_LEVELS[length(SP_LEVELS)]),
    # (its content in one plain div: in a recent shiny the panel has no box of its own, and the div is then the one item
    # the side panel sees, as the panel is in an older one - OUTPUT CONTAINERS in app_styles.R)
    conditionalPanel("input.sp_level == 'municipality' && !input.sp_follow", div(
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
      })),
    div(uiOutput("sp_types_head")),                                 # each output in a plain div: OUTPUT CONTAINERS in app_styles.R
    checkboxGroupInput("sp_types", NULL, choices = character(0)),
    div(uiOutput("sp_types_summary")),
    div(class = "facet", span(t("sp_period"), info(t("sp_period_help")))),
    div(class = "chips",
        actionLink("sp_view_latest", t("view_latest_year", sub("\\|.*$", "", if (length(full)) full[1] else per[1])), class = "active"),
        actionLink("sp_view_five", t("view_five")), actionLink("sp_view_all", t("view_all"))),
    shinyWidgets::sliderTextInput("sp_period", NULL, choices = rev(names(per)), selected = rep(names(if (length(full)) full[1] else per[1]), 2),
                                  grid = FALSE, force_edges = TRUE, width = "100%"),
    div(class = "facet", span(t("sp_territory"))),
    selectizeInput("sp_territory", NULL, choices = NULL, width = "100%"),
    div(uiOutput("sp_territory_info")),
    if (HAS_MAP)
      radioButtons("sp_shade", tagList(t("shade_by"), info(HTML(t("sp_shade_help")), "rules", lang)), inline = TRUE,
                   choices = shade_options(lang, HAS_POPM), selected = shade_default(HAS_POPM)),
    div(class = "foot", uiOutput("sp_foot", inline = TRUE), tags$br(), t("footer"), " · ", cite_link(lang))
  )
}

ui_country <- function(lang) {
  t <- function(k, ...) TR(k, lang, ...)
  # The time series of the page: the chart by number, under it the chart by rate (shown when the country has population
  # figures) and, under both, the period bar they share
  series <- function() ts_ui(lang, TS_GRANS, extra = uiOutput("sp_rate_block"))
  bslib::page_navbar(
    title = page_title(lang, paste(t("app_title"), "·", CNAME(lang))), window_title = paste(t("app_title"), "·", CNAME(lang)), lang = lang, theme = bslib::bs_theme(version = 5), id = "nav",
    header = page_header(lang),
    sidebar = bslib::sidebar(width = 340, open = "desktop", ui_country_controls(lang)),
    bslib::nav_panel(CNAME(lang), value = CC,
      div(class = "sp-title", uiOutput("sp_title")),
      bslib::layout_columns(col_widths = c(5, 7), fill = FALSE, class = "country-grid",
        div(uiOutput("sp_profile"), uiOutput("sp_breakdown")),
        if (!HAS_MAP) div(div(class = "table-sp", uiOutput("sp_table")), series()) else
          bslib::navset_underline(id = "sp_view",
            bslib::nav_panel(t("sp_map"), div(class = "map-wrap", leafletOutput("sp_map", height = "66vh"), div(class = "where-box", uiOutput("sp_where")))),
            bslib::nav_panel(t("sp_table"), div(class = "table-sp", uiOutput("sp_table"))),
            bslib::nav_panel(t("ts_tab"), series()))),
      div(uiOutput("sp_notes"))),
    !!!nav_switcher(CC, lang)
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
    tt <- type_table(); nm <- category_name(tt, lang)
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
    tr <- type_tree(); b <- "total"; bl <- lower_first(t("panel_total"), lang)        # "total" in the page's language
    lv <- block_leaves(); n <- type_counts(); lv <- lv[n[lv] > 0]
    ticked <- if (types_ready()) intersect(input$sp_types %||% character(0), lv) else lv
    if (length(lv) == 0 || setequal(ticked, lv)) list(ids = b, key = b, label = bl, all = TRUE)
    else if (length(ticked) == 0) list(ids = character(0), key = "none", label = t("none"), all = FALSE)
    else if (length(ticked) == 1) list(ids = ticked, key = ticked, label = names(lv)[match(ticked, lv)], all = FALSE)
    else list(ids = ticked, key = "custom", label = paste0(bl, " (", t("n_of_types", length(ticked), length(lv)), ")"), all = FALSE)
  })
  output$sp_types_head <- renderUI({
    if (length(block_leaves()) == 0) return(NULL)
    div(class = "facet", span(t("sp_type"), info(t("sp_block_help"), "rules", lang)), span(actionLink("sp_types_all", t("all")), " · ", actionLink("sp_types_none", t("none"))))
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

  # The population behind a rate (pop_for_year, pop_for_period in app_helpers.R): the figure of the year of the data;
  # a year without one takes the nearest published and is marked †. Several years added up take the average of the
  # figure of each year. A financial year takes the figure of the calendar year it begins in.
  POP_LAG <- if (FY) 1L else 0L
  pop_year <- function(year) if (HAS_POPM) pop_for_year(POPM, year, POP_LAG) else NULL
  period_pop <- function(per) if (HAS_POPM) pop_for_period(POPM, per$periods$year, POP_LAG) else NULL
  POP_MARK <- " †"

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
      # The territory open stays open; one level up, the one it belongs to (as the map does when it is followed); else the most populated
      up <- character(); a <- keep
      for (i in 1:6) { if (is.null(a) || length(a) != 1 || is.na(a) || !nzchar(a)) break; a <- unname(parent_of[a]); if (!is.na(a)) up <- c(up, a) }
      pick <- intersect(c(keep, up), ids)
      sel <- if (length(pick)) pick[1] else best %||% ids[1]; auto_sel(sel)
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
    if (per$n > 1) paste0(YEAR_TEXT(per$first_year), "–", YEAR_TEXT(per$year))
    else if (per$to_month >= 12) YEAR_TEXT(per$year) else paste0(month_names(lang, TRUE)[Q_START], "–", month_names(lang, TRUE)[Q_END(per$quarter)], " ", YEAR_TEXT(per$year))
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
    pp <- period_pop(per)
    d$population <- if (is.null(pp)) NA else unname(pp$pop[d$territory_id])
    d$pop_own <- if (is.null(pp)) "" else { f <- unname(pp$filled[d$territory_id]); ifelse(is.na(f), "", f) }     # the year a territory took when it has no figure in the year of the others
    d$n <- apply_threshold(d$cumulative, MARTS$threshold)
    d$name <- ifelse(d$territory_id == CC, CNAME(lang), ifelse(d$level == "region" & lvl != "region", ref_name(d$territory_id), display_name(d$territory_id, lvl)))
    d$rate <- ifelse(is.na(d$n) | is.na(d$population), NA, 1000 * d$n / d$population / per$n)   # a range: the yearly average, on the average population of its years
    if (lvl == "municipality" && all_munis()) {          # each municipality against its own region
      ref <- d$rate[match(region_of(d$territory_id), d$territory_id)]
      d$ratio <- ifelse(d$level == "municipality", d$rate / ref, NA)
    } else {
      ref <- d$rate[d$territory_id == ref_id()]
      d$ratio <- if (length(ref) == 1 && !is.na(ref)) d$rate / ref else rep(NA_real_, nrow(d))
    }
    list(d = d, level = lvl, ref_id = ref_id(), ref_name = ref_name(ref_id()), per = per, alt = alt(), src = SM(), types = st,
         pop_text = if (is.null(pp)) NULL else pop_years_text(pp$years, lang), pop_exact = is.null(pp) || pp$exact, pop = pp,   # which population years the rates stand on
         x_ref = t("sp_x_country", if (ref_id() == CC) CNAME(lang) else lc(LEVEL_LABEL("region", lang, col = TRUE), lang)),
         vs_ref = t("sp_vs_country", if (ref_id() == CC) CNAME(lang) else lc(LEVEL_LABEL("region", lang, col = TRUE), lang)))
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
      div(class = "sub", x$types$label, " · ", period_text(), if (!is.na(f$population)) paste0(" · ", t("n_noun", fmt_n(f$population, lang), lower_all(t("sp_col_pop"), lang)), " (", if (nzchar(f$pop_own)) f$pop_own else x$pop_text, ")", if (!x$pop_exact || nzchar(f$pop_own)) POP_MARK)),
      div(class = "kpis",
          kpi(if (is.na(f$n)) t("less_than", MARTS$threshold) else fmt_n(f$n, lang), tt("sp_k_n")),
          kpi(if (is.na(f$rate)) "—" else fmt_r(f$rate, lang), t("sp_k_rate")),
          if (id != x$ref_id) kpi(if (is.na(f$ratio)) "—" else sprintf(t("times"), fmt_r(f$ratio, lang)), tagList(x$vs_ref, info(t("lq_note"), "glossary", lang)))),
      if (id != x$ref_id) comparison_block(f$rate, x$d$rate[x$d$territory_id == x$ref_id][1], x$d$rate[x$d$level == x$level], x$ref_name,
                                           t("noun_rate", tt("sp_k_n"), t("sp_k_rate")), lc(LEVEL_LABEL(x$level, lang), lang), lang, n_events = f$n),
      { ad <- area_density(GEO_LEVEL(x$level), id, f$population, lang); if (!is.null(ad)) div(class = "sub", ad) },
      if (!is.na(f$n) && f$n < MIN_EVENTS_STABLE) div(class = "alert alert-secondary small py-1", t("few_events", MIN_EVENTS_STABLE)),
      if ((!x$pop_exact || nzchar(f$pop_own)) && !is.na(f$population)) div(class = "alert alert-secondary small py-1", pop_nearest_text(x$pop, lang, id)),
      if (x$per$n == 1 && x$per$to_month < 12) div(class = "alert alert-secondary small py-1", t("sp_partial_note", month_names(lang, TRUE)[Q_START], month_names(lang, TRUE)[Q_END(x$per$quarter)])),
      if (x$per$n > 1) div(class = "alert alert-secondary small py-1", t("sp_range_note", x$per$n, YEAR_TEXT(x$per$year)), if (x$per$dropped_partial) paste0(" ", t("sp_range_dropped"))),
      near_block(x, id))
  })
  # The territories that border the chosen one, at the level of the map (P-063): the same source, types and period
  # as the profile, of this country only. Their figures are read from the whole level, not from the rows on screen:
  # a municipality on the edge of its region borders municipalities of the next one, which the table does not list.
  # A bordering territory the source gives no figure for (a small municipality) is counted, not listed.
  near_block <- function(x, id) {
    if (identical(id, x$ref_id) || identical(id, CC)) return(NULL)                 # the reference is not a territory of the level; a country without boundaries has no layer, and bordering() then answers nothing
    ids <- bordering(GEO_LEVEL(x$level), BOXES[[x$level]], id); if (length(ids) == 0) return(NULL)
    type <- if (length(x$types$ids) == 0) "__none__" else x$types$ids
    d <- CBS(); d <- d[d$territory_id %in% c(id, ids) & d$level == x$level & d$offence_type_id %in% type, ]; d <- d[in_periods(d, x$per), ]
    rows <- d |> group_by(territory_id) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
    pop <- if (is.null(x$pop)) NA else unname(x$pop$pop[rows$territory_id])
    rows$n <- apply_threshold(rows$cumulative, MARTS$threshold)
    rows$rate <- ifelse(is.na(rows$n) | is.na(pop), NA, 1000 * rows$n / pop / x$per$n)
    rows$name <- vapply(rows$territory_id, function(t) display_name(t, x$level) %||% t, "")
    near_ui(rows, id, lang, n_col = tt("sp_col_n"), rate_col = if (HAS_POPM) t("sp_col_rate"), n_text = function(n) if (is.na(n)) t("less_than", MARTS$threshold) else fmt_n(n, lang),
            click = "near_pick", missing = length(setdiff(ids, rows$territory_id)), name_col = level_name(x$level))
  }
  observeEvent(input$near_pick, { id <- input$near_pick$value; if (!is.null(id) && id %in% CB_ALL$territory_id) updateSelectizeInput(session, "sp_territory", selected = id) })

  # Breakdown of the chosen territory by offence type (same period), with the reference rate alongside
  output$sp_breakdown <- renderUI({
    x <- country_data(); id <- input$sp_territory; req(id); per <- x$per
    d <- CBS(); d <- d[d$territory_id %in% c(id, x$ref_id) & in_periods(d, per) & !d$sensitive, ]
    if (per$n > 1) d <- d |> group_by(territory_id, offence_type_id) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
    if (nrow(d[d$territory_id == id, ]) == 0) return(NULL)
    pp <- period_pop(per); pop <- function(i) if (is.null(pp)) NA else unname(pp$pop[i])
    types <- cb_types(lang, type_table(), aggregates()); aggregates <- aggregates()
    m <- d[d$territory_id == id & !d$offence_type_id %in% aggregates & !is.na(d$cumulative), ]
    # The rows in the order of the figures as they are shown: the largest first, a figure under the threshold last
    # (among its like by name, so that its place says nothing about it), equal figures by name. A click on the title
    # of a column puts them in the order of that column (P-067); the total keeps the foot.
    m$name <- names(types)[match(m$offence_type_id, types)]; m$name[is.na(m$name)] <- m$offence_type_id[is.na(m$name)]
    m$n <- apply_threshold(m$cumulative, MARTS$threshold); m <- order_rows(m, "n", desc = TRUE)
    row <- function(type, strong = FALSE) {
      n_t <- apply_threshold(d$cumulative[d$territory_id == id & d$offence_type_id == type][1], MARTS$threshold)
      n_r <- d$cumulative[d$territory_id == x$ref_id & d$offence_type_id == type][1]
      rate_t <- if (is.na(n_t)) NA else 1000 * n_t / pop(id) / per$n; rate_r <- if (is.na(n_r)) NA else 1000 * n_r / pop(x$ref_id) / per$n
      name <- names(types)[match(type, types)]; if (is.na(name)) name <- type
      tags$tr(class = if (type %in% x$types$ids) "sel" else if (strong) "ref", `data-pin` = if (strong) "bottom",
              sort_td(tags$a(onclick = sprintf("Shiny.setInputValue('sp_type_click', '%s', {priority: 'event'})", type), name), name),
              sort_td(if (is.na(n_t)) t("less_than", MARTS$threshold) else fmt_n(n_t, lang), n_t, "text-end"),
              sort_td(if (is.na(rate_t)) "—" else fmt_r(rate_t, lang), rate_t, "text-end"),
              sort_td(if (is.na(rate_r)) "—" else fmt_r(rate_r, lang), rate_r, "text-end text-muted"))
    }
    # A type shown that is already counted inside another type shown (residential burglary inside burglary) is named in
    # the note with the names on screen, so that the note reads like the table in every language and for every country
    tt_all <- type_table(); shown <- m$offence_type_id
    inside <- tt_all[tt_all$offence_type_id %in% shown & tt_all$included_in %in% setdiff(shown, aggregates), c("offence_type_id", "included_in"), drop = FALSE]
    type_label <- function(id) { n <- names(types)[match(id, types)]; if (is.na(n)) id else n }
    included <- if (nrow(inside) > 0) t(if (nrow(inside) == 1) "sp_breakdown_included" else "sp_breakdown_included_many",
                                        type_label(inside$offence_type_id[1]), type_label(inside$included_in[1]))
    note <- if (x$alt) meta_text(x$src, "breakdown_note", lang, CLANG, default = t("sp_breakdown_note"))
            else paste(c(t("sp_breakdown_note"), included), collapse = if (lang %in% c("zh", "ja")) "" else " ")
    tagList(h6(class = "mt-2 mb-1", t("sp_breakdown"), info(note)),
      div(class = "table-sp", style = "max-height:none", sort_table(id = "breakdown",
        tags$thead(tags$tr(sort_th(t("sp_col_type"), "name", lang, "text"), sort_th(tt("sp_col_n"), "n", lang, order = "descending"), sort_th(t("sp_col_rate"), "rate", lang), sort_th(x$ref_name, "ref", lang))),
        tags$tbody(lapply(m$offence_type_id, row), if (any(d$territory_id == id & d$offence_type_id == "total")) row("total", TRUE)))))
  })

  # Time series (the Series tab): the selected territory (else the country or the covered region) and the chosen types,
  # in two charts over the same period, by number and by rate, with the period bar they share under both
  ts_id <- reactive({ id <- input$sp_territory %||% ""; if (nzchar(id)) id else top_id() })
  ts_name <- function(id) if (id == CC) CNAME(lang) else if (id == top_id()) ref_name(id) else display_name(id, eff_level())
  ts_native <- reactive({
    st <- sel_types(); if (length(st$ids) == 0) return(NULL)
    id <- ts_id()
    d <- CBS(); d <- d[d$territory_id == id & d$offence_type_id %in% st$ids, ]
    if (length(TS_GRANS) > 1) { d <- d[!is.na(d$quarterly), ]; if (nrow(d) == 0) return(NULL)
      s <- d |> group_by(year, quarter) |> summarise(n = sum(quarterly), .groups = "drop") |> as.data.frame()
      m <- if (FY) c(4L, 7L, 10L, 1L)[s$quarter] else (s$quarter - 1L) * 3L + 1L; y <- if (FY) ifelse(s$quarter == 4, s$year, s$year - 1L) else s$year
    } else { d <- d[d$quarter == 4 & !is.na(d$cumulative), ]; if (nrow(d) == 0) return(NULL)
      s <- d |> group_by(year) |> summarise(n = sum(cumulative), .groups = "drop") |> as.data.frame()
      m <- if (FY) 4L else 1L; y <- if (FY) s$year - 1L else s$year }
    data.frame(date = as.Date(sprintf("%d-%02d-01", y, m)), n = s$n)
  })
  ts_view <- ts_server(input, output, session, lang, ts_native, heading = reactive(paste0(ts_name(ts_id()), " · ", sel_types()$label)),
                        gran_default = if (length(TS_GRANS) > 1) "quarter" else "year", native_unit = if (length(TS_GRANS) > 1) "quarter" else "year", fy = FY, year_text = YEAR_TEXT, noun = function() tt("sp_k_n"))
  # The rate of each full year of the same territory and types (the figure of the year, threshold applied, per 1,000 of
  # the population of that year), and the rate of its reference territory for the line. NULL = no rate at all.
  # Each row keeps the inhabitants it stands on and their year: the head of the chart tells how they moved.
  rate_years <- reactive({
    st <- sel_types(); if (length(st$ids) == 0 || !HAS_POPM) return(NULL)
    id <- ts_id(); ref <- if (id == top_id()) id else ref_id(); ids <- unique(c(id, ref))
    d <- CBS(); d <- d[d$territory_id %in% ids & d$offence_type_id %in% st$ids & d$quarter == 4 & !is.na(d$cumulative), ]
    if (!any(d$territory_id == id)) return(NULL)
    d <- d |> group_by(territory_id, year) |> summarise(cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
    py <- lapply(setNames(nm = sort(unique(d$year))), pop_year)                # the population of each year of data, and the year it is from
    d$pop <- unname(mapply(function(y, t) unname(py[[as.character(y)]]$pop[t]), d$year, d$territory_id))
    d$rate <- 1000 * apply_threshold(d$cumulative, MARTS$threshold) / d$pop
    years <- sort(unique(d$year[d$territory_id == id]))
    of <- function(t, col = "rate") { r <- d[d$territory_id == t, ]; r[[col]][match(years, r$year)] }
    own <- vapply(years, function(y) unname(py[[as.character(y)]]$filled[id]), 1L)                # a year this territory took for itself, where the others have a figure
    exact <- vapply(years, function(y) py[[as.character(y)]]$exact, TRUE) & is.na(own)
    s <- data.frame(year = years, start = as.Date(sprintf("%d-%02d-01", if (FY) years - 1L else years, if (FY) 4L else 1L)), label = YEAR_TEXT(years),
                    rate = of(id), ref = if (ref != id) of(ref) else NA_real_, pop = of(id, "pop"), pop_wanted = years - POP_LAG,
                    pop_year = ifelse(is.na(own), vapply(years, function(y) as.integer(py[[as.character(y)]]$year), 1L), own), mark = ifelse(exact, "", "†"), stringsAsFactors = FALSE)   # † = a year without a population figure of its own
    if (all(is.na(s$rate)) && all(is.na(s$ref))) NULL else s
  })
  # The years the period bar leaves in view: the complete years that the period of the chart by number touches (the
  # bar is the same for both charts). 2021 Q3 to 2023 Q2 shows 2021, 2022 and 2023; the last year alone, when it is
  # not complete yet, shows none.
  rate_shown <- reactive({
    s <- rate_years(); if (is.null(s)) return(NULL)
    s[ts_touched(s$start, ts_view(), "year"), , drop = FALSE]
  })
  has_rate <- reactiveVal(FALSE); observe(has_rate(!is.null(rate_years())))      # the block is built once, not at every change of the selection
  output$sp_rate_block <- renderUI({
    if (!has_rate()) return(NULL)
    div(class = "ts-rate", uiOutput("sp_rate_head"), plotOutput("sp_rate", height = "250px", click = "sp_rate_click"), div(class = "ts-line", uiOutput("sp_rate_note")))
  })
  # The words of the chart are those of every chart by rate of the app (rate_head_ui, rate_click_ui: app_helpers.R)
  output$sp_rate_head <- renderUI({
    s <- rate_years(); if (is.null(s)) return(NULL)
    rate_head_ui(s, rate_shown(), ts_name(ts_id()), ref_name(ref_id()), sel_types()$label, lang, t("sp_evolution"), t("sp_col_pop"))
  })
  rate_bars <- reactiveVal(NULL); rate_clicked <- reactiveVal(NULL)
  output$sp_rate <- render_chart({ b <- tryCatch(ts_plot_rate(rate_shown(), lang), error = ts_blank); rate_bars(b); rate_clicked(NULL) }, res = 96)
  observeEvent(input$sp_rate_click, { b <- rate_bars(); if (is.null(b) || !nrow(b)) return(); rate_clicked(b[which.min(abs(b$x - input$sp_rate_click$x)), ]) })
  output$sp_rate_note <- renderUI(rate_click_ui(rate_clicked(), rate_years(), ts_name(ts_id()), ref_name(ref_id()), lang, t("sp_col_pop")))   # the bar clicked

  # The table of territories: from the highest to the lowest ratio against the reference at first; a click on the
  # title of a column orders by that column, and a second click turns the order round
  SORT_COLS <- c("name", "population", "n", "rate", "ratio")
  sort_by <- reactiveVal(list(col = "ratio", desc = TRUE))
  observeEvent(input$sp_sort, { col <- input$sp_sort; cur <- sort_by(); if (!isTRUE(col %in% SORT_COLS)) return()
    sort_by(list(col = col, desc = if (identical(cur$col, col)) !cur$desc else col != "name")) })
  output$sp_table <- renderUI({
    x <- country_data(); d <- x$d; so <- sort_by()
    refs <- d[d$territory_id == CC | (d$level == "region" & x$level != "region" & d$territory_id == x$ref_id), ]
    refs <- refs[order(refs$territory_id != CC), ]
    if (x$level == "region") refs <- refs[refs$territory_id == CC, ]
    main <- d[d$level == x$level & !d$territory_id %in% refs$territory_id, ]
    if (isTRUE(input$sp_follow) && !is.null(visible_ids())) main <- main[main$territory_id %in% visible_ids(), ]
    main <- order_rows(main, so$col, so$desc)
    cell <- function(v) tags$td(class = "text-end", v)
    row <- function(r, cls = NULL, link = FALSE) tags$tr(class = cls,
      tags$td(if (link) tags$a(onclick = sprintf("Shiny.setInputValue('sp_click', '%s', {priority: 'event'})", r$territory_id), r$name) else r$name),
      tags$td(class = "text-end", title = if (nzchar(r$pop_own)) pop_nearest_text(x$pop, lang, r$territory_id), if (is.na(r$population)) "—" else paste0(fmt_n(r$population, lang), if (nzchar(r$pop_own)) POP_MARK)),
      cell(if (is.na(r$n)) t("less_than", MARTS$threshold) else fmt_n(r$n, lang)),
      cell(if (is.na(r$rate)) "—" else fmt_r(r$rate, lang)), cell(if (is.na(r$ratio) || r$territory_id == x$ref_id) "—" else sprintf(t("times"), fmt_r(r$ratio, lang))))
    if (nrow(main) == 0) return(p(class = "note", t("sp_no_data")))
    # The title of a column: a link that orders the table by it, marked when it is the order in use
    title_cell <- function(col, label, extra = NULL, cls = "text-end") tags$th(class = cls, `aria-sort` = if (so$col == col) (if (so$desc) "descending" else "ascending"),
      tags$a(class = "sort", role = "button", tabindex = "0", title = t("sp_sort"), onclick = sprintf("Shiny.setInputValue('sp_sort', '%s', {priority: 'event'})", col),
             onkeydown = "if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); this.click(); }",
             label, if (so$col == col) span(class = "sort-mark", .noWS = "before", HTML(if (so$desc) "&nbsp;&#9660;" else "&nbsp;&#9650;"))), extra)
    conc <- concentration_text(main$n, lc(LEVEL_LABEL(x$level, lang), lang), tt("sp_k_n"), lang)
    tagList(tags$table(class = "table table-sm",
      tags$thead(tags$tr(title_cell("name", level_name(x$level), cls = NULL), title_cell("population", if (is.null(x$pop_text)) t("sp_col_pop") else paste0(t("sp_col_pop"), " (", x$pop_text, ")", if (!x$pop_exact) POP_MARK)), title_cell("n", tt("sp_col_n")),
                         title_cell("rate", t("sp_col_rate")), title_cell("ratio", x$x_ref, info(t("lq_note"), "glossary", lang)))),
      tags$tbody(lapply(seq_len(nrow(refs)), function(i) row(refs[i, ], "ref")),
                 lapply(seq_len(nrow(main)), function(i) row(main[i, ], if (identical(main$territory_id[i], input$sp_territory)) "sel", link = TRUE)))),
      if (!is.null(conc)) p(class = "note mt-1", conc, " ", info(t("concentration_note"), "glossary", lang)))
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
    map <- map |> clearGroup("areas") |> clearGroup("sel_area") |> clearGroup("outline") |> clearGroup("bg") |> removeControl("sp_legend") |> removeControl("sp_legend_nodata") |> removeControl("sp_legend_na")
    if (is.null(g) || nrow(g) == 0) return(map)
    g <- polygons_to_draw(g, d$territory_id)               # an old classification's polygons only where they have figures; the grey ones first (P-057)
    if (nrow(g) == 0) return(map)
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
      map <- map |> addLegend("bottomright", pal = sc$pal, values = v[with_data & !is.na(v)], layerId = "sp_legend",
        title = HTML(paste0(tt(if (shade == "n") "sp_legend_number" else "sp_legend_rate"), "<br><small>", htmltools::htmlEscape(title), "</small>")),
        labFormat = sc$format, opacity = 0.8)
      if (any(with_data & is.na(v))) map <- map |> addControl(legend_dashed(t("less_than", MARTS$threshold), SUPPRESSED), position = "bottomright", layerId = "sp_legend_na", className = "info legend")
    }
    suppressed <- if (is.null(v)) rep(FALSE, nrow(g)) else with_data & is.na(v)
    if (any(!with_data) || (x$level == "municipality" && all_munis()))
      map <- map |> addLegend("bottomright", colors = NO_DATA, labels = if (x$level == "municipality") NOT_PUBLISHED_LEGEND(lang) else t("sp_legend_no_data"), layerId = "sp_legend_nodata", opacity = 0.8)
    map <- map |> addPolygons(data = g, layerId = ~territory_id, group = "areas", color = ifelse(with_data, "#475569", "#94a3b8"),
                              weight = ifelse(suppressed, 1.4, ifelse(with_data, 0.9, 0.5)), dashArray = ifelse(suppressed, "4 3", "1 0"), opacity = 0.9, fillColor = fill, fillOpacity = opac, label = lbl,
                              labelOptions = LABEL_OPTIONS, highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE))
    if (!x$level %in% c("region", "province") && !is.null(GEO_REGION))
      map <- map |> addPolylines(data = if (all_munis()) GEO_REGION else GEO_REGION[GEO_REGION$territory_id == x$ref_id, ],
                                 group = "outline", color = "#1f2933", weight = 2, options = pathOptions(interactive = FALSE))
    if (!is.null(sel) && nzchar(sel) && sel %in% g$territory_id)
      map <- map |> addPolygons(data = g[g$territory_id == sel, ], group = "sel_area", color = "#c2410c", weight = 4, fill = FALSE, dashArray = "6 4", options = pathOptions(interactive = FALSE))
    map
  }

  output$sp_foot <- renderUI(paste(SOURCE_TEXT(lang, SM()), if (HAS_MAP) BOUNDARIES_TEXT_OF(lang, SM()) else ""))
  # Cite this data: the source shown (the country's, or the second source when it is the one on screen), the period on
  # screen and the day the source was last read
  observeEvent(input$cite, {
    m <- SM(); own <- function(what) { x <- m[[paste0(what, "_local")]]; if (is.null(x) || is.na(x) || !nzchar(x)) m[[what]] else x }
    showModal(cite_modal(list(publisher = own("publisher"), title = if (is.null(m$source_local) || is.na(m$source_local) || !nzchar(m$source_local)) m$source_name else m$source_local,
                              url = m$url, licence = m$licence, period = period_text(),
                              consulted = if ("loaded_at" %in% names(m) && !is.na(m$loaded_at)) fmt_date(as.Date(m$loaded_at), lang) else NULL, threshold = MARTS$threshold),
                         lang, paste0(CC, if (src() != "main") paste0("_", src()))))
  })
  # The box over the map that says where the map is: the country and, level by level, the territory the reader is in.
  # The level on the map is marked; the others are links that change the level (the same as the Level control).
  output$sp_where <- renderUI({
    if (!HAS_MAP) return(NULL)
    lvl <- eff_level(); id <- input$sp_territory %||% ""
    chain <- list(); a <- id
    for (i in 1:6) { if (length(a) != 1 || is.na(a) || !nzchar(a)) break; lv <- unname(level_of[a]); if (!is.na(lv)) chain[[lv]] <- a; a <- unname(parent_of[a]) }
    top <- SP_LEVELS[1]
    rows <- list(list(level = t("col_country"), name = CNAME(lang), send = if (!identical(lvl, top)) "sp_level_pick", value = top))
    for (l in SP_LEVELS) {
      cid <- chain[[l]]; here <- identical(l, lvl)
      rows <- c(rows, list(list(level = LEVEL_LABEL(l, lang, col = TRUE), name = if (is.null(cid)) "—" else display_name(cid, l), current = here, off = is.null(cid) && !here,
                                send = if (!here) "sp_level_pick", value = l)))
    }
    if (alt()) rows <- c(rows, list(list(level = level_name(lvl), name = if (nzchar(id)) display_name(id, lvl) else "—", current = TRUE)))
    where_box(rows, t("where_title"))
  })
  observeEvent(input$sp_level_pick, {
    l <- input$sp_level_pick$value
    if (isTRUE(l %in% SP_LEVELS) && !identical(l, input$sp_level)) updateRadioButtons(session, "sp_level", selected = l)
  })
  output$sp_notes <- renderUI({
    x <- country_data()
    note <- if (x$alt) meta_text(x$src, "notes", lang, CLANG, default = t("sp_note")) else t("sp_note")
    div(class = "note mt-2", p(HTML(note)), p(t("sp_sensitive")), if (is.null(x$pop_text)) p(NO_POP_TEXT(lang)) else p(POP_NOTE(lang, x$pop_text, x$src), if (!x$pop_exact) paste0(" ", pop_nearest_text(x$pop, lang))))
  })
}


COUNTRY_FUNCTIONS <- c("cb_types", "cb_periods", "ui_country_controls", "ui_country", "server_country")

# Prepare the environment of a country (NULL if it has no data)
# The light part of a country: its metadata, names and level labels, with no figures or boundaries. Enough for
# the navigation bar and the About page; make_country() builds on it when the country's view is opened.
country_light <- function(cc) {
  app_env <- environment(country_light)
  if (!has_rows(CMETA) || !cc %in% CMETA$country) return(NULL)
  if (!"source" %in% names(CMETA)) CMETA$source <- "main"
  src_meta <- CMETA[CMETA$country == cc, ]; meta <- src_meta[src_meta$source == "main", ][1, ]
  if (nrow(src_meta[src_meta$source == "main", ]) == 0) return(NULL)
  e <- new.env(parent = app_env)
  e$CC <- cc; e$META <- meta; e$SRC_META <- src_meta; e$CLANG <- meta$lang %||% "en"
  # Second sources with figures (a regional police...): each one covers a territory (a region id or the country)
  with_data <- if (has_rows(CSOURCES_WITH_DATA)) CSOURCES_WITH_DATA$source[CSOURCES_WITH_DATA$country == cc] else character()
  e$ALT_SOURCES <- src_meta[src_meta$source != "main" & src_meta$source %in% with_data, ]
  # A country is offered when its main source holds figures AND a national total: until November 2026 a country whose
  # total had been read as empty (Germany, P-074) was offered and its page failed. It is left out and said in the console.
  main_rows <- if (has_rows(CSOURCES_WITH_DATA)) CSOURCES_WITH_DATA[CSOURCES_WITH_DATA$country == cc & CSOURCES_WITH_DATA$source == "main", ] else NULL
  no_total <- has_rows(main_rows) && "n_total" %in% names(main_rows) && !any(main_rows$n_total > 0, na.rm = TRUE)
  if (no_total) message(cc, ": figures loaded but no national total, so the country is not offered in the app. Reload it (menu option 14) and read its messages.")
  e$HAS_DATA <- "main" %in% with_data && !no_total
  e$SP_LEVELS <- intersect(c("region", "province", "municipality"), strsplit(meta$levels %||% "", ";")[[1]])
  # Country name: local name in the local language, a translated name (country_<cc> key) elsewhere, English as the fallback
  e$CNAME <- function(lang) {
    if (lang == e$CLANG && nzchar(meta$name_local %||% "")) return(meta$name_local)
    k <- paste0("country_", cc); tx <- TR(k, lang); if (!identical(tx, k)) tx else meta$name_en %||% cc }
  # What a source counts when it is not recorded offences (complaints, reported cases...): the module's words in English
  # and in the local language; other languages through the kind of measure (msr_<kind>_title / _noun), else English.
  # NULL when the module names nothing (recorded offences, the default texts). what = "count_noun" or "measure_title".
  e$MEASURE_LABEL <- function(what, lang, m = meta) { x <- m[[paste0(what, if (lang == e$CLANG) "_local" else "_en")]]
    if (lang != "en" && lang != e$CLANG) {
      kind <- m$measure_kind %||% ""; k <- paste0("msr_", kind, if (what == "count_noun") "_noun" else "_title")
      tx <- if (nzchar(kind)) TR(k, lang) else k; x <- if (!identical(tx, k)) tx else m[[paste0(what, "_en")]] }
    if (is.null(x) || is.na(x) || !nzchar(x)) NULL else x }
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
    # (a country can have its own word for a level, "lvl_departments@pe": Peru's departments are not France's)
    if (!is.na(x) && lang != "en" && lang != e$CLANG) { k <- paste0("lvl_", gsub("[^a-z]", "", tolower(sub("\\s*\\(.*$", "", x)))); tx <- TR(k, lang, city = tolower(cc)); if (!identical(tx, k)) x <- tx }
    if (is.na(x) || !nzchar(x)) TR(paste0(if (col) "sp_col_" else "sp_level_", level), lang) else unname(x)
  }
  e
}

make_country <- function(cc) {
  app_env <- environment(make_country)
  e <- country_light(cc); if (is.null(e) || !e$HAS_DATA) return(NULL)
  meta <- e$META; src_meta <- e$SRC_META
  crime <- country_crime(cc); pop <- country_population(cc)
  if (!has_rows(crime)) return(NULL)
  crime <- fill_single_provinces(crime)
  e$CB_ALL <- crime; e$CB <- crime[crime$source == "main", ]; e$POPM <- pop; e$HAS_CB <- nrow(e$CB) > 0; e$HAS_POPM <- has_rows(pop)
  if (!e$HAS_CB) return(NULL)
  e$ALT_SOURCES <- src_meta[src_meta$source != "main" & src_meta$source %in% crime$source, ]
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
  # Financial-year sources (England and Wales, Scotland): "2026 Q1" is April-June 2025 of the year 2025/26, so the
  # years read "2025/26" and a partial period runs from April, not January
  e$FY <- isTRUE(meta$financial_year)
  e$TS_GRANS <- if (any(e$CB$quarter != 4, na.rm = TRUE)) c("quarter", "year") else "year"          # a yearly source has no quarters to show
  e$YEAR_TEXT <- function(y) if (e$FY) sprintf("%d/%02d", as.integer(y) - 1L, as.integer(y) %% 100L) else as.character(y)
  e$Q_START <- if (e$FY) 4L else 1L
  e$Q_END <- function(q) if (e$FY) ((as.integer(q) * 3L + 2L) %% 12L) + 1L else as.integer(q) * 3L
  r <- TERRITORY[TERRITORY$level == "region" & TERRITORY$territory_id %in% e$CB$territory_id, ]
  e$SP_REGIONS <- setNames(r$territory_id, pretty_name(r$name))[order(r$name)]
  e$DEFAULT_REGION <- if (nzchar(meta$default_region %||% "") && meta$default_region %in% e$SP_REGIONS) meta$default_region else e$SP_REGIONS[1]
  # Offence types of a source (no sensitive ones) and its aggregates (total first)
  # Name of a category in the page's language: the source's own name in the country's language, the English one elsewhere.
  # A table made only of the names shared by every country (the Eurostat / ICCS ones of app/category_names.csv) comes in
  # every language, also in the country's own when its module has no local names (the countries read from Eurostat only)
  e$category_name <- function(df, lang) {
    if (lang == e$CLANG && !all(is.na(df$name_local) | !nzchar(df$name_local) | df$name_local == df$name_en)) df$name_local
    else tr_categories(df$name_en, lang)
  }
  e$TYPE_TABLE <- function(src = "main") { d <- e$CB_ALL[e$CB_ALL$source == src, ]; unique(d[!d$sensitive, c("offence_type_id", "included_in", "name_local", "name_en")]) }
  e$AGGREGATES_OF <- function(tt) { a <- unique(c("total", tt$included_in[!is.na(tt$included_in)])); a[a %in% tt$offence_type_id] }
  e$CB_TYPE_TABLE <- e$TYPE_TABLE("main"); e$AGGREGATES <- e$AGGREGATES_OF(e$CB_TYPE_TABLE)
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
  mlab <- e$MEASURE_LABEL
  # Texts that depend on what the source counts: the title, the KPI noun, the legends and the count column.
  # m = the metadata row of the source shown (the main one by default; the server passes the active one)
  e$TRS <- function(k, lang, m = meta, ...) {
    if (k == "sp_title") { tk <- m$title_key %||% ""; if (nzchar(tk) && !identical(TR(tk, "en"), tk)) return(TR(tk, lang)) }
    if (!is.null(mlab("count_noun", lang, m))) {
      noun <- mlab("count_noun", lang, m)
      if (k == "sp_k_n") return(noun)
      if (k == "sp_legend_rate") return(upper_first(TR("noun_rate", lang, noun, TR("sp_k_rate", lang)), lang))
      if (k == "sp_legend_number" || k == "sp_col_n") return(upper_first(noun, lang))
      if (k == "sp_title" && !is.null(mlab("measure_title", lang, m))) return(mlab("measure_title", lang, m))
    }
    TR(k, lang, ...)
  }
  e$TR <- function(k, lang, ...) e$TRS(k, lang, meta, ...)
  for (fn in COUNTRY_FUNCTIONS) { g <- get(fn, envir = app_env); environment(g) <- e; assign(fn, g, envir = e) }
  e
}
