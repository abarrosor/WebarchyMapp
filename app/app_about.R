# About the data --------------------------------------------------------------------------------------
# Part of the app: sourced by app/app.R (same environment); nothing here runs on its own.

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
  rows <- unlist(lapply(names(cities), function(city) { m <- cities[[city]]$META; cname <- cities[[city]]$CITY_NAME
    c(list(list(key_or(m$about_source_key, paste0(m$source_name, " (", cname, ")")), key_or(m$about_measures_key, m$measure), key_or(m$about_territory_key, cname), months(c(m$first_month, m$last_month)), m$licence, m$url)),
      Filter(Negate(is.null), panel_rows(city, cname))) }), recursive = FALSE)
  mc <- if (length(cities)) cities[[1]]$META else NULL; mb <- if ("barcelona" %in% names(cities)) cities$barcelona$META else NULL
  # One row per country (the first one is DEFAULT_COUNTRY): source, measure and notes, levels, period, licence, population and boundaries
  # One row per country and source (main source first, then its second sources)
  rows <- c(unlist(lapply(names(COUNTRY_LIGHT), function(cc) { e <- COUNTRY_LIGHT[[cc]]; ms <- e$SRC_META[e$SRC_META$source == "main" | e$SRC_META$source %in% e$ALT_SOURCES$source, ]
    lapply(seq_len(nrow(ms)), function(i) { m <- ms[i, ]
      per <- if (!is.null(m$first_period) && !is.na(m$first_period)) { fy <- isTRUE(m$financial_year); yt <- function(y) if (fy) sprintf("%d/%02d", y - 1, y %% 100) else as.character(y); q <- m$last_period %% 10
        sprintf("%s – %s (%s)", yt(m$first_period %/% 10), yt(m$last_period %/% 10), month_names(lang, TRUE)[if (fy) ((q * 3 + 2) %% 12) + 1 else q * 3]) } else ""
      lv <- if (m$source == "main") e$SP_LEVELS else strsplit(m$levels, ";")[[1]]
      lvls <- paste(vapply(lv, function(l) e$LEVEL_LABEL(l, lang), ""), collapse = ", ")
      cname <- e$CNAME(lang)
      list(paste0(m$source_name, " (", cname, ")"), paste0(m$measure, if (nzchar(m$notes_en %||% "")) paste0(" ", m$notes_en) else ""), paste0(cname, ": ", lvls), per,
           paste0(m$licence, if (nzchar(m$population_source %||% "")) paste0(" · ", t("about_pop_from", m$population_source)) else "",
                  if (nzchar(m$boundaries_attribution %||% "")) paste0(" · ", t("boundaries_by", m$boundaries_attribution)) else ""), m$url) }) }), recursive = FALSE), rows)
  rows <- Filter(Negate(is.null), rows)
  # The three tables of the page can be put in the order of any of their columns (P-067): text, ordered by the browser
  titles <- function(keys) tags$thead(tags$tr(lapply(keys, function(k) sort_th(t(k), sub("^about_col_", "", k), lang, "text"))))
  table <- sort_table(id = "about_sources",
    titles(c("about_col_source", "about_col_measures", "about_col_territory", "about_col_period", "about_col_licence")),
    tags$tbody(lapply(rows, function(r) tags$tr(
      tags$td(tags$b(r[[1]]), if (!is.null(link(r[[6]]))) tagList(tags$br(), link(r[[6]]))),
      tags$td(r[[2]]), tags$td(r[[3]]), tags$td(r[[4]]), tags$td(r[[5]])))))
  first_calls <- if (!is.null(mc)) fmt_month(as.Date(mc$first_month), lang) else "—"
  # The years of a city's gaps (mart_city_gaps); Barcelona's paragraph of "why there is not more" is written only while its files are cut
  gap_years <- function(g) sort(unique(unlist(Map(function(a, b) as.integer(format(a, "%Y")):as.integer(format(b, "%Y")), g$first_month, g$last_month))))
  bcn_gaps <- if ("barcelona" %in% names(cities)) cities$barcelona$GAPS else NULL
  # How each source counts: unit, where, date, revisions, known breaks (from the modules' metadata; "—" when the module says nothing)
  dash <- function(x) if (is.null(x) || is.na(x) || !nzchar(x)) "—" else x
  mrows <- c(
    unlist(lapply(names(COUNTRY_LIGHT), function(cc) { e <- COUNTRY_LIGHT[[cc]]; ms <- e$SRC_META[e$SRC_META$source == "main" | e$SRC_META$source %in% e$ALT_SOURCES$source, ]
      lapply(seq_len(nrow(ms)), function(i) { m <- ms[i, ]
        list(paste0(m$source_name, " (", e$CNAME(lang), ")"), e$MEASURE_LABEL("count_noun", lang, m) %||% t("sp_k_n"), dash(m$method_where), dash(m$method_date), dash(m$method_revisions), dash(m$notes_en)) }) }), recursive = FALSE),
    lapply(names(cities), function(city) { m <- cities[[city]]$META; g <- cities[[city]]$GAPS
      # known breaks: the module's own note, and the years whose file the publisher cut short (mart_city_gaps), written from the data
      gap <- if (has_rows(g)) t(paste0("about_gap_", g$reason[1]), years_text(gap_years(g)), fmt_n(length(unique(g$event_type)), lang)) else ""
      notes <- c(if (nzchar(m$note_break_key %||% "")) t(m$note_break_key) else "", gap)
      list(paste0(m$source_name, " (", cities[[city]]$CITY_NAME, ")"), t("k_calls", city = city, kind = m$measure_kind %||% "calls"), dash(m$method_where), dash(m$method_date), dash(m$method_revisions), dash(paste(notes[nzchar(notes)], collapse = " "))) }))
  method_table <- sort_table(id = "about_method", class = "table table-sm small",
    titles(c("about_col_source", "about_col_unit", "about_col_where", "about_col_date", "about_col_revisions", "about_col_breaks")),
    tags$tbody(lapply(mrows, function(r) tags$tr(lapply(r, function(v) tags$td(v))))))
  bslib::page_navbar(
    title = page_title(lang, paste(t("app_title"), "·", t("nav_about"))), window_title = paste(t("app_title"), "·", t("nav_about")), lang = lang, theme = bslib::bs_theme(version = 5), id = "nav",
    header = page_header(lang),
    bslib::nav_panel(t("nav_about"), value = "about", div(class = "about",
      h1(class = "h3", t("nav_about")),
      p(t("about_intro_1")), p(t("about_intro_2")),
      # Four tabs instead of one long scroll (U9): sources (and surveys), how each source counts, glossary, rules
      bslib::navset_underline(id = "about_tabs",
        bslib::nav_panel(t("about_h_sources"), value = "sources",
          h2(id = "about-sources", class = "h5 mt-3", t("about_h_sources")), tags$details(open = NA, tags$summary(t("about_show_table", length(rows))), table),
          if (has_rows(CITY_SURVEY_META)) tagList(h2(id = "about-surveys", t("about_h_surveys")), p(t("about_surveys_intro")), sort_table(id = "about_surveys",
        titles(c("about_col_source", "about_col_measures", "about_col_period", "about_col_licence")),
        tags$tbody(lapply(seq_len(nrow(CITY_SURVEY_META)), function(i) { s <- CITY_SURVEY_META[i, ]; cname <- if (!is.null(cities[[s$city]])) cities[[s$city]]$CITY_NAME else s$city
          tags$tr(tags$td(tags$b(paste0(s$name_en, " (", cname, ")")), if (!is.null(link(s$url))) tagList(tags$br(), link(s$url))), tags$td(s$measure), tags$td(paste(s$first_year, "–", s$last_year)), tags$td(paste(s$licence, "·", s$publisher))) }))))),
        bslib::nav_panel(t("about_h_method"), value = "method",
          h2(id = "about-method", class = "h5 mt-3", t("about_h_method")), p(t("about_method_intro")), tags$details(tags$summary(t("about_show_table", length(mrows))), method_table)),
        bslib::nav_panel(t("about_h_glossary"), value = "glossary",
          h2(id = "about-glossary", class = "h5 mt-3", t("about_h_glossary")), tags$dl(class = "row", lapply(c("recorded", "dark_figure", "rate", "lq", "percentile", "cumulative", "revision", "threshold"), function(g) tagList(tags$dt(class = "col-sm-3", t(paste0("gl_", g, "_t"))), tags$dd(class = "col-sm-9", t(paste0("gl_", g, "_d"))))))),
        bslib::nav_panel(t("about_h_rules"), value = "rules",
          h2(id = "about-rules", class = "h5 mt-3", t("about_h_rules")), tags$ul(lapply(c(t("about_rule_1"), t("about_rule_2"), t("about_rule_3", MARTS$threshold, MARTS$threshold), t("about_rule_4"), t("about_rule_5"), t("about_rule_6")), function(x) tags$li(HTML(x)))),
          h2(id = "about-why", class = "h5 mt-3", t("about_h_why")), tags$ul(lapply(c(t("about_why_1"), t("about_why_2"), if (has_rows(bcn_gaps)) t("about_why_bcn", years_text(gap_years(bcn_gaps))), t("about_why_3"), t("about_why_4"), t("about_why_5"), t("about_why_6"), t("about_why_7", first_calls, if (!is.null(COUNTRY_LIGHT[[DEFAULT_COUNTRY]]) && !is.na(COUNTRY_LIGHT[[DEFAULT_COUNTRY]]$META$first_period %||% NA)) COUNTRY_LIGHT[[DEFAULT_COUNTRY]]$META$first_period %/% 10 else NA), t("about_why_8"), if (length(COUNTRY_LIGHT) > 1) t("about_why_countries")), function(x) tags$li(HTML(x)))),
          h2(id = "about-limits", class = "h5 mt-3", t("about_h_limits")), tags$ul(lapply(c(t("about_limit_1"), t("about_limit_2"), t("about_limit_3"), t("about_limit_4")), function(x) tags$li(x))))),
      # a link with #about-<section> (from a ⓘ, say) opens the tab that holds it
      tags$script(HTML("(function(){ var h = location.hash; if (!h) return; var el = document.querySelector(h); if (!el) return; var pane = el.closest('.tab-pane'); if (!pane) return; var a = document.querySelector('#about_tabs a[data-bs-toggle=\"tab\"][data-value=\"' + pane.getAttribute('data-value') + '\"]') || document.querySelector('#about_tabs a[href=\"#' + pane.id + '\"]'); if (a && window.bootstrap) { bootstrap.Tab.getOrCreateInstance(a).show(); setTimeout(function(){ el.scrollIntoView(); }, 150); } })();")),
      p(class = "upd", t("about_updated", MARTS$threshold)))),
    !!!nav_switcher("about", lang)
  )
}
