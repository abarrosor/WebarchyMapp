# The app on the test database: every view renders, in more than one language ---------------------------------
# app/app.R is sourced once (it reads the marts of the test copy); its ui(req) is called like Shiny does.
use_test_project()

app_env <- local({
  env <- NULL
  function() {
    use_test_project()
    if (!is.null(env)) return(env)
    load_fixtures()
    e <- new.env(parent = globalenv())
    quiet(source("app/app.R", local = e, encoding = "UTF-8"))
    env <<- e; e
  }
})
render <- function(e, view, lang) { ui <- e$ui(list(QUERY_STRING = sprintf("?view=%s&lang=%s", view, lang))); paste(as.character(htmltools::renderTags(ui)$html), collapse = "\n") }

# The elements of a page, each with its parent: a small reader of the HTML that htmltools writes (well formed; scripts,
# styles and comments are taken out first, their text may hold a "<"). One row per opening tag: its name, id and
# classes, the condition it is shown on when it is a conditional panel, and the row of its parent.
html_elements <- function(h) {
  h <- gsub("(?s)<!--.*?-->|<script\\b.*?</script>|<style\\b.*?</style>", "", h, perl = TRUE)
  m <- gregexpr("<(/?)([A-Za-z][A-Za-z0-9-]*)((?:\"[^\"]*\"|'[^']*'|[^'\">])*)>", h, perl = TRUE)[[1]]
  tok <- regmatches(h, list(m))[[1]]
  closing <- startsWith(tok, "</"); name <- tolower(sub("^</?([A-Za-z][A-Za-z0-9-]*).*$", "\\1", tok))
  void <- name %in% c("area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr") | grepl("/>$", tok)
  attr_of <- function(a) { r <- regmatches(tok, regexpr(sprintf("\\s%s=\"[^\"]*\"", a), tok)); out <- rep(NA_character_, length(tok)); out[grepl(sprintf("\\s%s=\"", a), tok)] <- sub(sprintf("^\\s%s=\"([^\"]*)\"$", a), "\\1", r); out }
  el <- data.frame(row = seq_along(tok), tag = name, id = attr_of("id"), class = attr_of("class"), shown_if = attr_of("data-display-if"), parent = NA_integer_, stringsAsFactors = FALSE)
  el$cond <- !is.na(el$shown_if)
  stack <- integer()
  for (i in seq_along(tok)) {
    if (closing[i]) { k <- which(name[stack] == name[i]); if (length(k)) stack <- stack[seq_len(max(k) - 1)]; next }   # an element left open inside it goes with it
    if (length(stack)) el$parent[i] <- stack[length(stack)]
    if (!void[i]) stack <- c(stack, i)
  }
  el[!closing, ]
}
classes_of <- function(x) if (is.na(x)) character() else strsplit(trimws(x), "\\s+")[[1]]

test_that("the app starts on the test database and lists the loaded places only", {
  e <- app_env()
  expect_true(all(c("XT") %in% names(e$COUNTRY_LIGHT)))
  expect_true(all(c("zurich", "toronto", "buenosaires") %in% names(e$CITY_LIGHT)))
  expect_false("madrid" %in% names(e$CITY_LIGHT))                  # no files for it in the test copy
  expect_true("about" %in% e$VIEWS_AVAILABLE)
  expect_equal(e$fallback_view("not-a-view"), e$VIEWS_AVAILABLE[1])
})

test_that("every view renders in two languages and carries the loading notice and its own name", {
  e <- app_env()
  for (view in e$VIEWS_AVAILABLE) for (lang in c("en", "es")) {
    html <- render(e, view, lang)
    expect_true(nchar(html) > 5000, info = paste(view, lang))
    expect_true(grepl("busy-overlay", html, fixed = TRUE), info = paste(view, lang))
    expect_true(grepl(if (lang == "en") "Loading %s" else "Cargando %s", html, fixed = TRUE), info = paste(view, lang))
  }
  expect_true(grepl("Zürich", render(e, "zurich", "de"), fixed = TRUE))
  expect_true(grepl("Toronto", render(e, "toronto", "fr"), fixed = TRUE))
  expect_true(grepl("Testland", render(e, "XT", "en"), fixed = TRUE))
})

test_that("every view renders in every language of the selector, which lists them all with the page's one selected", {
  e <- app_env()
  langs <- names(e$LANGUAGES)
  expect_length(langs, 32)
  expect_setequal(langs, names(e$TRANSLATIONS))
  for (view in e$VIEWS_AVAILABLE) for (lang in langs) {
    html <- tryCatch(render(e, view, lang), error = function(err) paste("ERROR", conditionMessage(err)))
    expect_true(nchar(html) > 5000, info = paste(view, lang, substr(html, 1, 200)))
    expect_equal(lengths(regmatches(html, gregexpr("<option value=\"[a-z]{2}\"", html))) >= 32, TRUE, info = paste(view, lang))
    expect_true(grepl(sprintf("<option value=\"%s\" selected", lang), html, fixed = TRUE), info = paste(view, lang))
  }
  # The names shared by every country come in the page's language (app/category_names.csv); any other name comes back unchanged
  expect_equal(e$tr_category(c("Theft", "Robbery", "A name of one source"), "de"), c("Diebstahl", "Raub", "A name of one source"))
  expect_equal(e$tr_category("Theft", "en"), "Theft")
  expect_equal(e$tr_categories(c("Theft", "Robbery"), "de"), c("Diebstahl", "Raub"))                    # a whole list of shared names
  expect_equal(e$tr_categories(c("Theft", "A name of one source"), "de"), c("Theft", "A name of one source"))   # never half and half
  # Capitals with the Turkish pair of i's; the other languages as usual
  expect_equal(e$upper_first("ilçe", "tr"), "İlçe"); expect_equal(e$lower_first("İller", "tr"), "iller"); expect_equal(e$upper_first("ilçe", "en"), "Ilçe")
  # Number + noun and noun + rate in the order of each language
  expect_equal(e$tr("n_noun", "en", "5", "calls"), "5 calls"); expect_equal(e$tr("n_noun", "lt", "5", "skambučiai"), "skambučiai: 5")
  expect_equal(e$tr("noun_rate", "tr", "çağrı", "1.000 kişi başına"), "1.000 kişi başına çağrı")
})

test_that("a city view uses its own words and a yearly city shows years, not months", {
  e <- app_env()
  z <- render(e, "zurich", "de")
  expect_true(grepl("Stadtkreis", z, fixed = TRUE))
  expect_false(grepl("Monat", z, fixed = TRUE) && !grepl("Jahr", z, fixed = TRUE))
  b <- render(e, "buenosaires", "es")
  expect_true(grepl("Comuna", b, fixed = TRUE) || grepl("comuna", b, fixed = TRUE))
})

test_that("a city page keeps its sidebar inside the map tab and gives the statistics tab a bar with the shared filters", {
  e <- app_env()
  for (city in c("zurich", "buenosaires")) {
    h <- render(e, city, "en")
    at <- function(x) { i <- regexpr(x, h, fixed = TRUE); if (i < 0) NA_integer_ else as.integer(i) }
    map <- at('data-value="map"'); stats <- at('data-value="stats"'); pane_stats <- at('id="stats-page"')
    expect_false(anyNA(c(map, stats, pane_stats)), info = city)
    side <- at('id="side-filters"'); bar <- at('id="stats-bar"'); slot <- at('id="stats-filters"'); search <- at('id="neighbourhood"'); zone <- at('id="stats_zone"')
    expect_true(side < pane_stats && search < pane_stats, info = city)          # the sidebar (search box, filters, profile) comes with the map...
    expect_true(bar > pane_stats && slot > bar && zone > bar, info = city)      # ...the statistics page has its own bar: the area and a place for the filters
    expect_equal(lengths(regmatches(h, gregexpr('id="wm-filters"', h, fixed = TRUE))), 1, info = city)   # one set of filters, moved by the browser
    for (id in c('id="block"', 'id="types"', 'id="period"', 'id="types_count"')) expect_true(at(id) > side && at(id) < pane_stats, info = paste(city, id))
    expect_equal(lengths(regmatches(h, gregexpr("bslib-sidebar-layout", h, fixed = TRUE))) >= 1, TRUE, info = city)
    expect_false(grepl("<bslib-tooltip", substr(h, side, at('id="shade"')), fixed = TRUE), info = city)   # nothing inside the moved block is a custom element (it would break when moved)
  }
})

test_that("the menus of places of the top bar: searchable when long, in the order of the page's language, found by other names, never Bootstrap's active", {
  e <- app_env()
  country <- function(cc, local = "") { force(cc); list(CNAME = function(l) e$TR(paste0("country_", cc), l), META = list(name_local = local)) }
  countries <- list(ES = country("ES", "España"), DE = country("DE", "Deutschland"), AT = country("AT"), FR = country("FR"), GBE = country("GBE"), NL = country("NL"), IT = country("IT"), PT = country("PT"), CH = country("CH"), PE = country("PE"))
  city <- function(id, name, code, en = name) list(CITY_ID = id, CITY_NAME = name, code = code, META = list(name_en = en))
  cities <- list(madrid = city("ES-13-28-28079", "Madrid", "MAD"), london = city("GBE-LONDON", "London (Metropolitan Police area)", "LON", "London"), denhaag = city("NL-PV28-GM0518", "'s-Gravenhage", "DHG", "Den Haag"),
                 zurich = city("CH-ZH-261", "Zürich", "ZRH"), amsterdam = city("NL-PV27-GM0363", "Amsterdam", "AMS"), rotterdam = city("NL-PV28-GM0599", "Rotterdam", "RTM"), paris = city("FR-75056", "Paris", "PAR"),
                 berlin = city("DE-11", "Berlin", "BER"), lima = city("PE-LIMA", "Lima y Callao", "LIM", "Lima and Callao"), smallville = city("ES-X", "Smallville (test area)", "SMV", "Smallville"))
  html <- function(view, lang, ci = cities, co = countries) paste(as.character(htmltools::tagList(e$nav_switcher(view, lang, cities = ci, countries = co))), collapse = "\n")
  attrs <- function(h, a) { m <- regmatches(h, gregexpr(paste0(a, '="[^"]*"'), h))[[1]]; gsub(paste0('^', a, '="|"$'), "", m) }
  h <- html("london", "es")
  expect_equal(lengths(regmatches(h, gregexpr('class="wm-menu-search"', h, fixed = TRUE))), 2)                    # more than 8 places: a box to type in, in both menus
  expect_true(grepl('placeholder="Buscar entre 10 países…"', h, fixed = TRUE)); expect_true(grepl('placeholder="Buscar entre 10 ciudades…"', h, fixed = TRUE))
  names_shown <- attrs(h, "data-name")
  expect_equal(names_shown[1:10], c("Alemania", "Austria", "España", "Francia", "Inglaterra y Gales", "Italia", "Países Bajos", "Perú", "Portugal", "Suiza"))
  expect_equal(names_shown[11:20], c("Ámsterdam", "Berlín", "La Haya", "Lima y Callao", "Londres", "Madrid", "París", "Róterdam", "Smallville", "Zúrich"))   # usual names in Spanish, accents apart in the order; a note in brackets dropped
  expect_false(grepl('dropdown-item[^"]*[" ]active', h))                                                           # Shiny reads the page's tab from the active links of the navbar: a second one would empty input$nav
  expect_equal(lengths(regmatches(h, gregexpr("wm-current", h, fixed = TRUE))), 1)
  expect_true(grepl('class="dropdown-item wm-current" href="?view=london&amp;lang=es" aria-current="page" data-name="Londres"', h, fixed = TRUE))
  keys <- attrs(h, "data-k"); names(keys) <- names_shown
  for (w in c("LON", "London", "Londra", "伦敦", "England and Wales", "Inglaterra y Gales", "Reino Unido", "United Kingdom")) expect_true(grepl(w, keys[["Londres"]], fixed = TRUE), info = w)   # found by any of its names, by its country and by "Reino Unido"
  for (w in c("DE", "Germany", "Deutschland", "Allemagne")) expect_true(grepl(w, keys[["Alemania"]], fixed = TRUE), info = w)
  expect_true(grepl("Holanda", keys[["Países Bajos"]], fixed = TRUE)); expect_true(grepl("The Hague", keys[["La Haya"]], fixed = TRUE)); expect_true(grepl("Holland", keys[["La Haya"]], fixed = TRUE))
  expect_false(grepl("Metropolitan", keys[["Londres"]], fixed = TRUE))
  # German: Österreich sorts with the O; the country of the page is the title of its menu
  de <- attrs(html("AT", "de"), "data-name")[1:10]
  expect_equal(de, c("Deutschland", "England und Wales", "Frankreich", "Italien", "Niederlande", "Österreich", "Peru", "Portugal", "Schweiz", "Spanien"))
  expect_true(grepl(">Österreich</a>", html("AT", "de"), fixed = TRUE) || grepl("dropdown-toggle[^>]*>\\s*Österreich", html("AT", "de")))
  # a short list has no box to type in
  few <- html("madrid", "en", ci = cities[1:3], co = countries[1:3])
  expect_false(grepl("wm-menu-search", few, fixed = TRUE)); expect_equal(lengths(regmatches(few, gregexpr("wm-plain", few, fixed = TRUE))), 2)
  expect_equal(attrs(few, "data-name")[4:6], c("London", "Madrid", "The Hague"))
})

test_that("the About page lists the loaded sources with their licences", {
  e <- app_env()
  a <- render(e, "about", "en")
  for (s in c("Zürich", "Toronto", "Buenos Aires", "Testland", "Open Government Licence")) expect_true(grepl(s, a, fixed = TRUE), info = s)
})

test_that("the city server computes the profile, the statistics table and the types summary for a selected Kreis", {
  e <- app_env()
  v <- e$get_view("zurich")
  nb <- db_query("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE 'CH-ZH-261%' ORDER BY territory_id")$territory_id[6]
  labels <- v$month_labels("en")                                    # a yearly city: the labels are years
  shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(length(labels) - 1, length(labels))], neighbourhood = nb, stats_level = "neighbourhood", shade = "rate")
    session$elapse(400)                                             # the period input is debounced
    types <- db_query("SELECT event_type FROM mart_city_event_types WHERE city = 'zurich' AND block = 'public_safety'")$event_type
    expect_equal(length(types), 4)                                  # the visible public-safety titles of the fixture
    session$setInputs(types = types)                                # what the browser does when the checkboxes are filled
    session$elapse(400)
    prof <- paste(as.character(output$profile$html), collapse = "")
    expect_true(grepl("Kreis 6", prof, fixed = TRUE))
    expect_true(grepl("per 1,000", prof, fixed = TRUE))             # a rate: the Einwohner of the file reached the mart
    expect_true(is.null(output$types_summary$html) || !nzchar(paste(as.character(output$types_summary$html), collapse = "")))   # every type ticked: no summary line
    expect_true(grepl("All (4)", paste(as.character(output$types_count$html), collapse = ""), fixed = TRUE))                     # the text of the types dropdown of the statistics bar
    session$setInputs(types = types[1:2]); session$elapse(400)
    expect_true(grepl("2 of 4 types", paste(as.character(output$types_summary$html), collapse = ""), fixed = TRUE))
    expect_true(grepl("2 of 4 types", paste(as.character(output$types_count$html), collapse = ""), fixed = TRUE))
    expect_true(grepl("Top 10", paste(as.character(output$stats_title$html), collapse = ""), fixed = TRUE))
    expect_true(grepl('<table class="table shiny-table table-striped spacing-s sortable"', paste(as.character(output$stats_table$html), collapse = ""), fixed = TRUE))   # the ranking renders: a table whose titles order it (P-067)
    # the area of the statistics tab follows the neighbourhood of the map; the title of the chart by type names it
    session$setInputs(types = types, stats_zone = nb); session$elapse(400)
    expect_true(grepl("Kreis 6", paste(as.character(output$types_title$html), collapse = ""), fixed = TRUE))
    expect_true(grepl("Kreis 6", paste(as.character(output$ts_title$html), collapse = ""), fixed = TRUE))
    html <- function(x) paste(as.character(x$html), collapse = "")
    expect_false(grepl("Time series:", html(output$types_notes), fixed = TRUE))
    invisible(output$types_plot)                                    # drawing the chart records where its bars are
    session$setInputs(types_click = list(x = 1, y = 4.3)); session$flushReact()                 # the top bar of four (the largest title)
    note <- html(output$types_notes); expect_true(grepl("Time series: .+ only", note))
    picked <- sub(".*Time series: (.+) only.*", "\\1", note)
    expect_true(grepl(picked, html(output$ts_title), fixed = TRUE))                              # the series is now of that type alone
    session$setInputs(types_click = list(x = 1, y = 4.3)); session$flushReact()                 # the same bar again: back to every type
    expect_false(grepl("Time series:", html(output$types_notes), fixed = TRUE))
    session$setInputs(stats_zone = "city"); session$elapse(400)
    expect_true(grepl("Zürich", paste(as.character(output$types_title$html), collapse = ""), fixed = TRUE))
  })
})

test_that("the time series: the bars a chosen range shows, whole periods when the grouping changes, names in the page's language", {
  e <- app_env()
  d <- data.frame(date = seq(as.Date("2023-01-01"), as.Date("2025-06-01"), by = "month"), n = (1:30) * 10)
  m <- e$ts_build(d, "month", "en"); q <- e$ts_build(d, "quarter", "en"); y <- e$ts_build(d, "year", "en")
  expect_equal(c(nrow(m), nrow(q), nrow(y)), c(30, 10, 3))
  expect_equal(c(q$start[2], q$end[2]), as.Date(c("2023-04-01", "2023-06-01")))           # the first and the last month inside a bar
  expect_equal(y$complete, c(TRUE, TRUE, FALSE)); expect_equal(y$n[1], sum((1:12) * 10))
  expect_equal(q$label[1], "2023 Q1"); expect_equal(q$short[1:2], c("23 Q1", "23 Q2"))
  expect_equal(e$ts_build(d, "quarter", "es")$short[1], "23 T1"); expect_equal(e$ts_build(d, "quarter", "nl")$short[1], "23 K1")   # the letter of the language, never a fixed one
  # Financial years (April-March): the quarters and the years keep their "2024/25" names
  fd <- data.frame(date = as.Date(c("2024-04-01", "2024-07-01", "2024-10-01", "2025-01-01", "2025-04-01")), n = 1:5)
  fy_text <- function(y) sprintf("%d/%02d", y - 1L, y %% 100L)
  fq <- e$ts_build(fd, "quarter", "en", native = "quarter", fy = TRUE, year_text = fy_text)
  expect_equal(fq$label, c("2024/25 Q1", "2024/25 Q2", "2024/25 Q3", "2024/25 Q4", "2025/26 Q1")); expect_equal(fq$short[4], "24/25 Q4")
  fyr <- e$ts_build(fd, "year", "en", native = "quarter", fy = TRUE, year_text = fy_text)
  expect_equal(fyr$label, c("2024/25", "2025/26")); expect_equal(fyr$complete, c(TRUE, FALSE)); expect_equal(fyr$n, c(10, 5))
  # The bars of a range: those that start inside it; everything without a range
  expect_equal(e$ts_window(q, NULL), c(1L, 10L))
  r <- c(q$start[2], q$end[4])                                       # quarters 2 to 4 of 2023, as the period bar keeps them
  expect_equal(e$ts_window(q, r), c(2L, 4L))
  expect_equal(e$ts_window(m, r), c(4L, 12L))                        # seen by month: April to December, the whole quarters
  expect_equal(e$ts_window(y, r), c(1L, 1L))                         # no year starts inside it: the year it falls in, never an empty chart
  expect_equal(e$ts_window(y, as.Date(c("2023-11-01", "2025-02-01"))), c(2L, 3L))
  expect_equal(e$ts_window(q, as.Date(c("2030-01-01", "2030-06-01"))), c(10L, 10L))
  expect_equal(e$ts_span_text("2019", "2023"), "2019 – 2023"); expect_equal(e$ts_span_text("2023", "2023"), "2023")
  # The period those bars cover, and the complete years a second chart shows under the same period bar
  expect_null(e$ts_period(q, NULL, "month")); expect_null(e$ts_period(NULL, r, "month"))
  expect_equal(e$ts_period(q, r, "month"), as.Date(c("2023-04-01", "2024-01-01")))               # to the day after December
  expect_equal(e$ts_period(y, r, "month"), as.Date(c("2023-01-01", "2024-01-01")))               # seen by year, the bar in view is the whole of 2023
  yrs <- as.Date(c("2022-01-01", "2023-01-01", "2024-01-01"))
  expect_equal(e$ts_touched(yrs, NULL), c(TRUE, TRUE, TRUE))
  expect_equal(e$ts_touched(yrs, as.Date(c("2023-07-01", "2024-07-01"))), c(FALSE, TRUE, TRUE))  # mid-2023 to mid-2024: both years
  expect_equal(e$ts_touched(yrs, e$ts_period(q, r, "month")), c(FALSE, TRUE, FALSE))             # it ends with December: 2024 is not touched
  expect_equal(e$ts_touched(yrs, as.Date(c("2025-07-01", "2026-07-01"))), c(FALSE, FALSE, FALSE))
  expect_equal(e$ts_touched(as.Date(c("2023-04-01", "2024-04-01")), as.Date(c("2024-01-01", "2024-04-01"))), c(TRUE, FALSE))   # financial years: January to March belongs to the one begun in April
  last <- as.Date(c("2024-06-02", "2025-06-01"))                    # what the "last year" link chooses on this series, which ends in June 2025
  expect_equal(e$ts_period(m, last, "month"), as.Date(c("2024-07-01", "2025-07-01")))
  expect_equal(e$ts_touched(yrs, e$ts_period(m, last, "month")), c(FALSE, FALSE, TRUE))          # the complete year it touches (2024), never an empty chart
  # The period bar: from the first day to the day after the last period; a bar of the chart starts at a share of that span
  expect_equal(e$ts_period_end(as.Date(c("2025-12-01", "2025-10-01", "2025-04-01")), "month"), as.Date(c("2026-01-01", "2025-11-01", "2025-05-01")))
  expect_equal(e$ts_period_end(as.Date("2025-10-01"), "quarter"), as.Date("2026-01-01")); expect_equal(e$ts_period_end(as.Date("2025-04-01"), "year"), as.Date("2026-04-01"))
  expect_equal(e$ts_nav_span(d, "month"), as.Date(c("2023-01-01", "2025-07-01")))
  ed <- e$ts_nav_edges(y, d, "month")
  expect_length(ed, 4); expect_equal(ed[c(1, 4)], c(0, 1)); expect_true(all(diff(ed) > 0))
  expect_equal(ed[2], 365 / as.numeric(as.Date("2025-07-01") - as.Date("2023-01-01")))
  expect_equal(e$ts_nav_edges(m, d, "month")[2], 31 / as.numeric(as.Date("2025-07-01") - as.Date("2023-01-01")))
  # The charts draw (a file device): every bar of the range comes back for the clicks, figures under the threshold as NA
  f <- tempfile(fileext = ".png"); grDevices::png(f, width = 600, height = 250, res = 96)
  on.exit({ if (grDevices::dev.cur() > 1) grDevices::dev.off(); unlink(f) }, add = TRUE)
  b <- e$ts_plot_main(m, NULL, "en", threshold = 10)                 # 30 bars: one name in every three under them
  expect_equal(nrow(b), 30); expect_true(all(diff(b$x) > 0)); expect_equal(b$label, m$label)
  b <- e$ts_plot_main(q, r, "en", threshold = 200)
  expect_equal(b$label, q$label[2:4]); expect_true(is.na(b$n[1])); expect_equal(b$n[2:3], c(240, 330))
  expect_null(e$ts_plot_main(NULL, NULL, "en"))
  s <- data.frame(label = c("2023", "2024", "2025"), rate = c(12.5, NA, 0.4), ref = c(10, 11, 12))
  b <- e$ts_plot_rate(s, "es"); expect_equal(b$rate, s$rate); expect_equal(b$ref, s$ref); expect_equal(b$label, s$label)
  b <- e$ts_plot_rate(transform(s, ref = NA_real_), "es"); expect_true(all(is.na(b$ref)))                 # no reference: bars only
  expect_null(e$ts_plot_rate(s[0, ], "en"))
  expect_silent(e$ts_plot_nav(NULL)); expect_silent(e$ts_plot_nav(fd, "quarter")); expect_silent(e$ts_plot_nav(d, "month"))
  grDevices::dev.off()
  expect_gt(file.size(f), 500)                                       # the last one drawn (the small chart of the period bar) is in the file
})

test_that("a table goes from the highest to the lowest value of a column, rows without a value last and names whatever their accents", {
  e <- app_env()
  t <- data.frame(name = c("Zamora", "Ávila", "Madrid", "Burgos"), ratio = c(NA, 0.5, 1.2, 1.2), n = c(1, 5, NA, 7), stringsAsFactors = FALSE)
  expect_equal(e$order_rows(t)$name, c("Burgos", "Madrid", "Ávila", "Zamora"))                    # equal values by name; no value: last
  expect_equal(e$order_rows(t, "ratio", desc = FALSE)$name, c("Ávila", "Burgos", "Madrid", "Zamora"))
  expect_equal(e$order_rows(t, "n")$name, c("Burgos", "Ávila", "Zamora", "Madrid"))
  expect_equal(e$order_rows(t, "name", desc = FALSE)$name, c("Ávila", "Burgos", "Madrid", "Zamora"))   # Á with the A
  expect_equal(e$order_rows(t, "name", desc = TRUE)$name, c("Zamora", "Madrid", "Burgos", "Ávila"))
  expect_equal(nrow(e$order_rows(t[0, ])), 0)
})

test_that("a country page: the chart by rate under the chart by number and one period bar under both, the table ordered by its column titles", {
  e <- app_env()
  h <- render(e, "XT", "en")
  at <- function(x) { i <- regexpr(x, h, fixed = TRUE); if (i < 0) NA_integer_ else as.integer(i) }
  bar <- at('id="ts_range"'); main <- at('id="ts_main"'); value <- at('id="ts_value"'); rate <- at('id="sp_rate_block"'); notes <- at('id="ts_notes"')
  expect_false(anyNA(c(bar, main, value, rate, notes)))
  expect_true(main < value && value < rate && rate < bar && bar < notes)   # the chart by number and its line, the chart by rate, then the period bar they share and its note
  expect_true(grepl('class="ts-block ts-two"', h, fixed = TRUE))
  for (cls in c("tsr-h tsr-h-from", "tsr-h tsr-h-to", "tsr-win", "tsr-tag tsr-tag-from", "tsr-tag tsr-tag-to")) expect_true(grepl(cls, h, fixed = TRUE), info = cls)
  expect_equal(lengths(regmatches(h, gregexpr('role="slider"', h, fixed = TRUE))), 2)
  expect_false(grepl('id="sp_evol"', h, fixed = TRUE))                      # the chart by rate is no longer beside the profile
  expect_false(grepl('id="sp_sort"', h, fixed = TRUE))                      # nor a control in the sidebar for the order of the table
  expect_false(grepl("data-brush-id", h, fixed = TRUE))
  hc <- render(e, "zurich", "en"); pos <- function(x) as.integer(regexpr(x, hc, fixed = TRUE))
  expect_true(pos('id="ts_main"') > 0 && pos('id="ts_main"') < pos('id="ts_value"') && pos('id="ts_value"') < pos('id="rate_block"') && pos('id="rate_block"') < pos('id="ts_range"') && pos('id="ts_range"') < pos('id="ts_notes"'))   # a city with a register: the same two charts over one bar
  expect_true(grepl('class="ts-block ts-two"', hc, fixed = TRUE))
  one <- paste(as.character(htmltools::renderTags(e$ts_ui("en", "year"))$html), collapse = "\n"); at1 <- function(x) as.integer(regexpr(x, one, fixed = TRUE))
  expect_true(at1('id="ts_main"') > 0 && at1('id="ts_main"') < at1('id="ts_range"') && at1('id="ts_range"') < at1('id="ts_value"') && at1('id="ts_value"') < at1('id="ts_notes"'))   # without a second chart: one chart, the bar right under it
  expect_false(grepl("ts-two", one, fixed = TRUE))
  v <- e$get_view("XT"); per <- names(v$cb_periods("XT", "en", v$CB))
  text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(x$html), collapse = "")))
  first <- function(a, b, x) { i <- regexpr(a, x, fixed = TRUE); j <- regexpr(b, x, fixed = TRUE); i > 0 && j > 0 && i < j }
  shiny::testServer(function(input, output, session) v$server_country(input, output, session, "en"), {
    session$setInputs(sp_level = "region", sp_period = rep(per[1], 2), sp_territory = "XT-R2"); session$flushReact()
    session$setInputs(sp_types = "a"); session$flushReact()               # what the browser does when the boxes are filled: every listed type ticked = the total
    # The table: 64 / 200,000 against 82 / 300,000 is 1.2 times the country, 18 / 100,000 is 0.66 times
    tb <- paste(as.character(output$sp_table$html), collapse = ""); tx <- text(output$sp_table)
    expect_true(grepl("1.2×", tx, fixed = TRUE) && grepl("0.66×", tx, fixed = TRUE))
    expect_true(first("Region One", "Region Two", tx))                     # from the highest ratio at first
    expect_true(grepl('aria-sort="descending"', tb, fixed = TRUE)); expect_equal(lengths(regmatches(tb, gregexpr('class="sort"', tb, fixed = TRUE))), 5)
    expect_true(grepl("sp_sort&#39;, &#39;ratio&#39;", tb, fixed = TRUE)); expect_true(grepl('title="Sort table"', tb, fixed = TRUE))   # a click on the title sends the column
    expect_true(first("Testland 300,000", "Region One", tx))               # the country stays on top, whatever the order
    session$setInputs(sp_sort = "ratio"); session$flushReact()            # the same title again: the other way round
    tx <- text(output$sp_table); expect_true(first("Region Two", "Region One", tx)); expect_true(grepl('aria-sort="ascending"', paste(as.character(output$sp_table$html), collapse = ""), fixed = TRUE))
    session$setInputs(sp_sort = "n"); session$flushReact();    expect_true(first("Region One", "Region Two", text(output$sp_table)))    # another column: its largest first
    session$setInputs(sp_sort = "name"); session$flushReact(); expect_true(first("Region One", "Region Two", text(output$sp_table)))    # names from A to Z
    session$setInputs(sp_sort = "name"); session$flushReact(); expect_true(first("Region Two", "Region One", text(output$sp_table)))
    session$setInputs(sp_sort = "not-a-column"); session$flushReact(); expect_true(first("Region Two", "Region One", text(output$sp_table)))   # anything else changes nothing
    # The series: by number with its period, and by rate (full years) under it, with the reference in the legend
    expect_true(grepl('id="sp_rate"', paste(as.character(output$sp_rate_block$html), collapse = ""), fixed = TRUE))
    expect_true(grepl("Region Two · total · Quarterly · 2025 Q1 – 2025 Q4", text(output$ts_title), fixed = TRUE))
    head <- text(output$sp_rate_head)
    expect_true(grepl("Per 1,000 inhabitants, full years", head, fixed = TRUE)); expect_true(grepl("Region Two · total · 2025", head, fixed = TRUE)); expect_true(grepl("Testland", head, fixed = TRUE))
    session$setInputs(ts_range = list(from = 2L, to = 3L, n = 4L)); session$flushReact()      # the handles of the period bar: quarters 2 and 3
    expect_true(grepl("Quarterly · 2025 Q2 – 2025 Q3", text(output$ts_title), fixed = TRUE))
    expect_true(grepl("Region Two · total · 2025", text(output$sp_rate_head), fixed = TRUE))   # the complete year those two quarters belong to
    session$setInputs(ts_gran = "year"); session$flushReact(); expect_true(grepl("Yearly · 2025", text(output$ts_title), fixed = TRUE))
    session$setInputs(ts_range = list(from = 1L, to = 9L, n = 9L)); session$flushReact()      # a choice for another number of bars (an old message): ignored
    expect_true(grepl("Yearly · 2025", text(output$ts_title), fixed = TRUE))
    invisible(output$sp_rate); invisible(output$ts_main); invisible(output$ts_nav)             # the three plots draw
    session$setInputs(sp_rate_click = list(x = 0.7, y = 0.1)); session$flushReact()
    expect_true(grepl("2025 · Region Two: 0.18 · Testland: 0.27", text(output$sp_rate_note), fixed = TRUE))    # 18 / 100,000 and 82 / 300,000
    session$setInputs(ts_click = list(x = 0.7, y = 0.1)); session$flushReact()
    expect_true(grepl("18 recorded offences", text(output$ts_value), fixed = TRUE))             # the bar clicked: on the line of its own chart
    expect_true(grepl("Drag the two handles", text(output$ts_notes), fixed = TRUE))             # the note of the period bar stays under the bar
    session$setInputs(sp_territory = "XT-R1"); session$flushReact()
    expect_true(grepl("Region One · total · 2025", text(output$sp_rate_head), fixed = TRUE))
  })
})

test_that("a rate stands on the population of its own year; a year without a figure borrows the nearest one and says so", {
  e <- app_env()
  pop <- data.frame(territory_id = rep(c("A", "B"), each = 3), year = rep(2021:2023, 2), population = c(1000, 1100, 1200, 500, 500, NA))
  p <- e$pop_for_year(pop, 2022)
  expect_equal(p$year, 2022L); expect_true(p$exact); expect_equal(unname(p$pop["A"]), 1100)
  p <- e$pop_for_year(pop, 2025); expect_equal(p$year, 2023L); expect_false(p$exact)             # figures newer than the latest population: the latest, marked
  p <- e$pop_for_year(pop, 2019); expect_equal(p$year, 2021L); expect_false(p$exact)             # older than the first one: the first, marked
  p <- e$pop_for_year(pop, 2023, lag = 1L); expect_equal(p$year, 2022L); expect_true(p$exact)    # the financial year 2022/23 stands on the people of 2022
  p <- e$pop_for_year(pop[pop$year != 2022, ], 2022); expect_equal(p$year, 2021L); expect_false(p$exact)   # a gap: the nearest earlier year
  # A territory without a figure in a year the others have takes its own nearest year, an earlier one first, and is listed
  one <- rbind(pop[pop$territory_id == "A", ], data.frame(territory_id = "C", year = c(2019L, 2024L), population = c(70, 90)))
  p <- e$pop_for_year(one, 2022)
  expect_equal(p$year, 2022L); expect_true(p$exact); expect_equal(unname(p$pop["C"]), 70); expect_equal(p$filled, c(C = 2019L))
  p <- e$pop_for_year(one, 2018)                                                                  # before every figure: the first year there is (C's 2019); A takes its own first one
  expect_equal(p$year, 2019L); expect_false(p$exact); expect_equal(unname(p$pop["C"]), 70); expect_equal(p$filled, c(A = 2021L)); expect_equal(unname(p$pop["A"]), 1000)
  expect_equal(e$pop_for_year(one, 2026)$filled, c(A = 2023L))                                    # after every figure: C has 2024, A its last one
  expect_length(e$pop_for_year(pop[pop$territory_id == "A", ], 2022)$filled, 0)
  pp <- e$pop_for_period(one, 2021:2023)
  expect_equal(unname(pp$filled["C"]), "2019"); expect_true(is.na(pp$filled["A"])); expect_true(pp$exact)                              # the years of the others are their own
  # Several years added up: the average of the figure of each year, so that their sum over it is the yearly average rate
  pp <- e$pop_for_period(pop, 2021:2023)
  expect_equal(unname(pp$pop["A"]), 1100); expect_equal(unname(pp$pop["B"]), 500)                 # B has no figure in 2023: the average of those it has
  expect_equal(pp$years, 2021:2023); expect_true(pp$exact); expect_length(pp$borrowed, 0)
  expect_equal(1000 * (100 + 110 + 120) / unname(pp$pop["A"]) / 3, 100)                           # 100 per 1,000 each year: 100 for the three years together
  pp <- e$pop_for_period(pop, 2023:2025)
  expect_equal(pp$years, 2023L); expect_false(pp$exact); expect_equal(pp$borrowed, 2023L); expect_equal(unname(pp$pop["A"]), 1200)
  pp <- e$pop_for_period(pop, 2019:2022)
  expect_equal(pp$years, 2021:2022); expect_equal(pp$borrowed, 2021L); expect_false(pp$exact)
  expect_equal(unname(pp$pop["A"]), 1025)                                                         # 2019 and 2020 on the people of 2021: (3 x 1000 + 1100) / 4
  # The note behind the †: the years without a figure and the year that stands in, not "the period" as a whole
  expect_equal(pp$missing, 2019:2020)
  expect_equal(e$pop_nearest_text(pp, "en"), "† No population figure for 2019, 2020: the rate uses the nearest published one (2021).")
  expect_equal(e$pop_nearest_text(e$pop_for_period(pop, 2023:2026), "es"), "† Sin cifra de población de 2024–2026: la tasa usa la más cercana publicada (2023).")
  expect_equal(e$years_text(c(2021, 2019, 2020, 2024, 2024)), "2019–2021, 2024"); expect_equal(e$years_text(integer(0)), ""); expect_equal(e$years_text(c(2025, 2026)), "2025, 2026")
  expect_equal(e$pop_years_text(2025L, "en"), "2025"); expect_equal(e$pop_years_text(2021:2025, "en"), "average 2021–2025")
  expect_equal(e$pop_years_text(2021:2025, "es"), "media 2021–2025"); expect_equal(e$pop_years_text(2021:2025, "ja"), "2021–2025 平均")
  # Which way the inhabitants went, and by how much, in the way each language writes a percentage
  expect_equal(e$pop_change_text(1000, 1069, "en"), "▲ +6.9%"); expect_equal(e$pop_change_text(1000, 988, "es"), "▼ −1,2\u00a0%")                # a no-break space: the sign never starts a line
  expect_equal(e$pop_change_text(200, 201, "tr"), "▲ +%0,50"); expect_equal(e$pop_change_text(1000, 1000.2, "en"), "=")
  expect_null(e$pop_change_text(NA, 5, "en")); expect_null(e$pop_change_text(0, 5, "en"))
  expect_equal(e$pop_trend_text(3280782, 2021L, 3506730, 2025L, "es"), "3.280.782 (2021) → 3.506.730 (2025) ▲ +6,9\u00a0%")
  expect_equal(e$pop_trend_text(NA, NA, 3506730, 2025L, "en"), "3,506,730 (2025)")
  expect_equal(e$pop_trend_text(3506730, 2025L, 3506730, 2025L, "en"), "3,506,730 (2025)")       # both ends on the same figure: one figure, no arrow
  expect_null(e$pop_trend_text(1000, 2021L, NA, 2022L, "en"))
})

test_that("the country page names the year of its inhabitants and marks the rates that borrow the population of another year", {
  e <- app_env(); v <- e$get_view("XT"); per <- names(v$cb_periods("XT", "en", v$CB))
  html <- function(x) paste(as.character(x$html), collapse = ""); text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", html(x)))
  run <- function(expr) shiny::testServer(function(input, output, session) v$server_country(input, output, session, "en"), {
    session$setInputs(sp_level = "region", sp_period = rep(per[1], 2), sp_territory = "XT-R2"); session$flushReact()
    session$setInputs(sp_types = "a"); session$flushReact()
    eval(expr)
  })
  # The figures and the population are both of 2025: the year is written, nothing is marked
  run(quote({
    expect_true(grepl("100,000 inhabitants (2025)", text(output$sp_profile), fixed = TRUE)); expect_false(grepl("†", html(output$sp_profile), fixed = TRUE))
    expect_true(grepl("Inhabitants (2025)", text(output$sp_table), fixed = TRUE)); expect_false(grepl("†", html(output$sp_table), fixed = TRUE))
    expect_true(grepl(", 2025.", text(output$sp_notes), fixed = TRUE)); expect_false(grepl("†", html(output$sp_notes), fixed = TRUE))
    head <- text(output$sp_rate_head)
    expect_true(grepl("Inhabitants (Region Two): 100,000 (2025)", head, fixed = TRUE)); expect_false(grepl("†", head, fixed = TRUE))
    invisible(output$sp_rate); session$setInputs(sp_rate_click = list(x = 0.7, y = 0.1)); session$flushReact()
    expect_true(grepl("Inhabitants: 100,000 (2025)", text(output$sp_rate_note), fixed = TRUE))
  }))
  # The latest population is of the year before the figures: it is used, and every place that shows a rate says so
  keep <- v$POPM; on.exit(v$POPM <- keep, add = TRUE)
  v$POPM$year <- 2024L
  run(quote({
    prof <- text(output$sp_profile)
    expect_true(grepl("100,000 inhabitants (2024) †", prof, fixed = TRUE))
    expect_true(grepl("† No population figure for 2025: the rate uses the nearest published one (2024).", prof, fixed = TRUE))
    expect_true(grepl("0.18", prof, fixed = TRUE))                                               # the rate is still there, on the people of 2024
    expect_true(grepl("Inhabitants (2024) †", text(output$sp_table), fixed = TRUE))
    expect_true(grepl(", 2024. † No population figure for 2025: the rate uses the nearest published one (2024).", text(output$sp_notes), fixed = TRUE))
    head <- text(output$sp_rate_head)
    expect_true(grepl("Inhabitants (Region Two): 100,000 (2024)", head, fixed = TRUE)); expect_true(grepl("nearest published one (2024)", head, fixed = TRUE))
  }))
  # Two population years for the one year of figures change nothing: the year of the figures is the one taken
  v$POPM <- rbind(keep, transform(keep, year = 2024L, population = population / 2))
  run(quote({
    expect_true(grepl("100,000 inhabitants (2025)", text(output$sp_profile), fixed = TRUE)); expect_false(grepl("†", html(output$sp_profile), fixed = TRUE))
  }))
  # One region without a figure in the year of the others: it takes its own latest one, and only its row and its profile are marked
  v$POPM <- rbind(keep[keep$territory_id != "XT-R2", ], transform(keep[keep$territory_id == "XT-R2", ], year = 2023L))
  run(quote({
    prof <- text(output$sp_profile)
    expect_true(grepl("100,000 inhabitants (2023) †", prof, fixed = TRUE)); expect_true(grepl("No population figure for 2025: the rate uses the nearest published one (2023)", prof, fixed = TRUE))
    tb <- text(output$sp_table)
    expect_true(grepl("Inhabitants (2025)", tb, fixed = TRUE)); expect_false(grepl("Inhabitants (2025) †", tb, fixed = TRUE))          # the column is of 2025...
    expect_true(grepl("100,000 †", tb, fixed = TRUE)); expect_false(grepl("200,000 †", tb, fixed = TRUE))                              # ...and the one row on another year says so
    expect_true(grepl("Inhabitants (Region Two): 100,000 (2023)", text(output$sp_rate_head), fixed = TRUE))
    expect_true(grepl("nearest published one (2023)", text(output$sp_rate_head), fixed = TRUE))
  }))
})

test_that("a city's rates stand on the residents of the years of the period, name that register, say which way it went and mark a borrowed year", {
  e <- app_env(); v <- e$get_view("zurich")
  expect_equal(v$REGISTER_YEARS, 2024:2025)
  nb <- db_query("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE 'CH-ZH-261%' ORDER BY territory_id")$territory_id[6]
  y24 <- as.Date(c("2024-01-01", "2024-01-01")); y25 <- as.Date(c("2025-01-01", "2025-01-01")); both <- as.Date(c("2024-01-01", "2025-01-01"))
  d24 <- v$compute("public_safety", y24); d25 <- v$compute("public_safety", y25); d <- v$compute("public_safety", both)
  expect_equal(unname(d24$pop$pop[nb]), 26000); expect_equal(unname(d25$pop$pop[nb]), 26500); expect_equal(unname(d$pop$pop[nb]), 26250)   # each year its own Einwohner; both years: their average
  expect_equal(unname(d24$pop$pop[v$CITY_ID]), 318000); expect_equal(unname(d25$pop$pop[v$CITY_ID]), 324000)
  expect_true(d24$pop$exact); expect_true(d$pop$exact); expect_equal(d$pop$years, 2024:2025); expect_equal(d24$pop$years, 2024L)
  row <- function(x) x$neighbourhoods[x$neighbourhoods$territory_id == nb, ]
  expect_equal(row(d24)$rate, 1000 * row(d24)$n / 26000); expect_equal(row(d25)$rate, 1000 * row(d25)$n / 26500); expect_equal(row(d)$rate, 1000 * row(d)$n / 26250)
  expect_equal(d24$city$rate, 1000 * d24$city$n / 318000); expect_equal(d25$city$rate, 1000 * d25$city$n / 324000)
  labels <- v$month_labels("en")                                    # a yearly city: "2024", "2025"
  types <- db_query("SELECT event_type FROM mart_city_event_types WHERE city = 'zurich' AND block = 'public_safety'")$event_type
  html <- function(x) paste(as.character(x$html), collapse = "")
  shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(1, 1)], neighbourhood = nb, stats_level = "neighbourhood", shade = "rate"); session$elapse(400)
    session$setInputs(types = types); session$elapse(400)
    prof <- html(output$profile)                                    # one year: its register, with its date, in view (not behind a ⓘ)
    expect_true(grepl("26,000 residents (register 2024-01-01)", prof, fixed = TRUE)); expect_false(grepl("†", prof, fixed = TRUE)); expect_false(grepl("(2024) →", prof, fixed = TRUE))
    notes <- html(output$stats_notes)
    expect_true(grepl("318,000 residents (register 2024-01-01)", notes, fixed = TRUE)); expect_false(grepl("†", notes, fixed = TRUE))
    session$setInputs(period = labels[c(1, 2)]); session$elapse(400)
    prof <- html(output$profile)                                    # two years: the average, and which way the residents went
    expect_true(grepl("26,250 residents (register average 2024–2025)", prof, fixed = TRUE)); expect_false(grepl("†", prof, fixed = TRUE))
    expect_true(grepl("26,000 (2024) → 26,500 (2025) ▲ +1.9%", prof, fixed = TRUE))
    expect_true(grepl("318,000 (2024) → 324,000 (2025) ▲ +1.9%", html(output$stats_notes), fixed = TRUE))
  })
  # A city with the latest register only (2025): the figures of 2024 stand on it, and say so
  keep <- v$POPY; on.exit(v$POPY <- keep, add = TRUE)
  v$POPY <- keep[keep$year == 2025, ]
  b <- v$compute("public_safety", y24)
  expect_false(b$pop$exact); expect_equal(b$pop$borrowed, 2025L); expect_equal(unname(b$pop$pop[nb]), 26500); expect_equal(row(b)$rate, 1000 * row(b)$n / 26500)
  shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(1, 1)], neighbourhood = nb, stats_level = "neighbourhood", shade = "rate"); session$elapse(400)
    session$setInputs(types = types); session$elapse(400)
    prof <- html(output$profile)
    expect_true(grepl("26,500 residents (register 2025-01-01) †", prof, fixed = TRUE))
    expect_true(grepl("† No population figure for 2024: the rate uses the nearest published one (2025).", prof, fixed = TRUE))
    expect_true(grepl("† No population figure for 2024: the rate uses the nearest published one (2025).", html(output$stats_notes), fixed = TRUE))
    session$setInputs(period = labels[c(2, 2)]); session$elapse(400)                               # the year of the register: no mark
    expect_false(grepl("†", html(output$profile), fixed = TRUE)); expect_false(grepl("†", html(output$stats_notes), fixed = TRUE))
  })
  # One Kreis without its register of 2024: it alone takes its nearest one and is marked; the city is not
  v$POPY <- keep[!(keep$territory_id == nb & keep$year == 2024), ]
  f <- v$compute("public_safety", y24)
  expect_true(f$pop$exact); expect_equal(unname(f$pop$filled[nb]), "2025"); expect_equal(unname(f$pop$pop[nb]), 26500)
  expect_true(v$pop_marked(f$pop, nb)); expect_false(v$pop_marked(f$pop, v$CITY_ID))
  expect_equal(e$pop_nearest_text(f$pop, "en", nb), "† No population figure for 2024: the rate uses the nearest published one (2025).")
})

test_that("what a source lacks is said under the filters and in the profile, never shown as zero, and a category left empty does not break the page", {
  e <- app_env(); v <- e$get_view("zurich")
  types <- db_query("SELECT event_type FROM mart_city_event_types WHERE city = 'zurich' AND block = 'public_safety' ORDER BY 1")$event_type
  nb <- db_query("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE 'CH-ZH-261%' ORDER BY territory_id")$territory_id[6]
  y24 <- as.Date(c("2024-01-01", "2024-01-01")); y25 <- as.Date(c("2025-01-01", "2025-01-01")); both <- as.Date(c("2024-01-01", "2025-01-01"))
  expect_null(v$gap_info("public_safety", both, "en"))                                     # a source with no gap: nothing is said
  old <- list(GAPS = v$GAPS, base = v$base, BLOCKS = v$BLOCKS)
  on.exit({ v$GAPS <- old$GAPS; v$base <- old$base; v$BLOCKS <- old$BLOCKS }, add = TRUE)
  gap <- function(ty) data.frame(city = "zurich", block = "public_safety", event_type = ty, first_month = as.Date("2025-01-01"), last_month = as.Date("2025-12-01"), reason = "cut_file", stringsAsFactors = FALSE)
  v$GAPS <- gap(c(types[1:2], "a type the source never held"))
  g <- v$gap_info("public_safety", both, "en")
  expect_equal(g$n, 2); expect_equal(g$of, 4); expect_equal(g$years, 2025L); expect_equal(sort(g$types), sort(types[1:2]))
  expect_true(startsWith(g$text, "2025: the file published by the source is cut short. Types of this category missing from it: 2 of 4 ("))
  expect_true(endsWith(g$text, "They are missing, not zero: do not compare this period with others."))
  expect_true(startsWith(v$gap_info("public_safety", y25, "es")$text, "2025: el fichero que publica la fuente está cortado. Tipos de esta categoría que faltan en él: 2 de 4 ("))
  expect_null(v$gap_info("public_safety", y24, "en")); expect_null(v$gap_info("coexistence", both, "en"))      # a year or a category the gap does not touch
  for (l in e$LANGS %||% c("en", "es", "de", "ja")) expect_false(grepl("%s", v$gap_info("public_safety", both, l)$text, fixed = TRUE))
  # the whole category missing in 2025, as in Barcelona: no figure, the sentence instead of "no type selected", and no error from an empty list of types
  v$base <- old$base[!(old$base$block == "public_safety" & format(old$base$month, "%Y") == "2025"), ]; v$GAPS <- gap(types)
  labels <- v$month_labels("en"); html <- function(x) paste(as.character(x$html), collapse = "")
  shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(2, 2)], neighbourhood = nb, stats_level = "neighbourhood", shade = "rate"); session$elapse(400)
    expect_true(grepl("Types of this category missing from it: 4 of 4", html(output$gap_note), fixed = TRUE))
    prof <- html(output$profile)
    expect_true(grepl("They are missing, not zero", prof, fixed = TRUE)); expect_false(grepl("No call type selected", prof, fixed = TRUE))
    session$setInputs(period = labels[c(1, 2)]); session$elapse(400); session$setInputs(types = types); session$elapse(400)
    prof <- html(output$profile)                                    # both years: the figures of 2024, with the sentence in the profile as well
    expect_true(grepl("gap-note", prof, fixed = TRUE)); expect_true(grepl("2025: the file published by the source is cut short", prof, fixed = TRUE))
    session$setInputs(period = labels[c(1, 1)]); session$elapse(400)
    expect_false(grepl("cut short", html(output$profile), fixed = TRUE)); expect_equal(html(output$gap_note), "")      # 2024 alone: whole
  })
  # the page opens on the first category that has events in its opening period
  b <- v$base; b$block[b$call_type %in% types[3:4]] <- "coexistence"; b <- rbind(b, transform(old$base[old$base$call_type %in% types[3:4] & format(old$base$month, "%Y") == "2025", ], block = "coexistence"))
  v$base <- b; v$BLOCKS <- c("public_safety", "coexistence")
  ui <- paste(as.character(v$ui_city("en")), collapse = "")
  expect_true(grepl('value="coexistence" checked="checked"', ui, fixed = TRUE)); expect_false(grepl('value="public_safety" checked="checked"', ui, fixed = TRUE))
  v$base <- old$base; v$BLOCKS <- old$BLOCKS
  expect_true(grepl('value="public_safety" checked="checked"', paste(as.character(v$ui_city("en")), collapse = ""), fixed = TRUE))
  # the About page writes the cut years from the same table
  light <- e$CITY_LIGHT; light$zurich$GAPS <- gap(types)
  about <- paste(as.character(e$ui_about("en", light)), collapse = "")
  expect_true(grepl("2025: files cut short by the publisher. Types missing in those years: 4 (missing, not zero).", about, fixed = TRUE))
  expect_false(grepl("files cut short", paste(as.character(e$ui_about("en", e$CITY_LIGHT)), collapse = ""), fixed = TRUE))
})

test_that("the registers of a city: the yearly table, the latest register where that table has nothing, and the latest alone in an older database", {
  e <- app_env()
  years <- data.frame(territory_id = c("X", "X-D1", "X", "Y"), year = c(2024L, 2024L, 2025L, 2025L), population = c(100, 100, 110, 5),
                      ref_date = as.Date(c("2024-01-01", "2024-01-01", "2025-01-01", "2025-01-01")))
  latest <- data.frame(level = "x", territory_id = c("X", "X-D1", "Y"), population = c(111, 111, 6), ref_date = as.Date("2025-06-01"))
  r <- e$city_registers(years, latest, "X")
  expect_equal(nrow(r), 4); expect_false("Y" %in% r$territory_id)                                 # the rows of the city and of what hangs from it, no other city
  expect_equal(r$population[r$territory_id == "X" & r$year == 2025], 110)                         # the register of the year (its first), not the latest of that year
  expect_equal(r$population[r$territory_id == "X-D1" & r$year == 2025], 111)                      # the yearly table has no district in 2025: the latest register stands in
  r <- e$city_registers(NULL, latest, "X"); expect_equal(nrow(r), 2); expect_equal(unique(r$year), 2025L)   # no yearly table: one year, the latest register
  expect_equal(nrow(e$city_registers(NULL, NULL, "X")), 0); expect_equal(nrow(e$city_registers(years, NULL, "Z")), 0)
  # The months of a period weigh its years: three months of 2021 and nine of 2022
  pop <- data.frame(territory_id = "A", year = 2021:2022, population = c(1000, 1100))
  expect_equal(unname(e$pop_for_period(pop, c(rep(2021L, 3), rep(2022L, 9)))$pop["A"]), 1075)
  expect_equal(unname(e$pop_for_period(pop, 2021:2022)$pop["A"]), 1050)
})

# --- What floats over a map, and the citation of a source (P-058) -------------------------------------------------
test_that("a city page: the box that says where the map is and the card for the profile sit over the map; the profile has its place in the side panel", {
  e <- app_env(); h <- render(e, "zurich", "en")
  for (x in c('class="map-wrap', 'id="where"', 'id="profile-card"', 'id="profile-card-body"', 'id="profile-dock"', 'id="profile-dock-btn"', 'id="profile-float-btn"', 'role="dialog"'))
    expect_true(grepl(x, h, fixed = TRUE), info = x)
  count <- function(x, text = h) lengths(regmatches(text, gregexpr(x, text, fixed = TRUE)))
  expect_equal(count('id="profile"'), 1)                                   # one profile, moved by the browser between its two places
  expect_true(regexpr('id="profile-dock"', h, fixed = TRUE) < regexpr('id="profile"', h, fixed = TRUE))   # built inside the side panel
  expect_true(grepl("Show in the side panel", h, fixed = TRUE)); expect_true(grepl("Show over the map", h, fixed = TRUE))
  expect_equal(count('data-wm-send="cite"'), 2)                            # Cite this data: in the foot of the side panel and of the statistics page
  for (x in c("wm-profile-place", "profile_moved", "map_narrow", "data-wm-copy", "data-wm-send")) expect_true(grepl(x, e$JS, fixed = TRUE), info = x)   # the script that goes with them
  hs <- render(e, "zurich", "es")
  expect_true(grepl("Ver en el panel lateral", hs, fixed = TRUE)); expect_true(grepl("Ver sobre el mapa", hs, fixed = TRUE)); expect_true(grepl("Citar estos datos", hs, fixed = TRUE))
  hx <- render(e, "XT", "en"); expect_equal(count('data-wm-send="cite"', hx), 1); expect_false(grepl('id="profile-card"', hx, fixed = TRUE))   # a country: the link, no card
})

test_that("the box over a city's map names the city, then the district and the neighbourhood chosen; the city is the way back", {
  e <- app_env(); html <- function(x) paste(as.character(x$html), collapse = "")
  rows_of <- function(w) regmatches(w, gregexpr('class="where-row( [^"]*)?"', w))[[1]]
  v <- e$get_view("buenosaires")                                           # a city with districts (comunas)
  nb <- db_query(paste0("SELECT n.territory_id, n.name, d.name AS district FROM dim_territory n JOIN dim_territory d ON d.territory_id = n.parent_id WHERE n.level = 'neighbourhood' AND n.territory_id LIKE '", CITY_MODULES$buenosaires$id, "-%' ORDER BY 1"))[1, ]
  shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
    session$setInputs(neighbourhood = ""); session$flushReact()
    w <- html(output$where)
    expect_true(grepl("Where you are", w, fixed = TRUE)); expect_true(grepl("Buenos Aires", w, fixed = TRUE))
    expect_equal(rows_of(w), c('class="where-row current"', 'class="where-row off"', 'class="where-row off"'))
    expect_false(grepl("data-wm-send", w, fixed = TRUE))                   # nothing to go back from
    expect_true(grepl("Comuna", w, fixed = TRUE)); expect_true(grepl("Barrio", w, fixed = TRUE))   # the city's own words for its levels
    session$setInputs(neighbourhood = nb$territory_id, profile_moved = 1); session$flushReact()
    w <- html(output$where)
    expect_equal(rows_of(w), c('class="where-row"', 'class="where-row"', 'class="where-row current"'))
    expect_true(grepl('data-wm-send="where_pick"', w, fixed = TRUE))       # the city row is now a link back to the whole city
    expect_true(grepl(nb$name, w, fixed = TRUE)); expect_true(grepl(nb$district, w, fixed = TRUE))
    expect_true(grepl('aria-current="true"', w, fixed = TRUE))
  })
  z <- e$get_view("zurich")                                                # a city without districts: two levels
  shiny::testServer(function(input, output, session) z$server_city(input, output, session, "de"), {
    session$setInputs(neighbourhood = ""); session$flushReact()
    w <- html(output$where)
    expect_equal(rows_of(w), c('class="where-row current"', 'class="where-row off"'))
    expect_true(grepl("Wo Sie sind", w, fixed = TRUE)); expect_true(grepl("Stadtkreis", w, fixed = TRUE))
  })
})

test_that("the box of a map: the level shown is marked, a level to go to is a link that carries its value, a level not reached yet is dimmed", {
  e <- app_env()
  w <- paste(as.character(e$where_box(list(list(level = "Country", name = "Spain", send = "sp_level_pick", value = "region"),
                                           list(level = "Region", name = "Cataluña", send = "sp_level_pick", value = "region"),
                                           list(level = "Province", name = "Barcelona", current = TRUE),
                                           list(level = "Municipality", name = "—", off = TRUE, send = "sp_level_pick", value = "municipality")), "Where you are")), collapse = "")
  expect_equal(lengths(regmatches(w, gregexpr("<a ", w, fixed = TRUE))), 3)
  expect_true(grepl('<div class="where-row current" aria-current="true">', w, fixed = TRUE))       # the level on the map is not a link
  expect_true(grepl('class="where-row off" href="#" data-wm-send="sp_level_pick" data-wm-value="municipality"', w, fixed = TRUE))
  expect_true(grepl('role="navigation" aria-label="Where you are"', w, fixed = TRUE))
})

test_that("a polygon of a classification that no longer exists is drawn only where it has figures, and never over one that has", {
  e <- app_env()
  g <- sf::st_sf(territory_id = c("old1", "new1", "old2", "new2"), current = c(FALSE, TRUE, FALSE, TRUE), geometry = sf::st_sfc(square(0, 0), square(0, 0), square(1, 1), square(1, 1), crs = 4326))
  expect_equal(e$polygons_to_draw(g, c("new1", "old2"))$territory_id, c("new2", "new1", "old2"))   # old1 is not drawn; new2, without figures, goes first (under the others)
  g$current <- c(0L, 1L, 0L, 1L); expect_equal(e$polygons_to_draw(g, character())$territory_id, c("new1", "new2"))   # as a GeoPackage gives the column back
  g$current <- NULL; expect_setequal(e$polygons_to_draw(g, "new1")$territory_id, g$territory_id)   # a layer without the column: every polygon is current
  expect_equal(utils::tail(e$polygons_to_draw(g, "new1")$territory_id, 1), "new1")
})

test_that("the citation names the source, its licence, the period on screen and what was done to the figures; the app is never the publisher", {
  e <- app_env()
  parts <- list(publisher = "Madrid City Council - Municipal Police", title = "Incidencias recibidas en la Emisora Central.", url = "https://datos.madrid.es/x", licence = "CC BY 4.0",
                period = "Jul 2026 – Sep 2026", consulted = "2026-11-10", threshold = 10)
  l <- e$cite_lines(parts, "en")
  expect_equal(l[1], "Madrid City Council - Municipal Police. Incidencias recibidas en la Emisora Central. https://datos.madrid.es/x")
  expect_equal(l[2:4], c("Licence CC BY 4.0.", "Period shown: Jul 2026 – Sep 2026.", "Source last read: 2026-11-10."))
  expect_equal(l[5], "Changes: figures aggregated by WebarchyMapp, a personal project that is not an official product of the publisher; counts below 10 are not shown.")
  l2 <- e$cite_lines(utils::modifyList(parts, list(consulted = NULL, licence = "", url = NA)), "es")   # what a source lacks is left out, not written empty
  expect_length(l2, 3); expect_equal(l2[1], "Madrid City Council - Municipal Police. Incidencias recibidas en la Emisora Central."); expect_match(l2[2], "^Periodo mostrado: "); expect_match(l2[3], "^Cambios: ")
  b <- e$cite_bibtex(parts, "en", "madrid")
  expect_match(b, "^@misc\\{madrid_2026,")
  for (x in c("author       = {{Madrid City Council - Municipal Police}}", "title        = {Incidencias recibidas en la Emisora Central}", "year         = {2026}", "howpublished = {\\url{https://datos.madrid.es/x}}", "counts below 10 are not shown."))
    expect_true(grepl(x, b, fixed = TRUE), info = x)
  expect_true(grepl("100\\% of A\\&B", e$cite_bibtex(utils::modifyList(parts, list(title = "100% of A&B")), "en"), fixed = TRUE))
  m <- paste(as.character(e$cite_modal(parts, "fr", "madrid")), collapse = "")
  for (x in c("Citer ces données", 'data-wm-copy="cite-text"', "Période affichée : Jul 2026", "BibTeX", "Copier", 'data-wm-done="Copié"', "Fermer")) expect_true(grepl(x, m, fixed = TRUE), info = x)
  for (lang in names(e$LANGUAGES)) {                                       # in every language: five lines, the app and the threshold named in the last one
    x <- e$cite_lines(parts, lang); expect_length(x, 5)
    expect_true(grepl("WebarchyMapp", x[5], fixed = TRUE) && grepl("10", x[5], fixed = TRUE), info = lang)
    expect_true(grepl("2026-11-10", x[4], fixed = TRUE) && grepl("Sep 2026", x[3], fixed = TRUE), info = lang)
  }
  load_fixtures()
  expect_false(is.na(db_query("SELECT loaded_at FROM mart_country_meta WHERE country = 'XT'")$loaded_at))   # the day the source was last read reaches the app
})


test_that("the statistics of a city: a short series opens by month, few bars stay narrow, and a city without a complete year has no seasonal block", {
  e <- app_env()
  # The unit the chart over time opens with: by month up to two years of months, then by quarter; a yearly source by year
  expect_equal(e$ts_default_gran("month", 10), "month"); expect_equal(e$ts_default_gran("month", 24), "month")
  expect_equal(e$ts_default_gran("month", 25), "quarter"); expect_equal(e$ts_default_gran("year", 5), "year")
  z <- e$get_view("zurich"); b <- e$get_view("buenosaires")
  expect_equal(z$TS_GRAN, "year"); expect_equal(b$TS_GRAN, e$ts_default_gran("month", length(b$MONTHS)))
  # ...is the one ticked on the page; the links to the last years are offered only to a series longer than they are
  html <- function(...) paste(as.character(htmltools::renderTags(e$ts_ui("en", c("month", "quarter", "year"), ...))$html), collapse = "\n")
  ticked <- function(h, unit) grepl(sprintf('name="ts_gran" value="%s" checked="checked"', unit), h, fixed = TRUE)
  short <- html(selected = "month", span_months = 10)
  expect_true(ticked(short, "month")); expect_false(grepl("ts_last_", short, fixed = TRUE)); expect_false(grepl("ts-chips", short, fixed = TRUE))
  mid <- html(span_months = 30)
  expect_true(ticked(mid, "quarter")); expect_true(grepl('id="ts_last_1"', mid, fixed = TRUE)); expect_false(grepl('id="ts_last_3"', mid, fixed = TRUE)); expect_true(grepl('id="ts_all"', mid, fixed = TRUE))
  long <- html()
  expect_true(ticked(long, "quarter")); expect_true(all(vapply(c("ts_last_1", "ts_last_3", "ts_last_5", "ts_all"), function(k) grepl(sprintf('id="%s"', k), long, fixed = TRUE), TRUE)))
  # Few bars on a wide chart: each keeps its slot and is at most 110 pixels wide, centred; many bars: 80% of the slot, as before
  f <- e$bar_fit(3, plot_px = 1800)
  expect_equal(f$width, 110 / 600); expect_equal(f$xlim, c((1 - f$width) / 2, 3 + (1 - f$width) / 2))
  mid_points <- as.numeric(graphics::barplot(c(4, 5, 6), width = f$width, space = f$space, plot = FALSE))
  expect_equal(mid_points - f$xlim[1], c(0.5, 1.5, 2.5))                                             # the middle of each third of the axis
  expect_equal(e$bar_fit(40, plot_px = 1200)$width, 0.8); expect_equal(e$bar_fit(3, plot_px = NA)$width, 0.8); expect_equal(e$bar_fit(0, plot_px = 500)$width, 110 / 500)   # no device to measure: the usual share; no bars: as for one
  # The seasonal profile is a block of the page only when the city has one: it needs a complete calendar year
  expect_true(grepl('data-display-if="output.has_season"', render(e, "zurich", "en"), fixed = TRUE))
  months <- function(from, k) seq(as.Date(from), by = "month", length.out = k)
  expect_null(e$season_profile(months("2026-01-01", 10), rep(100, 10)))                              # Madrid: ten months of 2026
  expect_null(e$season_profile(as.Date(character()), numeric()))
  s <- e$season_profile(c(months("2024-03-01", 32), months("2025-01-01", 12)), c(rep(10, 32), rep(2, 12)))   # March 2024 to October 2026, 2025 in two parts
  expect_equal(s$years, 2025L); expect_equal(nrow(s$avg), 12); expect_equal(s$avg$n, rep(12, 12)); expect_equal(s$total, 144)
})

test_that("an output container is never styled, never placed, and never set straight into a flex or grid layout: a recent shiny draws it as if it were not there", {
  e <- app_env()
  # Since shiny 1.8.1 a page on Bootstrap 5 draws the div of uiOutput() / tableOutput() and the one of conditionalPanel()
  # with display: contents, and gives the second a class of its own. The copy this code is written on had shiny 1.8.0,
  # the founder's computer 1.14: the page was laid out with boxes that are not there on his screen, and a check that
  # looked for class="stats-pair-season" could not find class="shiny-panel-conditional stats-pair-season" (P-064).
  # The rule that makes a page the same on both: whatever must be a box, or one item of a layout, is a plain div.
  css <- gsub("(?s)/\\*.*?\\*/", "", e$CSS, perl = TRUE)
  flex <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*display:\\s*(inline-)?(flex|grid)[^{}]*\\}", css, perl = TRUE))[[1]]
  subject <- unlist(lapply(strsplit(sub("(?s)\\{.*$", "", flex, perl = TRUE), ",", fixed = TRUE), function(x) sub("(?s)^.*[ >+~]", "", trimws(x), perl = TRUE)))    # the last part of each selector: the element the rule lays out
  marks <- unique(c(unlist(regmatches(subject, gregexpr("[.#][A-Za-z0-9_-]+", subject))),                 # the stylesheet's own flex and grid containers...
                    ".html-fill-container", ".bslib-gap-spacing", ".sidebar-content", ".bslib-grid", ".d-flex", ".d-grid", ".d-inline-flex", ".row"))   # ...and those of bslib and Bootstrap
  expect_true(all(c(".stats-pair", ".stats-split", "#stats-bar", ".facet", ".kpis") %in% marks))
  lays_out <- function(el, i) any(c(paste0(".", classes_of(el$class[i])), if (!is.na(el$id[i])) paste0("#", el$id[i])) %in% marks)
  faults <- function(h) {
    el <- html_elements(h); out <- character()
    box <- which((el$tag == "div" & vapply(el$class, function(x) "shiny-html-output" %in% classes_of(x), TRUE)) | el$cond)
    for (i in box) {
      what <- if (el$cond[i]) "a conditional panel" else paste0("#", el$id[i])
      own <- classes_of(el$class[i]); own <- own[!grepl("^(shiny-|html-fill-)", own)]
      if (length(own)) out <- c(out, sprintf("%s carries the class %s", what, paste(own, collapse = " ")))
      if (!el$cond[i] && grepl(sprintf("#%s\\s*[,{]", el$id[i]), css, perl = TRUE)) out <- c(out, sprintf("%s is addressed by the stylesheet", what))
      up <- match(el$parent[i], el$row); if (is.na(up) || !lays_out(el, up)) next
      inside <- which(el$parent == el$row[i])                         # a conditional panel may stand in a layout when all it holds is one plain div
      if (el$cond[i] && length(inside) == 1 && el$tag[inside] == "div" && !lays_out(el, inside)) next
      out <- c(out, sprintf("%s stands straight in a flex or grid container (%s)", what, paste(c(el$id[up], el$class[up])[!is.na(c(el$id[up], el$class[up]))], collapse = " ")))
    }
    list(n = length(box), faults = out)
  }
  # The reader tells the three faults apart, written as shiny 1.8.0 and as shiny 1.14 write them
  old <- '<div class="stats-pair"><div data-display-if="output.x" data-ns-prefix="" class="stats-pair-season"><hr/><div id="a" class="shiny-html-output"></div></div><div id="where" class="shiny-html-output where-box"></div></div>'
  new <- gsub('data-display-if="output.x" data-ns-prefix="" class="stats-pair-season"', 'class="shiny-panel-conditional stats-pair-season" data-display-if="output.x" data-ns-prefix="" style="display: none;"', old, fixed = TRUE)
  for (h in c(old, new)) { f <- faults(h); expect_equal(f$n, 3L)
    expect_setequal(f$faults, c("a conditional panel carries the class stats-pair-season", "a conditional panel stands straight in a flex or grid container (stats-pair)",
                                "#where carries the class where-box", "#where stands straight in a flex or grid container (stats-pair)")) }
  good <- '<div class="stats-pair"><div class="stats-pair-season"><div class="shiny-panel-conditional" data-display-if="output.x"><hr/><div><div id="a" class="shiny-html-output"></div></div></div></div><div class="where-box"><div id="where" class="shiny-html-output"></div></div><div class="sidebar-content"><div class="shiny-panel-conditional" data-display-if="input.y"><div><select id="s"></select><input id="t" type="text"/></div></div></div></div>'
  expect_equal(faults(good), list(n = 4L, faults = character()))
  expect_equal(faults(sub('<div><select id="s">', '<div class="d-flex"><select id="s">', good, fixed = TRUE))$faults, "a conditional panel stands straight in a flex or grid container (sidebar-content)")
  expect_equal(faults('<div id="stats-bar"><div id="b" class="shiny-html-output"></div></div>')$faults, "#b stands straight in a flex or grid container (stats-bar)")
  css_was <- css; css <- paste(css, "#b { min-height: 1rem; }"); expect_equal(faults('<div><div id="b" class="shiny-html-output"></div></div>')$faults, "#b is addressed by the stylesheet"); css <- css_was
  # Every page of the app, and a country's page with its map (the test country has none: the page is built as if it had)
  pages <- lapply(c(zurich = "zurich", toronto = "toronto", buenosaires = "buenosaires", XT = "XT", about = "about"), function(v) render(e, v, "en"))
  xt <- e$get_view("XT"); had <- xt$HAS_MAP; xt$HAS_MAP <- TRUE
  pages$XT_map <- tryCatch(paste(as.character(htmltools::renderTags(xt$ui_country("en"))$html), collapse = "\n"), finally = xt$HAS_MAP <- had)
  expect_false(xt$HAS_MAP); expect_true(grepl('id="sp_where"', pages$XT_map, fixed = TRUE)); expect_false(grepl('id="sp_where"', pages$XT, fixed = TRUE))
  found <- lapply(pages, faults)
  for (v in names(found)) expect_equal(found[[v]]$faults, character(), info = v)
  expect_true(all(vapply(found[c("zurich", "toronto", "buenosaires", "XT", "XT_map")], function(f) f$n, 1L) >= 10L)); expect_equal(found$about$n, 0L)   # the reader does see them
  # The plain divs that are the boxes: the note over a map, the line of a bar clicked, the time of year, a country's title
  parent_class <- function(h, id) { el <- html_elements(h); i <- match(id, el$id); el$class[match(el$parent[i], el$row)] }
  expect_equal(parent_class(pages$buenosaires, "where"), "where-box"); expect_equal(parent_class(pages$XT_map, "sp_where"), "where-box")
  expect_equal(parent_class(pages$buenosaires, "ts_value"), "ts-line"); expect_equal(parent_class(pages$XT, "ts_value"), "ts-line"); expect_equal(parent_class(pages$XT, "sp_title"), "sp-title")
  expect_true(is.na(parent_class(pages$buenosaires, "season_notes")) && is.na(parent_class(pages$buenosaires, "stats_table")) && is.na(parent_class(pages$XT, "sp_notes")))   # a div with no class
  el <- html_elements(pages$buenosaires); panel <- which(el$shown_if %in% "output.has_season"); expect_length(panel, 1L)
  expect_equal(el$class[match(el$parent[panel], el$row)], "stats-pair-season"); expect_equal(el$class[match(el$parent[match(el$parent[panel], el$row)], el$row)], "stats-pair")
  expect_true(grepl(".ts-two:has(.ts-rate) .ts-line { min-height: 1.6rem; }", e$CSS, fixed = TRUE)); expect_true(grepl(".wm-toggle-over .sp-title { margin-left: 2.6rem;", e$CSS, fixed = TRUE))
  expect_false(grepl("shiny-html-output|shiny-panel-conditional", css))      # nor does the stylesheet change how shiny draws them
})

test_that("room for the button of the side panel is made only where the page says the button lies over something: a title, a map, the first field of the open panel", {
  e <- app_env()
  # An older bslib lays the button that opens and closes the side panel over the top left corner of the page; bslib
  # 0.12 (the founder's) gives it, on a narrow screen, a row of its own above the page. Room made for the button by
  # rules that always held left, on his telephone, the title of a country indented under an empty band and the zoom
  # buttons of a map lowered for nothing (seen on 17 November 2026; P-068). The room is now the answer to questions
  # the page puts to itself. What is held here: no rule makes room without such an answer.
  css <- gsub("(?s)/\\*.*?\\*/", "", e$CSS, perl = TRUE)
  rules <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css))[[1]]; sel <- trimws(sub("(?s)\\{.*$", "", rules, perl = TRUE)); body <- sub("(?s)^[^{]*\\{", "", rules, perl = TRUE)
  room <- grepl("margin-left: 2\\.[26]rem|top: [23]\\.[46]rem", body)                          # what room is: a title or a field moved right, the zoom buttons of a map moved down
  expect_equal(sum(room), 4L)
  for (x in sel[room]) expect_true(all(grepl("^\\s*\\.bslib-sidebar-layout(\\.sidebar-collapsed)?\\.wm-toggle-over(-map|-side)?[ >]", strsplit(x, ",", fixed = TRUE)[[1]])), info = x)
  for (rule in c(".bslib-sidebar-layout.sidebar-collapsed.wm-toggle-over > .main > *:first-child { margin-left: 2.6rem; }",
                 ".bslib-sidebar-layout.wm-toggle-over .sp-title { margin-left: 2.6rem; padding-left: 0; }",
                 ".bslib-sidebar-layout.wm-toggle-over-map .leaflet-top.leaflet-left { top: 3.4rem; }",
                 ".bslib-sidebar-layout.wm-toggle-over-side > .sidebar > .sidebar-content > *:first-child, .bslib-sidebar-layout.wm-toggle-over-side .sidebar .shiny-input-container:has(#neighbourhood) { margin-left: 2.2rem; width: calc(100% - 2.2rem) !important; }"))
    expect_true(grepl(rule, e$CSS, fixed = TRUE), info = rule)
  expect_true(grepl(".sp-title { padding-top: .5rem; padding-left: .6rem; }", e$CSS, fixed = TRUE))   # with no button beside it, the title of a country keeps off the edge of a narrow screen
  # The script that asks: three answers written on the layout, asked again when the window, a tab or the panel changes
  for (piece in c("'wm-toggle-over'", "'wm-toggle-over-map'", "'wm-toggle-over-side'", "getBoundingClientRect", "'collapse-toggle'", "'sidebar-content'", ".leaflet-container, .leaflet.html-widget",
                  "addEventListener('resize'", "addEventListener('shown.bs.tab'", "attributeFilter: ['class']", "window.wmToggleLook",
                  "Math.min(a.right, b.right) - Math.max(a.left, b.left) >= 8 && Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top) >= 8"))   # to lie over means by 8 px or more: bslib 0.12's button ends 2 px inside the first row of the open panel and covers nothing
    expect_true(grepl(piece, e$JS, fixed = TRUE), info = piece)
  # What the rules and the script lean on in a page: the button, the page and the panel are the children of one layout;
  # the panel holds its fields in one box, and the first of them is the search box of a city or the first control of
  # a country; a map is a leaflet widget
  xt <- e$get_view("XT"); had <- xt$HAS_MAP; xt$HAS_MAP <- TRUE
  xt_map <- tryCatch(paste(as.character(htmltools::renderTags(xt$ui_country("en"))$html), collapse = "\n"), finally = xt$HAS_MAP <- had)
  pages <- c(lapply(c(zurich = "zurich", buenosaires = "buenosaires", XT = "XT"), function(v) render(e, v, "en")), list(XT_map = xt_map))
  first_field <- c(zurich = "neighbourhood", buenosaires = "neighbourhood", XT = "sp_level", XT_map = "sp_follow")
  for (v in names(pages)) {
    el <- html_elements(pages[[v]]); has <- function(i, cls) vapply(el$class[i], function(x) cls %in% classes_of(x), TRUE, USE.NAMES = FALSE)
    layout <- which(has(seq_len(nrow(el)), "bslib-sidebar-layout")); expect_length(layout, 1L)
    kids <- which(el$parent %in% el$row[layout]); expect_equal(c(sum(has(kids, "collapse-toggle")), sum(has(kids, "main")), sum(has(kids, "sidebar"))), c(1, 1, 1), info = v)
    content <- which(el$parent %in% el$row[kids[has(kids, "sidebar")]] & has(seq_len(nrow(el)), "sidebar-content")); expect_length(content, 1L)
    first <- which(el$parent %in% el$row[content])[1]
    within <- function(r) { k <- el$row[el$parent %in% r]; c(k, unlist(lapply(k, within))) }
    expect_true(first_field[[v]] %in% el$id[match(c(el$row[first], within(el$row[first])), el$row)], info = v)
    map <- which(el$id %in% c("map", "sp_map")); expect_length(map, if (v == "XT") 0L else 1L)
    if (length(map)) expect_true(all(c("leaflet", "html-widget") %in% classes_of(el$class[map])), info = v)
  }
})

test_that("the light and the dark look: a button first in every top bar, the reader's system by default and asked in the browser only, two values for every colour, a map and the colours of the figures that stay as they are", {
  e <- app_env()
  # 1. The button: the first thing of the top bar of every page (top left), a real button that carries the two texts it
  #    may show, in the page's language; the title of the window is still the name of the page and nothing else
  for (v in c("zurich", "buenosaires", "XT", "about")) for (l in c("en", "es")) {
    tags <- htmltools::renderTags(e$ui(list(QUERY_STRING = sprintf("?view=%s&lang=%s", v, l)))); h <- paste(as.character(tags$html), collapse = "\n"); head <- paste(as.character(tags$head), collapse = "\n")
    el <- html_elements(h); b <- which(vapply(el$class, function(x) "wm-theme" %in% classes_of(x), TRUE, USE.NAMES = FALSE)); expect_length(b, 1L)
    expect_equal(el$tag[b], "button"); brand <- match(el$parent[b], el$row); expect_true("navbar-brand" %in% classes_of(el$class[brand]), info = v)
    expect_equal(which(el$parent %in% el$row[brand])[1], b, info = v)
    expect_true(grepl(sprintf('<button type="button" class="wm-theme" data-to-dark="%s" data-to-light="%s"', e$TRANSLATIONS[[l]][["theme_dark"]], e$TRANSLATIONS[[l]][["theme_light"]]), h, fixed = TRUE), info = paste(v, l))
    title <- regmatches(head, regexpr("<title>[^<]*</title>", head)); expect_length(title, 1L); expect_true(grepl("^<title>WebarchyMapp · [^<>]+</title>$", title), info = paste(v, l, title))
    # The look is set before anything is drawn: in the head, before the app's stylesheet. (Not "before the first style
    # of the head": bslib 0.12 puts a one-line style of its own there, and the check failed on the founder's computer
    # for that reason alone on 18 November 2026. Whatever is in the head runs before the page is drawn.)
    first <- regexpr("localStorage.getItem('wm-theme')", head, fixed = TRUE); sheet <- regexpr(":root, [data-bs-theme='light'] { --wm-surface:", head, fixed = TRUE)
    expect_true(first > 0, info = v); expect_true(sheet > 0, info = v); expect_true(first < sheet, info = v)
  }
  for (l in names(e$LANGUAGES)) for (k in c("theme_dark", "theme_light")) expect_true(nzchar(e$TRANSLATIONS[[l]][[k]]), info = paste(l, k))
  for (l in names(e$LANGUAGES)) expect_false(identical(e$TRANSLATIONS[[l]][["theme_dark"]], e$TRANSLATIONS[[l]][["theme_light"]]), info = l)

  # 2. The reader's system is asked in the browser, by the standard question, and the answer goes nowhere: nothing of
  #    the computer is read by the code of the project, nothing is written into it (the code is published), and the
  #    browser keeps one word (light or dark) for this app alone. The server is told that word, to draw its charts.
  block <- function(from, to) { a <- regexpr(from, e$JS, fixed = TRUE); b <- regexpr(to, e$JS, fixed = TRUE); expect_true(a > 0 && b > a); substr(e$JS, a, b - 1) }
  theme_js <- block("// The light and the dark look (P-070).", "// The button that opens and closes the side panel")
  for (js in list(e$THEME_HEAD_JS, theme_js)) {
    expect_true(grepl("matchMedia('(prefers-color-scheme: dark)')", js, fixed = TRUE)); expect_true(grepl("data-bs-theme", js, fixed = TRUE))
    expect_false(grepl("fetch\\(|XMLHttpRequest|sendBeacon|document\\.cookie|navigator\\.|location\\.host|WebSocket|\\.src\\s*=|indexedDB", js))
  }
  expect_true(grepl("localStorage.getItem('wm-theme')", e$THEME_HEAD_JS, fixed = TRUE)); expect_true(grepl("var KEY = 'wm-theme'", theme_js, fixed = TRUE))
  expect_equal(lengths(regmatches(theme_js, gregexpr("Shiny.setInputValue(", theme_js, fixed = TRUE))), 1L); expect_true(grepl("Shiny.setInputValue('theme', t)", theme_js, fixed = TRUE))
  expect_true(grepl("localStorage.removeItem(KEY)", theme_js, fixed = TRUE))                     # a choice that is the system's own is not kept: the page follows the system again
  keys <- unique(c(unlist(regmatches(e$JS, gregexpr("(?<=var KEY = ')[^']+", e$JS, perl = TRUE))), unlist(regmatches(e$THEME_HEAD_JS, gregexpr("(?<=Item\\(')[^']+", e$THEME_HEAD_JS, perl = TRUE)))))
  expect_setequal(keys, c("wm-theme", "wm-profile-place"))                                      # all the browser keeps for this app: the look, and where the reader put the profile
  expect_false(grepl("localStorage\\.(set|get|remove)Item\\('", e$JS))                         # in the page's script a key is named once, as KEY
  code <- unlist(lapply(c(list.files("app", "\\.R$", full.names = TRUE), list.files("R", "\\.R$", full.names = TRUE)), readLines, encoding = "UTF-8", warn = FALSE))
  expect_false(any(grepl("AppsUseLightTheme|readRegistry|AppleInterfaceStyle|gsettings|color-scheme'\\)\\s*<-", code)))   # no code of the project asks the computer for its look

  # 3. Every colour of the stylesheet has a name and two values; the rules name the colour and never write it
  cols <- e$THEME_COLOURS; expect_true(length(cols) >= 25 && all(lengths(cols) == 2) && !anyDuplicated(names(cols)) && all(grepl("^[a-z0-9-]+$", names(cols))))
  css <- gsub("(?s)/\\*.*?\\*/", "", e$CSS, perl = TRUE); rules <- regmatches(css, gregexpr("[^{}]+\\{[^{}]*\\}", css))[[1]]
  sel <- trimws(sub("(?s)\\{.*$", "", rules, perl = TRUE)); body <- sub("(?s)^[^{]*\\{", "", rules, perl = TRUE)
  defines <- grepl("--wm-surface:", body, fixed = TRUE); expect_equal(sum(defines), 2L)
  expect_equal(sel[defines], c(":root, [data-bs-theme='light']", "[data-bs-theme='dark']"))
  value <- function(rule, name) sub(sprintf("(?s)^.*--wm-%s: ([^;]+);.*$", name), "\\1", rule, perl = TRUE)
  for (n in names(cols)) { expect_equal(c(value(body[defines][1], n), value(body[defines][2], n)), c(cols[[n]][1], cols[[n]][2]), info = n)
    expect_true(any(grepl(sprintf("var(--wm-%s)", n), body[!defines], fixed = TRUE)), info = n) }                        # none is defined for nothing
  used <- unique(unlist(regmatches(body[!defines], gregexpr("var\\(--wm-[a-z0-9-]+", body[!defines])))); expect_setequal(sub("var\\(--wm-", "", used), names(cols))   # and none is used without a value
  written <- unlist(regmatches(body[!defines], gregexpr("(?<![0-9a-zA-Z%])#[0-9a-fA-F]{3,8}\\b|rgba?\\(2[0-9]{2},\\s*2[0-9]{2},\\s*2[0-9]{2}[^)]*\\)", body[!defines], perl = TRUE)))
  expect_setequal(written, c("#3182bd", "#4e79a7", "#86b7fe", "#fff"))                          # the marks of the app's own blue, the same in both looks, and one fallback of the framework
  expect_equal(sum(written == "#fff"), 1L); expect_true(grepl("var(--bs-dropdown-bg, #fff)", css, fixed = TRUE))
  expect_equal(c(cols$surface[1], cols[["ink-3"]][1], cols$accent[1], cols$sel[1]), c("#fff", "#64748b", "#1d5fa6", "#fff7ed"))   # the light look is the one the page always had
  # ...and both looks can be read: the inks against their ground (WCAG contrast, 4.5 for text)
  lum <- function(hex) { hex <- sub("^#", "", hex); if (nchar(hex) == 3) hex <- paste(rep(strsplit(hex, "")[[1]], each = 2), collapse = ""); v <- strtoi(substring(hex, c(1, 3, 5), c(2, 4, 6)), 16L) / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4); sum(v * c(0.2126, 0.7152, 0.0722)) }
  contrast <- function(a, b) { l <- sort(c(lum(a), lum(b)), decreasing = TRUE); (l[1] + 0.05) / (l[2] + 0.05) }
  expect_equal(round(contrast("#000", "#fff"), 1), 21)
  for (n in c("ink", "ink-strong", "ink-btn", "ink-2", "ink-3", "ink-4", "accent", "accent-strong", "accent-2")) {
    expect_true(contrast(cols[[n]][1], "#ffffff") >= 4.5, info = paste(n, "light")); expect_true(contrast(cols[[n]][2], "#1d1f21") >= 4.5, info = paste(n, "dark"))
    expect_true(contrast(cols[[n]][2], cols$soft[2]) >= 4.5, info = paste(n, "dark, on a ground set apart")) }
  expect_true(contrast(cols[["on-accent"]][1], cols$accent[1]) >= 4.5 && contrast(cols[["on-accent"]][2], cols$accent[2]) >= 4.5)

  # 4. A map in the dark look (P-071): the whole map has light and dark exchanged and its hues kept, by ONE rule, so that
  #    the areas and their legend can never disagree. Inside the map the colours stay the light ones (the script marks
  #    every map data-bs-theme=light and never turns it round with the page) for that rule to exchange. The box over the
  #    map and the button of the side panel are part of the page and follow it.
  expect_true(grepl("[data-bs-theme='dark'] .leaflet.html-widget { filter: invert(1) hue-rotate(180deg); }", e$CSS, fixed = TRUE))
  expect_equal(sum(grepl("filter:", body, fixed = TRUE)), 1L)                                    # one rule, and nothing else is turned round that way (a chart is drawn in its ink, not filtered)
  expect_true(grepl("querySelectorAll('.leaflet.html-widget')", theme_js, fixed = TRUE)); expect_true(grepl("el.setAttribute('data-bs-theme', 'light')", theme_js, fixed = TRUE))
  expect_true(grepl("if (el === root || el.hasAttribute('data-wm-keep')) return;", theme_js, fixed = TRUE))
  w <- paste(as.character(e$where_box(list(list(level = "City", name = "Zürich", current = TRUE)), "Where you are")), collapse = "")
  expect_true(grepl('<div class="where" role="navigation"', w, fixed = TRUE)); expect_false(grepl("data-bs-theme|data-wm-keep", w))
  # What the texts say of a map has to be true of the light map (most = the darkest blue) and of the dark one (most = the
  # brightest): "a stronger colour", never "darker". The two help texts of the maps were reworded in the 32 languages
  # for that (they said "look dark ... darker does not mean danger"); none of them names a dark tone now.
  dark_word <- c(en = "dark", es = "oscur", ca = "fosc", gl = "escur", fr = "fonc|sombre", it = "scur", de = "dunk", pt = "escur", zh = "深|暗", ja = "濃|暗", ko = "진하|진한|어둡|어두", nl = "donker",
                 da = "mørk", sv = "mörk", no = "mørk", fi = "tumm", is = "dökk|dekkr", pl = "ciemn", cs = "tmav", sk = "tmav", hu = "sötét", ro = "închis|întunec", bg = "тъмн", el = "σκούρ|σκοτειν",
                 hr = "tamn", sl = "temn", sr = "тамн", sq = "errët", et = "tume", lv = "tumš", lt = "tams", tr = "koyu")
  expect_setequal(names(dark_word), names(e$LANGUAGES))
  for (l in names(dark_word)) for (k in c("shade_help", "sp_shade_help")) expect_false(grepl(dark_word[[l]], e$TRANSLATIONS[[l]][[k]], ignore.case = TRUE), info = paste(l, k))
  expect_true(grepl("stand out even with a normal rate. A stronger colour does not mean danger.", e$TRANSLATIONS$en[["shade_help"]], fixed = TRUE))
  expect_true(grepl("destacan casi siempre. Un color más intenso no significa peligro.", e$TRANSLATIONS$es[["sp_shade_help"]], fixed = TRUE))
  # The texts that still name a tone are these and no others: the paler bar of an incomplete period (a chart: its
  # colours are the same in both looks), the "dark figure" of criminology, and the two texts of the button itself.
  # A new text that names a tone has to be read against the two looks before it joins this list.
  tone <- names(e$TRANSLATIONS$en)[grepl("\\b(dark|darker|darkest|light|lighter|lightest|pale|paler|bright|brighter|faint|fainter)\\b", unlist(e$TRANSLATIONS$en), ignore.case = TRUE)]
  expect_setequal(tone, c("ts_help", "gl_dark_figure_t", "reading_3@cases", "theme_dark", "theme_light"))

  # 5. The charts are images: drawn on no ground of their own, in the ink of the look, and again when the look changes.
  #    The colours that carry data are the same in both looks, so that what the texts say of them stays true
  src <- lapply(list.files("app", "\\.R$", full.names = TRUE), readLines, encoding = "UTF-8", warn = FALSE); code <- unlist(src)
  code <- code[!grepl("^\\s*#", code)]
  expect_equal(sum(lengths(regmatches(code, gregexpr("renderPlot(", code, fixed = TRUE)))), 1L); expect_true(any(grepl("shiny::renderPlot({", code, fixed = TRUE)))   # every chart goes through render_chart()
  expect_true(sum(lengths(regmatches(code, gregexpr("<- render_chart({", code, fixed = TRUE)))) >= 9)
  expect_true(grepl('bg = "transparent"', paste(deparse(e$render_chart), collapse = " "), fixed = TRUE))
  was <- e$INK$dark; on.exit(e$INK$dark <- was, add = TRUE)
  e$INK$dark <- FALSE; expect_equal(e$ink(c("#333333", "#475569", "#3182bd", "#ffffff")), c("#333333", "#475569", "#3182bd", "#ffffff"))
  e$INK$dark <- TRUE
  expect_equal(e$ink(c("#3182bd", "#9ecae1", "#64748b", "#94a3b8", "#4e79a7", "#8fb3d6")), c("#3182bd", "#9ecae1", "#64748b", "#94a3b8", "#4e79a7", "#8fb3d6"))   # the bars, the paler bar of an incomplete period, the hatching: as they were
  expect_true(lum("#9ecae1") > lum("#3182bd"))                                                   # "an incomplete last period is lighter": in both looks
  for (light in c("#333333", "#666666", "#475569", "#555555")) { dark <- e$ink(light); expect_false(identical(dark, light)); expect_true(contrast(dark, "#1d1f21") >= 4.5, info = light); expect_true(contrast(light, "#ffffff") >= 4.5, info = light) }
  expect_true(contrast(e$ink("#ffffff"), "#1d1f21") < 1.2)                                       # the edge that sets a mark off its ground has the colour of the ground
  expect_true(lum(e$ink("#08519c")) > lum(e$ink("#c6dbef")))                                     # the bar chosen stands out from the others: darker than them on white, lighter on dark
  e$INK$dark <- FALSE; expect_true(lum("#08519c") < lum("#c6dbef"))
  z <- e$get_view("zurich"); labels <- z$month_labels("en"); types <- unique(z$base$call_type[z$base$block == "public_safety"])
  shiny::testServer(function(input, output, session) z$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(1, length(labels))], neighbourhood = "", stats_level = "neighbourhood", sort = "rate", shade = "rate", stats_zone = "city")
    session$elapse(400); session$setInputs(types = types); session$elapse(400)
    first <- output$stats_plot$src; expect_true(grepl("^data:image/png;base64,", first)); expect_false(e$INK$dark)       # no word from the page yet: the light ink
    session$setInputs(theme = "dark"); session$flushReact(); dark <- output$stats_plot$src; expect_true(e$INK$dark); expect_false(identical(dark, first))   # drawn again, in the other ink
    session$setInputs(theme = "light"); session$flushReact(); light <- output$stats_plot$src; expect_false(e$INK$dark); expect_identical(light, first)
    session$setInputs(theme = "anything else"); session$flushReact(); invisible(output$stats_plot$src); expect_false(e$INK$dark)
  })
})

test_that("the statistics of a city: by type and over time side by side, names that fit their chart, no link that repeats another, and Madrid's sentence on where its series begins", {
  e <- app_env()
  # The chart by type and the chart over time share a row on a wide screen (the stylesheet says from which width); the ranking comes after both
  h <- render(e, "buenosaires", "en")
  at <- function(x) { i <- regexpr(x, h, fixed = TRUE); if (i < 0) NA_integer_ else as.integer(i) }
  pair <- at('class="stats-pair"'); left <- at('class="stats-pair-types"'); right <- at('class="stats-pair-series"'); under <- at('class="stats-pair-season"')
  types <- at('id="types_plot"'); series <- at('id="ts_main"'); bar <- at('id="ts_nav"'); season <- at('id="season_plot"'); top <- at('id="stats_plot"')
  expect_false(anyNA(c(pair, left, right, under, types, series, bar, season, top)))
  expect_true(pair < left && left < types && types < right && right < series && series < bar && bar < under && under < season && season < top)   # by type, over time, time of year; the ranking after them
  # The time of year is shown only when the city has one. Its place in the pair is a plain div and the conditional panel is
  # inside it: a recent shiny gives that panel a class of its own and no box (P-064), so the class and the place cannot be the panel's
  expect_true(grepl('class="stats-pair-season">\\s*<div[^>]*\\sdata-display-if="output.has_season"', h, perl = TRUE))
  expect_true(grepl(".stats-pair { display: grid", e$CSS, fixed = TRUE)); expect_true(grepl("@media (min-width: 1200px)", e$CSS, fixed = TRUE))
  expect_true(grepl(".stats-pair-series { grid-column: 2; grid-row: 1 / span 2; }", e$CSS, fixed = TRUE))          # the charts over time at the right, beside the chart by type and the time of year
  expect_true(grepl(".stats-pair-series > .ts-block > hr:first-child { display: none; }", e$CSS, fixed = TRUE))   # side by side, no rule between them

  # A series seen as another one for a moment (the months and the unit of the city's environment, put back afterwards)
  z <- e$get_view("zurich"); b <- e$get_view("buenosaires")
  with_series <- function(v, months, unit, f) { old <- list(v$MONTHS, v$TIME_UNIT); on.exit({ v$MONTHS <- old[[1]]; v$TIME_UNIT <- old[[2]] }); v$MONTHS <- months; v$TIME_UNIT <- unit; f(v) }
  months <- function(from, k) seq(as.Date(from), by = "month", length.out = k); years <- function(from, k) seq(as.Date(from), by = "year", length.out = k)
  page <- function(v) paste(as.character(v$ui_city("en")), collapse = "")
  has <- function(html, ids) vapply(ids, function(k) grepl(sprintf('id="%s"', k), html, fixed = TRUE), TRUE, USE.NAMES = FALSE)
  # The links of the period: none that would do what another one does. Madrid's calls (nine months of one year): no link at all
  expect_equal(with_series(b, months("2026-01-01", 9), "month", function(v) v$views_offered()), "year")
  expect_equal(with_series(b, months("2024-03-01", 31), "month", function(v) v$views_offered()), c("year", "all"))
  expect_equal(with_series(b, months("2019-01-01", 93), "month", function(v) v$views_offered()), c("year", "five", "all"))
  expect_equal(with_series(z, years("2021-01-01", 5), "year", function(v) v$views_offered()), c("year", "all"))         # five years: "5 years" would be "all"
  expect_equal(with_series(z, years("2009-01-01", 17), "year", function(v) v$views_offered()), c("year", "five", "all"))
  one <- with_series(b, months("2026-01-01", 9), "month", page)
  expect_false(any(has(one, c("view_year", "view_five", "view_all", "ts_last_1", "ts_all")))); expect_true(all(has(one, c("period", "ts_main", "types_plot"))))
  expect_equal(has(with_series(b, months("2024-03-01", 31), "month", page), c("view_year", "view_five", "view_all", "ts_last_1", "ts_last_3")), c(TRUE, FALSE, TRUE, TRUE, FALSE))
  expect_true(all(has(with_series(b, months("2019-01-01", 93), "month", page), c("view_year", "view_five", "view_all", "ts_last_1", "ts_last_3", "ts_last_5", "ts_all"))))
  # The length of a series: from its first period to its last, twelve months for each year of a yearly source
  expect_equal(with_series(b, months("2026-01-01", 9), "month", function(v) v$series_months()), 9)
  expect_equal(with_series(b, months("2024-03-01", 31)[-5], "month", function(v) v$series_months()), 31)               # a month missing in between: the span, not the count
  expect_equal(with_series(z, years("2021-01-01", 5), "year", function(v) v$series_months()), 60)
  expect_equal(with_series(z, as.Date(character()), "year", function(v) v$series_months()), 0)
  expect_equal(has(with_series(z, years("2021-01-01", 5), "year", page), c("ts_last_1", "ts_last_3", "ts_last_5")), c(TRUE, TRUE, FALSE))   # five years of a yearly source

  # Where the series begins: a sentence only for the city that has one, with the first month loaded written in it
  expect_null(z$series_start_note("en")); expect_null(b$series_start_note("es"))
  expect_false(grepl("series-start", h, fixed = TRUE))
  as_madrid <- function(lang) { old <- b$CITY; on.exit(b$CITY <- old); b$CITY <- "madrid"; with_series(b, months("2026-01-01", 9), "month", function(v) as.character(v$series_start_note(lang))) }
  es <- as_madrid("es")
  expect_true(grepl('class="note series-start"', es, fixed = TRUE)); expect_true(grepl("se publican desde ene 2026:", es, fixed = TRUE)); expect_false(grepl("%s", es, fixed = TRUE))
  expect_true(grepl("published from Jan 2026 on:", as_madrid("en"), fixed = TRUE)); expect_true(grepl("never added to this one", as_madrid("en"), fixed = TRUE))
  for (l in names(e$LANGUAGES)) {
    x <- e$TRANSLATIONS[[l]][["series_start_madrid"]]
    expect_true(!is.na(x) && nchar(x) > 60, info = l)                                  # a whole sentence in every language (the East Asian ones are short)
    expect_equal(lengths(regmatches(x, gregexpr("%", x, fixed = TRUE))), 1L, info = l); expect_true(grepl("%s", x, fixed = TRUE), info = l); expect_false(grepl("<", x, fixed = TRUE), info = l)
  }
  expect_false(any(grepl("^series_start_(zurich|buenosaires|toronto)$", names(e$TRANSLATIONS$en))))   # no such sentence for the cities that say nothing

  # The names at the left of horizontal bars: cut when wider than their share of the chart, measured and not counted
  m <- function(s) nchar(s) * 0.1                                                        # a ruler for the test: a tenth of an inch per letter
  f <- e$fit_labels(c("Theft", "A very long name of a type of offence", NA), 2, measure = m)
  expect_equal(f$labels, c("Theft", "A very long name of…", "")); expect_equal(f$width, 2)                   # no space left before the dots
  expect_equal(e$fit_labels(c("Theft", "Robbery"), 2, measure = m), list(labels = c("Theft", "Robbery"), width = 0.7))
  expect_equal(e$fit_labels(character(), 2, measure = m), list(labels = character(), width = 0))
  expect_equal(e$fit_labels("WWWW", 0.05, measure = m)$labels, "W…")                    # never less than one letter
  # One name in every how many bars: the names side by side, in steps that read well, counted back from the last bar
  expect_equal(vapply(c(0.6, 1, 1.01, 2.3, 4.5, 7, 12.5, NA, 5000), e$label_step, 1), c(1, 1, 2, 3, 6, 12, 24, 1, 5000))
  expect_equal(which(e$named_bars(9, 2)), c(1, 3, 5, 7, 9)); expect_equal(which(e$named_bars(10, 3)), c(1, 4, 7, 10)); expect_true(all(e$named_bars(5, 1)))
  expect_true(e$named_bars(40, 12)[40])                                                  # the latest period is always named
  # The charts draw on a narrow device too (half a page, a phone): long figures, a missing value, a single bar
  file <- tempfile(fileext = ".png"); grDevices::png(file, width = 420, height = 250, res = 96)
  on.exit({ if (grDevices::dev.cur() > 1) grDevices::dev.off(); unlink(file) }, add = TRUE)
  plain <- function(x) format(x, big.mark = ",")
  expect_length(e$ts_bars(c(5375, 5150, NA, 6381), c("Jan 26", "Feb 26", "Mar 26", "Apr 26"), rep(TRUE, 4), plain, "fewer than 10", "en"), 4)
  expect_length(e$ts_bars(c(rep(1234567, 15), NA), paste("Jan", 11:26), rep(TRUE, 16), plain, "fewer than 10", "en"), 16)   # figures wider than their bars: only what is missing is written
  expect_length(e$ts_bars(NA_real_, "2026", TRUE, plain, "fewer than 10", "en"), 1)
  expect_length(e$ts_bars(1:200, sprintf("%03d", 1:200), rep(c(TRUE, FALSE), 100), plain, "fewer than 10", "en", line = rep(50, 200)), 200)
  real <- e$fit_labels(c("Theft", strrep("W", 80)), 1.5)                                 # measured on the device
  expect_equal(real$labels[1], "Theft"); expect_true(endsWith(real$labels[2], "…")); expect_lte(real$width, 1.5); expect_gt(real$width, 1.2)
  grDevices::dev.off()
})

test_that("a city's statistics: the rate of each complete year under the chart by number, over the same period, as in the countries", {
  e <- app_env()
  z <- e$get_view("zurich"); b <- e$get_view("buenosaires")
  set <- function(v, values, f) { old <- mget(names(values), envir = v); on.exit(list2env(old, envir = v)); list2env(values, envir = v); f(v) }   # some values of a city's environment for a moment
  months <- function(from, k) seq(as.Date(from), by = "month", length.out = k)
  # The complete years: every year of a yearly source; of a monthly one, those with their twelve months
  expect_equal(z$full_years(), c(2024L, 2025L)); expect_equal(b$full_years(), c(2024L, 2025L))
  expect_equal(set(b, list(MONTHS = months("2026-01-01", 9)), function(v) v$full_years()), integer())            # Madrid's calls: nine months of 2026
  expect_equal(set(b, list(MONTHS = months("2024-03-01", 31)), function(v) v$full_years()), 2025L)               # March 2024 to September 2026
  expect_equal(set(b, list(MONTHS = as.Date(character())), function(v) v$full_years()), integer())
  # The whole city: the events of the year per 1,000 of the residents of that year, no reference
  types <- unique(z$base$call_type[z$base$block == "public_safety"])
  n_year <- function(v, rows) vapply(c(2024, 2025), function(y) sum(rows$events[format(rows$month, "%Y") == y]), 1)
  s <- z$rate_by_year("public_safety", types, "city", z$CITY_ID)
  rows <- z$base[z$base$block == "public_safety", ]
  expect_equal(s$year, c(2024L, 2025L)); expect_equal(s$label, c("2024", "2025")); expect_equal(s$start, as.Date(c("2024-01-01", "2025-01-01")))
  expect_equal(s$pop, c(318000, 324000)); expect_equal(s$rate, 1000 * n_year(z, rows) / c(318000, 324000)); expect_true(all(is.na(s$ref)))
  expect_equal(s$pop_year, c(2024L, 2025L)); expect_equal(s$mark, c("", "")); expect_length(attr(s, "lacking"), 0)
  # A neighbourhood: its own events on its own residents, and the city as the reference
  nb <- z$geo_nb$territory_id[1]
  k <- z$rate_by_year("public_safety", types, "neighbourhood", nb)
  expect_equal(k$rate, 1000 * n_year(z, rows[rows$neighbourhood_id %in% nb, ]) / k$pop); expect_equal(k$ref, s$rate); expect_true(all(k$pop < 30000))
  one <- z$rate_by_year("public_safety", types[1], "city", z$CITY_ID)                                             # fewer types: a lower rate
  expect_true(all(one$rate < s$rate)); expect_null(z$rate_by_year("public_safety", character(), "city", z$CITY_ID))
  # A register of another year stands in, and the rate says so (†); an area without a register has no rate of its own, the city's line stays
  a <- b$rate_by_year("public_safety", unique(b$base$call_type), "city", b$CITY_ID)
  expect_equal(a$mark, c("†", "†")); expect_equal(a$pop_year, c(2022L, 2022L)); expect_equal(a$pop_wanted, c(2024L, 2025L)); expect_equal(a$pop, c(870000, 870000))
  d <- b$rate_by_year("public_safety", unique(b$base$call_type), "district", b$geo_di$territory_id[1])
  expect_false(anyNA(d$rate)); expect_equal(d$ref, a$rate)
  n <- b$rate_by_year("public_safety", unique(b$base$call_type), "neighbourhood", b$geo_nb$territory_id[1])
  expect_true(all(is.na(n$rate))); expect_equal(n$ref, a$rate)
  # Under the threshold there is no rate (and with no rate at all, no chart); a city without a register has none either
  expect_null(set(z, list(THRESHOLD = 1e9), function(v) v$rate_by_year("public_safety", types, "city", v$CITY_ID)))
  expect_null(set(z, list(HAS_RATES = FALSE), function(v) v$rate_by_year("public_safety", types, "city", v$CITY_ID)))
  expect_null(set(b, list(MONTHS = months("2026-01-01", 9)), function(v) v$rate_by_year("public_safety", unique(v$base$call_type), "city", v$CITY_ID)))
  # A year in which the source lacks one of the types has no rate (missing is not zero); without that type, it has
  gaps <- data.frame(city = "zurich", block = "public_safety", event_type = types[1], first_month = as.Date("2024-01-01"), last_month = as.Date("2024-12-01"), reason = "cut_file", stringsAsFactors = FALSE)
  expect_equal(set(z, list(GAPS = gaps), function(v) v$gap_years("public_safety", types)), 2024L)
  expect_equal(set(z, list(GAPS = gaps), function(v) v$gap_years("public_safety", types[-1])), integer()); expect_equal(z$gap_years("public_safety", types), integer())
  g <- set(z, list(GAPS = gaps), function(v) v$rate_by_year("public_safety", types, "neighbourhood", nb))
  expect_true(is.na(g$rate[1]) && is.na(g$ref[1])); expect_equal(g$rate[2], k$rate[2]); expect_equal(attr(g, "lacking"), 2024L)
  expect_false(anyNA(set(z, list(GAPS = gaps), function(v) v$rate_by_year("public_safety", types[-1], "city", v$CITY_ID))$rate))
  # What the page holds under the chart by number: the place of the chart; nothing without a register; and, while no
  # year is complete, the title and a sentence in the place of the chart
  ui <- function(x) paste(as.character(x), collapse = "")
  expect_true(grepl('id="rate_block"', ui(z$rate_extra_ui("en")), fixed = TRUE)); expect_null(set(z, list(HAS_RATES = FALSE), function(v) v$rate_extra_ui("en")))
  none <- set(b, list(MONTHS = months("2026-01-01", 9)), function(v) ui(v$rate_extra_ui("es")))
  expect_true(grepl("Por 1.000 residentes, años completos", none, fixed = TRUE)); expect_true(grepl("Todavía no hay ningún año completo", none, fixed = TRUE)); expect_false(grepl("rate_block", none, fixed = TRUE))
  for (l in names(e$LANGUAGES)) for (key in c("rate_years_title", "col_residents", "rate_years_none")) expect_true(nzchar(e$TRANSLATIONS[[l]][[key]]), info = paste(l, key))
  expect_equal(e$tr("rate_years_title", "de"), e$tr("sp_evolution", "de")); expect_false(identical(e$tr("rate_years_title", "es"), e$tr("sp_evolution", "es")))   # residents where the language tells them from inhabitants

  # The words of the chart, shared with the countries
  txt <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = "")))
  head <- txt(e$rate_head_ui(k, k, "Kreis 1", "Zürich", "Public safety", "en", "Per 1,000 residents, full years", "Residents"))
  expect_true(grepl("Per 1,000 residents, full years", head, fixed = TRUE)); expect_true(grepl("Kreis 1 · Public safety · 2024 – 2025", head, fixed = TRUE))
  expect_true(grepl("Zürich", head, fixed = TRUE)); expect_true(grepl("Residents (Kreis 1): 21,000 (2024) → 21,500 (2025) ▲ +2.4", head, fixed = TRUE)); expect_false(grepl("†", head, fixed = TRUE))
  expect_false(grepl("NO REFERENCE", txt(e$rate_head_ui(s, s, "Zürich", "NO REFERENCE", "Public safety", "en", "T", "Residents")), fixed = TRUE))   # the city itself: no reference in the legend
  expect_true(grepl("† No population figure for 2024, 2025: the rate uses the nearest published one (2022).", txt(e$rate_head_ui(a, a, "Buenos Aires", "x", "Public safety", "en", "T", "Residents")), fixed = TRUE))
  hit <- data.frame(x = 1, label = "2025", rate = k$rate[2], ref = k$ref[2])
  note <- txt(e$rate_click_ui(hit, k, "Kreis 1", "Zürich", "en", "Residents"))
  expect_true(grepl(sprintf("2025 · Kreis 1: %s · Zürich: %s · Residents: 21,000 (2024) → 21,500 (2025)", e$fmt_r(k$rate[2], "en"), e$fmt_r(k$ref[2], "en")), note, fixed = TRUE))
  expect_null(e$rate_click_ui(NULL, k, "Kreis 1", "Zürich", "en", "Residents"))

  # On the page: the block, its head and its line follow the area, the types and the period of the bar
  labels <- z$month_labels("en")
  shiny::testServer(function(input, output, session) z$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(length(labels) - 1, length(labels))], neighbourhood = "", stats_level = "neighbourhood", shade = "rate", stats_zone = "city")
    session$elapse(400); session$setInputs(types = types); session$elapse(400)
    expect_true(grepl('id="rate_plot"', paste(as.character(output$rate_block$html), collapse = ""), fixed = TRUE))
    h1 <- txt(output$rate_head$html)
    expect_true(grepl("Per 1,000 residents, full years", h1, fixed = TRUE)); expect_true(grepl("Zürich · Public safety · 2024 – 2025", h1, fixed = TRUE))
    expect_true(grepl("Residents (Zürich): 318,000 (2024) → 324,000 (2025) ▲ +1.9", h1, fixed = TRUE))
    invisible(output$rate_plot); invisible(output$ts_main)
    session$setInputs(rate_click = list(x = 1.8, y = 1)); session$flushReact()                               # the second bar of two
    expect_true(grepl(sprintf("2025 · Zürich: %s · Residents: 318,000 (2024) → 324,000 (2025)", e$fmt_r(s$rate[2], "en")), txt(output$rate_note$html), fixed = TRUE))
    session$setInputs(ts_range = list(from = 1L, to = 1L, n = 2L)); session$flushReact()                      # the period bar: the first year alone
    expect_true(grepl("Yearly · 2024", txt(output$ts_title$html), fixed = TRUE)); expect_true(grepl("Zürich · Public safety · 2024", txt(output$rate_head$html), fixed = TRUE))
    expect_false(grepl("2025", sub("Per 1,000 residents, full years", "", txt(output$rate_head$html)), fixed = TRUE))
    session$setInputs(stats_zone = nb); session$elapse(400)                                                  # a neighbourhood: the city becomes the reference
    h2 <- txt(output$rate_head$html); nm <- unname(e$name_of[nb])
    expect_true(grepl(paste0(nm, " · Public safety · 2024"), h2, fixed = TRUE)); expect_true(grepl("&#9679; - +Zürich", h2))   # the legend: the area, and the city as a dashed line
    session$setInputs(types = character(0)); session$elapse(400)                                             # no type ticked: no chart by rate
    expect_true(is.null(output$rate_block$html) || !nzchar(paste(as.character(output$rate_block$html), collapse = "")))
  })
})

test_that("the figures of a city's statistics page as an Excel file: what the page shows, nothing under the threshold, a note beside every empty cell, names Excel accepts", {
  skip_if_not_installed("writexl"); skip_if_not_installed("readxl")
  e <- app_env(); b <- e$get_view("buenosaires"); z <- e$get_view("zurich")
  set <- function(v, values, f) { old <- mget(names(values), envir = v); on.exit(list2env(old, envir = v)); list2env(values, envir = v); f(v) }
  read <- function(f) { n <- readxl::excel_sheets(f); stats::setNames(lapply(n, function(s) as.data.frame(readxl::read_excel(f, sheet = s, .name_repair = "minimal"))), n) }
  # Sheet names: none of the characters Excel refuses, 31 at most, no two alike whatever their capitals, never empty
  given <- c("By type", "Time of year: 2019/2025 [all]?", "data", "Data", "DATA", "", NA, "'quoted'", strrep("a", 40), strrep("a", 40), "A*B\\C")
  expect_equal(e$xl_sheet_names(given), c("By type", "Time of year 2019 2025 all", "data", "Data (2)", "DATA (3)", "Sheet", "Sheet (2)", "quoted", strrep("a", 31), paste0(strrep("a", 27), " (2)"), "A B C"))
  expect_equal(e$xl_sheet_names(e$xl_sheet_names(given)), e$xl_sheet_names(given)); expect_equal(e$xl_sheet_names(character()), character())
  # The page has the button under its bar; a country's page and About have none; without the package that writes the file, no page has it
  for (v in c("buenosaires", "zurich", "toronto")) { el <- html_elements(render(e, v, "en")); i <- match("xl_stats", el$id); expect_equal(el$tag[i], "a", info = v); expect_equal(el$class[match(el$parent[i], el$row)], "stats-tools", info = v) }
  expect_false(grepl('id="xl_stats"', render(e, "XT", "en"), fixed = TRUE)); expect_false(grepl('id="xl_stats"', render(e, "about", "en"), fixed = TRUE))
  expect_true(e$HAS_XLSX); expect_false(set(e, list(HAS_XLSX = FALSE), function(x) grepl('id="xl_stats"', render(x, "buenosaires", "en"), fixed = TRUE)))
  expect_true(grepl("Download these figures (Excel)", render(e, "buenosaires", "en"), fixed = TRUE)); expect_true(grepl("Descargar estas cifras (Excel)", render(e, "buenosaires", "es"), fixed = TRUE))

  # Buenos Aires (months of 2024 and 2025, a register of 2022, no residents by neighbourhood): the whole city, the last three months
  labels <- b$month_labels("en"); types <- unique(b$base$call_type); nb <- b$geo_nb$territory_id[1]; di <- b$geo_di$territory_id[1]
  rows <- b$base[b$base$month >= as.Date("2025-10-01"), ]
  shiny::testServer(function(input, output, session) b$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(length(labels) - 2, length(labels))], neighbourhood = "", stats_level = "district", sort = "rate", shade = "rate", stats_zone = "city")
    session$elapse(400); session$setInputs(types = types); session$elapse(400)
    f <- tempfile(fileext = ".xlsx"); session$userData$xl_stats(f); w <- read(f)
    expect_equal(names(w), c("About this file", "By type", "Time series", "Per 1,000 res.", "Time of year", "Top 5 comunas"))
    # By type: the figures of the chart, as numbers
    by_type <- tapply(rows$events, rows$call_type, sum)
    expect_equal(names(w[["By type"]]), c("Type of offence", "Offences", "Note")); expect_true(is.numeric(w[["By type"]]$Offences))
    expect_equal(sort(w[["By type"]]$Offences), sort(as.numeric(by_type))); expect_true(all(is.na(w[["By type"]]$Note)))
    expect_setequal(w[["By type"]][[1]], unname(vapply(names(by_type), b$type_name, "", "en")))
    # Over time: every month of the series (the period bar shows them all), with its first day as a date
    by_month <- tapply(b$base$events, b$base$month, sum)
    ts <- w[["Time series"]]; expect_equal(names(ts), c("Period", "First day", "Offences", "Note")); expect_equal(nrow(ts), 24L)
    expect_equal(as.Date(ts[["First day"]]), as.Date(names(by_month))); expect_equal(ts$Offences, as.numeric(by_month)); expect_equal(ts$Period[c(1, 24)], c("Jan 2024", "Dec 2025"))
    # The rate of each complete year, on the register there is (2022), and that is said beside it
    rt <- w[["Per 1,000 res."]]; a <- b$rate_by_year("public_safety", types, "city", b$CITY_ID)
    expect_equal(names(rt), c("Period", "Per 1,000 res.", "Residents", "Note")); expect_equal(rt$Period, c("2024", "2025")); expect_equal(rt[[2]], round(a$rate, 2)); expect_equal(rt$Residents, c(870000, 870000))
    expect_true(all(grepl("2022", rt$Note, fixed = TRUE)) && all(startsWith(rt$Note, "†")))
    # The time of year: the average of each calendar month over the two complete years
    sp <- e$season_profile(b$base$month, b$base$events); toy <- w[["Time of year"]]
    expect_equal(names(toy), c("Period", "Offences · Average", "Note")); expect_equal(toy$Period[c(1, 12)], c("January", "December")); expect_equal(toy[[2]], round(sp$avg$n, 1))
    # The ranking: the rows of the page's table, with their rate and how many times the city's
    top <- w[["Top 5 comunas"]]; expect_equal(names(top), c("#", "Comuna", "Offences", "Per 1,000 res.", "× city")); expect_equal(top[["#"]], seq_len(nrow(top)))
    expect_false(is.unsorted(rev(top[["Per 1,000 res."]]))); expect_true(all(top$Offences >= b$THRESHOLD)); expect_true(all(top$Comuna %in% unname(e$name_of[b$geo_di$territory_id])))
    # The first sheet: the choices of the page, what the figures are, the citation of the source and what each sheet holds
    about <- w[["About this file"]]; said <- paste(about[[1]], collapse = "\n"); expect_equal(names(about), "WebarchyMapp · Buenos Aires · Statistics")       # one column of sentences
    expect_equal(about[[1]][1:4], c("Area: Buenos Aires", "Category: Public safety", "Type of offence: All (3)", "Period: Oct 2025 – Dec 2025"))
    for (k in c("File made on ", "An empty cell is not a zero", "figure under 10", "cite the original source", "Licence ", "Period shown: Oct 2025 – Dec 2025.", "counts below 10 are not shown",
                "Time series · Buenos Aires · Public safety · Monthly · Jan 2024 – Dec 2025", "Top 5 comunas · Public safety · Oct 2025 – Dec 2025")) expect_true(grepl(k, said, fixed = TRUE), info = k)
    expect_true(grepl(b$meta$publisher, said, fixed = TRUE)); expect_true(all(startsWith(utils::tail(about[[1]], 5), paste0(names(w)[-1], ": "))))
    # The test session's own download gives the same file (where the version of shiny hands it over as a path)
    out <- tryCatch(output$xl_stats, error = function(err) NULL)
    if (is.character(out) && length(out) == 1 && file.exists(out)) expect_equal(readxl::excel_sheets(out), names(w)) else expect_true(is.function(session$userData$xl_stats))

    # The series seen by year, and the period bar on its first year alone: the sheet holds the bar in view, as the chart
    session$setInputs(ts_gran = "year"); session$elapse(400); session$setInputs(ts_range = list(from = 1L, to = 1L, n = 2L)); session$flushReact()
    session$userData$xl_stats(f); w <- read(f)
    expect_equal(w[["Time series"]]$Period, "2024"); expect_equal(w[["Time series"]]$Offences, sum(b$base$events[format(b$base$month, "%Y") == "2024"]))
    expect_equal(w[["Per 1,000 res."]]$Period, "2024"); expect_true(grepl("Yearly · 2024", paste(w[[1]][[1]], collapse = "\n"), fixed = TRUE))
    session$setInputs(ts_range = list(from = 1L, to = 2L, n = 2L)); session$flushReact(); session$setInputs(ts_gran = "month"); session$elapse(400)   # both years again (the bar is told while it still has two bars), then by month

    # A district: its own rate and, beside it, the city's. A neighbourhood without residents: no rate of its own, the city's stays
    session$setInputs(stats_zone = di); session$elapse(400); session$userData$xl_stats(f); w <- read(f)
    expect_equal(names(w[["Per 1,000 res."]]), c("Period", "Per 1,000 res.", "Buenos Aires · Per 1,000 res.", "Residents", "Note")); expect_equal(w[["Per 1,000 res."]][[3]], round(a$rate, 2))
    expect_equal(w[[1]][[1]][1], paste0("Area: ", unname(e$name_of[di]), " (Comuna)"))
    session$setInputs(stats_zone = nb); session$elapse(400); session$userData$xl_stats(f); w <- read(f)
    expect_true(all(is.na(w[["Per 1,000 res."]][[2]])) && all(is.na(w[["Per 1,000 res."]]$Residents))); expect_equal(w[["Per 1,000 res."]][[3]], round(a$rate, 2))
    mine <- b$base[b$base$neighbourhood_id %in% nb, ]; expect_equal(w[["Time series"]]$Offences, as.numeric(tapply(mine$events, mine$month, sum)))

    # One type alone on the chart over time (a bar of the chart by type clicked): the series is that type's, and the first sheet says so
    session$setInputs(stats_zone = "city"); session$elapse(400); invisible(output$types_plot)
    session$setInputs(types_click = list(x = 1, y = 1e6)); session$flushReact(); session$userData$xl_stats(f); w <- read(f)   # the top bar of the chart
    one <- sub("^Time series: (.*) only$", "\\1", grep("^Time series: .* only$", w[[1]][[1]], value = TRUE)); expect_length(one, 1L)
    kept <- names(by_type)[vapply(names(by_type), b$type_name, "", "en") == one]; expect_length(kept, 1L)
    expect_equal(w[["Time series"]]$Offences, as.numeric(tapply(b$base$events[b$base$call_type == kept], b$base$month[b$base$call_type == kept], sum)))
    expect_equal(nrow(w[["By type"]]), 3L)                                       # the chart by type keeps its three bars

    # No type ticked: a file with its first sheet alone, which says so
    session$setInputs(types = character(0)); session$elapse(400); session$userData$xl_stats(f); w <- read(f)
    expect_equal(names(w), "About this file"); expect_true(grepl("No offence type selected", paste(w[[1]][[1]], collapse = "\n"), fixed = TRUE))
  })

  # Under the threshold: an empty cell and its note, never the count. A neighbourhood's months are 33 offences; with a threshold of 40 none is shown
  count_cells <- function(w) unlist(lapply(w[-1], function(d) unlist(d[vapply(names(d), function(n) n %in% c("Offences", "Offences · Average"), TRUE)])))
  was <- e$MARTS$threshold; e$MARTS$threshold <- 40; on.exit(e$MARTS$threshold <- was, add = TRUE)
  set(b, list(THRESHOLD = 40), function(v) shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(length(labels), length(labels))], neighbourhood = "", stats_level = "neighbourhood", sort = "n", shade = "number", stats_zone = nb, ts_gran = "month")
    session$elapse(400); session$setInputs(types = types); session$elapse(400)
    f <- tempfile(fileext = ".xlsx"); session$userData$xl_stats(f); w <- read(f)
    expect_true(all(is.na(w[["By type"]]$Offences))); expect_equal(w[["By type"]]$Note, rep("<40", 3)); expect_equal(nrow(w[["By type"]]), 3L)       # 11 of each type in the month
    expect_true(all(is.na(w[["Time series"]]$Offences))); expect_equal(w[["Time series"]]$Note, rep("<40", 24))                                         # 33 in each month
    cells <- count_cells(w); expect_true(length(cells) > 30); expect_true(all(is.na(cells) | cells >= 40))                                             # no sheet holds a count under the threshold
    expect_false(any(unlist(lapply(w, names)) %in% c("raw", "events")))
    expect_true(grepl("figure under 40", paste(w[[1]][[1]], collapse = "\n"), fixed = TRUE))
  }))
  e$MARTS$threshold <- was
  # The time of year stands on the whole city: a month whose average rests on fewer events than the threshold is left empty in the sheet
  x <- list(zone = list(level = "city", id = b$CITY_ID, name = "Buenos Aires"), block = "public_safety", types = types, all_ticked = TRUE, selection = "Public safety", range = as.Date(c("2025-10-01", "2025-12-01")),
            season = list(years = 2024:2025, avg = data.frame(mo = 1:12, n = c(4, 4.5, 5, 30, rep(100, 8))), total = 0), cite = list(publisher = "P", title = "T"), made = as.POSIXct("2026-11-17 08:30:00", tz = "UTC"))
  toy <- b$city_workbook(x, "en")[["Time of year"]]
  late <- b$city_workbook(c(x[names(x) != "made"], list(made = as.POSIXct("2026-11-17 00:30:00", tz = "Europe/Madrid"))), "en")[[1]][[1]]
  expect_true(any(grepl("File made on 2026-11-17 00:30 ", late, fixed = TRUE)))                                                                       # half past midnight in Madrid is still the 16th in Greenwich
  expect_equal(toy[[2]], c(NA, NA, 5, 30, rep(100, 8))); expect_equal(toy$Note, c("<10", "<10", rep("", 10)))                                          # 8 and 9 events in two years; 10 is shown
  # A year the source lacks (a file cut by its publisher) has no rate, and the sheet says why; the bars of that year say it too
  gaps <- data.frame(city = "buenosaires", block = "public_safety", event_type = types[1], first_month = as.Date("2024-01-01"), last_month = as.Date("2024-12-01"), reason = "cut_file", stringsAsFactors = FALSE)
  g <- set(b, list(GAPS = gaps), function(v) { r <- v$rate_by_year("public_safety", types, "city", v$CITY_ID)
    s <- data.frame(start = as.Date(c("2024-01-01", "2025-01-01")), end = as.Date(c("2024-12-01", "2025-12-01")), label = c("2024", "2025"), n = c(2928, 2928), complete = c(TRUE, FALSE)); attr(s, "gran") <- "year"
    v$city_workbook(c(x[names(x) != "season"], list(series = s, series_what = "Buenos Aires · Public safety", rate = r, lacking = attr(r, "lacking"))), "en") })
  expect_true(is.na(g[["Per 1,000 res."]][[2]][1]) && !is.na(g[["Per 1,000 res."]][[2]][2])); expect_true(nchar(g[["Per 1,000 res."]]$Note[1]) > 60 && grepl("2024", g[["Per 1,000 res."]]$Note[1], fixed = TRUE))
  expect_true(grepl("2024", g[["Time series"]]$Note[1], fixed = TRUE)); expect_equal(g[["Time series"]]$Note[2], "incomplete period"); expect_equal(names(g), c("About this file", "Time series", "Per 1,000 res."))

  # Zürich (a yearly source, residents by neighbourhood, registers of 2024 and 2025): by year, no time of year, the neighbourhoods ranked
  zl <- z$month_labels("en"); zt <- unique(z$base$call_type[z$base$block == "public_safety"])
  shiny::testServer(function(input, output, session) z$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = zl[c(length(zl), length(zl))], neighbourhood = "", stats_level = "neighbourhood", sort = "n", shade = "rate", stats_zone = "city")
    session$elapse(400); session$setInputs(types = zt); session$elapse(400)
    f <- tempfile(fileext = ".xlsx"); session$userData$xl_stats(f); w <- read(f)
    expect_false("Time of year" %in% names(w)); expect_equal(w[["Time series"]]$Period, c("2024", "2025")); expect_true(all(is.na(w[["Per 1,000 res."]]$Note)))
    top <- w[[length(w)]]; expect_equal(names(top)[1:3], c("#", "Stadtkreis", "Offences")); expect_equal(names(w)[length(w)], "Top 10 Stadtkreise"); expect_false(is.unsorted(rev(top$Offences))); expect_true(nrow(top) >= 2 && nrow(top) <= 10)
  })
  # In every language the file can be written and its sheets have names Excel accepts (the same number of sheets, none over 31 characters)
  s <- data.frame(start = as.Date(c("2024-01-01", "2025-01-01")), end = as.Date(c("2024-12-01", "2025-12-01")), label = c("2024", "2025"), n = c(2928, NA), complete = c(TRUE, TRUE)); attr(s, "gran") <- "year"
  full <- c(x, list(bars = data.frame(type = types, n = c(243, NA, 12), label = types, stringsAsFactors = FALSE), series = s, series_what = "Buenos Aires · Public safety",
                    rate = b$rate_by_year("public_safety", types, "city", b$CITY_ID), top = list(districts = data.frame(name = c("A", "B"), n = c(351, 135), rate = c(1.95, 0.61)), city = list(n = 729, rate = 0.84), by = "rate", level = "district")))
  bad <- character(); f <- tempfile(fileext = ".xlsx")
  for (l in names(e$LANGUAGES)) { wb <- b$city_workbook(full, l); nm <- e$xl_sheet_names(names(wb))
    if (length(wb) != 6 || !identical(nm, names(wb)) || any(nchar(nm) > 31) || anyDuplicated(tolower(nm)) || any(grepl("[][\\\\/?*:]", nm))) bad <- c(bad, paste0(l, ": ", paste(names(wb), collapse = " | ")))
    got <- tryCatch(readxl::excel_sheets(e$xl_write(wb, f)), error = function(err) conditionMessage(err))             # written, and read again with its six sheets
    if (length(got) != 6 || any(nchar(got) > 31) || anyDuplicated(tolower(got))) bad <- c(bad, paste0(l, ": not written as given: ", paste(got, collapse = " | "))) }
  expect_equal(bad, character())
  expect_equal(names(b$city_workbook(full, "fi"))[c(4, 6)], c("1\u00a0000 as.", "Top 5 comunat"))                    # "/ 1 000 as." and "Top 5: comunat": the slash and the colon are taken out
  expect_equal(set(b, list(CITY = "a-city-with-no-words-of-its-own"), function(v) names(v$city_workbook(full, "el"))[6]), "Δημοτικά διαμερίσματα")   # the Greek title has 32 letters: the tab takes the name of the level
})

test_that("the table of countries is gone: no view, no link in the top bar, no text of its own, and the update no longer builds it", {
  e <- app_env()
  expect_false("countries" %in% e$VIEWS_AVAILABLE); expect_equal(e$fallback_view("countries"), e$VIEWS_AVAILABLE[1])
  expect_false(exists("ui_countries", envir = e, inherits = FALSE)); expect_false(exists("CRATES", envir = e, inherits = FALSE))
  for (view in c("XT", "zurich", "about")) {
    h <- render(e, view, "en")
    expect_false(grepl("view=countries", h, fixed = TRUE), info = view); expect_false(grepl("Countries (table)", h, fixed = TRUE), info = view)
  }
  expect_false(any(grepl("^countries_|^nav_countries$|^cr_open$|^col_rate100k$", names(e$TRANSLATIONS$en))))
  expect_false(grepl("Eurostat", e$tr("about_why_countries", "en"), fixed = TRUE)); expect_true(grepl("never compare two countries.", e$tr("about_why_countries", "en"), fixed = TRUE))
  expect_false(exists("eurostat_rates_update", mode = "function")); expect_true(exists("jsonstat_to_df", mode = "function"))   # the reader of JSON-stat stays: three modules use it
  # A table left by an earlier update goes with the next build of the app tables; nothing else of the database is lost
  n <- db_query("SELECT count(*) AS n FROM mart_city_events")$n
  quiet(with_db(function(con) { DBI::dbExecute(con, "CREATE OR REPLACE TABLE mart_country_rates AS SELECT 'ES' AS country, 1.5 AS rate"); build_marts(con) }))
  expect_false("mart_country_rates" %in% db_query("SELECT table_name FROM information_schema.tables")$table_name)
  expect_equal(db_query("SELECT count(*) AS n FROM mart_city_events")$n, n)
})

test_that("the areas that border one: found by their outlines, shown with the figures of the same source, types and period, in cities and in countries", {
  e <- app_env()
  # A 3 x 3 grid of squares that share their sides (0.02 degrees, about 2 km), one square 30 m away from the grid (a
  # sliver, as boundaries simplified for a map leave), one 600 m away, and an island far off
  grid <- expand.grid(i = 0:2, j = 0:2)
  polys <- c(lapply(seq_len(nrow(grid)), function(k) square(2 + 0.02 * grid$i[k], 40 + 0.02 * grid$j[k])),
             list(square(2.06 + 30 / 85000, 40), square(2.06 + 600 / 85000, 40.04), square(5, 45)))
  ids <- c(sprintf("G%d%d", grid$i, grid$j), "SLIVER", "FAR", "ISLAND")
  g <- sf::st_sf(territory_id = ids, geometry = sf::st_sfc(polys, crs = 4326)); bx <- e$polygon_boxes(g)
  expect_setequal(e$bordering(g, bx, "G11"), setdiff(sprintf("G%d%d", grid$i, grid$j), "G11"))     # the middle square: the eight around it (a shared corner is a border too)
  expect_setequal(e$bordering(g, bx, "G00"), c("G10", "G01", "G11"))
  expect_setequal(e$bordering(g, bx, "G20"), c("G10", "G11", "G21", "SLIVER"))                     # 30 m apart: neighbours all the same
  expect_setequal(e$bordering(g, bx, "SLIVER"), c("G20", "G21")); expect_false("FAR" %in% e$bordering(g, bx, "G22"))
  expect_equal(e$bordering(g, bx, "ISLAND"), character()); expect_true("FAR" %in% e$bordering(g, bx, "G22", tol = 1000))
  expect_equal(e$bordering(g, bx, "NOT THERE"), character()); expect_equal(e$bordering(NULL, bx, "G11"), character()); expect_equal(e$bordering(g, NULL, "G11"), character())
  # The same answer whichever way the question is asked: on the sphere, on a plane centred on the area, in degrees
  for (eng in c("sphere", "plane", "degrees")) {
    expect_setequal(e$bordering(g, bx, "G20", engines = eng), c("G10", "G11", "G21", "SLIVER")); expect_setequal(e$bordering(g, bx, "G00", engines = eng), c("G10", "G01", "G11"))
    expect_equal(e$bordering(g, bx, "ISLAND", engines = eng), character()); expect_false("FAR" %in% e$bordering(g, bx, "G22", engines = eng))
  }
  expect_setequal(e$bordering(sf::st_set_crs(g, NA), bx, "G20"), c("G10", "G11", "G21", "SLIVER"))     # a layer that lost its reference system: the boxes are in degrees, so are the outlines
  # An outline the sphere refuses (a ring that crosses itself, 30 m west of the grid) leaves nobody without neighbours:
  # the question goes to the plane, with what sf brings (no other package), for the area itself and for the ones beside it
  bow <- sf::st_polygon(list(matrix(c(1.98 - 30 / 85000, 40, 2 - 30 / 85000, 40.02, 2 - 30 / 85000, 40, 1.98 - 30 / 85000, 40.02, 1.98 - 30 / 85000, 40), ncol = 2, byrow = TRUE)))
  g2 <- sf::st_sf(territory_id = c(ids, "BOW"), geometry = sf::st_sfc(c(polys, list(bow)), crs = 4326)); bx2 <- e$polygon_boxes(g2)
  expect_setequal(e$bordering(g2, bx2, "G00"), c("G10", "G01", "G11", "BOW")); expect_setequal(e$bordering(g2, bx2, "BOW"), c("G00", "G01"))
  expect_setequal(e$bordering(g2, bx2, "G00", engines = "plane"), c("G10", "G01", "G11", "BOW")); expect_setequal(e$bordering(g2, bx2, "G22"), c("G11", "G12", "G21"))
  expect_true(sf::sf_use_s2())                                                                   # and the sphere is left as it was
  # One tolerance whatever the size of the areas, so that when A borders B, B borders A, and so that two large areas
  # that stand a kilometre apart are not neighbours: three squares of one degree that touch, a small one 30 m from the
  # first, a large one 1 km from the second, and one far away
  mixed <- sf::st_sf(territory_id = c("B1", "B2", "B3", "SMALL", "KM", "ALONE"),
                     geometry = sf::st_sfc(list(square(10, 50, 1), square(11, 50, 1), square(10, 51, 1), square(10 - 0.02 - 30 / 71500, 50.5), square(12 + 1000 / 71500, 50, 1), square(20, 60, 1)), crs = 4326))
  mb <- e$polygon_boxes(mixed); nb <- lapply(setNames(mixed$territory_id, mixed$territory_id), function(id) e$bordering(mixed, mb, id))
  expect_setequal(nb$B1, c("B2", "B3", "SMALL")); expect_setequal(nb$B2, c("B1", "B3")); expect_equal(nb$SMALL, "B1"); expect_equal(nb$KM, character()); expect_equal(nb$ALONE, character())
  for (a in names(nb)) for (b in nb[[a]]) expect_true(a %in% nb[[b]], info = paste(a, b))
  expect_true("KM" %in% e$bordering(mixed, mb, "B2", tol = 1500))

  # The block: the chosen area among its neighbours from the highest to the lowest rate, its row marked, the others links
  txt <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = "")))
  rows <- data.frame(territory_id = c("A", "B", "C", "D"), name = c("Alpha", "Beta", "Gamma", "Delta"), n = c(70, 176, NA, 48), rate = c(2.2, 7.2, NA, 1.3), stringsAsFactors = FALSE)
  n_text <- function(n) if (is.na(n)) "fewer than 10" else format(n)
  ui <- e$near_ui(rows, "A", "en", n_col = "Calls", rate_col = "Per 1,000 res.", n_text = n_text, click = "near_pick", missing = 2)
  h <- paste(as.character(ui), collapse = ""); t1 <- txt(ui)
  expect_true(grepl("Bordering areas (5)", t1, fixed = TRUE))                                     # three listed and two the source says nothing about
  expect_true(grepl("Per 1,000 res. in the bordering areas: 1.3 – 7.2 (median 4.2)", t1, fixed = TRUE))
  expect_true(regexpr("Beta", t1) < regexpr("Alpha", t1) && regexpr("Alpha", t1) < regexpr("Delta", t1) && regexpr("Delta", t1) < regexpr("Gamma", t1))   # by rate; the one without a rate last
  expect_true(grepl('<tr class="sel">\\s*<td data-v="alpha">Alpha</td>', h)); expect_true(grepl('data-wm-send="near_pick" data-wm-value="B"', h, fixed = TRUE)); expect_false(grepl('data-wm-value="A"', h, fixed = TRUE))
  expect_true(grepl("fewer than 10", t1, fixed = TRUE)); expect_true(grepl("Bordering areas with no figure in this source: 2", t1, fixed = TRUE))
  counts <- txt(e$near_ui(rows, "A", "es", n_col = "Avisos", rate_col = NULL, n_text = n_text, click = "near_pick"))    # a city without residents by neighbourhood: by number
  expect_true(grepl("Áreas colindantes (3)", counts, fixed = TRUE)); expect_true(grepl("Avisos en las áreas colindantes: 48 – 176 (mediana 112)", counts, fixed = TRUE)); expect_false(grepl("Por 1.000", counts, fixed = TRUE))
  expect_null(e$near_ui(rows[1, ], "A", "en", n_col = "Calls", n_text = n_text, click = "near_pick"))                  # nothing borders it: no block
  expect_false(grepl("median", txt(e$near_ui(rows[1:2, ], "A", "en", n_col = "Calls", rate_col = "Per 1,000 res.", n_text = n_text, click = "near_pick")), fixed = TRUE))   # one neighbour: no range
  for (l in names(e$LANGUAGES)) for (key in c("near_title", "near_help", "near_range", "near_more")) expect_true(nzchar(e$TRANSLATIONS[[l]][[key]]), info = paste(l, key))

  # A city: the test neighbourhoods stand 400 m apart (no neighbour); moved side by side, each has its own
  z <- e$get_view("zurich"); set <- function(v, values, f) { old <- if (length(values)) mget(names(values), envir = v) else list(); on.exit(list2env(old, envir = v)); list2env(values, envir = v); f(v) }
  types <- unique(z$base$call_type[z$base$block == "public_safety"]); d <- z$compute("public_safety", range(z$MONTHS), types)
  expect_null(z$near_block(z$geo_nb$territory_id[1], d, "en"))
  side <- z$geo_nb; sf::st_geometry(side) <- sf::st_sfc(lapply(seq_len(nrow(side)), function(k) square(8.5 + 0.02 * ((k - 1) %% 4), 47.35 + 0.02 * ((k - 1) %/% 4))), crs = 4326)
  first <- side$territory_id[1]; around <- side$territory_id[c(2, 5, 6)]                                               # a corner of a grid four wide
  blk <- set(z, list(geo_nb = side, NB_BOXES = e$polygon_boxes(side)), function(v) v$near_block(first, d, "en"))
  hb <- paste(as.character(blk), collapse = ""); tb <- txt(blk)
  expect_true(grepl("Bordering areas (3)", tb, fixed = TRUE)); expect_true(grepl("Per 1,000 res. in the bordering areas:", tb, fixed = TRUE))
  for (id in around) { expect_true(grepl(sprintf('data-wm-value="%s"', id), hb, fixed = TRUE), info = id); expect_true(grepl(unname(e$name_of[id]), tb, fixed = TRUE), info = id) }
  mine <- d$neighbourhoods[d$neighbourhoods$territory_id == first, ]
  expect_true(grepl(sprintf("%s %s %s", unname(e$name_of[first]), e$fmt_n(mine$n, "en"), e$fmt_r(mine$rate, "en")), tb, fixed = TRUE))     # the same figures as its profile
  labels <- z$month_labels("en")
  set(z, list(geo_nb = side, NB_BOXES = e$polygon_boxes(side)), function(v) shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(length(labels) - 1, length(labels))], neighbourhood = first, stats_level = "neighbourhood", shade = "rate")
    session$elapse(400); session$setInputs(types = types); session$elapse(400)
    prof <- paste(as.character(output$profile$html), collapse = "")
    expect_true(grepl("Bordering areas", prof, fixed = TRUE)); expect_true(grepl(sprintf('data-wm-send="near_pick" data-wm-value="%s"', around[1]), prof, fixed = TRUE))
    session$setInputs(near_pick = list(value = around[1], at = 1)); session$flushReact()                                 # a click on a neighbour: nothing breaks (the browser then opens it)
    session$setInputs(near_pick = list(value = "not-a-neighbourhood", at = 2)); session$flushReact()
  }))

  # A country: the territories of the level of the map, read from the whole level; the test country has no boundaries (no block), and with two regions side by side it has one
  v <- e$get_view("XT"); per <- names(v$cb_periods("XT", "en", v$CB))
  regions <- sf::st_sf(territory_id = c("XT-R1", "XT-R2"), geometry = sf::st_sfc(list(square(10, 50, 1), square(11, 50, 1)), crs = 4326))
  run <- function(values) set(v, values, function(v) { out <- NULL
    shiny::testServer(function(input, output, session) v$server_country(input, output, session, "en"), {
      session$setInputs(sp_level = "region", sp_period = rep(per[1], 2), sp_territory = "XT-R2"); session$flushReact(); session$setInputs(sp_types = "a"); session$flushReact()
      out <<- paste(as.character(output$sp_profile$html), collapse = "")
      session$setInputs(near_pick = list(value = "XT-R1", at = 1)); session$flushReact() })
    out })
  expect_false(grepl("Bordering areas", run(list()), fixed = TRUE))
  with_map <- run(list(GEO_REGION = regions, BOXES = list(region = e$polygon_boxes(regions))))
  tw <- txt(with_map)
  expect_true(grepl("Bordering areas (1)", tw, fixed = TRUE)); expect_true(grepl('data-wm-send="near_pick" data-wm-value="XT-R1"', with_map, fixed = TRUE))
  expect_true(grepl("Region One 64 0.32", tw, fixed = TRUE)); expect_true(grepl("Region Two 18 0.18", tw, fixed = TRUE))   # 64 / 200,000 and 18 / 100,000, as in the table of the page
  expect_true(regexpr("Region One 64", tw) < regexpr("Region Two 18", tw))
})


# Every table in the order of a column (P-067) --------------------------------------------------------------------
# The tables of a piece of HTML that the browser orders, as the browser reads them: the titles (the column each one
# names, what it holds, the order the table arrives in) and the rows (a row that keeps its place, the value each cell
# is ordered by and the text it shows). The text of a hover note is not the text of a cell.
sortable_tables <- function(x) {
  h <- gsub("(?s)<template\\b.*?</template>", "", paste(as.character(x), collapse = ""), perl = TRUE)
  at <- function(tag, a) { m <- regmatches(tag, regexec(sprintf("\\s%s=\"([^\"]*)\"", a), tag))[[1]]; if (length(m) == 2) m[2] else NA_character_ }
  open <- function(s, tag) regmatches(s, regexpr(sprintf("^<%s\\b[^>]*>", tag), s))
  each <- function(s, tag) regmatches(s, gregexpr(sprintf("(?s)<%s\\b[^>]*>.*?</%s>", tag, tag), s, perl = TRUE))[[1]]
  txt <- function(s) trimws(gsub("\\s+", " ", gsub("&nbsp;", " ", gsub("<[^>]+>", " ", s))))
  col <- function(ths, a) vapply(ths, function(s) at(open(s, "th"), a), "", USE.NAMES = FALSE)
  out <- lapply(each(h, "table"), function(tb) {
    ths <- each(tb, "th"); body <- regmatches(tb, regexpr("(?s)<tbody\\b.*</tbody>", tb, perl = TRUE)); trs <- if (length(body)) each(body, "tr") else character()
    list(class = at(open(tb, "table"), "class"), id = at(open(tb, "table"), "data-sort-id"), body = if (length(body)) body else "",
         head = data.frame(key = col(ths, "data-sort-key"), type = col(ths, "data-sort-type"), first = col(ths, "data-sort-first"), aria = col(ths, "aria-sort"),
                           ask = col(ths, "data-sort-ask"), ask_value = col(ths, "data-sort-ask-value"),
                           link = vapply(ths, function(s) grepl('<a class="sort" role="button" tabindex="0" title="[^"]+">', s), TRUE, USE.NAMES = FALSE),
                           mark = vapply(ths, function(s) grepl('class="sort-mark"', s, fixed = TRUE), TRUE, USE.NAMES = FALSE), label = vapply(ths, txt, "", USE.NAMES = FALSE), stringsAsFactors = FALSE),
         rows = lapply(trs, function(tr) { tds <- each(tr, "td")
           list(pin = at(open(tr, "tr"), "data-pin"), class = at(open(tr, "tr"), "class"), v = vapply(tds, function(s) at(open(s, "td"), "data-v"), "", USE.NAMES = FALSE), text = vapply(tds, txt, "", USE.NAMES = FALSE)) }))
  })
  out[vapply(out, function(tb) !is.na(tb$class) && grepl("\\bsortable\\b", tb$class), TRUE)]
}
# The rows that move, in the order the browser gives them for a column and a way (app_styles.R): a cell without a
# value last whichever way, equal values by the first column of text, from A. Written here a second time, on purpose.
browser_order <- function(tb, key, desc) {
  k <- match(key, tb$head$key); names_at <- match("text", tb$head$type)
  free <- which(vapply(tb$rows, function(r) is.na(r$pin), TRUE)); if (!length(free)) return(free)
  v <- vapply(tb$rows[free], function(r) r$v[k], ""); nm <- vapply(tb$rows[free], function(r) if (is.na(names_at)) "" else r$v[names_at], "")
  o <- if (tb$head$type[k] == "num") order(!nzchar(v), if (desc) -as.numeric(v) else as.numeric(v), nm, method = "radix")
       else order(!nzchar(v), v, nm, decreasing = c(FALSE, desc, FALSE), method = "radix")
  free[o]
}
# A table arrives in the order its marked title says (TRUE), or in an order no title claims (NA)
arrives_as_marked <- function(tb) {
  k <- which(!is.na(tb$head$aria)); if (length(k) != 1) return(NA)
  identical(browser_order(tb, tb$head$key[k], tb$head$aria[k] == "descending"), which(vapply(tb$rows, function(r) is.na(r$pin), TRUE)))
}
column <- function(tb, key, what = "v") vapply(tb$rows, function(r) r[[what]][match(key, tb$head$key)], "")

test_that("every table can be put in the order of any of its columns: titles that order, a value under each cell and none under a figure that is not shown, a total that keeps its place, an order the table really arrives in", {
  e <- app_env()
  set <- function(v, values, f) { old <- mget(names(values), envir = v); on.exit(list2env(old, envir = v)); list2env(values, envir = v); f(v) }
  seen <- list(); keep <- function(tb, threshold = 10) { seen[[length(seen) + 1]] <<- c(tb, list(threshold = threshold)); tb }

  # The pieces. The value a cell is ordered by: nothing for what is not shown, a plain number, a name without accents
  expect_equal(c(e$sort_value(NA), e$sort_value(NULL), e$sort_value(numeric()), e$sort_value(NA_character_), e$sort_value(NaN), e$sort_value(Inf)), rep("", 6))
  expect_equal(c(e$sort_value(12), e$sort_value(0.1 + 0.2), e$sort_value(1234567.891), e$sort_value(2.5e10), e$sort_value(0)), c("12", "0.3", "1234567.891", "25000000000", "0"))
  expect_equal(c(e$sort_value("Ávila"), e$sort_value("San Sebastián de los Reyes"), e$sort_value("Straße")), c("avila", "san sebastian de los reyes", "strasse"))
  # The title of a column: a link like the one of the countries' table, marked when the table arrives in its order
  th <- as.character(e$sort_th("Calls", "n", "en", order = "descending", extra = htmltools::span(class = "x", "i")))
  expect_true(grepl('<th class="text-end" data-sort-key="n" data-sort-type="num" aria-sort="descending">', th, fixed = TRUE))
  expect_true(grepl('<a class="sort" role="button" tabindex="0" title="Sort table">', th, fixed = TRUE)); expect_true(grepl('class="sort-mark">&nbsp;&#9660;</span>', th, fixed = TRUE))
  expect_true(regexpr("</a>", th, fixed = TRUE) < regexpr('class="x"', th, fixed = TRUE))            # what follows the title (a hover note) is not part of the link
  expect_false(grepl("data-sort-ask", th, fixed = TRUE))                                         # a title orders the rows on screen, unless it says it asks the server for other rows:
  expect_true(grepl('data-sort-key="n" data-sort-type="num" data-sort-ask="rank_by" data-sort-ask-value="rate">', as.character(e$sort_th("Calls", "n", "en", class = NULL, ask = c("rank_by", "rate"))), fixed = TRUE))
  plain <- as.character(e$sort_th("Name", "name", "es", "text"))
  expect_false(grepl("aria-sort|sort-mark|text-end|data-sort-first", plain)); expect_true(grepl('data-sort-type="text"', plain, fixed = TRUE)); expect_true(grepl('title="Ordenar la tabla"', plain, fixed = TRUE))
  expect_true(grepl('data-sort-first="asc"', as.character(e$sort_th("#", "rank", "en", first = "asc")), fixed = TRUE)); expect_true(grepl("&#9650;", as.character(e$sort_th("x", "x", "en", order = "ascending")), fixed = TRUE))
  expect_equal(as.character(e$sort_td("<10", NA, "text-end")), '<td class="text-end" data-v="">&lt;10</td>')      # a figure that is not shown: no value
  expect_equal(as.character(e$sort_td("1,234", 1234)), '<td data-v="1234">1,234</td>')
  for (l in names(e$LANGUAGES)) expect_true(nzchar(e$TRANSLATIONS[[l]][["sp_sort"]]), info = l)

  # Every table of the app is one of these, but three: the countries' table of territories, which the server orders
  # (its rows change with the map), and two that have no rows to put in order (a district against its city year by
  # year, and the three bars of the rates of a profile). A new table has to be one or the other, and none is drawn
  # by shiny's own table any more.
  src <- lapply(c(about = "app_about.R", city = "app_city.R", country = "app_country.R", helpers = "app_helpers.R"), function(f) readLines(file.path("app", f), encoding = "UTF-8", warn = FALSE))
  count <- function(x, pattern) sum(lengths(regmatches(x, gregexpr(pattern, x, fixed = TRUE))))
  expect_equal(vapply(src, count, 0, pattern = "tags$table("), c(about = 0, city = 1, country = 2, helpers = 1))
  expect_equal(vapply(src, count, 0, pattern = "sort_table("), c(about = 3, city = 4, country = 1, helpers = 2))
  expect_equal(sum(vapply(src, count, 0, pattern = "renderTable(")) + sum(vapply(src, count, 0, pattern = "tableOutput(")), 0)
  # The browser's side: the script that orders, with what it leans on, and the look of a title
  for (piece in c("table.sortable thead a.sort", "data-sort-key", "data-sort-type", "data-sort-first", "data-pin", "data-sort-id", "data-v", "aria-sort", "MutationObserver", "window.wmSort",
                  "th.getAttribute('data-sort-ask')", "th.getAttribute('data-sort-ask-value')", "Shiny.setInputValue(ask, {"))
    expect_true(grepl(piece, e$JS, fixed = TRUE), info = piece)
  expect_true(grepl("table.sortable thead a.sort { color: inherit; text-decoration: none; cursor: pointer; }", e$CSS, fixed = TRUE)); expect_true(grepl(".rank-wrap { overflow-x: auto; }", e$CSS, fixed = TRUE))

  # A city's ranking: the areas of the chart, in its order; the control above says by what, and the table says so too
  z <- e$get_view("zurich"); labels <- z$month_labels("en"); types <- unique(z$base$call_type[z$base$block == "public_safety"])
  d0 <- z$compute("public_safety", range(z$MONTHS), types); nbd <- d0$neighbourhoods                                     # what the page reads, worked out apart from it
  nbd$name <- paste0(unname(e$name_of[nbd$territory_id]), " (", unname(e$name_of[unname(e$parent_of[nbd$territory_id])]), ")")
  first10 <- function(by) e$order_rows(nbd[!is.na(nbd[[by]]), ], by, desc = TRUE)[1:10, ]
  shiny::testServer(function(input, output, session) z$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = labels[c(1, length(labels))], neighbourhood = "", stats_level = "neighbourhood", sort = "rate", shade = "rate", stats_zone = "city")
    session$elapse(400); session$setInputs(types = types); session$elapse(400)
    tb <- sortable_tables(output$stats_table$html); expect_length(tb, 1); tb <- keep(tb[[1]]); top <- first10("rate")
    expect_equal(tb$head$key, c("rank", "name", "n", "rate", "ratio")); expect_equal(tb$head$type, c("num", "text", "num", "num", "num")); expect_true(all(tb$head$link))
    expect_equal(tb$head$aria, c(NA, NA, NA, "descending", NA)); expect_equal(tb$head$mark, c(FALSE, FALSE, FALSE, TRUE, FALSE)); expect_equal(tb$head$first, c("asc", NA, NA, NA, NA))
    expect_true(is.na(tb$id))                                                                    # drawn again, it starts in the order of the control: it keeps none of its own
    expect_equal(tb$head$ask, c(NA, NA, "rank_by", NA, NA)); expect_equal(tb$head$ask_value, c(NA, NA, "n", NA, NA))   # by rate: the title of the numbers asks for the ranking by number; those of the rates turn the rows round
    expect_length(tb$rows, 10); expect_true(arrives_as_marked(tb)); expect_equal(column(tb, "rank"), as.character(1:10))
    expect_equal(column(tb, "name"), e$fold_name(top$name)); expect_equal(as.numeric(column(tb, "n")), top$n); expect_equal(as.numeric(column(tb, "rate")), top$rate)
    expect_equal(as.numeric(column(tb, "ratio")), top$rate / d0$city$rate); expect_equal(column(tb, "rate", "text"), unname(e$fmt_r(top$rate, "en")))
    # another order of these same rows is the browser's affair; by name, by number, the other way round: none loses a row
    for (key in tb$head$key) for (desc in c(TRUE, FALSE)) expect_setequal(browser_order(tb, key, desc), 1:10)
    expect_equal(column(tb, "name")[browser_order(tb, "name", FALSE)], sort(e$fold_name(top$name), method = "radix"))
    session$setInputs(sort = "n"); session$flushReact(); tb <- keep(sortable_tables(output$stats_table$html)[[1]])
    expect_equal(tb$head$aria, c(NA, NA, "descending", NA, NA)); expect_true(arrives_as_marked(tb)); expect_equal(as.numeric(column(tb, "n")), first10("n")$n)
    expect_equal(tb$head$ask, c(NA, NA, NA, "rank_by", "rank_by")); expect_equal(tb$head$ask_value, c(NA, NA, NA, "rate", "rate"))   # by number: the titles of the rate and of its ratio to the city ask for the ranking by rate
  })
  # The titles of the numbers and of the rates decide which areas are in the ranking, as the control above it does
  # (the founder, 18 November 2026; P-068). In the test data the ten areas with most events are the ten with the
  # highest rates; for the check the last neighbourhood is made smaller: by number it stays among the ten, by rate it
  # leaves them and another one comes in.
  smaller <- z$base; last <- smaller$neighbourhood_id == max(smaller$neighbourhood_id); smaller$events[last] <- as.integer(round(smaller$events[last] * 0.56))
  set(z, list(base = smaller), function(v) {
    nb1 <- v$compute("public_safety", range(v$MONTHS), types)$neighbourhoods
    nb1$name <- paste0(unname(e$name_of[nb1$territory_id]), " (", unname(e$name_of[unname(e$parent_of[nb1$territory_id])]), ")")
    ten <- function(by) e$fold_name(e$order_rows(nb1[!is.na(nb1[[by]]), ], by, desc = TRUE)$name[1:10])
    expect_false(setequal(ten("n"), ten("rate"))); expect_equal(length(intersect(ten("n"), ten("rate"))), 9)   # the case the check is about
    shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
      session$setInputs(block = "public_safety", period = labels[c(1, length(labels))], neighbourhood = "", stats_level = "neighbourhood", sort = "rate", shade = "rate", stats_zone = "city")
      session$elapse(400); session$setInputs(types = types); session$elapse(400)
      now <- function() sortable_tables(output$stats_table$html)[[1]]
      click <- function(value) { session$setInputs(rank_by = list(value = value, at = as.numeric(Sys.time()))); session$flushReact(); now() }
      tb <- now(); expect_equal(column(tb, "name"), ten("rate")); expect_equal(tb$head$aria, c(NA, NA, NA, "descending", NA))
      tb <- click("n"); expect_equal(column(tb, "name"), ten("n")); expect_equal(tb$head$aria, c(NA, NA, "descending", NA, NA)); expect_true(arrives_as_marked(tb))   # other areas, not the same ones in another order
      expect_equal(tb$head$ask_value, c(NA, NA, NA, "rate", "rate"))
      expect_equal(column(click("name"), "name"), ten("n")); expect_equal(column(click(NULL), "name"), ten("n"))        # nothing but the number or the rate is a ranking
      tb <- click("rate"); expect_equal(column(tb, "name"), ten("rate")); expect_equal(tb$head$aria, c(NA, NA, NA, "descending", NA)); expect_true(arrives_as_marked(tb))
      session$setInputs(sort = "n"); session$flushReact(); expect_equal(column(now(), "name"), ten("n"))                  # the control, touched last, decides again...
      expect_equal(column(click("rate"), "name"), ten("rate"))                                                           # ...and so does a title touched after it
    })
  })
  # Equal figures go by name, in the table as in the chart beside it and in the Excel file (the three read the same rows)
  tied <- data.frame(territory_id = c("a", "b", "c", "d"), name = c("Delta", "Álamo", "Beta", "Cedro"), n = c(20, 20, 30, 20), rate = c(2, 2, 3, NA), stringsAsFactors = FALSE)
  expect_equal(e$order_rows(tied, "n")$name, c("Beta", "Álamo", "Cedro", "Delta")); expect_equal(e$order_rows(tied, "rate")$name, c("Beta", "Álamo", "Delta", "Cedro"))

  # A neighbourhood's profile over three months with a threshold of 60: one of its types made four times larger for
  # the check (the types of a test neighbourhood are all alike), so that one is shown and the other two are not
  b <- e$get_view("buenosaires"); months <- b$MONTHS; bl <- b$month_labels("en"); nb <- b$geo_nb$territory_id[1]; btypes <- unique(b$base$call_type)
  larger <- b$base; grown <- larger$neighbourhood_id == nb & larger$call_type == sort(btypes)[2]; larger$events[grown] <- larger$events[grown] * 4L
  mine <- larger[larger$neighbourhood_id == nb & larger$month %in% months[1:3], ]; real <- tapply(mine$events, mine$call_type, sum)
  thr <- 60; expect_equal(c(sum(real < thr), sum(real >= thr)), c(2, 1))                         # the case the check is about
  low_text <- sprintf("&lt;%d", thr)
  was <- e$MARTS$threshold; on.exit(e$MARTS$threshold <- was, add = TRUE)
  notes <- e$TYPE_NOTES; e$TYPE_NOTES <- c(notes, stats::setNames("type_note_other_offences", sort(btypes)[1])); on.exit(e$TYPE_NOTES <- notes, add = TRUE)   # one of the types with a hover note, as Madrid has
  set(b, list(THRESHOLD = thr, base = larger), function(v) shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
    session$setInputs(block = "public_safety", period = bl[c(1, 3)], neighbourhood = nb, stats_level = "district", sort = "n", shade = "n", stats_zone = "city")
    session$elapse(400); session$setInputs(types = btypes); session$elapse(400)
    tabs <- sortable_tables(output$profile$html); ids <- vapply(tabs, function(x) x$id, ""); expect_true("types" %in% ids)
    tb <- keep(tabs[[match("types", ids)]], thr)
    expect_equal(tb$head$key, c("name", "n")); expect_equal(tb$head$type, c("text", "num")); expect_true(all(tb$head$link)); expect_equal(tb$head$aria, c(NA, "descending")); expect_equal(tb$head$label[1], "Type")
    shown <- column(tb, "n"); expect_equal(sum(!nzchar(shown)), sum(real < thr)); expect_setequal(as.numeric(shown[nzchar(shown)]), unname(real[real >= thr]))
    expect_true(all(column(tb, "n", "text")[!nzchar(shown)] == low_text)); expect_true(arrives_as_marked(tb))
    hidden <- column(tb, "name")[!nzchar(shown)]; expect_equal(hidden, sort(hidden, method = "radix"))   # the ones not shown: last, by name; their place says nothing about them
    expect_true(grepl('class="info wm-tip"', tb$body, fixed = TRUE)); expect_true(grepl("Generic City Council category", tb$body, fixed = TRUE))   # the note of a type travels with its row: a plain element
    for (desc in c(TRUE, FALSE)) expect_equal(utils::tail(column(tb, "n")[browser_order(tb, "n", desc)], sum(real < thr)), rep("", sum(real < thr)))   # ...whichever way the column is ordered
    # the ranking beside it: a level without residents has a column of rates with nothing to order by
    session$setInputs(stats_level = "neighbourhood"); session$flushReact(); rk <- keep(sortable_tables(output$stats_table$html)[[1]], thr)
    expect_true(all(!nzchar(column(rk, "rate"))) && all(column(rk, "rate", "text") == "—") && all(!nzchar(column(rk, "ratio")))); expect_true(arrives_as_marked(rk)); expect_true(all(as.numeric(column(rk, "n")) >= thr))
    expect_true(all(is.na(rk$head$ask)))                                                         # ...and no title offers a ranking by rate there
    # Asked for all the same (the control): by number, never a title with nothing under it (as it was until 18 November 2026)
    session$setInputs(sort = "rate"); session$flushReact(); asked <- sortable_tables(output$stats_table$html)[[1]]
    expect_equal(asked$head$aria, c(NA, NA, "descending", NA, NA)); expect_equal(column(asked, "name"), column(rk, "name")); expect_true(all(is.na(asked$head$ask)))
    # Back at a level with residents: by number, as the control now says, and the ranking by rate on offer again
    session$setInputs(stats_level = "district"); session$flushReact(); di <- sortable_tables(output$stats_table$html)[[1]]
    expect_equal(di$head$aria, c(NA, NA, "descending", NA, NA)); expect_equal(di$head$ask_value, c(NA, NA, NA, "rate", "rate"))
    session$setInputs(rank_by = list(value = "rate", at = 1)); session$flushReact(); expect_equal(sortable_tables(output$stats_table$html)[[1]]$head$aria, c(NA, NA, NA, "descending", NA))
  }))
  rows <- data.frame(territory_id = c("A", "B", "C", "D", "E"), name = c("Alpha", "Beta", "Gamma", "Delta", "Épsilon"), n = c(70, 176, NA, 48, NA), rate = c(2.2, 7.2, NA, 1.3, NA), stringsAsFactors = FALSE)
  n_text <- function(n) if (is.na(n)) "fewer than 10" else format(n)
  near <- sortable_tables(e$near_ui(rows, "A", "en", n_col = "Calls", rate_col = "Per 1,000 res.", n_text = n_text, click = "near_pick", name_col = "Neighbourhood")); expect_length(near, 1); near <- keep(near[[1]])
  expect_equal(near$id, "near"); expect_equal(near$head$key, c("name", "n", "rate")); expect_equal(near$head$aria, c(NA, NA, "descending")); expect_equal(near$head$label[1], "Neighbourhood")
  expect_equal(column(near, "name"), c("beta", "alpha", "delta", "epsilon", "gamma")); expect_equal(column(near, "n"), c("176", "70", "48", "", "")); expect_true(arrives_as_marked(near))
  expect_equal(column(near, "name")[browser_order(near, "n", FALSE)], c("delta", "alpha", "beta", "epsilon", "gamma"))     # from the smallest: the two without a figure still last, by name
  expect_equal(column(near, "name")[browser_order(near, "name", TRUE)], c("gamma", "epsilon", "delta", "beta", "alpha"))
  by_n <- keep(sortable_tables(e$near_ui(rows, "A", "es", n_col = "Avisos", rate_col = NULL, n_text = n_text, click = "near_pick", name_col = "Barrio"))[[1]])
  expect_equal(by_n$head$key, c("name", "n")); expect_equal(by_n$head$aria, c(NA, "descending")); expect_true(arrives_as_marked(by_n))

  # The panels of a city (the test data has none): the whole city by indicator, and a district against the city
  dis <- b$geo_di$territory_id; P <- data.frame(city = "buenosaires", panel_id = c("motives", "groups"), kind = c("city", "district"), name_en = c("Persons by motive", "Offences by group"),
    name_local = c("Personas por motivo", "Delitos por grupo"), publisher = "Test", url = "", licence = "CC0", measure = "persons", title_key = "", note_key = "", note2_key = "", col_key = "", value_key = "",
    other_period_key = "", top_n = c(NA_integer_, 3L), default_indicator = "", stringsAsFactors = FALSE)
  I <- data.frame(city = "buenosaires", panel_id = rep(c("motives", "groups"), c(4, 5)), indicator = c("m_b", "m_a", "m_c", "m_tot", "g_1", "g_2", "g_3", "g_4", "g_tot"),
    name_local = c("Beta", "Alfa", "Gamma", "Total", "Uno", "Dos", "Tres", "Cuatro", "Todo"), name_en = c("Beta", "Alpha", "Gamma", "Total", "One", "Two", "Three", "Four", "All"), name_key = "",
    sort_order = c(1:4, 1:5), is_total = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE), stringsAsFactors = FALSE)
  one <- function(panel, where, ind, value) data.frame(city = "buenosaires", panel_id = panel, month = rep(months, each = length(ind) * length(where)), territory_id = rep(rep(where, each = length(ind)), length(months)),
                                                       indicator = rep(ind, length(where) * length(months)), value = rep(as.integer(value), length(where) * length(months)), stringsAsFactors = FALSE)
  V <- rbind(one("motives", b$CITY_ID, c("m_b", "m_a", "m_c", "m_tot"), c(30, 30, 5, 65)), one("groups", dis, c("g_1", "g_2", "g_3", "g_4", "g_tot"), c(3, 20, 20, 2, 45)))
  set(b, list(PANELS = P, PANEL_VALUES = V, PANEL_IND = I), function(v) {
    # a district in one month: Two and Three (20 each), One (3) and Four (2) under the threshold; three rows asked for
    blk <- sortable_tables(v$panel_block("groups", dis[1], months[c(1, 1)], "en")); expect_length(blk, 1); blk <- keep(blk[[1]])
    expect_equal(blk$id, "pn_groups"); expect_equal(blk$head$key, c("name", "n", "rate", "city")); expect_equal(blk$head$aria, c(NA, "descending", NA, NA)); expect_true(all(blk$head$link))
    expect_equal(column(blk, "name"), c("three", "two", "four", "all"))                         # equal figures by name; of the two that are not shown, the first by name: not the larger one
    expect_equal(column(blk, "n"), c("20", "20", "", "45")); expect_equal(column(blk, "n", "text")[3], "&lt;10"); expect_equal(column(blk, "rate")[3], "")
    expect_equal(vapply(blk$rows, function(r) r$pin, ""), c(NA, NA, NA, "bottom")); expect_true(arrives_as_marked(blk))
    for (key in blk$head$key) for (desc in c(TRUE, FALSE)) expect_true(all(browser_order(blk, key, desc) %in% 1:3))   # the total is not among the rows that move
    shiny::testServer(function(input, output, session) v$server_city(input, output, session, "en"), {
      session$setInputs(block = "public_safety", period = bl[c(1, 3)], neighbourhood = "", stats_level = "district", sort = "n", shade = "n", stats_zone = "city")
      session$elapse(400); session$setInputs(types = btypes); session$elapse(400)
      pn <- sortable_tables(output$pn_motives_table$html); expect_length(pn, 1); pn <- keep(pn[[1]])
      expect_equal(pn$id, "pn_motives_table"); expect_equal(pn$head$key, c("name", "n", "share", "before")); expect_equal(pn$head$aria, c(NA, "descending", NA, NA)); expect_true(all(pn$head$link))
      expect_equal(column(pn, "name")[1:3], c("alpha", "beta", "gamma")); expect_equal(column(pn, "n")[1:3], c("90", "90", "15")); expect_equal(round(as.numeric(column(pn, "share")[1:3]), 1), c(46.2, 46.2, 7.7))
      expect_equal(vapply(pn$rows, function(r) r$pin, ""), c(NA, NA, NA, "bottom")); expect_equal(pn$rows[[4]]$text[1:2], c("Total", "195")); expect_true(arrives_as_marked(pn))
      session$setInputs(pn_motives_click = "m_c"); session$flushReact(); again <- sortable_tables(output$pn_motives_table$html)[[1]]
      expect_equal(again$id, "pn_motives_table"); expect_equal(vapply(again$rows, function(r) r$class, ""), c(NA, NA, "sel", "ref"))   # drawn again with the row chosen: the same name, so the browser gives its order back
    })
  })

  # A country: the breakdown of a territory by type, the total at the foot; a figure under the threshold has no value
  x <- e$get_view("XT"); per <- names(x$cb_periods("XT", "en", x$CB))
  breakdown <- function() { out <- NULL
    shiny::testServer(function(input, output, session) x$server_country(input, output, session, "en"), {
      session$setInputs(sp_level = "region", sp_period = rep(per[1], 2), sp_territory = "XT-R2"); session$flushReact(); session$setInputs(sp_types = "a"); session$flushReact()
      out <<- sortable_tables(output$sp_breakdown$html) })
    out }
  bd <- breakdown(); expect_length(bd, 1); bd <- keep(bd[[1]])
  expect_equal(bd$id, "breakdown"); expect_equal(bd$head$key, c("name", "n", "rate", "ref")); expect_equal(bd$head$type, c("text", "num", "num", "num")); expect_equal(bd$head$aria, c(NA, "descending", NA, NA)); expect_true(all(bd$head$link))
  expect_equal(vapply(bd$rows, function(r) r$pin, ""), c(NA, "bottom")); expect_equal(column(bd, "n"), c("16", "18")); expect_true(arrives_as_marked(bd))
  e$MARTS$threshold <- 17; low <- keep(breakdown()[[1]], 17); e$MARTS$threshold <- was
  expect_equal(column(low, "n"), c("", "18")); expect_equal(column(low, "n", "text")[1], "&lt;17"); expect_equal(column(low, "rate")[1], ""); expect_true(nzchar(column(low, "ref")[1]))

  # The page about the data: its tables are text, each column a title that orders; no order claimed, none kept from the server
  about <- sortable_tables(e$ui_about("en", e$CITY_LIGHT)); expect_true(all(c("about_sources", "about_method") %in% vapply(about, function(tb) tb$id, "")))
  for (tb in about) { expect_true(all(tb$head$type == "text") && all(tb$head$link) && all(is.na(tb$head$aria)) && !anyDuplicated(tb$head$key), info = tb$id); expect_true(length(tb$rows) > 0, info = tb$id) }
  expect_equal(about[[match("about_sources", vapply(about, function(tb) tb$id, ""))]]$head$key, c("source", "measures", "territory", "period", "licence"))

  # Whatever the table: a title for every column, one order claimed at most, and under each figure the value it shows
  # or none: nothing under the threshold reaches the browser by this door
  expect_equal(length(seen), 10); none <- 0
  for (tb in seen) {
    info <- paste(tb$id, paste(tb$head$key, collapse = "/"))
    expect_true(all(tb$head$link) && !anyNA(tb$head$key) && !anyDuplicated(tb$head$key) && all(tb$head$type %in% c("num", "text")), info = info)
    expect_true(sum(!is.na(tb$head$aria)) <= 1 && identical(tb$head$mark, !is.na(tb$head$aria)), info = info)
    expect_false(grepl("<bslib-", tb$body, fixed = TRUE), info = info)                           # a row that moves holds no custom element: a bslib tooltip breaks when it is taken out and put back
    for (r in tb$rows[vapply(tb$rows, function(r) is.na(r$pin), TRUE)]) for (k in which(tb$head$type == "num")) {
      v <- r$v[k]; shown <- r$text[k]; empty <- shown %in% c("—", sprintf("&lt;%d", tb$threshold), "fewer than 10")
      expect_equal(!nzchar(v), empty, info = paste(info, tb$head$key[k], shown)); if (empty) none <- none + 1 else expect_true(is.finite(as.numeric(v)), info = info)
      if (tb$head$key[k] == "n" && nzchar(v)) expect_true(as.numeric(v) >= tb$threshold, info = paste(info, v))
    }
  }
  expect_true(none >= 10)                                                                        # the case was there to be checked
})
