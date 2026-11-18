# Helpers: formats, the time-series block and its period bar, the population behind a rate, tables in the order of a column, names, ⓘ, KPIs, the comparison card, density, concentration ---
# Part of the app: sourced by app/app.R (same environment); nothing here runs on its own.

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

# --- Time series: a bar chart at a chosen granularity (month / quarter / year) with a period bar below it ---
# The period bar is a small chart of the whole series with two handles (from, to) and the window between them (the
# browser side is in app_styles.R): dragging a handle or the window chooses whole periods of the chart, the preset
# links choose the last 1 / 3 / 5 years or everything. Figures under the threshold are not drawn (P-009); an incomplete
# last period is drawn lighter and marked *. Nothing here needs a package beyond shiny.
# Aggregate a native series (date = period start, n) to the granularity; fy = financial years (April-March).
# `start` and `end` are the first and the last native period inside each bar.
ts_build <- function(d, gran, lang, native = "month", fy = FALSE, year_text = as.character) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- d[order(d$date), ]
  y <- as.integer(format(d$date, "%Y")); m <- as.integer(format(d$date, "%m")); fyy <- if (fy) y + (m >= 4) else y
  q <- if (fy) ((m - 4) %% 12) %/% 3 + 1 else (m - 1) %/% 3 + 1
  d$key <- switch(gran, month = format(d$date, "%Y-%m"), quarter = sprintf("%d-%d", fyy, q), year = sprintf("%d", fyy))
  agg <- d |> group_by(key) |> summarise(start = min(date), end = max(date), n = sum(n), parts = n(), .groups = "drop") |> arrange(start) |> as.data.frame()
  expected <- switch(gran, month = 1, quarter = if (native == "month") 3 else 1, year = switch(native, month = 12, quarter = 4, 1))
  agg$complete <- agg$parts >= expected
  yy <- as.integer(substr(agg$key, 1, 4)); qq <- suppressWarnings(as.integer(sub("^[0-9]{4}-", "", agg$key)))
  agg$label <- switch(gran, month = fmt_month(agg$start, lang), quarter = sprintf(TR("ts_quarter_label", lang), year_text(yy), qq), year = year_text(yy))
  agg$short <- switch(gran, month = paste0(substr(month_names(lang)[as.integer(format(agg$start, "%m"))], 1, 3), " ", format(agg$start, "%y")),
                      quarter = sprintf(TR("ts_quarter_label", lang), if (fy) sprintf("%02d/%02d", (yy - 1) %% 100, yy %% 100) else format(agg$start, "%y"), qq), year = year_text(yy))   # "24 Q3", "24 T3", "24 K3": the letter of the language
  agg
}
# The bars a chosen range shows, as the numbers of the first and the last one: those that start inside it. Never
# none: a range that falls inside one bar (a few months seen by year) shows that bar. NULL = the whole series.
ts_window <- function(agg, range) {
  n <- nrow(agg); if (is.null(range)) return(c(1L, n))
  i <- which(agg$start >= range[1] & agg$start <= range[2])
  if (length(i) == 0) { k <- max(1L, sum(agg$start <= range[1])); return(c(k, k)) }
  c(min(i), max(i))
}
# The period those bars cover: from the first day of the first one to the day after the last one (NULL = everything)
ts_period <- function(agg, range, native = "month") {
  if (is.null(agg) || is.null(range)) return(NULL)
  w <- ts_window(agg, range); c(agg$start[w[1]], ts_period_end(agg$end[w[2]], native))
}
# The periods of a second series (their first days, each one `unit` long) that such a period touches: under a chart by
# quarter, a chart by year shows the years its quarters belong to
ts_touched <- function(start, period, unit = "year") if (is.null(period)) rep(TRUE, length(start)) else start < period[2] & ts_period_end(start, unit) > period[1]
# Margins (in lines) that fit the device: a plot drawn while its box is still small (a tab opening, a phone, a
# display scaled by Windows) must not stop with "figure margins too large"; the margins shrink instead
fit_mar <- function(mar) {
  din <- par("din"); line <- par("csi"); if (!all(is.finite(din)) || !is.finite(line) || line <= 0) return(mar)
  f <- min(1, 0.6 * din[2] / (line * (mar[1] + mar[3])), 0.6 * din[1] / (line * (mar[2] + mar[4])))
  mar * f
}
ts_span_text <- function(from, to) if (identical(from, to)) from else paste0(from, " – ", to)     # "2019 – 2023"; one period: "2023"
# The unit a chart over time opens with. A series by month opens by quarter, which reads well over years; a short one
# (up to two years of months: Madrid's calls begin in January 2026) opens by month, since by quarter it would be three
# or four bars, the last of them half empty.
ts_default_gran <- function(time_unit, n_months) if (identical(time_unit, "year")) "year" else if (isTRUE(n_months <= 24)) "month" else "quarter"
# The seasonal profile of a series by month: the average of each calendar month over the complete calendar years (the
# years with their twelve months). NULL when there is none: a series that has not completed a calendar year yet
# (Madrid's calls begin in January 2026) has no season to show, and the page then has no such block.
season_profile <- function(month, n) {
  if (length(month) == 0) return(NULL)
  m <- data.frame(month = as.Date(month), n = n) |> group_by(month) |> summarise(n = sum(n), .groups = "drop")
  m$year <- as.integer(format(m$month, "%Y")); m$mo <- as.integer(format(m$month, "%m"))
  full <- names(which(table(m$year) == 12)); m <- m[m$year %in% as.integer(full), ]
  if (nrow(m) == 0) return(NULL)
  avg <- m |> group_by(mo) |> summarise(n = mean(n), .groups = "drop")
  list(avg = avg, years = sort(as.integer(full)), total = sum(m$n))
}
# Bars that do not grow fat when they are few. barplot() gives every bar its share of the width, so three bars on a
# wide screen are three walls. Here each bar keeps its own slot (the names under them stay spread along the axis)
# and is at most `max_px` pixels wide, centred in it. With many bars the result is barplot()'s own look (bars of
# 80% of their slot). Returns the `width`, `space` and `xlim` to give to barplot(); call it after par(mar = ...).
bar_fit <- function(k, max_px = 110, plot_px = graphics::par("pin")[1] * 96) {
  if (!is.finite(k) || k < 1) k <- 1
  slot <- if (is.finite(plot_px) && plot_px > 0) plot_px / k else NA_real_
  w <- if (is.na(slot)) 0.8 else min(0.8, max_px / slot)
  list(width = w, space = (1 - w) / w, xlim = c((1 - w) / 2, k + (1 - w) / 2))
}
# The names at the left of a chart of horizontal bars. A name wider than `max_in` inches is cut and ends in "…"; the
# width is measured as the name is drawn (`measure`: by default on the open device, at the size `cex`), not by its
# number of letters, so that names in any script keep to their share of the chart and the margin is as wide as the
# widest name, no more (a margin by number of letters left a blank strip at the left of a narrow chart).
# Returns the names and the width of the widest one, in inches.
fit_labels <- function(lab, max_in, cex = 0.8, measure = function(s) graphics::strwidth(s, units = "inches", cex = cex)) {
  lab <- as.character(lab); lab[is.na(lab)] <- ""
  if (length(lab) == 0) return(list(labels = lab, width = 0))
  w <- measure(lab)
  for (i in which(w > max_in)) {
    n <- nchar(lab[i]); cut <- function(n) paste0(sub("\\s+$", "", substr(lab[i], 1, n)), "…")
    while (n > 1 && measure(cut(n)) > max_in) n <- n - 1
    lab[i] <- cut(n)
  }
  list(labels = lab, width = max(measure(lab)))
}
# One name in every how many bars, when a name is `need` bar slots wide: the smallest step that leaves them side by
# side, among those that read well for months and quarters (2, 3, 4, 6, then whole dozens: the same month every year)
label_step <- function(need) {
  if (!is.finite(need) || need <= 1) return(1)
  steps <- c(2, 3, 4, 6, 12 * seq_len(100)); s <- steps[steps >= need]
  if (length(s)) s[1] else ceiling(need)
}
named_bars <- function(k, every) (k - seq_len(k)) %% every == 0          # which of k bars get their name: the last one, and one in every `every` back from it
ts_blank <- function(e = NULL) { par(mar = c(0, 0, 0, 0)); plot.new(); NULL }     # a device too small to draw in: an empty plot, not an error on the page
# --- Charts in the light and the dark look (P-070) ---------------------------------------------------------------
# A chart is an image drawn by R: the page cannot recolour it. render_chart() is renderPlot() with two things added:
# the image has no ground of its own (the page shows through), and it is drawn in the ink of the look the page says
# it is in (input$theme, sent by the script of the theme button) and drawn again when that changes.
# The colours that carry data are the same in both looks (the blue of the bars, the paler blue of an incomplete
# period, the hatching under the threshold): what the texts say of them stays true. Only the ink changes: the text,
# the axes, the reference lines, the edge that sets a mark off its ground, and the pair that tells the chosen bar from
# the others. ink() gives the dark counterpart of a light ink, and the colour itself for anything else.
INK <- new.env()
INK$dark <- FALSE
DARK_INK <- c("#333333" = "#e5e9ee", "#666666" = "#a7adb5", "#475569" = "#b9c4d2", "#555555" = "#aab3bd", "#ffffff" = "#1d1f21",
              "#08519c" = "#9ecae1", "#c6dbef" = "#2c4a66")       # the last two: the bar chosen and the others (which is the lighter one says nothing of the figures)
ink <- function(col) { if (!isTRUE(INK$dark)) return(col); d <- unname(DARK_INK[tolower(col)]); ifelse(is.na(d), col, d) }
render_chart <- function(expr, ..., env = parent.frame()) {
  code <- substitute(expr); force(env)
  shiny::renderPlot({
    session <- shiny::getDefaultReactiveDomain()
    INK$dark <- identical(session$input$theme, "dark")
    if (INK$dark) graphics::par(fg = "#cfd6de", col = "#e5e9ee", col.axis = "#cfd6de", col.lab = "#e5e9ee", col.main = "#e5e9ee", col.sub = "#cfd6de")
    eval(code, env)
  }, ..., bg = "transparent")
}

ts_plot_empty <- function(lang) { ts_blank(); text(0.5, 0.5, TR("ts_empty", lang), col = ink("#666666")); invisible(NULL) }
# One series as bars, in the style every time series of the app shares: a figure on each bar when the bars are few,
# an incomplete period lighter and marked *, a missing value (under the threshold, no population) written out and
# not drawn. `line` adds a second series as a dashed line with points (the reference of a rate): the figures of the
# bars then get a white edge so that they read where the line crosses them. Returns the positions of the bars.
ts_bars <- function(v, lab, complete, fmt, missing_text, lang, col = "#3182bd", line = NULL, line_col = ink("#475569")) {
  k <- length(v); shown <- ifelse(is.na(v), 0, v)
  top <- suppressWarnings(max(c(shown, line), na.rm = TRUE)); if (!is.finite(top) || top <= 0) top <- 1
  op <- par(mar = fit_mar(c(3.4, 4.2, 0.6, 0.4)), las = 1, cex.axis = 0.8, mgp = c(2.8, 0.5, 0)); on.exit(par(op))
  fit <- bar_fit(k)
  b <- barplot(shown, width = fit$width, space = fit$space, xlim = fit$xlim, col = ifelse(complete, col, adjustcolor(col, 0.45)), border = NA, ylim = c(0, top * 1.15), ylab = "", yaxt = "n")
  # The names under the bars: as many as fit side by side, measured as they are drawn (each bar has a slot 1 wide
  # along the axis, see bar_fit), counted back from the last bar so that the latest period is always named. Written
  # here and not by barplot(), whose axis leaves out every name when some of them are empty; and with the step chosen
  # here, since axis() by itself drops the names that would touch, one here and one there.
  names_x <- ifelse(complete, lab, paste0(lab, "*")); cx <- if (k > 24) 0.7 else 0.8
  named <- named_bars(k, label_step(1.15 * max(strwidth(names_x, cex = cx))))
  axis(1, at = b[named], labels = names_x[named], tick = FALSE, cex.axis = cx, gap.axis = 0)
  at <- pretty(c(0, top)); axis(2, at = at, labels = fmt_n(at, lang), lwd = 0, lwd.ticks = 1)
  if (!is.null(line)) { lines(b, line, lty = 2, lwd = 1.5, col = line_col); points(b, line, pch = 21, cex = 1.15, bg = line_col, col = ink("#ffffff")) }   # points with an edge of the colour of the ground: they read on the bars too
  # A figure on each bar when the bars are few and the figures fit side by side (a narrow chart with long figures
  # shows none: a click on a bar gives its value); what is missing is written out whether it fits or not, since an
  # empty place must never read as zero
  if (k <= 16) {
    txt <- ifelse(is.na(v), missing_text, fmt(v))
    w <- which(if (max(strwidth(txt[!is.na(v)], cex = 0.72), 0) <= 1) rep(TRUE, k) else is.na(v))
    if (!is.null(line) && length(w)) { px <- 1.3 / 96 * c(diff(par("usr")[1:2]) / par("pin")[1], diff(par("usr")[3:4]) / par("pin")[2])      # 1.3 pixels in the units of each axis
      for (a in seq(0, 2 * pi, length.out = 9)[-9]) text(b[w] + cos(a) * px[1], shown[w] + sin(a) * px[2], txt[w], pos = 3, cex = 0.72, col = ink("#ffffff"), xpd = TRUE) }
    if (length(w)) text(b[w], shown[w], txt[w], pos = 3, cex = 0.72, col = ink("#333333"), xpd = TRUE)
  }
  as.numeric(b)
}
# The main chart; returns the bar positions so that a click can name a bar
ts_plot_main <- function(agg, range, lang, threshold = MARTS$threshold, col = "#3182bd") {
  if (is.null(agg) || nrow(agg) == 0) return(ts_plot_empty(lang))
  w <- ts_window(agg, range); agg <- agg[w[1]:w[2], ]
  v <- apply_threshold(agg$n, threshold)
  b <- ts_bars(v, agg$short, agg$complete, function(x) fmt_n(x, lang), TR("less_than", lang, threshold), lang, col)
  invisible(data.frame(x = b, label = agg$label, n = v, complete = agg$complete))
}
# The same chart for a rate by period: `s` = data.frame(label, rate, ref), `ref` being the rate of the reference
# territory (NA when there is none), drawn as the dashed line. A period without a rate is written "—"; `mark`
# (optional) is written after the name of a bar: † for a rate on the population of another year.
ts_plot_rate <- function(s, lang, col = "#3182bd") {
  if (is.null(s) || nrow(s) == 0) return(ts_plot_empty(lang))
  b <- ts_bars(s$rate, paste0(s$label, if (is.null(s$mark)) "" else s$mark), rep(TRUE, nrow(s)), function(x) fmt_r(x, lang), "—", lang, col, line = if (any(!is.na(s$ref))) s$ref)
  invisible(data.frame(x = b, label = s$label, rate = s$rate, ref = s$ref))
}
# The words around a chart by rate over complete years (the countries and the cities share them). `s` = one row per
# complete year (label, rate, ref, pop, pop_wanted, pop_year, mark), `sh` = the rows the period bar leaves in view.
# The head: the title, whose rate it is and of what, the legend (the reference only when there is one), the people
# behind the first and the last year in view with the way they went - a rate also moves when its people do - and
# what a † stands for.
rate_head_ui <- function(s, sh, name, ref_name, what, lang, title, people) {
  k <- nrow(sh)
  moved <- if (k > 0) pop_trend_text(sh$pop[1], sh$pop_year[1], sh$pop[k], sh$pop_year[k], lang)
  tagList(h5(class = "mt-3 mb-1", tags$b(title)),
          div(class = "sub", name, " · ", what, if (k > 0) paste0(" · ", ts_span_text(sh$label[1], sh$label[k]))),
          div(class = "note", span(style = "display:inline-block;width:10px;height:10px;background:#3182bd;margin-right:4px"), name,
              if (any(!is.na(s$ref))) tagList(span(style = "color:#475569;margin:0 4px 0 12px", HTML("- &#9679; -")), ref_name)),
          if (!is.null(moved)) div(class = "note", paste0(people, " (", name, "): ", moved)),
          if (k > 0 && any(nzchar(sh$mark))) div(class = "note", TR("pop_nearest", lang, years_text(sh$pop_wanted[nzchar(sh$mark)]), years_text(sh$pop_year[nzchar(sh$mark)]))))
}
# The line of the bar clicked: the year, the rate of the territory and of its reference, and its people against the year before
rate_click_ui <- function(hit, s, name, ref_name, lang, people) {
  if (is.null(hit)) return(NULL)
  val <- function(v) if (is.na(v)) "—" else fmt_r(v, lang)
  i <- if (is.null(s)) NA else match(hit$label, s$label)
  moved <- if (!is.na(i) && !is.na(s$pop[i])) pop_trend_text(if (i > 1) s$pop[i - 1] else NA, if (i > 1) s$pop_year[i - 1] else NA, s$pop[i], s$pop_year[i], lang)
  p(class = "note mb-1", tags$b(hit$label), paste0(" · ", name, ": ", val(hit$rate)), if (!is.na(hit$ref)) paste0(" · ", ref_name, ": ", val(hit$ref)),
    if (!is.null(moved)) paste0(" · ", people, ": ", moved))
}
# The period bar. Its small chart takes the whole width of its image (no side margins) and spans from the first day
# of the series to the day after its last period, so that a share of the width is a share of that span: the browser
# places the handles in % of the width, at the shares the server sends (where each bar of the chart starts).
TS_NAV_AXIS <- 21L       # pixels the year labels take under the small chart (the handles stop above them: --tsr-axis in the CSS)
ts_period_end <- function(x, native = "month") { lt <- as.POSIXlt(x); lt$mon <- lt$mon + switch(native, month = 1L, quarter = 3L, 12L); as.Date(lt) }   # the day after the period that starts on x
ts_nav_span <- function(d, native = "month") c(min(d$date), ts_period_end(max(d$date), native))
ts_nav_edges <- function(agg, d, native = "month") { sp <- as.numeric(ts_nav_span(d, native)); c((as.numeric(agg$start) - sp[1]) / (sp[2] - sp[1]), 1) }
ts_plot_nav <- function(d, native = "month", col = "#8fb3d6") {
  if (is.null(d) || nrow(d) == 0) return(invisible(ts_blank()))
  d <- d[order(d$date), ]; span <- as.numeric(ts_nav_span(d, native))
  op <- par(mai = c(TS_NAV_AXIS, 0, 2, 0) / 96, mgp = c(0, 0.1, 0), cex.axis = 0.65, tcl = -0.15); on.exit(par(op))
  from <- as.numeric(d$date); to <- pmin(as.numeric(ts_period_end(d$date, native)), c(from[-1], Inf)); gap <- 0.12 * (to - from)
  plot(NA, xlim = span, ylim = c(0, max(d$n, 1, na.rm = TRUE)), axes = FALSE, xlab = "", ylab = "", xaxs = "i", yaxs = "i")
  rect(from + gap, 0, to - gap, ifelse(is.na(d$n), 0, d$n), col = col, border = NA)
  yrs <- seq(as.Date(format(min(d$date), "%Y-01-01")), max(d$date), by = "year"); yrs <- yrs[yrs >= min(d$date)]
  yrs <- yrs[as.numeric(yrs) + 1.25 * strwidth("0000", cex = 0.65) < span[2]]                    # a year that would be cut by the right edge is not written
  if (length(yrs)) axis(1, at = as.numeric(yrs), labels = format(yrs, "%Y"), lwd = 0, lwd.ticks = 1, col.axis = ink("#555555"), hadj = 0)
  invisible()
}
# The elements of the period bar: the labels of the two handles, the small chart, the two parts outside the window, the
# window and the handles (sliders for a keyboard: the arrow keys move them)
ts_nav_ui <- function(lang) div(class = "ts-nav", id = "ts_range",
  div(class = "tsr-tags", `aria-hidden` = "true", span(class = "tsr-tag tsr-tag-from"), span(class = "tsr-tag tsr-tag-to")),
  div(class = "tsr-track", style = sprintf("--tsr-axis: %dpx", TS_NAV_AXIS),
      plotOutput("ts_nav", height = "84px"),
      div(class = "tsr-dim tsr-dim-l"), div(class = "tsr-dim tsr-dim-r"), div(class = "tsr-win"),
      div(class = "tsr-h tsr-h-from", tabindex = "0", role = "slider", `aria-label` = TR("ts_period", lang)),
      div(class = "tsr-h tsr-h-to", tabindex = "0", role = "slider", `aria-label` = TR("ts_period", lang))))
# The UI of the block (one per page: the ids are fixed). `extra` is a second chart of the same period (the chart by
# rate of the countries): it goes under the main chart, and the period bar under both, as the control they share.
ts_ui <- function(lang, gran_choices, title_key = "ts_title", extra = NULL, selected = NULL, span_months = Inf) {
  # `selected`: the unit the chart opens with (default: the one before the coarsest). `span_months`: the length of the
  # series when it is known beforehand (a city): the links "last year / 3 years / 5 years" are offered only when the
  # series is longer than they are, and none of them leaves no row of links at all.
  presets <- c(1, 3, 5)[c(12, 36, 60) < span_months]
  div(class = if (is.null(extra)) "ts-block" else "ts-block ts-two",   # a plain div: inside a fill container the plots would collapse to 0 px
    tags$hr(), h5(class = "mt-2 mb-1", tags$b(TR(title_key, lang)), " ", info(TR("ts_help", lang))),
    uiOutput("ts_title"),
    div(class = "d-flex flex-wrap align-items-center gap-3 ts-controls",
        if (length(gran_choices) > 1) radioButtons("ts_gran", NULL, inline = TRUE, choices = setNames(gran_choices, vapply(gran_choices, function(g) TR(paste0("ts_", g), lang), "")),
                                                   selected = if (!is.null(selected) && selected %in% gran_choices) selected else gran_choices[length(gran_choices) - 1]),
        if (length(presets)) div(class = "chips ts-chips", span(class = "text-muted small", TR("ts_period", lang), " "),
                                 lapply(presets, function(k) actionLink(paste0("ts_last_", k), TR(paste0("ts_last_", k), lang))), actionLink("ts_all", TR("ts_all", lang), class = "active"))),
    plotOutput("ts_main", height = "250px", click = "ts_click"),
    if (is.null(extra)) tagList(ts_nav_ui(lang), div(class = "ts-line", uiOutput("ts_value")), uiOutput("ts_notes"))
    else tagList(div(class = "ts-line", uiOutput("ts_value")), extra, ts_nav_ui(lang), uiOutput("ts_notes")))
}
# The server side, shared: `native()` gives the series (date, n) of the current selection, `heading()` its text.
# Returns the period in view as a reactive (NULL = everything, else its first day and the day after its last one),
# for a second chart that shares the period bar.
ts_server <- function(input, output, session, lang, native, heading, gran_default, native_unit, fy = FALSE, year_text = as.character, noun = function() "") {
  rng <- reactiveVal(NULL); bars <- reactiveVal(NULL); clicked <- reactiveVal(NULL)
  gran <- reactive(input$ts_gran %||% gran_default)
  last_years <- function(k) { d <- native(); if (is.null(d) || nrow(d) == 0) return(NULL); to <- max(d$date); from <- seq(to, by = paste0("-", k, " year"), length.out = 2)[2] + 1; c(max(from, min(d$date)), to) }
  for (k in c(1, 3, 5)) local({ kk <- k; observeEvent(input[[paste0("ts_last_", kk)]], { rng(last_years(kk)); session$sendCustomMessage("chip_ts", paste0("ts_last_", kk)) }) })
  observeEvent(input$ts_all, { rng(NULL); session$sendCustomMessage("chip_ts", "ts_all") })
  agg <- reactive({ d <- native(); if (is.null(d) || nrow(d) == 0) return(NULL); ts_build(d, gran(), lang, native_unit, fy, year_text) })
  # A period chosen for another place or type that this series has no figures in: everything again
  observe({ d <- native(); r <- isolate(rng()); if (is.null(r) || is.null(d) || nrow(d) == 0) return()
    if (r[2] < min(d$date) || r[1] > max(d$date)) { rng(NULL); session$sendCustomMessage("chip_ts", "ts_all") } })
  # The period bar: the bars of the chart along it (where each starts, its label) and the chosen ones go to the browser...
  observe({ a <- agg(); r <- rng()
    if (is.null(a)) return(session$sendCustomMessage("ts_range", list(id = "ts_range", n = 0L)))
    w <- ts_window(a, r)
    session$sendCustomMessage("ts_range", list(id = "ts_range", n = nrow(a), edges = ts_nav_edges(a, native(), native_unit), labels = as.list(a$label), from = w[1], to = w[2])) })
  # ...and a choice made on it comes back as the numbers of its first and last bar; both ends of the bar = everything
  observeEvent(input$ts_range, { a <- agg(); r <- input$ts_range; if (is.null(a) || !identical(as.integer(r$n), nrow(a))) return()
    i <- max(1L, min(nrow(a), as.integer(r$from))); j <- max(i, min(nrow(a), as.integer(r$to)))
    if (i == 1L && j == nrow(a)) { rng(NULL); session$sendCustomMessage("chip_ts", "ts_all") }
    else { rng(c(a$start[i], a$end[j])); session$sendCustomMessage("chip_ts", "") } })
  output$ts_title <- renderUI({ a <- agg(); if (is.null(a)) return(NULL); w <- ts_window(a, rng())
    div(class = "sub", heading(), " · ", TR(paste0("ts_", gran()), lang), paste0(" · ", ts_span_text(a$label[w[1]], a$label[w[2]]))) })
  output$ts_main <- render_chart({ a <- agg(); b <- tryCatch(ts_plot_main(a, rng(), lang), error = ts_blank); bars(b); clicked(NULL) }, res = 96)
  output$ts_nav <- render_chart({ tryCatch(ts_plot_nav(native(), native_unit), error = ts_blank) }, res = 96)
  observeEvent(input$ts_click, { b <- bars(); if (is.null(b) || !nrow(b)) return(); i <- which.min(abs(b$x - input$ts_click$x)); clicked(b[i, ]) })
  # The line of the chart: the bar clicked and, when a bar in view is incomplete, what its * means (one line: nothing moves at a click)
  output$ts_value <- renderUI({ a <- agg(); if (is.null(a)) return(NULL); c <- clicked(); w <- ts_window(a, rng()); partial <- any(!a$complete[w[1]:w[2]])
    if (is.null(c) && !partial) return(NULL)
    p(class = "note mb-1", if (!is.null(c)) tagList(tags$b(c$label, .noWS = "after"), ": ", if (is.na(c$n)) TR("less_than", lang, MARTS$threshold) else TR("n_noun", lang, fmt_n(c$n, lang), noun()), if (!c$complete) paste0(" (", TR("ts_partial_one", lang), ")")),
      if (partial) paste0(if (!is.null(c)) " · ", TR("ts_partial", lang))) })
  output$ts_notes <- renderUI({ if (is.null(agg())) return(NULL); p(class = "note", TR("ts_nav_help", lang)) })      # under the period bar: how to use it
  period <- reactive(ts_period(agg(), rng(), native_unit))
  # The bars the chart shows, as it draws them (the ones in view, the threshold applied, the unit as an attribute), for
  # whoever needs the figures themselves (the Excel file of a page). An attribute of what the function returns, so
  # that those who call it for the period are left as they were.
  attr(period, "shown") <- reactive({ a <- agg(); if (is.null(a)) return(NULL); w <- ts_window(a, rng()); a <- a[w[1]:w[2], , drop = FALSE]
    a$n <- apply_threshold(a$n, MARTS$threshold); attr(a, "gran") <- gran(); a })
  invisible(period)
}
# --- The population behind a rate --------------------------------------------------------------------------
# A rate takes the population of the year of its figures. `pop` = data.frame(territory_id, year, population).
# `lag` = 1 for financial years (April-March, named by the year they end in): their population is that of the
# calendar year they begin in, the one whose mid-year estimate falls inside them. A year without a figure of its
# own takes the nearest earlier one, else the first one there is, and says so: `exact` FALSE, `year` = the one taken.
# The same for one territory that has no figure in a year the others have: it takes its own nearest year
# (`filled`: territory -> year taken).
pop_for_year <- function(pop, year, lag = 0L) {
  pop <- pop[!is.na(pop$population), ]
  have <- sort(unique(as.integer(pop$year))); want <- as.integer(year) - as.integer(lag)
  y <- if (any(have <= want)) max(have[have <= want]) else have[1]
  p <- pop[pop$year == y, ]
  out <- setNames(p$population, p$territory_id); filled <- setNames(integer(0), character(0))
  rest <- pop[!pop$territory_id %in% p$territory_id, ]
  if (nrow(rest) > 0) {
    rest <- rest[order(rest$territory_id, ifelse(rest$year <= want, want - rest$year, 1000 + rest$year - want)), ]; rest <- rest[!duplicated(rest$territory_id), ]
    out <- c(out, setNames(rest$population, rest$territory_id)); filled <- setNames(as.integer(rest$year), rest$territory_id)
  }
  list(year = y, wanted = want, exact = y == want, pop = out, filled = filled)
}
# The registers of one city by year: the rows of the yearly table (mart_population_years) that belong to it, and the
# latest register (mart_population) for any territory the yearly table does not hold in that year, so that a city
# never loses a rate it had. A database without the yearly table gives the latest register as the one year there is.
city_registers <- function(years, latest, city_id) {
  cols <- c("territory_id", "year", "population", "ref_date")
  mine <- function(x) x[x$territory_id == city_id | startsWith(x$territory_id, paste0(city_id, "-")), , drop = FALSE]
  y <- if (!is.null(years) && nrow(years) > 0) mine(years)[, cols] else NULL
  l <- if (!is.null(latest) && nrow(latest) > 0 && "ref_date" %in% names(latest)) mine(latest) else NULL
  if (!is.null(l) && nrow(l) > 0) {
    l$year <- suppressWarnings(as.integer(format(as.Date(l$ref_date), "%Y"))); l <- l[!is.na(l$year), cols]
    if (!is.null(y)) l <- l[!paste(l$territory_id, l$year) %in% paste(y$territory_id, y$year), ]
    y <- rbind(y, l)
  }
  if (is.null(y)) y <- data.frame(territory_id = character(), year = integer(), population = numeric(), ref_date = as.Date(character()))
  y$year <- as.integer(y$year); y$ref_date <- as.Date(y$ref_date); rownames(y) <- NULL
  y
}

# The population behind the rate of a period of one or several years: by territory, the average of the figure each
# year had (the years added up over it are the yearly average rate, each year on its own people). A year given
# several times counts as many times: the months of a city's period, each with the register of its year.
# `years` = the population years used; `missing` = the years the period asks for and nobody published; `borrowed` =
# the years taken in their place; `filled` = for the territories that lack a figure the others have, the years they
# took ("2019, 2021") and `filled_for` the years they took them for.
years_text <- function(y) {                       # 2019, 2020, 2021, 2024 -> "2019–2021, 2024"
  y <- sort(unique(as.integer(y[!is.na(y)]))); if (length(y) == 0) return("")
  runs <- split(y, cumsum(c(1L, diff(y) != 1L)))
  paste(vapply(runs, function(r) if (length(r) > 2) paste0(r[1], "–", r[length(r)]) else paste(r, collapse = ", "), ""), collapse = ", ")
}
pop_for_period <- function(pop, years, lag = 0L) {
  yy <- as.integer(years); u <- sort(unique(yy)); w <- as.numeric(table(factor(yy, levels = u)))
  ps <- lapply(u, function(y) pop_for_year(pop, y, lag))
  ids <- unique(unlist(lapply(ps, function(p) names(p$pop))))
  m <- do.call(cbind, lapply(ps, function(p) unname(p$pop[ids]))); wm <- matrix(w, nrow = nrow(m), ncol = ncol(m), byrow = TRUE) * !is.na(m)
  avg <- rowSums(m * wm, na.rm = TRUE) / rowSums(wm); avg[is.nan(avg)] <- NA
  used <- vapply(ps, function(p) as.integer(p$year), 1L); exact <- vapply(ps, function(p) p$exact, TRUE)
  wanted <- vapply(ps, function(p) as.integer(p$wanted), 1L)
  by_id <- function(v) if (length(v)) vapply(split(unname(v), names(v)), years_text, "") else setNames(character(0), character(0))
  filled <- by_id(unlist(lapply(ps, function(p) p$filled)))
  filled_for <- by_id(unlist(lapply(ps, function(p) setNames(rep(as.integer(p$wanted), length(p$filled)), names(p$filled)))))
  list(pop = setNames(round(avg), ids), years = sort(unique(used)), missing = sort(unique(wanted[!exact])), borrowed = sort(unique(used[!exact])), exact = all(exact),
       filled = filled, filled_for = filled_for)
}
# The note behind a †: the population years the period asks for and has no figure for, and the years that stand in.
# `id`: a territory that lacks a figure the others have (its own years are added to those of everybody)
pop_nearest_text <- function(pp, lang, id = NULL) {
  own <- !is.null(id) && id %in% names(pp$filled)
  both <- function(a, b) { x <- c(a, b); paste(x[nzchar(x)], collapse = ", ") }
  TR("pop_nearest", lang, both(years_text(pp$missing), if (own) pp$filled_for[[id]] else ""), both(years_text(pp$borrowed), if (own) pp$filled[[id]] else ""))
}
pop_years_text <- function(years, lang) if (length(years) == 0) "" else if (length(years) == 1) as.character(years) else TR("avg_years", lang, paste0(min(years), "–", max(years)))   # "2025", "average 2021–2025"
# How a population moved between two figures: "▲ +6.9%", "▼ −1.2%" or "=" (less than 0.05 % either way)
pop_change_text <- function(from, to, lang) {
  if (is.na(from) || is.na(to) || from <= 0) return(NULL)
  ch <- 100 * (to - from) / from
  if (abs(ch) < 0.05) "=" else paste0(if (ch > 0) "▲ +" else "▼ −", TR("pct", lang, fmt_r(abs(ch), lang)))
}
# "3,280,782 (2021) → 3,506,730 (2025) ▲ +6.9%": the inhabitants at both ends of a chart by rate; one figure when
# both ends are the same year
pop_trend_text <- function(pop1, year1, pop2, year2, lang) {
  one <- function(p, y) paste0(fmt_n(round(p), lang), " (", y, ")")
  if (is.na(pop2)) return(NULL)
  if (is.na(pop1) || identical(as.integer(year1), as.integer(year2))) return(one(pop2, year2))
  paste0(one(pop1, year1), " → ", one(pop2, year2), " ", pop_change_text(pop1, pop2, lang))
}
# --- Tables in the order of a column (P-067) ----------------------------------------------------------------------
# A click on the title of a column puts a table in the order of that column, and a second click turns it round (the
# founder, 17 November 2026: every table of the app, like the countries' table of territories). The countries' table
# is ordered on the server, because its rows change with the map (app_country.R, sp_table). Every other table is
# ordered in the browser (app_styles.R, "Tables in the order of a column"): no trip to the server, and nothing else
# of the page is drawn again. What the server writes for the browser:
#   table  sort_table(): class "sortable". With an `id` (data-sort-id), the browser keeps the order the reader chose
#          and gives it back when the table is drawn again (another area, another period). Without one, a table drawn
#          again starts in the server's order (a city's ranking: its order is the one of the control above it).
#   th     sort_th(): the title is a link, as in the countries' table. `key` names the column; `type` says what it
#          holds ("num": the largest first at the first click; "text": from A to Z; `first` says otherwise);
#          `order` ("descending" / "ascending") marks the column the table arrives in the order of. `ask` =
#          c(input, value): a click on this title does not order the rows on screen but asks the server, through
#          that input, for a table by this column (a city's ranking: which areas are listed depends on it; P-068).
#   td     sort_td(): data-v is what the browser orders by. For a figure it is written from the same value the cell
#          shows: a figure that is not shown (NA: under the threshold, not published) has no value and stays last
#          whichever way the column is ordered, so that the place of a row says nothing about a figure that is not
#          shown. For a name it is the name without accents or capitals (fold_name()), as in order_rows().
#   tr     data-pin = "bottom" (or "top"): a row that keeps its place whatever the order (a total).
# A row that moves holds no bslib tooltip or popover (info()): those are custom elements that break when they are
# taken out of the page and put back. A hover note inside a row is a plain element (tip()); in a title, info() is fine.
# The order a table arrives in is that of order_rows() too: the largest first, no value last, equal values by name.
sort_value <- function(v) {
  if (length(v) != 1 || is.na(v) || (is.numeric(v) && !is.finite(v))) return("")
  if (is.numeric(v)) sprintf("%.15g", v) else fold_name(as.character(v))
}
sort_table <- function(..., id = NULL, class = "table table-sm") tags$table(class = paste(class, "sortable"), `data-sort-id` = id, ...)
sort_th <- function(label, key, lang, type = "num", class = if (identical(type, "num")) "text-end", order = NULL, first = NULL, extra = NULL, ask = NULL)
  tags$th(class = class, `data-sort-key` = key, `data-sort-type` = type, `data-sort-first` = first, `aria-sort` = order,
          `data-sort-ask` = if (length(ask)) ask[[1]], `data-sort-ask-value` = if (length(ask) > 1) ask[[2]],
    tags$a(class = "sort", role = "button", tabindex = "0", title = TR("sp_sort", lang), label,
           if (!is.null(order)) span(class = "sort-mark", .noWS = "before", HTML(if (identical(order, "descending")) "&nbsp;&#9660;" else "&nbsp;&#9650;"))), extra)   # no space before the mark: it stays on the line of the last word of the title
sort_td <- function(shown, v, class = NULL, ...) tags$td(class = class, `data-v` = sort_value(v), ..., shown)

# --- The areas that border one (P-063) ----------------------------------------------------------------------
# `geo` = the polygons of one level (sf, lon/lat, a column territory_id), `boxes` = their bounding boxes
# (polygon_boxes()), `id` = the area. First the polygons whose box comes near the box of the area (a comparison of
# numbers: instant, whatever the size of the layer), then, among those few, the ones whose outline really comes
# within `tol` metres of the area's. An island has none. Returns the ids of the bordering polygons.
#
# Why a tolerance, and why 60 m (measured on 17 November 2026 on the outlines of the test copy):
# - Outlines taken as they are published are exact: in Spain's 8,044 municipalities, in Madrid's 131 neighbourhoods
#   and in Barcelona's 73, two areas either touch or stand more than 20 m apart. For them the tolerance changes
#   little: 60 m adds the pairs that face each other across a square without sharing a line (5 pairs to the 24,303
#   that touch among the municipalities, 3 to 414 in Madrid, 6 to 195 in Barcelona).
# - The update simplifies the outlines of twenty countries one polygon at a time (save_country_gpkg(), 50 to 500 m),
#   which pulls apart neighbours that share a short border. Tried on Madrid's and Barcelona's neighbourhoods
#   simplified that way at 100 m: asking for contact loses 5 to 6 % of the real neighbours; 15 m, 2 to 3 %; 60 m,
#   none or one. On municipalities of ordinary size at 150 m: contact loses 0.4 %, 60 m 0.1 %.
# - It does not grow with the size of the areas: large areas share long borders, which no simplification pulls
#   apart, and a tolerance of 0.4 % of their size (tried first) would have made neighbours of Ciudad Real and
#   Cáceres, which stand 1 km apart and share no border. One number for every level also means that when A borders
#   B, B borders A: a reader who clicks a neighbour finds, among its neighbours, the area he came from.
#
# The question is asked on the sphere (s2, which sf brings and uses for lon/lat). An outline the sphere refuses (a
# ring that crosses itself) sends it to a plane centred on the area, in metres (PROJ and GEOS, which sf brings
# too), and, should that fail as well, to plain degrees. No other package is needed. `engines` is there for the tests.
bordering <- function(geo, boxes, id, tol = 60, engines = c("sphere", "plane", "degrees")) {
  if (is.null(geo) || is.null(boxes) || length(id) != 1 || is.na(id)) return(character())
  i <- match(id, boxes$territory_id); if (is.na(i)) return(character())
  lat <- (boxes$ymin[i] + boxes$ymax[i]) / 2; lon <- (boxes$xmin[i] + boxes$xmax[i]) / 2
  k <- 111320; kx <- k * max(0.2, cos(lat * pi / 180))                               # metres in a degree, north-south and east-west
  dy <- tol / k; dx <- tol / kx
  near <- boxes$xmin <= boxes$xmax[i] + dx & boxes$xmax >= boxes$xmin[i] - dx & boxes$ymin <= boxes$ymax[i] + dy & boxes$ymax >= boxes$ymin[i] - dy
  near[i] <- FALSE; near[is.na(near)] <- FALSE
  cand <- boxes$territory_id[near]; if (length(cand) == 0) return(character())
  g <- geo[match(c(id, cand), geo$territory_id), ]; if (anyNA(g$territory_id)) return(character())
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326                                    # the boxes are in degrees: so are the outlines
  within <- function(x, d) lengths(sf::st_is_within_distance(x[-1, ], x[1, ], dist = d)) > 0
  ask <- list(sphere  = function() within(g, tol),
              plane   = function() within(sf::st_transform(g, sprintf("+proj=aeqd +lat_0=%.5f +lon_0=%.5f +datum=WGS84 +units=m", lat, lon)), tol),
              degrees = function() within(sf::st_set_crs(g, NA), dy))
  for (engine in engines) {
    hit <- tryCatch(suppressMessages(suppressWarnings(ask[[engine]]())), error = function(e) NULL)
    if (!is.null(hit) && length(hit) == length(cand)) return(cand[hit])
  }
  character()
}
# The block "bordering areas" of a profile: the chosen area among the areas that border it, with the figures of the
# same source, types and period, from the highest to the lowest (by rate when there is one), so that it shows at a
# glance whether the area is like its neighbours; over the table, the range and the median of the neighbours. An
# area in time, against its people and against the places around it: the three comparisons a figure needs.
# `rows` = territory_id, name, n (NA under the threshold), rate (NA without one), the chosen area among them.
# `missing` = how many bordering areas the source gives no figure for. A name is a link that sends its id to the
# input `click` (data-wm-send, app_styles.R). `name_col` = the title of the column of names (what the areas are).
# The table can be put in the order of any of its columns (P-067). NULL when the area borders nothing.
near_ui <- function(rows, id, lang, n_col, rate_col = NULL, n_text, click, missing = 0, name_col = "") {
  others <- rows[rows$territory_id != id, , drop = FALSE]
  if (nrow(others) == 0 && missing == 0) return(NULL)
  by_rate <- !is.null(rate_col) && any(!is.na(others$rate))
  rows <- order_rows(rows, if (by_rate) "rate" else "n", desc = TRUE)
  v <- if (by_rate) others$rate else others$n; v <- v[!is.na(v)]
  fmt <- function(x) if (by_rate) fmt_r(x, lang) else fmt_n(round(x), lang)
  tagList(
    h6(TR("near_title", lang), paste0(" (", nrow(others) + missing, ")"), info(TR("near_help", lang))),
    if (length(v) >= 2) div(class = "note mb-1", TR("near_range", lang, if (by_rate) rate_col else n_col, fmt(min(v)), fmt(max(v)), fmt(stats::median(v)))),
    if (nrow(rows) > 0) sort_table(id = "near", class = "table table-sm near-table",
      tags$thead(tags$tr(sort_th(name_col, "name", lang, "text"), sort_th(n_col, "n", lang, order = if (!by_rate) "descending"),
                         if (!is.null(rate_col)) sort_th(rate_col, "rate", lang, order = if (by_rate) "descending"))),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) { r <- rows[i, ]; me <- identical(r$territory_id, id)
        tags$tr(class = if (me) "sel",
          sort_td(if (me) r$name else tags$a(href = "#", `data-wm-send` = click, `data-wm-value` = r$territory_id, r$name), r$name),
          sort_td(n_text(r$n), r$n, "text-end"),
          if (!is.null(rate_col)) sort_td(if (is.na(r$rate)) "—" else fmt_r(r$rate, lang), r$rate, "text-end")) }))),
    if (missing > 0) div(class = "note", TR("near_more", lang, missing)))
}
# Rows of a table in the order of one of its columns: the largest first when `desc`, the rows without a value last,
# equal values by name; by name, in the order of the letters whatever their accents
order_rows <- function(d, col = "ratio", desc = TRUE) {
  nm <- fold_name(d$name)
  if (identical(col, "name")) return(d[order(nm, decreasing = desc, method = "radix"), , drop = FALSE])
  v <- d[[col]]
  d[order(is.na(v), if (desc) -v else v, nm, method = "radix"), , drop = FALSE]
}
block_name <- function(b, lang) TR(paste0("block_", b), lang)
readable_type <- function(x) { x <- tolower(sub("\\.\\.\\.$", "", x)); paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))) }
# "Rozas de Madrid (Las)" -> "Las Rozas de Madrid"; "Hospitalet de Llobregat (L')" -> "L'Hospitalet..."
pretty_name <- function(x) {
  x <- sub("^(.*), (.*)$", "\\2 \\1", x)                    # "Balears, Illes" -> "Illes Balears"
  x <- sub("^(.*) \\((L')\\)$", "\\2\\1", x)
  sub("^(.*) \\((El|La|Los|Las|Els|Les|Es|Sa|A|O|Os|As)\\)$", "\\2 \\1", x)
}
# Localised name of a reference row (Spanish in Spanish; elsewhere the English name, translated when the whole table is
# made of the names shared by every country)
local_name <- function(df, id_col, lang) setNames(if (lang == "es") df$name_es else tr_categories(df$name_en, lang), df[[id_col]])

# ⓘ: a hover tooltip; with `more` (an About section: glossary, rules, method) a click popover that ends with a link to it (U12)
info <- function(text, more = NULL, lang = "en") {
  if (is.null(more)) return(bslib::tooltip(span(class = "info", HTML("&#9432;")), text, placement = "right"))
  bslib::popover(span(class = "info", HTML("&#9432;")), tagList(if (inherits(text, "html")) text else span(text), tags$br(),
                 tags$a(class = "small", href = paste0("?view=about&lang=", lang, "#about-", more), TR("about_more", lang))), placement = "right")
}
# A hover note that survives being moved in the page: the filters the two tabs of a city share travel between the
# sidebar of the map and the bar of the statistics page, and a bslib tooltip (a custom element) cannot be attached
# twice. A plain element, served by one delegated Bootstrap tooltip on the body (JS in app_styles.R); the browser's
# own title is the fallback.
tip <- function(text) span(class = "info wm-tip", tabindex = "0", title = text, `data-bs-toggle` = "tooltip", `data-bs-placement` = "right", HTML("&#9432;"))
count_label <- function(text, n, lang, note = NA, mark = info) {
  tagList(text, span(class = "cnt", paste0("(", fmt_n(n, lang), ")")), if (!is.na(note %||% NA)) mark(note))
}
kpi <- function(v, l) div(class = "kpi", div(class = "v", v), div(class = "l", l))
lc <- function(x, lang = "en") lower_first(x, lang)   # "Police areas (ABP)" -> "police areas (ABP)" inside a sentence

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
    div(class = "cmp-head", span(class = "cmp-title", t("cmp_title", ref_label), info(t("cmp_note"), "glossary", lang)),
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

# --- The polygons of a layer to draw (P-057) -------------------------------------------------------------------
# Classifications change (NUTS editions): a layer can hold the polygons of an edition that no longer exists beside
# those of the current one (column `current`, FALSE for the old; a layer without the column is all current). A
# polygon of an old edition is drawn only when it has figures in the period on screen, and the polygons without
# figures are drawn first, so that an area with figures never lies under a grey one.
polygons_to_draw <- function(g, with_figures) {
  has <- g$territory_id %in% with_figures
  cur <- if ("current" %in% names(g)) !(g$current %in% c(FALSE, 0)) else rep(TRUE, nrow(g))
  g <- g[has | cur, ]
  g[order(g$territory_id %in% with_figures), ]
}

# --- The box over a map that says where the map is (P-058) --------------------------------------------------
# One row per level, from the widest to the narrowest: the word for the level and the name of the place the reader is in
# at that level ("—" when the map is not that far in yet). The row of the level shown on the map is marked `current`.
# A row with `send` is a link: a click sets that input to list(value, at) (the delegated handler of app_styles.R).
# rows: list of list(level, name, current, off, send, value)
where_box <- function(rows, title) {
  row <- function(r) {
    cls <- paste(c("where-row", if (isTRUE(r$current)) "current", if (isTRUE(r$off)) "off"), collapse = " ")
    inside <- tagList(span(class = "where-level", r$level), span(class = "where-name", title = r$name, r$name))
    if (!is.null(r$send)) tags$a(class = cls, href = "#", `data-wm-send` = r$send, `data-wm-value` = r$value %||% "", `aria-current` = if (isTRUE(r$current)) "true", inside)
    else div(class = cls, `aria-current` = if (isTRUE(r$current)) "true", inside)
  }
  div(class = "where", role = "navigation", `aria-label` = title, div(class = "where-title", title), div(class = "where-rows", lapply(rows, row)))
}

# --- The figures of a page as an Excel file (P-065) ---------------------------------------------------------------
# A page hands over what it shows as a named list of data frames, one per sheet, and writexl writes the file (a free
# package that needs neither Excel nor Java; the automated tests already use it). Without it the pages have no
# download button. What goes into a sheet is the page's business (city_workbook() in app_city.R); the rule it keeps:
# only what the page shows, a figure under the threshold as an empty cell with a note beside it.
HAS_XLSX <- requireNamespace("writexl", quietly = TRUE)
# Sheet names Excel accepts: none of \ / ? * [ ] :, no apostrophe at either end, 31 characters at most, no two alike
xl_sheet_names <- function(x) {
  x <- gsub("[][\\\\/?*:]", " ", as.character(x)); x <- trimws(gsub("[ \t\r\n]+", " ", x)); x <- gsub("^'+|'+$", "", x)   # plain spaces only: what else counts as a space depends on the computer
  x[is.na(x) | !nzchar(x)] <- "Sheet"; x <- substr(x, 1, 31)
  for (i in seq_along(x)[-1]) { k <- 1; base <- x[i]
    while (tolower(x[i]) %in% tolower(x[seq_len(i - 1)])) { k <- k + 1; end <- paste0(" (", k, ")"); x[i] <- paste0(substr(base, 1, 31 - nchar(end)), end) } }   # Excel takes "Data" and "data" for one name
  x
}
xl_write <- function(sheets, file) {
  names(sheets) <- xl_sheet_names(names(sheets))
  writexl::write_xlsx(sheets, path = file, col_names = TRUE, format_headers = FALSE)   # plain titles: a centred one is cut at both ends in a narrow column
  invisible(file)
}

# --- "Cite this data" (P-058) -----------------------------------------------------------------------------------
# The figures belong to their publisher: the citation names the original source (publisher, dataset, address, licence),
# the period shown and the day the source was last read, and says what the app did to the figures. The app is never
# given as the publisher of the data (it distributes none). The names of the publisher and of the dataset are the
# source's own; the words between them come from the translations.
# parts: list(publisher, title, url, licence, period, consulted, threshold)
cite_clean <- function(x) { if (is.null(x) || length(x) == 0) return(NULL); x <- trimws(as.character(x[1])); if (is.na(x) || !nzchar(x)) NULL else sub("[. ]+$", "", x) }
cite_lines <- function(parts, lang) {
  head <- c(cite_clean(parts$publisher), cite_clean(parts$title))
  url <- cite_clean(parts$url)
  c(paste0(paste(c(head, url), collapse = ". "), if (is.null(url) && length(head)) "."),
    if (!is.null(cite_clean(parts$licence))) paste0(TR("licence", lang, cite_clean(parts$licence)), "."),
    if (!is.null(cite_clean(parts$period))) paste0(TR("cite_period", lang, cite_clean(parts$period)), "."),
    if (!is.null(cite_clean(parts$consulted))) paste0(TR("cite_consulted", lang, cite_clean(parts$consulted)), "."),
    TR("cite_changes", lang, parts$threshold %||% MARTS$threshold))
}
# The same as a BibTeX entry (@misc): the publisher as a corporate author, the address apart, the rest in the note
cite_bibtex <- function(parts, lang, key = "source") {
  esc <- function(x) gsub("([%&_#$])", "\\\\\\1", gsub("[{}]", "", x %||% ""))
  year <- regmatches(cite_clean(parts$consulted) %||% "", regexpr("(19|20)[0-9]{2}", cite_clean(parts$consulted) %||% ""))
  note <- paste(cite_lines(parts, lang)[-1], collapse = " ")
  paste0("@misc{", gsub("[^a-z0-9]+", "_", tolower(key)), if (length(year)) paste0("_", year), ",\n",
         "  author       = {{", esc(cite_clean(parts$publisher)), "}},\n",
         "  title        = {", esc(cite_clean(parts$title)), "},\n",
         if (length(year)) paste0("  year         = {", year, "},\n"),
         if (!is.null(cite_clean(parts$url))) paste0("  howpublished = {\\url{", cite_clean(parts$url), "}},\n"),
         "  note         = {", esc(note), "}\n}")
}
cite_link <- function(lang) tags$a(class = "cite-link", href = "#", `data-wm-send` = "cite", `data-wm-value` = "open", TR("cite_link", lang))
cite_modal <- function(parts, lang, key = "source") {
  modalDialog(title = TR("cite_link", lang), easyClose = TRUE, footer = modalButton(TR("close", lang)),
    div(class = "cite-modal",
        p(TR("cite_intro", lang)),
        div(id = "cite-text", class = "cite-box", paste(cite_lines(parts, lang), collapse = "\n")),
        tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", `data-wm-copy` = "cite-text", `data-wm-done` = TR("cite_copied", lang), TR("cite_copy", lang)),
        tags$details(tags$summary("BibTeX"), tags$pre(id = "cite-bib", cite_bibtex(parts, lang, key)))))
}
