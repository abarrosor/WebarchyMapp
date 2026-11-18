# Styles and scripts: CSS, JavaScript (shared filters of a city that follow the tab, hover notes, the period bar of the time series, loading notice, phone bar, the profile card over a city's map), the page header, the navbar --------
# Part of the app: sourced by app/app.R (same environment); nothing here runs on its own.

# --- Styles -----------------------------------------------------------------------
# OUTPUT CONTAINERS. The div that uiOutput() / tableOutput() writes around its content, and the one of
# conditionalPanel(), are never given a class of this stylesheet, never addressed by their id here, and never set
# straight into a flex or grid container: what must be a box, or one item of a layout, is a plain div() around
# them. Since shiny 1.8.1 a page on Bootstrap 5 draws those containers as if they were not there (display:
# contents: their children take their place in the parent), so a position, a margin or a minimum height given to
# one does nothing, and in a grid its children scatter over the cells. The founder's computer has shiny 1.14; the
# copy this code is tried on had 1.8.0, where they are ordinary boxes: three checks that failed on his computer on
# 17 November 2026 showed it (P-064). A test holds the rule for every page.
# --- The two looks of the page: light and dark (P-070) -----------------------------------------------------------
# The page is light or dark as the reader's system says (the browser answers the question "prefers-color-scheme";
# nothing of the computer is read by this code or written anywhere), or as the reader chose with the button of the
# top bar, which the browser remembers for this app alone. The framework (Bootstrap 5.3) recolours its own parts when
# the page carries data-bs-theme="dark". This stylesheet does the same for its own: each of its colours has a name
# and two values, the light one first; the rules below name the colour and never write it.
# A map in the dark look (P-071, the founder's choice): the whole map, with its tiles, its areas, its legend, its zoom
# buttons and its labels, has light and dark exchanged and its hues kept, by one rule of the stylesheet. One rule for
# all of it, so that the areas and their legend can never disagree: the group with most is the strongest colour in
# both looks (the dark blue of the light map, the bright blue of the dark one). For that rule to turn round, the
# colours inside a map stay the light ones: a map carries data-bs-theme="light". The help texts of the maps say
# "a stronger colour", which is true of both looks; they said "darker" before. The charts are images drawn by R: see
# render_chart() in app_helpers.R.
THEME_COLOURS <- list(
  "surface"       = c("#fff",    "var(--bs-body-bg)"),         # the ground of a bar, a panel, a box
  "soft"          = c("#f1f5f9", "#2a3038"),                   # a ground set apart: a figure, a comparison, a reference row
  "soft-2"        = c("#f8fafc", "#24292f"),                   # the box of a citation
  "hover"         = c("#f1f3f5", "#343a40"),
  "sel"           = c("#fff7ed", "#47361c"),                   # the row of the area chosen
  "current"       = c("#e8f1fb", "#1e3a5c"),                   # the place of the page in a menu
  "ink"           = c("#1f2933", "#e9edf2"),
  "ink-strong"    = c("#0f172a", "#f8fafc"),
  "ink-btn"       = c("#222",    "#e9edf2"),
  "ink-2"         = c("#475569", "#b9c4d2"),                   # labels
  "ink-3"         = c("#64748b", "#9aa7b8"),                   # notes
  "ink-4"         = c("#666",    "#a7adb5"),
  "ink-off"       = c("#9aa5b1", "#6b7785"),                   # a level not reached yet
  "line"          = c("#e2e8f0", "#3a424b"),
  "line-b"        = c("#e5e7eb", "#3a424b"),
  "line-c"        = c("#d1d5db", "#495057"),
  "line-soft"     = c("#eef1f4", "#30363d"),
  "line-strong"   = c("#94a3b8", "#7c8a9b"),
  "field-line"    = c("#ced4da", "#495057"),
  "edge"          = c("rgba(0,0,0,.3)", "rgba(255,255,255,.28)"),
  "spin-track"    = c("#d6dde6", "#3a424b"),
  "busy-track"    = c("#cfe0f3", "#2a3f58"),
  "accent"        = c("#1d5fa6", "#6aa9e9"),                   # what can be clicked or is marked: darker on white, lighter on dark
  "accent-strong" = c("#174a82", "#8fc1f5"),
  "accent-2"      = c("#1d4ed8", "#7aa7ff"),
  "on-accent"     = c("#fff",    "#0b1b2b"),                   # ink on the accent colour
  "veil"          = c("rgba(255,255,255,.72)", "rgba(29,31,33,.72)"),
  "veil-busy"     = c("rgba(255,255,255,.78)", "rgba(29,31,33,.78)"),
  "where-bg"      = c("rgba(255,255,255,.95)", "rgba(29,31,33,.95)"))
theme_vars <- function(k) paste0("--wm-", names(THEME_COLOURS), ": ", vapply(THEME_COLOURS, `[`, "", k), ";", collapse = " ")
THEME_CSS <- paste0(
  "  :root, [data-bs-theme='light'] { ", theme_vars(1), " }\n",
  "  [data-bs-theme='dark'] { ", theme_vars(2), " }\n")
# The look is set before the page is drawn (first thing in <head>), so that a dark page never opens white
THEME_HEAD_JS <- "(function() { var t = null; try { t = localStorage.getItem('wm-theme'); } catch (e) {} if (t !== 'light' && t !== 'dark') t = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light'; document.documentElement.setAttribute('data-bs-theme', t); })();"
# The button: it says what a click will do (the two texts travel with it; the script writes the one that applies)
theme_button <- function(lang) tags$button(type = "button", class = "wm-theme", `data-to-dark` = TR("theme_dark", lang), `data-to-light` = TR("theme_light", lang), title = TR("theme_dark", lang), `aria-label` = TR("theme_dark", lang),
  HTML("<svg class='wm-theme-moon' viewBox='0 0 24 24' aria-hidden='true'><path d='M20.5 13.5A8.5 8.5 0 1 1 10.5 3.5a6.6 6.6 0 0 0 10 10z'/></svg><svg class='wm-theme-sun' viewBox='0 0 24 24' aria-hidden='true'><circle cx='12' cy='12' r='4'/><path d='M12 2v2.5M12 19.5V22M2 12h2.5M19.5 12H22M4.9 4.9l1.8 1.8M17.3 17.3l1.8 1.8M4.9 19.1l1.8-1.8M17.3 6.7l1.8-1.8'/></svg>"))
# The title of a page in its top bar: the button first (top left), then the name
page_title <- function(lang, name) tagList(theme_button(lang), span(class = "wm-title", title = name, name))   # the whole name stays at hand where a narrow screen cuts it

CSS <- paste0(THEME_CSS, "
  .bslib-page-navbar .bslib-sidebar-layout > .main { padding: 0; }
  /* Cities: the sidebar belongs to the map tab (a layout inside it); the statistics page has none and scrolls on its own */
  .tab-pane[data-value='map'], .tab-pane[data-value='stats'] { padding: 0 !important; gap: 0 !important; }
  #stats-page { flex: 1 1 auto; min-height: 0; overflow-y: auto; padding: 0 1.25rem 1.5rem; }
  .tab-pane[data-value='map'] .bslib-sidebar-layout.sidebar-collapsed > .main > *:first-child { margin-left: 0; }   /* the map keeps the whole width... */
  .bslib-sidebar-layout.wm-toggle-over-map .leaflet-top.leaflet-left { top: 3.4rem; }    /* ...and the zoom buttons of a map go below the sidebar button, where that button lies over the map */
  #map { height: 100% !important; min-height: 420px; }
  /* The button that hides and shows the sidebar: a visible tab above the map (leaflet controls sit at z-index 1000) */
  .bslib-sidebar-layout > .collapse-toggle { z-index: 1100; background: var(--wm-surface); border: 1px solid var(--wm-edge); border-radius: .5rem; box-shadow: 0 1px 5px rgba(0,0,0,.3); width: 1.9rem; height: 2.8rem; display: flex; align-items: center; justify-content: center; color: var(--wm-ink-btn); }
  .bslib-sidebar-layout > .collapse-toggle:hover { background: var(--wm-hover); }
  .bslib-sidebar-layout.sidebar-collapsed > .collapse-toggle { left: .35rem; }
  /* Room for that button, only where it lies over something (P-068). The script below asks the page itself and writes
     the answer on the layout: wm-toggle-over (over what the page holds), wm-toggle-over-map (over a map),
     wm-toggle-over-side (over the left of the first thing the open panel holds). An older bslib lays the button over
     the top left corner of the page; a newer one gives it, on a narrow screen, a row of its own, and room made for it
     there is a title indented under an empty band */
  .bslib-sidebar-layout.sidebar-collapsed.wm-toggle-over > .main > *:first-child { margin-left: 2.6rem; }   /* room for the button next to the first title */
  .bslib-sidebar-layout.wm-toggle-over-side > .sidebar > .sidebar-content > *:first-child, .bslib-sidebar-layout.wm-toggle-over-side .sidebar .shiny-input-container:has(#neighbourhood) { margin-left: 2.2rem; width: calc(100% - 2.2rem) !important; }   /* the first thing the open panel holds clears the button */
  /* Phones: the map first and tall, the profile and the filters below it, no dead margins */
  @media (max-width: 575.98px) {
    #map { height: 72vh !important; min-height: 60vh; }
    #sp_map { height: 60vh !important; }
    .bslib-sidebar-layout > .main, .bslib-sidebar-layout.sidebar-collapsed > .main { padding: 0 !important; }
    .bslib-sidebar-layout > .main > *:first-child, .bslib-sidebar-layout.sidebar-collapsed > .main > *:first-child, .bslib-sidebar-layout.sidebar-collapsed.wm-toggle-over > .main > *:first-child { margin-left: 0; }
    .sp-title { padding-top: .5rem; padding-left: .6rem; }
    .bslib-sidebar-layout.wm-toggle-over .sp-title { margin-left: 2.6rem; padding-left: 0; }   /* the title of a country beside the button, where the button lies over the page */
    .bslib-sidebar-layout > .collapse-toggle { top: .45rem; }
    .bslib-sidebar-layout:not(.sidebar-collapsed) > .main { visibility: hidden; }   /* an open sidebar takes the screen: the map and its controls wait behind */
    .bslib-sidebar-layout > .sidebar { background: var(--wm-surface); }
    .country-grid > *:nth-child(2) { order: -1; }
    .kpi .v { font-size: 1.05rem; }
    .table-sp { max-height: none; }
  }
  .bslib-sidebar-layout > .sidebar > .sidebar-content { padding: .75rem .85rem; gap: .5rem; }
  .sidebar .form-group, .sidebar .shiny-input-container { margin-bottom: .25rem; }
  .sidebar .control-label { font-weight: 600; font-size: .85rem; margin-bottom: .1rem; }
  .sidebar .accordion-button { padding: .55rem 1.6rem !important; font-weight: 600; font-size: .9rem; }
  .sidebar .accordion-body { padding: .3rem 1.6rem .8rem !important; }
  .sidebar .shiny-input-container:has(#neighbourhood) { width: calc(100% - 1.8rem) !important; }
  /* The button of the light and the dark look, first thing in the top bar (P-070): a moon to go dark, a sun to go light */
  .wm-theme { display: inline-flex; align-items: center; justify-content: center; width: 2rem; height: 2rem; padding: 0; margin: -.25rem .55rem -.25rem 0; vertical-align: middle; border: 1px solid var(--wm-field-line); border-radius: .4rem; background: transparent; color: inherit; cursor: pointer; }
  .wm-theme { flex: 0 0 auto; }
  .wm-theme:hover, .wm-theme:focus-visible { background: var(--wm-hover); }
  /* ...and the name after it gives way on a narrow screen (it ends in an ellipsis), so that the button never pushes the menu button off the bar */
  .navbar .navbar-brand:has(.wm-theme) { display: flex; align-items: center; min-width: 0; flex: 0 1 auto; padding-top: 0; padding-bottom: 0; }   /* a box now, where it was a run of text: without its padding the bar keeps its height */
  .navbar .navbar-brand .wm-title { min-width: 0; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  @media (max-width: 400px) { .navbar .navbar-brand .wm-title { font-size: 1.1rem; } .wm-theme { width: 1.8rem; height: 1.8rem; margin-right: .4rem; } }
  .wm-theme svg { width: 1.1rem; height: 1.1rem; fill: none; stroke: currentColor; stroke-width: 2; stroke-linecap: round; stroke-linejoin: round; }
  .wm-theme .wm-theme-sun, [data-bs-theme='dark'] .wm-theme .wm-theme-moon { display: none; }
  [data-bs-theme='dark'] .wm-theme .wm-theme-sun { display: inline; }
  /* A map in the dark look: light and dark exchanged, hues kept, for the whole map at once (see THEME_COLOURS above) */
  [data-bs-theme='dark'] .leaflet.html-widget { filter: invert(1) hue-rotate(180deg); }
  /* The dark look: what the names of the colours do not reach. The arrow of the box of types is drawn in the stylesheet */
  [data-bs-theme='dark'] .btn-outline-secondary { --bs-btn-color: var(--wm-ink-2); --bs-btn-border-color: var(--wm-line-c); --bs-btn-hover-color: var(--wm-ink); --bs-btn-hover-bg: var(--wm-hover); --bs-btn-hover-border-color: var(--wm-line-strong); --bs-btn-active-color: var(--wm-ink); --bs-btn-active-bg: var(--wm-hover); --bs-btn-active-border-color: var(--wm-line-strong); --bs-btn-disabled-color: var(--wm-ink-off); --bs-btn-disabled-border-color: var(--wm-line); }   /* its grey is fixed in the framework: unreadable on a dark ground */
  [data-bs-theme='dark'] #stats-bar .f-types > summary { background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='%23dee2e6' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3e%3c/svg%3e\"); }
  .info { cursor: help; color: var(--wm-ink-3); font-weight: 400; margin-left: .25rem; }
  .tooltip { pointer-events: none; }      /* a hover note never takes the pointer from its own icon (its arrow touches it) */
  .foot { font-size: .72rem; color: var(--wm-ink-3); margin-top: auto; }
  .facet { font-weight: 600; font-size: .85rem; margin: .5rem 0 .15rem; display: flex; justify-content: space-between; }
  .facet a { font-weight: 400; font-size: .78rem; }
  .cnt { color: var(--wm-ink-3); font-size: .78rem; margin-left: .2rem; }
  #types .checkbox, #block .radio { margin: 0 0 .1rem; }
  #types label, #block label { font-size: .85rem; }
  #types .checkbox label, #block .radio label { display: inline-flex; align-items: flex-start; max-width: 100%; }   /* a long name wraps under its own text, not under the box */
  #types .checkbox label > input, #block .radio label > input { flex: 0 0 auto; margin-top: .22em; }
  .chips a { font-size: .78rem; margin-right: .6rem; }
  /* The filters both tabs share: stacked in the sidebar of the map (the list of types open, 'all / none' on the line of its title)... */
  .wm-filters .f-types > summary { display: none; }
  .sidebar .f-types-wrap { position: relative; }
  .sidebar .f-types-links { position: absolute; top: .5rem; right: 0; font-size: .78rem; }
  /* ...and a bar across the top of the statistics page: area, category, types (a dropdown with the boxes in columns), period */
  #stats-bar { position: relative; z-index: 20; background: var(--wm-surface); border-bottom: 1px solid var(--wm-line); margin: 0 -1.25rem .9rem; padding: .3rem 1.25rem .55rem;
               display: grid; grid-template-columns: minmax(210px, 1.15fr) minmax(170px, .8fr) minmax(190px, .9fr) minmax(270px, 1.5fr); gap: .1rem 1.6rem; align-items: start; }
  #stats-filters, #stats-filters > .wm-filters { display: contents; }
  #stats-bar .f-group { min-width: 0; }
  #stats-bar .f-alert { grid-column: 1 / -1; margin-top: .4rem; }
  #stats-bar .shiny-input-container { margin-bottom: 0; width: 100% !important; }
  #stats-bar .facet { margin-top: .25rem; }
  #stats-bar .f-types > summary { display: block; list-style: none; cursor: pointer; border: 1px solid var(--wm-field-line); border-radius: .375rem; padding: .375rem 2.2rem .375rem .75rem; min-height: calc(1.5em + .75rem + 2px);
               white-space: nowrap; overflow: hidden; text-overflow: ellipsis; background: var(--wm-surface) url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'%3e%3cpath fill='none' stroke='%23343a40' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='m2 5 6 6 6-6'/%3e%3c/svg%3e\") no-repeat right .75rem center / 16px 12px; }
  #stats-bar .f-types > summary::-webkit-details-marker { display: none; }
  #stats-bar .f-types[open] > summary { border-color: #86b7fe; box-shadow: 0 0 0 .2rem rgba(13,110,253,.2); }
  #stats-bar .f-types-body { position: absolute; left: 1.25rem; right: 1.25rem; top: 100%; max-height: min(60vh, 540px); overflow-y: auto; background: var(--wm-surface); border: 1px solid var(--wm-field-line); border-radius: .375rem;
               box-shadow: 0 8px 22px rgba(0,0,0,.16); padding: .55rem .9rem .7rem; }      /* under the bar, as wide as the bar (the bar is its positioned ancestor) */
  #stats-bar .f-types-links { font-size: .8rem; margin-bottom: .35rem; padding-bottom: .3rem; border-bottom: 1px solid var(--wm-line-soft); }
  #stats-bar #types .shiny-options-group { column-width: 235px; column-gap: 1.5rem; }
  #stats-bar #types .checkbox { break-inside: avoid; }
  #stats-bar .filter-summary { display: none; }
  @media (min-width: 992px) and (min-height: 680px) { #stats-bar { position: sticky; top: 0; } }   /* stays in sight while the charts scroll */
  @media (max-width: 991.98px) { #stats-bar { grid-template-columns: 1fr 1fr; } }
  @media (max-width: 575.98px) {
    #stats-page { padding: 0 .75rem 1rem; }
    #stats-bar { grid-template-columns: 1fr; margin: 0 -.75rem .75rem; padding: .25rem .75rem .6rem; }
    #stats-bar .f-types-body { position: static; box-shadow: none; margin-top: .3rem; max-height: 50vh; }
  }
  /* The button that hands over the figures of the statistics page as an Excel file: a quiet row under the bar, at the right */
  .stats-tools { display: flex; justify-content: flex-end; align-items: center; gap: .1rem; margin: -.35rem 0 .15rem; }
  .stats-tools .btn { font-size: .78rem; padding: .15rem .6rem; }
  #types_plot { cursor: pointer; }
  /* The chart by type and the charts over time of a city: side by side from 1200 px of width (alone, each stretched
     from one edge of the page to the other). At the left the chart by type, in 5/12 of the width and never under 520
     px (its names need their room), and the time of year under it; at the right the series by number, the rate of
     each complete year and the period bar they share. On a narrower screen: one under the other (by type, over time,
     time of year), each under its rule. */
  .stats-pair { display: grid; grid-template-columns: minmax(0, 1fr); }
  @media (min-width: 1200px) {
    .stats-pair { grid-template-columns: minmax(520px, 5fr) minmax(0, 7fr); grid-template-rows: auto 1fr; column-gap: 2.5rem; align-items: start; }
    .stats-pair-types { grid-column: 1; grid-row: 1; }
    .stats-pair-series { grid-column: 2; grid-row: 1 / span 2; }
    .stats-pair-season { grid-column: 1; grid-row: 2; }
    .stats-pair-series > .ts-block > hr:first-child { display: none; }
  }
  /* A chart and its note: one under the other in a column of the page (and on a phone), side by side while the page
     is one column of a middling width */
  .stats-split { display: grid; grid-template-columns: minmax(0, 1fr); column-gap: 1.5rem; align-items: start; }
  @media (min-width: 768px) and (max-width: 1199.98px) { .stats-split { grid-template-columns: minmax(0, 7fr) minmax(0, 5fr); } }
  .ts-controls .shiny-input-container { margin-bottom: 0; }
  .ts-controls .shiny-input-radiogroup .form-check-inline { margin-right: .6rem; }
  /* The period bar of a time series: the small chart of the whole series, two handles (from, to) with their labels above
     them, the window between them and the two parts outside it (faded). Placed in % of the width by the script below.
     Under the chart in the cities; under the two charts (by number, by rate) in the countries, which share it. */
  .ts-nav { position: relative; padding: 0 18px; margin: .1rem 0 .3rem; user-select: none; -webkit-user-select: none; }
  .tsr-tags { position: relative; height: 1.45rem; }
  .tsr-tag { position: absolute; left: 0; bottom: 4px; white-space: nowrap; font-size: .72rem; font-weight: 600; line-height: 1; padding: .22rem .42rem; border-radius: .3rem; background: var(--wm-accent); color: var(--wm-on-accent); pointer-events: none; }
  .tsr-tag[hidden] { display: none; }
  .tsr-track { position: relative; border-top: 1px solid var(--wm-line-b); cursor: pointer; touch-action: pan-y; }
  .tsr-track img { pointer-events: none; -webkit-user-drag: none; }
  .tsr-dim, .tsr-win, .tsr-h { position: absolute; top: 0; bottom: var(--tsr-axis, 21px); display: none; }
  .ts-nav.tsr-on .tsr-dim, .ts-nav.tsr-on .tsr-win, .ts-nav.tsr-on .tsr-h { display: block; }
  .ts-nav:not(.tsr-on) .tsr-tag { display: none; }
  .tsr-dim { background: var(--wm-veil); pointer-events: none; }
  .tsr-dim-l { left: 0; } .tsr-dim-r { right: 0; }
  .tsr-win { box-sizing: border-box; border-top: 2px solid var(--wm-accent); border-bottom: 2px solid var(--wm-accent); background: rgba(49,130,189,.10); cursor: grab; }
  /* A handle: a line on the edge of the window and a grip outside it (to the left of 'from', to the right of 'to'), so
     that the two grips never cover each other nor the bars chosen, however narrow the window */
  .tsr-h { width: 28px; cursor: ew-resize; outline: none; }
  .tsr-h-from { margin-left: -22px; } .tsr-h-to { margin-left: -6px; }
  .tsr-h::before { content: ''; position: absolute; top: 0; bottom: 0; width: 2px; background: var(--wm-accent); }
  .tsr-h-from::before { left: 21px; } .tsr-h-to::before { left: 5px; }
  .tsr-h::after { content: ''; position: absolute; left: 7px; top: 50%; width: 14px; height: 34px; margin-top: -17px; box-sizing: border-box; border: 1px solid var(--wm-on-accent); box-shadow: 0 1px 3px rgba(0,0,0,.4);
                  background: var(--wm-accent) linear-gradient(90deg, transparent 3px, var(--wm-on-accent) 3px, var(--wm-on-accent) 4.5px, transparent 4.5px, transparent 7.5px, var(--wm-on-accent) 7.5px, var(--wm-on-accent) 9px, transparent 9px) no-repeat center / 100% 12px; }
  .tsr-h-from::after { border-radius: 5px 0 0 5px; } .tsr-h-to::after { border-radius: 0 5px 5px 0; }
  .tsr-h:hover::after, .ts-nav.tsr-dragging .tsr-h:focus::after { background-color: var(--wm-accent-strong); }
  .tsr-h:focus-visible::after { box-shadow: 0 0 0 3px rgba(13,110,253,.4); }
  .ts-nav.tsr-dragging, .ts-nav.tsr-dragging .tsr-win { cursor: grabbing; }
  /* Two charts over one period bar (the countries, and the cities that have a register): each chart keeps a line for
     the bar clicked, so that the chart and the bar under it do not move at a click */
  .ts-two:has(.ts-rate) .ts-line { min-height: 1.6rem; }
  .ts-two .ts-nav { margin-top: .55rem; }
  .period-pick { display: none; align-items: center; gap: .4rem; }
  .period-pick .shiny-input-container { margin-bottom: 0; flex: 1 1 0; width: auto !important; }
  /* Phones: a bar at the bottom with the page's tabs and a Menu button (U8); the month selectors replace the slider (U11) */
  #wm-bottom { display: none; }
  @media (max-width: 575.98px) {
    #wm-bottom { display: flex; position: fixed; left: 0; right: 0; bottom: 0; height: 3.1rem; background: var(--wm-surface); border-top: 1px solid var(--wm-line-c); z-index: 1150; padding-bottom: env(safe-area-inset-bottom); }
    #wm-bottom button { flex: 1 1 0; border: 0; background: none; font-size: .78rem; color: var(--wm-ink-2); padding: 0 .25rem; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
    #wm-bottom button.active { color: var(--wm-accent-2); font-weight: 700; box-shadow: inset 0 3px 0 var(--wm-accent-2); }
    body { padding-bottom: 3.1rem; }
    .period-pick { display: flex; }
    .shiny-input-container:has(> #period), .shiny-input-container:has(#period) { display: none; }
  }
  .chips a.active { font-weight: 700; text-decoration: none; color: var(--wm-ink); }
  .filter-summary { font-size: .75rem; color: var(--wm-ink-2); }
  .profile-head { display: flex; justify-content: space-between; align-items: start; }
  .profile h4 { margin: 0; font-size: 1.15rem; }
  .profile .sub { color: var(--wm-ink-3); font-size: .8rem; margin-bottom: .5rem; }
  .kpis { display: flex; gap: .4rem; margin-bottom: .6rem; }
  .kpi { flex: 1; background: var(--wm-soft); border-radius: .5rem; padding: .4rem .5rem; }
  .kpi .v { font-size: 1.2rem; font-weight: 650; line-height: 1.15; }
  .kpi .l { font-size: .7rem; color: var(--wm-ink-2); }
  .profile h6 { font-size: .82rem; font-weight: 650; margin: .6rem 0 .2rem; }
  .profile .table { font-size: .8rem; margin-bottom: .3rem; }
  .profile .alert { font-size: .76rem; padding: .35rem .55rem; margin: .35rem 0; }
  .profile details { font-size: .78rem; margin-top: .35rem; }
  .profile details ul { padding-left: 1.1rem; margin: .3rem 0; }
  .note { font-size: .76rem; color: var(--wm-ink-3); }
  .cmp { background: var(--wm-soft); border-radius: .5rem; padding: .5rem .65rem; margin: .4rem 0 .6rem; }
  .cmp-head { display: flex; justify-content: space-between; align-items: center; margin-bottom: .35rem; }
  .cmp-title { font-size: .72rem; color: var(--wm-ink-2); text-transform: uppercase; letter-spacing: .04em; }
  .cmp-pill { font-size: .78rem; font-weight: 700; padding: .1rem .55rem; border-radius: 999px; }
  .cmp-bar { height: 8px; background: var(--wm-line); border-radius: 999px; overflow: hidden; }
  .cmp-fill { height: 8px; border-radius: 999px; }
  .cmp-rate { text-align: center; font-weight: 700; font-size: 1.15rem; margin-top: .35rem; }
  .cmp-sub { text-align: center; font-size: .72rem; color: var(--wm-ink-3); }
  .about-toc { display: flex; flex-wrap: wrap; gap: .4rem 1rem; font-size: .85rem; margin: .25rem 0 1rem; }
  .about details > summary { cursor: pointer; font-size: .85rem; color: var(--wm-ink-2); margin-bottom: .5rem; }
  .table-sp { max-height: 68vh; overflow-y: auto; }
  .table-sp table { font-size: .82rem; }
  .table-sp thead th { position: sticky; top: 0; background: var(--wm-surface); }
  .table-sp tr.ref td { font-weight: 650; background: var(--wm-soft); }
  .table-sp tr.sel td { background: var(--wm-sel); font-weight: 650; }
  .table-sp a { cursor: pointer; }
  /* The bordering areas of a profile: a small table, the chosen area marked among its neighbours, each name a link */
  .near-table { font-size: .82rem; margin-bottom: .4rem; }
  .near-table th { font-weight: 600; color: var(--wm-ink-2); }
  .near-table tr.sel td { background: var(--wm-sel); font-weight: 650; }
  .near-table a { text-decoration: none; }
  .near-table a:hover, .near-table a:focus-visible { text-decoration: underline; }
  /* A column title orders its table by that column: the countries' table (on the server) and every table of class sortable (in the browser, P-067) */
  .table-sp thead a.sort, table.sortable thead a.sort { color: inherit; text-decoration: none; cursor: pointer; }
  .table-sp thead a.sort:hover, .table-sp thead a.sort:focus-visible, table.sortable thead a.sort:hover, table.sortable thead a.sort:focus-visible { text-decoration: underline; }
  .table-sp thead .sort-mark, table.sortable thead .sort-mark { color: var(--wm-accent); font-size: .72em; }
  .rank-wrap { overflow-x: auto; }                                                    /* a city's ranking on a narrow screen: the table scrolls inside its own box, the page does not */
  #sp_types .checkbox { margin: 0 0 .1rem; }
  #sp_types label { font-size: .85rem; }
  .navbar .form-select { width: auto; font-size: .85rem; padding: .2rem 1.8rem .2rem .5rem; }
  .navbar .dropdown-menu a.nav-link { padding: .3rem 1rem; }
  /* The menus of places of the top bar (countries, cities): the list keeps inside the window and scrolls; a search box stays on top of it */
  .navbar .wm-menu { min-width: 17rem; max-width: calc(100vw - 1rem); max-height: min(78vh, 40rem); overflow-y: auto; overscroll-behavior: contain; padding: 0 0 .35rem; z-index: 1200; }   /* above the map's own buttons and legend (1000) */
  .navbar .wm-menu.wm-plain { padding-top: .35rem; }
  .navbar .wm-menu.wm-end { right: 0 !important; left: auto !important; }          /* opened towards the left when it would leave the window on the right */
  .wm-menu-search { position: sticky; top: 0; z-index: 2; background: var(--bs-dropdown-bg, #fff); padding: .5rem .6rem .45rem; border-bottom: 1px solid var(--wm-line-soft); margin-bottom: .25rem; }
  .wm-menu .dropdown-item { display: flex; justify-content: space-between; align-items: baseline; gap: 1.2rem; padding: .32rem 1rem; font-size: .9rem; }
  .wm-menu .dropdown-item .sub { color: var(--wm-ink-3); font-size: .78rem; font-weight: 400; }
  .wm-menu .dropdown-item.wm-current { font-weight: 700; background: var(--wm-current); }     /* the place of this page */
  .wm-menu .dropdown-item.wm-first { background: var(--wm-soft); box-shadow: inset 3px 0 0 #3182bd; }   /* what Enter opens while typing */
  .wm-menu-empty { padding: .4rem 1rem; color: var(--wm-ink-3); font-size: .85rem; }
  .wm-menu li[hidden] { display: none !important; }
  @media (max-width: 575.98px) { .navbar .wm-menu { max-height: 52vh; max-width: none; } }
  .about { max-width: 960px; margin: 0 auto; padding: 1rem 1rem 3rem; }
  .about h2 { font-size: 1.15rem; margin: 1.8rem 0 .6rem; }
  .about p, .about li { font-size: .92rem; line-height: 1.5; }
  .about li { margin-bottom: .4rem; }
  .about table { font-size: .82rem; }
  .about table td, .about table th { vertical-align: top; padding: .35rem .5rem; }
  .about .upd { color: var(--wm-ink-4); font-size: .8rem; margin-top: 2rem; }
  /* Loading: a full-page notice while a place is fetched (shown on the click, until the new page arrives) and a
     thin bar under the top edge while Shiny recomputes (after 0.4 s, so that quick changes do not flash) */
  #busy-overlay { position: fixed; inset: 0; background: var(--wm-veil-busy); z-index: 3000; display: none; flex-direction: column; align-items: center; justify-content: center; gap: .9rem; color: var(--wm-ink); font-size: 1.05rem; text-align: center; padding: 1rem; }
  #busy-overlay.show { display: flex; }
  #busy-overlay .spin { width: 2.6rem; height: 2.6rem; border: .32rem solid var(--wm-spin-track); border-top-color: #4e79a7; border-radius: 50%; animation: wm-spin .9s linear infinite; }
  #busy-overlay .sub { color: var(--wm-ink-4); font-size: .85rem; }
  @keyframes wm-spin { to { transform: rotate(360deg); } }
  #busy-bar { position: fixed; top: 0; left: 0; width: 100%; height: 3px; z-index: 3001; pointer-events: none; opacity: 0; transition: opacity .2s .4s;
              background: linear-gradient(90deg, #4e79a7 0 35%, var(--wm-busy-track) 35%); background-size: 220% 100%; animation: wm-bar 1.1s linear infinite; }
  html.shiny-busy #busy-bar { opacity: 1; }
  @keyframes wm-bar { from { background-position: 120% 0; } to { background-position: -120% 0; } }
  /* What floats over a map (P-058): the box that says where the map is (top left) and, in a city, the card with the
     profile of the neighbourhood clicked (on the right; a sheet from the bottom when the map is narrow) */
  .map-wrap { position: relative; min-height: 0; }
  .where-box { position: absolute; top: .6rem; left: 3.5rem; z-index: 1000; max-width: calc(100% - 4.5rem); pointer-events: none; }
  .where { pointer-events: auto; overflow: hidden; background: var(--wm-where-bg); border-radius: .5rem; box-shadow: 0 1px 5px rgba(0,0,0,.3); padding: .3rem .65rem .4rem; font-size: .78rem; line-height: 1.25; }
  .where-title { font-size: .64rem; text-transform: uppercase; letter-spacing: .05em; color: var(--wm-ink-3); margin-bottom: .1rem; }
  .where-rows { display: table; border-spacing: 0; }
  .where-row { display: table-row; color: var(--wm-ink-2); text-decoration: none; }
  .where-level, .where-name { display: table-cell; padding: .05rem 0; vertical-align: baseline; }
  .where-level { padding-right: .65rem; white-space: nowrap; }
  .where-level::before { content: ''; display: inline-block; box-sizing: border-box; width: .5rem; height: .5rem; margin-right: .4rem; border-radius: 50%; border: 1.5px solid var(--wm-line-strong); background: var(--wm-surface); }
  .where-name { max-width: 13rem; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  .where-row.current { color: var(--wm-ink-strong); font-weight: 650; }
  .where-row.current .where-level::before { background: var(--wm-accent); border-color: var(--wm-accent); }
  .where-row.off { color: var(--wm-ink-off); }
  a.where-row { cursor: pointer; }
  a.where-row:hover, a.where-row:focus-visible { color: var(--wm-accent); }
  a.where-row:hover .where-level, a.where-row:focus-visible .where-level { text-decoration: underline; }
  .profile-card { display: none; position: absolute; top: .6rem; right: .6rem; bottom: 1.7rem; width: min(24rem, calc(100% - 1.2rem)); z-index: 1050; background: var(--wm-surface); border-radius: .6rem;
                  box-shadow: 0 2px 14px rgba(0,0,0,.3); overflow-y: auto; overscroll-behavior: contain; padding: .15rem .8rem .7rem; }
  .profile-card.open { display: block; }
  .profile-card-tools, .profile-dock-tools { text-align: right; line-height: 1; }
  .profile-card-tools button, .profile-dock-tools button { font-size: .72rem; color: var(--wm-ink-3); text-decoration: none; padding: .3rem 0 .1rem; border: 0; background: none; }
  .profile-card-tools button:hover, .profile-dock-tools button:hover { color: var(--wm-accent); text-decoration: underline; }
  .profile-dock-tools { display: none; }
  body.wm-nb-open:not(.wm-profile-on-map) .profile-dock-tools { display: block; }
  body.wm-nb-open.wm-profile-on-map #acc .accordion-item[data-value='profile'] { display: none; }   /* the profile is on the map: its place in the side panel is empty */
  .map-wrap.narrow .profile-card { top: auto; left: 0; right: 0; bottom: 0; width: auto; max-height: 62%; border-radius: .8rem .8rem 0 0; box-shadow: 0 -2px 14px rgba(0,0,0,.3); }
  .map-wrap.narrow .where-title { display: none; }
  .map-wrap.narrow .where { font-size: .72rem; padding: .25rem .5rem; }
  .map-wrap.narrow .where-name { max-width: 9.5rem; }
  /* Cite this data: a quiet link, and the text to copy */
  .cite-link { color: inherit; text-decoration: underline dotted; text-underline-offset: 2px; cursor: pointer; }
  .cite-box { white-space: pre-wrap; font-size: .84rem; background: var(--wm-soft-2); border: 1px solid var(--wm-line); border-radius: .4rem; padding: .6rem .7rem; margin: .5rem 0; }
  .cite-modal details { font-size: .8rem; margin-top: .7rem; }
  .cite-modal pre { font-size: .74rem; white-space: pre-wrap; background: var(--wm-soft-2); border: 1px solid var(--wm-line); border-radius: .4rem; padding: .5rem .6rem; margin-top: .3rem; }
")
JS <- "// Cities: one set of filters for both tabs. It sits in the sidebar of the map and moves to the bar at the top of the
// statistics page when that tab is shown (and back): the inputs are the same elements, so the choice is the same.
window.wmPlaceFilters = function(mode) {
  var f = document.getElementById('wm-filters'), slot = document.getElementById(mode === 'stats' ? 'stats-filters' : 'side-filters');
  if (!f || !slot) return;
  var d = f.querySelector('details.f-types');
  if (f.parentNode !== slot) { slot.appendChild(f); if (window.jQuery) setTimeout(function() { jQuery(window).trigger('resize'); }, 0); }
  if (d) d.open = (mode !== 'stats');                 // the sidebar shows the list of types, the bar a closed dropdown
};
(function() {   // the hover notes inside the filters (class wm-tip): one delegated tooltip, so that a moved note keeps working
  function init() { if (window.bootstrap && bootstrap.Tooltip && document.querySelector('.wm-tip, .wm-filters')) new bootstrap.Tooltip(document.body, { selector: '.wm-tip', placement: 'right', container: 'body' }); }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init); else init();
})();
document.addEventListener('shown.bs.tab', function(e) { var v = e.target && e.target.getAttribute && e.target.getAttribute('data-value'); if (v === 'stats' || v === 'map') window.wmPlaceFilters(v); });
document.addEventListener('click', function(e) { var d = document.querySelector('#stats-filters details.f-types[open]'); if (d && !d.contains(e.target)) d.open = false; });
document.addEventListener('keydown', function(e) { if (e.key !== 'Escape') return; var d = document.querySelector('#stats-filters details.f-types[open]'); if (d) d.open = false; });
Shiny.addCustomMessageHandler('mode', function(m) { document.body.classList.toggle('mode-stats', m === 'stats'); window.wmPlaceFilters(m === 'stats' ? 'stats' : 'map'); });
Shiny.addCustomMessageHandler('chip', function(id) {
  document.querySelectorAll('.chips:not(.ts-chips) a').forEach(function(a) { a.classList.toggle('active', a.id === id); });
});
Shiny.addCustomMessageHandler('chip_ts', function(id) {
  document.querySelectorAll('.ts-chips a').forEach(function(a) { a.classList.toggle('active', a.id === id); });
});
// The period bar of a time series (.ts-nav): two handles - from, to - over the small chart of the whole series and the
// window between them. The server sends the bars of the chart (the share of the width where each starts, and its
// label) and the chosen ones (message 'ts_range'); dragging a handle or the window, or pressing elsewhere on the bar
// (the nearest handle comes to the pointer), moves by whole bars and sends the choice back as input.<id> = {from, to, n}.
// The arrow keys move a handle that has the focus (Home / End: to the ends).
(function() {
  function clamp(v, lo, hi) { return Math.max(lo, Math.min(hi, v)); }
  function place(w) {
    var s = w._tsr; if (!s) return;
    w.classList.toggle('tsr-on', s.n > 0); if (!s.n) return;
    var a = s.edges[s.from - 1], b = s.edges[s.to], pct = function(x) { return (x * 100).toFixed(3) + '%'; };
    s.dimL.style.width = pct(a); s.dimR.style.left = pct(b); s.win.style.left = pct(a); s.win.style.width = pct(b - a);
    s.hF.style.left = pct(a); s.hT.style.left = pct(b);
    s.tagF.textContent = s.labels[s.from - 1]; s.tagT.textContent = s.labels[s.to - 1]; s.tagT.hidden = s.from === s.to;
    [[s.hF, s.from], [s.hT, s.to]].forEach(function(h) {
      h[0].setAttribute('aria-valuemin', 1); h[0].setAttribute('aria-valuemax', s.n); h[0].setAttribute('aria-valuenow', h[1]); h[0].setAttribute('aria-valuetext', s.labels[h[1] - 1]);
    });
    var W = s.track.clientWidth; if (!W) return;              // hidden (a tab not shown yet): placed again when it gets a size
    // The labels: each above its handle, inside the block; side by side above the window when they would touch
    var pad = s.track.offsetLeft, wf = s.tagF.offsetWidth, wt = s.tagT.hidden ? 0 : s.tagT.offsetWidth, gap = 4, lo = -pad, hi = W + pad;
    var xf = clamp(a * W - wf / 2, lo, hi - wf), xt = clamp(b * W - wt / 2, lo, hi - wt);
    if (s.tagT.hidden) xf = clamp((a + b) / 2 * W - wf / 2, lo, hi - wf);
    else if (xf + wf + gap > xt) { xf = clamp((a + b) / 2 * W - (wf + gap + wt) / 2, lo, hi - wf - gap - wt); xt = xf + wf + gap; }
    s.tagF.style.transform = 'translateX(' + Math.round(xf) + 'px)'; s.tagT.style.transform = 'translateX(' + Math.round(xt) + 'px)';
  }
  function nearest(s, x) { var k = 0, best = 9; for (var i = 0; i < s.edges.length; i++) { var d = Math.abs(s.edges[i] - x); if (d < best) { best = d; k = i; } } return k; }
  function share(s, e) { var r = s.track.getBoundingClientRect(); return r.width ? clamp((e.clientX - r.left) / r.width, 0, 1) : 0; }
  function drag(w, x) {
    var s = w._tsr, d = s.drag; if (!d) return;
    if (d.mode === 'from') s.from = clamp(nearest(s, x) + 1, 1, s.to);
    else if (d.mode === 'to') s.to = clamp(nearest(s, x), s.from, s.n);
    else { var len = d.to - d.from, f = clamp(nearest(s, s.edges[d.from - 1] + x - d.x) + 1, 1, s.n - len); s.from = f; s.to = f + len; }   // the window: the same number of bars
    place(w);
  }
  function send(w, now) {
    var s = w._tsr; clearTimeout(s.timer);
    var go = function() {
      var k = s.n + ':' + s.from + ':' + s.to; if (k === s.sent || !window.Shiny || !Shiny.setInputValue) return;
      s.sent = k; Shiny.setInputValue(w.id, { from: s.from, to: s.to, n: s.n }, { priority: 'event' });
    };
    if (now) go(); else s.timer = setTimeout(go, 300);        // while dragging: when the pointer rests
  }
  function down(w, e) {
    var s = w._tsr; if (!s.n || (e.pointerType === 'mouse' && e.button !== 0)) return;
    var x = share(s, e), t = e.target, mode = t === s.hF ? 'from' : t === s.hT ? 'to' : t === s.win ? 'move' : null, jump = !mode;
    if (jump) mode = Math.abs(x - s.edges[s.from - 1]) <= Math.abs(x - s.edges[s.to]) ? 'from' : 'to';
    s.drag = { mode: mode, x: x, from: s.from, to: s.to };
    try { s.track.setPointerCapture(e.pointerId); } catch (err) {}
    w.classList.add('tsr-dragging'); e.preventDefault();
    if (mode !== 'move') { try { (mode === 'from' ? s.hF : s.hT).focus({ preventScroll: true }); } catch (err) {} }
    if (jump) { drag(w, x); send(w, false); }
  }
  function move(w, e) { var s = w._tsr; if (!s.drag) return; drag(w, share(s, e)); send(w, false); }
  function up(w, e) {
    var s = w._tsr; if (!s.drag) return;
    s.drag = null; w.classList.remove('tsr-dragging'); try { s.track.releasePointerCapture(e.pointerId); } catch (err) {}
    send(w, true);
  }
  function key(w, e) {
    var s = w._tsr, h = e.target; if (!s || !s.n || (h !== s.hF && h !== s.hT)) return;
    var step = { ArrowLeft: -1, ArrowDown: -1, ArrowRight: 1, ArrowUp: 1, PageDown: -4, PageUp: 4, Home: -s.n, End: s.n }[e.key]; if (!step) return;
    e.preventDefault();
    if (h === s.hF) s.from = clamp(s.from + step, 1, s.to); else s.to = clamp(s.to + step, s.from, s.n);
    place(w); send(w, false);
  }
  function state(w) {
    if (w._tsr) return w._tsr;
    var q = function(c) { return w.querySelector(c); };
    var s = w._tsr = { n: 0, from: 1, to: 1, edges: [0, 1], labels: [''], drag: null, sent: '', timer: null, track: q('.tsr-track'), win: q('.tsr-win'),
                       dimL: q('.tsr-dim-l'), dimR: q('.tsr-dim-r'), hF: q('.tsr-h-from'), hT: q('.tsr-h-to'), tagF: q('.tsr-tag-from'), tagT: q('.tsr-tag-to') };
    s.track.addEventListener('pointerdown', function(e) { down(w, e); });
    s.track.addEventListener('pointermove', function(e) { move(w, e); });
    s.track.addEventListener('pointerup', function(e) { up(w, e); });
    s.track.addEventListener('pointercancel', function(e) { up(w, e); });
    w.addEventListener('keydown', function(e) { key(w, e); });
    if (window.ResizeObserver) new ResizeObserver(function() { place(w); }).observe(s.track);
    return s;
  }
  Shiny.addCustomMessageHandler('ts_range', function(m) {
    var w = document.getElementById(m.id); if (!w) return;
    var s = state(w), n = m.n || 0;
    if (s.drag && n === s.n) return;                            // the echo of a choice that is still being dragged
    s.drag = null; w.classList.remove('tsr-dragging'); s.n = n;
    if (n) { s.edges = [].concat(m.edges); s.labels = [].concat(m.labels); s.from = clamp(m.from, 1, n); s.to = clamp(m.to, s.from, n); s.sent = n + ':' + s.from + ':' + s.to; }
    place(w);
  });
})();
// Loading notice: shown when a link to another view is clicked (the new page replaces it when it arrives;
// a heavy country takes seconds the first time), hidden again if the page comes back from the browser cache
window.wmLoading = function(what) {
  var o = document.getElementById('busy-overlay'); if (!o) return;
  o.querySelector('.txt').textContent = (o.getAttribute('data-loading') || 'Loading %s').replace('%s', what || '');
  o.classList.add('show');
};
document.addEventListener('click', function(e) {
  var a = e.target.closest ? e.target.closest('a[href^=\"?view=\"]') : null;
  if (!a || e.ctrlKey || e.metaKey || e.shiftKey || e.button !== 0) return;
  window.wmLoading(a.getAttribute('data-name') || a.textContent.trim());
});
// The menus of places of the top bar (countries, cities): type to filter the list (accents and case apart; the code and
// the English and local names find a place too), Enter opens the best match, the arrows walk the list, and the menu
// opens towards the side where it fits. The links stay links: nothing here is needed to follow one.
(function() {
  var LETTERS = {'\\u0131': 'i', '\\u0142': 'l', '\\u00f8': 'o', '\\u0111': 'd', '\\u00f0': 'd', '\\u00fe': 'th', '\\u00e6': 'ae', '\\u0153': 'oe', '\\u00df': 'ss'};
  function norm(s) { s = String(s || '').toLowerCase(); if (s.normalize) s = s.normalize('NFD').replace(/[\\u0300-\\u036f]/g, '');
    return s.replace(/[\\u0131\\u0142\\u00f8\\u0111\\u00f0\\u00fe\\u00e6\\u0153\\u00df]/g, function(c) { return LETTERS[c]; }); }
  function setup(menu) {
    if (menu._wm) return; menu._wm = true;
    var input = menu.querySelector('.wm-menu-search input'), empty = menu.querySelector('.wm-menu-empty');
    var items = Array.prototype.slice.call(menu.querySelectorAll('a.dropdown-item'));
    items.forEach(function(a) { var n = a.getAttribute('data-name') || a.textContent; a._name = norm(n); a._key = norm(n + ' ' + (a.getAttribute('data-k') || '') + ' ' + a.textContent); });
    var shown = items.slice();
    function visible() { return shown; }
    // The places that hold the text, the best matches first: a name that starts with it, then a word that does (another
    // name of the place, its country), then any place holding it; the list's own order inside each group and when the
    // box is empty. The first one is marked: it is what Enter opens.
    function filter() {
      var q = input ? norm(input.value).trim() : '';
      var ranked = items.map(function(a, i) {
        var s = !q ? 2 : a._name.indexOf(q) === 0 ? 0 : (' ' + a._key).indexOf(' ' + q) >= 0 ? 1 : a._key.indexOf(q) >= 0 ? 2 : 9;
        a.parentNode.hidden = s === 9; a.classList.remove('wm-first');
        return { a: a, s: s, i: i };
      }).sort(function(x, y) { return (x.s - y.s) || (x.i - y.i); });
      ranked.forEach(function(r) { menu.insertBefore(r.a.parentNode, empty || null); });
      shown = ranked.filter(function(r) { return r.s < 9; }).map(function(r) { return r.a; });
      if (q && shown.length) shown[0].classList.add('wm-first');
      if (empty) empty.hidden = shown.length > 0;
      return q && shown.length ? shown[0] : null;
    }
    menu._wmReset = function() { if (input) input.value = ''; filter(); };
    if (!input) return;
    menu._wmInput = input; menu._wmVisible = visible;
    input.addEventListener('input', function() { filter(); menu.scrollTop = 0; });
    input.addEventListener('keydown', function(e) {
      if (e.key === 'Enter') { var b = filter(); e.preventDefault(); if (b) b.click(); }
      else if (e.key === 'ArrowDown' && shown.length) { e.preventDefault(); e.stopPropagation(); shown[0].focus(); }
    });
  }
  // Keys pressed on a place of the list. Bootstrap walks the list with the arrows from a listener that runs first (on the
  // document, capturing) and lets nothing through, so this one sits on the window: up from the first place goes back
  // to the box, and a letter typed on the list goes on in the box.
  window.addEventListener('keydown', function(e) {
    var t = e.target; if (!t || !t.classList || !t.classList.contains('dropdown-item') || !t.closest) return;
    var menu = t.closest('.wm-menu'); if (!menu || !menu._wmInput) return;
    var v = menu._wmVisible();
    if (e.key === 'ArrowUp' && v.length && t === v[0]) { e.preventDefault(); e.stopPropagation(); menu._wmInput.focus(); }
    else if (e.key && e.key.length === 1 && e.key !== ' ' && !e.ctrlKey && !e.metaKey && !e.altKey) menu._wmInput.focus();
  }, true);
  function menuOf(e) { var p = e.target && e.target.parentNode; return p && p.querySelector ? p.querySelector('.wm-menu') : null; }
  document.addEventListener('shown.bs.dropdown', function(e) {
    var menu = menuOf(e); if (!menu) return; setup(menu);
    menu.classList.remove('wm-end'); menu.style.maxHeight = '';
    if (getComputedStyle(menu).position === 'absolute' && menu.getBoundingClientRect().right > document.documentElement.clientWidth - 4) menu.classList.add('wm-end');
    // Never past the bottom of the window (nor under the phone's bottom bar): the list scrolls inside what is left
    var r = menu.getBoundingClientRect(), bar = document.getElementById('wm-bottom');
    var room = window.innerHeight - r.top - 10 - (bar && getComputedStyle(bar).display !== 'none' ? bar.offsetHeight : 0);
    if (r.height > room && room >= 160) menu.style.maxHeight = Math.floor(room) + 'px';
    var cur = menu.querySelector('a.wm-current');      // the place of this page in the middle of the list
    if (cur) menu.scrollTop = Math.max(0, cur.getBoundingClientRect().top - menu.getBoundingClientRect().top + menu.scrollTop - menu.clientHeight / 2);
    var input = menu.querySelector('.wm-menu-search input');
    if (input && window.matchMedia && window.matchMedia('(hover: hover) and (pointer: fine)').matches) input.focus();   // not on a touch screen: its keyboard would cover the list
  });
  document.addEventListener('hidden.bs.dropdown', function(e) { var menu = menuOf(e); if (menu && menu._wmReset) menu._wmReset(); });
  function init() { Array.prototype.forEach.call(document.querySelectorAll('.wm-menu'), setup); }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init); else init();
})();
window.addEventListener('pageshow', function() { var o = document.getElementById('busy-overlay'); if (o) o.classList.remove('show'); });
// Phones: a bar at the bottom with the page's tabs (Map / Statistics of a city; Map / Table / Series of a country) and a
// Menu button that opens the top menu (places, About, language). Built from the links the page already has.
(function() {
  function build() {
    if (document.getElementById('wm-bottom')) return;
    var tabs = Array.from(document.querySelectorAll('.navbar .navbar-nav a.nav-link[data-bs-toggle=\"tab\"]'));
    if (tabs.length < 2) tabs = Array.from(document.querySelectorAll('#sp_view a.nav-link[data-bs-toggle=\"tab\"]'));
    var bar = document.createElement('nav'); bar.id = 'wm-bottom';
    tabs.forEach(function(a) {
      var b = document.createElement('button'); b.type = 'button'; b.textContent = a.textContent.trim(); if (a.classList.contains('active')) b.className = 'active';
      b.addEventListener('click', function() { if (window.bootstrap) bootstrap.Tab.getOrCreateInstance(a).show(); else a.click(); window.scrollTo(0, 0); });
      a.addEventListener('shown.bs.tab', function() { Array.from(bar.children).forEach(function(c) { c.classList.remove('active'); }); b.classList.add('active'); });
      bar.appendChild(b);
    });
    var tg = document.querySelector('.navbar-toggler, .navbar-toggle');   // bslib names it navbar-toggle
    if (tg) { var m = document.createElement('button'); m.type = 'button'; m.textContent = (window.wmLabels && window.wmLabels.menu) || 'Menu'; m.addEventListener('click', function() { window.scrollTo(0, 0); tg.click(); }); bar.appendChild(m); }
    document.body.appendChild(bar);
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', build); else build();
})();
// Cities: the profile of the neighbourhood clicked opens in a card over the map, with a cross to close it (Escape closes
// it too). A link in the card moves the profile to the side panel, and one there brings it back: the choice is kept in
// this browser. The card and the side panel show the same output (#profile), moved from one to the other. It is moved
// empty: the notes inside it are custom elements that cannot be attached to the page twice, so its content is
// thrown away before the move and the server is asked to send it again (input profile_moved).
(function() {
  var KEY = 'wm-profile-place';
  function where() { try { return localStorage.getItem(KEY) === 'side' ? 'side' : 'map'; } catch (e) { return 'map'; } }
  function move(p, target) {
    if (p.parentNode === target) return;
    if (window.Shiny && Shiny.unbindAll) Shiny.unbindAll(p);
    if (window.jQuery) jQuery(p).empty(); else p.innerHTML = '';
    target.appendChild(p);
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('profile_moved', Date.now(), { priority: 'event' });
    if (window.jQuery) setTimeout(function() { jQuery(p).trigger('shown'); }, 0);      // Shiny looks again at what is on screen
  }
  function place() {
    var p = document.getElementById('profile'), card = document.getElementById('profile-card'), body = document.getElementById('profile-card-body'), dock = document.getElementById('profile-dock');
    if (!p || !card || !body || !dock) return;
    var open = document.body.classList.contains('wm-nb-open'), onMap = where() === 'map';
    card.classList.toggle('open', onMap && open);
    document.body.classList.toggle('wm-profile-on-map', onMap);
    move(p, (onMap && open) ? body : dock);
  }
  function tell() { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('profile_place', where()); }
  Shiny.addCustomMessageHandler('profile', function(m) {
    var was = document.body.classList.contains('wm-nb-open');
    document.body.classList.toggle('wm-nb-open', !!(m && m.open)); place();
    var card = document.getElementById('profile-card'); if (card && m && m.open && (!was || m.changed)) card.scrollTop = 0;
  });
  document.addEventListener('click', function(e) {
    var b = e.target && e.target.closest ? e.target.closest('#profile-dock-btn, #profile-float-btn') : null; if (!b) return;
    try { localStorage.setItem(KEY, b.id === 'profile-dock-btn' ? 'side' : 'map'); } catch (err) {}
    place(); tell();
  });
  document.addEventListener('keydown', function(e) {
    if (e.key !== 'Escape' || !document.querySelector('#profile-card.open') || document.querySelector('.modal.show')) return;
    var x = document.getElementById('close_profile'); if (x) x.click();
  });
  // A narrow map (a phone, a small window): the card is a sheet at the bottom, and the server is told, so that the map
  // leaves room below the neighbourhood and not at its right
  function watch() {
    var w = document.querySelector('.map-wrap'); if (!w) return;
    var last = null;
    var check = function() {
      if (!w.clientWidth) return;
      var n = w.clientWidth < 720; w.classList.toggle('narrow', n);
      if (n !== last) { last = n; if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('map_narrow', n); }
    };
    if (window.ResizeObserver) new ResizeObserver(check).observe(w); else window.addEventListener('resize', check);
    check();
  }
  function start() { place(); tell(); watch(); }
  if (window.jQuery) jQuery(document).on('shiny:connected', start); else document.addEventListener('DOMContentLoaded', start);
})();
// Links that send one value to the server (the levels of the box over the map, Cite this data), and the button that
// copies a text
document.addEventListener('click', function(e) {
  var a = e.target && e.target.closest ? e.target.closest('a[data-wm-send]') : null; if (!a) return;
  e.preventDefault();
  if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(a.getAttribute('data-wm-send'), { value: a.getAttribute('data-wm-value') || '', at: Date.now() }, { priority: 'event' });
});
// The light and the dark look (P-070). The page opens in the look of the reader's system (the browser answers the
// question prefers-color-scheme: nothing of the computer is read or kept), set before it is drawn by the few lines at
// the top of <head>. The button of the top bar turns it round. A choice that differs from the system's is remembered by
// the browser, for this app alone (localStorage, key wm-theme: it never leaves the browser); a choice that is the
// system's own is not kept, so the page follows the system again, also when the system changes while the page is
// open. The server is told the look in input.theme, for one thing only: to draw its charts in that ink.
// Inside a map the colours stay the light ones (data-bs-theme=light, data-wm-keep): the stylesheet turns the whole map
// round in the dark look (see THEME_COLOURS). Other parts that the framework gave a look of their own (the top bar of
// bslib 0.12 carries data-bs-theme=auto) follow the page, and get their own back in the light look.
(function() {
  var KEY = 'wm-theme', media = window.matchMedia ? window.matchMedia('(prefers-color-scheme: dark)') : null;
  var system = function() { return media && media.matches ? 'dark' : 'light'; };
  var stored = function() { try { var t = localStorage.getItem(KEY); return t === 'light' || t === 'dark' ? t : null; } catch (e) { return null; } };
  var chosen = stored();
  var now = function() { return chosen || system(); };
  var apply = function() {
    var t = now(), root = document.documentElement; if (root.getAttribute('data-bs-theme') !== t) root.setAttribute('data-bs-theme', t);
    Array.prototype.forEach.call(document.querySelectorAll('.leaflet.html-widget'), function(el) { if (!el.hasAttribute('data-wm-keep')) { el.setAttribute('data-wm-keep', '1'); el.setAttribute('data-bs-theme', 'light'); } });
    Array.prototype.forEach.call(document.querySelectorAll('[data-bs-theme]'), function(el) {
      if (el === root || el.hasAttribute('data-wm-keep')) return;
      if (!el.hasAttribute('data-wm-theme-was')) el.setAttribute('data-wm-theme-was', el.getAttribute('data-bs-theme'));
      var want = t === 'dark' ? 'dark' : el.getAttribute('data-wm-theme-was'); if (el.getAttribute('data-bs-theme') !== want) el.setAttribute('data-bs-theme', want);
    });
    Array.prototype.forEach.call(document.querySelectorAll('.wm-theme'), function(b) {
      var label = b.getAttribute(t === 'dark' ? 'data-to-light' : 'data-to-dark') || '';
      b.setAttribute('title', label); b.setAttribute('aria-label', label); b.setAttribute('aria-pressed', t === 'dark' ? 'true' : 'false');
    });
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('theme', t);
  };
  document.addEventListener('click', function(e) {
    var b = e.target && e.target.closest ? e.target.closest('.wm-theme') : null; if (!b) return;
    e.preventDefault();
    var next = now() === 'dark' ? 'light' : 'dark'; chosen = next === system() ? null : next;
    try { if (chosen) localStorage.setItem(KEY, chosen); else localStorage.removeItem(KEY); } catch (err) {}
    apply();
  });
  if (media) { if (media.addEventListener) media.addEventListener('change', apply); else if (media.addListener) media.addListener(apply); }
  window.addEventListener('storage', function(e) { if (e.key === KEY) { chosen = stored(); apply(); } });   // the app open in another tab
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', apply); else apply();
  if (window.jQuery) jQuery(document).on('shiny:connected', apply);
  window.wmTheme = { now: now, system: system, apply: apply };
})();
// The button that opens and closes the side panel: room is made for it only where it lies over something (P-068).
// An older bslib lays the button over the top left corner of the page, and the stylesheet makes room for it there
// (the first title moves right, the zoom buttons of a map move down, the first field of the open panel moves right).
// A newer one (0.12, the founder's) gives the button, on a narrow screen, a row of its own above the page: the room
// is then not needed, and the title of a country stood indented under an empty band. So the room is no longer a rule
// that always holds but the answer to three questions put to the page itself, each written as a class on the layout:
//   wm-toggle-over       the button lies over what the page holds (its padding aside)
//   wm-toggle-over-map   the button lies over a map of the page
//   wm-toggle-over-side  the button lies over the row of the first thing the open panel holds, on its left half
// The questions are put again when the window changes size, a tab is shown or the panel opens or closes. The classes
// change nothing the answers depend on (the button, the box of the page, the map and the row keep their places), so
// asking again after answering cannot go round in circles.
(function() {
  var box = function(el) { return el.getBoundingClientRect(); };
  var has = function(r) { return r.right - r.left > 0 && r.bottom - r.top > 0; };
  // To lie over means by 8 px or more each way: in bslib 0.12 the button ends 2 px inside the row of the first field of the open panel and covers nothing of it
  var over = function(a, b) { return has(a) && has(b) && Math.min(a.right, b.right) - Math.max(a.left, b.left) >= 8 && Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top) >= 8; };
  var inner = function(el) { var r = box(el), s = getComputedStyle(el), n = function(v) { return parseFloat(v) || 0; };
    return { left: r.left + n(s.paddingLeft), right: r.right - n(s.paddingRight), top: r.top + n(s.paddingTop), bottom: r.bottom - n(s.paddingBottom) }; };
  var child = function(of, cls) { for (var i = 0; i < of.children.length; i++) if (of.children[i].classList.contains(cls)) return of.children[i]; return null; };
  var set = function(el, cls, on) { if (el.classList.contains(cls) !== on) el.classList.toggle(cls, on); };
  var look = function() {
    Array.prototype.forEach.call(document.querySelectorAll('.bslib-sidebar-layout'), function(layout) {
      var toggle = child(layout, 'collapse-toggle'), main = child(layout, 'main'), side = child(layout, 'sidebar');
      if (!toggle || !main || !has(box(layout))) return;   // a tab that is not shown keeps its answers: asked again when it is
      var t = box(toggle), maps = main.querySelectorAll('.leaflet-container, .leaflet.html-widget'), onMap = false, onSide = false, i;
      for (i = 0; i < maps.length; i++) if (over(t, box(maps[i]))) onMap = true;
      var content = side && (child(side, 'sidebar-content') || side), first = content && content.firstElementChild;
      if (first) { var c = inner(content), f = box(first); onSide = over(t, { left: c.left, right: c.right, top: f.top, bottom: f.bottom }) && t.left + t.right < c.left + c.right; }
      set(layout, 'wm-toggle-over', over(t, inner(main))); set(layout, 'wm-toggle-over-map', onMap); set(layout, 'wm-toggle-over-side', onSide);
    });
  };
  var soon = function() { look(); setTimeout(look, 120); setTimeout(look, 700); };   // now, once the page has settled, and once the panel has finished sliding
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', soon); else soon();
  window.addEventListener('load', soon); window.addEventListener('resize', soon); document.addEventListener('shown.bs.tab', soon);
  if (window.MutationObserver) new MutationObserver(function(ms) { for (var i = 0; i < ms.length; i++) if (ms[i].target.classList && ms[i].target.classList.contains('bslib-sidebar-layout')) { soon(); return; } })
    .observe(document.documentElement, { attributes: true, attributeFilter: ['class'], subtree: true });
  window.wmToggleLook = look;
})();
// Tables in the order of a column (P-067). A table of class sortable is put in the order of a column by a click on the
// title of that column (a.sort inside a th that names the column in data-sort-key and says in data-sort-type whether
// it holds numbers or text), and turned round by a second click: numbers from the largest at the first click, text
// from A (data-sort-first says otherwise). The value of a cell is its data-v, which the server writes from the very
// value the cell shows; a cell with an empty data-v has no value and stays last whichever way the column is ordered,
// so that the place of a row says nothing about a figure that is not shown. Equal values go by the first column of
// text, from A. A row with data-pin keeps its place (a total). A column of text without data-v goes by its text,
// accents and capitals aside. A table with data-sort-id keeps the order the reader chose when the server draws it
// again; the countries' table of territories is not one of these: the server orders it (app_country.R). A title with
// data-sort-ask does not order the rows on screen: it sends its value to the server through that input, and the
// server draws the table again by that column (a city's ranking, where the column decides which areas are listed).
(function() {
  var kept = {};
  var fold = function(s) { s = (s || '').toLowerCase(); if (s.normalize) s = s.normalize('NFD').replace(/[\\u0300-\\u036f]/g, ''); return s.replace(/\\s+/g, ' ').trim(); };
  var value = function(td, type) {
    if (!td) return null;
    var v = td.getAttribute('data-v');
    if (type === 'num') { if (v === null || v === '') return null; v = parseFloat(v); return isNaN(v) ? null : v; }
    if (v === null) v = fold(td.textContent);
    return (v === '' || v === '\\u2014') ? null : v;
  };
  var state = function(table) { var th = table.querySelector('thead th[aria-sort]'); return th ? { key: th.getAttribute('data-sort-key'), desc: th.getAttribute('aria-sort') === 'descending' } : null; };
  var order = function(table, key, desc) {
    var ths = Array.prototype.slice.call(table.querySelectorAll('thead th')), col = -1, names = -1;
    ths.forEach(function(th, i) { if (th.getAttribute('data-sort-key') === key) col = i; if (names < 0 && th.getAttribute('data-sort-type') === 'text') names = i; });
    if (col < 0) return false;
    var type = ths[col].getAttribute('data-sort-type') || 'text';
    Array.prototype.forEach.call(table.tBodies, function(body) {
      var top = [], free = [], bottom = [];
      Array.prototype.forEach.call(body.rows, function(r, i) { if (r._wmAt === undefined) r._wmAt = i; var pin = r.getAttribute('data-pin'); (pin === 'top' ? top : pin === 'bottom' ? bottom : free).push(r); });
      free = free.map(function(r) { return { row: r, v: value(r.cells[col], type), name: names < 0 ? null : value(r.cells[names], 'text') }; });
      free.sort(function(a, b) {
        if ((a.v === null) !== (b.v === null)) return a.v === null ? 1 : -1;                       // no value: last, whichever way
        if (a.v !== null && a.v !== b.v) return (a.v < b.v ? -1 : 1) * (desc ? -1 : 1);
        if (a.name !== b.name) return a.name === null ? 1 : b.name === null ? -1 : a.name < b.name ? -1 : 1;   // equal values: by name, from A
        return a.row._wmAt - b.row._wmAt;
      });
      top.concat(free.map(function(x) { return x.row; }), bottom).forEach(function(r) { body.appendChild(r); });
    });
    ths.forEach(function(th, i) {
      var a = th.querySelector('a.sort'), mark = a && a.querySelector('.sort-mark'); if (mark) a.removeChild(mark);
      if (i !== col) { th.removeAttribute('aria-sort'); return; }
      th.setAttribute('aria-sort', desc ? 'descending' : 'ascending');
      if (a) { mark = document.createElement('span'); mark.className = 'sort-mark'; mark.textContent = '\\u00a0' + (desc ? '\\u25bc' : '\\u25b2'); a.appendChild(mark); }
    });
    return true;
  };
  var click = function(a) {
    var th = a.closest('th'), table = a.closest('table'), key = th.getAttribute('data-sort-key'), now = state(table), ask = th.getAttribute('data-sort-ask');
    if (ask && window.Shiny && Shiny.setInputValue) { Shiny.setInputValue(ask, { value: th.getAttribute('data-sort-ask-value') || key, at: Date.now() }, { priority: 'event' }); return; }
    var first = th.getAttribute('data-sort-first') || (th.getAttribute('data-sort-type') === 'num' ? 'desc' : 'asc');
    var desc = now && now.key === key ? !now.desc : first === 'desc';
    if (!order(table, key, desc)) return;
    var id = table.getAttribute('data-sort-id'); if (id) kept[id] = { key: key, desc: desc };
  };
  var title = function(e) { return e.target && e.target.closest ? e.target.closest('table.sortable thead a.sort') : null; };
  document.addEventListener('click', function(e) { var a = title(e); if (!a) return; e.preventDefault(); click(a); });
  document.addEventListener('keydown', function(e) { if (e.key !== 'Enter' && e.key !== ' ') return; var a = title(e); if (!a) return; e.preventDefault(); click(a); });
  // A table drawn again by the server: back in the order the reader had chosen for it
  var again = function() {
    Array.prototype.forEach.call(document.querySelectorAll('table.sortable[data-sort-id]'), function(table) {
      if (table._wmSeen) return; table._wmSeen = true;
      var k = kept[table.getAttribute('data-sort-id')], now = state(table);
      if (k && !(now && now.key === k.key && now.desc === k.desc)) order(table, k.key, k.desc);
    });
  };
  if (window.MutationObserver) new MutationObserver(again).observe(document.documentElement, { childList: true, subtree: true });
  window.wmSort = { order: order, state: state, kept: kept };
})();
document.addEventListener('click', function(e) {
  var b = e.target && e.target.closest ? e.target.closest('button[data-wm-copy]') : null; if (!b) return;
  var el = document.getElementById(b.getAttribute('data-wm-copy')); if (!el) return;
  var label = b.textContent, done = function() { b.textContent = b.getAttribute('data-wm-done') || label; setTimeout(function() { b.textContent = label; }, 1600); };
  var text = el.textContent;
  if (navigator.clipboard && navigator.clipboard.writeText) navigator.clipboard.writeText(text).then(done, function() {});
  else { var r = document.createRange(); r.selectNodeContents(el); var sel = window.getSelection(); sel.removeAllRanges(); sel.addRange(r); try { document.execCommand('copy'); done(); } catch (err) {} }
});"

# The two elements of the loading notice, one per page (texts in the page's language)
busy_ui <- function(lang) tagList(
  tags$div(id = "busy-bar"),
  tags$div(id = "busy-overlay", `data-loading` = TR("loading_view", lang), tags$div(class = "spin"), tags$div(class = "txt"), tags$div(class = "sub", TR("loading_sub", lang))))
page_header <- function(lang) tagList(tags$head(tags$script(HTML(THEME_HEAD_JS)), tags$style(HTML(CSS)), tags$script(HTML(sprintf("window.wmLabels = {menu: %s};", jsonlite::toJSON(TR("nav_menu", lang), auto_unbox = TRUE)))), tags$script(HTML(JS))), busy_ui(lang))

# --- Navbar: the places (a menu of countries, a menu of cities), About, the language ------------
# Text for ordering names whatever their accents (Österreich with the O, España with the E, Łotwa with the L, Ελλάδα
# without its accent); the Cyrillic and East Asian scripts keep their own order
FOLD_LETTERS <- local({
  groups <- c(a = "áàâäãåāăą", e = "éèêëēėęě", i = "íìîïīįı", o = "óòôöõøōő", u = "úùûüūůűų", y = "ýÿ", n = "ñńňņ", c = "çćč", d = "ďđð",
              g = "ğģ", k = "ķ", l = "ĺļľł", r = "ŕř", s = "śšşș", t = "ťţțþ", z = "źżž",
              "α" = "ά", "ε" = "έ", "η" = "ή", "ι" = "ίϊΐ", "ο" = "ό", "υ" = "ύϋΰ", "ω" = "ώ")
  list(from = paste(groups, collapse = ""), to = paste(rep(names(groups), nchar(groups)), collapse = ""))
})
fold_name <- function(x) chartr(FOLD_LETTERS$from, FOLD_LETTERS$to, gsub("ß", "ss", gsub("æ", "ae", tolower(x), fixed = TRUE), fixed = TRUE))

# A menu of places of the top bar: a dropdown of links (a link opens in a new tab like any other) that keeps inside the
# window and scrolls, the place of the page marked, and - when the list is long - a box to type a name in (JS above:
# accents and case apart, Enter opens the best match, the arrows walk the list). `rows`: data.frame(href, label, sub,
# key, current), in the order to show; `key` holds other words that find the place (its code, its English and local names).
# The marked place has its own class and NOT Bootstrap's `active`: Shiny reads the page's tab from the active links of
# the navbar, and a second one would leave input$nav empty.
MENU_SEARCH_MIN <- 8
place_menu <- function(title, rows, search_label, none_label) {
  search <- nrow(rows) > getOption("webarchy.menu_search_min", MENU_SEARCH_MIN)
  bslib::nav_item(class = "dropdown wm-place",
    tags$a(class = "nav-link dropdown-toggle", href = "#", role = "button", `data-bs-toggle` = "dropdown", `aria-expanded` = "false", title),
    tags$ul(class = if (search) "dropdown-menu wm-menu" else "dropdown-menu wm-menu wm-plain",
      if (search) tags$li(class = "wm-menu-search", tags$input(type = "search", class = "form-control form-control-sm", placeholder = search_label, `aria-label` = search_label, autocomplete = "off", spellcheck = "false")),
      lapply(seq_len(nrow(rows)), function(i) tags$li(tags$a(
        class = if (rows$current[i]) "dropdown-item wm-current" else "dropdown-item", href = rows$href[i], `aria-current` = if (rows$current[i]) "page" else NULL,
        `data-name` = rows$label[i], `data-k` = rows$key[i], span(rows$label[i]), if (nzchar(rows$sub[i])) span(class = "sub", rows$sub[i])))),
      if (search) tags$li(class = "wm-menu-empty", hidden = NA, none_label)))
}

nav_switcher <- function(view, lang, cities = CITY_LIGHT, countries = COUNTRY_LIGHT) {
  t <- function(k, ...) TR(k, lang, ...)
  url <- function(v, l = lang) paste0("?view=", v, "&lang=", l)
  link <- function(href, text, active) tags$a(href = href, text, class = "nav-link", style = if (active) "font-weight:700" else NULL)
  select <- tags$select(class = "form-select form-select-sm", `aria-label` = t("language"),
                        onchange = sprintf("if (window.wmLoading) window.wmLoading(this.options[this.selectedIndex].text); window.location.href='?view=%s&lang=' + this.value", view),
                        lapply(names(LANGUAGES), function(l) tags$option(value = l, selected = if (l == lang) NA else NULL, LANGUAGES[[l]])))
  langs <- names(LANGUAGES)
  named <- function(k, l) { tx <- TR(k, l); if (identical(tx, k)) NULL else tx }
  # A country by its name in the page's language (a country without figures of its own, the country of a city, by its translated name)
  cname <- function(cc, l = lang) if (cc %in% names(countries)) countries[[cc]]$CNAME(l) else named(paste0("country_", cc), l) %||% cc
  # A city by its usual name in the page's language (cityname_<key> in translations.csv: Londres, La Haya, 伦敦...), else by
  # the name of its territory without a note in brackets
  city_name <- function(c, l = lang) named(paste0("cityname_", c), l) %||% sub("\\s*\\(.*\\)\\s*$", "", cities[[c]]$CITY_NAME)
  local_name <- function(cc) if (cc %in% names(countries)) countries[[cc]]$META$name_local %||% "" else ""
  # Other words people look a place up by (placewords_<code> in translations.csv: "Reino Unido" for England and Wales,
  # Scotland and Northern Ireland, "Holanda", "Czech Republic"...), in every language
  other_words <- function(code) unlist(lapply(langs, function(l) named(paste0("placewords_", code), l)))
  words <- function(...) { w <- unique(unlist(list(...))); paste(w[!is.na(w) & nzchar(w)], collapse = " ") }
  sorted <- function(d) d[order(fold_name(d$label), method = "radix"), , drop = FALSE]
  # Countries by name; a country is also found by its code and by its name in any of the languages (Germany, Alemania, Deutschland)
  country_rows <- function() sorted(do.call(rbind, lapply(names(countries), function(cc) data.frame(
    href = url(cc), label = cname(cc), sub = "", current = identical(view, cc), stringsAsFactors = FALSE,
    key = words(cc, vapply(langs, function(l) cname(cc, l), ""), local_name(cc), other_words(cc))))))
  # Cities by name, each with its country beside it: a city is found by its names in the other languages too, and typing
  # a country lists its cities
  city_rows <- function() sorted(do.call(rbind, lapply(names(cities), function(c) {
    cc <- sub("-.*$", "", cities[[c]]$CITY_ID); several <- length(countries) > 1
    data.frame(href = url(c), label = city_name(c), sub = if (several) cname(cc) else "", current = identical(view, c), stringsAsFactors = FALSE,
               key = words(c, cities[[c]]$code %||% "", sub("\\s*\\(.*\\)\\s*$", "", cities[[c]]$CITY_NAME), cities[[c]]$META$name_en %||% "", vapply(langs, function(l) city_name(c, l), ""), other_words(c),
                           if (several) c(cc, vapply(langs, function(l) cname(cc, l), ""), local_name(cc), other_words(cc))))
  })))
  list(
    bslib::nav_spacer(),
    if (length(countries) == 1) bslib::nav_item(link(url(names(countries)), cname(names(countries)), view == names(countries)))
    else if (length(countries) > 1) place_menu(if (view %in% names(countries)) cname(view) else t("nav_country"), country_rows(), t("nav_search_country", length(countries)), t("nav_no_match")),
    if (length(cities) > 0) place_menu(t("nav_cities"), city_rows(), t("nav_search_city", length(cities)), t("nav_no_match")),
    bslib::nav_item(link(url("about"), t("nav_about"), view == "about")),
    bslib::nav_item(select)
  )
}
