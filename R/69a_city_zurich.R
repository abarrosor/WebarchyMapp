# Zürich (ZRH): offences of the Swiss Criminal Code by Stadtkreis, yearly ---------------------------------------
# City module of the shared city framework (R/60_cities.R). Source: the police crime statistics (PKS) of the
# Kantonspolizei Zürich as open data of the Kanton Zürich (dataset 1202, file KTZH_00001202_00003600.csv):
# offences by municipality (> 10,000 residents) and, for the city of Zürich, by its 12 Stadtkreise, one row per
# year (since 2009), law and StGB title ("Haupttitel": offences against life and limb, against property, against
# liberty, against sexual integrity...), with the completed and attempted offences, the residents (Einwohner)
# and the frequency per 1,000 residents (Häufigkeitszahl) of each unit. Hierarchy: Zürich (a node under the
# canton CH-ZH of the Switzerland module) > one district (the city: the source has no level between the city
# and the Kreise) > the 12 Stadtkreise as neighbourhoods. A yearly source (time_unit = "year"). The event
# types are the StGB titles; offences against sexual integrity and against the family count in the totals
# only (sensitive); the total rows of the file are loaded as aggregates and never shown by area. Residents:
# the Einwohner column of the file, one register per year. Boundaries: Stadtkreise from the open data WFS
# of the City of Zürich (reference/zurich_sources.csv).
#
# Written against the published file layout (its columns were checked on 2026-11-10: Ausgangsjahr,
# Gemeinde_BFS_Nr, Gemeindename, Stadtkreis_BFS_Nr, Stadtkreis_Name, Gesetz_Nummer, Gesetz_Abk,
# Reihenfolge_Haupttitel, Haupttitel, Straftaten_total, Straftaten_vollendet, Straftaten_versucht, Einwohner,
# Häufigkeitszahl) but not run against it: `zurich_inspect()` prints what was downloaded.

ID_ZURICH <- "CH-ZH-261"
ZRH_DIR <- file.path(CONFIG$dir_raw, "city_zurich")
zrh_sources <- function() read_reference("zurich_sources.csv")
zrh_source <- function(key) { s <- zrh_sources(); u <- s$url[s$key == key]; if (length(u) && !is.na(u) && nzchar(trimws(u))) trimws(u) else NA_character_ }
zrh_dir <- function(sub) { d <- file.path(ZRH_DIR, sub); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
zrh_newest <- function(sub, pattern) { f <- newest_first(list.files(file.path(ZRH_DIR, sub), pattern, full.names = TRUE)); if (length(f)) f[1] else NA_character_ }
zrh_district_id <- function() paste0(ID_ZURICH, "-D00")

# --- Download: the PKS file (yearly; refreshed after 60 days) and the Stadtkreise layer -----------------------------
zrh_download <- function(days = 60) {
  prev <- zrh_newest("pks", "\\.csv$")
  if (!is.na(prev) && file_age_days(prev) < days) message("= Zürich PKS file downloaded less than ", days, " days ago") else
    download_versioned(zrh_source("pks_csv"), zrh_dir("pks"), name = "pks_gemeinden_stadtkreise.csv")
  prev <- zrh_newest("boundaries", "\\.(geojson|json)$")
  if (is.na(prev) || file_age_days(prev) > 365) download_versioned(zrh_source("kreise_wfs"), zrh_dir("boundaries"), name = "stadtkreise.geojson")
  invisible(TRUE)
}

# --- Read: the rows of the city of Zürich by Stadtkreis ----------------------------------------------------------
zrh_read <- function(f) {
  read <- function(enc) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE, locale = readr::locale(encoding = enc))
  d <- read("UTF-8"); if (!any(grepl("^Haupttitel$|^Stadtkreis", names(d)))) d <- read("latin1")
  if (ncol(d) < 3) { d <- readr::read_delim(f, delim = ";", col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8")) }
  nm <- names(d); pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col <- list(year = pick("^Ausgangsjahr$|^Jahr$"), gemeinde = pick("^Gemeindename$"), kreis = pick("^Stadtkreis_Name$"), kreis_nr = pick("^Stadtkreis_BFS_Nr$"),
              law = pick("^Gesetz_Abk$"), title = pick("^Haupttitel$"), n = pick("^Straftaten_total$"), pop = pick("^Einwohner$"), order = pick("^Reihenfolge_Haupttitel$"))
  if (is.na(col$year) || is.na(col$title) || is.na(col$n) || (is.na(col$kreis) && is.na(col$kreis_nr))) stop("unexpected columns in ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun zurich_inspect()")
  kreis <- if (!is.na(col$kreis)) trimws(d[[col$kreis]]) else rep(NA_character_, nrow(d))
  is_kreis <- !is.na(kreis) & nzchar(kreis) & grepl("kreis", kreis, ignore.case = TRUE)
  if (!is.na(col$gemeinde)) is_kreis <- is_kreis & grepl("^Z(ü|u|ue)rich", trimws(d[[col$gemeinde]]), ignore.case = TRUE)
  d <- d[is_kreis, ]; kreis <- kreis[is_kreis]
  if (nrow(d) == 0) stop("no Stadtkreis rows of Zürich in ", basename(f), " (Stadtkreis_Name 'Kreis 1'...): run zurich_inspect()")
  num <- suppressWarnings(as.integer(sub("^.*?([0-9]{1,2})\\s*$", "\\1", kreis)))
  law <- if (!is.na(col$law)) trimws(d[[col$law]]) else "StGB"; title <- trimws(d[[col$title]])
  data.frame(year = suppressWarnings(as.integer(d[[col$year]])), kreis = num, kreis_name = kreis,
             event_type = ifelse(is.na(law) | law == "" | law == "StGB", title, paste0(law, ": ", title)),
             n = suppressWarnings(as.numeric(gsub("[^0-9.]", "", d[[col$n]]))), pop = if (!is.na(col$pop)) suppressWarnings(as.numeric(gsub("[^0-9.]", "", d[[col$pop]]))) else NA_real_,
             file = basename(f), stringsAsFactors = FALSE)
}

# --- Territory: the 12 Stadtkreise (WFS: bezeichnung "Kreis 6", name 6) under one district ------------------------
zrh_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_ZURICH) > 1 && file.exists(CITY_MODULES$zurich$gpkg)) { message("= Zürich territory kept (", city_neighbourhood_count(con, ID_ZURICH), " Stadtkreise)"); return(invisible(FALSE)) }
  f <- zrh_newest("boundaries", "\\.(geojson|json)$"); if (is.na(f)) stop("no Stadtkreise layer in ", file.path(ZRH_DIR, "boundaries"), " (option 14 with ZRH)")
  g <- sf::st_read(f, quiet = TRUE); nm <- names(g)
  pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col_lab <- pick("^bezeichnung$|^kreis|^stadtkreis"); col_num <- pick("^name$|^knr$|^nummer$|^kreisnummer$")
  num <- if (!is.na(col_num)) suppressWarnings(as.integer(round(as.numeric(g[[col_num]])))) else NA_integer_
  if (all(is.na(num)) && !is.na(col_lab)) num <- suppressWarnings(as.integer(sub("^.*?([0-9]{1,2})\\s*$", "\\1", as.character(g[[col_lab]]))))
  if (all(is.na(num))) stop("Stadtkreise layer: no Kreis number; fields are ", paste(nm, collapse = ", "))
  g <- g[!is.na(num) & num >= 1 & num <= 12, ]; num <- num[!is.na(num) & num >= 1 & num <= 12]
  if (nrow(g) < 2) stop("Stadtkreise layer: ", nrow(g), " Kreis polygons found (12 expected); fields are ", paste(nm, collapse = ", "))
  g <- clean_polygons(g)                                                       # the WFS rings cross themselves: repaired the planar way (R/04_territory.R)
  district_id <- zrh_district_id(); code <- sprintf("%02d", num)
  neighbourhoods <- sf::st_sf(territory_id = paste0(district_id, "-B", code), level = "neighbourhood", parent_id = district_id, country = "CH", official_code = code, code_scheme = "Stadt Zürich Stadtkreis",
                              name = paste("Kreis", num), area_m2 = g$area_m2, file = basename(f), geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, rep(district_id, nrow(g)))
  districts <- sf::st_sf(territory_id = district_id, level = "district", parent_id = ID_ZURICH, country = "CH", official_code = "261", code_scheme = "BFS",
                         name = "Zürich", area_m2 = tryCatch(as.numeric(sf::st_area(dis)), error = function(e) sum(g$area_m2)), file = basename(f), geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = ID_ZURICH, level = "municipality", parent_id = "CH-ZH", country = "CH", official_code = "261", code_scheme = "BFS", name = "Zürich", stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES$zurich$gpkg)
  invisible(neighbourhoods)
}

# --- Ingest: year x Kreis x StGB title --------------------------------------------------------------------------
zrh_ingest <- function(con) {
  f <- zrh_newest("pks", "\\.csv$"); if (is.na(f)) stop("no PKS file in ", file.path(ZRH_DIR, "pks"), " (option 14 with ZRH)")
  d <- zrh_read(f); d <- d[!is.na(d$year) & !is.na(d$kreis) & !is.na(d$n), ]
  types <- read_reference("zurich_event_types.csv", bool_cols = "sensitive")
  d$type_code <- types$source_code[match(normalize_text(d$event_type), normalize_text(types$source_code))]
  d$type_code[is.na(d$type_code)] <- d$event_type[is.na(d$type_code)]
  district_id <- zrh_district_id()
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND parent_id = ", DBI::dbQuoteString(con, district_id)))$territory_id
  d$neighbourhood_id <- paste0(district_id, "-B", sprintf("%02d", d$kreis)); d$neighbourhood_id[!d$neighbourhood_id %in% known] <- NA
  lost <- unique(d$kreis_name[is.na(d$neighbourhood_id)]); if (length(lost)) message("  Kreise of the file not in the layer (counted at city level): ", paste(lost, collapse = ", "))
  ev <- d |> dplyr::group_by(year, neighbourhood_id, event_type = type_code) |> dplyr::summarise(events = sum(n), .groups = "drop") |> as.data.frame()
  ev$month <- as.Date(sprintf("%d-01-01", ev$year)); ev$district_id <- district_id; ev$city <- "zurich"; ev$file <- basename(f); ev$events <- as.integer(round(ev$events))
  replace_city_rows(con, "stg_city_events", "zurich", ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  seen <- unique(ev$event_type); knownt <- types[types$source_code %in% seen, ]; extra <- setdiff(seen, knownt$source_code)
  t <- rbind(data.frame(city = "zurich", event_type = knownt$source_code, block = knownt$block, name_local = knownt$name_local, name_en = knownt$name_en, sensitive = knownt$sensitive, stringsAsFactors = FALSE),
             if (length(extra)) data.frame(city = "zurich", event_type = extra, block = ifelse(grepl("^Total StGB|^Total Straftaten|^Total$|insgesamt", extra, ignore.case = TRUE), "aggregate", "public_safety"),
                                           name_local = sub("^Total ", "", extra), name_en = sub("^Total ", "", extra), sensitive = grepl("sexuell|Familie|^AIG", extra, ignore.case = TRUE), stringsAsFactors = FALSE))
  if (length(extra)) message("  titles not in zurich_event_types.csv (kept under their own name): ", paste(extra, collapse = " / "))
  replace_city_rows(con, "ref_city_event_types", "zurich", t)
  ZRH_STATE$pop <- unique(d[!is.na(d$pop) & !is.na(d$neighbourhood_id), c("year", "neighbourhood_id", "pop", "file")])
  log_ingest(con, "city_zurich", basename(f), NA, paste(range(ev$year), collapse = "-"), nrow(ev), sum(ev$events))
  message(sprintf("  zurich: %s rows, %d-%d, %d Stadtkreise, %d titles (yearly; %d shown by area)", fmt_int(nrow(ev)), min(ev$year), max(ev$year), length(unique(stats::na.omit(ev$neighbourhood_id))), nrow(t), sum(t$block %in% MARTS$blocks & !t$sensitive)))
  invisible(ev)
}
ZRH_STATE <- new.env()

# --- Population: the Einwohner column, one register per year (the app uses the latest) ----------------------------
zrh_population <- function(con) {
  pop <- ZRH_STATE$pop
  if (is.null(pop) || nrow(pop) == 0) { f <- zrh_newest("pks", "\\.csv$"); if (is.na(f)) stop("no PKS file: option 14 with ZRH"); d <- zrh_read(f); district_id <- zrh_district_id()
    d$neighbourhood_id <- paste0(district_id, "-B", sprintf("%02d", d$kreis)); pop <- unique(d[!is.na(d$pop) & !is.na(d$year), c("year", "neighbourhood_id", "pop", "file")]) }
  if (nrow(pop) == 0) stop("the PKS file has no Einwohner column: counts only, no rates")
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND parent_id = ", DBI::dbQuoteString(con, zrh_district_id())))$territory_id
  pop <- pop[pop$neighbourhood_id %in% known, ]; pop <- pop[!duplicated(pop[, c("year", "neighbourhood_id")]), ]
  if (nrow(pop) == 0) stop("no Einwohner row matches a Stadtkreis")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(zrh_district_id(), "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = pop$neighbourhood_id, ref_date = as.Date(sprintf("%d-01-01", pop$year)), population = as.integer(round(pop$pop)), file = pop$file))
  })
  y <- max(pop$year)
  message(sprintf("  population: %d Stadtkreise, %d-%d (%s residents in %d)", length(unique(pop$neighbourhood_id)), min(pop$year), y, fmt_int(sum(pop$pop[pop$year == y])), y))
  invisible(TRUE)
}

zurich_inspect <- function() {
  for (sub in c("pks", "boundaries")) { cat("\n", sub, ":\n", sep = ""); print(list.files(file.path(ZRH_DIR, sub))) }
  f <- zrh_newest("pks", "\\.csv$")
  if (!is.na(f)) { x <- readr::read_csv(f, n_max = 5000, col_types = readr::cols(.default = "c"), show_col_types = FALSE); cat("\ncolumns:", paste(names(x), collapse = " | "), "\n"); for (k in intersect(c("Gemeindename", "Stadtkreis_Name", "Gesetz_Abk", "Haupttitel", "Ausgangsjahr"), names(x))) { cat("\n", k, ":\n", sep = ""); print(utils::head(sort(unique(x[[k]])), 40)) } }
  b <- zrh_newest("boundaries", "\\.(geojson|json)$")
  if (!is.na(b)) { g <- tryCatch(sf::st_read(b, quiet = TRUE), error = function(e) NULL); if (!is.null(g)) cat("\nboundaries:", nrow(g), "features; fields:", paste(names(g), collapse = ", "), "\n") else cat("\nboundaries file not readable; first line:", readLines(b, n = 1, warn = FALSE), "\n") }
  invisible(TRUE)
}

register_city(list(
  code = "ZRH", key = "zurich", id = ID_ZURICH, name_en = "Zürich", name_local = "Zürich", lang = "de", country = "CH", after_countries = TRUE,
  time_unit = "year", measure_kind = "offences", stats_level = "neighbourhood",
  boundaries_attribution = "Stadt Zürich, Open Data (Stadtkreise)",
  method = list(where = "Stadtkreis where the offence was committed, as recorded by the Kantonspolizei Zürich (PKS)", date = "Year the offence was recorded in the police crime statistics", revisions = "The whole file is republished every spring with the new year; earlier years can change"),
  source = list(name = "Offences of the Criminal Code by Stadtkreis (PKS, Kanton Zürich open data)", name_local = "Straftaten nach Stadtkreis (Polizeiliche Kriminalstatistik, Open Data Kanton Zürich)",
                publisher = "Kantonspolizei Zürich, Statistisches Amt des Kantons Zürich (Open Data)", url = "https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/1202@kantonspolizei-kanton-zuerich",
                licence = "Open use, attribution required (Kanton Zürich open data terms)",
                measure = "Offences of the Swiss Criminal Code (StGB) recorded by the police, completed and attempted, by title of the code; yearly; the residents of each Kreis come with the file. Offences against sexual integrity and against the family count in the totals only."),
  about_source_key = "about_src_zurich", about_measures_key = "about_zurich_measures", about_territory_key = "about_zurich_territory",
  download = zrh_download, territory = zrh_territory, ingest = zrh_ingest, population = zrh_population
))
