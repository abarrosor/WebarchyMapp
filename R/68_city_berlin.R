# Berlin (BER): the Kriminalitätsatlas by Bezirksregion, yearly ---------------------------------------------
# City module of the shared city framework (R/60_cities.R). Source: "Kriminalitätsatlas Berlin" (Polizei Berlin,
# Berlin Open Data, CC BY-SA): one Excel workbook with the recorded cases (Fallzahlen) and the frequency figure
# (Häufigkeitszahl, HZ: cases per 100,000 residents) of 17 offence categories by Bezirk (12) and Bezirksregion
# (143 LOR units of the 2021 layout; 138 before it), one sheet per year since 2016. Hierarchy: Berlin (a node under the Land DE-11 of the Germany
# module) > the 12 Bezirke as districts > the Bezirksregionen as neighbourhoods. A yearly source (time_unit =
# "year"). The categories shown are the seven disjoint groups of the atlas (robbery, assault, coercion and
# threats, theft, fire offences, criminal damage, drug offences); its sub-categories (street robbery, bicycle
# theft...) and its own aggregates are loaded but not shown by area, so that nothing is counted twice
# (reference/berlin_event_types.csv). Residents: implied by the atlas itself (cases × 100,000 / HZ of the
# all-offences row), so the rates of the app reproduce the atlas's own figures.
# Boundaries: LOR Bezirksregionen 2021 from the Geoportal Berlin (CC BY 3.0 DE, reference/berlin_sources.csv).
#
# Written against the published description, not run against the live workbook: `berlin_inspect()` prints the
# sheets, the header row found and the category columns of what was downloaded.

ID_BERLIN <- "DE-11-BERLIN"
BERLIN_DIR <- file.path(CONFIG$dir_raw, "city_berlin")
BERLIN_BEZIRKE <- c(`01` = "Mitte", `02` = "Friedrichshain-Kreuzberg", `03` = "Pankow", `04` = "Charlottenburg-Wilmersdorf", `05` = "Spandau", `06` = "Steglitz-Zehlendorf",
                    `07` = "Tempelhof-Schöneberg", `08` = "Neukölln", `09` = "Treptow-Köpenick", `10` = "Marzahn-Hellersdorf", `11` = "Lichtenberg", `12` = "Reinickendorf")
berlin_sources <- function() read_reference("berlin_sources.csv")
berlin_source <- function(key) { s <- berlin_sources(); u <- s$url[s$key == key]; if (length(u) && !is.na(u) && nzchar(trimws(u))) trimws(u) else NA_character_ }
berlin_dir <- function(sub) { d <- file.path(BERLIN_DIR, sub); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
berlin_newest <- function(sub, pattern) { f <- newest_first(list.files(file.path(BERLIN_DIR, sub), pattern, full.names = TRUE)); if (length(f)) f[1] else NA_character_ }
berlin_district_id <- function(bez) paste0(ID_BERLIN, "-D", bez)
# A category name as a comparable key: the workbook hyphenates and breaks its headers ("Körper-\nverletzungen
# -insgesamt-", "Rauschgift-delikte"): letters and digits only, lower case
berlin_key <- function(x) gsub("[^[:alnum:]]", "", tolower(enc2utf8(as.character(x))))

# --- Download: the workbook (direct link, else the first .xlsx link of the dataset page) and the LOR layer ------
berlin_download <- function(days = 60) {
  prev <- berlin_newest("atlas", "\\.xlsx$")
  if (!is.na(prev) && file_age_days(prev) < days) message("= Berlin atlas downloaded less than ", days, " days ago") else {
    ok <- tryCatch({ download_versioned(berlin_source("atlas_xlsx"), berlin_dir("atlas"), name = "kriminalitaetsatlas_berlin.xlsx"); TRUE }, error = function(e) { message("  direct link failed (", conditionMessage(e), "): looking at the dataset page"); FALSE })
    if (!ok) {
      page <- get_text(berlin_source("atlas_page")); if (is.null(page)) stop("neither the direct link nor the dataset page could be read")
      links <- unique(regmatches(page, gregexpr("https?://[^\"' <>]+\\.xlsx", page))[[1]])
      if (!length(links)) stop("no .xlsx link on the dataset page: update atlas_xlsx in reference/berlin_sources.csv")
      download_versioned(links[1], berlin_dir("atlas"), name = "kriminalitaetsatlas_berlin.xlsx")
    }
  }
  prev <- berlin_newest("boundaries", "\\.(geojson|json)$")
  if (is.na(prev) || file_age_days(prev) > 365) download_versioned(berlin_source("lor_wfs"), berlin_dir("boundaries"), name = "lor_bezirksregionen.geojson")
  invisible(TRUE)
}

# --- The workbook: one data frame of region key, name, year, category, cases and HZ ---------------------------
berlin_read_sheet <- function(f, sheet) {
  x <- suppressMessages(readxl::read_excel(f, sheet = sheet, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE); if (nrow(x) < 3) return(NULL)
  is_key <- function(v) grepl("^[0-9]{2}([0-9]{2}){0,3}$", trimws(v %||% ""))
  share <- vapply(x, function(v) mean(is_key(v)), numeric(1)); if (max(share, na.rm = TRUE) < 0.3) return(NULL)
  key_col <- which.max(share); rows <- which(is_key(x[[key_col]])); first <- min(rows)
  header <- NULL                                                    # the nearest row above the data with at least three filled cells
  for (up in 1:4) { if (first - up < 1) break; h <- trimws(unlist(x[first - up, ])); if (sum(!is.na(h) & nzchar(h)) >= 3) { header <- h; break } }
  if (is.null(header)) return(NULL)
  name_col <- which(vapply(seq_along(x), function(k) k != key_col && mean(grepl("[A-Za-zÄÖÜäöüß]{3,}", x[[k]][rows])) > 0.8 && !grepl("insgesamt|raub|diebstahl|delikte|verletz|schädigung|kiez", header[k], ignore.case = TRUE), logical(1)))[1]
  body <- x[rows, , drop = FALSE]
  cat_cols <- setdiff(which(nzchar(header) & !is.na(header)), c(key_col, name_col))
  cat_cols <- cat_cols[vapply(cat_cols, function(k) mean(!is.na(suppressWarnings(as.numeric(gsub("[^0-9.,-]", "", body[[k]]))))) > 0.5, logical(1))]
  if (!length(cat_cols)) return(NULL)
  num <- function(v) suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", v))))
  keyv <- trimws(body[[key_col]]); namev <- if (is.na(name_col)) rep(NA_character_, nrow(body)) else trimws(body[[name_col]])
  do.call(rbind, lapply(cat_cols, function(k) data.frame(key = keyv, name = namev, category = header[k], value = num(body[[k]]), stringsAsFactors = FALSE, row.names = NULL)))
}

berlin_read <- function(f) {
  sheets <- readxl::excel_sheets(f); out <- list()
  for (sh in sheets) {
    yr <- regmatches(sh, regexpr("20[0-9]{2}", sh)); if (!length(yr)) next
    kind <- if (grepl("hz|häufigkeit|haeufigkeit", sh, ignore.case = TRUE)) "hz" else if (grepl("fall|fälle|faelle|zahl", sh, ignore.case = TRUE)) "cases" else next
    d <- berlin_read_sheet(f, sh); if (is.null(d)) next
    d$year <- as.integer(yr); d$kind <- kind; out[[sh]] <- d
  }
  if (!length(out)) stop("no sheet of ", basename(f), " has a year in its name and a column of LOR keys: run berlin_inspect()")
  do.call(rbind, out)
}

# --- Territory: Bezirksregionen (LOR, 6-digit keys) and the 12 Bezirke --------------------------------------------
berlin_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_BERLIN) > 1 && file.exists(CITY_MODULES$berlin$gpkg)) { message("= Berlin territory kept (", city_neighbourhood_count(con, ID_BERLIN), " Bezirksregionen)"); return(invisible(FALSE)) }
  f <- berlin_newest("boundaries", "\\.(geojson|json)$"); if (is.na(f)) stop("no LOR boundaries in ", file.path(BERLIN_DIR, "boundaries"), " (option 14 with BER)")
  g <- sf::st_read(f, quiet = TRUE); nm <- names(g)
  code_col <- nm[vapply(nm, function(k) is.character(g[[k]]) && mean(grepl("^[0-9]{6}$", g[[k]])) > 0.8, logical(1))][1]
  if (is.na(code_col)) { num6 <- nm[vapply(nm, function(k) is.numeric(g[[k]]) && all(g[[k]] >= 10000 & g[[k]] < 1000000, na.rm = TRUE), logical(1))][1]; if (!is.na(num6)) { g[[num6]] <- sprintf("%06d", as.integer(g[[num6]])); code_col <- num6 } }
  if (is.na(code_col)) stop("LOR file: no column of 6-digit Bezirksregion keys; fields are ", paste(nm, collapse = ", "))
  name_col <- nm[grepl("bzr_name|bezirksregion|^name$|bezeichn", nm, ignore.case = TRUE) & nm != code_col][1]
  g <- sf::st_make_valid(sf::st_transform(g, 4326)); code <- g[[code_col]]; bez <- substr(code, 1, 2)
  g <- g[bez %in% names(BERLIN_BEZIRKE), ]; code <- code[bez %in% names(BERLIN_BEZIRKE)]; bez <- substr(code, 1, 2)
  district_id <- berlin_district_id(bez)
  neighbourhoods <- sf::st_sf(territory_id = paste0(district_id, "-B", code), level = "neighbourhood", parent_id = district_id, country = "DE", official_code = code, code_scheme = "LOR Bezirksregion 2021",
                              name = if (!is.na(name_col)) as.character(g[[name_col]]) else code, area_m2 = as.numeric(sf::st_area(g)), file = basename(f), geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, district_id)
  districts <- sf::st_sf(territory_id = dis$territory_id, level = "district", parent_id = ID_BERLIN, country = "DE", official_code = sub("^.*-D", "", dis$territory_id), code_scheme = "Berlin Bezirk",
                         name = unname(BERLIN_BEZIRKE[sub("^.*-D", "", dis$territory_id)]), area_m2 = as.numeric(sf::st_area(dis)), file = basename(f), geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = ID_BERLIN, level = "municipality", parent_id = "DE-11", country = "DE", official_code = "11000", code_scheme = "WebarchyMapp (the whole Land)", name = "Berlin", stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES$berlin$gpkg)
}

# --- Ingest: cases by Bezirksregion, year and category; residents implied by the HZ of the all-offences row -------
berlin_ingest <- function(con) {
  f <- berlin_newest("atlas", "\\.xlsx$"); if (is.na(f)) stop("no atlas workbook in ", file.path(BERLIN_DIR, "atlas"), " (option 14 with BER)")
  d <- berlin_read(f)
  types <- read_reference("berlin_event_types.csv", bool_cols = "sensitive")
  d$event_type <- types$source_code[match(berlin_key(d$category), berlin_key(types$source_code))]
  unknown <- unique(d$category[is.na(d$event_type)]); if (length(unknown)) message("  categories not in berlin_event_types.csv (left out): ", paste(utils::head(unknown, 10), collapse = " / "))
  d <- d[!is.na(d$event_type) & !is.na(d$value), ]
  cases <- d[d$kind == "cases", ]; hz <- d[d$kind == "hz", ]
  if (nrow(cases) == 0) stop("no case-number sheet recognised in ", basename(f), ": run berlin_inspect()")
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id, parent_id, official_code FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_BERLIN, "-%"))))
  cases <- cases[substr(cases$key, 1, 2) %in% names(BERLIN_BEZIRKE), ]          # the city total row (999999...) is not a place
  # Keys: "010101" a Bezirksregion; "010000" the Bezirk total (also written as "01"); "019900" the cases of the
  # Bezirk that the atlas could not assign to a region. The Bezirk rows are not added to their regions: only the
  # remainder (total minus the regions, which holds the 9900 cases) goes in as a district-level row
  is_total <- nchar(cases$key) == 2 | grepl("^[0-9]{2}0000$", cases$key); is_rest <- grepl("^[0-9]{2}9900$", cases$key)
  bezirk <- cases[is_total, ]; bezirk$key <- substr(bezirk$key, 1, 2)
  reg <- cases[!is_total & !is_rest & nchar(cases$key) == 6, ]
  reg$neighbourhood_id <- known$territory_id[match(reg$key, known$official_code)]
  lost <- unique(reg$key[is.na(reg$neighbourhood_id)]); if (length(lost)) message("  ", length(lost), " Bezirksregion keys not in the boundaries (counted in their Bezirk): ", paste(utils::head(lost, 5), collapse = ", "))
  reg$district_id <- berlin_district_id(substr(reg$key, 1, 2))
  # Bezirk rows minus their regions: what the atlas assigns to the Bezirk without a region (usually nothing)
  ev <- rbind(reg[, c("year", "neighbourhood_id", "district_id", "event_type", "value")],
              if (nrow(bezirk)) { b <- bezirk; b$district_id <- berlin_district_id(b$key); b$neighbourhood_id <- NA_character_
                s <- reg |> dplyr::group_by(district_id, year, event_type) |> dplyr::summarise(v = sum(value), .groups = "drop")
                b$value <- b$value - s$v[match(paste(b$district_id, b$year, b$event_type), paste(s$district_id, s$year, s$event_type))]
                b <- b[!is.na(b$value) & b$value > 0, c("year", "neighbourhood_id", "district_id", "event_type", "value")]; b })
  ev$month <- as.Date(sprintf("%d-01-01", ev$year)); ev$city <- "berlin"; ev$file <- basename(f); ev$events <- as.integer(round(ev$value))
  ev <- ev |> dplyr::group_by(city, month, neighbourhood_id, district_id, event_type, file) |> dplyr::summarise(events = sum(events), .groups = "drop") |> as.data.frame()
  replace_city_rows(con, "stg_city_events", "berlin", ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  seen <- types[types$source_code %in% ev$event_type, ]
  t <- data.frame(city = "berlin", event_type = seen$source_code, block = seen$block, name_local = seen$name_local, name_en = seen$name_en, sensitive = seen$sensitive, stringsAsFactors = FALSE)
  replace_city_rows(con, "ref_city_event_types", "berlin", t)
  # residents implied by the all-offences row: cases * 100,000 / HZ, per region and year (every year is kept: one register per year)
  tot <- types$source_code[types$block == "aggregate"][1]
  is_region <- function(k) nchar(k) == 6 & !grepl("^[0-9]{2}(0000|9900)$", k)
  c0 <- cases[cases$event_type == tot & is_region(cases$key), ]; h0 <- hz[hz$event_type == tot & is_region(hz$key), ]
  pop <- merge(c0[, c("key", "year", "value")], h0[, c("key", "year", "value")], by = c("key", "year"), suffixes = c("_c", "_hz"))
  pop <- pop[!is.na(pop$value_hz) & pop$value_hz > 0, ]; pop$population <- round(pop$value_c * 1e5 / pop$value_hz)
  BERLIN_STATE$pop <- pop
  yrs <- as.integer(format(ev$month, "%Y"))
  log_ingest(con, "city_berlin", basename(f), NA, paste(range(yrs), collapse = "-"), nrow(ev), sum(ev$events))
  message(sprintf("  berlin: %s rows, %d-%d, %d Bezirksregionen, %d categories (yearly; %d shown by area)", fmt_int(nrow(ev)), min(yrs), max(yrs), length(unique(stats::na.omit(ev$neighbourhood_id))), nrow(t), sum(t$block %in% MARTS$blocks)))
  invisible(ev)
}
BERLIN_STATE <- new.env()

berlin_population <- function(con) {
  pop <- BERLIN_STATE$pop; if (is.null(pop)) { berlin_ingest(con); pop <- BERLIN_STATE$pop }
  if (is.null(pop) || nrow(pop) == 0) stop("no residents could be derived from the atlas (no HZ sheet, or no all-offences column): counts only, no rates")
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id, official_code FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_BERLIN, "-%"))))
  pop$territory_id <- known$territory_id[match(pop$key, known$official_code)]; pop <- pop[!is.na(pop$territory_id) & !is.na(pop$population) & pop$population > 0, ]
  pop <- pop[!duplicated(pop[, c("territory_id", "year")]), ]
  if (nrow(pop) == 0) stop("none of the regions of the atlas is a Bezirksregion of the territory: counts only, no rates")
  y <- max(pop$year)
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_BERLIN, "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = pop$territory_id, ref_date = as.Date(sprintf("%d-12-31", pop$year)), population = as.integer(pop$population), file = "Kriminalitätsatlas (cases × 100,000 / HZ)"))
  })
  message(sprintf("  population: %d Bezirksregionen, implied by the atlas's HZ of %d-%d (%s residents in %d)", length(unique(pop$territory_id)), min(pop$year), y, fmt_int(sum(pop$population[pop$year == y])), y))
  invisible(TRUE)
}

berlin_inspect <- function() {
  for (sub in c("atlas", "boundaries")) { cat("\n", sub, ":\n", sep = ""); print(list.files(file.path(BERLIN_DIR, sub))) }
  f <- berlin_newest("atlas", "\\.xlsx$")
  if (!is.na(f)) { cat("\nsheets:", paste(readxl::excel_sheets(f), collapse = " | "), "\n")
    d <- tryCatch(berlin_read(f), error = function(e) { cat(conditionMessage(e), "\n"); NULL })
    if (!is.null(d)) { cat("years:", paste(sort(unique(d$year)), collapse = " "), "; kinds:", paste(unique(d$kind), collapse = " "), "\ncategories:\n"); print(unique(d$category)); cat("keys (first):", paste(utils::head(unique(d$key), 15), collapse = " "), "\n") } }
  b <- berlin_newest("boundaries", "\\.(geojson|json)$")
  if (!is.na(b)) { g <- tryCatch(sf::st_read(b, quiet = TRUE), error = function(e) NULL); if (!is.null(g)) cat("\nboundaries:", nrow(g), "features; fields:", paste(names(g), collapse = ", "), "\n") else cat("\nboundaries file not readable; first line:", readLines(b, n = 1, warn = FALSE), "\n") }
  invisible(TRUE)
}

register_city(list(
  code = "BER", key = "berlin", id = ID_BERLIN, name_en = "Berlin", name_local = "Berlin", lang = "de", country = "DE",
  time_unit = "year", measure_kind = "offences", stats_level = "neighbourhood",
  boundaries_attribution = "Geoportal Berlin, LOR Bezirksregionen (CC BY 3.0 DE)",
  method = list(where = "Bezirksregion where the offence was committed (LOR 2021), as assigned by the Berlin police", date = "Year the case was recorded in the police crime statistics", revisions = "A new workbook every spring; earlier years are republished with it"),
  source = list(name = "Kriminalitätsatlas Berlin (Polizei Berlin)", name_local = "Kriminalitätsatlas Berlin (Polizei Berlin)",
                publisher = "Polizei Berlin, Berlin Open Data", url = "https://daten.berlin.de/datensaetze/kriminalitatsatlas-berlin", licence = "CC BY-SA",
                measure = "Recorded cases of the police crime statistics (PKS) for 17 offence categories by Bezirksregion, yearly; the app shows the seven disjoint groups (robbery, assault, coercion and threats, theft, fire offences, criminal damage, drug offences) and keeps the sub-categories and the atlas's aggregates out of the map. The total shown is the sum of the seven groups, not all offences. Residents implied by the atlas's frequency figures."),
  about_source_key = "about_src_berlin", about_measures_key = "about_berlin_measures", about_territory_key = "about_berlin_territory",
  download = berlin_download, territory = berlin_territory, ingest = berlin_ingest, population = berlin_population
))
