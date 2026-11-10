# London (LON): offences recorded by the Metropolitan Police by ward, monthly --------------------------------
# City module of the shared city framework (R/60_cities.R). Source: "MPS Recorded Crime: Geographic
# Breakdown" on the London Datastore (Greater London Authority / Metropolitan Police Service, Open Government
# Licence): offences recorded by the Met by month, major and minor category, ward and borough, in two CSVs
# (the most recent 24 months, and the historical series). The City of London has its own police force and is
# not in the data. Hierarchy: the Metropolitan Police area as the city node > the 32 boroughs as districts
# (reference/london_boroughs.csv: name -> GSS code) > wards as neighbourhoods (GSS codes E05...).
# Boundaries: ONS Open Geography portal (generalised wards, OGL), queried by the ward codes of the data.
# Population: not published with the data; a ward population file (ONS mid-year estimates or Census 2021)
# placed in data/raw/city_london/population/ gives the rates, otherwise counts only.
# Types: MinorText of the Met classification (block public_safety; sexual offences and homicide sensitive).
#
# Written against the published file layout, not run against the live files: the reader detects the columns
# by name and `london_inspect()` prints the headers of what was downloaded.

ID_LONDON <- "GBE-E23000001-E12000007"      # under the Metropolitan Police force area of the England and Wales module
LONDON_DIR <- file.path(CONFIG$dir_raw, "city_london")
london_sources <- function() read_reference("london_sources.csv")
london_source <- function(key) { s <- london_sources(); u <- s$url[s$key == key]; if (length(u) && !is.na(u) && nzchar(trimws(u))) trimws(u) else NA_character_ }
london_boroughs <- function() read_reference("london_boroughs.csv")
london_dir <- function(sub) { d <- file.path(LONDON_DIR, sub); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
london_newest <- function(sub, pattern) { f <- newest_first(list.files(file.path(LONDON_DIR, sub), pattern, full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

# --- Download: the ward-level CSVs named in the dataset's resource list -------------------------------------------
london_download <- function(days = 20) {
  dir <- london_dir("crime")
  prev <- london_newest("crime", "\\.csv$")
  if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < days) { message("= London crime files downloaded less than ", days, " days ago"); return(invisible(FALSE)) }
  manual <- london_source("ward_csv_manual")
  if (!is.na(manual)) { download_versioned(manual, dir, name = "mps_ward_level_crime_manual.csv"); return(invisible(TRUE)) }
  res <- london_resources()
  res <- res[grepl("ward level crime", res$title, ignore.case = TRUE) & !grepl("old ward", res$title, ignore.case = TRUE) & grepl("csv", paste(res$format, res$url), ignore.case = TRUE), ]
  if (nrow(res) == 0) stop("no ward-level CSV in the dataset's resources: run london_inspect() to see them")
  # the dataset keeps every past version of each file: the newest copy of each title
  res <- res[order(res$coverage_to, res$checked, decreasing = TRUE), ]; res <- res[!duplicated(res$title), ]
  for (i in seq_len(nrow(res))) {
    name <- paste0("mps_ward_", gsub("[^a-z0-9]+", "_", tolower(res$title[i])), ".csv")
    attempt(paste("London", res$title[i]), download_versioned(res$url[i], dir, name = name))
  }
  invisible(TRUE)
}

# Resources of the dataset from the London Datastore API (JSON, resources keyed by id)
london_resources <- function() {
  api <- london_source("dataset_api"); if (is.na(api)) stop("no dataset_api URL in reference/london_sources.csv")
  txt <- get_text(api); if (is.null(txt)) stop("the London Datastore API could not be read (", api, "): put a direct CSV URL in reference/london_sources.csv, key ward_csv_manual")
  j <- jsonlite::fromJSON(txt, simplifyVector = FALSE); r <- j$resources
  if (is.null(r) || length(r) == 0) stop("the API answer has no resources")
  if (!is.null(names(r))) r <- lapply(names(r), function(id) c(r[[id]], list(id = id)))
  field <- function(x, k) if (is.null(x[[k]])) NA_character_ else as.character(x[[k]])
  data.frame(id = vapply(r, field, "", "id"), title = vapply(r, field, "", "title"), format = vapply(r, field, "", "format"), url = vapply(r, field, "", "url"),
             coverage_to = vapply(r, field, "", "temporal_coverage_to"), checked = vapply(r, field, "", "check_timestamp"), stringsAsFactors = FALSE)
}

# A ward-level CSV: MajorText, MinorText, WardName, WardCode, LookUp_BoroughName, then one column per month (YYYYMM)
london_read <- function(f) {
  d <- readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE)
  nm <- names(d)
  pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col <- list(major = pick("^major|^group$"), minor = pick("^minor|^sub.?group$"), ward_name = pick("^ward.?name"), ward_code = pick("^ward.?code|^wd[0-9]*cd"), borough = pick("borough"))   # the historical file says Group / SubGroup
  months <- nm[grepl("^[0-9]{6}$", nm)]
  if (is.na(col$minor) || is.na(col$ward_code) || length(months) == 0) stop("unexpected columns in ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun london_inspect()")
  long <- do.call(rbind, lapply(months, function(mc) data.frame(
    major = if (!is.na(col$major)) d[[col$major]] else NA_character_, minor = d[[col$minor]], ward_name = if (!is.na(col$ward_name)) d[[col$ward_name]] else NA_character_,
    ward_code = trimws(d[[col$ward_code]]), borough = if (!is.na(col$borough)) d[[col$borough]] else NA_character_,
    month = as.Date(paste0(mc, "01"), "%Y%m%d"), n = suppressWarnings(as.numeric(d[[mc]])), file = basename(f), stringsAsFactors = FALSE)))
  long[!is.na(long$n) & !is.na(long$month), ]
}

london_rows <- function() {
  files <- list.files(file.path(LONDON_DIR, "crime"), "\\.csv$", full.names = TRUE)
  if (length(files) == 0) stop("no ward-level CSV in ", file.path(LONDON_DIR, "crime"), " (option 14 with LON)")
  files <- newest_first(files)
  d <- do.call(rbind, lapply(files, london_read))
  # the most recently downloaded file wins for a month it shares with an older one (the 24-month file revises the historical one)
  rank <- match(d$file, basename(files)); d <- d[order(rank), ]
  d <- d[!duplicated(d[, c("ward_code", "minor", "month")]), ]
  d
}

# Borough of each ward from the data (name -> GSS code through the reference table)
london_borough_code <- function(name) {
  b <- london_boroughs(); key <- normalize_text(name)
  alias <- lapply(seq_len(nrow(b)), function(i) c(b$borough_name[i], if (!is.na(b$aliases[i]) && nzchar(b$aliases[i])) strsplit(b$aliases[i], "|", fixed = TRUE)[[1]]))
  all_names <- unlist(alias); all_codes <- rep(b$borough_code, lengths(alias))
  all_codes[match(key, normalize_text(all_names))]
}

# --- Territory: wards from the ONS portal, boroughs dissolved from them -------------------------------------------
london_boundaries_download <- function(ward_codes) {
  dir <- london_dir("boundaries"); svc <- london_source("wards_service"); if (is.na(svc)) stop("no wards_service URL in reference/london_sources.csv")
  prev <- london_newest("boundaries", "_wards_[0-9]+\\.geojson$")
  if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < 365) return(invisible(FALSE))
  # the field of the ward code (WD24CD, WD25CD...) from the service name's vintage
  yy <- regmatches(svc, regexpr("(?<=_)[0-9]{4}(?=_)", svc, perl = TRUE)); fld <- if (length(yy)) paste0("WD", substr(yy, 3, 4), "CD") else "WD24CD"
  chunks <- split(ward_codes, ceiling(seq_along(ward_codes) / 80)); k <- 0L
  for (ch in chunks) {
    k <- k + 1L
    where <- utils::URLencode(sprintf("%s IN (%s)", fld, paste0("'", ch, "'", collapse = ",")), reserved = TRUE)
    url <- sprintf("%s?where=%s&outFields=*&outSR=4326&f=geojson", svc, where)
    download_versioned(url, dir, name = sprintf("london_wards_%02d.geojson", k))
  }
  invisible(TRUE)
}

london_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_LONDON) > 0 && file.exists(CITY_MODULES$london$gpkg)) { message("= London territory kept (", city_neighbourhood_count(con, ID_LONDON), " wards)"); return(invisible(FALSE)) }
  d <- london_rows(); wards <- unique(d[, c("ward_code", "ward_name", "borough")]); wards <- wards[grepl("^E05", wards$ward_code), ]
  wards <- wards[!duplicated(wards$ward_code), ]
  wards$borough_code <- london_borough_code(wards$borough)
  lost <- unique(wards$borough[is.na(wards$borough_code)]); if (length(lost)) message("  boroughs not in reference/london_boroughs.csv (their wards left out): ", paste(lost, collapse = ", "))
  wards <- wards[!is.na(wards$borough_code), ]
  attempt("London ward boundaries download", london_boundaries_download(wards$ward_code))
  files <- list.files(file.path(LONDON_DIR, "boundaries"), "_wards_[0-9]+\\.geojson$", full.names = TRUE)
  if (length(files) == 0) stop("no ward boundaries in ", file.path(LONDON_DIR, "boundaries"))
  # newest copy of every chunk
  k <- sub(".*_wards_([0-9]+)\\.geojson$", "\\1", files); files <- unname(vapply(split(files, k), function(x) newest_first(x)[1], ""))
  g <- do.call(rbind, lapply(files, function(f) { x <- sf::st_read(f, quiet = TRUE); nm <- names(x); cd <- nm[grepl("^WD[0-9]{2}CD$", nm)][1]; nmn <- nm[grepl("^WD[0-9]{2}NM$", nm)][1]
    if (is.na(cd)) stop("no WDyyCD field in ", basename(f), ": ", paste(nm, collapse = ", ")); sf::st_sf(code = x[[cd]], gname = if (!is.na(nmn)) x[[nmn]] else NA_character_, geometry = sf::st_geometry(x)) }))
  g <- g[!duplicated(g$code), ]; g <- g[g$code %in% wards$ward_code, ]
  if (nrow(g) == 0) stop("the boundary files hold none of the ward codes of the data (vintage mismatch? change wards_service in reference/london_sources.csv)")
  g <- sf::st_make_valid(sf::st_transform(g, 4326))
  w <- wards[match(g$code, wards$ward_code), ]
  district_id <- paste0(ID_LONDON, "-D", w$borough_code)
  neighbourhoods <- sf::st_sf(territory_id = paste0(district_id, "-B", g$code), level = "neighbourhood", parent_id = district_id, country = "GBE", official_code = g$code, code_scheme = "ONS GSS (ward)",
                              name = ifelse(is.na(w$ward_name) | !nzchar(w$ward_name), g$gname, w$ward_name), area_m2 = as.numeric(sf::st_area(g)), file = basename(files[1]), geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, district_id)
  b <- london_boroughs(); bname <- b$borough_name[match(sub("^.*-D", "", dis$territory_id), b$borough_code)]
  districts <- sf::st_sf(territory_id = dis$territory_id, level = "district", parent_id = ID_LONDON, country = "GBE", official_code = sub("^.*-D", "", dis$territory_id), code_scheme = "ONS GSS (borough)",
                         name = bname, area_m2 = as.numeric(sf::st_area(dis)), file = basename(files[1]), geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = ID_LONDON, level = "municipality", parent_id = "GBE-E23000001", country = "GBE", official_code = "E12000007", code_scheme = "WebarchyMapp (Metropolitan Police area)",
                      name = "London (Metropolitan Police area)", stringsAsFactors = FALSE)
  missing_poly <- setdiff(wards$ward_code, g$code); if (length(missing_poly)) message("  ", length(missing_poly), " wards of the data without polygon (counted in their borough only)")
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES$london$gpkg)
}

# --- Ingest -------------------------------------------------------------------------------------------------------
london_ingest <- function(con) {
  d <- london_rows(); d <- d[grepl("^E05", d$ward_code), ]
  d$borough_code <- london_borough_code(d$borough); d <- d[!is.na(d$borough_code), ]
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id, official_code FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_LONDON, "-%"))))
  d$neighbourhood_id <- known$territory_id[match(d$ward_code, known$official_code)]
  d$district_id <- paste0(ID_LONDON, "-D", d$borough_code)
  d$event_type <- trimws(d$minor)
  ev <- d |> dplyr::group_by(month, neighbourhood_id, district_id, event_type) |> dplyr::summarise(events = sum(n), .groups = "drop") |> as.data.frame()
  ev$city <- "london"; ev$file <- paste(unique(d$file), collapse = ";"); ev$events <- as.integer(round(ev$events))
  replace_city_rows(con, "stg_city_events", "london", ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  cat_of <- unique(d[, c("event_type", "major")]); cat_of <- cat_of[!duplicated(cat_of$event_type), ]
  sens <- grepl("sexual|rape|homicide", paste(cat_of$major, cat_of$event_type), ignore.case = TRUE)
  t <- data.frame(city = "london", event_type = cat_of$event_type, block = "public_safety", name_local = cat_of$event_type, name_en = cat_of$event_type, sensitive = sens, stringsAsFactors = FALSE)
  replace_city_rows(con, "ref_city_event_types", "london", t)
  log_ingest(con, "city_london", ev$file[1], NA, paste(format(range(ev$month), "%Y-%m"), collapse = " to "), nrow(ev), sum(ev$events))
  message(sprintf("  london: %s event rows, %d months (%s to %s), %d wards, %d types", fmt_int(nrow(ev)), length(unique(ev$month)), format(min(ev$month), "%m/%Y"), format(max(ev$month), "%m/%Y"), length(unique(stats::na.omit(ev$neighbourhood_id))), nrow(t)))
  invisible(ev)
}

# --- Population: a ward population file (any CSV or Excel with a ward code column and a total) ---------------------
# The ONS workbooks (mid-year ward estimates, Census 2021 TS001 by ward) have title rows and several sheets: every
# sheet is scanned for a column of E05 codes; the population is the column whose header says all / total / persons /
# population, else the numeric column with the largest sum. A plain CSV works the same way.
london_scan_population <- function(x) {
  x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE); if (nrow(x) < 2) return(NULL)
  is_code <- function(v) grepl("^E05[0-9]{6}$", trimws(v %||% ""))
  share <- vapply(x, function(v) mean(is_code(v)), numeric(1)); if (max(share, na.rm = TRUE) < 0.3) return(NULL)
  code_col <- which.max(share); rows <- which(is_code(x[[code_col]])); first <- min(rows)
  header <- if (first > 1) tolower(trimws(unlist(x[first - 1, ]))) else rep("", ncol(x))
  num <- function(v) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", v)))
  body <- x[rows, , drop = FALSE]
  numeric_cols <- which(vapply(body, function(v) mean(!is.na(num(v))) > 0.8, logical(1))); numeric_cols <- setdiff(numeric_cols, code_col)
  if (!length(numeric_cols)) return(NULL)
  named <- numeric_cols[grepl("^all|total|persons|population|^all ages", header[numeric_cols])]
  pop_col <- if (length(named)) named[1] else numeric_cols[which.max(vapply(numeric_cols, function(k) sum(num(body[[k]]), na.rm = TRUE), numeric(1)))]
  out <- data.frame(code = trimws(body[[code_col]]), population = num(body[[pop_col]]), stringsAsFactors = FALSE)
  attr(out, "column") <- if (nzchar(header[pop_col])) header[pop_col] else paste("column", pop_col)
  out[!is.na(out$population) & out$population > 0, ]
}

london_population <- function(con) {
  dir <- london_dir("population")
  manual <- london_source("population_manual"); if (!is.na(manual) && length(list.files(dir)) == 0) attempt("London population download", download_versioned(manual, dir))
  files <- newest_first(list.files(dir, "\\.(csv|xlsx|xls)$", full.names = TRUE))
  if (length(files) == 0) stop("no ward population file in ", dir, " (ONS mid-year ward estimates or Census 2021 by ward): counts only, no rates")
  f <- files[1]; pop <- NULL
  if (grepl("\\.csv$", f)) pop <- london_scan_population(readr::read_csv(f, col_names = FALSE, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE))
  else for (sh in readxl::excel_sheets(f)) { pop <- london_scan_population(readxl::read_excel(f, sheet = sh, col_names = FALSE, col_types = "text")); if (!is.null(pop) && nrow(pop) > 100) break }
  if (is.null(pop) || nrow(pop) == 0) stop("no column of ward codes (E05...) with a population next to it in ", basename(f))
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id, official_code FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_LONDON, "-%"))))
  pop$territory_id <- known$territory_id[match(pop$code, known$official_code)]; lost <- sum(is.na(pop$territory_id)); pop <- pop[!is.na(pop$territory_id), ]
  if (nrow(pop) == 0) stop("none of the ward codes of ", basename(f), " is a ward of the data (different ward vintage?)")
  yr <- regmatches(basename(f), regexpr("20[0-9]{2}", basename(f))); ref <- as.Date(sprintf("%s-06-30", if (length(yr)) yr else format(Sys.Date(), "%Y")))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_LONDON, "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = pop$territory_id, ref_date = ref, population = as.integer(round(pop$population)), file = basename(f)))
  })
  message(sprintf("  population: %d wards from %s (%s); %d codes of the file are not wards of the data", nrow(pop), basename(f), attr(pop, "column") %||% "", lost))
  invisible(TRUE)
}

london_inspect <- function() {
  for (sub in c("crime", "boundaries", "population")) { cat("\n", sub, ":\n", sep = ""); print(list.files(file.path(LONDON_DIR, sub))) }
  for (f in list.files(file.path(LONDON_DIR, "crime"), "\\.csv$", full.names = TRUE)) { cat("\n", basename(f), "\n"); print(names(readr::read_csv(f, n_max = 2, col_types = readr::cols(.default = "c"), show_col_types = FALSE))) }
  res <- tryCatch(london_resources(), error = function(e) { cat("\nAPI:", conditionMessage(e), "\n"); NULL })
  if (!is.null(res)) { res <- res[order(res$coverage_to, decreasing = TRUE), ]; print(utils::head(res[grepl("ward", res$title, ignore.case = TRUE), c("title", "format", "coverage_to", "url")], 12)) }
  invisible(TRUE)
}

register_city(list(
  code = "LON", key = "london", id = ID_LONDON, name_en = "London", name_local = "London", lang = "en", country = "GBE",
  measure_kind = "offences", stats_level = "district",
  boundaries_attribution = "Office for National Statistics, Open Geography portal (OGL v3); contains OS data © Crown copyright",
  method = list(where = "Ward where the offence took place, as recorded by the Metropolitan Police", date = "Month the offence was recorded (not when it happened)", revisions = "The 24-month file revises recent months on every publication; the historical file is republished"),
  source = list(name = "Metropolitan Police recorded crime by ward (London Datastore)", name_local = "Metropolitan Police recorded crime by ward (London Datastore)",
                publisher = "Greater London Authority / Metropolitan Police Service", url = "https://data.london.gov.uk/dataset/recorded_crime_summary", licence = "Open Government Licence",
                measure = "Offences recorded by the Metropolitan Police under the Home Office Counting Rules, by month, ward, borough and category (major / minor text); the City of London (its own police force) is not included. No official total in the file: the total shown is the sum of the categories."),
  about_source_key = "about_src_london", about_measures_key = "about_london_measures", about_territory_key = "about_london_territory",
  download = london_download, territory = london_territory, ingest = london_ingest, population = london_population
))
