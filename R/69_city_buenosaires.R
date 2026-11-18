# Buenos Aires (BUE): reported offences by barrio and comuna, monthly -------------------------------------------
# City module of the shared city framework (R/60_cities.R). Source: "Delitos" on the Buenos Aires City open-data
# portal (Ministerio de Justicia y Seguridad, Creative Commons Attribution): one CSV per year since 2016, one row
# per reported offence with its type and sub-type (robo, hurto, lesiones, homicidio, amenazas, road traffic
# casualties), date, hour band, barrio, comuna and coordinates. The module keeps only month × barrio × sub-type
# (no day, hour or coordinates leave the raw file). Hierarchy: the city (a node under the AR-02 province of the
# Argentina module, which is independent of it) > the 15 comunas as districts > the 48 barrios as neighbourhoods
# (barrios GeoJSON of the same portal). Types: sub-type (reference/buenosaires_event_types.csv; an unknown one is
# kept under its own name; homicide and femicide count in the totals only; road traffic casualties are loaded but
# not shown by area). Population: not in the source; a file by barrio or comuna in data/raw/city_buenosaires/
# population/ gives the rates, otherwise counts only.
#
# Written against the published file layout (checked on the 2024 file), not run against the live files:
# `buenosaires_inspect()` prints the headers of what was downloaded.

ID_BUENOSAIRES <- "AR-02-CABA"
BUE_DIR <- file.path(CONFIG$dir_raw, "city_buenosaires")
BUE_MONTHS <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")
bue_sources <- function() read_reference("buenosaires_sources.csv")
bue_source <- function(key) { s <- bue_sources(); u <- s$url[s$key == key]; if (length(u) && !is.na(u) && nzchar(trimws(u))) trimws(u) else NA_character_ }
bue_dir <- function(sub) { d <- file.path(BUE_DIR, sub); dir.create(d, recursive = TRUE, showWarnings = FALSE); d }
bue_newest <- function(sub, pattern) { f <- newest_first(list.files(file.path(BUE_DIR, sub), pattern, full.names = TRUE)); if (length(f)) f[1] else NA_character_ }
bue_district_id <- function(comuna) paste0(ID_BUENOSAIRES, "-D", sprintf("%02d", as.integer(comuna)))
# A barrio name as a comparable key: the layer says "La Boca", the data "BOCA"; accents and case folded
bue_key <- function(x) sub("^(LA|EL|LOS|LAS) ", "", normalize_text(x))

# --- Download: the yearly CSVs (missing years, and the two latest refreshed) and the barrios layer ----------------
bue_download <- function(days = 20, since = 2016L) {
  pattern <- bue_source("csv_pattern"); if (is.na(pattern)) stop("no csv_pattern in reference/buenosaires_sources.csv")
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  for (y in since:this_year) {
    prev <- bue_newest("delitos", sprintf("_delitos_%d\\.csv$", y))
    if (!is.na(prev) && (y < this_year - 1 || file_age_days(prev) < days)) next
    tryCatch(download_versioned(sub("{year}", y, pattern, fixed = TRUE), bue_dir("delitos"), name = sprintf("delitos_%d.csv", y)),
             error = function(e) if (y == this_year && grepl("404", conditionMessage(e))) message("  Buenos Aires ", y, ": not published yet (the portal adds the year once it is complete)") else message("Buenos Aires ", y, ": ", conditionMessage(e), "\n(The rest continues.)"))
  }
  prev <- bue_newest("boundaries", "\\.geojson$")
  if (is.na(prev) || file_age_days(prev) > 365) download_versioned(bue_source("barrios_geojson"), bue_dir("boundaries"), name = "barrios.geojson")
  invisible(TRUE)
}

bue_read <- function(f) {
  d <- readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
  nm <- names(d); pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col <- list(year = pick("^anio$|^año$"), month = pick("^mes$"), date = pick("^fecha$"), tipo = pick("^tipo$"), subtipo = pick("^subtipo$"), barrio = pick("^barrio$"), comuna = pick("^comuna$"), n = pick("^cantidad$"))
  if (is.na(col$tipo) || (is.na(col$date) && (is.na(col$year) || is.na(col$month)))) stop("unexpected columns in ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun buenosaires_inspect()")
  month <- if (!is.na(col$date)) as.Date(substr(d[[col$date]], 1, 10)) else NA
  if (all(is.na(month))) month <- as.Date(sprintf("%s-%02d-01", d[[col$year]], match(normalize_text(d[[col$month]]), BUE_MONTHS)))
  data.frame(month = as.Date(format(month, "%Y-%m-01")), tipo = d[[col$tipo]], subtipo = if (!is.na(col$subtipo)) d[[col$subtipo]] else d[[col$tipo]],
             barrio = if (!is.na(col$barrio)) d[[col$barrio]] else NA_character_, comuna = if (!is.na(col$comuna)) suppressWarnings(as.integer(d[[col$comuna]])) else NA_integer_,
             n = if (!is.na(col$n)) suppressWarnings(as.numeric(d[[col$n]])) else 1, file = basename(f), stringsAsFactors = FALSE)
}

# --- Territory: barrios (neighbourhoods) and comunas (districts) from the barrios layer -----------------------------
bue_territory <- function(con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, ID_BUENOSAIRES) > 1 && file.exists(CITY_MODULES$buenosaires$gpkg)) { message("= Buenos Aires territory kept (", city_neighbourhood_count(con, ID_BUENOSAIRES), " barrios)"); return(invisible(FALSE)) }
  f <- bue_newest("boundaries", "\\.geojson$"); if (is.na(f)) stop("no barrios layer in ", file.path(BUE_DIR, "boundaries"), " (option 14 with BUE)")
  g <- sf::st_read(f, quiet = TRUE); nm <- names(g)
  pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col_name <- pick("^nombre$|^barrio$|^name$"); col_com <- pick("^comuna$")
  if (is.na(col_name) || is.na(col_com)) stop("barrios layer: no nombre / comuna columns; fields are ", paste(nm, collapse = ", "))
  g <- sf::st_make_valid(sf::st_transform(g, 4326))
  name <- as.character(g[[col_name]]); comuna <- suppressWarnings(as.integer(g[[col_com]]))
  g <- g[!is.na(comuna), ]; name <- name[!is.na(comuna)]; comuna <- comuna[!is.na(comuna)]
  code <- sprintf("%02d%02d", comuna, stats::ave(seq_along(name), comuna, FUN = seq_along))
  district_id <- bue_district_id(comuna)
  neighbourhoods <- sf::st_sf(territory_id = paste0(district_id, "-B", code), level = "neighbourhood", parent_id = district_id, country = "AR", official_code = normalize_text(name), code_scheme = "GCBA barrio (name)",
                              name = tools::toTitleCase(tolower(name)), area_m2 = as.numeric(sf::st_area(g)), file = basename(f), geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, district_id)
  districts <- sf::st_sf(territory_id = dis$territory_id, level = "district", parent_id = ID_BUENOSAIRES, country = "AR", official_code = sub("^.*-D", "", dis$territory_id), code_scheme = "GCBA comuna",
                         name = paste("Comuna", as.integer(sub("^.*-D", "", dis$territory_id))), area_m2 = as.numeric(sf::st_area(dis)), file = basename(f), geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = ID_BUENOSAIRES, level = "municipality", parent_id = "AR-02", country = "AR", official_code = "02", code_scheme = "WebarchyMapp (Ciudad Autónoma)", name = "Buenos Aires", stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES$buenosaires$gpkg)
}

# --- Ingest ---------------------------------------------------------------------------------------------------------
bue_ingest <- function(con) {
  files <- list.files(file.path(BUE_DIR, "delitos"), "_delitos_[0-9]{4}\\.csv$", full.names = TRUE)
  if (!length(files)) stop("no yearly CSV in ", file.path(BUE_DIR, "delitos"), " (option 14 with BUE)")
  yr <- sub(".*_delitos_([0-9]{4})\\.csv$", "\\1", files); files <- unname(vapply(split(files, yr), function(x) newest_first(x)[1], ""))   # newest copy of every year
  d <- do.call(rbind, lapply(files, bue_read)); d <- d[!is.na(d$month) & !is.na(d$n), ]
  types <- read_reference("buenosaires_event_types.csv", bool_cols = "sensitive")
  sub <- trimws(d$subtipo); sub[is.na(sub) | !nzchar(sub)] <- trimws(d$tipo[is.na(sub) | !nzchar(sub)])
  d$event_type <- types$source_code[match(normalize_text(sub), normalize_text(types$source_code))]
  d$event_type[is.na(d$event_type)] <- sub[is.na(d$event_type)]
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id, parent_id, official_code FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_BUENOSAIRES, "-%"))))
  d$neighbourhood_id <- known$territory_id[match(bue_key(d$barrio), bue_key(known$official_code))]
  d$district_id <- ifelse(!is.na(d$neighbourhood_id), known$parent_id[match(d$neighbourhood_id, known$territory_id)], ifelse(!is.na(d$comuna) & d$comuna >= 1 & d$comuna <= 15, bue_district_id(d$comuna), NA))
  lost <- unique(d$barrio[is.na(d$neighbourhood_id) & !is.na(d$barrio)]); lost <- lost[!normalize_text(lost) %in% c(NA, "SIN DATO", "NO ESPECIFICADO", "SD")]
  if (length(lost)) message("  barrios of the data not in the layer (counted in their comuna): ", paste(utils::head(lost, 8), collapse = ", "))
  ev <- d |> dplyr::group_by(month, neighbourhood_id, district_id, event_type) |> dplyr::summarise(events = sum(n), .groups = "drop") |> as.data.frame()
  ev$city <- "buenosaires"; ev$file <- paste(basename(files), collapse = ";"); ev$events <- as.integer(round(ev$events))
  replace_city_rows(con, "stg_city_events", "buenosaires", ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  seen <- unique(ev$event_type); knownt <- types[types$source_code %in% seen, ]; extra <- setdiff(seen, knownt$source_code)
  plain <- function(x) { x <- tolower(x); paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))) }
  t <- rbind(data.frame(city = "buenosaires", event_type = knownt$source_code, block = knownt$block, name_local = knownt$name_local, name_en = knownt$name_en, sensitive = knownt$sensitive, stringsAsFactors = FALSE),
             if (length(extra)) data.frame(city = "buenosaires", event_type = extra, block = ifelse(grepl("vial", extra, ignore.case = TRUE), "traffic", "public_safety"), name_local = plain(extra), name_en = plain(extra),
                                           sensitive = grepl("homicidio|femicidio|sexual|violaci", extra, ignore.case = TRUE), stringsAsFactors = FALSE))
  if (length(extra)) message("  sub-types not in buenosaires_event_types.csv (kept under their own name): ", paste(extra, collapse = " / "))
  replace_city_rows(con, "ref_city_event_types", "buenosaires", t)
  log_ingest(con, "city_buenosaires", ev$file[1], NA, paste(format(range(ev$month), "%Y-%m"), collapse = " to "), nrow(ev), sum(ev$events))
  message(sprintf("  buenosaires: %s offences in %s event rows, %d months (%s to %s), %d barrios, %d types; %d%% without barrio (comuna totals)", fmt_int(sum(ev$events)), fmt_int(nrow(ev)), length(unique(ev$month)),
                  format(min(ev$month), "%m/%Y"), format(max(ev$month), "%m/%Y"), length(unique(stats::na.omit(ev$neighbourhood_id))), nrow(t), round(100 * sum(ev$events[is.na(ev$neighbourhood_id)]) / sum(ev$events))))
  invisible(ev)
}

# --- Population: a file by barrio or by comuna (none in the source) --------------------------------------------------
# A population table by barrio or comuna: a CSV, or an Excel cuadro of the INDEC / the city's statistics office
# (a title block above the header row; "Comuna 1"... or 1...; the latest census year as a column). Returns
# data.frame(unit = "barrio" | "comuna", key, population, year)
bue_read_population <- function(f) {
  year_of <- function(x) { m <- regmatches(x, regexpr("(19|20)[0-9]{2}", x)); if (length(m)) as.integer(m[1]) else NA_integer_ }
  file_year <- year_of(sub("^[0-9]{8}_", "", basename(f)))          # "c2022_caba_est_c1_1.xlsx" -> 2022, not the date of the download
  if (grepl("\\.xlsx?$", f, ignore.case = TRUE)) {
    sheets <- readxl::excel_sheets(f); best <- NULL
    is_year <- function(v) grepl("^(19|20)[0-9]{2}$", v)
    for (sh in sheets) {
      x <- suppressMessages(readxl::read_excel(f, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal")); x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
      hit <- which(apply(x, 1, function(r) any(grepl("^(comuna|barrio)s?$", trimws(r), ignore.case = TRUE)) & sum(!is.na(r)) >= 2))
      if (!length(hit)) next
      h <- hit[1]; header <- trimws(unlist(x[h, ])); header[is.na(header)] <- ""
      key_col <- which(grepl("^(comuna|barrio)s?$", header, ignore.case = TRUE))[1]
      # The INDEC cuadros spread the header over two lines: the labels ("Población", "Variación absoluta"...) and,
      # below, the census years of each column ("2010 | 2022"). The row under the labels is a header line too when it
      # holds a year and no other number (a data row starts with a code or a figure)
      header2 <- if (h < nrow(x)) { v <- trimws(unlist(x[h + 1, ])); v[is.na(v)] <- ""; v } else rep("", ncol(x))
      two_lines <- any(is_year(header2)) && !any(grepl("^[0-9][0-9.,]*$", header2) & !is_year(header2))
      years <- unname(vapply(header, year_of, 1L)); if (two_lines) years <- ifelse(is_year(header2), suppressWarnings(as.integer(header2)), years)
      years[key_col] <- NA
      # The population column: the latest census year, else the column labelled población / habitantes / total
      pop_col <- if (any(!is.na(years))) which.max(years) else { p <- which(grepl("poblaci|habitantes|^total", header, ignore.case = TRUE)); if (length(p)) p[1] else NA }
      if (is.na(pop_col)) next
      if (two_lines) h <- h + 1
      body <- x[(h + 1):nrow(x), , drop = FALSE]
      key <- trimws(body[[key_col]]); pop <- suppressWarnings(as.numeric(gsub("[^0-9]", "", body[[pop_col]])))
      keep <- !is.na(key) & nzchar(key) & !grepl("^total|^ciudad|^fuente|^nota", key, ignore.case = TRUE) & !is.na(pop)
      if (sum(keep) < 2) next
      unit <- if (grepl("barrio", header[key_col], ignore.case = TRUE)) "barrio" else "comuna"
      yr <- if (!is.na(years[pop_col])) years[pop_col] else if (!is.na(year_of(sh))) year_of(sh) else file_year
      best <- data.frame(unit = unit, key = key[keep], population = pop[keep], year = yr, stringsAsFactors = FALSE); break
    }
    if (is.null(best)) stop("no sheet of ", basename(f), " has a 'Comuna' or 'Barrio' header with a population column (a census year or 'Población')")
    return(best)
  }
  d <- readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE)
  nm <- names(d); pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col_b <- pick("^barrio"); col_c <- pick("^comuna"); col_p <- pick("poblaci|^total|habitantes|^population")
  if (is.na(col_p) || (is.na(col_b) && is.na(col_c))) stop("population file: needs a barrio or comuna column and a population column; found ", paste(nm, collapse = ", "))
  yr <- year_of(col_p); if (is.na(yr)) yr <- file_year                # "poblacion_2022" names the year, else the file does
  data.frame(unit = if (!is.na(col_b)) "barrio" else "comuna", key = trimws(d[[if (!is.na(col_b)) col_b else col_c]]), population = suppressWarnings(as.numeric(gsub("[^0-9.]", "", d[[col_p]]))),
             year = yr, stringsAsFactors = FALSE)
}

# --- Population: a file by barrio or by comuna (the INDEC census cuadro by comuna is downloaded when nothing is there)
bue_population <- function(con) {
  dir <- bue_dir("population"); manual <- bue_source("population_manual")
  files <- newest_first(list.files(dir, full.names = TRUE)); files <- files[!dir.exists(files)]
  if (!is.na(manual) && !length(files)) { attempt("Buenos Aires population download", download_versioned(manual, dir)); files <- newest_first(list.files(dir, full.names = TRUE)) }
  if (!length(files)) stop("no population file in ", dir, " (a CSV with barrio or comuna and population; Censo 2022 by comuna, or the barrio estimates of the GCBA): counts only, no rates")
  f <- files[1]; d <- bue_read_population(f); d <- d[!is.na(d$population) & d$population > 0, ]
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id, level, official_code FROM dim_territory WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_BUENOSAIRES, "-%"))))
  ids <- if (d$unit[1] == "barrio") known$territory_id[known$level == "neighbourhood"][match(bue_key(d$key), bue_key(known$official_code[known$level == "neighbourhood"]))] else
    bue_district_id(suppressWarnings(as.integer(gsub("[^0-9]", "", d$key))))
  keep <- !is.na(ids) & ids %in% known$territory_id
  if (!any(keep)) stop("no row of ", basename(f), " matches a barrio or comuna (first keys: ", paste(utils::head(d$key, 5), collapse = ", "), ")")
  yr <- d$year[1]; ref <- as.Date(sprintf("%d-01-01", if (!is.na(yr)) yr else as.integer(format(Sys.Date(), "%Y"))))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(ID_BUENOSAIRES, "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = ids[keep], ref_date = ref, population = as.integer(round(d$population[keep])), file = basename(f)))
  })
  message(sprintf("  population: %d %s from %s (%s residents, %s)", sum(keep), if (d$unit[1] == "barrio") "barrios" else "comunas", basename(f), fmt_int(sum(d$population[keep])), if (!is.na(yr)) yr else "year unknown"))
  invisible(TRUE)
}

buenosaires_inspect <- function() {
  for (sub in c("delitos", "boundaries", "population")) { cat("\n", sub, ":\n", sep = ""); print(list.files(file.path(BUE_DIR, sub))) }
  for (f in utils::head(newest_first(list.files(file.path(BUE_DIR, "delitos"), "\\.csv$", full.names = TRUE)), 2)) { cat("\n", basename(f), "\n"); x <- readr::read_csv(f, n_max = 2000, col_types = readr::cols(.default = "c"), show_col_types = FALSE); print(names(x)); if ("subtipo" %in% names(x)) print(table(x$subtipo)) }
  invisible(TRUE)
}

register_city(list(
  code = "BUE", key = "buenosaires", id = ID_BUENOSAIRES, name_en = "Buenos Aires", name_local = "Buenos Aires", lang = "es", country = "AR",
  freshness_months = 20,                                        # monthly figures in one file per year: the portal adds a year once it is complete
  measure_kind = "offences", stats_level = "district",
  boundaries_attribution = "Gobierno de la Ciudad de Buenos Aires, Buenos Aires Data (barrios)",
  method = list(where = "Barrio and comuna where the offence took place, as geolocated by the city", date = "Date of the offence (month)", revisions = "One file per year, republished; the two latest years are refreshed on every update"),
  source = list(name = "Reported offences by barrio (Buenos Aires Data)", name_local = "Delitos por barrio (Buenos Aires Data)",
                publisher = "Gobierno de la Ciudad de Buenos Aires, Ministerio de Justicia y Seguridad", url = "https://data.buenosaires.gob.ar/dataset/delitos", licence = "Creative Commons Attribution (Buenos Aires Data terms)",
                measure = "Offences reported to the police and justice system of the City, geolocated to a barrio, by month and sub-type (robbery, theft, injuries, threats; homicide and femicide in the totals only; road traffic casualties loaded but not shown by area). No official total: the total shown is the sum of the sub-types."),
  about_source_key = "about_src_buenosaires", about_measures_key = "about_buenosaires_measures", about_territory_key = "about_buenosaires_territory",
  download = bue_download, territory = bue_territory, ingest = bue_ingest, population = bue_population
))
