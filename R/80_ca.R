# Canada: police-reported crime by police service and province ------------------------------
# Source: Statistics Canada, Uniform Crime Reporting Survey (UCR), "Incident-based crime
# statistics, by detailed violations": table 35-10-0177-01 (Canada, provinces and territories,
# census metropolitan areas) and the eight police-service tables 35-10-0178-01 to 35-10-0185-01
# (one per region: Atlantic, Quebec, Ontario, Manitoba, Saskatchewan, Alberta, British Columbia,
# territories). Yearly, 1998 onwards, revised each July. Statistics Canada Open Licence.
# Measure: "Actual incidents" = criminal incidents substantiated by the police, counted by the
# most serious violation of the incident. Levels: region = province/territory (13),
# municipality = police service / RCMP detachment / OPP-SQ detachment (the unit StatCan
# publishes; its area is a policing jurisdiction, not a municipality). The national row comes
# from 35-10-0177-01 (or the sum of the provinces when that table is not downloaded).
# Census metropolitan areas and the Canadian Forces Military Police are skipped.
#
# Total = "Total, all Criminal Code violations (excluding traffic) [50]": StatCan's headline
# crime rate. Criminal Code traffic (impaired driving) is unmapped; drug and other federal
# statute violations are listed as types but sit outside that total (see ca_offence_types.csv).
# Population = StatCan's own policing-area denominator, recovered from the published rate:
# population = actual incidents / rate per 100,000 x 100,000 (for "Total, all violations [0]").
#
# The full-table CSV zips are large (hundreds of MB zipped, several GB unzipped for Ontario and
# Quebec): the CSV is extracted to the session temp folder and filtered by DuckDB (streaming),
# never loaded whole into R. Written against the published structure (REF_DATE, GEO, DGUID,
# Violations, Statistics, UOM, ..., VALUE, STATUS, SYMBOL, TERMINATED, DECIMALS), verified on
# samples of 35-10-0177/0178/0179/0180/0185. `ca_inspect()` prints the header of each file.

CA_STATE <- new.env()   # population recovered from the rates, handed from ca_ingest to ca_population
CA_TABLES <- c(                    # StatCan product ids; full-table zip = https://www150.statcan.gc.ca/n1/tbl/csv/<pid>-eng.zip
  national = "35100177",           # Canada, provinces/territories, CMAs (national row; provinces as fallback)
  atlantic = "35100178", quebec = "35100179", ontario = "35100180", manitoba = "35100181",
  saskatchewan = "35100182", alberta = "35100183", british_columbia = "35100184", territories = "35100185"
)
CA_ZIP_URL <- "https://www150.statcan.gc.ca/n1/tbl/csv/%s-eng.zip"
CA_WDS_URL <- "https://www150.statcan.gc.ca/t1/wds/rest/getFullTableDownloadCSV/%s/en"   # JSON {"status","object" = zip url}
# 2021 Census cartographic boundary file, provinces and territories (ESRI REST, GeoJSON, WGS84, simplified server-side)
CA_PROVINCES_URL <- paste0("https://geo.statcan.gc.ca/geo_wa/rest/services/2021/Cartographic_boundary_files/MapServer/0/query",
                           "?where=1%3D1&outFields=PRUID,PRENAME,PRFNAME,DGUID&outSR=4326&maxAllowableOffset=0.005&geometryPrecision=4&f=geojson")
CA_STATISTICS <- c(incidents = "Actual incidents", rate = "Rate per 100,000 population")
CA_DGUID <- list(country = "2021A000011124", region = "^2021A0002", municipality = "^2021A0025")

# Accepted names for every field the loader needs (first match wins, case-insensitive)
CA_COLUMNS <- list(
  year      = c("^REF_DATE$", "^P.RIODE_DE_R.F.RENCE$", "^year$"),
  geo       = c("^GEO$", "^G.O$"),
  dguid     = c("^DGUID$", "^IDUGD$"),
  violation = c("^Violations?$", "^Infractions?$"),
  statistic = c("^Statistics$", "^Statistiques$"),
  value     = c("^VALUE$", "^VALEUR$"),
  status    = c("^STATUS$", "^STATUT$")
)
ca_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }

# Newest downloaded zip of a table (NA if none)
ca_zip <- function(table) {
  f <- newest_first(list.files(COUNTRY_MODULES$CA$dir_raw, paste0("_", CA_TABLES[[table]], "-eng\\.zip$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}
# Name of the data CSV inside a StatCan zip (<pid>.csv; the <pid>_MetaData.csv is ignored)
ca_csv_name <- function(zip) {
  info <- utils::unzip(zip, list = TRUE)
  main <- info$Name[grepl("\\.csv$", info$Name, ignore.case = TRUE) & !grepl("metadata", info$Name, ignore.case = TRUE)]
  if (length(main) != 1) stop("Expected one data CSV inside ", basename(zip), "; found: ", paste(info$Name, collapse = ", "))
  list(name = main, size = info$Length[info$Name == main])
}

# Print the content and the header of every downloaded zip (to adjust CA_COLUMNS if StatCan renames fields)
ca_inspect <- function() {
  for (tb in names(CA_TABLES)) {
    z <- ca_zip(tb); if (is.na(z)) { message(tb, ": not downloaded"); next }
    csv <- ca_csv_name(z)
    message(sprintf("%s: %s -> %s (%.0f MB uncompressed)", tb, basename(z), csv$name, csv$size / 1e6))
    h <- unz(z, csv$name); first <- readLines(h, n = 3, warn = FALSE, encoding = "UTF-8"); close(h)
    message("  ", paste(first, collapse = "\n  "))
  }
}

# Download the full-table zips (yearly release in July: skipped when less than `days` old)
ca_download <- function(days = 90, tables = names(CA_TABLES)) {
  m <- COUNTRY_MODULES$CA
  for (tb in tables) {
    prev <- ca_zip(tb)
    if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < days) { message("= CA ", tb, " table downloaded less than ", days, " days ago"); next }
    pid <- CA_TABLES[[tb]]
    # The Web Data Service confirms the zip address; the known pattern is used if it does not answer
    url <- sprintf(CA_ZIP_URL, pid)
    wds <- get_text(sprintf(CA_WDS_URL, pid))
    if (!is.null(wds)) { u <- regmatches(wds, regexpr('https://[^"]+\\.zip', wds)); if (length(u) == 1) url <- u }
    message("  CA ", tb, " (", pid, "): large file, please wait")
    download_versioned(url, m$dir_raw, name = paste0(pid, "-eng.zip"))
  }
}

# Extract the data CSV of a zip into the session temp folder (kept while the size matches)
ca_extract <- function(zip) {
  csv <- ca_csv_name(zip)
  dir <- file.path(tempdir(), "ca_statcan", tools::file_path_sans_ext(basename(zip)))
  out <- file.path(dir, csv$name)
  if (!file.exists(out) || file.size(out) != csv$size) {
    message(sprintf("  extracting %s from %s (%.0f MB on disk)", csv$name, basename(zip), csv$size / 1e6))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    ok <- tryCatch({ utils::unzip(zip, files = csv$name, exdir = dir, overwrite = TRUE); TRUE }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!ok || !file.exists(out)) {   # zip64 archives: fall back to the system unzip when R's internal one fails
      if (nzchar(Sys.which("unzip"))) utils::unzip(zip, files = csv$name, exdir = dir, overwrite = TRUE, unzip = Sys.which("unzip"))
      if (!file.exists(out)) stop("Could not extract ", csv$name, " from ", basename(zip))
    }
  }
  out
}

# Read one table with DuckDB: only the two statistics and the violation codes asked for.
# Returns year, geo, dguid, code (UCR code taken from the square brackets), statistic, value, status.
ca_read <- function(con, csv, codes) {
  str <- function(x) as.character(DBI::dbQuoteString(con, x)); id <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  src <- sprintf("read_csv(%s, header = true, all_varchar = true)", str(csv))
  nm <- names(DBI::dbGetQuery(con, paste0("SELECT * FROM ", src, " LIMIT 0")))
  nm <- sub("^﻿", "", nm)
  col <- lapply(CA_COLUMNS, ca_pick, nm = nm)
  if (any(is.na(unlist(col[c("year", "geo", "violation", "statistic", "value")]))))
    stop("Unexpected columns in ", basename(csv), ": ", paste(nm, collapse = " | "), "\nRun ca_inspect() and adjust CA_COLUMNS in R/80_ca.R")
  code_expr <- sprintf("regexp_extract(%s, '\\[(\\d+)\\]\\s*$', 1)", id(col$violation))
  sql <- sprintf("SELECT TRY_CAST(%s AS INTEGER) AS year, %s AS geo, %s AS dguid, %s AS code, %s AS statistic,
                         TRY_CAST(%s AS DOUBLE) AS value, %s AS status
                  FROM %s WHERE %s IN (%s) AND %s IN (%s)",
                 id(col$year), id(col$geo), if (is.na(col$dguid)) "''" else id(col$dguid), code_expr, id(col$statistic),
                 id(col$value), if (is.na(col$status)) "''" else id(col$status),
                 src, id(col$statistic), paste(str(unname(CA_STATISTICS)), collapse = ", "),
                 code_expr, paste(str(codes), collapse = ", "))
  DBI::dbGetQuery(con, sql)
}

# Geography of the rows: level and territory id from the DGUID (fallback: the code in the GEO label).
# CA (country), CA-<pp> (province/territory), CA-<pp>-<ppnnn> (police service, pp = its province).
ca_geography <- function(d, keep) {
  gcode <- ifelse(grepl("\\[\\d+\\]\\s*$", d$geo), sub(".*\\[(\\d+)\\]\\s*$", "\\1", d$geo), NA_character_)
  dg <- ifelse(is.na(d$dguid), "", d$dguid)
  level <- ifelse(dg == CA_DGUID$country | trimws(d$geo) == "Canada", "country",
           ifelse(grepl(CA_DGUID$region, dg) | (dg == "" & nchar(gcode) == 2), "region",
           ifelse(grepl(CA_DGUID$municipality, dg) | (dg == "" & nchar(gcode) == 5), "municipality", NA_character_)))
  d$level <- level; d$gcode <- gcode
  d$territory_id <- ifelse(level == "country", "CA", ifelse(level == "region", paste0("CA-", gcode), paste("CA", substr(gcode, 1, 2), gcode, sep = "-")))
  d$parent_id <- ifelse(level == "region", "CA", ifelse(level == "municipality", paste0("CA-", substr(gcode, 1, 2)), NA))
  d$name <- trimws(sub("\\s*\\[\\d+\\]\\s*$", "", d$geo))
  d[!is.na(d$level) & d$level %in% keep & (d$level == "country" | !is.na(gcode)), ]
}

ca_ingest <- function(con) {
  types <- read_reference("ca_offence_types.csv")
  wanted <- unique(c(types$source_code[types$offence_type_id != "unmapped"], "0"))    # "0" = all violations, for the population
  rows <- list(); pops <- list(); files <- character(); CA_STATE$prov <- list()
  for (tb in names(CA_TABLES)) {
    z <- ca_zip(tb); if (is.na(z)) { message("  no CA ", tb, " table yet"); next }
    d <- ca_read(con, ca_extract(z), wanted)
    d <- ca_geography(d, keep = if (tb == "national") c("country", "region") else c("region", "municipality"))
    d <- d[!is.na(d$year), ]
    # ".." (not available: the service did not exist or did not report) is dropped; "x"/"F" (suppressed) stays as NA
    d <- d[!is.na(d$value) | trimws(d$status) %in% c("x", "F"), ]
    files <- c(files, basename(z))
    inc <- d[d$statistic == CA_STATISTICS[["incidents"]], ]
    if (tb != "national") inc <- inc[inc$level == "municipality", ]      # provinces of the regional tables only as fallback
    rows[[tb]] <- data.frame(country = "CA", territory_id = inc$territory_id, level = inc$level, offence_code = inc$code, year = inc$year, n = inc$value, file = basename(z), stringsAsFactors = FALSE)
    if (tb != "national") {
      # Provinces from the regional table: used only when the national table is missing
      CA_STATE$prov[[tb]] <- d[d$statistic == CA_STATISTICS[["incidents"]] & d$level == "region", c("territory_id", "level", "code", "year", "value")]
      u <- inc[!duplicated(inc$territory_id), ]
      ensure_territories(con, "CA", u$territory_id, "municipality", u$parent_id, u$name)
    }
    # Population recovered from the rate of "Total, all violations [0]" (StatCan's policing-area population)
    a <- d[d$code == "0", c("territory_id", "level", "year", "statistic", "value")]
    i <- a[a$statistic == CA_STATISTICS[["incidents"]], ]; r <- a[a$statistic == CA_STATISTICS[["rate"]], ]
    p <- merge(i, r, by = c("territory_id", "level", "year"), suffixes = c("_n", "_r"))
    p <- p[!is.na(p$value_n) & !is.na(p$value_r) & p$value_r > 0, ]
    if (nrow(p)) pops[[tb]] <- data.frame(country = "CA", territory_id = p$territory_id, level = p$level, year = p$year, population = round(p$value_n / p$value_r * 1e5))
    if (nrow(inc)) message(sprintf("  %s: %s rows, %d territories, %d-%d", tb, fmt_int(nrow(inc)), length(unique(inc$territory_id)), min(inc$year), max(inc$year)))
    else message("  ", tb, ": no usable rows (run ca_inspect())")
  }
  if (length(rows) == 0) stop("No Statistics Canada table found in ", COUNTRY_MODULES$CA$dir_raw)
  crime <- do.call(rbind, rows)
  crime <- crime[crime$offence_code != "0", ]                       # "[0]" includes traffic and federal statutes: not shown
  if (!"region" %in% crime$level && length(CA_STATE$prov)) {        # national table missing: provinces from the regional tables
    pr <- do.call(rbind, CA_STATE$prov); pr <- pr[pr$code != "0", ]
    crime <- rbind(crime, data.frame(country = "CA", territory_id = pr$territory_id, level = pr$level, offence_code = pr$code, year = pr$year, n = pr$value, file = "regional tables", stringsAsFactors = FALSE))
  }
  crime <- yearly_to_cumulative(crime)
  if (!"CA" %in% crime$territory_id) {                              # national figures: sum of the provinces and territories
    nat <- crime[crime$level == "region", ] |> dplyr::group_by(country, offence_code, year, quarter) |> dplyr::summarise(cumulative = if (all(is.na(cumulative))) NA_real_ else sum(cumulative, na.rm = TRUE), .groups = "drop")
    nat$territory_id <- "CA"; nat$level <- "country"; nat$file <- "sum of provinces and territories"
    crime <- rbind(crime, nat[, names(crime)])
  }
  replace_country_rows(con, "stg_country_crime", "CA", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_CA", paste(unique(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  if (length(pops)) {
    pop <- do.call(rbind, pops); pop <- pop[!duplicated(pop[, c("territory_id", "year")]), ]   # provinces: national table first
    if (!"CA" %in% pop$territory_id) { natp <- pop[pop$level == "region", ] |> dplyr::group_by(country, year) |> dplyr::summarise(population = sum(population), .groups = "drop"); natp$territory_id <- "CA"; natp$level <- "country"; pop <- rbind(pop, natp[, names(pop)]) }
    CA_STATE$pop <- pop
  }
  invisible(crime)
}

ca_population <- function(con) {
  pop <- CA_STATE$pop
  if (is.null(pop)) { ca_ingest(con); pop <- CA_STATE$pop }
  if (is.null(pop)) stop("No population could be recovered from the Statistics Canada rates")
  replace_country_rows(con, "ref_country_population", "CA", pop)
  message(sprintf("  population: %d territories, %d-%d (Statistics Canada policing-area population, from incidents / rate)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

# Provinces and territories: 2021 Census cartographic boundary file (StatCan ESRI REST, GeoJSON)
ca_boundaries <- function() {
  dir <- COUNTRY_MODULES$CA$dir_raw
  f <- newest_first(list.files(dir, "_statcan_2021_provinces\\.geojson$", full.names = TRUE))
  if (length(f) == 0 || difftime(Sys.time(), file.mtime(f[1]), units = "days") > 365) f <- download_versioned(CA_PROVINCES_URL, dir, name = "statcan_2021_provinces.geojson")
  g <- sf::st_read(f[1], quiet = TRUE)
  if (!"PRUID" %in% names(g) || nrow(g) < 13) stop("Unexpected provincial boundary file: ", paste(names(g), collapse = " | "))
  g$province_code <- as.character(g$PRUID)
  g[, c("province_code", attr(g, "sf_column"))]
}

# Territories: provinces/territories from reference/ca_provinces.csv (+ boundaries); police services are
# added by ca_ingest from the data (no open boundaries exist for policing areas: tables only)
ca_territory <- function(con) {
  provs <- read_reference("ca_provinces.csv")
  tbl <- data.frame(territory_id = paste0("CA-", provs$province_code), level = "region", parent_id = "CA", official_code = provs$province_code, name = provs$name_en)
  g <- tryCatch(ca_boundaries(), error = function(e) {
    message("  StatCan boundary service not available (", conditionMessage(e), "): trying Natural Earth")
    tryCatch({ ne <- natural_earth_admin1("CA"); ne$province_code <- provs$province_code[match(ne$iso_3166_2, provs$iso_3166_2)]; ne[!is.na(ne$province_code), c("province_code", attr(ne, "sf_column"))] },
             error = function(e2) { message("  Natural Earth not available: provinces without boundaries (", conditionMessage(e2), ")"); NULL })
  })
  save_country_territory(con, "CA", tbl, scheme = "SGC")
  if (!is.null(g) && nrow(g)) {
    g$territory_id <- paste0("CA-", g$province_code); g$name <- provs$name_en[match(g$province_code, provs$province_code)]
    g <- g[!is.na(g$name), c("territory_id", "name", attr(g, "sf_column"))]
    save_country_gpkg(con, "CA", list(region = g), tolerance_m = 500)
  }
  invisible(tbl)
}

register_country(list(
  code = "CA", iso3 = "CAN", name_en = "Canada", name_local = "Canada", lang = "en",
  levels = c("region", "municipality"),
  level_labels = list(region = c("Provinces and territories", "Provinces et territoires", "Province or territory", "Province ou territoire"),
                      municipality = c("Police services", "Services de police", "Police service", "Service de police")),
  source = list(name = "Incident-based crime statistics, by detailed violations (Uniform Crime Reporting Survey), tables 35-10-0177-01 to 35-10-0185-01",
                publisher = "Statistics Canada, Canadian Centre for Justice and Community Safety Statistics",
                url = "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3510017701",
                licence = "Statistics Canada Open Licence",
                measure = "Actual incidents: criminal incidents substantiated by the police, one per incident (most serious violation), yearly. Total shown = Total, all Criminal Code violations (excluding traffic); drug and other federal statute violations are listed separately."),
  population_source = "Statistics Canada policing-area population, recovered from the published rate per 100,000 (actual incidents / rate x 100,000)",
  boundaries_attribution = "Source: Statistics Canada, 2021 Census cartographic boundary file (provinces and territories), Statistics Canada Open Licence",
  not_published = c("Not published: suppressed by Statistics Canada (status x or F)", "Non publié : supprimé par Statistique Canada (statut x ou F)"),
  notes_en = "Units are police services (municipal forces, RCMP/OPP/SQ detachments), not municipalities: a city may have several units and a detachment may cover several towns. Figures are revised one year back each July; unfounded incidents and new violation codes changed the series in 2009, 2014 and 2017. Neighbourhood-level data exist for Toronto (158 neighbourhoods, OGL Ontario) and Montreal (PDQ), outside this module.",
  default_region = "CA-35",
  download = ca_download, ingest = ca_ingest, territory = ca_territory, population = ca_population
))
