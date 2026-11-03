# United Kingdom: three statistical systems, three "countries" in one file ----------------
# The UK has no single police recorded crime series: England & Wales (Home Office), Scotland
# (Scottish Government / Police Scotland) and Northern Ireland (PSNI) count under different
# rules and publish different tables, so the app treats them as three countries that are
# never added or compared:
#
#   GBE  England and Wales   Home Office "Police recorded crime and outcomes open data tables",
#                            Community Safety Partnership (CSP, ~ local authority) tables (ODS),
#                            quarterly by FINANCIAL year; region = police force area (43).
#   GBS  Scotland            Scottish Government "Recorded Crimes and Offences" (data.gov.scot CSV),
#                            yearly by financial year, 32 council areas (no region level).
#   GBN  Northern Ireland    PSNI "Police Recorded Crime Monthly Data" (OpenDataNI CSV), monthly
#                            since April 2001, 11 policing districts (= local government districts).
#
# Quarters of GBE and GBS are FINANCIAL quarters (Q1 = April-June ... Q4 = January-March) and
# `year` is the calendar year in which the financial year ends: "2024 Q4" = the whole financial
# year 2023/24. GBN is monthly, so its quarters are calendar quarters (monthly_to_cumulative).
#
# All three sources are Crown copyright under the Open Government Licence v3.0; no key.
# Population: ONS mid-year estimates for the UK (one file with every local authority of the
# four nations; the Scottish and Northern Irish figures are those of NRS and NISRA).
# Boundaries: ONS Open Geography Portal (ArcGIS FeatureServer, OGL v3): police force areas,
# CSPs and the UK local authority districts (council areas, LGDs).
#
# The ODS tables need the readODS package (not in PACKAGES): the reader works behind
# requireNamespace() and also accepts CSV exports of the sheets dropped into the raw folder.
# Written against the published documentation (not run against the live files): the readers
# detect the columns by name; `gbe_inspect()`, `gbs_inspect()` and `gbn_inspect()` print
# the headers and the category labels found so that the reference tables can be adjusted.

GB_STATE <- new.env()   # ONS population table and the E&W lookup, shared between steps

# --- Sources ------------------------------------------------------------------------------------

GBE_SOURCES <- list(
  page      = "https://www.gov.uk/government/statistical-data-sets/police-recorded-crime-and-outcomes-open-data-tables",
  api       = "https://www.gov.uk/api/content/government/statistical-data-sets/police-recorded-crime-and-outcomes-open-data-tables",
  # CSP open data tables (ODS, one sheet per financial year); the first one is re-issued every quarter
  csp_2021_on   = "https://assets.publishing.service.gov.uk/media/6ad230f3a89985bb970267f2/prc-csp-mar21-jun26-tables-221026.ods",   # 50 MB
  csp_2016_2020 = "https://assets.publishing.service.gov.uk/media/6ad230c2dfa45fcb6753cb28/prc-csp-mar16-mar20-tables-221026.ods",   # 36 MB
  # CSP Name -> Force Name, Region, ONS Code (CSV, 22 KB)
  geo_ref   = "https://assets.publishing.service.gov.uk/media/680797798c1316be7978e6cb/recrime-geo-pfa.csv",
  # ONS lookup local authority district -> CSP -> police force area (April 2025), 332 rows
  lookup    = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD25_CSP25_PFA25_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&f=json",
  # Boundaries (GeoJSON, WGS84): police force areas Dec 2024 (ultra generalised), CSPs Dec 2023 (generalised)
  pfa_geo   = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Police_Force_Areas_Dec_2024_EW_BUC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson",
  csp_geo   = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Community_Safety_Partnerships_December_2023_Boundaries_EW_BGC/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson"
)
GBS_SOURCES <- list(
  page = "https://data.gov.scot/dataset/recorded_crimes_and_offences",
  csv  = "https://data.gov.scot/dataset/recorded_crimes_and_offences/resource/3716a560-80b0-4860-b11f-6a53d24deb21/download"   # 11 MB
)
GBN_SOURCES <- list(
  page        = "https://admin.opendatani.gov.uk/dataset/police-recorded-crime-in-northern-ireland",
  api         = "https://admin.opendatani.gov.uk/api/3/action/package_show?id=police-recorded-crime-in-northern-ireland",
  resource_id = "6fd51851-df78-4469-98c5-4f06953621a0",   # "Police Recorded Crime Monthly Data" (CSV)
  # CKAN serves uploads at <dataset>/resource/<id>/download[/<file name>]; used when the API is not reachable
  csv_guess   = "https://admin.opendatani.gov.uk/dataset/80dc9542-7b2a-48f5-bbf4-ccc7040d36af/resource/6fd51851-df78-4469-98c5-4f06953621a0/download"
)
# UK local authority districts May 2025 (generalised): council areas (S12...) and LGDs (N09...)
GB_LAD_GEO <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_MAY_2025_UK_BGC_V2/FeatureServer/0/query?where=LAD25CD%20LIKE%20%27S%25%27%20OR%20LAD25CD%20LIKE%20%27N%25%27&outFields=*&outSR=4326&f=geojson"
# ONS "Population estimates for UK, England and Wales, Scotland and Northern Ireland" (xlsx, 1 MB)
GB_ONS_MYE <- list(
  page = "https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland",
  file = "https://www.ons.gov.uk/file?uri=%2Fpeoplepopulationandcommunity%2Fpopulationandmigration%2Fpopulationestimates%2Fdatasets%2Fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2Fmid2025%2Fmye25tablesuk.xlsx"
)

# Accepted column names (first match wins, case-insensitive), as FR_COLUMNS
# Header of the Home Office CSP tables (2026 edition): Financial Year | Financial Quarter | Police Force |
# CSP Name | Offence Description | Offence Group | Offence Subgroup | Offence Code | Offence Count
GBE_COLUMNS <- list(fy = c("^Financial[ ._]?Year"), fq = c("^Financial[ ._]?Quarter"), force = c("^Police[ ._]?Force", "^Force[ ._]?Name", "^Force$"), csp = c("^CSP[ ._]?Name", "^CSP$"),
                    group = c("^Offence[ ._]?Group"), subgroup = c("^Offence[ ._]?Sub[ ._]?group"), code = c("^Offence[ ._]?Code"),
                    n = c("^Offence[ ._]?Count", "^Number[ ._]?of[ ._]?Offences", "^Offences$", "^Count$"))
GBS_COLUMNS <- list(code = c("^Geography[ ._]?Code"), name = c("^Geography[ ._]?Name"), type = c("^Geography[ ._]?Type"), date = c("^Date[ ._]?Code"), measurement = c("^Measurement"),
                    units = c("^Units"), value = c("^Value"), crime = c("^Crime[ ._]?Or[ ._]?Offence"))
GBN_COLUMNS <- list(year = c("^Calendar[ ._]?Year", "^Year$"), month = c("^Month"), district = c("^Policing[ ._]?District"), type = c("^Crime[ ._]?Type"),
                    measure = c("^Data[ ._]?Measure"), n = c("^Count", "^Value"))

# --- Shared helpers --------------------------------------------------------------------------------

gb_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }
gb_cols <- function(d, spec, needed, label) {
  col <- lapply(spec, gb_pick, nm = names(d))
  if (any(is.na(unlist(col[needed])))) stop("Unexpected columns in ", label, ": ", paste(names(d), collapse = " | "), "\nRun the *_inspect() function and adjust the *_COLUMNS list in R/74_gb.R")
  col
}
gb_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", trimws(as.character(x)))))   # "1,234" -> 1234; "/0", "", ".." -> NA
# Names comparable across sources: "London, City of" = "City of London", "&" = "and", no punctuation
gb_norm <- function(x) {
  x <- tolower(trimws(as.character(x))); x <- sub("^(.*), city of$", "city of \\1", x); x <- gsub("&", " and ", x)
  gsub("[^a-z0-9]", "", x)
}
gb_slug <- function(x) toupper(gsub("[^A-Za-z0-9]+", "_", trimws(x)))
# "2021/22", "2021-22", "2021/2022" -> 2022 (the calendar year in which the financial year ends); "2021" -> 2021
gb_fy_end <- function(x) {
  x <- trimws(as.character(x)); m <- regexpr("(19|20)[0-9]{2}", x)
  start <- ifelse(m > 0, suppressWarnings(as.integer(substr(x, m, m + 3))), NA_integer_)
  ifelse(!is.na(start) & grepl("(19|20)[0-9]{2}\\s*[/-]\\s*[0-9]{2,4}", x), start + 1L, start)
}
gb_file <- function(cc, pattern) {
  f <- newest_first(list.files(COUNTRY_MODULES[[cc]]$dir_raw, pattern, full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}
gb_recent <- function(f, days) !is.na(f) && difftime(Sys.time(), file.mtime(f), units = "days") < days
gb_fetch <- function(cc, url, name, days = 60) {   # download unless a copy younger than `days` exists
  prev <- gb_file(cc, paste0("_", gsub("([.()+])", "\\\\\\1", name), "$"))
  if (gb_recent(prev, days)) { message("= ", name, " downloaded less than ", days, " days ago"); return(invisible(prev)) }
  download_versioned(url, COUNTRY_MODULES[[cc]]$dir_raw, name = name)
}
gb_read_csv <- function(f) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE, na = character())
gb_read_geojson <- function(f) { g <- sf::st_read(f, quiet = TRUE); if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326; g }

# ONS mid-year estimates (every UK local authority, one xlsx): data.frame(code, name, year, population).
# The current edition is found on the dataset page (mye<YY>tablesuk.xlsx); the known URL is the fallback.
# Sheet "MYE4" (time series mid-2011 onwards) is preferred; "MYE2 - Persons" (one year, "All ages") otherwise.
gb_ons_mye_url <- function() {
  html <- get_text(GB_ONS_MYE$page)
  yy <- if (!is.null(html)) regmatches(html, gregexpr("mye([0-9]{2})tablesuk\\.xlsx", html))[[1]] else character()
  yy <- unique(as.integer(sub("tablesuk\\.xlsx$", "", sub("^mye", "", yy))))
  if (length(yy) == 0) return(GB_ONS_MYE$file)
  yy <- max(yy)
  sprintf("https://www.ons.gov.uk/file?uri=%%2Fpeoplepopulationandcommunity%%2Fpopulationandmigration%%2Fpopulationestimates%%2Fdatasets%%2Fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%%2Fmid20%02d%%2Fmye%02dtablesuk.xlsx", yy, yy)
}
gb_ons_mye_download <- function(cc) {
  prev <- gb_file(cc, "_ons_mye_uk\\.xlsx$")
  if (gb_recent(prev, 120)) return(invisible(prev))
  url <- tryCatch(gb_ons_mye_url(), error = function(e) GB_ONS_MYE$file)
  download_versioned(url, COUNTRY_MODULES[[cc]]$dir_raw, name = "ons_mye_uk.xlsx")
}
gb_ons_mye <- function(cc) {
  if (!is.null(GB_STATE$mye)) return(GB_STATE$mye)
  f <- gb_file(cc, "_ons_mye_uk\\.xlsx$")
  # Any of the three modules may have downloaded it: look in the other raw folders too
  if (is.na(f)) for (o in c("GBE", "GBS", "GBN")) if (!is.null(COUNTRY_MODULES[[o]])) { f <- gb_file(o, "_ons_mye_uk\\.xlsx$"); if (!is.na(f)) break }
  if (is.na(f)) stop("ONS mid-year estimates not downloaded (ons_mye_uk.xlsx)")
  sheets <- readxl::excel_sheets(f)
  sheet <- c(grep("^MYE4", sheets, value = TRUE), grep("^MYE2.*Person", sheets, value = TRUE, ignore.case = TRUE))
  if (length(sheet) == 0) stop("No MYE4 / MYE2 sheet in ", basename(f), ": ", paste(sheets, collapse = " | "))
  raw <- readxl::read_excel(f, sheet = sheet[1], col_names = FALSE, col_types = "text", .name_repair = "minimal")
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  hdr <- which(tolower(trimws(raw[[1]])) == "code")[1]
  if (is.na(hdr)) stop("No header row starting with 'Code' in sheet ", sheet[1], " of ", basename(f))
  names(raw) <- trimws(as.character(unlist(raw[hdr, ]))); d <- raw[-seq_len(hdr), , drop = FALSE]
  d <- d[!is.na(d$Code) & grepl("^[ENSWK][0-9]{8}$", d$Code), ]
  name_col <- gb_pick(names(d), c("^Name", "^Area")); if (is.na(name_col)) name_col <- names(d)[2]
  ycols <- names(d)[grepl("(19|20)[0-9]{2}", names(d))]
  out <- if (length(ycols) > 0) {
    do.call(rbind, lapply(ycols, function(cn) data.frame(code = d$Code, name = d[[name_col]], year = as.integer(regmatches(cn, regexpr("(19|20)[0-9]{2}", cn))), population = gb_num(d[[cn]]), stringsAsFactors = FALSE)))
  } else {
    all_col <- gb_pick(names(d), c("^All ages", "^All\\s", "^Total"))
    if (is.na(all_col)) stop("No year columns and no 'All ages' column in sheet ", sheet[1], " of ", basename(f))
    yy <- as.integer(sub("^mye", "", regmatches(basename(f), regexpr("mye[0-9]{2}", basename(f)))))   # mye25 -> 2025
    if (length(yy) == 0) stop("Cannot tell the year of ", basename(f), " (expected mye<YY>tablesuk.xlsx)")
    data.frame(code = d$Code, name = d[[name_col]], year = 2000L + yy, population = gb_num(d[[all_col]]), stringsAsFactors = FALSE)
  }
  out <- out[!is.na(out$population), ]
  message(sprintf("  ONS mid-year estimates: %d areas, %d-%d (%s, sheet %s)", length(unique(out$code)), min(out$year), max(out$year), basename(f), sheet[1]))
  GB_STATE$mye <- out
  out
}
# Population rows of one country from the ONS table: `codes` = named vector territory_id -> ONS code
# (several codes for one territory are added up: police force areas = their local authorities)
gb_population_rows <- function(cc, mye, ids, codes, level) {
  m <- data.frame(territory_id = ids, code = codes, stringsAsFactors = FALSE) |> dplyr::inner_join(mye, by = "code", relationship = "many-to-many")
  m <- m |> dplyr::group_by(territory_id, year) |> dplyr::summarise(population = sum(population), parts = dplyr::n(), .groups = "drop")
  want <- table(ids)
  m <- m[m$parts == as.integer(want[m$territory_id]), ]   # all the parts of the territory present in that year
  if (nrow(m) == 0) return(NULL)
  data.frame(country = cc, territory_id = m$territory_id, level = level, year = m$year, population = m$population, stringsAsFactors = FALSE)
}

# ===================================================================================================
# GBE · England and Wales --------------------------------------------------------------------------
# ===================================================================================================

# The gov.uk page is re-issued every quarter with a new media id: ask the content API for the current
# CSP attachment (title "Police recorded crime Community Safety Partnership open data, year ending ...")
gbe_current_csp_url <- function() {
  js <- get_text(GBE_SOURCES$api); if (is.null(js)) return(GBE_SOURCES$csp_2021_on)
  att <- tryCatch(jsonlite::fromJSON(js, simplifyVector = TRUE)$details$attachments, error = function(e) NULL)
  if (is.null(att) || !all(c("title", "url") %in% names(att))) return(GBE_SOURCES$csp_2021_on)
  i <- grepl("Community Safety Partnership", att$title, ignore.case = TRUE) & grepl("\\.ods$", att$url) & grepl("20[2-9][0-9]", att$title)
  if (!any(i)) return(GBE_SOURCES$csp_2021_on)
  ends <- vapply(regmatches(att$title[i], gregexpr("20[0-9]{2}", att$title[i])), function(y) max(as.integer(y)), 0)
  att$url[i][which.max(ends)]
}

gbe_download <- function(days = 60) {
  gb_fetch("GBE", gbe_current_csp_url(), "gbe_csp_2021_on.ods", days)
  gb_fetch("GBE", GBE_SOURCES$csp_2016_2020, "gbe_csp_2016_2020.ods", 3650)    # closed period: once
  gb_fetch("GBE", GBE_SOURCES$geo_ref, "gbe_geo_ref.csv", days)
  gb_fetch("GBE", GBE_SOURCES$lookup, "gbe_lookup_lad_csp_pfa.json", 365)
  gb_fetch("GBE", GBE_SOURCES$pfa_geo, "gbe_pfa.geojson", 3650)
  gb_fetch("GBE", GBE_SOURCES$csp_geo, "gbe_csp.geojson", 3650)
  gb_ons_mye_download("GBE")
}

# Files that hold CSP tables: the ODS files and any CSV export of their sheets (gbe_csp*.csv)
# Ordered newest period first by name (gbe_csp_2021_on before gbe_csp_2016_2020), not by download time
gbe_table_files <- function() { f <- list.files(COUNTRY_MODULES$GBE$dir_raw, "gbe_csp.*\\.(ods|csv)$", full.names = TRUE); f[order(sub("^[0-9]{8}_", "", basename(f)), decreasing = TRUE)] }

# Read one CSP table (every sheet that has the expected header) -> character data frame
gbe_read_table <- function(f) {
  if (grepl("\\.csv$", f)) return(gb_read_csv(f))
  if (!requireNamespace("readODS", quietly = TRUE))
    stop("Reading ", basename(f), " needs the readODS package: install.packages(\"readODS\"), or export its sheets to CSV (gbe_csp_<year>.csv) into ", COUNTRY_MODULES$GBE$dir_raw)
  parts <- list()
  for (s in readODS::list_ods_sheets(f)) {
    d <- tryCatch(readODS::read_ods(f, sheet = s, col_types = NA, col_names = FALSE, as_tibble = FALSE), error = function(e) { message("    sheet ", s, ": could not be read (", conditionMessage(e), ")"); NULL })
    if (is.null(d) || nrow(d) < 2) next
    hdr <- which(apply(d[seq_len(min(10, nrow(d))), , drop = FALSE], 1, function(r) any(grepl("^Offence Group", r, ignore.case = TRUE)) && any(grepl("^Financial Year", r, ignore.case = TRUE))))[1]
    if (is.na(hdr)) { message("    sheet ", s, ": no CSP table, skipped"); next }
    names(d) <- trimws(as.character(unlist(d[hdr, ]))); d <- d[-seq_len(hdr), , drop = FALSE]
    d[] <- lapply(d, as.character)
    parts[[s]] <- d[, !is.na(names(d)) & nzchar(names(d)), drop = FALSE]
    message(sprintf("    sheet %s: %s rows", s, fmt_int(nrow(d))))
  }
  if (length(parts) == 0) stop("No sheet of ", basename(f), " has the CSP columns (Financial Year, Offence Group...)")
  dplyr::bind_rows(parts)
}

gbe_inspect <- function() {
  for (f in gbe_table_files()) {
    d <- gbe_read_table(f); col <- lapply(GBE_COLUMNS, gb_pick, nm = names(d))
    message(basename(f), "\n  ", paste(names(d), collapse = " | "))
    for (k in c("fy", "fq", "group", "subgroup")) if (!is.na(col[[k]])) message("  ", k, ": ", paste(sort(unique(d[[col[[k]]]])), collapse = " / "))
    if (!is.na(col$force)) message("  forces: ", length(unique(d[[col$force]])), "; CSPs: ", if (!is.na(col$csp)) length(unique(d[[col$csp]])) else NA)
    if (!is.na(col$group) && !is.na(col$subgroup)) { types <- read_reference("gbe_offence_types.csv"); u <- setdiff(unique(c(d[[col$group]], d[[col$subgroup]])), types$source_code); if (length(u)) message("  not in gbe_offence_types.csv: ", paste(u, collapse = " / ")) }
  }
  for (p in c("_gbe_geo_ref\\.csv$", "_gbe_lookup_lad_csp_pfa\\.json$", "_gbe_pfa\\.geojson$", "_gbe_csp\\.geojson$", "_ons_mye_uk\\.xlsx$")) message(p, ": ", basename(gb_file("GBE", p)))
}

# Lookup local authority -> CSP -> PFA (ONS, April 2025): data.frame(lad_code, lad_name, csp_code, csp_name, pfa_code, pfa_name)
gbe_lookup <- function() {
  if (!is.null(GB_STATE$lookup)) return(GB_STATE$lookup)
  f <- gb_file("GBE", "_gbe_lookup_lad_csp_pfa\\.json$"); if (is.na(f)) return(NULL)
  js <- jsonlite::fromJSON(f, simplifyVector = TRUE); a <- js$features$attributes
  if (is.null(a)) stop("No features/attributes in ", basename(f))
  pick <- function(p) { cn <- grep(p, names(a), value = TRUE, ignore.case = TRUE); if (length(cn) == 0) stop("Column ", p, " missing in the lookup: ", paste(names(a), collapse = " | ")); as.character(unlist(a[[cn[1]]])) }
  GB_STATE$lookup <- data.frame(lad_code = pick("^LAD[0-9]*CD$"), lad_name = pick("^LAD[0-9]*NM$"), csp_code = pick("^CSP[0-9]*CD$"), csp_name = pick("^CSP[0-9]*NM$"),
                                pfa_code = pick("^PFA[0-9]*CD$"), pfa_name = pick("^PFA[0-9]*NM$"), stringsAsFactors = FALSE)
  GB_STATE$lookup
}
# Home Office geographical reference: CSP Name, Force Name, Region, ONS Code
gbe_geo_ref <- function() {
  f <- gb_file("GBE", "_gbe_geo_ref\\.csv$"); if (is.na(f)) return(NULL)
  d <- gb_read_csv(f); col <- list(csp = gb_pick(names(d), "^CSP"), force = gb_pick(names(d), "^Force"), code = gb_pick(names(d), "ONS|Code"))
  if (any(is.na(unlist(col)))) stop("Unexpected columns in ", basename(f), ": ", paste(names(d), collapse = " | "))
  out <- data.frame(csp_name = trimws(d[[col$csp]]), force_name = trimws(d[[col$force]]), csp_code = trimws(d[[col$code]]), stringsAsFactors = FALSE)
  out$csp_code[!grepl("^[EW][0-9]{8}$", out$csp_code)] <- NA
  out
}
gbe_forces <- function() read_reference("gbe_police_forces.csv")

# Codes of the police force areas (from reference/gbe_police_forces.csv) and of the CSPs (ONS lookup,
# then the Home Office reference) for the names used in the tables; unknown names get a slug code.
gbe_pfa_code <- function(force_name) {
  forces <- gbe_forces(); key <- c(gb_norm(forces$force_name), gb_norm(forces$pfa_name)); val <- c(forces$pfa_code, forces$pfa_code)
  code <- val[match(gb_norm(force_name), key)]
  ifelse(is.na(code), gb_slug(force_name), code)
}
gbe_csp_code <- function(csp_name) {
  key <- character(); val <- character()
  lk <- tryCatch(gbe_lookup(), error = function(e) NULL); if (!is.null(lk)) { key <- c(key, gb_norm(lk$csp_name)); val <- c(val, lk$csp_code) }
  gr <- tryCatch(gbe_geo_ref(), error = function(e) NULL); if (!is.null(gr)) { ok <- !is.na(gr$csp_code); key <- c(key, gb_norm(gr$csp_name[ok])); val <- c(val, gr$csp_code[ok]) }
  code <- val[match(gb_norm(csp_name), key)]
  ifelse(is.na(code), gb_slug(csp_name), code)
}

# Standardise one table: fy_end, quarter, force, csp, group, subgroup, n
gbe_standardise <- function(d, f) {
  col <- gb_cols(d, GBE_COLUMNS, c("fy", "fq", "force", "csp", "group", "n"), paste("the England & Wales CSP table", basename(f)))
  out <- data.frame(year = gb_fy_end(d[[col$fy]]), quarter = as.integer(gsub("[^0-9]", "", d[[col$fq]])), force = trimws(d[[col$force]]), csp = trimws(d[[col$csp]]),
                    group = trimws(d[[col$group]]), subgroup = if (!is.na(col$subgroup)) trimws(d[[col$subgroup]]) else NA_character_, n = gb_num(d[[col$n]]), stringsAsFactors = FALSE)
  out <- out[!is.na(out$year) & !is.na(out$quarter) & out$quarter %in% 1:4 & !is.na(out$n) & nzchar(out$csp), ]
  # Not comparable with the CSP level: British Transport Police and the centrally recorded fraud (Action Fraud, Cifas, UK Finance)
  out <- out[!grepl("british transport|action fraud|cifas|uk finance|financial fraud", out$force, ignore.case = TRUE), ]
  out[!grepl("^fraud", out$group, ignore.case = TRUE), ]
}

gbe_ingest <- function(con) {
  files <- gbe_table_files()
  if (length(files) == 0) stop("No CSP table in ", COUNTRY_MODULES$GBE$dir_raw, " (gbe_csp_*.ods or a CSV export)")
  parts <- list()
  for (f in files) { message("  reading ", basename(f), " (ODS files take several minutes)"); parts[[f]] <- gbe_standardise(gbe_read_table(f), f); parts[[f]]$file <- basename(f) }
  d <- dplyr::bind_rows(parts)
  # The latest file supersedes the older ones where they overlap (a financial year is taken from one file only)
  first_file <- d |> dplyr::group_by(year) |> dplyr::summarise(file = file[1], .groups = "drop")   # files are newest first
  d <- dplyr::inner_join(d, first_file, by = c("year", "file"))
  d$pfa <- gbe_pfa_code(d$force); d$csp_code <- gbe_csp_code(d$csp)
  d$territory_id <- paste("GBE", d$pfa, d$csp_code, sep = "-"); d$region_id <- paste0("GBE-", d$pfa)
  # Offence groups, their subgroups (only when they differ from the group) and a synthetic total
  by_group <- d |> dplyr::group_by(territory_id, region_id, year, quarter, file, offence_code = group) |> dplyr::summarise(n = sum(n), .groups = "drop")
  sub <- d[!is.na(d$subgroup) & nzchar(d$subgroup) & gb_norm(d$subgroup) != gb_norm(d$group), ]
  by_sub <- sub |> dplyr::group_by(territory_id, region_id, year, quarter, file, offence_code = subgroup) |> dplyr::summarise(n = sum(n), .groups = "drop")
  total <- d |> dplyr::group_by(territory_id, region_id, year, quarter, file) |> dplyr::summarise(n = sum(n), .groups = "drop"); total$offence_code <- "TOTAL_RECORDED"
  muni <- dplyr::bind_rows(by_group, by_sub, total[, names(by_group)]); muni$level <- "municipality"
  reg <- muni |> dplyr::group_by(territory_id = region_id, year, quarter, file, offence_code) |> dplyr::summarise(n = sum(n), .groups = "drop"); reg$level <- "region"; reg$region_id <- reg$territory_id
  nat <- reg |> dplyr::group_by(year, quarter, file, offence_code) |> dplyr::summarise(n = sum(n), .groups = "drop"); nat$territory_id <- "GBE"; nat$level <- "country"; nat$region_id <- NA
  all <- dplyr::bind_rows(muni, reg, nat)
  # Financial quarters are additive (negative corrections before 2019 included): cumulative within the financial year
  all <- all |> dplyr::group_by(territory_id, offence_code, year) |> dplyr::arrange(quarter, .by_group = TRUE) |> dplyr::mutate(cumulative = cumsum(n)) |> dplyr::ungroup()
  all$country <- "GBE"
  # Territories named in the tables but missing from dim_territory (name changes, merged CSPs)
  forces <- gbe_forces(); rf <- unique(d[, c("region_id", "force")]); rf <- rf[!duplicated(rf$region_id), ]
  fn <- forces$force_name[match(sub("^GBE-", "", rf$region_id), forces$pfa_code)]
  ensure_territories(con, "GBE", rf$region_id, "region", "GBE", ifelse(is.na(fn), rf$force, fn))
  u <- !duplicated(d$territory_id); ensure_territories(con, "GBE", d$territory_id[u], "municipality", d$region_id[u], d$csp[u])
  out <- as.data.frame(all[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  replace_country_rows(con, "stg_country_crime", "GBE", out)
  log_ingest(con, "country_GBE", paste(basename(files), collapse = ";"), NA, paste(range(out$year), collapse = "-"), nrow(out))
  types <- read_reference("gbe_offence_types.csv"); unknown <- setdiff(unique(out$offence_code), types$source_code)
  message(sprintf("  England & Wales: %s rows, %d CSPs, %d force areas, financial years ending %d-%d%s", fmt_int(nrow(out)), length(unique(muni$territory_id)), length(unique(reg$territory_id)), min(out$year), max(out$year),
                  if (length(unknown)) paste0("\n  categories not in gbe_offence_types.csv (ignored until mapped): ", paste(unknown, collapse = " / ")) else ""))
  invisible(out)
}

# Territories: 43 police force areas (reference table) and the CSPs of the ONS lookup; boundaries from the ONS portal
gbe_territory <- function(con) {
  forces <- gbe_forces()
  tbl <- data.frame(territory_id = paste0("GBE-", forces$pfa_code), level = "region", parent_id = "GBE", official_code = forces$pfa_code, name = forces$force_name, stringsAsFactors = FALSE)
  lk <- tryCatch(gbe_lookup(), error = function(e) { message("  ONS lookup not available: ", conditionMessage(e)); NULL })
  if (is.null(lk)) { gr <- tryCatch(gbe_geo_ref(), error = function(e) NULL); if (!is.null(gr)) lk <- data.frame(csp_code = gr$csp_code, csp_name = gr$csp_name, pfa_code = gbe_pfa_code(gr$force_name))[!is.na(gr$csp_code), ] }
  if (!is.null(lk)) {
    csp <- unique(lk[, c("csp_code", "csp_name", "pfa_code")]); csp <- csp[!duplicated(csp$csp_code), ]
    tbl <- rbind(tbl, data.frame(territory_id = paste("GBE", csp$pfa_code, csp$csp_code, sep = "-"), level = "municipality", parent_id = paste0("GBE-", csp$pfa_code), official_code = csp$csp_code, name = csp$csp_name, stringsAsFactors = FALSE))
  }
  save_country_territory(con, "GBE", tbl, scheme = "ONS")
  layers <- list()
  fp <- gb_file("GBE", "_gbe_pfa\\.geojson$")
  if (!is.na(fp)) { g <- gb_read_geojson(fp); cd <- grep("^PFA[0-9]*CD$", names(g), value = TRUE)[1]; nm <- grep("^PFA[0-9]*NM$", names(g), value = TRUE)[1]
    if (!is.na(cd)) { g$territory_id <- paste0("GBE-", g[[cd]]); g$name <- g[[nm]]; layers$region <- g[, c("territory_id", "name", attr(g, "sf_column"))] } }
  fc <- gb_file("GBE", "_gbe_csp\\.geojson$")
  if (!is.na(fc) && !is.null(lk)) { g <- gb_read_geojson(fc); cd <- grep("^CSP[0-9]*CD$", names(g), value = TRUE)[1]; nm <- grep("^CSP[0-9]*NM$", names(g), value = TRUE)[1]
    if (!is.na(cd)) { pfa <- lk$pfa_code[match(g[[cd]], lk$csp_code)]; ok <- !is.na(pfa)
      g <- g[ok, ]; g$territory_id <- paste("GBE", pfa[ok], g[[cd]], sep = "-"); g$name <- g[[nm]]; layers$municipality <- g[, c("territory_id", "name", attr(g, "sf_column"))] } }
  if (length(layers)) save_country_gpkg(con, "GBE", layers, tolerance_m = 100) else message("  no E&W boundaries downloaded: tables only")
  invisible(tbl)
}

# Population: ONS local authorities added up into police force areas (lookup); a CSP gets a figure
# only when it is made of whole local authorities (a CSP that is a part of a district has none)
gbe_population <- function(con) {
  mye <- gb_ons_mye("GBE"); lk <- gbe_lookup()
  if (is.null(lk)) stop("ONS lookup LAD -> CSP -> PFA not downloaded")
  lad_pfa <- unique(lk[, c("lad_code", "pfa_code")])
  reg <- gb_population_rows("GBE", mye, paste0("GBE-", lad_pfa$pfa_code), lad_pfa$lad_code, "region")
  n_csp_per_lad <- table(unique(lk[, c("lad_code", "csp_code")])$lad_code)
  whole <- lk[n_csp_per_lad[lk$lad_code] == 1, ]                    # local authorities that lie entirely in one CSP
  csp <- whole[!whole$csp_code %in% unique(lk$csp_code[n_csp_per_lad[lk$lad_code] > 1]), ]   # CSPs made only of whole districts
  muni <- gb_population_rows("GBE", mye, paste("GBE", csp$pfa_code, csp$csp_code, sep = "-"), csp$lad_code, "municipality")
  ew <- mye[mye$code %in% c("K04000001"), ]
  if (nrow(ew) == 0) ew <- mye[mye$code %in% c("E92000001", "W92000004"), ] |> dplyr::group_by(year) |> dplyr::summarise(population = sum(population), .groups = "drop")
  nat <- if (nrow(ew)) data.frame(country = "GBE", territory_id = "GBE", level = "country", year = ew$year, population = ew$population) else NULL
  pop <- dplyr::bind_rows(reg, muni, nat)
  replace_country_rows(con, "ref_country_population", "GBE", as.data.frame(pop))
  message(sprintf("  population: %d force areas, %d CSPs (%d CSPs are parts of a district: no rates), %d-%d", length(unique(reg$territory_id)), length(unique(muni$territory_id)), length(unique(lk$csp_code)) - length(unique(muni$territory_id)), min(pop$year), max(pop$year)))
}

# ===================================================================================================
# GBS · Scotland -----------------------------------------------------------------------------------
# ===================================================================================================

gbs_download <- function(days = 60) {
  gb_fetch("GBS", GBS_SOURCES$csv, "gbs_recorded_crime.csv", days)
  gb_fetch("GBS", GB_LAD_GEO, "gb_lad_scotland_ni.geojson", 3650)
  gb_ons_mye_download("GBS")
}

gbs_inspect <- function() {
  f <- gb_file("GBS", "_gbs_recorded_crime\\.csv$"); if (is.na(f)) { message("no Scottish file yet"); return(invisible()) }
  d <- gb_read_csv(f); col <- lapply(GBS_COLUMNS, gb_pick, nm = names(d))
  message(basename(f), "\n  ", paste(names(d), collapse = " | "))
  for (k in c("type", "date", "measurement", "units")) if (!is.na(col[[k]])) message("  ", k, ": ", paste(sort(unique(d[[col[[k]]]])), collapse = " / "))
  if (!is.na(col$crime)) { types <- read_reference("gbs_offence_types.csv"); u <- setdiff(unique(trimws(d[[col$crime]])), types$source_code)
    message("  Crime Or Offence: ", length(unique(d[[col$crime]])), " labels", if (length(u)) paste0("\n  not in gbs_offence_types.csv: ", paste(u, collapse = " / ")) else "") }
  print(utils::head(as.data.frame(d), 5))
}

gbs_ingest <- function(con) {
  f <- gb_file("GBS", "_gbs_recorded_crime\\.csv$"); if (is.na(f)) stop("No Scottish file in ", COUNTRY_MODULES$GBS$dir_raw)
  d <- gb_read_csv(f); col <- gb_cols(d, GBS_COLUMNS, c("code", "date", "value", "crime"), paste("the Scottish file", basename(f)))
  out <- data.frame(code = trimws(d[[col$code]]), name = if (!is.na(col$name)) trimws(d[[col$name]]) else NA_character_, year = gb_fy_end(d[[col$date]]),
                    crime = trimws(d[[col$crime]]), n = gb_num(d[[col$value]]), stringsAsFactors = FALSE)
  # Counts only (the file also carries rates per 10,000 population)
  keep <- rep(TRUE, nrow(out))
  if (!is.na(col$measurement)) keep <- keep & grepl("count", d[[col$measurement]], ignore.case = TRUE)
  else if (!is.na(col$units)) keep <- keep & !grepl("per|ratio|rate", d[[col$units]], ignore.case = TRUE)
  out <- out[keep & !is.na(out$year) & nzchar(out$crime), ]
  out$level <- ifelse(grepl("^S92", out$code) | gb_norm(out$name) %in% "scotland", "country", "municipality")
  out$territory_id <- ifelse(out$level == "country", "GBS", paste0("GBS-", out$code))
  out <- out[out$level == "country" | grepl("^S12", out$code), ]
  if (nrow(out) == 0) stop("No council area rows (GeographyCode S12...) in ", basename(f), "; run gbs_inspect()")
  # Synthetic total: the categories the reference table places directly under "total" (All Crimes +
  # the offence groups except road traffic); the official totals of crimes and of offences stay separate
  types <- read_reference("gbs_offence_types.csv"); top <- types$source_code[!is.na(types$included_in) & types$included_in == "total" & types$offence_type_id != "total"]
  tot <- out[out$crime %in% top, ] |> dplyr::group_by(territory_id, level, year) |> dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
  tot$crime <- "TOTAL_CRIMES_OFFENCES"
  crime <- dplyr::bind_rows(out[, c("territory_id", "level", "year", "crime", "n")], tot[, c("territory_id", "level", "year", "crime", "n")])
  crime <- crime |> dplyr::group_by(territory_id, level, year, crime) |> dplyr::summarise(n = sum(n), .groups = "drop")   # one row per key
  crime <- yearly_to_cumulative(data.frame(country = "GBS", territory_id = crime$territory_id, level = crime$level, offence_code = crime$crime, year = crime$year, n = crime$n, file = basename(f)))
  if (!"GBS" %in% crime$territory_id) {   # no national rows in the file: sum of the council areas
    nat <- crime |> dplyr::group_by(country, offence_code, year, quarter, file) |> dplyr::summarise(cumulative = sum(cumulative), .groups = "drop"); nat$territory_id <- "GBS"; nat$level <- "country"
    crime <- rbind(crime, nat[, names(crime)])
  }
  u <- !duplicated(out$territory_id) & out$level == "municipality"
  ensure_territories(con, "GBS", out$territory_id[u], "municipality", "GBS", out$name[u])
  replace_country_rows(con, "stg_country_crime", "GBS", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_GBS", basename(f), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  unknown <- setdiff(unique(crime$offence_code), types$source_code)
  message(sprintf("  Scotland: %s rows, %d council areas, financial years ending %d-%d; total built from %d of %d expected categories%s", fmt_int(nrow(crime)), sum(u), min(crime$year), max(crime$year),
                  length(intersect(top, unique(out$crime))), length(unique(types$offence_type_id[types$source_code %in% top])),
                  if (length(unknown)) paste0("\n  labels not in gbs_offence_types.csv (ignored until mapped): ", paste(unknown, collapse = " / ")) else ""))
  invisible(crime)
}

# Council areas (32) from the ONS UK local authority layer (S12 codes), parent = GBS
gbs_territory <- function(con) {
  f <- gb_file("GBS", "_gb_lad_scotland_ni\\.geojson$"); if (is.na(f)) f <- gb_file("GBN", "_gb_lad_scotland_ni\\.geojson$")
  # Without the layer only the country row is written here; the council areas come from the data (ensure_territories)
  tbl <- data.frame(territory_id = "GBS", level = "country", parent_id = NA_character_, official_code = "S92000003", name = "Scotland", stringsAsFactors = FALSE)
  layers <- list()
  if (!is.na(f)) {
    g <- gb_read_geojson(f); cd <- grep("^LAD[0-9]*CD$", names(g), value = TRUE)[1]; nm <- grep("^LAD[0-9]*NM$", names(g), value = TRUE)[1]
    g <- g[grepl("^S12", g[[cd]]), ]
    tbl <- data.frame(territory_id = paste0("GBS-", g[[cd]]), level = "municipality", parent_id = "GBS", official_code = g[[cd]], name = g[[nm]], stringsAsFactors = FALSE)
    g$territory_id <- tbl$territory_id; g$name <- tbl$name; layers$municipality <- g[, c("territory_id", "name", attr(g, "sf_column"))]
    if (nrow(tbl) == 0) tbl <- data.frame(territory_id = "GBS", level = "country", parent_id = NA_character_, official_code = "S92000003", name = "Scotland", stringsAsFactors = FALSE)
  } else message("  ONS local authority layer not downloaded: council areas are created from the data, without boundaries")
  save_country_territory(con, "GBS", tbl, scheme = "ONS")
  if (length(layers)) save_country_gpkg(con, "GBS", layers, tolerance_m = 100)
  invisible(tbl)
}

gbs_population <- function(con) {
  mye <- gb_ons_mye("GBS"); s <- mye[grepl("^S12", mye$code), ]
  pop <- data.frame(country = "GBS", territory_id = paste0("GBS-", s$code), level = "municipality", year = s$year, population = s$population)
  nat <- mye[mye$code == "S92000003", ]
  if (nrow(nat)) pop <- rbind(pop, data.frame(country = "GBS", territory_id = "GBS", level = "country", year = nat$year, population = nat$population))
  if (nrow(pop) == 0) stop("No Scottish council areas (S12...) in the ONS file")
  replace_country_rows(con, "ref_country_population", "GBS", pop)
  message(sprintf("  population: %d council areas, %d-%d (ONS mid-year estimates = NRS figures)", length(unique(s$code)), min(pop$year), max(pop$year)))
}

# ===================================================================================================
# GBN · Northern Ireland ---------------------------------------------------------------------------
# ===================================================================================================

# The CSV is an upload on OpenDataNI (CKAN): its file name is only known through the API
gbn_csv_url <- function() {
  js <- get_text(GBN_SOURCES$api)
  if (!is.null(js)) {
    res <- tryCatch(jsonlite::fromJSON(js, simplifyVector = TRUE)$result$resources, error = function(e) NULL)
    if (!is.null(res) && "url" %in% names(res)) {
      i <- which(res$id == GBN_SOURCES$resource_id); if (length(i) == 0) i <- grep("monthly", res$name, ignore.case = TRUE)
      if (length(i) > 0 && nzchar(res$url[i[1]])) return(res$url[i[1]])
    }
  }
  message("  OpenDataNI API not reachable: trying the generic CKAN download URL")
  GBN_SOURCES$csv_guess
}

gbn_download <- function(days = 60) {
  gb_fetch("GBN", gbn_csv_url(), "gbn_recorded_crime_monthly.csv", days)
  gb_fetch("GBN", GB_LAD_GEO, "gb_lad_scotland_ni.geojson", 3650)
  gb_ons_mye_download("GBN")
}

gbn_inspect <- function() {
  f <- gb_file("GBN", "_gbn_recorded_crime_monthly\\.csv$"); if (is.na(f)) { message("no Northern Irish file yet"); return(invisible()) }
  d <- gb_read_csv(f); col <- lapply(GBN_COLUMNS, gb_pick, nm = names(d))
  message(basename(f), "\n  ", paste(names(d), collapse = " | "))
  for (k in c("month", "district", "measure")) if (!is.na(col[[k]])) message("  ", k, ": ", paste(sort(unique(d[[col[[k]]]])), collapse = " / "))
  if (!is.na(col$type)) { types <- read_reference("gbn_offence_types.csv"); u <- setdiff(unique(trimws(d[[col$type]])), types$source_code); if (length(u)) message("  not in gbn_offence_types.csv: ", paste(u, collapse = " / ")) }
  print(utils::head(as.data.frame(d), 5))
}

gbn_districts <- function() read_reference("gbn_districts.csv")

gbn_ingest <- function(con) {
  f <- gb_file("GBN", "_gbn_recorded_crime_monthly\\.csv$"); if (is.na(f)) stop("No Northern Irish file in ", COUNTRY_MODULES$GBN$dir_raw)
  d <- gb_read_csv(f); col <- gb_cols(d, GBN_COLUMNS, c("year", "month", "district", "type", "n"), paste("the PSNI file", basename(f)))
  mon <- trimws(d[[col$month]]); month <- suppressWarnings(as.integer(mon))
  month[is.na(month)] <- match(tolower(substr(mon[is.na(month)], 1, 3)), tolower(month.abb))   # "Apr" -> 4
  out <- data.frame(year = as.integer(gb_num(d[[col$year]])), month = month, district = trimws(d[[col$district]]), type = trimws(d[[col$type]]), n = gb_num(d[[col$n]]), stringsAsFactors = FALSE)
  if (!is.na(col$measure)) out <- out[grepl("^police recorded crime$", trimws(d[[col$measure]]), ignore.case = TRUE), ]   # not the outcomes rows
  out <- out[!is.na(out$year) & !is.na(out$month) & nzchar(out$type), ]
  if (nrow(out) == 0) stop("No 'Police Recorded Crime' rows in ", basename(f), "; run gbn_inspect()")
  # Policing districts = local government districts (reference table); "Northern Ireland" = the official national row
  dist <- gbn_districts(); key <- c(gb_norm(dist$psni_name), gb_norm(dist$name), gb_norm(gsub("\\bCity\\b", "", dist$psni_name)))
  code <- rep(dist$lgd_code, 3)[match(gb_norm(out$district), key)]
  code[is.na(code)] <- rep(dist$lgd_code, 3)[match(gb_norm(gsub("\\bCity\\b", "", out$district, ignore.case = TRUE)), key)][is.na(code)]
  national <- gb_norm(out$district) == "northernireland"
  out$level <- ifelse(national, "country", "municipality"); out$territory_id <- ifelse(national, "GBN", paste0("GBN-", ifelse(is.na(code), gb_slug(out$district), code)))
  out$name <- ifelse(is.na(code), out$district, dist$name[match(code, dist$lgd_code)])
  m <- out |> dplyr::group_by(country = "GBN", territory_id, level, offence_code = type, year, month) |> dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
  crime <- monthly_to_cumulative(as.data.frame(m)); crime$file <- basename(f)
  if (!"GBN" %in% crime$territory_id) {
    nat <- crime |> dplyr::group_by(country, offence_code, year, quarter, file) |> dplyr::summarise(cumulative = sum(cumulative), .groups = "drop"); nat$territory_id <- "GBN"; nat$level <- "country"
    crime <- rbind(crime, nat[, names(crime)])
  }
  u <- !duplicated(out$territory_id) & out$level == "municipality"
  ensure_territories(con, "GBN", out$territory_id[u], "municipality", "GBN", out$name[u])
  replace_country_rows(con, "stg_country_crime", "GBN", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_GBN", basename(f), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  types <- read_reference("gbn_offence_types.csv"); unknown <- setdiff(unique(crime$offence_code), types$source_code)
  message(sprintf("  Northern Ireland: %s rows, %d policing districts, %d-%dQ%d%s%s", fmt_int(nrow(crime)), sum(u), min(crime$year), max(crime$year), max(crime$quarter[crime$year == max(crime$year)]),
                  if (any(is.na(code) & !national)) paste0("\n  districts not in gbn_districts.csv: ", paste(unique(out$district[is.na(code) & !national]), collapse = " / ")) else "",
                  if (length(unknown)) paste0("\n  crime types not in gbn_offence_types.csv (ignored until mapped): ", paste(unknown, collapse = " / ")) else ""))
  invisible(crime)
}

# Policing districts = the 11 local government districts (reference table); boundaries from the ONS UK layer (N09 codes)
gbn_territory <- function(con) {
  dist <- gbn_districts()
  tbl <- data.frame(territory_id = paste0("GBN-", dist$lgd_code), level = "municipality", parent_id = "GBN", official_code = dist$lgd_code, name = dist$name, stringsAsFactors = FALSE)
  save_country_territory(con, "GBN", tbl, scheme = "ONS")
  f <- gb_file("GBN", "_gb_lad_scotland_ni\\.geojson$"); if (is.na(f)) f <- gb_file("GBS", "_gb_lad_scotland_ni\\.geojson$")
  if (!is.na(f)) {
    g <- gb_read_geojson(f); cd <- grep("^LAD[0-9]*CD$", names(g), value = TRUE)[1]; nm <- grep("^LAD[0-9]*NM$", names(g), value = TRUE)[1]
    g <- g[grepl("^N09", g[[cd]]), ]; g$territory_id <- paste0("GBN-", g[[cd]]); g$name <- g[[nm]]
    save_country_gpkg(con, "GBN", list(municipality = g[, c("territory_id", "name", attr(g, "sf_column"))]), tolerance_m = 100)
  } else message("  ONS local authority layer not downloaded: tables only")
  invisible(tbl)
}

gbn_population <- function(con) {
  mye <- gb_ons_mye("GBN"); s <- mye[grepl("^N09", mye$code), ]
  pop <- data.frame(country = "GBN", territory_id = paste0("GBN-", s$code), level = "municipality", year = s$year, population = s$population)
  nat <- mye[mye$code == "N92000002", ]
  if (nrow(nat)) pop <- rbind(pop, data.frame(country = "GBN", territory_id = "GBN", level = "country", year = nat$year, population = nat$population))
  if (nrow(pop) == 0) stop("No Northern Irish districts (N09...) in the ONS file")
  replace_country_rows(con, "ref_country_population", "GBN", pop)
  message(sprintf("  population: %d districts, %d-%d (ONS mid-year estimates = NISRA figures)", length(unique(s$code)), min(pop$year), max(pop$year)))
}

# --- Registration --------------------------------------------------------------------------------------

GB_NOT_PUBLISHED <- c("Not published by the source", "Not published by the source")
GB_POP_SOURCE <- "ONS mid-year population estimates (UK local authorities; Scottish and Northern Irish figures from NRS and NISRA)"

register_country(list(
  code = "GBE", iso3 = "GBR", name_en = "England and Wales", name_local = "England and Wales", lang = "en",
  levels = c("region", "municipality"),
  level_labels = list(region = c("Police force areas", "Police force areas", "Police force area", "Police force area"),
                      municipality = c("Community Safety Partnerships", "Community Safety Partnerships", "Community Safety Partnership", "Community Safety Partnership")),
  source = list(name = "Police recorded crime open data tables (Community Safety Partnerships)", publisher = "Home Office", url = GBE_SOURCES$page,
                licence = "Open Government Licence v3.0",
                measure = "Offences recorded by the police under the Home Office Counting Rules, by financial-year quarter (Q4 = the whole year ending in March), excluding fraud and British Transport Police; the total shown is the sum of the offence groups."),
  population_source = GB_POP_SOURCE, boundaries_attribution = "Contains OS data © Crown copyright and database right; Source: Office for National Statistics licensed under the Open Government Licence v3.0",
  not_published = GB_NOT_PUBLISHED,
  notes_en = "Quarters are financial-year quarters (April-March); year = the year the financial year ends in. Recording practice changed in 2014 (HMICFRS) and burglary was reclassified in April 2017; quarterly figures before 2019 can be negative (corrections). Some CSPs are parts of a local authority and have no population (no rates).",
  default_region = "GBE-E23000001",
  download = gbe_download, ingest = gbe_ingest, territory = gbe_territory, population = gbe_population
))

register_country(list(
  code = "GBS", iso3 = "GBR", name_en = "Scotland", name_local = "Scotland", lang = "en",
  levels = c("municipality"),
  level_labels = list(municipality = c("Council areas", "Council areas", "Council area", "Council area")),
  source = list(name = "Recorded Crimes and Offences", publisher = "Scottish Government (Police Scotland data)", url = GBS_SOURCES$page,
                licence = "Open Government Licence v3.0",
                measure = "Crimes and offences recorded by Police Scotland, by financial year (quarter 4 = the year ending in March); the total shown is the sum of all crimes and of the offence groups except road traffic offences."),
  population_source = GB_POP_SOURCE, boundaries_attribution = "Contains OS data © Crown copyright and database right; Source: Office for National Statistics licensed under the Open Government Licence v3.0",
  not_published = GB_NOT_PUBLISHED,
  notes_en = "Scottish counting rules differ from those of England and Wales and Northern Ireland: never compare. 'Crimes' (more serious) and 'offences' are separate official totals; the classification was revised in 2023/24.",
  default_region = "GBS-S12000036",
  download = gbs_download, ingest = gbs_ingest, territory = gbs_territory, population = gbs_population
))

register_country(list(
  code = "GBN", iso3 = "GBR", name_en = "Northern Ireland", name_local = "Northern Ireland", lang = "en",
  levels = c("municipality"),
  level_labels = list(municipality = c("Policing districts", "Policing districts", "Policing district", "Policing district")),
  source = list(name = "Police Recorded Crime Monthly Data", publisher = "Police Service of Northern Ireland (OpenDataNI)", url = GBN_SOURCES$page,
                licence = "Open Government Licence (OGL)",
                measure = "Crimes recorded by the PSNI under the Home Office Counting Rules, monthly, by policing district (official total included)."),
  population_source = GB_POP_SOURCE, boundaries_attribution = "Contains OS data © Crown copyright and database right; Source: Office for National Statistics licensed under the Open Government Licence v3.0",
  not_published = GB_NOT_PUBLISHED,
  notes_en = "Monthly data since April 2001 (complete calendar quarters only); burglary was reclassified in April 2017. The OpenDataNI dataset was last updated in January 2022 (data to December 2021): PSNI now publishes the monthly figures by policing district only as PDF bulletins, so later years are not loaded.",
  default_region = "GBN-N09000003",
  download = gbn_download, ingest = gbn_ingest, territory = gbn_territory, population = gbn_population
))
