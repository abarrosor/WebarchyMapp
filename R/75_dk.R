# Denmark: reported offences by municipality and region ------------------------------------
# Source: Statistics Denmark (Danmarks Statistik), StatBank table STRAF11 "Anmeldte forbrydelser
# efter område, overtrædelsens art og tid": offences reported to the police (one report = one
# count), by municipality of the offence and quarter of the offence date, 2007Q1 onwards,
# 91 offence codes (Criminal Code chapters and their items, plus special legislation; the Road
# Traffic Act is NOT in this table). Open, key-less JSON/CSV API, CC BY 4.0.
# Population: StatBank FOLK1A (population on the first day of the quarter; Q1 = 1 January).
# Boundaries: Eurostat GISCO LAU (DK LAU = kommuner); regions are unions of their municipalities.
#
# The API is called with GET URLs (the StatBank API accepts GET; "BULK" is the streaming CSV
# format without the 1,000,000-cell limit). BULK omits the cells that are zero, so the loader
# rebuilds the full grid before accumulating the quarters. The reader detects columns by name;
# `dk_inspect()` prints the headers of the downloaded files when something changes.

DK_API <- "https://api.statbank.dk/v1/data"
DK_RESOURCES <- c(
  # every area x offence code x quarter, codes instead of labels (~760,000 rows, ~17 MB)
  straf11 = paste0(DK_API, "/STRAF11/BULK?OMR%C3%85DE=*&OVERTR%C3%86D=*&Tid=*&valuePresentation=Code"),
  # population by area and quarter, total sex / age / marital status (~8,000 rows)
  folk1a  = paste0(DK_API, "/FOLK1A/CSV?OMR%C3%85DE=*&K%C3%98N=TOT&ALDER=IALT&CIVILSTAND=TOT&Tid=*&valuePresentation=Code")
)
# Accepted column names (first match wins, case-insensitive; Danish letters avoided in the patterns)
DK_COLUMNS <- list(area = c("^OMR", "^area$"), type = c("^OVERTR", "^offence"), time = c("^TID$", "^time$"), value = c("^INDHOLD$", "^value$"))
DK_NATIONAL <- "000"          # "Hele landet"
DK_UNKNOWN  <- "998"          # "Uoplyst kommune": inside the national total only

dk_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }

dk_file <- function(name) {
  f <- newest_first(list.files(COUNTRY_MODULES$DK$dir_raw, paste0("_dk_", name, "\\.csv$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}

# StatBank CSV/BULK: semicolon separated, UTF-8 with BOM, all columns as text
dk_read <- function(f) readr::read_delim(f, delim = ";", col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = "UTF-8"), progress = FALSE, show_col_types = FALSE)

dk_inspect <- function() {
  for (nm in names(DK_RESOURCES)) { f <- dk_file(nm); if (is.na(f)) { message(nm, ": not downloaded"); next }
    d <- dk_read(f); message(nm, ": ", basename(f), " (", fmt_int(nrow(d)), " rows)\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
}

dk_download <- function(days = 30) {
  m <- COUNTRY_MODULES$DK
  for (nm in names(DK_RESOURCES)) {
    prev <- dk_file(nm)
    if (!is.na(prev) && file_age_days(prev) < days) { message("= DK ", nm, " downloaded less than ", days, " days ago"); next }
    download_versioned(DK_RESOURCES[[nm]], m$dir_raw, name = paste0("dk_", nm, ".csv"))
  }
}

# Municipality -> region table (Statistics Denmark codes), with names
dk_municipalities <- function() read_reference("dk_municipalities.csv")

# Area code -> territory_id (NA for codes that are not territories, e.g. "998 Uoplyst kommune")
dk_ids <- function(code, muni) {
  code <- trimws(code)
  regs <- unique(muni$region_code)
  reg_of <- muni$region_code[match(code, muni$municipality_code)]
  ifelse(code == DK_NATIONAL, "DK",
         ifelse(code %in% regs, paste0("DK-", code),
                ifelse(!is.na(reg_of), paste("DK", reg_of, code, sep = "-"), NA_character_)))
}
dk_level <- function(id) ifelse(id == "DK", "country", ifelse(nchar(id) == 6, "region", "municipality"))

# Standardise a StatBank extract to: area, type (optional), year, quarter, value
dk_standardise <- function(d, what) {
  nm <- names(d); col <- lapply(DK_COLUMNS, dk_pick, nm = nm)
  if (is.na(col$area) || is.na(col$time) || is.na(col$value))
    stop("Unexpected columns in the Danish ", what, " file: ", paste(nm, collapse = " | "), "\nRun dk_inspect() and adjust DK_COLUMNS in R/75_dk.R")
  tid <- trimws(d[[col$time]])
  out <- data.frame(area = trimws(d[[col$area]]), type = if (!is.na(col$type)) trimws(d[[col$type]]) else NA_character_,
                    year = suppressWarnings(as.integer(substr(tid, 1, 4))), quarter = suppressWarnings(as.integer(sub("^.*K", "", tid))),
                    value = suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", d[[col$value]])))), stringsAsFactors = FALSE)
  out[!is.na(out$year) & !is.na(out$quarter), ]
}

dk_ingest <- function(con) {
  f <- dk_file("straf11"); if (is.na(f)) stop("No STRAF11 file yet in ", COUNTRY_MODULES$DK$dir_raw)
  muni <- dk_municipalities()
  d <- dk_standardise(dk_read(f), "STRAF11")
  d$territory_id <- dk_ids(d$area, muni)
  unknown <- setdiff(unique(d$area[is.na(d$territory_id)]), DK_UNKNOWN)
  if (length(unknown)) message("  area codes not in reference/dk_municipalities.csv (left out): ", paste(unknown, collapse = ", "))
  d <- d[!is.na(d$territory_id), ]
  # BULK omits zero cells: rebuild the full grid (area x code x quarter) so that cumsum is right.
  # A cell present without a number is suppressed -> NA (the app shows "< threshold").
  periods <- unique(d[, c("year", "quarter")]); periods <- periods[order(periods$year, periods$quarter), ]
  grid <- merge(merge(data.frame(territory_id = unique(d$territory_id)), data.frame(type = unique(d$type))), periods)
  d$present <- TRUE
  g <- dplyr::left_join(grid, d[, c("territory_id", "type", "year", "quarter", "value", "present")], by = c("territory_id", "type", "year", "quarter"))
  g$value[is.na(g$present)] <- 0
  crime <- g |> dplyr::group_by(territory_id, type, year) |> dplyr::arrange(quarter, .by_group = TRUE) |>
    dplyr::mutate(cumulative = cumsum(value)) |> dplyr::ungroup() |> as.data.frame()   # cumsum keeps NA from a suppressed quarter onwards
  crime <- data.frame(country = "DK", territory_id = crime$territory_id, level = dk_level(crime$territory_id), offence_code = crime$type,
                      year = crime$year, quarter = crime$quarter, cumulative = crime$cumulative, file = basename(f))
  replace_country_rows(con, "stg_country_crime", "DK", crime)
  log_ingest(con, "country_DK", basename(f), sha256_file(f), paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  STRAF11: %s rows, %d territories, %dQ%d-%dQ%d", fmt_int(nrow(crime)), length(unique(crime$territory_id)),
                  periods$year[1], periods$quarter[1], periods$year[nrow(periods)], periods$quarter[nrow(periods)]))
  invisible(crime)
}

# Population on 1 January (FOLK1A, first quarter) for the country, the regions and the municipalities
dk_population <- function(con) {
  f <- dk_file("folk1a"); if (is.na(f)) stop("No FOLK1A file yet in ", COUNTRY_MODULES$DK$dir_raw)
  muni <- dk_municipalities()
  d <- dk_standardise(dk_read(f), "FOLK1A")
  d <- d[d$quarter == 1 & !is.na(d$value), ]
  d$territory_id <- dk_ids(d$area, muni); d <- d[!is.na(d$territory_id), ]
  pop <- data.frame(country = "DK", territory_id = d$territory_id, level = dk_level(d$territory_id), year = d$year, population = d$value)
  pop <- pop[!duplicated(pop[, c("territory_id", "year")]), ]
  replace_country_rows(con, "ref_country_population", "DK", pop)
  message(sprintf("  population: %d territories, %d-%d (FOLK1A, 1 January)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

# Territories: regions and municipalities from reference/dk_municipalities.csv; polygons from GISCO LAU
dk_territory <- function(con) {
  muni <- dk_municipalities()
  regs <- unique(muni[, c("region_code", "region_name")])
  tbl <- rbind(
    data.frame(territory_id = paste0("DK-", regs$region_code), level = "region", parent_id = "DK", official_code = regs$region_code, name = regs$region_name),
    data.frame(territory_id = paste("DK", muni$region_code, muni$municipality_code, sep = "-"), level = "municipality",
               parent_id = paste0("DK-", muni$region_code), official_code = muni$municipality_code, name = muni$municipality_name))
  save_country_territory(con, "DK", tbl, scheme = "Danmarks Statistik")
  lau <- tryCatch(gisco_lau("DK"), error = function(e) { message("  GISCO LAU not available: municipalities without boundaries (", conditionMessage(e), ")"); NULL })
  if (!is.null(lau)) {
    code <- sprintf("%03d", suppressWarnings(as.integer(gsub("\\D", "", lau$LAU_ID))))   # "101" or "0101" -> "101"
    i <- match(code, muni$municipality_code); ok <- !is.na(i)
    if (!any(ok)) { message("  GISCO LAU_ID does not match the Danish municipality codes (e.g. ", paste(utils::head(lau$LAU_ID, 3), collapse = ", "), "): no boundaries"); return(invisible(tbl)) }
    g <- lau[ok, ]; g$territory_id <- paste("DK", muni$region_code[i[ok]], code[ok], sep = "-"); g$name <- muni$municipality_name[i[ok]]
    g <- g[, c("territory_id", "name", attr(g, "sf_column"))]
    gr <- g; gr$territory_id <- sub("-[^-]+$", "", gr$territory_id); gr <- gr |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop")
    gr$name <- regs$region_name[match(sub("^DK-", "", gr$territory_id), regs$region_code)]
    save_country_gpkg(con, "DK", list(region = gr, municipality = g), tolerance_m = 100)
  }
  invisible(tbl)
}

register_country(list(
  code = "DK", iso3 = "DNK", name_en = "Denmark", name_local = "Danmark", lang = "da",
  levels = c("region", "municipality"),
  level_labels = list(region = c("Regions", "Regioner", "Region", "Region"), municipality = c("Municipalities", "Kommuner", "Municipality", "Kommune")),
  source = list(name = "Anmeldte forbrydelser (StatBank STRAF11)", publisher = "Danmarks Statistik (Statistics Denmark)",
                url = "https://www.statbank.dk/STRAF11", licence = "CC BY 4.0",
                measure = "Offences reported to the police (anmeldelser: one count per report), Criminal Code and special legislation, Road Traffic Act excluded; quarterly by municipality of the offence and quarter of the offence date."),
  population_source = "Danmarks Statistik, StatBank FOLK1A (population on 1 January)",
  boundaries_attribution = "© EuroGeographics for the administrative boundaries (Eurostat GISCO LAU)",
  not_published = c("Not published by Statistics Denmark", "Ikke offentliggjort af Danmarks Statistik"),
  notes_en = "Quarters follow the date of the offence, so the latest quarters are revised later. Sexual-offence provisions changed on 1 July 2013 (break). 2023 under-counts animal/hunting laws by about 1,700 reports. Christiansø and the small islands have tiny populations: their rates are noisy. 'Own calculations based on data from Statistics Denmark'.",
  method = list(where = "Municipality where the offence was committed (gerningssted)", date = "Quarter of the offence date (gerningstidspunkt), not of the report", revisions = "The latest quarters are revised as late reports arrive; sexual-offence provisions changed on 1 July 2013"),
  default_region = "DK-084",
  download = dk_download, ingest = dk_ingest, territory = dk_territory, population = dk_population
))
