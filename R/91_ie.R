# Ireland: recorded crime incidents by Garda Division --------------------------------------------
# Source: CSO PxStat table CJQ10 "Recorded Crime Incidents (New Garda Operating Model)", quarterly
# 2003Q1 onwards, 21 Garda Divisions x 85 ICCS categories (16 groups + sub-categories), CC BY 4.0,
# no key. CSV through the PxStat Cube API (~170 000 rows, ~20 MB). Counts are per quarter (not
# cumulative): the module adds them up within each year. There is no official total: the total is
# the sum of the 16 ICCS groups (sub-categories are only counted inside their group).
# Garda Divisions are the only sub-national level and have no open boundaries: the app shows
# tables for Ireland (no map). Population by Garda Division is not published: only the national
# population (PEA01, persons in April, thousands) is loaded, so divisional rates are unavailable.
#
# Written against the live CSV header and the JSON-stat metadata (1 Oct 2026), not run against
# the full file: the reader detects the columns by name; `ie_inspect()` prints the header.

IE_RESOURCES <- c(
  crime      = "https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/CJQ10/CSV/1.0/en",
  population = "https://ws.cso.ie/public/api.restful/PxStat.Data.Cube_API.ReadDataset/PEA01/CSV/1.0/en"
)
IE_COLUMNS <- list(
  quarter  = c("^TLIST\\(Q1\\)$", "^Quarter$"),
  year     = c("^TLIST\\(A1\\)$", "^Year$"),
  div_code = c("^C02481V05453$", "^C0[0-9]+V[0-9]+$"),
  div_name = c("^Garda Division$"),
  offence  = c("^C02480V03003$"),
  age      = c("^Age Group$"), sex = c("^Sex$"),
  unit     = c("^UNIT$"),
  value    = c("^VALUE$")
)

ie_dir <- function() COUNTRY_MODULES$IE$dir_raw
ie_file <- function(what) { f <- newest_first(list.files(ie_dir(), paste0("_ie_", what, "\\.csv$"), full.names = TRUE)); if (length(f)) f[1] else NA_character_ }
ie_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm); if (length(i)) return(nm[i[1]]) }; NA_character_ }
ie_read <- function(f) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE)

ie_download <- function(days = 60) {
  for (what in names(IE_RESOURCES)) {
    prev <- ie_file(what)
    if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < days) { message("= IE ", what, " downloaded less than ", days, " days ago"); next }
    download_versioned(IE_RESOURCES[[what]], ie_dir(), name = paste0("ie_", what, ".csv"))
  }
}

ie_inspect <- function() {
  for (what in names(IE_RESOURCES)) { f <- ie_file(what); if (is.na(f)) { message("no IE ", what, " file yet"); next }
    d <- ie_read(f); message(what, ": ", basename(f), " (", fmt_int(nrow(d)), " rows)\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
}

ie_ingest <- function(con) {
  f <- ie_file("crime"); if (is.na(f)) stop("No CSO CJQ10 file in ", ie_dir(), " (run the download first)")
  types <- read_reference("ie_offence_types.csv"); divs <- read_reference("ie_garda_divisions.csv")
  d <- ie_read(f); nm <- names(d); col <- lapply(IE_COLUMNS, ie_pick, nm = nm)
  # The offence code column is the C0...V... column that is not the division column
  if (is.na(col$offence)) { cands <- setdiff(grep("^C0[0-9]+V[0-9]+$", nm, value = TRUE), col$div_code); if (length(cands)) col$offence <- cands[1] }
  if (is.na(col$quarter) || is.na(col$div_code) || is.na(col$offence) || is.na(col$value))
    stop("Unexpected columns in the CSO CJQ10 file: ", paste(nm, collapse = " | "), "\nRun ie_inspect() and adjust IE_COLUMNS in R/91_ie.R")
  q <- gsub("[^0-9]", "", d[[col$quarter]])                                # "20031" or "2003Q1" -> 20031
  x <- data.frame(code = trimws(d[[col$div_code]]), div_name = if (!is.na(col$div_name)) d[[col$div_name]] else NA_character_,
                  offence_code = trimws(d[[col$offence]]), year = as.integer(substr(q, 1, 4)), quarter = as.integer(substr(q, 5, 5)),
                  n = suppressWarnings(as.numeric(d[[col$value]])), stringsAsFactors = FALSE)
  x <- x[!is.na(x$year) & !is.na(x$quarter) & x$quarter %in% 1:4 & nzchar(x$offence_code), ]
  unknown <- setdiff(unique(x$offence_code), types$source_code)
  if (length(unknown)) message("  ICCS codes without a row in reference/ie_offence_types.csv (left out): ", paste(unknown, collapse = ", "))
  x <- x[x$offence_code %in% types$source_code, ]
  x$territory_id <- paste0("IE-", x$code)
  # Synthetic total = sum of the 16 ICCS groups (2-digit codes) that are not 'unmapped'
  groups <- types$source_code[nchar(types$source_code) == 2 & types$offence_type_id != "unmapped"]
  tot <- x[x$offence_code %in% groups, ] |> dplyr::group_by(territory_id, code, div_name, year, quarter) |>
    dplyr::summarise(n = if (all(is.na(n))) NA_real_ else sum(n, na.rm = TRUE), .groups = "drop")
  tot$offence_code <- "TOTAL_GROUPS"
  x <- rbind(x, tot[, names(x)])
  # Quarterly counts -> cumulative from 1 January (a quarter missing in the source makes the later ones unknown)
  cum <- x |> dplyr::group_by(territory_id, offence_code, year) |> dplyr::arrange(quarter, .by_group = TRUE) |>
    dplyr::mutate(cumulative = cumsum(ifelse(is.na(n), NA_real_, n))) |> dplyr::ungroup()
  crime <- data.frame(country = "IE", territory_id = cum$territory_id, level = "region", offence_code = cum$offence_code, year = cum$year, quarter = cum$quarter,
                      cumulative = cum$cumulative, file = basename(f), stringsAsFactors = FALSE)
  # State = sum of the divisions (CJQ10 has no State row)
  nat <- crime |> dplyr::group_by(country, offence_code, year, quarter, file) |> dplyr::summarise(cumulative = if (all(is.na(cumulative))) NA_real_ else sum(cumulative, na.rm = TRUE), .groups = "drop")
  nat$territory_id <- "IE"; nat$level <- "country"
  crime <- rbind(crime, nat[, names(crime)])
  names_of <- unique(x[, c("territory_id", "code", "div_name")]); names_of$name <- ifelse(is.na(names_of$div_name) | names_of$div_name == "", divs$name[match(names_of$code, divs$code)], names_of$div_name)
  ensure_territories(con, "IE", names_of$territory_id, "region", "IE", names_of$name)
  n_div <- length(unique(crime$territory_id[crime$level == "region"]))
  if (n_div != 21) message("  [!!] ", n_div, " Garda Divisions found (expected 21)")
  replace_country_rows(con, "stg_country_crime", "IE", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_IE", basename(f), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  %s rows, %d divisions, %dQ%d-%dQ%d", fmt_int(nrow(crime)), n_div, min(crime$year), min(crime$quarter[crime$year == min(crime$year)]), max(crime$year), max(crime$quarter[crime$year == max(crime$year)])))
  invisible(crime)
}

# National population only (PEA01: persons in April, thousands). No population by Garda Division is published.
ie_population <- function(con) {
  f <- ie_file("population"); if (is.na(f)) stop("No CSO PEA01 file in ", ie_dir())
  d <- ie_read(f); nm <- names(d); col <- lapply(IE_COLUMNS, ie_pick, nm = nm)
  if (is.na(col$year) || is.na(col$value)) stop("Unexpected columns in the CSO PEA01 file: ", paste(nm, collapse = " | "), "\nRun ie_inspect()")
  keep <- rep(TRUE, nrow(d))
  if (!is.na(col$age)) keep <- keep & grepl("^all ages$", d[[col$age]], ignore.case = TRUE)
  if (!is.na(col$sex)) keep <- keep & grepl("^both sexes$", d[[col$sex]], ignore.case = TRUE)
  v <- suppressWarnings(as.numeric(d[[col$value]][keep])); yr <- as.integer(gsub("[^0-9]", "", d[[col$year]][keep]))
  unit <- if (!is.na(col$unit)) tolower(d[[col$unit]][keep][1]) else "thousand"
  if (grepl("thousand", unit)) v <- v * 1000
  pop <- data.frame(country = "IE", territory_id = "IE", level = "country", year = yr, population = v)
  pop <- pop[!is.na(pop$year) & !is.na(pop$population) & pop$year >= 2000, ]
  if (nrow(pop) == 0) stop("No 'All ages / Both sexes' rows found in the PEA01 file")
  replace_country_rows(con, "ref_country_population", "IE", pop)
  message(sprintf("  population: national only, %d-%d (CSO PEA01); rates by Garda Division are unavailable", min(pop$year), max(pop$year)))
}

# Garda Divisions from the reference list (names); no boundaries are published as open data
ie_territory <- function(con) {
  divs <- read_reference("ie_garda_divisions.csv")
  tbl <- data.frame(territory_id = paste0("IE-", divs$code), level = "region", parent_id = "IE", official_code = divs$code, name = divs$name)
  save_country_territory(con, "IE", tbl, scheme = "CSO C02481V05453")
  message("  Garda Division boundaries are not open data: Ireland is shown as tables (no gpkg)")
  invisible(tbl)
}

register_country(list(
  code = "IE", iso3 = "IRL", name_en = "Ireland", name_local = "Ireland / Éire", lang = "en",
  levels = c("region"),
  level_labels = list(region = c("Garda Divisions", "Garda Divisions", "Garda Division", "Garda Division")),
  source = list(name = "CJQ10 Recorded Crime Incidents by Garda Division (CSO PxStat)", publisher = "Central Statistics Office (CSO), from An Garda Síochána PULSE",
                url = "https://data.cso.ie/table/CJQ10", licence = "CC BY 4.0",
                measure = "Recorded crime incidents (PULSE) by quarter of recording and Garda Division, ICCS classification; no official total: the total shown is the sum of the 16 ICCS groups."),
  population_source = "CSO PEA01 (national population estimates, April); no population by Garda Division is published, so divisional rates are unavailable",
  boundaries_attribution = "",
  not_published = c("Not available in the CSO table for this division and quarter", "Not available in the CSO table for this division and quarter"),
  notes_en = "Garda Divisions (New Operating Model, 2025) are policing areas, not counties, and have no open boundaries; statistics 2015-2023 were published 'Under Reservation'; fraud (09) from 2023Q3 covers only public reports (not comparable with earlier quarters); drink and drug driving are merged from 2025Q1.",
  default_region = "IE-400",
  download = ie_download, ingest = ie_ingest, territory = ie_territory, population = ie_population
))
