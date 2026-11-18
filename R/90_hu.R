# Hungary: registered crimes by county (vármegye) and Budapest ----------------------------------
# Source: KSH STADAT table 11.1.2.1 "Regisztrált bűncselekmények vármegye és régió szerint"
# (iga0008), yearly 2009 onwards, CSV (~30 kB). Figures come from the unified police-prosecution
# statistics (ENyÜBS; Ministry of Interior + Prosecutor General), by place of commission, counted
# at the closure of the investigation. 7 categories: total + 6 selected offences.
# The national row includes offences with unknown or foreign location: it is kept as published.
# Population: KSH STADAT 22.1.2.1 "Resident population by sex, county and region, 1 January"
# (nep0034). Boundaries: Eurostat GISCO NUTS 3 (= vármegyék + Budapest), else geoBoundaries.
# Licence: KSH terms of use (free re-use with the attribution "Forrás: KSH"); not Creative Commons.
#
# Written against the HTML rendering of the tables, not run against the CSV files: the layout
# is one row per territorial unit, one column per year, category blocks introduced by a label
# row; `hu_inspect()` prints the first lines of the downloaded files to adjust HU_FILES if needed.

HU_FILES <- c(
  crime      = "https://www.ksh.hu/stadat_files/iga/en/iga0008.csv",   # 11.1.2.1 registered crimes by county and region (English labels)
  population = "https://www.ksh.hu/stadat_files/nep/en/nep0034.csv"    # 22.1.2.1 resident population by sex, county and region, 1 January
)
HU_SKIP <- "region|large region|^of which|^name$|^level$"           # rows that are neither a county nor a category

hu_dir <- function() COUNTRY_MODULES$HU$dir_raw
hu_counties <- function() read_reference("hu_counties.csv")
hu_file <- function(what) { f <- newest_first(list.files(hu_dir(), paste0("_hu_", what, "\\.csv$"), full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

hu_download <- function(days = 60) {
  for (what in names(HU_FILES)) {
    prev <- hu_file(what)
    if (!is.na(prev) && file_age_days(prev) < days) { message("= HU ", what, " downloaded less than ", days, " days ago"); next }
    download_versioned(HU_FILES[[what]], hu_dir(), name = paste0("hu_", what, ".csv"))
  }
}

# Read a STADAT CSV as a character matrix (delimiter detected; UTF-8 or Latin-2)
hu_read <- function(f) {
  lines <- readLines(f, warn = FALSE)                       # bytes as they are; the whole file decides the encoding
  utf8 <- all(validUTF8(lines)); enc <- if (utf8) "UTF-8" else "windows-1250"   # KSH exports: UTF-8 (often with BOM) or Central European Windows
  first <- iconv(utils::head(lines, 3), if (utf8) "UTF-8" else "windows-1250", "UTF-8", sub = "")
  sep <- names(which.max(sapply(c(";" = ";", "," = ",", "\t" = "\t"), function(s) sum(lengths(regmatches(first, gregexpr(s, first, fixed = TRUE)))))))
  d <- readr::read_delim(f, delim = sep, col_names = FALSE, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE,
                         locale = readr::locale(encoding = enc), skip_empty_rows = TRUE)
  m <- as.matrix(d); m[is.na(m)] <- ""; m <- apply(m, 2, function(col) iconv(col, "UTF-8", "UTF-8", sub = ""))   # never leave an invalid byte for the regexes
  if (is.null(dim(m))) m <- matrix(m, nrow = 1); m
}
hu_number <- function(x) { x <- gsub("[^0-9]", "", gsub("\\s", "", x)); suppressWarnings(as.numeric(ifelse(nzchar(x), x, NA))) }   # "104,253" / "104 253" -> 104253; ".." -> NA
hu_clean <- function(x) tolower(trimws(gsub("[¹²³⁰-₟*]", "", x)))   # drop superscript footnote marks (plain footnote letters, e.g. "regiona", are tolerated by the patterns)

# Standardise a STADAT wide table: block (category or sex section), code (county code / HU / NA), year, n
hu_standardise <- function(m, counties) {
  hdr <- which(apply(m, 1, function(r) sum(grepl("^(19|20)[0-9]{2}$", trimws(r))) >= 5))[1]
  if (is.na(hdr)) stop("No header row with years found")
  years <- suppressWarnings(as.integer(trimws(m[hdr, ]))); ycol <- which(!is.na(years))
  block <- NA_character_; rows <- list()
  for (i in seq(hdr + 1, nrow(m))) {
    lab_raw <- unname(m[i, 1]); lab <- hu_clean(lab_raw); if (!nzchar(lab)) next   # unname: the cell carries its column name (X1), and a named block made data.frame() warn once per row
    vals <- hu_number(m[i, ycol])
    if (all(is.na(vals)) && !grepl(HU_SKIP, lab)) { block <- lab; next }       # a label row introduces a block
    if (grepl("^total[a-g]? ?country|^country total|^hungary|^magyarorsz|^ország összesen", lab)) code <- "HU"
    else { hit <- which(vapply(counties$match_pattern, function(p) grepl(p, lab, perl = TRUE), logical(1))); code <- if (length(hit)) counties$code[hit[1]] else NA }   # region rows never start with a county name
    if (is.na(code)) next
    rows[[length(rows) + 1]] <- data.frame(block = block, code = code, year = unname(years[ycol]), n = unname(vals), stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) stop("No county rows recognised (check reference/hu_counties.csv match_pattern)")
  do.call(rbind, rows)
}

hu_inspect <- function() {
  for (what in names(HU_FILES)) { f <- hu_file(what); if (is.na(f)) { message("no HU ", what, " file yet"); next }
    m <- hu_read(f); message(what, ": ", basename(f), " (", nrow(m), " x ", ncol(m), ")"); print(utils::head(m[, seq_len(min(6, ncol(m)))], 8))
    d <- tryCatch(hu_standardise(m, hu_counties()), error = function(e) { message(conditionMessage(e)); NULL })
    if (!is.null(d)) message("  blocks: ", paste(unique(d$block), collapse = " | "), "\n  codes: ", length(unique(d$code)), ", years ", min(d$year), "-", max(d$year)) }
}

hu_ingest <- function(con) {
  f <- hu_file("crime"); if (is.na(f)) stop("No KSH crime file in ", hu_dir(), " (run the download first)")
  types <- read_reference("hu_offence_types.csv"); counties <- hu_counties()
  d <- hu_standardise(hu_read(f), counties)
  d$block[is.na(d$block)] <- "total number of crimes"                       # the first block (before "Of which:") is the total
  d$offence_code <- NA_character_
  for (i in seq_len(nrow(types))) { hit <- is.na(d$offence_code) & grepl(types$match_pattern[i], d$block, perl = TRUE); d$offence_code[hit] <- types$source_code[i] }
  unknown <- unique(d$block[is.na(d$offence_code)])
  if (length(unknown)) message("  categories without a row in reference/hu_offence_types.csv (left out): ", paste(unknown, collapse = " | "))
  d <- d[!is.na(d$offence_code) & !is.na(d$year), ]
  crime <- data.frame(country = "HU", territory_id = ifelse(d$code == "HU", "HU", paste0("HU-", d$code)), level = ifelse(d$code == "HU", "country", "region"),
                      offence_code = d$offence_code, year = d$year, n = d$n, file = basename(f), stringsAsFactors = FALSE)
  crime <- yearly_to_cumulative(crime[!duplicated(crime[, c("territory_id", "offence_code", "year")]), ])
  n_reg <- length(unique(crime$territory_id[crime$level == "region"]))
  if (n_reg != 20) message("  [!!] ", n_reg, " counties recognised (expected 20): run hu_inspect()")
  replace_country_rows(con, "stg_country_crime", "HU", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_HU", basename(f), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  %s rows, %d counties, %d-%d, categories: %s", fmt_int(nrow(crime)), n_reg, min(crime$year), max(crime$year), paste(unique(crime$offence_code), collapse = ", ")))
  invisible(crime)
}

# Population on 1 January: the "Total" (both sexes) section of nep0034
hu_population <- function(con) {
  f <- hu_file("population"); if (is.na(f)) stop("No KSH population file in ", hu_dir())
  d <- hu_standardise(hu_read(f), hu_counties())
  d$block[is.na(d$block)] <- "(first)"
  both <- grep("^total[a-g]?$|^together|^összesen|^mindösszesen|^both", unique(d$block), value = TRUE)
  blk <- if (length(both)) both[1] else utils::tail(unique(d$block), 1)       # KSH order: Males, Females, Total
  message("  population section used: '", blk, "' (sections: ", paste(unique(d$block), collapse = " | "), ")")
  d <- d[d$block == blk & !is.na(d$n), ]
  pop <- data.frame(country = "HU", territory_id = ifelse(d$code == "HU", "HU", paste0("HU-", d$code)), level = ifelse(d$code == "HU", "country", "region"), year = d$year, population = d$n)
  pop <- pop[!duplicated(pop[, c("territory_id", "year")]), ]
  nat <- pop$population[pop$territory_id == "HU"]
  if (length(nat) && (max(nat) < 8e6 || max(nat) > 12e6)) message("  [!!] national population ", max(nat), " looks wrong: check the section/units in hu_inspect()")
  replace_country_rows(con, "ref_country_population", "HU", pop)
  message(sprintf("  population: %d territories, %d-%d (KSH STADAT 22.1.2.1)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

hu_territory <- function(con) {
  counties <- hu_counties()
  tbl <- data.frame(territory_id = paste0("HU-", counties$code), level = "region", parent_id = "HU", official_code = counties$nuts3, name = counties$name_hu)
  save_country_territory(con, "HU", tbl, scheme = "NUTS 3")
  g <- tryCatch({ x <- gisco_nuts(3); x <- x[x$CNTR_CODE == "HU", ]; x$code <- counties$code[match(x$NUTS_ID, counties$nuts3)]; x[!is.na(x$code), ] },
                error = function(e) { message("  GISCO NUTS not available (", conditionMessage(e), "): trying geoBoundaries"); NULL })
  if (is.null(g) || nrow(g) != 20) {
    g <- tryCatch({ x <- geoboundaries("HUN", 1); nm <- hu_clean(x$shapeName); hit <- sapply(nm, function(l) { h <- which(vapply(counties$match_pattern, function(p) grepl(p, l, perl = TRUE), logical(1))); if (length(h)) counties$code[h[1]] else NA })
                    x$code <- unname(hit); x[!is.na(x$code), ] }, error = function(e) { message("  geoBoundaries not available: no boundaries (", conditionMessage(e), ")"); NULL })
  }
  if (!is.null(g) && nrow(g)) {
    if (nrow(g) != 20) message("  [!!] ", nrow(g), " county polygons matched (expected 20)")
    g$territory_id <- paste0("HU-", g$code); g$name <- counties$name_hu[match(g$code, counties$code)]
    save_country_gpkg(con, "HU", list(region = g[, c("territory_id", "name", attr(g, "sf_column"))]), tolerance_m = 150)
  }
  invisible(tbl)
}

register_country(list(
  code = "HU", iso3 = "HUN", name_en = "Hungary", name_local = "Magyarország", lang = "hu",
  levels = c("region"),
  level_labels = list(region = c("Counties", "Vármegyék", "County", "Vármegye")),
  source = list(name = "STADAT 11.1.2.1 Regisztrált bűncselekmények vármegye és régió szerint (KSH)", publisher = "Központi Statisztikai Hivatal (KSH), from BM / Legfőbb Ügyészség (ENyÜBS)",
                url = "https://www.ksh.hu/stadat_files/iga/en/iga0008.html", licence = "KSH terms of use: free re-use with the attribution 'Forrás: KSH' (not a Creative Commons licence)",
                measure = "Crimes registered in the unified police-prosecution statistics (regisztrált bűncselekmények), by county of commission, yearly, counted at the closure of the investigation; official total plus 6 selected offences."),
  population_source = "KSH STADAT 22.1.2.1 resident population on 1 January by county", boundaries_attribution = "© EuroGeographics for the administrative boundaries (Eurostat GISCO NUTS 2024)",
  not_published = c("Not published by KSH for this county and year", "A KSH erre a vármegyére és évre nem közöl adatot"),
  notes_en = "The national total includes crimes with unknown or foreign location (higher than the sum of the counties); statistics system change in 2018; drug abuse was counted under public nuisance until 2012; only 6 offence types are published by county.",
  method = list(where = "County (vármegye) where the crime was committed; unknown or foreign location in the national total only", date = "Year the investigation closed (registration in ENyÜBS), not the year of the event", revisions = "Yearly STADAT tables; earlier years updated when the prosecution service revises them; statistics system change in 2018"),
  default_region = "HU-BU",
  download = hu_download, ingest = hu_ingest, territory = hu_territory, population = hu_population
))
