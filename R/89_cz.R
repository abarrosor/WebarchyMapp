# Czechia: offences registered by the Police by kraj ---------------------------------------
# Source: ČSÚ (Czech Statistical Office) publication "Kriminalita - data za rok YYYY"
# (product 080009-YY), workbook "Všechny tabulky publikace" (XLSX, ~0.6 MB): 22 tables
# "<category> – počet registrovaných skutků v ČR podle krajů, 2015–2025" (counts, then per
# 100 000 inhabitants, then clearance rate in the same sheet; only the counts are read).
# Figures come from the Police Presidium (ESSK); offences are assigned to the kraj where the
# Police registered them. Licence: ČSÚ terms of use = CC BY 4.0. Yearly only (quarter = 4).
# Each edition covers the last 11 years; a new edition appears every June: add its URL to
# CZ_PRODUCTS (from https://csu.gov.cz/kriminalita > Publikace Kriminalita > Všechny tabulky).
# Population: Eurostat demo_r_pjangrp3 (kraje = NUTS 3). Boundaries: Eurostat GISCO NUTS 3.
# Written against the published table list and PDF layout, not run against the XLSX: the
# reader finds the title and header cells by text. `cz_inspect()` prints what it detects.

CZ_PRODUCTS <- c(
  "2025" = "https://csu.gov.cz/docs/107508/5662846a-b7e4-804a-3cc7-df86f19030bf/08000926t.xlsx?version=1.0"   # Kriminalita - data za rok 2025 (080009-26)
)
CZ_TITLE <- "registrovan[ýy]ch skutk[ůu].*podle kraj"      # tables of registered offences by kraj

cz_dir <- function() COUNTRY_MODULES$CZ$dir_raw
cz_regions <- function() read_reference("cz_regions.csv")
cz_files <- function() {                 # newest version of each edition
  f <- list.files(cz_dir(), "_cz_kriminalita_[0-9]{4}\\.xlsx$", full.names = TRUE)
  if (length(f) == 0) return(character())
  ed <- sub("^.*_cz_kriminalita_([0-9]{4})\\.xlsx$", "\\1", f)
  out <- unlist(lapply(split(f, ed), function(x) newest_first(x)[1])); out[order(names(out))]
}

cz_download <- function(days = 60) {
  for (ed in names(CZ_PRODUCTS)) {
    name <- sprintf("cz_kriminalita_%s.xlsx", ed)
    prev <- newest_first(list.files(cz_dir(), paste0("_", name, "$"), full.names = TRUE))
    if (length(prev) && difftime(Sys.time(), file.mtime(prev[1]), units = "days") < days) { message("= CZ ", ed, " workbook downloaded less than ", days, " days ago"); next }
    download_versioned(CZ_PRODUCTS[[ed]], cz_dir(), name = name)
  }
}

cz_number <- function(x) { x <- gsub("[^0-9]", "", gsub("\\s", "", x)); suppressWarnings(as.numeric(ifelse(nzchar(x), x, NA))) }   # "247 628" -> 247628; "-", ".", "i.d." -> NA

# Read one workbook: category, region code (NA = ČR), year, n
cz_read <- function(f, regs = cz_regions()) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is needed to read the ČSÚ workbook: install.packages('readxl')")
  out <- list()
  for (sh in readxl::excel_sheets(f)) {
    d <- as.data.frame(readxl::read_excel(f, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    if (nrow(d) == 0) next
    cells <- as.matrix(d); cells[is.na(cells)] <- ""
    title <- cells[grepl(CZ_TITLE, cells, ignore.case = TRUE) & !grepl("ob[ěe]t[íi]|st[íi]han|odsouzen|v[ěe]zn[ěe]n", cells, ignore.case = TRUE)]
    if (length(title) == 0) next
    category <- trimws(sub("\\s*[–-]\\s*po[čc]et.*$", "", sub("^Tab\\.?\\s*[0-9.]+\\s*", "", title[1])))
    hdr <- which(apply(cells, 1, function(r) any(grepl("^[Úu]zem[íi]$", trimws(r)))))[1]
    if (is.na(hdr)) { message("  sheet ", sh, ": title found but no 'Území' header row"); next }
    years <- suppressWarnings(as.integer(trimws(gsub("[^0-9]", "", cells[hdr, ]))))
    ycol <- which(!is.na(years) & years > 1990 & years < 2100)
    seen <- character(); rows <- list()
    for (i in seq(hdr + 1, nrow(cells))) {
      lab <- tolower(trimws(cells[i, 1])); if (!nzchar(lab)) lab <- tolower(trimws(cells[i, 2]))
      if (!nzchar(lab)) next
      code <- if (grepl("^[čc]r\\b|^[čc]esk[áa] republika|^celkem", lab)) "CZ" else { m <- which(vapply(regs$match_pattern, function(p) grepl(p, lab, perl = TRUE), logical(1))); if (length(m)) regs$code[m[1]] else NA }
      if (is.na(code)) next
      if (code %in% seen) break                                     # second block (per 100 000) starts: stop
      seen <- c(seen, code)
      rows[[length(rows) + 1]] <- data.frame(category = category, code = code, year = unname(years[ycol]), n = unname(cz_number(cells[i, ycol])), stringsAsFactors = FALSE)
    }
    if (length(rows)) out[[sh]] <- do.call(rbind, rows)
  }
  if (length(out) == 0) stop("No table '... počet registrovaných skutků ... podle krajů' found in ", basename(f), "\nRun cz_inspect() and adjust CZ_TITLE in R/89_cz.R")
  do.call(rbind, out)
}

cz_inspect <- function() {
  for (f in cz_files()) {
    message(basename(f), ": sheets ", paste(readxl::excel_sheets(f), collapse = " | "))
    d <- tryCatch(cz_read(f), error = function(e) { message(conditionMessage(e)); NULL })
    if (!is.null(d)) { message("  categories: ", paste(unique(d$category), collapse = " | ")); message("  years ", min(d$year, na.rm = TRUE), "-", max(d$year, na.rm = TRUE), ", regions ", length(unique(d$code))); print(utils::head(d, 5)) }
  }
}

cz_ingest <- function(con) {
  files <- cz_files(); if (length(files) == 0) stop("No ČSÚ workbook in ", cz_dir(), " (run the download first)")
  types <- read_reference("cz_offence_types.csv")
  parts <- list()
  for (f in files) { d <- cz_read(f); d$file <- basename(f); d$edition <- as.integer(sub("^.*_cz_kriminalita_([0-9]{4})\\.xlsx$", "\\1", f)); parts[[f]] <- d }
  d <- do.call(rbind, parts)
  # Category label -> source_code (match_pattern of the reference table)
  lab <- tolower(d$category); d$offence_code <- NA_character_
  for (i in seq_len(nrow(types))) { hit <- is.na(d$offence_code) & grepl(types$match_pattern[i], lab, perl = TRUE); d$offence_code[hit] <- types$source_code[i] }
  unknown <- unique(d$category[is.na(d$offence_code)])
  if (length(unknown)) message("  categories without a row in reference/cz_offence_types.csv (left out): ", paste(unknown, collapse = " | "))
  d <- d[!is.na(d$offence_code) & !is.na(d$year), ]
  d <- d[order(d$edition, decreasing = TRUE), ]; d <- d[!duplicated(d[, c("code", "offence_code", "year")]), ]   # newest edition wins
  crime <- data.frame(country = "CZ", territory_id = ifelse(d$code == "CZ", "CZ", paste0("CZ-", d$code)), level = ifelse(d$code == "CZ", "country", "region"),
                      offence_code = d$offence_code, year = d$year, n = d$n, file = d$file, stringsAsFactors = FALSE)
  crime <- yearly_to_cumulative(crime)
  replace_country_rows(con, "stg_country_crime", "CZ", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_CZ", paste(basename(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  %s rows, %d kraje, %d-%d, %d categories", fmt_int(nrow(crime)), length(unique(crime$territory_id[crime$level == "region"])), min(crime$year), max(crime$year), length(unique(crime$offence_code))))
  invisible(crime)
}

# Population on 1 January by kraj (= NUTS 3) and for Czechia, from Eurostat
cz_population <- function(con) {
  regs <- cz_regions()
  e <- eurostat_population_nuts3("CZ", since = 2014)
  e <- e[!is.na(e$value) & !is.na(e$year), ]
  code <- ifelse(e$geo == "CZ", "CZ", regs$code[match(e$geo, regs$nuts3)])
  pop <- data.frame(country = "CZ", territory_id = ifelse(code == "CZ", "CZ", paste0("CZ-", code)), level = ifelse(code == "CZ", "country", "region"), year = e$year, population = e$value)
  pop <- unique(pop[!is.na(code), ])
  if (nrow(pop) == 0) stop("No NUTS 3 population for CZ in the Eurostat answer")
  replace_country_rows(con, "ref_country_population", "CZ", pop)
  message(sprintf("  population: %d territories, %d-%d (Eurostat demo_r_pjangrp3)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

cz_territory <- function(con) {
  regs <- cz_regions()
  tbl <- data.frame(territory_id = paste0("CZ-", regs$code), level = "region", parent_id = "CZ", official_code = regs$nuts3, name = regs$name_cs)
  save_country_territory(con, "CZ", tbl, scheme = "NUTS 3")
  g <- tryCatch(gisco_nuts(3), error = function(e) { message("  GISCO NUTS not available: no boundaries (", conditionMessage(e), ")"); NULL })
  if (!is.null(g)) {
    g <- g[g$CNTR_CODE == "CZ", ]; g$territory_id <- paste0("CZ-", regs$code[match(g$NUTS_ID, regs$nuts3)]); g$name <- regs$name_cs[match(g$NUTS_ID, regs$nuts3)]
    g <- g[!is.na(g$name), c("territory_id", "name", attr(g, "sf_column"))]
    if (nrow(g) != 14) message("  [!!] ", nrow(g), " NUTS 3 polygons matched for CZ (expected 14): check the nuts3 codes in reference/cz_regions.csv")
    if (nrow(g)) save_country_gpkg(con, "CZ", list(region = g), tolerance_m = 150)
  }
  invisible(tbl)
}

register_country(list(
  code = "CZ", iso3 = "CZE", name_en = "Czechia", name_local = "Česko", lang = "cs",
  levels = c("region"),
  level_labels = list(region = c("Regions", "Kraje", "Region", "Kraj")),
  source = list(name = "Kriminalita - data za rok (ČSÚ, from Police Presidium ESSK)", publisher = "Český statistický úřad (ČSÚ)",
                url = "https://csu.gov.cz/kriminalita", licence = "CC BY 4.0 (ČSÚ terms of use)",
                measure = "Offences registered by the Police of the Czech Republic (registrované skutky), yearly, by the kraj of registration; 22 TSK categories with an official total."),
  population_source = "Eurostat demo_r_pjangrp3 (population on 1 January, NUTS 3 = kraje)", boundaries_attribution = "© EuroGeographics for the administrative boundaries (Eurostat GISCO NUTS 2024)",
  not_published = c("Not published by ČSÚ for this region and year", "ČSÚ tento údaj za kraj a rok nezveřejňuje"),
  notes_en = "Offences are counted where the Police registered them, not where they happened; the 2016 national total includes offences of the Foreign Police Directorate (higher than the sum of the kraje); the theft damage threshold doubled on 1 October 2020 (property crime fell by up to a quarter).",
  default_region = "CZ-10",
  download = cz_download, ingest = cz_ingest, territory = cz_territory, population = cz_population
))
