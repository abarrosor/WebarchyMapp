# Netherlands: recorded crime by gemeente and provincie ------------------------------------
# Source: CBS StatLine 83648NED "Geregistreerde criminaliteit; soort misdrijf, regio" (CC BY 4.0):
# offences (misdrijven) registered by the police, yearly 2010 onwards, by gemeente of the
# reporting year, provincie, police regional unit and the whole country. 75 offence codes in a
# hierarchy (official total T001161, 8 main groups CRI1000..CRI9000, sub-groups below them).
# Counts are rounded to multiples of 5; "." (unknown / unreliable / secret) arrives as null.
# Population: CBS StatLine 70072ned "Regionale kerncijfers Nederland" (TotaleBevolking_1, 1 January,
# same RegioS codes, dissolved gemeenten included). Gemeente -> provincie: CBS "Gebieden in
# Nederland <year>" tables (one per year). Boundaries: CBS Gebiedsindelingen via PDOK WFS (CC BY 4.0).
# Everything is read through the CBS OData v3 services (JSON, no key). The ODataFeed has no row
# cap and pages with odata.nextLink; the loader also falls back to $skip in steps of 10,000.
#
# Written against the OData metadata (not run against the live files): the readers detect
# columns by name and fail with the header of the file. `nl_inspect()` prints what was downloaded.
# The monthly wijk/buurt table 47022NED (data.politie.nl) is NOT loaded here: see the notes.

NL_API  <- "https://opendata.cbs.nl/ODataApi/OData"      # metadata (small answers, 10,000-row cap)
NL_FEED <- "https://opendata.cbs.nl/ODataFeed/odata"     # data (no cap; pages with odata.nextLink)
NL_CRIME <- list(table = "83648NED", value = "TotaalGeregistreerdeMisdrijven_1")
NL_POP   <- list(table = "70072ned", value = "TotaleBevolking_1", since = 2010L)
# "Gebieden in Nederland" StatLine tables (gemeente codes/names + province of each gemeente), by year
NL_GEBIEDEN <- c(`2010` = "80397ned", `2011` = "81008ned", `2012` = "81498ned", `2013` = "82000NED", `2014` = "82496NED",
                 `2015` = "82949NED", `2016` = "83287NED", `2017` = "83553NED", `2018` = "83859NED", `2019` = "84378NED",
                 `2020` = "84721NED", `2021` = "84929NED", `2022` = "85067NED", `2023` = "85385NED", `2024` = "85755NED",
                 `2025` = "86059NED", `2026` = "86247NED", `2027` = "86438NED")
NL_BOUNDARY_YEAR <- 2025L   # CBS Gebiedsindelingen edition (= gemeente classification of the latest crime year)
NL_WFS <- paste0("https://service.pdok.nl/cbs/gebiedsindelingen/%d/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0",
                 "&typeName=gebiedsindelingen:%s_gegeneraliseerd&outputFormat=json&srsName=EPSG:4326&count=1000")
# Accepted column names (regex, first match wins)
NL_COLUMNS <- list(code = c("^SoortMisdrijf$"), region = c("^RegioS$", "^WijkenEnBuurten$"), period = c("^Perioden$"),
                   crime = c("^TotaalGeregistreerdeMisdrijven", "^GeregistreerdeMisdrijven_"), pop = c("^TotaleBevolking", "^BevolkingOp1Januari"))

nl_dir  <- function() COUNTRY_MODULES$NL$dir_raw
nl_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }
nl_json <- function(f) jsonlite::fromJSON(f, simplifyVector = TRUE)
nl_trim <- function(d) { for (cn in names(d)) if (is.character(d[[cn]])) d[[cn]] <- trimws(d[[cn]]); d }   # CBS pads codes with spaces
nl_newest <- function(pattern) { f <- newest_first(list.files(nl_dir(), pattern, full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

# OData URL of one table, one period, selected fields (JSON)
nl_feed_url <- function(table, period, select) sprintf("%s/%s/TypedDataSet?$format=json&$filter=Perioden%%20eq%%20'%s'&$select=%s", NL_FEED, table, period, select)

# Download all pages of an OData answer as <name>_p01.json, _p02.json ... (versioned copies)
nl_odata_pages <- function(url, name, page_size = 10000, dir = nl_dir()) {
  files <- character(); k <- 1L; skip <- 0L; u <- url
  repeat {
    f <- download_versioned(u, dir, name = sprintf("%s_p%02d.json", name, k)); files <- c(files, f)
    j <- nl_json(f); n <- NROW(j$value); skip <- skip + n
    nxt <- j[["odata.nextLink"]]
    if (!is.null(nxt) && nzchar(nxt[1])) u <- nxt[1] else if (n >= page_size) u <- paste0(url, "&$skip=", skip) else break
    k <- k + 1L; if (k > 300L) stop("Too many pages for ", name)
  }
  invisible(files)
}

# Newest copy of each page of a prefix (e.g. "83648NED_2025"), rows of all pages bound together
nl_pages <- function(prefix, dir = nl_dir()) {
  f <- list.files(dir, paste0("_", prefix, "_p[0-9]+\\.json$"), full.names = TRUE)
  if (!length(f)) return(character())
  k <- sub(".*_p([0-9]+)\\.json$", "\\1", f)
  unname(vapply(split(f, k), function(x) newest_first(x)[1], ""))
}
nl_read_pages <- function(prefix, dir = nl_dir()) {
  parts <- lapply(nl_pages(prefix, dir), function(f) { v <- nl_json(f)$value; if (NROW(v)) nl_trim(as.data.frame(v, stringsAsFactors = FALSE)) else NULL })
  parts <- parts[!vapply(parts, is.null, TRUE)]
  if (!length(parts)) return(NULL)
  d <- do.call(rbind, parts)
  if ("ID" %in% names(d)) d <- d[!duplicated(d$ID), , drop = FALSE]
  d
}
# Years for which pages of a table exist locally
nl_years_local <- function(table) sort(unique(as.integer(sub(paste0(".*_", table, "_([0-9]{4})_p[0-9]+\\.json$"), "\\1", list.files(nl_dir(), paste0("_", table, "_[0-9]{4}_p[0-9]+\\.json$"))))))

# Perioden metadata of a table: Key, Title, Status ("Definitief" / "Voorlopig")
nl_periods <- function(table) {
  f <- nl_newest(paste0("_", table, "_Perioden\\.json$")); if (is.na(f)) return(NULL)
  p <- nl_trim(as.data.frame(nl_json(f)$value, stringsAsFactors = FALSE)); p$year <- as.integer(substr(p$Key, 1, 4)); p
}

# Print what was downloaded (to adjust NL_COLUMNS or the $select fields if CBS renames something)
nl_inspect <- function() {
  f <- list.files(nl_dir(), "\\.json$", full.names = TRUE)
  if (!length(f)) { message("No NL file yet in ", nl_dir()); return(invisible(NULL)) }
  prefixes <- unique(sub("(_p[0-9]+)?\\.json$", "", sub("^[0-9]{8}(_[0-9]{6})?_", "", basename(f))))
  for (p in prefixes) {
    d <- if (length(nl_pages(p))) nl_read_pages(p) else { ff <- nl_newest(paste0("_", p, "\\.json$")); v <- if (is.na(ff)) NULL else nl_json(ff)$value; if (NROW(v)) nl_trim(as.data.frame(v, stringsAsFactors = FALSE)) else NULL }
    message(p, ": ", if (is.null(d)) "no rows" else paste0(fmt_int(nrow(d)), " rows\n  ", paste(names(d), collapse = " | ")))
    if (!is.null(d)) print(utils::head(d, 3))
  }
  g <- list.files(nl_dir(), "\\.geojson$", full.names = TRUE); if (length(g)) message("boundaries: ", paste(basename(g), collapse = ", "))
}

# --- Download -------------------------------------------------------------------------------------

nl_download <- function(days = 30) {
  dir <- nl_dir()
  recent <- function(pattern, d) { f <- nl_newest(pattern); !is.na(f) && file_age_days(f) < d }
  # 1. Crime table: metadata, then one request per year (final years already on disk are not asked again)
  if (recent(paste0("_", NL_CRIME$table, "_[0-9]{4}_p01\\.json$"), days)) message("= NL crime table downloaded less than ", days, " days ago") else {
    for (meta in c("TableInfos", "Perioden", "SoortMisdrijf", "RegioS"))
      download_versioned(sprintf("%s/%s/%s", NL_API, NL_CRIME$table, meta), dir, name = sprintf("%s_%s.json", NL_CRIME$table, meta))
    per <- nl_periods(NL_CRIME$table); have <- nl_years_local(NL_CRIME$table)
    for (i in seq_len(nrow(per))) {
      if (per$year[i] %in% have && identical(tolower(per$Status[i] %||% ""), "definitief")) next
      message("  ", NL_CRIME$table, " ", per$Title[i], " (", per$Status[i] %||% "?", ")")
      nl_odata_pages(nl_feed_url(NL_CRIME$table, per$Key[i], paste0("ID,SoortMisdrijf,RegioS,Perioden,", NL_CRIME$value)), paste0(NL_CRIME$table, "_", per$year[i]))
    }
  }
  # 2. Population by region, 1 January, one request per year (small answers)
  if (recent(paste0("_", NL_POP$table, "_[0-9]{4}_p01\\.json$"), 3 * days)) message("= NL population downloaded less than ", 3 * days, " days ago") else {
    download_versioned(sprintf("%s/%s/Perioden", NL_API, NL_POP$table), dir, name = sprintf("%s_Perioden.json", NL_POP$table))
    per <- nl_periods(NL_POP$table); per <- per[per$year >= NL_POP$since, ]
    for (i in seq_len(nrow(per)))
      nl_odata_pages(nl_feed_url(NL_POP$table, per$Key[i], paste0("ID,RegioS,Perioden,", NL_POP$value)), paste0(NL_POP$table, "_", per$year[i]))
  }
  # 3. "Gebieden in Nederland": gemeente -> provincie of every year with crime data (final tables: once)
  years <- nl_years_local(NL_CRIME$table); if (!length(years)) years <- 2010:as.integer(format(Sys.Date(), "%Y"))
  for (y in years) {
    id <- NL_GEBIEDEN[as.character(y)]
    if (is.na(id)) { message("  no 'Gebieden in Nederland ", y, "' table id in NL_GEBIEDEN (R/72_nl.R): add it from https://opendata.cbs.nl/ODataCatalog/Tables"); next }
    if (!is.na(nl_newest(sprintf("_gebieden_%d\\.json$", y)))) next
    download_versioned(sprintf("%s/%s/TypedDataSet", NL_API, id), dir, name = sprintf("gebieden_%d.json", y))
  }
  # 4. Boundaries (generalised gemeenten and provincies, GeoJSON WGS84), refreshed yearly
  for (lv in c("gemeente", "provincie")) {
    if (recent(sprintf("_%s_%d\\.geojson$", lv, NL_BOUNDARY_YEAR), 365)) next
    download_versioned(sprintf(NL_WFS, NL_BOUNDARY_YEAR, lv), dir, name = sprintf("%s_%d.geojson", lv, NL_BOUNDARY_YEAR))
  }
}

# --- Reference tables -----------------------------------------------------------------------------

nl_provinces <- function() read_reference("nl_provinces.csv")

# Gemeente -> provincie from the "Gebieden in Nederland" files: gemeente, province, name, year (last year seen).
# Old editions (2010-2012) use bare numeric codes ("1680", "22"): the columns are found by their values.
nl_gemeente_province <- function() {
  files <- list.files(nl_dir(), "_gebieden_[0-9]{4}\\.json$", full.names = TRUE)
  if (!length(files)) stop("No 'Gebieden in Nederland' file in ", nl_dir(), ": run the NL download first")
  years <- as.integer(sub(".*_gebieden_([0-9]{4})\\.json$", "\\1", files))
  out <- list()
  for (y in sort(unique(years))) {
    f <- newest_first(files[years == y])[1]
    d <- nl_json(f)$value; if (!NROW(d)) next
    d <- nl_trim(as.data.frame(d, stringsAsFactors = FALSE)); nm <- names(d)
    gm_col <- nl_pick(nm, c("^RegioS$", "^Code_1$")); if (is.na(gm_col)) stop(basename(f), ": no gemeente column in: ", paste(nm, collapse = " | "))
    gm <- toupper(d[[gm_col]]); gm <- ifelse(grepl("^[0-9]{1,4}$", gm), sprintf("GM%04d", as.integer(gm)), gm)
    # the gemeente name: the "Naam_" column right after the "Code_" column that repeats the gemeente code
    code_cols <- nm[grepl("^Code_", nm)]
    same <- code_cols[vapply(code_cols, function(cn) { v <- toupper(d[[cn]]); v <- ifelse(grepl("^[0-9]{1,4}$", v), sprintf("GM%04d", suppressWarnings(as.integer(v))), v); mean(v == gm, na.rm = TRUE) > 0.95 }, TRUE)]
    name_col <- if (length(same)) nm[match(same[1], nm) + 1] else nl_pick(nm, "^Naam_2$")
    # the province: the "Code_" column whose values are all PV20..PV31 (or 20..31)
    pv_col <- NA_character_
    for (cn in code_cols) {
      v <- d[[cn]]; v <- v[!is.na(v) & v != ""]; if (!length(v)) next
      if (all(grepl("^PV(2[0-9]|3[01])$", v)) || (all(grepl("^(2[0-9]|3[01])$", v)) && length(unique(v)) >= 10)) { pv_col <- cn; break }
    }
    if (is.na(pv_col)) { message("  gebieden ", y, ": no province column found (", basename(f), "), skipped"); next }
    pv <- d[[pv_col]]; pv <- ifelse(grepl("^[0-9]{2}$", pv), paste0("PV", pv), pv)
    ok <- grepl("^GM[0-9]{4}$", gm) & grepl("^PV[0-9]{2}$", pv)
    out[[as.character(y)]] <- data.frame(gemeente = gm[ok], province = pv[ok], name = if (!is.na(name_col)) d[[name_col]][ok] else gm[ok], year = y, stringsAsFactors = FALSE)
  }
  if (!length(out)) stop("No usable 'Gebieden in Nederland' file in ", nl_dir())
  all <- do.call(rbind, out)
  all <- all[order(all$gemeente, -all$year), ]
  all[!duplicated(all$gemeente), ]      # the most recent province / name of each gemeente code
}

# Names of the regions of the crime table (RegioS metadata): used for gemeenten absent from the Gebieden files
nl_region_names <- function() {
  f <- nl_newest(paste0("_", NL_CRIME$table, "_RegioS\\.json$")); if (is.na(f)) return(character())
  r <- nl_trim(as.data.frame(nl_json(f)$value, stringsAsFactors = FALSE))
  setNames(sub(" \\((PV|LD|RE)\\)$", "", r$Title), r$Key)
}

# territory_id of NL01 / PVxx / GMxxxx codes (NA for codes we do not map: LD, RE, PV99, GM0000, unknown province)
nl_ids <- function(region, gm2pv) {
  pv_of <- gm2pv$province[match(region, gm2pv$gemeente)]
  is_nl <- region == "NL01"; is_pv <- grepl("^PV(2[0-9]|3[01])$", region); is_gm <- grepl("^GM[0-9]{4}$", region) & region != "GM0000"
  data.frame(territory_id = ifelse(is_nl, "NL", ifelse(is_pv, paste0("NL-", region), ifelse(is_gm & !is.na(pv_of), paste("NL", pv_of, region, sep = "-"), NA_character_))),
             level = ifelse(is_nl, "country", ifelse(is_pv, "region", ifelse(is_gm, "municipality", NA_character_))),
             orphan = is_gm & is.na(pv_of), stringsAsFactors = FALSE)
}

# --- Ingest ---------------------------------------------------------------------------------------

nl_ingest <- function(con) {
  years <- nl_years_local(NL_CRIME$table)
  if (!length(years)) stop("No ", NL_CRIME$table, " file in ", nl_dir())
  gm2pv <- nl_gemeente_province(); names_meta <- nl_region_names()
  rows <- list()
  for (y in years) {
    d <- nl_read_pages(paste0(NL_CRIME$table, "_", y)); if (is.null(d)) next
    nm <- names(d); col <- lapply(NL_COLUMNS[c("code", "region", "period", "crime")], nl_pick, nm = nm)
    if (anyNA(unlist(col))) stop("Unexpected columns in ", NL_CRIME$table, " ", y, ": ", paste(nm, collapse = " | "), "\nRun nl_inspect() and adjust NL_COLUMNS / NL_CRIME$value in R/72_nl.R")
    rows[[as.character(y)]] <- data.frame(region = d[[col$region]], offence_code = d[[col$code]], year = as.integer(substr(d[[col$period]], 1, 4)),
                                          n = suppressWarnings(as.numeric(d[[col$crime]])), file = paste0(NL_CRIME$table, "_", y), stringsAsFactors = FALSE)
  }
  crime <- do.call(rbind, rows)
  ids <- nl_ids(crime$region, gm2pv)
  if (any(ids$orphan)) message(sprintf("  %d gemeente codes without a province in the Gebieden files, dropped: %s", length(unique(crime$region[ids$orphan])), paste(utils::head(unique(crime$region[ids$orphan]), 8), collapse = " ")))
  crime$territory_id <- ids$territory_id; crime$level <- ids$level
  crime <- crime[!is.na(crime$territory_id), ]
  # A gemeente x year with every category null did not exist that year (classification of the reporting year): drop
  alive <- crime |> dplyr::group_by(territory_id, year) |> dplyr::summarise(any = any(!is.na(n)), .groups = "drop")
  crime <- dplyr::inner_join(crime, alive[alive$any, c("territory_id", "year")], by = c("territory_id", "year"))
  # Gemeenten seen in the data but not created by nl_territory (e.g. an edition missing in NL_GEBIEDEN)
  gm <- unique(crime[crime$level == "municipality", c("territory_id", "region")])
  ensure_territories(con, "NL", gm$territory_id, "municipality", sub("-[^-]+$", "", gm$territory_id),
                     dplyr::coalesce(gm2pv$name[match(gm$region, gm2pv$gemeente)], unname(names_meta[gm$region]), gm$region))
  out <- yearly_to_cumulative(data.frame(country = "NL", territory_id = crime$territory_id, level = crime$level, offence_code = crime$offence_code,
                                         year = crime$year, n = crime$n, file = crime$file, stringsAsFactors = FALSE))
  replace_country_rows(con, "stg_country_crime", "NL", out[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_NL", paste(unique(out$file), collapse = ";"), NA, paste(range(out$year), collapse = "-"), nrow(out))
  message(sprintf("  crime: %s rows, %d territories (%d gemeenten), %d-%d; null cells (CBS '.'): %s", fmt_int(nrow(out)), length(unique(out$territory_id)),
                  sum(!duplicated(out$territory_id) & out$level == "municipality"), min(out$year), max(out$year), fmt_int(sum(is.na(out$cumulative)))))
  invisible(out)
}

# --- Population -----------------------------------------------------------------------------------

nl_population <- function(con) {
  years <- nl_years_local(NL_POP$table)
  if (!length(years)) stop("No ", NL_POP$table, " file in ", nl_dir())
  gm2pv <- nl_gemeente_province()
  rows <- list()
  for (y in years) {
    d <- nl_read_pages(paste0(NL_POP$table, "_", y)); if (is.null(d)) next
    nm <- names(d); col <- lapply(NL_COLUMNS[c("region", "period", "pop")], nl_pick, nm = nm)
    if (anyNA(unlist(col))) stop("Unexpected columns in ", NL_POP$table, " ", y, ": ", paste(nm, collapse = " | "), "\nRun nl_inspect() and adjust NL_COLUMNS / NL_POP$value in R/72_nl.R")
    rows[[as.character(y)]] <- data.frame(region = d[[col$region]], year = as.integer(substr(d[[col$period]], 1, 4)), population = suppressWarnings(as.numeric(d[[col$pop]])), stringsAsFactors = FALSE)
  }
  pop <- do.call(rbind, rows); ids <- nl_ids(pop$region, gm2pv)
  pop <- data.frame(country = "NL", territory_id = ids$territory_id, level = ids$level, year = pop$year, population = pop$population, stringsAsFactors = FALSE)
  pop <- pop[!is.na(pop$territory_id) & !is.na(pop$population), ]
  pop <- pop[!duplicated(pop[, c("territory_id", "year")]), ]
  replace_country_rows(con, "ref_country_population", "NL", pop)
  message(sprintf("  population: %d territories, %d-%d (CBS %s, 1 January)", length(unique(pop$territory_id)), min(pop$year), max(pop$year), NL_POP$table))
}

# --- Territories ----------------------------------------------------------------------------------

# Provincies from reference/nl_provinces.csv; gemeenten (current and dissolved since 2010) from the
# Gebieden files; polygons (current gemeenten and provincies) from the PDOK WFS GeoJSON
nl_territory <- function(con) {
  prov <- nl_provinces()
  tbl <- data.frame(territory_id = paste0("NL-", prov$province_code), level = "region", parent_id = "NL", official_code = prov$province_code, name = prov$province_name, stringsAsFactors = FALSE)
  gm <- tryCatch(nl_gemeente_province(), error = function(e) { message("  ", conditionMessage(e)); NULL })
  if (!is.null(gm)) {
    gm <- gm[gm$province %in% prov$province_code, ]
    tbl <- rbind(tbl, data.frame(territory_id = paste("NL", gm$province, gm$gemeente, sep = "-"), level = "municipality", parent_id = paste0("NL-", gm$province),
                                 official_code = gm$gemeente, name = gm$name, stringsAsFactors = FALSE))
  }
  save_country_territory(con, "NL", tbl, scheme = "CBS")
  layers <- list()
  read_layer <- function(lv) {
    f <- nl_newest(sprintf("_%s_%d\\.geojson$", lv, NL_BOUNDARY_YEAR)); if (is.na(f)) return(NULL)
    g <- sf::st_read(f, quiet = TRUE); nm <- names(g)
    code <- nl_pick(nm, c("^statcode$", "^gemeentecode$", "^provinciecode$", "^code$")); name <- nl_pick(nm, c("^statnaam$", "^gemeentenaam$", "^provincienaam$", "^naam$"))
    if (is.na(code)) { message("  ", basename(f), ": no code column (", paste(nm, collapse = " | "), ")"); return(NULL) }
    g$official_code <- trimws(as.character(g[[code]])); g$name <- if (!is.na(name)) as.character(g[[name]]) else g$official_code
    g
  }
  gp <- read_layer("provincie")
  if (!is.null(gp)) { gp$territory_id <- paste0("NL-", gp$official_code); gp <- gp[gp$official_code %in% prov$province_code, c("territory_id", "name", attr(gp, "sf_column"))]; layers$region <- gp }
  gg <- read_layer("gemeente")
  if (!is.null(gg) && !is.null(gm)) {
    gg$territory_id <- tbl$territory_id[match(gg$official_code, tbl$official_code)]
    miss <- is.na(gg$territory_id); if (any(miss)) message("  ", sum(miss), " polygons without a gemeente in the Gebieden files (skipped): ", paste(utils::head(gg$official_code[miss], 5), collapse = " "))
    layers$municipality <- gg[!miss, c("territory_id", "name", attr(gg, "sf_column"))]
  }
  if (length(layers)) save_country_gpkg(con, "NL", layers, tolerance_m = 50) else message("  no NL boundaries yet: tables only")
  invisible(tbl)
}

register_country(list(
  code = "NL", iso3 = "NLD", name_en = "Netherlands", name_local = "Nederland", lang = "nl",
  levels = c("region", "municipality"),
  level_labels = list(region = c("Provinces", "Provincies", "Province", "Provincie"), municipality = c("Municipalities", "Gemeenten", "Municipality", "Gemeente")),
  source = list(name = "Geregistreerde criminaliteit; soort misdrijf, regio (StatLine 83648NED)", publisher = "Centraal Bureau voor de Statistiek (CBS), from Politie data",
                url = "https://opendata.cbs.nl/#/CBS/nl/dataset/83648NED/table", licence = "CC BY 4.0",
                measure = "Offences (misdrijven) registered by the police in a proces-verbaal, by place of commission, yearly, attempts included; official total 'Misdrijven, totaal' (includes traffic offences); counts rounded to multiples of 5."),
  population_source = "CBS StatLine 70072ned Regionale kerncijfers Nederland (population on 1 January, CC BY 4.0)",
  boundaries_attribution = "© CBS / Kadaster, Gebiedsindelingen via PDOK (CC BY 4.0)",
  not_published = c("Not published: unknown, unreliable or secret (CBS symbol '.')", "Niet gepubliceerd: onbekend, onvoldoende betrouwbaar of geheim (CBS-teken '.')"),
  notes_en = "Yearly figures by gemeente of the reporting year (dissolved gemeenten keep their own years); counts rounded to multiples of 5; attempts count as offences (homicide figures are far above completed killings); series break in July 2018 (several offences per registration); online WhatsApp-fraud reporting from April 2020; 2024-2025 provisional, revised every March.",
  method = list(where = "Municipality where the offence was committed (pleegplaats), as registered by the police", date = "Year the offence was registered by the police (report filed)", revisions = "The two most recent years are provisional and revised each spring; dissolved municipalities keep their own years"),
  default_region = "NL-PV27",
  download = nl_download, ingest = nl_ingest, territory = nl_territory, population = nl_population
))
