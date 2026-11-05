# Poland: offences recorded by the Police by powiat and województwo ------------------------
# Source: GUS, Bank Danych Lokalnych (BDL) REST API, CC BY 4.0, no key. Subject P3588
# "Przestępstwa stwierdzone przez Policję w zakończonych postępowaniach przygotowawczych
# wg powiatów (dane kwartalne)" (2016-2024, level 5 = powiaty) and P4601 (same, 2025 onwards).
# Each variable is a category x period; the periods are already cumulative from 1 January
# ("1 kwartał", "pierwsze półrocze", "1-3 kwartały", "rok") so they map straight to quarter 1-4.
# Województwa = sum of their powiaty; Poland = sum of the województwa.
# Population: BDL variable 72305 "ludność ogółem" (31 XII) at levels 0, 2 and 5.
# Boundaries: Eurostat GISCO LAU (gminy, TERYT codes) dissolved into powiaty and województwa.
#
# Anonymous API limits: 5 requests/s, 100 per 15 min, 1 000 per 12 h. The module waits
# PL_SLEEP seconds between calls (~100 calls per 15 min); the first download (~240 calls)
# takes about 40 minutes. Written against the API documentation (api.stat.gov.pl/Home/BdlApi),
# not run against the live service: the variable list and the unit ids are discovered at run
# time and checked against reference/pl_voivodeships.csv. `pl_inspect()` prints what was found.

PL_API <- "https://bdl.stat.gov.pl/api/v1"
PL_SUBJECTS <- c("P3588", "P4601")      # quarterly counts by powiat: 2016-2024 and 2025 onwards
PL_POP_VARIABLE <- 72305                # "ludność ogółem" (stan w dniu 31 XII)
PL_PAGE_SIZE <- 100                     # maximum of the API
PL_SLEEP <- 9.2                         # seconds between calls (anonymous limit: 100 per 15 min)
PL_STATE <- new.env()

pl_dir <- function() COUNTRY_MODULES$PL$dir_raw
pl_file <- function(name) { f <- newest_first(list.files(pl_dir(), paste0("_", gsub("([.()+])", "\\\\\\1", name), "$"), full.names = TRUE)); if (length(f)) f[1] else NA_character_ }
pl_recent <- function(name, days) { f <- pl_file(name); !is.na(f) && difftime(Sys.time(), file.mtime(f), units = "days") < days }

# One paced API call saved as a versioned JSON file (returns its path)
pl_get <- function(path, name) {
  last <- PL_STATE$last_call %||% 0
  wait <- PL_SLEEP - as.numeric(Sys.time()) + last
  if (wait > 0) Sys.sleep(wait)
  PL_STATE$last_call <- as.numeric(Sys.time())
  url <- paste0(PL_API, path, if (grepl("\\?", path)) "&" else "?", "format=json")
  download_versioned(url, pl_dir(), name = name)
}
pl_json <- function(f) jsonlite::fromJSON(f, simplifyVector = FALSE)

# All pages of a list endpoint (units, variables, data): one file per page
pl_get_pages <- function(path, name, days = 60) {
  page <- 0
  repeat {
    nm <- sprintf("%s_p%d.json", name, page)
    if (pl_recent(nm, days)) f <- pl_file(nm)
    else f <- pl_get(sprintf("%s%spage=%d&page-size=%d", path, if (grepl("\\?", path)) "&" else "?", page, PL_PAGE_SIZE), nm)
    j <- pl_json(f)
    total <- j$totalRecords %||% length(j$results)
    page <- page + 1
    if (page * PL_PAGE_SIZE >= total || length(j$results) == 0) break
  }
  invisible(page)
}
pl_pages <- function(name) {              # saved pages of one list, newest version of each
  f <- list.files(pl_dir(), paste0("_", name, "_p[0-9]+\\.json$"), full.names = TRUE)
  if (length(f) == 0) return(character())
  key <- sub("^[0-9_]+_", "", basename(f))
  unlist(lapply(split(f, key), function(x) newest_first(x)[1]))
}

# Variables (category x period) of the crime subjects, as a data frame: id, label, period
pl_variables <- function() {
  rows <- list()
  for (s in PL_SUBJECTS) for (f in pl_pages(paste0("pl_variables_", s))) for (v in pl_json(f)$results) {
    dims <- unlist(v[grep("^n[1-5]$", names(v))]); dims <- trimws(dims[!is.na(dims) & nzchar(dims)])
    is_period <- grepl("kwarta|p[oó][lł]rocz|^\\[?rok\\]?$|^rok\\b", dims, ignore.case = TRUE)
    period <- if (any(is_period)) dims[is_period][1] else "rok"
    rows[[length(rows) + 1]] <- data.frame(id = v$id, subject = s, label = paste(dims[!is_period], collapse = " - "), period = period,
                                           unit = v$measureUnitName %||% NA_character_, stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) return(data.frame(id = integer(), subject = character(), label = character(), period = character(), unit = character()))
  do.call(rbind, rows)
}
pl_quarter <- function(period) {
  p <- tolower(period)
  ifelse(grepl("p[oó][lł]rocz", p), 2L, ifelse(grepl("1\\s*[-–]\\s*3|trzy|3 kwarta|i-iii", p), 3L, ifelse(grepl("^1 kwarta|^i kwarta|pierwszy kwarta", p), 1L, 4L)))
}
# Category label -> source_code of reference/pl_offence_types.csv (match_pattern column, first match wins)
pl_source_code <- function(label, types) {
  lab <- tolower(gsub("^przest[eę]pstwa( stwierdzone( przez policj[eę])?( w zako[nń]czonych post[eę]powaniach przygotowawczych)?)?\\s*[-–:]?\\s*", "", tolower(label)))
  out <- rep(NA_character_, length(lab))
  for (i in seq_len(nrow(types))) { hit <- is.na(out) & grepl(types$match_pattern[i], lab, perl = TRUE); out[hit] <- types$source_code[i] }
  out
}

pl_download <- function(days = 60) {
  m <- COUNTRY_MODULES$PL
  # 1. Territorial units (names and hierarchy): województwa (level 2) and powiaty (level 5)
  pl_get_pages("/units?level=2", "pl_units_l2", days)
  pl_get_pages("/units?level=5", "pl_units_l5", days)
  # 2. Variables of the crime subjects
  for (s in PL_SUBJECTS) pl_get_pages(paste0("/variables?subject-id=", s), paste0("pl_variables_", s), days)
  vars <- pl_variables()
  if (nrow(vars) == 0) stop("No variables found for subjects ", toString(PL_SUBJECTS), " - check ", PL_API, "/subjects/P3588")
  message(sprintf("  %d variables (category x period) in %s", nrow(vars), toString(unique(vars$subject))))
  # 3. Data by variable, all years, powiaty (unit-level 5): ~4 pages per variable
  for (i in seq_len(nrow(vars))) pl_get_pages(sprintf("/data/by-variable/%s?unit-level=5", vars$id[i]), sprintf("pl_data_%s", vars$id[i]), days)
  # 4. Population at levels 0 (Poland), 2 and 5
  for (lv in c(0, 2, 5)) pl_get_pages(sprintf("/data/by-variable/%d?unit-level=%d", PL_POP_VARIABLE, lv), sprintf("pl_pop_l%d", lv), days)
  invisible(TRUE)
}

# BDL unit id (12 digits: macroregion 2, województwo TERYT 2, podregion 2, powiat TERYT 2, gmina 3, type 1)
pl_teryt_voiv <- function(id) substr(id, 3, 4)
# BDL unit id: 12 digits, województwo TERYT in digits 3-4 (verified against reference/pl_voivodeships.csv);
# the powiat TERYT is NOT recoverable from the id (two attempts gave 194 and 160 distinct codes), so the
# powiat identifier is the whole BDL unit id: PL-<woj>-<unit id>. Polygons are matched by name.
pl_teryt_powiat <- function(id) id
pl_ids <- function(unit_id, level) {
  v <- pl_teryt_voiv(unit_id)
  if (level == "region") paste0("PL-", v) else paste("PL", v, unit_id, sep = "-")
}
# Matching key of a powiat name: "Powiat m. st. Warszawa" / "Powiat bolesławiecki" -> "warszawa" / "boleslawiecki"
pl_name_key <- function(x) { k <- tolower(normalize_text(chartr("łŁ", "lL", x))); k <- gsub("^powiat\\s+", "", k); k <- gsub("\\b(m\\.?|st\\.?|miasto|city|county|powiat)\\b", " ", k); k <- gsub("[^a-z0-9 ]", "", k); trimws(gsub("\\s+", " ", k)) }

# Flatten the saved data pages of one variable: unit_id, unit_name, year, val, attr
pl_read_data <- function(name) {
  rows <- list()
  for (f in pl_pages(name)) for (u in pl_json(f)$results) for (v in u$values)
    rows[[length(rows) + 1]] <- data.frame(unit_id = u$id, unit_name = u$name, year = as.integer(v$year), val = if (is.null(v$val)) NA_real_ else as.numeric(v$val),
                                           attr = v$attrId %||% NA_integer_, file = basename(f), stringsAsFactors = FALSE)
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

pl_units <- function(level) {            # saved /units pages -> id, name, parent
  rows <- list()
  for (f in pl_pages(paste0("pl_units_l", level))) for (u in pl_json(f)$results) rows[[length(rows) + 1]] <- data.frame(id = u$id, name = u$name, parent = u$parentId %||% NA_character_, stringsAsFactors = FALSE)
  if (length(rows) == 0) NULL else do.call(rbind, rows)
}

pl_inspect <- function() {
  vars <- pl_variables(); types <- read_reference("pl_offence_types.csv")
  if (nrow(vars)) { vars$source_code <- pl_source_code(vars$label, types); vars$quarter <- pl_quarter(vars$period); print(vars) }
  else message("No variable list downloaded yet (", pl_dir(), ")")
  for (lv in c(2, 5)) { u <- pl_units(lv); message("units level ", lv, ": ", if (is.null(u)) 0 else nrow(u)); if (!is.null(u)) print(utils::head(u, 5)) }
  f <- pl_pages("pl_pop_l0"); if (length(f)) { j <- pl_json(f[1]); message("population variable: ", j$variableId %||% "?", " ", j$measureUnitName %||% "", " - ", j$results[[1]]$name %||% "") }
}

pl_ingest <- function(con) {
  types <- read_reference("pl_offence_types.csv")
  vars <- pl_variables()
  if (nrow(vars) == 0) stop("No BDL variable list in ", pl_dir(), " (run the download first)")
  vars$source_code <- pl_source_code(vars$label, types); vars$quarter <- pl_quarter(vars$period)
  unknown <- unique(vars$label[is.na(vars$source_code)])
  if (length(unknown)) message("  categories without a row in reference/pl_offence_types.csv (left out): ", paste(unknown, collapse = " | "))
  vars <- vars[!is.na(vars$source_code), ]
  rows <- list(); files <- character()
  for (i in seq_len(nrow(vars))) {
    d <- pl_read_data(sprintf("pl_data_%s", vars$id[i])); if (is.null(d)) next
    d <- d[nchar(d$unit_id) == 12, ]
    rows[[length(rows) + 1]] <- data.frame(country = "PL", territory_id = pl_ids(d$unit_id, "province"), level = "province", offence_code = vars$source_code[i],
                                           year = d$year, quarter = vars$quarter[i], cumulative = d$val, file = d$file, unit_name = d$unit_name,
                                           rank = match(vars$subject[i], PL_SUBJECTS), stringsAsFactors = FALSE)
    files <- c(files, unique(d$file))
  }
  if (length(rows) == 0) stop("No BDL data pages found in ", pl_dir(), " (run pl_inspect())")
  crime <- do.call(rbind, rows)
  # The two subjects may overlap in a year: keep one figure per key (the later subject wins)
  crime <- crime[order(crime$rank, decreasing = TRUE), ]; crime <- crime[!duplicated(crime[, c("territory_id", "offence_code", "year", "quarter")]), ]
  ensure_territories(con, "PL", crime$territory_id, "province", sub("-[^-]+$", "", crime$territory_id), crime$unit_name)
  crime$unit_name <- NULL; crime$rank <- NULL
  sum_up <- function(df, id_fun, level) {
    df$territory_id <- id_fun(df$territory_id)
    out <- df |> dplyr::group_by(country, territory_id, offence_code, year, quarter) |>
      dplyr::summarise(cumulative = if (any(is.na(cumulative))) NA_real_ else sum(cumulative), file = file[1], .groups = "drop")
    out$level <- level; out[, names(df)]
  }
  reg <- sum_up(crime, function(x) sub("-[^-]+$", "", x), "region")          # województwa = sum of their powiaty
  nat <- sum_up(reg, function(x) "PL", "country")                               # Poland = sum of the województwa
  crime <- rbind(crime, reg, nat)
  n_pow <- length(unique(crime$territory_id[crime$level == "province"])); n_voi <- length(unique(crime$territory_id[crime$level == "region"]))
  if (n_pow < 370 || n_pow > 390 || n_voi != 16) message(sprintf("  [!!] %d powiaty and %d województwa found (expected ~380 and 16): run pl_inspect()", n_pow, n_voi))
  replace_country_rows(con, "stg_country_crime", "PL", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_PL", paste(unique(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  %s rows, %d powiaty, %d-%d, categories: %s", fmt_int(nrow(crime)), n_pow, min(crime$year), max(crime$year), paste(unique(crime$offence_code), collapse = ", ")))
  invisible(crime)
}

pl_population <- function(con) {
  parts <- list()
  for (lv in c(0, 2, 5)) {
    d <- pl_read_data(sprintf("pl_pop_l%d", lv)); if (is.null(d)) next
    level <- c("0" = "country", "2" = "region", "5" = "province")[[as.character(lv)]]
    tid <- if (lv == 0) "PL" else pl_ids(d$unit_id, level)
    parts[[length(parts) + 1]] <- data.frame(country = "PL", territory_id = tid, level = level, year = d$year, population = d$val)
  }
  if (length(parts) == 0) stop("No population pages (variable ", PL_POP_VARIABLE, ") in ", pl_dir())
  pop <- do.call(rbind, parts); pop <- pop[!is.na(pop$population), ]
  f <- pl_pages("pl_pop_l0"); if (length(f)) { j <- pl_json(f[1]); if (!is.null(j$measureUnitName) && !grepl("osob", j$measureUnitName, ignore.case = TRUE)) message("  [!!] population unit is '", j$measureUnitName, "': check PL_POP_VARIABLE (expected 'ludność ogółem' in osoba)") }
  replace_country_rows(con, "ref_country_population", "PL", pop)
  message(sprintf("  population: %d territories, %d-%d (GUS BDL variable %d)", length(unique(pop$territory_id)), min(pop$year), max(pop$year), PL_POP_VARIABLE))
}

# Territories: names from the API units; polygons from GISCO LAU (gminy) dissolved by TERYT
pl_territory <- function(con) {
  voiv <- read_reference("pl_voivodeships.csv")
  u2 <- pl_units(2); u5 <- pl_units(5)
  regs <- data.frame(territory_id = paste0("PL-", voiv$teryt), level = "region", parent_id = "PL", official_code = voiv$teryt, name = voiv$name_pl)
  if (!is.null(u2)) {                   # check the id scheme against the reference names
    api <- data.frame(teryt = pl_teryt_voiv(u2$id), name = u2$name)
    bad <- api[!toupper(api$name) %in% toupper(voiv$name_pl[match(api$teryt, voiv$teryt)]), ]
    if (nrow(bad)) message("  [!!] API województwa whose TERYT (digits 3-4 of the unit id) does not match reference/pl_voivodeships.csv: ", paste(bad$teryt, bad$name, collapse = "; "))
  }
  tbl <- regs
  if (!is.null(u5)) {
    pow <- data.frame(territory_id = pl_ids(u5$id, "province"), level = "province", parent_id = paste0("PL-", pl_teryt_voiv(u5$id)), official_code = pl_teryt_powiat(u5$id), name = u5$name)
    tbl <- rbind(tbl, pow[!duplicated(pow$territory_id), ])
  } else message("  no /units level 5 file yet: powiaty will be added from the data without names")
  layers <- list()
  # Boundaries: geoBoundaries ADM1 (województwa) and ADM2 (powiaty, CC BY 4.0), matched by name inside the województwo
  gb <- function(adm) tryCatch(geoboundaries("POL", adm), error = function(e) { message("  geoBoundaries ADM", adm, " not available (", conditionMessage(e), ")"); NULL })
  g1 <- gb(1)
  if (!is.null(g1)) {
    # geoBoundaries names the województwa in English ("Masovian Voivodeship"): match the Polish and the English names
    vk <- function(x) gsub("\\b(voivodeship|province|region)\\b", "", pl_name_key(x)); vk <- function(x) trimws(gsub("\\s+", " ", vk_raw(x)))
    vk_raw <- function(x) gsub("\\b(voivodeship|province|region)\\b", "", pl_name_key(x))
    key1 <- vk(g1$shapeName)
    g1$teryt <- voiv$teryt[match(key1, vk(voiv$name_pl))]; miss <- is.na(g1$teryt); if (any(miss) && "name_en" %in% names(voiv)) g1$teryt[miss] <- voiv$teryt[match(key1[miss], vk(voiv$name_en))]
    if (any(is.na(g1$teryt))) message("  województwa not matched to geoBoundaries ADM1: ", paste(g1$shapeName[is.na(g1$teryt)], collapse = ", "))
    g1 <- g1[!is.na(g1$teryt), ]
    if (nrow(g1) == 0) g1 <- NULL
  }
  if (!is.null(g1)) {
    g1$territory_id <- paste0("PL-", g1$teryt); g1$name <- voiv$name_pl[match(g1$teryt, voiv$teryt)]
    layers$region <- g1[, c("territory_id", "name", attr(g1, "sf_column"))]
    g2 <- gb(2)
    if (!is.null(g2) && !is.null(u5)) {
      g2 <- sf::st_make_valid(sf::st_transform(g2, 4326)); r1 <- sf::st_transform(layers$region, 4326)
      inside <- sf::st_within(suppressWarnings(sf::st_point_on_surface(g2)), r1, sparse = TRUE)
      g2$teryt <- vapply(inside, function(i) if (length(i)) r1$territory_id[i[1]] else NA_character_, character(1))
      pow <- tbl[tbl$level == "province", ]
      g2$territory_id <- pow$territory_id[match(paste(g2$teryt, pl_name_key(g2$shapeName)), paste(pow$parent_id, pl_name_key(pow$name)))]
      # "Powiat m. Wrocław" vs the land powiat "wrocławski": a city powiat name equals the city; try the land name as a second pass
      miss <- is.na(g2$territory_id)
      if (any(miss)) g2$territory_id[miss] <- pow$territory_id[match(paste(g2$teryt[miss], pl_name_key(g2$shapeName[miss])), paste(pow$parent_id, gsub("ski$|cki$|zki$", "", pl_name_key(pow$name))))]
      g2 <- g2[!is.na(g2$territory_id) & !duplicated(g2$territory_id), ]
      if (nrow(g2)) { g2$name <- pow$name[match(g2$territory_id, pow$territory_id)]; layers$province <- g2[, c("territory_id", "name", attr(g2, "sf_column"))] }
      message(sprintf("  boundaries: geoBoundaries (CC BY 4.0), %d of %d powiaty matched by name%s", nrow(g2), nrow(pow), if (nrow(g2) < nrow(pow) * 0.5) paste0(" (e.g. ADM2 names: ", paste(utils::head(gb(2)$shapeName, 4), collapse = ", "), ")") else ""))
    }
  }
  lau <- NULL   # GISCO LAU is not used for Poland (LAU_ID values are not TERYT codes in the 2023 file)
  if (!is.null(lau)) {
    code <- gsub("[^0-9]", "", lau$LAU_ID); code <- ifelse(nchar(code) == 6, paste0("0", code), code)   # a lost leading zero
    ok <- nchar(code) == 7 & substr(code, 1, 2) %in% sprintf("%02d", as.integer(voiv$teryt))
    if (sum(ok) < 2000) message("  [!!] LAU_ID does not look like 7-digit TERYT codes (", sum(ok), " usable; e.g. ", paste(utils::head(lau$LAU_ID, 3), collapse = ", "), "): powiat polygons skipped")
    else {
      g <- lau[ok, ]; g$territory_id <- paste("PL", substr(code[ok], 1, 2), substr(code[ok], 1, 4), sep = "-")
      message("  dissolving gminy into powiaty and województwa (a minute or two)")
      gp <- g[, c("territory_id", attr(g, "sf_column"))] |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop")
      gp$name <- tbl$name[match(gp$territory_id, tbl$territory_id)]; gp$name[is.na(gp$name)] <- sub("^.*-", "", gp$territory_id[is.na(gp$name)])
      gr <- gp; gr$territory_id <- sub("-[^-]+$", "", gr$territory_id); gr <- gr[, c("territory_id", attr(gr, "sf_column"))] |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop")
      gr$name <- voiv$name_pl[match(sub("^PL-", "", gr$territory_id), voiv$teryt)]
      # powiaty seen in the polygons but not in the API units: add them without names
      extra <- setdiff(gp$territory_id, tbl$territory_id)
      if (length(extra)) tbl <- rbind(tbl, data.frame(territory_id = extra, level = "province", parent_id = sub("-[^-]+$", "", extra), official_code = sub("^.*-", "", extra), name = sub("^.*-", "", extra)))
      layers <- list(region = gr, province = gp)
    }
  }
  save_country_territory(con, "PL", tbl, scheme = "TERYT")
  if (length(layers)) save_country_gpkg(con, "PL", layers, tolerance_m = 150)
  invisible(tbl)
}

register_country(list(
  code = "PL", iso3 = "POL", name_en = "Poland", name_local = "Polska", lang = "pl",
  levels = c("region", "province"),
  level_labels = list(region = c("Voivodeships", "Województwa", "Voivodeship", "Województwo"), province = c("Counties", "Powiaty", "County", "Powiat")),
  source = list(name = "Bank Danych Lokalnych - przestępstwa stwierdzone przez Policję (GUS)", publisher = "Główny Urząd Statystyczny (GUS)",
                url = "https://bdl.stat.gov.pl/", licence = "CC BY 4.0",
                measure = "Offences confirmed by the Police in completed preparatory proceedings (przestępstwa stwierdzone), counted at the closure of the proceeding, cumulative by quarter, by powiat."),
  population_source = "GUS Bank Danych Lokalnych (ludność ogółem, 31 XII)", boundaries_attribution = "© EuroGeographics for the administrative boundaries (Eurostat GISCO LAU, dissolved into powiaty)",
  not_published = c("Not published by GUS for this unit and period", "Brak danych GUS dla tej jednostki i okresu"),
  notes_en = "Offences are counted when the preparatory proceeding closes, not when they are reported. Two classifications: 2016-2024 by Criminal Code chapter (life and health, property, family...), from 2025 a list of selected offences (homicide, rape, robbery, theft, burglary, car theft, corruption, cybercrime, other) that does not add up to the total; the official total (ogółem) exists in both. Detection-rate indicators are not loaded.",
  default_region = "PL-14",
  download = pl_download, ingest = pl_ingest, territory = pl_territory, population = pl_population
))
