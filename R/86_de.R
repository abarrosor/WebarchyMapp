# Germany: police-recorded offences by Land and Kreis ---------------------------------------
# Source: Bundeskriminalamt (BKA), Polizeiliche Kriminalstatistik (PKS), yearly table sets
# "Fälle - Länder, Kreise, Städte": the Länder Grundtabelle (all Straftatenschlüssel x 16 Länder)
# and the Kreise Grundtabelle (selected offence keys x ~400 Kreise / kreisfreie Städte, with the
# AGS key). One CSV (or XLSX) per reporting year, Datenlizenz Deutschland - Namensnennung 2.0.
# Measure kept: "erfasste Fälle" (offences recorded, incl. attempts), counted at the place of the
# offence (Tatort). No municipal level nationwide (the BKA city table covers only the ~80 cities
# with 100,000+ inhabitants; Berlin publishes Bezirksregionen separately: see modules/de.md).
#
# The yearly URLs live in reference/de_sources.csv (year, level, format, url, verified): bka.de
# could not be read reliably while this module was written, so only the 2018 URLs are verified
# and the others follow the later naming pattern. `de_check_sources()` tests every URL;
# `de_inspect()` prints the header of each downloaded file. The reader finds columns by name.
# Population: the "Einwohner" column of the BKA files when present; otherwise Eurostat
# demo_r_pjangrp3 (1 January, NUTS 1 = Länder, NUTS 3 = Kreise) joined through the NUTS code
# carried by the BKG VG250 boundaries (dl-de/by-2-0). Destatis GENESIS needs a login: not used.

DE_STATE <- new.env()
DE_VG250 <- "https://daten.gdz.bkg.bund.de/produkte/vg/vg250_ebenen_0101/aktuell/vg250_01-01.utm32s.gpkg.ebenen.zip"   # ~67 MB
DE_EUROSTAT <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/demo_r_pjangrp3?format=JSON&lang=EN&sex=T&age=TOTAL&unit=NR&sinceTimePeriod=2014&geoLevel=%s"
# Accepted column names (first match wins, case-insensitive)
DE_COLUMNS <- list(
  key    = c("^Schl[üu]e?ssel$", "^Straftatenschl", "^Schl"),
  label  = c("^Straftat$", "^Straftat", "^Bezeichnung"),
  ags    = c("^Gemeindeschl", "^Kreisschl", "^AGS$", "^Schl[üu]e?ssel Kreis", "^Regionalschl"),
  name   = c("^Stadt-/Landkreis", "^Kreis$", "^Kreis ", "^Stadt", "^Bundesland$", "^Land$", "^Gebiet"),
  value  = c("^Anzahl erfasste F[äa]e?lle$", "^erfasste F[äa]e?lle$", "^F[äa]e?lle$", "^erfasste F[äa]e?lle", "^Anzahl F[äa]e?lle"),   # "Faelle" (ASCII) in the 2018 CSV
  pop    = c("^Einwohner", "^Bev[öo]lkerung", "^EWZ$")
)

de_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }
de_sources <- function() { s <- read_reference("de_sources.csv"); s$year <- as.integer(s$year); s$format <- tolower(s$format %||% "csv"); s }
de_laender <- function() read_reference("de_laender.csv")
de_file <- function(level, year) {
  f <- newest_first(list.files(COUNTRY_MODULES$DE$dir_raw, sprintf("_de_%s_%d\\.(csv|xlsx)$", level, year), full.names = TRUE))
  if (length(f)) f[1] else NA_character_
}

de_download <- function(days = 90) {
  m <- COUNTRY_MODULES$DE; src <- de_sources(); this_year <- as.integer(format(Sys.Date(), "%Y"))
  for (i in seq_len(nrow(src))) {
    prev <- de_file(src$level[i], src$year[i])
    # yearly tables do not change once published: re-download only the two latest editions, every `days`
    if (!is.na(prev) && (src$year[i] < this_year - 2 || file_age_days(prev) < days)) next
    name <- sprintf("de_%s_%d.%s", src$level[i], src$year[i], src$format[i])
    tryCatch(download_versioned(src$url[i], m$dir_raw, name = name),
             error = function(e) message("  PKS ", src$year[i], " ", src$level[i], " not downloaded (", conditionMessage(e), "): check the URL in reference/de_sources.csv"))
    Sys.sleep(1)
  }
  if (is.na(de_vg250_zip())) tryCatch(download_versioned(DE_VG250, m$dir_raw), error = function(e) message("  BKG VG250 not available (", conditionMessage(e), "); GISCO NUTS will be used"))
  for (lv in c("nuts1", "nuts3")) {   # Eurostat population (small JSON files), refreshed every `days`
    prev <- newest_first(list.files(m$dir_raw, sprintf("_de_pop_%s\\.json$", lv), full.names = TRUE))
    if (length(prev) && file_age_days(prev[1]) < days) next
    tryCatch(download_versioned(sprintf(DE_EUROSTAT, lv), m$dir_raw, name = sprintf("de_pop_%s.json", lv)), error = function(e) message("  Eurostat population not available (", conditionMessage(e), ")"))
  }
}

# HTTP status of every URL in reference/de_sources.csv (run this first if downloads fail)
de_check_sources <- function() {
  src <- de_sources()
  for (i in seq_len(nrow(src))) {
    st <- tryCatch(httr2::request(src$url[i]) |> httr2::req_method("HEAD") |> httr2::req_user_agent(USER_AGENT) |> httr2::req_timeout(30) |>
                     httr2::req_error(is_error = function(r) FALSE) |> httr2::req_perform() |> httr2::resp_status(), error = function(e) conditionMessage(e))
    message(sprintf("%d %-5s %-4s -> %s", src$year[i], src$level[i], if (isTRUE(src$verified[i] == "yes")) "ok" else "?", st))
  }
}

# Read a BKA table: CSV (semicolon, several title rows before the header, Windows-1252 or UTF-8) or XLSX
de_read <- function(f) {
  if (grepl("\\.xlsx?$", f)) {
    sheets <- readxl::excel_sheets(f)
    for (sh in sheets) {
      raw <- readxl::read_excel(f, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal")
      h <- which(apply(raw, 1, function(r) any(grepl("^Schl", r)) && any(grepl("Straftat", r))))[1]
      if (!is.na(h)) { d <- raw[-seq_len(h), ]; names(d) <- trimws(as.character(unlist(raw[h, ]))); return(as.data.frame(d[, nzchar(names(d)) & !is.na(names(d))])) }
    }
    stop("No header row with 'Schlüssel' and 'Straftat' in ", basename(f), " (sheets: ", toString(sheets), ")")
  }
  lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
  if (!all(validUTF8(lines))) lines <- iconv(readLines(f, warn = FALSE), from = "windows-1252", to = "UTF-8")
  lines[1] <- sub("^﻿", "", lines[1])
  h <- grep("Schl.*Straftat|Straftat.*Schl", lines)[1]
  if (is.na(h)) stop("No header row with 'Schlüssel' and 'Straftat' in ", basename(f), "; first lines:\n", paste(utils::head(lines, 5), collapse = "\n"))
  sep <- if (lengths(regmatches(lines[h], gregexpr(";", lines[h]))) >= lengths(regmatches(lines[h], gregexpr(",", lines[h])))) ";" else ","
  d <- readr::read_delim(I(paste(lines[h:length(lines)], collapse = "\n")), delim = sep, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE, name_repair = "minimal")
  names(d) <- trimws(names(d)); as.data.frame(d[, nzchar(names(d))])
}

# Print the header of each downloaded table (to adjust DE_COLUMNS) and the keys the mapping lacks
de_inspect <- function() {
  for (f in newest_first(list.files(COUNTRY_MODULES$DE$dir_raw, "_de_(land|kreis)_\\d{4}\\.(csv|xlsx)$", full.names = TRUE))) {
    d <- tryCatch(de_read(f), error = function(e) { message(basename(f), ": ", conditionMessage(e)); NULL }); if (is.null(d)) next
    message(basename(f), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(d, 3))
  }
  k <- newest_first(list.files(COUNTRY_MODULES$DE$dir_raw, "_de_kreis_\\d{4}\\.(csv|xlsx)$", full.names = TRUE))
  if (length(k)) { d <- de_standardise(de_read(k[1]), "kreis"); miss <- unique(d[!d$key %in% read_reference("de_offence_types.csv")$source_code, c("key", "label")])
    if (nrow(miss)) { message("Keys in ", basename(k[1]), " without a row in reference/de_offence_types.csv (add them if wanted):"); print(miss, row.names = FALSE) } }
}

# Standardise one table to: key, label, code (AGS or Land name), name, value, pop
de_standardise <- function(d, level) {
  nm <- names(d); col <- lapply(DE_COLUMNS, de_pick, nm = nm)
  if (level == "land" && is.na(col$name)) col$name <- de_pick(nm, c("^Land", "^Bundesland", "^Gebiet"))
  if (is.na(col$key) || is.na(col$value) || (level == "kreis" && is.na(col$ags)) || (level == "land" && is.na(col$name) && is.na(col$ags)))
    stop("Unexpected columns in the German ", level, " table: ", paste(nm, collapse = " | "), "\nRun de_inspect() and adjust DE_COLUMNS in R/86_de.R")
  out <- data.frame(key = trimws(d[[col$key]]), label = if (!is.na(col$label)) trimws(d[[col$label]]) else NA_character_,
                    value = parse_es_number(d[[col$value]]), stringsAsFactors = FALSE)
  out$code <- if (!is.na(col$ags)) gsub("[^0-9]", "", d[[col$ags]]) else NA_character_
  out$name <- if (!is.na(col$name)) trimws(d[[col$name]]) else NA_character_
  out$pop <- if (!is.na(col$pop)) parse_es_number(d[[col$pop]]) else NA_real_
  out$key <- gsub("\\s", "", out$key)
  out[!is.na(out$key) & nzchar(out$key), ]
}

# Identifiers: DE-<Land AGS 2>-<Kreis AGS 5>
de_land_of <- function(x, laender) {        # Land AGS from a name ("Bayern") or a code ("09", "9", "09000")
  by_name <- laender$ags[match(normalize_text(x), normalize_text(laender$name))]
  num <- suppressWarnings(as.integer(substr(gsub("[^0-9]", "", x), 1, 2)))
  by_code <- ifelse(!is.na(num) & num >= 1 & num <= 16 & grepl("^[0-9]+$", x), sprintf("%02d", num), NA_character_)
  dplyr::coalesce(by_name, by_code)
}

de_ingest <- function(con) {
  laender <- de_laender(); src <- de_sources(); rows <- list(); pops <- list(); files <- character()
  for (y in sort(unique(src$year))) {
    for (lv in c("land", "kreis")) {
      f <- de_file(lv, y); if (is.na(f)) next
      d <- de_standardise(de_read(f), lv); files <- c(files, basename(f))
      if (lv == "land") {
        national <- normalize_text(d$name) %in% c("BUND", "BUNDESREPUBLIK DEUTSCHLAND", "DEUTSCHLAND", "BUNDESGEBIET INSGESAMT", "INSGESAMT") | (!is.na(d$code) & d$code %in% c("0", "00", "00000"))
        land <- de_land_of(ifelse(!is.na(d$code) & nzchar(d$code), d$code, d$name), laender)
        d$territory_id <- ifelse(national, "DE", ifelse(is.na(land), NA, paste0("DE-", land)))
        d$level <- ifelse(national, "country", "region")
      } else {
        ags <- sprintf("%05d", suppressWarnings(as.integer(d$code))); ags[is.na(d$code) | !nzchar(d$code)] <- NA
        land_row <- !is.na(ags) & substr(ags, 3, 5) == "000" & !ags %in% c("02000", "11000")   # Land subtotal rows, if any: Länder come from the Länder table
        d$territory_id <- ifelse(is.na(ags) | land_row, NA, paste("DE", substr(ags, 1, 2), ags, sep = "-"))
        d$level <- "province"
        if (any(land_row)) message(sprintf("  %d: %d Land rows in the Kreise table ignored", y, length(unique(ags[land_row]))))
        ensure_territories(con, "DE", d$territory_id[!is.na(d$territory_id)], "province", paste0("DE-", substr(ags[!is.na(d$territory_id)], 1, 2)), d$name[!is.na(d$territory_id)])
      }
      bad <- is.na(d$territory_id)
      if (any(bad)) message(sprintf("  %d %s: %d rows without a recognised territory dropped (%s)", y, lv, sum(bad), toString(utils::head(unique(d$name[bad]), 5))))
      d <- d[!bad, ]
      rows[[paste(lv, y)]] <- data.frame(country = "DE", territory_id = d$territory_id, level = d$level, offence_code = d$key, year = y, n = d$value, file = basename(f), stringsAsFactors = FALSE)
      p <- unique(d[!is.na(d$pop) & d$key == "------", c("territory_id", "level", "pop")])
      if (nrow(p)) pops[[paste(lv, y)]] <- data.frame(country = "DE", territory_id = p$territory_id, level = p$level, year = y, population = p$pop)
      message(sprintf("  %d %s: %s rows, %d territories, %d keys", y, lv, fmt_int(nrow(d)), length(unique(d$territory_id)), length(unique(d$key))))
    }
  }
  if (length(rows) == 0) stop("No PKS table found in ", COUNTRY_MODULES$DE$dir_raw, " (run de_check_sources() and fix reference/de_sources.csv)")
  crime <- do.call(rbind, rows)
  crime <- crime[!duplicated(crime[, c("territory_id", "offence_code", "year")]), ]
  crime <- yearly_to_cumulative(crime)
  # National row: from the Länder table when it has one, else the sum of the Länder (or of the Kreise)
  for (y in unique(crime$year)) {
    if (any(crime$year == y & crime$level == "country")) next
    base <- crime[crime$year == y & crime$level == if (any(crime$year == y & crime$level == "region")) "region" else "province", ]
    nat <- base |> dplyr::group_by(country, offence_code, year, quarter, file) |> dplyr::summarise(cumulative = if (all(is.na(cumulative))) NA_real_ else sum(cumulative, na.rm = TRUE), .groups = "drop")
    nat$territory_id <- "DE"; nat$level <- "country"
    crime <- rbind(crime, nat[, names(crime)])
  }
  crime <- crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")]
  replace_country_rows(con, "stg_country_crime", "DE", crime)
  log_ingest(con, "country_DE", paste(unique(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  DE_STATE$pop_in_files <- if (length(pops)) do.call(rbind, pops) else NULL
  invisible(crime)
}

# Population: Einwohner column of the BKA files if present, else Eurostat NUTS 1 / NUTS 3
de_population <- function(con) {
  if (is.null(DE_STATE$pop_in_files) && !"DE" %in% DBI::dbGetQuery(con, "SELECT DISTINCT country FROM stg_country_crime")$country) de_ingest(con)
  pop <- DE_STATE$pop_in_files
  if (!is.null(pop) && nrow(pop) > 0) {
    message("  population: 'Einwohner' column of the BKA tables")
  } else {
    laender <- de_laender(); nuts <- de_nuts_crosswalk(con)
    read_pop <- function(lv) {
      f <- newest_first(list.files(COUNTRY_MODULES$DE$dir_raw, sprintf("_de_pop_%s\\.json$", lv), full.names = TRUE))
      if (length(f) == 0) return(NULL)
      d <- jsonstat_to_df(jsonlite::fromJSON(f[1], simplifyVector = FALSE)); d <- d[!is.na(d$value) & grepl("^DE", d$geo), ]
      data.frame(nuts = d$geo, year = as.integer(d$time), population = d$value, stringsAsFactors = FALSE)
    }
    p1 <- read_pop("nuts1"); p3 <- read_pop("nuts3")
    if (is.null(p1) && is.null(p3)) stop("No population: the BKA tables have no Einwohner column and the Eurostat files are missing")
    pop <- list()
    if (!is.null(p1)) { p1$territory_id <- paste0("DE-", laender$ags[match(p1$nuts, laender$nuts1)]); p1 <- p1[!is.na(laender$ags[match(p1$nuts, laender$nuts1)]), ]
      pop$land <- data.frame(country = "DE", territory_id = p1$territory_id, level = "region", year = p1$year, population = p1$population) }
    if (!is.null(p3) && !is.null(nuts)) { p3$territory_id <- nuts$territory_id[match(p3$nuts, nuts$nuts)]; p3 <- p3[!is.na(p3$territory_id), ]
      pop$kreis <- data.frame(country = "DE", territory_id = p3$territory_id, level = "province", year = p3$year, population = p3$population)
      message(sprintf("  population: Eurostat NUTS 3 matched to %d Kreise through VG250", length(unique(p3$territory_id)))) }
    else if (!is.null(p3)) message("  no AGS <-> NUTS 3 crosswalk (VG250 not loaded): no Kreis population, no Kreis rates")
    pop <- do.call(rbind, pop)
    message("  population: Eurostat demo_r_pjangrp3 (1 January)")
  }
  natp <- pop[pop$level == "region", ] |> dplyr::group_by(country, year) |> dplyr::summarise(population = sum(population), .groups = "drop")
  if (nrow(natp)) { natp$territory_id <- "DE"; natp$level <- "country"; pop <- rbind(pop[, c("country", "territory_id", "level", "year", "population")], natp[, c("country", "territory_id", "level", "year", "population")]) }
  pop <- unique(pop[!is.na(pop$population), ])
  replace_country_rows(con, "ref_country_population", "DE", pop)
  message(sprintf("  population: %d territories, %d-%d", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

# --- Boundaries: BKG VG250 (Länder and Kreise layers of the GeoPackage) --------------------------
de_vg250_zip <- function() { f <- newest_first(list.files(COUNTRY_MODULES$DE$dir_raw, "vg250.*gpkg.*\\.zip$", full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

de_vg250_layer <- function(which) {   # which = "lan" | "krs"; NULL when the zip is missing or unreadable
  zip <- de_vg250_zip(); if (is.na(zip)) return(NULL)
  if (is.null(DE_STATE$gpkg) || !file.exists(DE_STATE$gpkg)) {
    tmp <- file.path(tempdir(), "vg250"); unlink(tmp, recursive = TRUE); utils::unzip(zip, exdir = tmp)
    gp <- list.files(tmp, "\\.gpkg$", full.names = TRUE, recursive = TRUE); if (length(gp) == 0) stop("no .gpkg inside ", basename(zip))
    DE_STATE$gpkg <- gp[1]
  }
  layers <- sf::st_layers(DE_STATE$gpkg)$name
  lyr <- grep(sprintf("(^|_)%s$", which), layers, ignore.case = TRUE, value = TRUE)
  if (length(lyr) == 0) stop("no ", toupper(which), " layer in VG250: ", toString(layers))
  g <- sf::st_read(DE_STATE$gpkg, layer = lyr[1], quiet = TRUE)
  col <- function(p) grep(p, names(g), ignore.case = TRUE, value = TRUE)[1]
  gf <- col("^GF$"); if (!is.na(gf)) g <- g[as.integer(g[[gf]]) >= 3, ]                       # land only (no coastal waters)
  ags <- col("^AGS$"); gen <- col("^GEN$"); nuts <- col("^NUTS$"); ewz <- col("^EWZ$")
  if (is.na(ags) || is.na(gen)) stop("VG250 ", lyr[1], " without AGS/GEN columns: ", paste(names(g), collapse = " | "))
  g$ags <- gsub("[^0-9]", "", g[[ags]]); g$name <- g[[gen]]; g$nuts <- if (!is.na(nuts)) g[[nuts]] else NA_character_; g$ewz <- if (!is.na(ewz)) suppressWarnings(as.numeric(g[[ewz]])) else NA_real_
  g <- g[, c("ags", "name", "nuts", "ewz", attr(g, "sf_column"))]
  g |> dplyr::group_by(ags) |> dplyr::summarise(name = dplyr::first(name), nuts = dplyr::first(nuts), ewz = sum(ewz, na.rm = TRUE), .groups = "drop")
}

# AGS <-> NUTS 3 crosswalk (territory_id, nuts) from VG250, cached in the session
de_nuts_crosswalk <- function(con) {
  if (!is.null(DE_STATE$nuts)) return(DE_STATE$nuts)
  k <- tryCatch(de_vg250_layer("krs"), error = function(e) NULL)
  if (is.null(k) || all(is.na(k$nuts))) return(NULL)
  DE_STATE$nuts <- data.frame(territory_id = paste("DE", substr(k$ags, 1, 2), substr(k$ags, 1, 5), sep = "-"), nuts = k$nuts, stringsAsFactors = FALSE)
  DE_STATE$nuts
}

de_territory <- function(con) {
  laender <- de_laender()
  tbl <- data.frame(territory_id = paste0("DE-", laender$ags), level = "region", parent_id = "DE", official_code = laender$ags, name = laender$name)
  layers <- list()
  k <- tryCatch(de_vg250_layer("krs"), error = function(e) { message("  VG250 not usable (", conditionMessage(e), ")"); NULL })
  if (!is.null(k)) {
    k <- k[nchar(k$ags) >= 5, ]; k$ags <- substr(k$ags, 1, 5)
    kreise <- data.frame(territory_id = paste("DE", substr(k$ags, 1, 2), k$ags, sep = "-"), level = "province", parent_id = paste0("DE-", substr(k$ags, 1, 2)), official_code = k$ags, name = k$name)
    tbl <- rbind(tbl, kreise)
    k$territory_id <- kreise$territory_id
    l <- tryCatch(de_vg250_layer("lan"), error = function(e) NULL)
    if (!is.null(l)) { l$ags <- substr(l$ags, 1, 2); l$territory_id <- paste0("DE-", l$ags); l$name <- laender$name[match(l$ags, laender$ags)]; l <- l[!is.na(l$name), ] }
    else { l <- k; l$territory_id <- kreise$parent_id; l <- l |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop"); l$name <- laender$name[match(sub("^DE-", "", l$territory_id), laender$ags)] }
    layers <- list(region = l[, c("territory_id", "name", attr(l, "sf_column"))], province = k[, c("territory_id", "name", attr(k, "sf_column"))])
    DE_STATE$nuts <- data.frame(territory_id = kreise$territory_id, nuts = k$nuts, stringsAsFactors = FALSE)
    message(sprintf("  boundaries: BKG VG250 (%d Länder, %d Kreise)", nrow(l), nrow(k)))
  } else {
    # Fallback: GISCO NUTS 1 for the Länder; Kreise then come without polygons (ensure_territories in de_ingest)
    n <- tryCatch(gisco_nuts(1), error = function(e) { message("  GISCO NUTS not available: Länder without boundaries (", conditionMessage(e), ")"); NULL })
    if (!is.null(n)) {
      n <- n[n$CNTR_CODE == "DE", ]; n$territory_id <- paste0("DE-", laender$ags[match(n$NUTS_ID, laender$nuts1)]); n <- n[!is.na(laender$ags[match(n$NUTS_ID, laender$nuts1)]), ]
      n$name <- laender$name[match(sub("^DE-", "", n$territory_id), laender$ags)]
      layers <- list(region = n[, c("territory_id", "name", attr(n, "sf_column"))])
      COUNTRY_MODULES$DE$boundaries_attribution <<- "© EuroGeographics for the administrative boundaries (Eurostat GISCO NUTS 1); Kreise shown as tables"
    }
  }
  save_country_territory(con, "DE", tbl, scheme = "AGS")
  if (length(layers)) save_country_gpkg(con, "DE", layers, tolerance_m = 150)
  invisible(tbl)
}

register_country(list(
  code = "DE", iso3 = "DEU", name_en = "Germany", name_local = "Deutschland", lang = "de",
  levels = c("region", "province"),
  level_labels = list(region = c("States (Länder)", "Länder", "State (Land)", "Land"), province = c("Districts (Kreise)", "Kreise und kreisfreie Städte", "District (Kreis)", "Kreis")),
  source = list(name = "Polizeiliche Kriminalstatistik (PKS), BKA", publisher = "Bundeskriminalamt (BKA)",
                url = "https://www.bka.de/DE/AktuelleInformationen/StatistikenLagebilder/PolizeilicheKriminalstatistik/pks_node.html",
                licence = "Datenlizenz Deutschland – Namensnennung – Version 2.0 (attribution: PKS Bundeskriminalamt, Berichtsjahr, Version)",
                measure = "Offences recorded by the police (erfasste Fälle, incl. attempts), counted at the place of the offence, yearly; official total 'Straftaten insgesamt'. Traffic and state-security offences are outside the PKS. Kreis tables cover selected offence groups only."),
  population_source = "Einwohner column of the BKA tables when present; otherwise Eurostat demo_r_pjangrp3 (population on 1 January, NUTS 1 = Länder, NUTS 3 = Kreise) joined through BKG VG250",
  boundaries_attribution = "© GeoBasis-DE / BKG (2026), VG250, dl-de/by-2-0",
  not_published = c("Not published by the BKA", "Vom BKA nicht veröffentlicht"),
  notes_en = "Offences are counted where committed (Tatort), so commuter and tourist hubs show high rates; the BKA warns against ranking areas. No municipal level nationwide (cities of 100,000+ only; Berlin publishes Bezirksregionen separately). Kreis reforms (e.g. Göttingen 2016) change units; PKS-Ausland separated from 2024.",
  method = list(where = "Kreis or Land where the offence was committed (Tatort)", date = "Year the case was handed over to the public prosecutor (Ausgangsstatistik), which may be later than the offence", revisions = "No revision after publication: the yearly PKS is final; definitions change by year (PKS-Ausland separated from 2024)"),
  default_region = "DE-11",
  download = de_download, ingest = de_ingest, territory = de_territory, population = de_population
))
