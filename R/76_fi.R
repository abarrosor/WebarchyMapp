# Finland: offences recorded by municipality and region (ICCS) -------------------------------
# Source: Statistics Finland (Tilastokeskus), StatFin PxWeb table 13kq "Offences recorded by the
# International Classification of Crime for Statistical Purposes (ICCS) and municipality",
# yearly, 2006 onwards: offences recorded by the police, customs and border guard in the year
# they became known, by municipality of the offence (abroad / unknown are inside the national
# total only). Open, key-less PxWeb API, CC BY 4.0. Yearly -> every row has quarter = 4.
# Population: StatFin 11ra "Key figures on population by region" (population on 31 December).
# Regions (maakunta) of the municipalities: Statistics Finland classification API, with
# reference/fi_municipalities.csv as fallback. Boundaries: Statistics Finland WFS (geo.stat.fi,
# CC BY 4.0), municipality and region layers 1:4,500,000.
#
# The PxWeb API needs a POST with a JSON query, so the module first downloads the table
# metadata (GET), builds the query from the variable codes it finds there (nothing is hard
# coded except text patterns) and saves every answer with keep_version(). The selection limit
# is 300,000 cells per call: the crime table is fetched one year at a time (~100,000 cells).
# `fi_inspect()` prints the variables, the ICCS codes and the files when something changes.

FI_PX     <- "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin"
# API ids are the short table codes inside their folder (the long "statfin_rpk_pxt_13kq.px" names of the
# web address return HTTP 400 in the API; verified 2026-11-03)
FI_TABLES <- c(crime = "rpk/13kq.px", population = "vaerak/11ra.px")
FI_YEAR   <- 2026       # municipal division used for the classification and the boundaries
FI_CLASS_URL <- sprintf("https://data.stat.fi/api/classifications/v2/correspondenceTables/kunta_1_%d0101%%23maakunta_1_%d0101/maps?content=data&meta=min&lang=en", FI_YEAR, FI_YEAR)
FI_WFS <- function(layer) sprintf("https://geo.stat.fi/geoserver/tilastointialueet/wfs?service=WFS&version=2.0.0&request=GetFeature&typeName=tilastointialueet:%s4500k_%d&outputFormat=json", layer, FI_YEAR)
# Text patterns that identify the variables and the values to select (case-insensitive)
FI_PATTERNS <- list(
  area      = "municipal|kunta|\\barea\\b|\\balue\\b|\\bregion\\b",
  iccs      = "iccs",
  offence   = "offence|rikos",
  contents  = "^information$|^tiedot$",
  measure   = "known|recorded",                 # "Offences known to the authorities (number)"
  pop_item  = "^population[ ,]*31 dec",         # 11ra key figure
  total     = "^total$|^all|yhteensä|kaikki|^whole",
  national  = "whole country|koko maa"
)

fi_dir  <- function() COUNTRY_MODULES$FI$dir_raw
fi_file <- function(name) { f <- newest_first(list.files(fi_dir(), paste0("_fi_", name, "$"), full.names = TRUE)); if (length(f) == 0) NA_character_ else f[1] }
fi_fresh <- function(f, days) !is.na(f) && difftime(Sys.time(), file.mtime(f), units = "days") < days

fi_read_json <- function(f) { txt <- readr::read_file(f); jsonlite::fromJSON(sub("^﻿", "", txt), simplifyVector = FALSE) }

# Table metadata (GET): downloaded when older than `days`, else the saved copy
fi_meta <- function(key, days = 30) {
  f <- fi_file(paste0(key, "_meta.json"))
  if (!fi_fresh(f, days)) f <- tryCatch(download_versioned(paste0(FI_PX, "/", FI_TABLES[[key]]), fi_dir(), name = paste0("fi_", key, "_meta.json")),
                                        error = function(e) { if (is.na(f)) stop("PxWeb metadata not available for ", key, ": ", conditionMessage(e)); message("  using the saved metadata (", conditionMessage(e), ")"); f })
  fi_read_json(f)
}
fi_vtext <- function(v) paste(v$text, v$code)
# Roles of the variables of a table: time, area, contents, type (ICCS / offence) and the rest
fi_vars <- function(meta) {
  vars <- meta$variables; txt <- vapply(vars, fi_vtext, "")
  is_time <- vapply(vars, function(v) isTRUE(v$time), logical(1)) | grepl("^(vuosi|year|kuukausi|month)", txt, ignore.case = TRUE)
  is_cont <- grepl(FI_PATTERNS$contents, vapply(vars, function(v) v$code, ""), ignore.case = TRUE) | grepl(FI_PATTERNS$contents, vapply(vars, function(v) v$text, ""), ignore.case = TRUE)
  is_area <- grepl(FI_PATTERNS$area, txt, ignore.case = TRUE) & !is_time & !is_cont
  is_type <- (grepl(FI_PATTERNS$iccs, txt, ignore.case = TRUE) | grepl(FI_PATTERNS$offence, txt, ignore.case = TRUE)) & !is_time & !is_cont & !is_area
  if (sum(is_type) > 1) is_type <- is_type & grepl(FI_PATTERNS$iccs, txt, ignore.case = TRUE)
  pick <- function(i) if (any(i)) vars[[which(i)[1]]] else NULL
  list(time = pick(is_time), area = pick(is_area), contents = pick(is_cont), type = pick(is_type),
       other = vars[!(is_time | is_cont | is_area | is_type)], all = vars)
}
fi_values <- function(v) data.frame(code = unlist(v$values), text = unlist(v$valueTexts), stringsAsFactors = FALSE)
fi_value_matching <- function(v, pattern) { x <- fi_values(v); i <- grep(pattern, x$text, ignore.case = TRUE); if (length(i)) x$code[i[1]] else x$code[1] }

# Build and send a PxWeb query; `select` = list(variable code -> character vector of codes or "*")
fi_post <- function(key, select, name) {
  q <- lapply(names(select), function(code) list(code = code, selection = list(filter = if (identical(select[[code]], "*")) "all" else "item", values = I(as.character(select[[code]])))))
  body <- list(query = q, response = list(format = "json"))
  tmp <- file.path(tempdir(), paste0("download_", name)); if (file.exists(tmp)) file.remove(tmp)
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  httr2::request(paste0(FI_PX, "/", FI_TABLES[[key]])) |> httr2::req_user_agent(USER_AGENT) |> httr2::req_body_json(body) |>
    httr2::req_retry(max_tries = 3) |> httr2::req_timeout(300) |> httr2::req_perform(path = tmp)
  if (!file.exists(tmp) || file.size(tmp) == 0) stop("Empty answer from PxWeb for ", name)
  keep_version(tmp, fi_dir(), name)
}
# Selections shared by every call: contents = the measure, optional variables omitted, mandatory extras = their total
fi_base_select <- function(vars, measure_pattern) {
  sel <- list(); if (!is.null(vars$contents)) sel[[vars$contents$code]] <- fi_value_matching(vars$contents, measure_pattern)
  for (v in vars$other) if (!isTRUE(v$elimination)) sel[[v$code]] <- fi_value_matching(v, FI_PATTERNS$total)
  sel
}

fi_download <- function(days = 60) {
  dir.create(fi_dir(), recursive = TRUE, showWarnings = FALSE)
  # 1. offences: one JSON per year (cells = municipalities x ICCS codes, well under the 300,000 limit)
  meta <- fi_meta("crime"); vars <- fi_vars(meta)
  if (is.null(vars$time) || is.null(vars$area) || is.null(vars$type)) stop("Unexpected variables in StatFin 13kq: ", paste(vapply(vars$all, fi_vtext, ""), collapse = " | "), "\nRun fi_inspect() and adjust FI_PATTERNS in R/76_fi.R")
  years <- fi_values(vars$time)$code; last <- suppressWarnings(max(as.integer(years), na.rm = TRUE))
  for (y in years) {
    prev <- fi_file(paste0("crime_", y, ".json"))
    final <- !is.na(suppressWarnings(as.integer(y))) && as.integer(y) < last - 1
    if (!is.na(prev) && (final || fi_fresh(prev, days))) next
    sel <- fi_base_select(vars, FI_PATTERNS$measure); sel[[vars$time$code]] <- y; sel[[vars$area$code]] <- "*"; sel[[vars$type$code]] <- "*"
    message("  StatFin 13kq ", y); fi_post("crime", sel, paste0("fi_crime_", y, ".json")); Sys.sleep(1)
  }
  # 2. population on 31 December, every area and year (~20,000 cells)
  if (!fi_fresh(fi_file("population.json"), days)) {
    pm <- fi_meta("population"); pv <- fi_vars(pm)
    if (is.null(pv$time) || is.null(pv$area) || is.null(pv$contents)) stop("Unexpected variables in StatFin 11ra: ", paste(vapply(pv$all, fi_vtext, ""), collapse = " | "))
    sel <- fi_base_select(pv, FI_PATTERNS$pop_item); sel[[pv$time$code]] <- "*"; sel[[pv$area$code]] <- "*"
    fi_post("population", sel, "fi_population.json")
  }
  # 3. municipality -> region classification and boundaries (yearly division: refreshed once a year)
  if (!fi_fresh(fi_file("kunta_maakunta.json"), 365)) tryCatch(download_versioned(FI_CLASS_URL, fi_dir(), name = "fi_kunta_maakunta.json"), error = function(e) message("  classification API not available (", conditionMessage(e), "): using reference/fi_municipalities.csv"))
  for (layer in c("kunta", "maakunta")) if (is.na(fi_file(paste0(layer, ".geojson")))) tryCatch(download_versioned(FI_WFS(layer), fi_dir(), name = paste0("fi_", layer, ".geojson")), error = function(e) message("  Statistics Finland WFS not available (", conditionMessage(e), ")"))
}

# PxWeb "json" answer -> data frame with one column per dimension (codes) and `value`
fi_read_px <- function(f) {
  js <- fi_read_json(f)
  cols <- data.frame(code = vapply(js$columns, function(c) c$code, ""), type = vapply(js$columns, function(c) c$type, ""), stringsAsFactors = FALSE)
  dims <- cols$code[cols$type != "c"]
  if (length(js$data) == 0) return(setNames(data.frame(matrix(character(), 0, length(dims)), value = numeric()), c(dims, "value")))
  keys <- do.call(rbind, lapply(js$data, function(r) as.character(unlist(r$key))))
  vals <- vapply(js$data, function(r) as.character(unlist(r$values)[1]), "")
  d <- as.data.frame(keys, stringsAsFactors = FALSE); names(d) <- dims
  d$value <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", vals)))     # ".", ".." (missing / not applicable) -> NA
  d
}

# Municipality -> region table: classification API file when present, else the reference CSV
fi_municipalities <- function() {
  ref <- read_reference("fi_municipalities.csv")
  f <- fi_file("kunta_maakunta.json")
  if (is.na(f)) return(ref)
  js <- tryCatch(fi_read_json(f), error = function(e) NULL)
  if (is.null(js) || length(js) == 0) return(ref)
  api <- do.call(rbind, lapply(js, function(m) data.frame(
    municipality_code = m$sourceItem$code, municipality_name = m$sourceItem$classificationItemNames[[1]]$name,
    region_code = m$targetItem$code, region_name = m$targetItem$classificationItemNames[[1]]$name, stringsAsFactors = FALSE)))
  api$region_name_fi <- ref$region_name_fi[match(api$region_code, ref$region_code)]
  rbind(api, ref[!ref$municipality_code %in% api$municipality_code, names(api)])
}

# Area code of StatFin ("SSS" whole country, "KU091" municipality, "MK01" region) -> territory_id
fi_ids <- function(code, muni, national_codes = "SSS") {
  code <- trimws(code)
  mk <- grepl("^MK[0-9]{2}$", code); ku <- grepl("^(KU)?[0-9]{3}$", code)
  kcode <- ifelse(ku, sub("^KU", "", code), NA_character_)
  reg <- muni$region_code[match(kcode, muni$municipality_code)]
  ifelse(code %in% national_codes, "FI", ifelse(mk, paste0("FI-", sub("^MK", "", code)), ifelse(!is.na(reg), paste("FI", reg, kcode, sep = "-"), NA_character_)))
}
fi_level <- function(id) ifelse(id == "FI", "country", ifelse(nchar(id) == 5, "region", "municipality"))
fi_national_codes <- function(area_var) { x <- fi_values(area_var); unique(c("SSS", x$code[grepl(FI_PATTERNS$national, x$text, ignore.case = TRUE)])) }

# Offence codes as they appear in reference/fi_offence_types.csv: the total -> "TOTAL"; a code that
# is not a plain ICCS number takes the number at the start of its label ("0101 Intentional homicide")
fi_offence_codes <- function(type_var) {
  x <- fi_values(type_var)
  norm <- ifelse(grepl(FI_PATTERNS$total, x$text, ignore.case = TRUE), "TOTAL",
                 ifelse(grepl("^[0-9]+$", x$code), x$code, ifelse(grepl("^[0-9]{2,6}\\b", x$text), regmatches(x$text, regexpr("^[0-9]{2,6}", x$text)), x$code)))
  norm[duplicated(norm) & norm == "TOTAL"] <- x$code[duplicated(norm) & norm == "TOTAL"]
  setNames(norm, x$code)
}

fi_crime_files <- function() {
  f <- list.files(fi_dir(), "_fi_crime_[0-9]+\\.json$", full.names = TRUE); if (length(f) == 0) return(character())
  y <- sub("^.*_fi_crime_([0-9]+)\\.json$", "\\1", f); f <- newest_first(f); y <- sub("^.*_fi_crime_([0-9]+)\\.json$", "\\1", f)
  f[!duplicated(y)]                               # newest file of each year
}

fi_ingest <- function(con) {
  files <- fi_crime_files(); if (length(files) == 0) stop("No StatFin 13kq file yet in ", fi_dir())
  meta <- fi_meta("crime", days = Inf); vars <- fi_vars(meta)
  muni <- fi_municipalities(); codes <- fi_offence_codes(vars$type); nat <- fi_national_codes(vars$area)
  rows <- list(); dropped <- character()
  for (f in files) {
    d <- fi_read_px(f)
    need <- c(vars$time$code, vars$area$code, vars$type$code)
    if (!all(need %in% names(d))) stop("Unexpected columns in ", basename(f), ": ", paste(names(d), collapse = " | "), "\nRun fi_inspect()")
    d$territory_id <- fi_ids(d[[vars$area$code]], muni, nat)
    dropped <- union(dropped, unique(d[[vars$area$code]][is.na(d$territory_id)]))
    d <- d[!is.na(d$territory_id), ]
    oc <- unname(codes[d[[vars$type$code]]]); oc[is.na(oc)] <- d[[vars$type$code]][is.na(oc)]
    rows[[f]] <- data.frame(country = "FI", territory_id = d$territory_id, level = fi_level(d$territory_id), offence_code = oc,
                            year = as.integer(substr(d[[vars$time$code]], 1, 4)), n = d$value, file = basename(f), stringsAsFactors = FALSE)
  }
  if (length(dropped)) message("  area codes without a region (inside the national total only): ", paste(dropped, collapse = ", "))
  crime <- do.call(rbind, rows); crime <- crime[!is.na(crime$year), ]
  # Regions = sum of their municipalities (the table has no regional rows); NA when every municipality is NA
  reg <- crime[crime$level == "municipality", ]; reg$territory_id <- sub("-[^-]+$", "", reg$territory_id); reg$level <- "region"
  reg <- reg |> dplyr::group_by(country, territory_id, level, offence_code, year, file) |> dplyr::summarise(n = if (all(is.na(n))) NA_real_ else sum(n, na.rm = TRUE), .groups = "drop") |> as.data.frame()
  crime <- yearly_to_cumulative(rbind(crime, reg[, names(crime)]))
  replace_country_rows(con, "stg_country_crime", "FI", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_FI", paste(basename(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  known <- read_reference("fi_offence_types.csv")$source_code; unmapped <- setdiff(unique(crime$offence_code), known)
  message(sprintf("  13kq: %s rows, %d territories, %d-%d; %d offence codes not in fi_offence_types.csv (left out, see fi_inspect())", fmt_int(nrow(crime)), length(unique(crime$territory_id)), min(crime$year), max(crime$year), length(unmapped)))
  invisible(crime)
}

fi_population <- function(con) {
  f <- fi_file("population.json"); if (is.na(f)) stop("No StatFin 11ra file yet in ", fi_dir())
  pm <- fi_meta("population", days = Inf); pv <- fi_vars(pm); muni <- fi_municipalities()
  d <- fi_read_px(f)
  d$territory_id <- fi_ids(d[[pv$area$code]], muni, fi_national_codes(pv$area)); d <- d[!is.na(d$territory_id) & !is.na(d$value), ]
  pop <- data.frame(country = "FI", territory_id = d$territory_id, level = fi_level(d$territory_id), year = as.integer(substr(d[[pv$time$code]], 1, 4)), population = d$value)
  # Regions without an official row (older MK codes): sum of their municipalities
  have <- unique(pop$territory_id[pop$level == "region"])
  reg <- pop[pop$level == "municipality", ]; reg$territory_id <- sub("-[^-]+$", "", reg$territory_id); reg$level <- "region"
  reg <- reg[!reg$territory_id %in% have, ] |> dplyr::group_by(country, territory_id, level, year) |> dplyr::summarise(population = sum(population), .groups = "drop") |> as.data.frame()
  pop <- unique(rbind(pop, reg[, names(pop)]))
  replace_country_rows(con, "ref_country_population", "FI", pop)
  message(sprintf("  population: %d territories, %d-%d (StatFin 11ra, 31 December)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

# Territories: regions and municipalities from the classification (or the CSV); polygons from the WFS files
fi_territory <- function(con) {
  muni <- fi_municipalities(); regs <- unique(muni[, c("region_code", "region_name")])
  tbl <- rbind(
    data.frame(territory_id = paste0("FI-", regs$region_code), level = "region", parent_id = "FI", official_code = regs$region_code, name = regs$region_name),
    data.frame(territory_id = paste("FI", muni$region_code, muni$municipality_code, sep = "-"), level = "municipality",
               parent_id = paste0("FI-", muni$region_code), official_code = muni$municipality_code, name = muni$municipality_name))
  save_country_territory(con, "FI", tbl, scheme = "Tilastokeskus")
  read_layer <- function(name, field) {
    f <- fi_file(paste0(name, ".geojson")); if (is.na(f)) return(NULL)
    g <- tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) { message("  cannot read ", basename(f), ": ", conditionMessage(e)); NULL })
    if (is.null(g)) return(NULL)
    if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 3067
    if (!field %in% names(g)) { message("  ", basename(f), " has no column '", field, "' (", paste(names(g), collapse = ", "), ")"); return(NULL) }
    g
  }
  gk <- read_layer("kunta", "kunta"); gm <- read_layer("maakunta", "maakunta"); layers <- list()
  if (!is.null(gk)) {
    code <- sprintf("%03d", suppressWarnings(as.integer(gk$kunta))); i <- match(code, muni$municipality_code); ok <- !is.na(i)
    g <- gk[ok, ]; g$territory_id <- paste("FI", muni$region_code[i[ok]], code[ok], sep = "-"); g$name <- muni$municipality_name[i[ok]]
    layers$municipality <- g[, c("territory_id", "name", attr(g, "sf_column"))]
  }
  if (!is.null(gm)) {
    code <- sprintf("%02d", suppressWarnings(as.integer(gm$maakunta))); ok <- code %in% regs$region_code
    g <- gm[ok, ]; g$territory_id <- paste0("FI-", code[ok]); g$name <- regs$region_name[match(code[ok], regs$region_code)]
    layers$region <- g[, c("territory_id", "name", attr(g, "sf_column"))]
  } else if (!is.null(layers$municipality)) {
    gr <- layers$municipality; gr$territory_id <- sub("-[^-]+$", "", gr$territory_id)
    gr <- gr |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop"); gr$name <- regs$region_name[match(sub("^FI-", "", gr$territory_id), regs$region_code)]
    layers$region <- gr
  }
  if (length(layers)) save_country_gpkg(con, "FI", layers[intersect(c("region", "municipality"), names(layers))], tolerance_m = 100) else message("  no boundary files: tables only")
  invisible(tbl)
}

# Print the variables of the tables, the ICCS codes, the files and the codes still unmapped
fi_inspect <- function() {
  for (key in names(FI_TABLES)) {
    f <- fi_file(paste0(key, "_meta.json")); if (is.na(f)) { message(key, ": no metadata downloaded"); next }
    meta <- fi_read_json(f); vars <- fi_vars(meta)
    message(key, ": ", meta$title, "\n  variables: ", paste(vapply(meta$variables, function(v) sprintf("%s [%s]%s", v$code, v$text, if (isTRUE(v$elimination)) " (optional)" else ""), ""), collapse = " | "))
    message("  roles: time = ", vars$time$code %||% "?", ", area = ", vars$area$code %||% "?", ", contents = ", vars$contents$code %||% "?", ", type = ", vars$type$code %||% "-")
    if (!is.null(vars$type)) { x <- fi_values(vars$type); x$normalised <- unname(fi_offence_codes(vars$type)[x$code]); x$in_csv <- x$normalised %in% read_reference("fi_offence_types.csv")$source_code; print(x) }
    if (!is.null(vars$contents)) print(fi_values(vars$contents))
  }
  for (f in c(fi_crime_files()[1], fi_file("population.json"))) if (!is.na(f)) { d <- fi_read_px(f); message(basename(f), ": ", fmt_int(nrow(d)), " rows\n  ", paste(names(d), collapse = " | ")); print(utils::head(d, 3)) }
}

register_country(list(
  code = "FI", iso3 = "FIN", name_en = "Finland", name_local = "Suomi", lang = "fi",
  levels = c("region", "municipality"),
  level_labels = list(region = c("Regions", "Maakunnat", "Region", "Maakunta"), municipality = c("Municipalities", "Kunnat", "Municipality", "Kunta")),
  source = list(name = "Offences recorded by ICCS and municipality (StatFin 13kq)", publisher = "Tilastokeskus (Statistics Finland)",
                url = "https://pxdata.stat.fi/PxWeb/pxweb/en/StatFin/StatFin__rpk/statfin_rpk_pxt_13kq.px/", licence = "CC BY 4.0",
                measure = "Offences known to the authorities (police, customs, border guard), counted in the year they became known, by municipality of the offence; yearly, ICCS classification."),
  population_source = "Tilastokeskus, StatFin 11ra (population on 31 December)",
  boundaries_attribution = "Tilastokeskus (Statistics Finland) municipal and regional boundaries 1:4 500 000, CC BY 4.0",
  not_published = c("Not published by Statistics Finland", "Tilastokeskus ei julkaise"),
  notes_en = "Yearly data (quarter 4 = whole year). Offences abroad or in an unknown municipality are only in the national figure. Breaks: Road Traffic Act 2020 (traffic infractions became administrative), sexual offence law 2023, PC chapters 12-13 not published from 2023. Åland is included as a region.",
  default_region = "FI-01",
  download = fi_download, ingest = fi_ingest, territory = fi_territory, population = fi_population
))
