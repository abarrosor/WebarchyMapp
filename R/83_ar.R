# Argentina: recorded offences by departamento/partido and provincia ---------------------------
# Source: Ministerio de Seguridad Nacional, Dirección Nacional de Estadística Criminal, SNIC
# (Sistema Nacional de Información Criminal): "hechos presuntamente delictuosos" registered by
# the police and other complaint-receiving bodies, yearly, by provincia (2000 onwards) and by
# departamento/partido (2014 onwards), on datos.gob.ar under CC BY 4.0. One row per
# territory x year x SNIC offence code with `cantidad_hechos` and `cantidad_victimas`.
# Levels: region = provincia (24, INDEC 2-digit code), municipality = departamento/partido
# (~530, INDEC 5-digit code: there is no municipal level in SNIC).
# Population: the `poblacion` column of the SNIC files when present, else INDEC projections.
# Boundaries: IGN Argentina WFS (departamento, provincia; codes in `in1`).
#
# Written against the documented structure (datos.gob.ar was not reachable from the audit
# sandbox): columns are detected by name; `ar_inspect()` prints the headers.
# URLs live in reference/ar_sources.csv (several candidates per key, tried in order).

AR_STATE <- new.env()
AR_COLUMNS <- list(
  prov_code = c("^provincia_id$", "^id_provincia$", "^cod_?provincia", "^provincia_codigo"),
  prov_name = c("^provincia_nombre$", "^provincia$", "^nombre_provincia"),
  dept_code = c("^departamento_id$", "^id_departamento$", "^cod_?departamento", "^departamento_codigo"),
  dept_name = c("^departamento_nombre$", "^departamento$", "^nombre_departamento", "^partido"),
  year      = c("^anio$", "^año$", "^ano$", "^year$"),
  code_id   = c("^codigo_delito_snic_id$", "^codigo_delito"),
  code_name = c("^codigo_delito_snic_nombre$", "^delito", "^nombre_delito", "^tipo_delito"),
  n         = c("^cantidad_hechos$", "^hechos$", "^cantidad$"),
  pop       = c("^poblacion", "^población")
)

ar_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }
ar_sources <- function() read_reference("ar_sources.csv")
ar_provinces <- function() read_reference("ar_provinces.csv")
ar_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", as.character(x)))))

ar_file <- function(name) {
  f <- newest_first(list.files(COUNTRY_MODULES$AR$dir_raw, paste0("_", gsub("([.()+])", "\\\\\\1", name), "$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}

# Download: for each key the candidate URLs are tried in order until one answers
ar_download <- function(days = 60) {
  m <- COUNTRY_MODULES$AR; src <- ar_sources()
  for (key in c("departamentos", "provincias", "pais", "ign_departamentos", "ign_provincias")) {
    rows <- src[src$key == key, ]; if (nrow(rows) == 0) next
    prev <- ar_file(rows$name[1])
    max_age <- if (grepl("^ign", key)) 365 else days
    if (!is.na(prev) && file_age_days(prev) < max_age) { message("= AR ", rows$name[1], " downloaded less than ", max_age, " days ago"); next }
    ok <- FALSE
    for (i in seq_len(nrow(rows))) {
      ok <- tryCatch({ download_versioned(rows$url[i], m$dir_raw, name = rows$name[i]); TRUE }, error = function(e) { message("  AR ", key, " candidate ", i, " failed: ", conditionMessage(e)); FALSE })
      if (ok) break
    }
    if (!ok) message("  AR ", key, ": no candidate URL answered; check ", if (grepl("^ign", key)) "the IGN WFS (https://wms.ign.gob.ar/geoserver)" else "the dataset page https://datos.gob.ar/dataset/seguridad-estadisticas-criminales-republica-argentina-por-provincias-departamentos", " and update reference/ar_sources.csv")
  }
}

ar_read <- function(f) {
  first <- readLines(f, n = 1, warn = FALSE, encoding = "UTF-8")
  sep <- if (lengths(regmatches(first, gregexpr(";", first))) > lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
  d <- readr::read_delim(f, delim = sep, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
  names(d) <- trimws(gsub("^﻿", "", names(d)))
  d
}

ar_inspect <- function() {
  for (key in c("departamentos", "provincias", "pais")) {
    f <- ar_file(ar_sources()$name[ar_sources()$key == key][1]); if (is.na(f)) { message(key, ": no file yet"); next }
    d <- ar_read(f); message(key, ": ", basename(f), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3))
    cn <- ar_pick(names(d), AR_COLUMNS$code_name); if (!is.na(cn)) message("  offence labels: ", paste(sort(unique(d[[cn]])), collapse = " ; "))
  }
  if (length(AR_STATE$unknown)) { message("Offence labels not in ar_offence_types.csv:"); print(AR_STATE$unknown) }
}

# Standardise one file to: prov_code, dept_code, dept_name, year, label, n, pop
ar_standardise <- function(d, key) {
  nm <- names(d); col <- lapply(AR_COLUMNS, ar_pick, nm = nm)
  if (is.na(col$year) || is.na(col$code_name) || is.na(col$n) || (key != "pais" && is.na(col$prov_code) && is.na(col$prov_name)))
    stop("Unexpected columns in the SNIC ", key, " file: ", paste(nm, collapse = " | "), "\nRun ar_inspect() and adjust AR_COLUMNS in R/83_ar.R")
  provs <- ar_provinces()
  year <- as.integer(substr(trimws(d[[col$year]]), 1, 4))                           # "2019" or "2019-01-01"
  prov <- if (!is.na(col$prov_code)) sprintf("%02d", as.integer(ar_num(d[[col$prov_code]]))) else rep(NA_character_, nrow(d))
  if (!is.na(col$prov_name)) { miss <- is.na(prov) | !prov %in% provs$province_code; prov[miss] <- provs$province_code[match(normalize_text(d[[col$prov_name]][miss]), normalize_text(provs$province_name))] }
  dept <- if (!is.na(col$dept_code)) gsub("[^0-9]", "", d[[col$dept_code]]) else NA_character_
  dept <- ifelse(!is.na(dept) & nchar(dept) %in% 4:5, sprintf("%05d", as.integer(dept)), NA_character_)
  out <- data.frame(prov_code = prov, dept_code = dept, dept_name = if (!is.na(col$dept_name)) trimws(d[[col$dept_name]]) else NA_character_,
                    year = year, label = trimws(d[[col$code_name]]), n = ar_num(d[[col$n]]),
                    pop = if (!is.na(col$pop)) ar_num(d[[col$pop]]) else NA_real_, stringsAsFactors = FALSE)
  out[!is.na(out$year) & out$label != "", ]
}

# Map the SNIC labels to the source codes of ar_offence_types.csv (accent- and case-insensitive)
ar_codes <- function(label, types) {
  code <- types$source_code[match(normalize_text(label), normalize_text(types$source_code))]
  unknown <- unique(label[is.na(code)])
  if (length(unknown)) { AR_STATE$unknown <- unique(c(AR_STATE$unknown, unknown)); message("  SNIC labels not in ar_offence_types.csv (ignored): ", paste(unknown, collapse = "; ")) }
  code
}

ar_ingest <- function(con) {
  src <- ar_sources(); types <- read_reference("ar_offence_types.csv"); AR_STATE$unknown <- NULL
  have <- vapply(c("departamentos", "provincias", "pais"), function(k) ar_file(src$name[src$key == k][1]), "")
  files <- list(); parts <- list(); pops <- list()
  for (key in names(have)) {
    f <- have[[key]]; if (is.na(f)) { message("  no SNIC ", key, " file yet"); next }
    d <- ar_standardise(ar_read(f), key); files[[key]] <- basename(f)
    d$offence_code <- ar_codes(d$label, types); d <- d[!is.na(d$offence_code), ]
    if (key == "departamentos") {
      d <- d[!is.na(d$prov_code), ]
      ok <- !is.na(d$dept_code)
      d$territory_id <- ifelse(ok, paste("AR", d$prov_code, d$dept_code, sep = "-"), NA)
      d$level <- "municipality"
      AR_STATE$depts <- unique(d[ok, c("territory_id", "prov_code", "dept_code", "dept_name")])
      if (is.na(have[["provincias"]])) {   # provinces as the sum of their departments (incl. "sin determinar")
        p <- d |> dplyr::group_by(prov_code, year, offence_code) |> dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
        parts$prov_from_dept <- data.frame(country = "AR", territory_id = paste0("AR-", p$prov_code), level = "region", offence_code = p$offence_code, year = p$year, n = p$n, file = basename(f))
      }
      d <- d[ok, ]
    } else if (key == "provincias") { d <- d[!is.na(d$prov_code), ]; d$territory_id <- paste0("AR-", d$prov_code); d$level <- "region" }
    else { d$territory_id <- "AR"; d$level <- "country" }
    g <- d |> dplyr::group_by(territory_id, level, offence_code, year) |> dplyr::summarise(n = sum(n, na.rm = TRUE), pop = suppressWarnings(max(pop, na.rm = TRUE)), .groups = "drop")
    parts[[key]] <- data.frame(country = "AR", territory_id = g$territory_id, level = g$level, offence_code = g$offence_code, year = g$year, n = g$n, file = basename(f))
    if (any(is.finite(g$pop))) pops[[key]] <- unique(data.frame(country = "AR", territory_id = g$territory_id, level = g$level, year = g$year, population = g$pop)[is.finite(g$pop), ])
    message(sprintf("  %s: %s rows, %d territories, %d-%d", key, fmt_int(nrow(d)), length(unique(d$territory_id)), min(d$year), max(d$year)))
  }
  if (length(parts) == 0) stop("No SNIC file found in ", COUNTRY_MODULES$AR$dir_raw)
  crime <- do.call(rbind, parts)
  if (!"pais" %in% names(parts)) {   # national = sum of the provinces
    nat <- crime[crime$level == "region", ] |> dplyr::group_by(country, offence_code, year) |> dplyr::summarise(n = sum(n), file = file[1], .groups = "drop")
    nat$territory_id <- "AR"; nat$level <- "country"; crime <- rbind(crime, nat[, names(crime)])
  }
  # Synthetic total = sum of the mapped offence types (never the unmapped ones, e.g. road deaths)
  counted <- types$source_code[!types$offence_type_id %in% c("unmapped", "total")]
  tot <- crime[crime$offence_code %in% counted, ] |> dplyr::group_by(country, territory_id, level, year, file) |> dplyr::summarise(n = sum(n), .groups = "drop")
  tot$offence_code <- "TOTAL_SNIC"
  crime <- yearly_to_cumulative(rbind(crime, tot[, names(crime)]))
  # Departments present in the data but absent from dim_territory
  if (!is.null(AR_STATE$depts)) ensure_territories(con, "AR", AR_STATE$depts$territory_id, "municipality", paste0("AR-", AR_STATE$depts$prov_code), AR_STATE$depts$dept_name)
  replace_country_rows(con, "stg_country_crime", "AR", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_AR", paste(unlist(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  AR_STATE$pop <- if (length(pops)) unique(do.call(rbind, pops)) else NULL; AR_STATE$ran <- TRUE
  message(sprintf("  AR: %s rows, %d territories, %d-%d", fmt_int(nrow(crime)), length(unique(crime$territory_id)), min(crime$year), max(crime$year)))
  invisible(crime)
}

# Population: the `poblacion` column of the SNIC files; else INDEC projections by departamento
ar_population <- function(con) {
  if (!isTRUE(AR_STATE$ran)) ar_ingest(con)
  pop <- AR_STATE$pop
  if (is.null(pop)) pop <- tryCatch(ar_indec_population(), error = function(e) { message("  INDEC projections not read: ", conditionMessage(e)); NULL })
  if (is.null(pop) || nrow(pop) == 0) stop("No population found (no 'poblacion' column in the SNIC files and no INDEC file): rates are not shown")
  if (!"region" %in% pop$level) {    # provinces = sum of their departments
    prov <- pop[pop$level == "municipality", ] |> dplyr::group_by(country, territory_id = sub("-[^-]+$", "", territory_id), year) |> dplyr::summarise(population = sum(population), .groups = "drop")
    if (nrow(prov)) { prov$level <- "region"; pop <- rbind(pop, prov[, names(pop)]) }
  }
  if (!"country" %in% pop$level) {   # national = sum of the provinces, when provinces exist
    natp <- pop[pop$level == "region", ] |> dplyr::group_by(country, year) |> dplyr::summarise(population = sum(population), .groups = "drop")
    if (nrow(natp)) { natp$territory_id <- "AR"; natp$level <- "country"; pop <- rbind(pop, natp[, names(pop)]) }
  }
  replace_country_rows(con, "ref_country_population", "AR", pop)
  message(sprintf("  population: %d territories, %d-%d", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

# INDEC "Proyecciones de población por departamento 2010-2025": one .xls per province, several
# tables per sheet. Heuristic reader: a header row with years, then rows "name, values...".
# The department name is matched to the IGN list within the province. Not verified against the files.
ar_indec_population <- function() {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("package readxl is needed")
  m <- COUNTRY_MODULES$AR; src <- ar_sources(); row <- src[src$key == "indec_poblacion", ][1, ]
  provs <- ar_provinces(); depts <- AR_STATE$depts
  if (is.null(depts)) stop("departments unknown: run the ingest first")
  slug <- function(x) { x <- tolower(normalize_text(x)); x <- gsub("[^a-z ]", "", x); gsub(" +", "_", trimws(x)) }
  out <- list()
  for (i in seq_len(nrow(provs))) {
    s <- if (provs$province_code[i] == "02") "caba" else if (provs$province_code[i] == "94") "tierra_del_fuego" else slug(provs$province_name[i])
    name <- sub("{slug}", s, row$name, fixed = TRUE); f <- ar_file(name)
    if (is.na(f)) f <- tryCatch(download_versioned(sub("{slug}", s, row$url, fixed = TRUE), m$dir_raw, name = name), error = function(e) NA_character_)
    if (is.na(f)) next
    for (sh in readxl::excel_sheets(f)) {
      x <- as.data.frame(readxl::read_excel(f, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
      hdr <- which(apply(x, 1, function(r) sum(grepl("^(19|20)[0-9]{2}$", trimws(r))) >= 5))
      for (h in hdr) {
        years <- suppressWarnings(as.integer(trimws(x[h, ]))); yc <- which(!is.na(years) & years >= 1990)
        j <- h + 1
        while (j <= nrow(x) && !all(is.na(x[j, ]))) {
          nm <- trimws(x[j, 1]); vals <- ar_num(x[j, yc])
          if (!is.na(nm) && any(!is.na(vals))) {
            id <- depts$territory_id[depts$prov_code == provs$province_code[i]][match(normalize_text(nm), normalize_text(depts$dept_name[depts$prov_code == provs$province_code[i]]))]
            if (!is.na(id)) out[[length(out) + 1]] <- data.frame(country = "AR", territory_id = id, level = "municipality", year = years[yc][!is.na(vals)], population = vals[!is.na(vals)])
          }
          j <- j + 1
        }
      }
    }
  }
  if (length(out) == 0) stop("no department table recognised in the INDEC files")
  pop <- unique(do.call(rbind, out))
  prov <- pop |> dplyr::group_by(country, territory_id = sub("-[^-]+$", "", territory_id), year) |> dplyr::summarise(population = sum(population), .groups = "drop"); prov$level <- "region"
  rbind(pop, prov[, names(pop)])
}

# Territories: provinces from reference/ar_provinces.csv; departments and boundaries from IGN WFS
ar_territory <- function(con) {
  m <- COUNTRY_MODULES$AR; src <- ar_sources(); provs <- ar_provinces()
  tbl <- data.frame(territory_id = paste0("AR-", provs$province_code), level = "region", parent_id = "AR", official_code = provs$province_code, name = provs$province_name)
  read_geo <- function(key) {
    name <- src$name[src$key == key][1]; f <- ar_file(name)
    if (is.na(f)) f <- tryCatch(download_versioned(src$url[src$key == key][1], m$dir_raw, name = name), error = function(e) NA_character_)
    if (is.na(f)) return(NULL)
    g <- tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL)
    if (is.null(g) || !all(c("in1", "nam") %in% names(g))) { message("  IGN ", key, ": unexpected layer (", paste(names(g), collapse = " | "), ")"); return(NULL) }
    g
  }
  layers <- list()
  gp <- read_geo("ign_provincias")
  if (!is.null(gp)) {
    gp$territory_id <- paste0("AR-", sprintf("%02d", as.integer(gp$in1))); gp$name <- provs$province_name[match(sub("^AR-", "", gp$territory_id), provs$province_code)]
    gp <- gp[!is.na(gp$name), ]; layers$region <- gp[, c("territory_id", "name", attr(gp, "sf_column"))]
  } else {
    gb <- tryCatch(geoboundaries("ARG", 1), error = function(e) NULL)
    if (!is.null(gb)) {
      code <- provs$province_code[match(normalize_text(gb$shapeName), normalize_text(provs$province_name))]
      code[is.na(code) & grepl("BUENOS AIRES", normalize_text(gb$shapeName)) & grepl("CIUDAD|CAPITAL|AUTONOMA", normalize_text(gb$shapeName))] <- "02"
      gb <- gb[!is.na(code), ]; gb$territory_id <- paste0("AR-", code[!is.na(code)]); gb$name <- provs$province_name[match(code[!is.na(code)], provs$province_code)]
      layers$region <- gb[, c("territory_id", "name", attr(gb, "sf_column"))]; message("  province boundaries from geoBoundaries (CC BY 4.0)")
    }
  }
  gd <- read_geo("ign_departamentos")
  if (!is.null(gd)) {
    code <- sprintf("%05d", as.integer(gd$in1)); prov <- substr(code, 1, 2)
    gd$territory_id <- paste("AR", prov, code, sep = "-"); gd$name <- as.character(gd$nam)
    gd <- gd[prov %in% provs$province_code, ]
    tbl <- rbind(tbl, data.frame(territory_id = gd$territory_id, level = "municipality", parent_id = sub("-[^-]+$", "", gd$territory_id), official_code = sub("^.*-", "", gd$territory_id), name = gd$name))
    layers$municipality <- gd[, c("territory_id", "name", attr(gd, "sf_column"))]
  } else message("  IGN departments not available: departments will be added from the SNIC file without boundaries")
  save_country_territory(con, "AR", tbl, scheme = "INDEC")
  if (length(layers)) save_country_gpkg(con, "AR", layers, tolerance_m = 200)
  invisible(tbl)
}

register_country(list(
  code = "AR", iso3 = "ARG", name_en = "Argentina", name_local = "Argentina", lang = "es",
  levels = c("region", "municipality"),
  level_labels = list(region = c("Provinces", "Provincias", "Province", "Provincia"),
                      municipality = c("Departments / partidos", "Departamentos / partidos", "Department", "Departamento")),
  source = list(name = "SNIC – Estadísticas criminales por provincias y departamentos", publisher = "Ministerio de Seguridad Nacional, Dirección Nacional de Estadística Criminal",
                url = "https://datos.gob.ar/dataset/seguridad-estadisticas-criminales-republica-argentina-por-provincias-departamentos",
                licence = "Creative Commons Attribution 4.0 (datos.gob.ar)",
                measure = "Hechos presuntamente delictuosos (alleged offences) registered by police and other complaint-receiving bodies and reported to SNIC, yearly, by SNIC offence code (`cantidad_hechos`). No official total: the total shown is the sum of the mapped codes."),
  population_source = "SNIC files ('poblacion' column) or INDEC population projections by departamento",
  boundaries_attribution = "Instituto Geográfico Nacional (IGN) de la República Argentina, capas SIG (WFS)",
  not_published = c("Not reported to SNIC by the jurisdiction for this year", "No informado al SNIC por la jurisdicción en este año"),
  notes_en = "Departments/partidos are the smallest SNIC unit (no municipalities). Coverage depends on each province reporting to SNIC; some provinces have gaps in some years. Road traffic deaths and injuries (culposos) are excluded from the total.",
  method = list(where = "Departamento / partido where the alleged offence was registered by the reporting police force", date = "Calendar year of registration", revisions = "Yearly publication; provinces may correct earlier years in later editions"),
  default_region = "AR-02",
  download = ar_download, ingest = ar_ingest, territory = ar_territory, population = ar_population
))
