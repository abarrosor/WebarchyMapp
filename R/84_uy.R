# Uruguay: complaints recorded by the police, by departamento and seccional policial ----------
# Source: Ministerio del Interior, Área de Estadística y Criminología Aplicada / Observatorio
# de Violencia y Criminalidad, "Delitos denunciados en el Uruguay" (catalogodatos.gub.uy),
# Licencia de Datos Abiertos del Gobierno de Uruguay (Licencia DAG, attribution). Quarterly
# update, events from January 2013 to the last closed quarter. Two microdata files:
#   - otros-delitos-2.csv: one row per complaint (denuncia) registered in the SGSP for
#     RAPIÑA, HURTO, LESIONES, VIOLENCIA DOMÉSTICA and ABIGEATO (attempts included), with
#     DELITO, FECHA, AÑO, MES, TRIMESTRE, DEPTO, JURISDICCION (seccional) ... (> 1 M rows)
#   - homicidios_dolosos_consumados.csv: one row per VICTIM (a different measure): loaded as a
#     separate sensitive series that is NOT added to the synthetic total.
# Levels: region = departamento (19, INE codes), municipality = seccional policial (from the
# JURISDICCION field; boundaries changed over time and some rows have none: those rows count
# in their departamento only). Boundaries: the Ministry's "Seccionales Policiales" KML
# (Licencia DAG); departamentos are unions of their seccionales (fallback geoBoundaries ADM1).
# Population: INE estimates 1996-2023 (A.1.2) and projections 2024-2045 (B.1.2) by departamento.
#
# Written against the published metadata and the visible first rows (not run against the
# live files): columns are found by name; `uy_inspect()` prints the headers.

UY_RESOURCES <- c(
  otros      = "https://catalogodatos.gub.uy/dataset/999f2edc-5ef5-4d41-bed7-824a5635ea8d/resource/4d76b716-a06a-45dd-ba1c-01fc6d88ee99/download/otros-delitos-2.csv",   # ~150 MB
  homicidios = "https://catalogodatos.gub.uy/dataset/999f2edc-5ef5-4d41-bed7-824a5635ea8d/resource/5ed98add-f127-4377-b529-aa8ad35b77e3/download/homicidios_dolosos_consumados.csv",
  seccionales = "https://catalogodatos.gub.uy/dataset/0d3f05e1-f5b9-45aa-9a3c-5dde755e9fee/resource/4b7423bf-a1c2-49a5-9c36-7e5944d0e7f6/download/seccionalespoliciales-kml.kml",
  pop_est    = "https://www5.ine.gub.uy/documents/Demograf%C3%ADayEESS/SERIES%20Y%20OTROS/Estimaciones%20y%20proyecciones/Revisi%C3%B3n%202025/A.1.2%20Departamentos.xlsx",
  pop_proj   = "https://www5.ine.gub.uy/documents/Demograf%C3%ADayEESS/SERIES%20Y%20OTROS/Estimaciones%20y%20proyecciones/Revisi%C3%B3n%202025/B.1.2%20Departamentos%20%28100ymas%292024-2045.xlsx"
)
UY_NAMES <- c(otros = "uy_otros_delitos.csv", homicidios = "uy_homicidios.csv", seccionales = "uy_seccionales.kml", pop_est = "uy_ine_estimaciones.xlsx", pop_proj = "uy_ine_proyecciones.xlsx")
UY_DAYS <- c(otros = 30, homicidios = 30, seccionales = 365, pop_est = 180, pop_proj = 180)
# Accepted names for every field the loader needs (first match wins, case-insensitive)
UY_COLUMNS <- list(
  delito = c("^DELITO$", "^TIPO_?DELITO$"),
  fecha  = c("^FECHA$", "^FECHA_?HECHO$"),
  year   = c("^A.{1,3}O$", "^ANIO$", "^YEAR$"),            # AÑO, whatever the encoding did to the Ñ
  month  = c("^MES$"),
  dept   = c("^DEPARTAMENTO$", "^DEPTO$", "^DPTO$"),
  juris  = c("^JURISDICCION$", "^SECCIONAL$"),
  tentativa = c("^TENTATIVA$")
)
UY_MONTHS <- c(ENERO = 1, FEBRERO = 2, MARZO = 3, ABRIL = 4, MAYO = 5, JUNIO = 6, JULIO = 7, AGOSTO = 8, SETIEMBRE = 9, SEPTIEMBRE = 9, OCTUBRE = 10, NOVIEMBRE = 11, DICIEMBRE = 12)

uy_file <- function(key) {
  f <- newest_first(list.files(COUNTRY_MODULES$UY$dir_raw, paste0("_", gsub(".", "\\.", UY_NAMES[[key]], fixed = TRUE), "$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}
uy_download <- function() {
  m <- COUNTRY_MODULES$UY
  for (k in names(UY_RESOURCES)) {
    prev <- uy_file(k)
    if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < UY_DAYS[[k]]) { message("= UY ", k, " downloaded less than ", UY_DAYS[[k]], " days ago"); next }
    tryCatch(download_versioned(UY_RESOURCES[[k]], m$dir_raw, name = UY_NAMES[[k]]), error = function(e) message("  UY ", k, ": ", conditionMessage(e)))
  }
}
uy_departamentos <- function() read_reference("uy_departamentos.csv")
uy_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }

# CSV with a header: separator and encoding detected, everything read as text
uy_read <- function(f) {
  probe <- readLines(f, n = 2000, warn = FALSE)
  enc <- if (all(validUTF8(probe))) "UTF-8" else "latin1"
  sep <- if (lengths(regmatches(probe[1], gregexpr(";", probe[1]))) > lengths(regmatches(probe[1], gregexpr(",", probe[1])))) ";" else ","
  d <- readr::read_delim(f, delim = sep, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = enc), trim_ws = TRUE, progress = FALSE, show_col_types = FALSE)
  names(d) <- trimws(gsub('^"|"$', "", names(d)))
  d
}
uy_inspect <- function() {
  for (k in c("otros", "homicidios")) { f <- uy_file(k); if (is.na(f)) next; d <- uy_read(f)
    message(k, ": ", basename(f), " (", fmt_int(nrow(d)), " rows)\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
  f <- uy_file("seccionales"); if (!is.na(f)) print(utils::head(sf::st_drop_geometry(sf::st_read(f, quiet = TRUE)), 3))
  for (k in c("pop_est", "pop_proj")) { f <- uy_file(k); if (is.na(f) || !requireNamespace("readxl", quietly = TRUE)) next
    for (sh in readxl::excel_sheets(f)) { message(k, " / ", sh); print(utils::head(as.data.frame(readxl::read_excel(f, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal")), 8)) } }
}

# Standardise one microdata file to: offence_code, year, month, dept_id, secc_id
uy_standardise <- function(d, key, deps) {
  nm <- names(d); col <- lapply(UY_COLUMNS, uy_pick, nm = nm)
  if (is.na(col$dept) || (is.na(col$fecha) && (is.na(col$year) || is.na(col$month))))
    stop("Unexpected columns in the Uruguayan ", key, " file: ", paste(nm, collapse = " | "), "\nRun uy_inspect() and adjust UY_COLUMNS in R/84_uy.R")
  if (key == "otros" && is.na(col$delito)) stop("No DELITO column in the Uruguayan otros-delitos file: ", paste(nm, collapse = " | "))
  out <- data.frame(offence_code = if (key == "otros") normalize_text(d[[col$delito]]) else rep("HOMICIDIO DOLOSO CONSUMADO", nrow(d)), stringsAsFactors = FALSE)
  date <- if (!is.na(col$fecha)) { x <- trimws(d[[col$fecha]]); dt <- as.Date(x, format = "%d.%m.%Y")
    for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d")) { miss <- is.na(dt); if (!any(miss)) break; dt[miss] <- as.Date(x[miss], format = fmt) }; dt } else as.Date(NA)
  out$year <- as.integer(format(date, "%Y")); out$month <- as.integer(format(date, "%m"))
  if (!is.na(col$year)) { y <- suppressWarnings(as.integer(d[[col$year]])); out$year[is.na(out$year)] <- y[is.na(out$year)] }
  if (!is.na(col$month)) { mth <- suppressWarnings(as.integer(d[[col$month]])); mth[is.na(mth)] <- UY_MONTHS[normalize_text(d[[col$month]])[is.na(mth)]]; out$month[is.na(out$month)] <- mth[is.na(out$month)] }
  dcode <- deps$department_code[match(normalize_text(d[[col$dept]]), normalize_text(deps$department_name))]
  out$dept_id <- ifelse(is.na(dcode), NA, paste0("UY-", dcode))
  secc <- if (!is.na(col$juris)) suppressWarnings(as.integer(sub("^.*?([0-9]+).*$", "\\1", d[[col$juris]]))) else NA_integer_
  secc[!grepl("[0-9]", d[[col$juris]] %||% "")] <- NA
  out$secc_id <- ifelse(is.na(out$dept_id) | is.na(secc), NA, paste(out$dept_id, secc, sep = "-"))
  unknown <- setdiff(unique(normalize_text(d[[col$dept]])), c(normalize_text(deps$department_name), NA))
  if (length(unknown)) message("  ", key, ": DEPARTAMENTO values counted at national level only: ", paste(unknown, collapse = ", "))
  out[!is.na(out$year) & !is.na(out$month), ]
}

uy_ingest <- function(con) {
  deps <- uy_departamentos(); types <- read_reference("uy_offence_types.csv")
  rows <- list(); files <- character()
  for (k in c("otros", "homicidios")) {
    f <- uy_file(k); if (is.na(f)) { message("  no UY ", k, " file yet"); next }
    d <- uy_standardise(uy_read(f), k, deps); files <- c(files, basename(f))
    count <- function(id, level) { x <- d[!is.na(id), ]; x$territory_id <- id[!is.na(id)]
      x |> dplyr::count(territory_id, offence_code, year, month, name = "n") |> dplyr::mutate(country = "UY", level = level, file = basename(f)) }
    rows[[k]] <- dplyr::bind_rows(count(rep("UY", nrow(d)), "country"), count(d$dept_id, "region"), count(d$secc_id, "municipality"))
    message(sprintf("  %s: %s rows, %d departamentos, %d seccionales, %d-%02d to %d-%02d", k, fmt_int(nrow(d)), length(unique(stats::na.omit(d$dept_id))), length(unique(stats::na.omit(d$secc_id))),
                    min(d$year), min(d$month[d$year == min(d$year)]), max(d$year), max(d$month[d$year == max(d$year)])))
  }
  if (length(rows) == 0) stop("No Uruguayan file found in ", COUNTRY_MODULES$UY$dir_raw)
  crime <- dplyr::bind_rows(rows)
  new_codes <- setdiff(unique(crime$offence_code), types$source_code)
  if (length(new_codes)) message("  [!!] DELITO values not in reference/uy_offence_types.csv (not shown until mapped): ", paste(new_codes, collapse = ", "))
  # "total" = sum of the complaint categories (NOT homicide victims, NOT an official total)
  if (!is.null(rows$otros)) {
    keep <- !rows$otros$offence_code %in% types$source_code[types$offence_type_id == "unmapped"]
    tot <- rows$otros[keep, ] |> dplyr::group_by(country, territory_id, level, year, month, file) |> dplyr::summarise(n = sum(n), .groups = "drop") |> dplyr::mutate(offence_code = "TOTAL_DENUNCIAS")
    crime <- dplyr::bind_rows(crime, tot[, names(crime)])
  }
  file_of <- crime |> dplyr::distinct(territory_id, level, offence_code, year, file)
  cum <- monthly_to_cumulative(crime[, c("country", "territory_id", "level", "offence_code", "year", "month", "n")])
  cum <- dplyr::left_join(cum, file_of, by = c("territory_id", "level", "offence_code", "year"))
  if (nrow(cum) == 0) stop("No complete quarter in the Uruguayan files")
  secc <- unique(cum$territory_id[cum$level == "municipality"])
  ensure_territories(con, "UY", secc, "municipality", sub("-[^-]+$", "", secc), paste("Seccional", sub("^.*-", "", secc)))
  replace_country_rows(con, "stg_country_crime", "UY", cum[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_UY", paste(files, collapse = ";"), NA, paste(range(cum$year), collapse = "-"), nrow(cum))
  invisible(cum)
}

# Attributes of the KML: the Ministry's export keeps them in an HTML table inside <description>
uy_kml_attr <- function(k, key) {
  if (key %in% names(k)) return(trimws(as.character(k[[key]])))
  desc <- if ("Description" %in% names(k)) k$Description else if ("description" %in% names(k)) k$description else rep("", nrow(k))
  m <- regmatches(desc, regexec(paste0("<td[^>]*>\\s*", key, "\\s*</td>\\s*<td[^>]*>([^<]*)</td>"), desc, ignore.case = TRUE))
  vapply(m, function(x) if (length(x) >= 2) trimws(x[2]) else NA_character_, character(1))
}

# Territories: departamentos from the reference; seccionales (and departamento polygons as
# their union) from the Ministry's KML; geoBoundaries ADM1 if the KML cannot be read.
uy_territory <- function(con) {
  deps <- uy_departamentos()
  tbl <- data.frame(territory_id = paste0("UY-", deps$department_code), level = "region", parent_id = "UY", official_code = deps$department_code, name = deps$department_name)
  layers <- list()
  f <- uy_file("seccionales")
  k <- if (!is.na(f)) tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) { message("  KML not readable: ", conditionMessage(e)); NULL }) else NULL
  if (!is.null(k) && nrow(k) > 0) {
    dname <- uy_kml_attr(k, "DEPARTAMEN"); secc <- suppressWarnings(as.integer(uy_kml_attr(k, "SECCION")))
    dcode <- deps$department_code[match(normalize_text(dname), normalize_text(deps$department_name))]
    ok <- !is.na(dcode) & !is.na(secc)
    if (!any(ok)) { message("  seccionales KML: attributes DEPARTAMEN/SECCION not found (columns: ", paste(names(k), collapse = ", "), ")"); k <- NULL }
    else {
      k <- sf::st_zm(sf::st_make_valid(sf::st_transform(k[ok, ], 4326)))
      k$territory_id <- paste("UY", dcode[ok], secc[ok], sep = "-"); k$name <- paste("Seccional", secc[ok])
      k <- k |> dplyr::group_by(territory_id, name) |> dplyr::summarise(.groups = "drop")        # a seccional split in several polygons -> one
      tbl <- rbind(tbl, data.frame(territory_id = k$territory_id, level = "municipality", parent_id = sub("-[^-]+$", "", k$territory_id), official_code = sub("^.*-", "", k$territory_id), name = k$name))
      gd <- k; gd$territory_id <- sub("-[^-]+$", "", gd$territory_id); gd <- gd |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop")
      gd$name <- deps$department_name[match(sub("^UY-", "", gd$territory_id), deps$department_code)]
      layers <- list(region = gd, municipality = k[, c("territory_id", "name", attr(k, "sf_column"))])
      message(sprintf("  %d seccionales, %d departamentos built from the KML", nrow(k), nrow(gd)))
    }
  }
  if (is.null(layers$region)) {
    g <- tryCatch(geoboundaries("URY", 1), error = function(e) { message("  geoBoundaries ADM1 not available: ", conditionMessage(e)); NULL })
    if (!is.null(g)) { g$territory_id <- paste0("UY-", deps$department_code[match(normalize_text(g$shapeName), normalize_text(deps$department_name))]); g$name <- deps$department_name[match(g$territory_id, paste0("UY-", deps$department_code))]
      layers$region <- g[!is.na(g$name), c("territory_id", "name", attr(g, "sf_column"))] }
  }
  save_country_territory(con, "UY", tbl, scheme = "INE departamento + seccional policial")
  if (length(layers)) save_country_gpkg(con, "UY", layers, tolerance_m = 100)
  invisible(tbl)
}

# INE workbooks: a header row with the years as columns; departamento either as the sheet name,
# as a row with the figures (one departamento per row) or as a title row followed by age/sex
# rows and a "Total" row. The first "Total" after a departamento title is taken.
uy_read_ine <- function(f, deps) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is needed for the INE files")
  num <- function(x) { x <- gsub("\\s", "", ifelse(is.na(x), "", x)); x <- ifelse(grepl("^[0-9]{1,3}(\\.[0-9]{3})+$", x), gsub(".", "", x, fixed = TRUE), x)
    x <- ifelse(grepl(",", x) & grepl("\\.", x), gsub(".", "", x, fixed = TRUE), x); suppressWarnings(as.numeric(gsub(",", ".", x))) }
  is_year <- function(x) { v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", ifelse(is.na(x), "", x)))); !is.na(v) & v == floor(v) & v >= 1990 & v <= 2080 & nchar(gsub("[^0-9]", "", ifelse(is.na(x), "", x))) <= 6 }
  dkeys <- normalize_text(deps$department_name); out <- list()
  for (sh in readxl::excel_sheets(f)) {
    s <- as.data.frame(suppressMessages(readxl::read_excel(f, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal")), stringsAsFactors = FALSE)
    if (nrow(s) < 2) next
    hdr <- which(apply(s, 1, function(r) sum(is_year(r))) >= 3)[1]; if (is.na(hdr)) next
    ycols <- which(is_year(unlist(s[hdr, ]))); years <- floor(as.numeric(gsub("[^0-9.]", "", unlist(s[hdr, ycols])))); other <- setdiff(seq_len(ncol(s)), ycols)
    body <- s[-(1:hdr), , drop = FALSE]
    lab <- apply(body[, other, drop = FALSE], 1, function(r) { r <- r[!is.na(r) & nzchar(trimws(r))]; if (length(r)) r[1] else NA })
    cur <- deps$department_code[match(normalize_text(sh), dkeys)]; pending <- !is.na(cur)
    for (i in seq_len(nrow(body))) {
      key <- normalize_text(lab[i]); v <- num(unlist(body[i, ycols]))
      if (!is.na(key) && key %in% dkeys) { cur <- deps$department_code[match(key, dkeys)]; pending <- TRUE
        if (sum(!is.na(v)) >= 1) { out[[length(out) + 1]] <- data.frame(code = cur, year = years[!is.na(v)], population = v[!is.na(v)]); pending <- FALSE } }
      else if (!is.na(key) && grepl("^TOTAL", key) && pending && !is.na(cur) && any(!is.na(v))) { out[[length(out) + 1]] <- data.frame(code = cur, year = years[!is.na(v)], population = v[!is.na(v)]); pending <- FALSE }
    }
  }
  if (length(out) == 0) stop("No departamento population recognised in ", basename(f), " (run uy_inspect())")
  dplyr::bind_rows(out) |> dplyr::distinct(code, year, .keep_all = TRUE)
}

uy_population <- function(con) {
  deps <- uy_departamentos(); parts <- list()
  for (k in c("pop_est", "pop_proj")) { f <- uy_file(k); if (is.na(f)) { message("  no INE ", k, " file yet"); next }
    parts[[k]] <- tryCatch(uy_read_ine(f, deps), error = function(e) { message("  ", conditionMessage(e)); NULL }) }
  pop <- dplyr::bind_rows(parts)
  if (nrow(pop) == 0) stop("No INE population file could be read (data/raw/country_uy/*uy_ine_*.xlsx)")
  pop <- pop |> dplyr::distinct(code, year, .keep_all = TRUE) |> dplyr::filter(population > 1000)   # drop rates / percentages read by mistake
  pop <- data.frame(country = "UY", territory_id = paste0("UY-", pop$code), level = "region", year = as.integer(pop$year), population = round(pop$population))
  full <- pop |> dplyr::group_by(year) |> dplyr::filter(dplyr::n() == nrow(deps)) |> dplyr::ungroup()    # national = sum of the 19 departamentos
  nat <- full |> dplyr::group_by(country, year) |> dplyr::summarise(population = sum(population), .groups = "drop") |> dplyr::mutate(territory_id = "UY", level = "country")
  pop <- dplyr::bind_rows(pop, nat[, names(pop)])
  replace_country_rows(con, "ref_country_population", "UY", pop)
  message(sprintf("  population: %d departamentos, %d-%d (INE estimates and projections; no population by seccional)", length(unique(pop$territory_id[pop$level == "region"])), min(pop$year), max(pop$year)))
}

register_country(list(
  code = "UY", iso3 = "URY", name_en = "Uruguay", name_local = "Uruguay", lang = "es",
  levels = c("region", "municipality"),
  level_labels = list(region = c("Departments", "Departamentos", "Department", "Departamento"),
                      municipality = c("Police precincts", "Seccionales policiales", "Police precinct", "Seccional policial")),
  source = list(name = "Delitos denunciados en el Uruguay (Ministerio del Interior)", publisher = "Ministerio del Interior, Observatorio de Violencia y Criminalidad",
                url = "https://catalogodatos.gub.uy/dataset/ministerio-del-interior-delitos_denunciados_en_el_uruguay",
                licence = "Licencia de Datos Abiertos del Gobierno de Uruguay (Licencia DAG)",
                measure = "Complaints (denuncias) registered in the Sistema de Gestión de Seguridad Pública for rapiña, hurto, lesiones, violencia doméstica and abigeato, attempts included, by month of the event; no official total (the total shown is their sum). Homicides are victims and are kept apart."),
  population_source = "INE Uruguay, estimaciones (1996-2023) y proyecciones (2024-2045) de población por departamento, revisión 2025",
  boundaries_attribution = "Ministerio del Interior, Seccionales Policiales (Licencia DAG); geoBoundaries (CC BY 4.0) as fallback",
  measure_labels = list(kind = "complaints", title = c("Reported offences (complaints)", "Delitos denunciados"), noun = c("complaints", "denuncias")),
  not_published = c("Not published", "No publicado"),
  notes_en = "Seccional boundaries changed over the period and some complaints have no seccional (counted in the departamento only); rates exist for departamentos only (no population by seccional); homicide victims are a separate series.",
  method = list(where = "Seccional policial (police jurisdiction) recorded in the complaint; complaints without a seccional count in the departamento only", date = "Month of the event (fecha del hecho) as stated in the complaint", revisions = "The Observatorio republishes the series; recent months are updated"),
  default_region = "UY-1",
  download = uy_download, ingest = uy_ingest, territory = uy_territory, population = uy_population
))
