# Peru: police complaints (denuncias) registered in SIDPOL, by distrito ----------------------
# Source: Ministerio del Interior (MININTER) / Policía Nacional del Perú (DIRTIC), dataset
# "Denuncias Policiales" on the Plataforma Nacional de Datos Abiertos: one CSV with the monthly
# number of complaints by distrito (6-digit ubigeo) and "modalidad" (Robo, Hurto, Estafa,
# Extorsión, Secuestro, Violencia contra la mujer..., Otros), January 2018 onwards, refreshed
# monthly. Licence: Open Data Commons Attribution License (ODC-BY), per the metadata document.
# Columns (official data dictionary, April 2026):
#   ANIO | MES | DPTO_HECHO_NEW | PROV_HECHO | DIST_HECHO | UBIGEO_HECHO | P_MODALIDADES | cantidad
# Levels: region = departamento (24 + Callao, 2-digit ubigeo), province = provincia (196,
# 4 digits), municipality = distrito (~1,870, 6 digits). Lima Metropolitana is the provincia de
# Lima (1501: 43 distritos) inside the departamento de Lima; Callao is departamento 07.
# Population: MININTER "Indicadores de seguridad ciudadana" (POBLACION_PROYECTADA by ámbito,
# ubigeo and year, INEI projections), same portal. Boundaries: OCHA COD (IGN, P-codes = PE +
# ubigeo, CC BY-IGO), else geoBoundaries ADM1-3 matched by name.
#
# The portal answers HTTP 418 to automated clients: the module tries the direct links in
# reference/pe_sources.csv and otherwise asks for a manual download into data/raw/country_pe/
# (the CSV, or the dataset zip the portal builds, are both accepted).
#
# Caveats shown in the app: complaints (what people report at a police station), not verified
# offences; no homicides in this file; "Otros" is about a third of the rows and is not broken
# down; several level shifts from registration changes (the largest in November 2024, when
# virtual complaints stopped being counted); a complaint is counted where it is filed.

PE_COLUMNS <- list(
  year = c("^ANIO$", "^A[ÑN]O$", "^YEAR$"), month = c("^MES$", "^MONTH$"),
  dpto = c("^DPTO_HECHO", "^DPTO", "^DEPARTAMENTO", "^REGION"), prov = c("^PROV_HECHO", "^PROV"), dist = c("^DIST_HECHO", "^DIST"),
  ubigeo = c("^UBIGEO_HECHO$", "^UBIGEO"), type = c("^P_MODALIDADES$", "MODALIDAD"), n = c("^cantidad$", "^CANTIDAD$", "^N$", "^TOTAL$"),
  # indicators file (population)
  ind_ambito = c("^AMBITO$"), ind_ubigeo = c("^UBIGEO_DASH$", "^UBIGEO"), ind_year = c("^ANIO$", "^A[ÑN]O$"), ind_pop = c("^POBLACION_PROYECTADA$", "^POBLACION")
)
PE_STATE <- new.env()

pe_dir <- function() COUNTRY_MODULES$PE$dir_raw
pe_sources <- function() read_reference("pe_sources.csv")
pe_source <- function(key) { s <- pe_sources(); u <- s$url[s$key == key]; if (length(u) && !is.na(u) && nzchar(trimws(u))) trimws(u) else NA_character_ }
pe_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }
pe_departamentos <- function() read_reference("pe_departamentos.csv")
pe_pad <- function(x, width) { d <- gsub("\\D", "", as.character(x)); d[!nzchar(d)] <- NA; ifelse(is.na(d), NA, formatC(suppressWarnings(as.integer(d)), width = width, flag = "0", format = "d")) }

# --- Files -------------------------------------------------------------------------------------

# Newest file whose name matches (CSV or zip); files inside the portal's dataset zip are extracted to a temp dir
pe_file <- function(pattern) { f <- newest_first(list.files(pe_dir(), pattern, full.names = TRUE, ignore.case = TRUE)); if (length(f)) f[1] else NA_character_ }
pe_unzip <- function(zip, inner_pattern) {
  tmp <- file.path(tempdir(), paste0("pe_", tools::file_path_sans_ext(basename(zip)))); unlink(tmp, recursive = TRUE); dir.create(tmp)
  utils::unzip(zip, exdir = tmp)
  inner_zips <- list.files(tmp, "\\.zip$", full.names = TRUE, recursive = TRUE)
  for (z in inner_zips) utils::unzip(z, exdir = file.path(tmp, tools::file_path_sans_ext(basename(z))))
  f <- list.files(tmp, inner_pattern, full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  if (!length(f)) return(NA_character_)
  f[which.max(file.size(f))]
}
# The complaints CSV: a CSV with "denuncias" in its name, or inside a zip with "denuncias" in its name
pe_complaints_file <- function() {
  f <- pe_file("denuncias.*\\.csv$"); if (!is.na(f)) return(f)
  z <- pe_file("denuncias.*\\.zip$"); if (!is.na(z)) return(pe_unzip(z, "denuncias.*\\.csv$"))
  NA_character_
}
# The indicators CSV (population): "Ind_Plan"/"Plan_Acc"/"indicadores" in the name, CSV or (nested) zip
pe_indicators_file <- function() {
  f <- pe_file("(ind_?_?plan|plan_acc|indicadores).*\\.csv$"); if (!is.na(f)) return(f)
  z <- pe_file("(ind_?_?plan|plan_acc|indicadores).*\\.zip$"); if (!is.na(z)) return(pe_unzip(z, "(ind_?_?plan|plan_acc|indicadores).*\\.csv$"))
  NA_character_
}

pe_manual_note <- function(what, page) sprintf(paste0("Download it by hand: open %s in a browser, click the %s file and save it in %s (the CSV or the dataset zip, keep the original name). ",
                                                      "Then run the update again (menu option 14, code PE)."), page, what, pe_dir())

# Download: the direct links of reference/pe_sources.csv (refused with HTTP 418 by the portal most of
# the time); nothing is downloaded again while a copy is less than `days` old
pe_download <- function(days = 30) {
  m <- COUNTRY_MODULES$PE
  try_get <- function(key, name, have) {
    if (!is.na(have) && difftime(Sys.time(), file.mtime(have), units = "days") < days) { message("= PE ", key, " downloaded less than ", days, " days ago"); return(invisible()) }
    url <- pe_source(key); if (is.na(url)) return(invisible())
    tryCatch(download_versioned(url, m$dir_raw, name = name),
             error = function(e) message("  PE ", key, ": the portal refused the automated download (", conditionMessage(e), ")\n  ", pe_manual_note(if (key == "denuncias") "CSV" else "zip", pe_source(paste0("page_", key)))))
  }
  try_get("denuncias", "pe_denuncias.csv", pe_file("denuncias.*\\.(csv|zip)$"))
  try_get("indicadores", "pe_indicadores.zip", pe_file("(ind_?_?plan|plan_acc|indicadores).*\\.(csv|zip)$"))
  # Boundaries: OCHA COD (P-codes) first, geoBoundaries as fallback (both cached by their own helpers)
  # geoBoundaries has no ADM3 for Peru: the distritos need the OCHA COD (IGN). First the HDX shapefile zip
  # (CKAN API, or a copy downloaded by hand into data/raw/country_pe/), then the ITOS FeatureServer
  if (is.na(pe_hdx_file())) tryCatch(pe_hdx_download(), error = function(e) message("  HDX COD zip not available (", conditionMessage(e), ")"))
  if (is.na(pe_hdx_file())) tryCatch(pe_cod_layers(), error = function(e) message("  COD service not available (", conditionMessage(e), "). Distritos stay as tables: download https://data.humdata.org/dataset/cod-ab-per (file per_adm_ign_*_SHP.zip) by hand into ", pe_dir()))
}

# --- Reading -----------------------------------------------------------------------------------

pe_read_csv <- function(f) {
  probe <- readLines(f, n = 50, warn = FALSE)
  enc <- if (all(validUTF8(probe))) "UTF-8" else "latin1"
  delim <- if (sum(gregexpr(";", probe[1])[[1]] > 0) > sum(gregexpr(",", probe[1])[[1]] > 0)) ";" else ","
  readr::read_delim(f, delim = delim, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = enc), trim_ws = TRUE,
                    progress = FALSE, show_col_types = FALSE, na = character())
}

# Complaints as a tidy table: ubigeo (6), dpto_code (2), prov_code (4), names, year, month, type, n
pe_read_complaints <- function(f) {
  d <- pe_read_csv(f); nm <- names(d); col <- lapply(PE_COLUMNS, pe_pick, nm = nm)
  need <- c("year", "month", "ubigeo", "type", "n")
  if (any(is.na(unlist(col[need])))) stop("Unexpected columns in ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun pe_inspect() and adjust PE_COLUMNS in R/93_pe.R")
  out <- data.frame(ubigeo = pe_pad(d[[col$ubigeo]], 6), year = suppressWarnings(as.integer(d[[col$year]])), month = suppressWarnings(as.integer(d[[col$month]])),
                    type = trimws(d[[col$type]]), n = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", d[[col$n]]))), stringsAsFactors = FALSE)
  out$dpto_name <- if (!is.na(col$dpto)) trimws(d[[col$dpto]]) else NA_character_
  out$prov_name <- if (!is.na(col$prov)) trimws(d[[col$prov]]) else NA_character_
  out$dist_name <- if (!is.na(col$dist)) trimws(d[[col$dist]]) else NA_character_
  out <- out[!is.na(out$ubigeo) & !is.na(out$year) & !is.na(out$month) & out$month %in% 1:12 & !is.na(out$n), ]
  out$dpto_code <- substr(out$ubigeo, 1, 2); out$prov_code <- substr(out$ubigeo, 1, 4)
  out
}

pe_inspect <- function() {
  f <- pe_complaints_file()
  if (is.na(f)) message("no complaints file yet in ", pe_dir(), "\n", pe_manual_note("CSV", pe_source("page_denuncias")))
  else { d <- pe_read_csv(f); message(basename(f), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 5))
    col <- pe_pick(names(d), PE_COLUMNS$type); if (!is.na(col)) { types <- read_reference("pe_offence_types.csv"); u <- setdiff(unique(trimws(d[[col]])), types$source_code)
      message("  modalidades: ", paste(sort(unique(d[[col]])), collapse = " / "), if (length(u)) paste0("\n  not in pe_offence_types.csv: ", paste(u, collapse = " / ")) else "") } }
  g <- pe_indicators_file()
  if (is.na(g)) message("no indicators (population) file yet in ", pe_dir())
  else { d <- pe_read_csv(g); message(basename(g), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 5)) }
  if (length(PE_STATE$unmatched)) { message("districts without boundaries (first 30): "); print(utils::head(PE_STATE$unmatched, 30)) }
}

# --- Territories and boundaries ----------------------------------------------------------------

pe_ids <- function(ubigeo) paste("PE", substr(ubigeo, 1, 2), substr(ubigeo, 1, 4), ubigeo, sep = "-")
pe_prov_ids <- function(prov_code) paste("PE", substr(prov_code, 1, 2), prov_code, sep = "-")
pe_title <- function(x) { x <- tolower(x); gsub("(^|[ -])(\\w)", "\\1\\U\\2", x, perl = TRUE) }

# OCHA COD FeatureServer: one GeoJSON per admin level, paged (ArcGIS caps a query at ~1,000-2,000 features)
pe_cod_layers <- function() {
  base <- pe_source("cod_service"); if (is.na(base)) stop("no COD service URL")
  dir <- file.path(pe_dir(), "cod"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (lv in 1:3) {
    have <- newest_first(list.files(dir, sprintf("_pe_cod_adm%d\\.geojson$", lv), full.names = TRUE))
    if (length(have) && difftime(Sys.time(), file.mtime(have[1]), units = "days") < 3650) next
    meta <- get_text(paste0(base, "?f=pjson")); if (is.null(meta)) stop("COD service not reachable")
    layers <- jsonlite::fromJSON(meta)$layers
    if (is.null(layers)) stop("COD service has no layers")
    i <- grep(sprintf("adm(in)?_?%d", lv), layers$name, ignore.case = TRUE)
    if (!length(i)) stop("no ADM", lv, " layer in the COD service (", paste(layers$name, collapse = ", "), ")")
    feats <- list(); offset <- 0
    repeat {
      url <- sprintf("%s/%d/query?where=1%%3D1&outFields=*&outSR=4326&f=geojson&resultOffset=%d&resultRecordCount=1000", base, layers$id[i[1]], offset)
      txt <- get_text(url, timeout = 300); if (is.null(txt)) stop("COD query failed for ADM", lv)
      js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
      n <- length(js$features); if (n == 0) break
      feats <- c(feats, js$features); offset <- offset + n
      if (n < 1000 && !isTRUE(js$properties$exceededTransferLimit)) break
    }
    tmp <- file.path(tempdir(), sprintf("pe_cod_adm%d.geojson", lv))
    writeLines(jsonlite::toJSON(list(type = "FeatureCollection", features = feats), auto_unbox = TRUE, null = "null", digits = NA), tmp)
    keep_version(tmp, dir, sprintf("pe_cod_adm%d.geojson", lv))
    message(sprintf("  COD ADM%d: %d features", lv, length(feats)))
  }
}
# OCHA COD shapefile zip from HDX (per_adm_ign_YYYYMMDD_SHP.zip, CC BY-IGO): found through the CKAN API
pe_hdx_file <- function() { f <- newest_first(list.files(pe_dir(), "per_adm.*shp.*\\.zip$", full.names = TRUE, ignore.case = TRUE)); f <- f[file.size(f) > 1000]; if (length(f)) f[1] else NA_character_ }
pe_hdx_download <- function() {
  js <- get_text("https://data.humdata.org/api/3/action/package_show?id=cod-ab-per"); if (is.null(js)) stop("HDX API not reachable")
  res <- jsonlite::fromJSON(js, simplifyVector = FALSE)$result$resources
  urls <- vapply(res, function(r) r$download_url %||% r$url %||% "", ""); nm <- vapply(res, function(r) r$name %||% "", "")
  i <- which(grepl("SHP\\.zip$", nm, ignore.case = TRUE) | grepl("SHP\\.zip$", urls, ignore.case = TRUE))
  if (!length(i)) stop("no shapefile zip among the HDX resources (", paste(nm, collapse = ", "), ")")
  i <- i[order(!grepl("per_adm_ign", paste(nm[i], urls[i]), ignore.case = TRUE))]     # the IGN 2020 file first; an older empty resource exists
  for (k in i) {
    f <- tryCatch(download_versioned(urls[k], pe_dir(), name = basename(sub("\\?.*$", "", urls[k]))), error = function(e) { message("  HDX resource ", nm[k], ": ", conditionMessage(e)); NULL })
    if (!is.null(f)) return(f)
  }
  stop("none of the HDX shapefile resources could be downloaded")
}
pe_cod_read <- function(lv) {
  z <- pe_hdx_file()
  if (!is.na(z)) {   # shapefile inside the HDX zip: per_admbnda_adm<lv>_ign_<date>.shp
    tmp <- file.path(tempdir(), "pe_hdx"); if (!dir.exists(tmp) || !length(list.files(tmp, "\\.shp$", recursive = TRUE))) { unlink(tmp, recursive = TRUE); dir.create(tmp); utils::unzip(z, exdir = tmp) }
    shp <- list.files(tmp, sprintf("adm(in|bnda_adm)?%d[^0-9].*\\.shp$|adm(in)?%d\\.shp$", lv, lv), full.names = TRUE, recursive = TRUE, ignore.case = TRUE)   # per_admbnda_adm3_... or per_admin3.shp
    if (!length(shp)) return(NULL)
    g <- sf::st_read(shp[1], quiet = TRUE); if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  } else {
    f <- newest_first(list.files(file.path(pe_dir(), "cod"), sprintf("_pe_cod_adm%d\\.geojson$", lv), full.names = TRUE))
    if (!length(f)) return(NULL)
    g <- sf::st_read(f[1], quiet = TRUE); if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  }
  pc <- grep(sprintf("^adm(in)?%d.?p.?cod|^IDDIST$|^IDPROV$|^IDDPTO$|^UBIGEO$", lv), names(g), ignore.case = TRUE, value = TRUE)[1]   # ADM3_PCODE, admin3Pcod, IDDIST...
  if (is.na(pc)) return(NULL)
  g$code <- pe_pad(g[[pc]], c(2, 4, 6)[lv])
  nmc <- grep(sprintf("^adm(in)?%d.?(_es|_en|name|na$)|^NOMBDIST$|^NOMBPROV$|^NOMBDEP$", lv), names(g), ignore.case = TRUE, value = TRUE)[1]
  g$name <- if (!is.na(nmc)) g[[nmc]] else g$code
  g[!is.na(g$code), c("code", "name", attr(g, "sf_column"))]
}

# Territory table: departamentos from the reference; provincias and distritos from the complaints
# file (official ubigeo + names) and the boundary layers. Boundaries: COD by P-code, else
# geoBoundaries by name (departamento > provincia > distrito).
pe_territory <- function(con) {
  deps <- pe_departamentos()
  tbl <- data.frame(territory_id = paste0("PE-", deps$department_code), level = "region", parent_id = "PE", official_code = deps$department_code, name = deps$department_name, stringsAsFactors = FALSE)
  f <- pe_complaints_file()
  if (!is.na(f)) {
    d <- pe_read_complaints(f); PE_STATE$complaints <- d
    pr <- d[!duplicated(d$prov_code), ]; pr <- pr[pr$dpto_code %in% deps$department_code, ]
    tbl <- rbind(tbl, data.frame(territory_id = pe_prov_ids(pr$prov_code), level = "province", parent_id = paste0("PE-", pr$dpto_code), official_code = pr$prov_code,
                                 name = ifelse(is.na(pr$prov_name) | pr$prov_name == "", pr$prov_code, pe_title(pr$prov_name))))
    di <- d[!duplicated(d$ubigeo), ]; di <- di[di$dpto_code %in% deps$department_code, ]
    tbl <- rbind(tbl, data.frame(territory_id = pe_ids(di$ubigeo), level = "municipality", parent_id = pe_prov_ids(di$prov_code), official_code = di$ubigeo,
                                 name = ifelse(is.na(di$dist_name) | di$dist_name == "", di$ubigeo, pe_title(di$dist_name))))
  } else message("  no complaints file yet: provincias and distritos are created at the first ingest")
  layers <- list()
  # 1. COD with P-codes
  cod <- lapply(1:3, function(lv) tryCatch(pe_cod_read(lv), error = function(e) NULL))
  known <- tbl$official_code[tbl$level == "municipality"]
  use_cod <- !is.null(cod[[3]]) && nrow(cod[[3]]) > 1000 && (length(known) == 0 || mean(cod[[3]]$code %in% known) > 0.8)
  if (!use_cod && !is.na(pe_hdx_file())) {   # say why the HDX zip is not usable (so the pattern or the field can be adjusted)
    z <- pe_hdx_file(); tmp <- file.path(tempdir(), "pe_hdx"); if (!dir.exists(tmp)) { dir.create(tmp); utils::unzip(z, exdir = tmp) }
    shps <- list.files(tmp, "\\.shp$", recursive = TRUE, ignore.case = TRUE)
    if (is.null(cod[[3]])) { shp3 <- grep("adm(in)?3", shps, ignore.case = TRUE, value = TRUE)
      if (!length(shp3)) message("  HDX zip ", basename(z), ": no ADM3 shapefile inside (files: ", paste(utils::head(shps, 6), collapse = ", "), ")")
      else { g <- sf::st_read(file.path(tmp, shp3[1]), quiet = TRUE); message("  HDX zip ", basename(z), ": ADM3 layer read but no ADM3_PCODE field (fields: ", paste(names(g), collapse = ", "), ")") } }
    else message(sprintf("  HDX zip %s: %d ADM3 features, %.0f%% of their codes match the ubigeos of the complaints (e.g. codes %s vs ubigeos %s): not used",
                         basename(z), nrow(cod[[3]]), 100 * mean(cod[[3]]$code %in% known), paste(utils::head(cod[[3]]$code, 3), collapse = ", "), paste(utils::head(known, 3), collapse = ", ")))
  }
  if (use_cod) {
    g1 <- cod[[1]]; g2 <- cod[[2]]; g3 <- cod[[3]]
    # Provincias and distritos the complaints file does not name yet (or no file at all) come from the layers
    if (!is.null(g2)) { new2 <- g2[!pe_prov_ids(g2$code) %in% tbl$territory_id & substr(g2$code, 1, 2) %in% deps$department_code, ]
      if (nrow(new2)) tbl <- rbind(tbl, data.frame(territory_id = pe_prov_ids(new2$code), level = "province", parent_id = paste0("PE-", substr(new2$code, 1, 2)), official_code = new2$code, name = pe_title(new2$name))) }
    new3 <- g3[!pe_ids(g3$code) %in% tbl$territory_id & substr(g3$code, 1, 2) %in% deps$department_code, ]
    if (nrow(new3)) tbl <- rbind(tbl, data.frame(territory_id = pe_ids(new3$code), level = "municipality", parent_id = pe_prov_ids(substr(new3$code, 1, 4)), official_code = new3$code, name = pe_title(new3$name)))
    g3$territory_id <- pe_ids(g3$code); layers$municipality <- g3[, c("territory_id", "name", attr(g3, "sf_column"))]
    if (!is.null(g2)) { g2$territory_id <- pe_prov_ids(g2$code); layers$province <- g2[, c("territory_id", "name", attr(g2, "sf_column"))] }
    if (!is.null(g1)) { g1$territory_id <- paste0("PE-", g1$code); layers$region <- g1[, c("territory_id", "name", attr(g1, "sf_column"))] }
    message(sprintf("  boundaries: OCHA COD (IGN) with P-codes, %d distritos", nrow(g3)))
  } else {
    # 2. geoBoundaries, matched by name level by level
    gb <- function(adm) tryCatch(geoboundaries("PER", adm), error = function(e) { message("  geoBoundaries ADM", adm, " not available (", conditionMessage(e), ")"); NULL })
    key <- function(x) { k <- normalize_text(x); k <- gsub("[^A-Z0-9 ]", "", k); k }
    g1 <- gb(1)
    if (!is.null(g1)) {
      alias <- do.call(rbind, lapply(seq_len(nrow(deps)), function(i) data.frame(code = deps$department_code[i], k = key(c(deps$department_name[i], strsplit(deps$source_names[i], ";")[[1]])))))
      g1$code <- alias$code[match(key(g1$shapeName), alias$k)]; g1 <- g1[!is.na(g1$code), ]
      g1$territory_id <- paste0("PE-", g1$code); g1$name <- deps$department_name[match(g1$code, deps$department_code)]
      layers$region <- g1[, c("territory_id", "name", attr(g1, "sf_column"))]
    }
    provs <- tbl[tbl$level == "province", ]; dists <- tbl[tbl$level == "municipality", ]
    g2 <- if (nrow(provs)) gb(2) else NULL
    if (!is.null(g2) && !is.null(layers$region)) {
      g2 <- sf::st_make_valid(sf::st_transform(g2, 4326)); reg <- sf::st_transform(layers$region, 4326)
      inside <- sf::st_within(suppressWarnings(sf::st_point_on_surface(g2)), reg, sparse = TRUE)
      g2$dpto <- vapply(inside, function(i) if (length(i)) sub("^PE-", "", reg$territory_id[i[1]]) else NA_character_, character(1))
      g2$territory_id <- provs$territory_id[match(paste(g2$dpto, key(g2$shapeName)), paste(substr(provs$official_code, 1, 2), key(provs$name)))]
      g2 <- g2[!is.na(g2$territory_id) & !duplicated(g2$territory_id), ]; g2$name <- provs$name[match(g2$territory_id, provs$territory_id)]
      layers$province <- g2[, c("territory_id", "name", attr(g2, "sf_column"))]
      g3 <- if (nrow(dists)) gb(3) else NULL
      if (!is.null(g3)) {
        g3 <- sf::st_make_valid(sf::st_transform(g3, 4326))
        inside <- sf::st_within(suppressWarnings(sf::st_point_on_surface(g3)), g2, sparse = TRUE)
        g3$prov <- vapply(inside, function(i) if (length(i)) g2$territory_id[i[1]] else NA_character_, character(1))
        g3$territory_id <- dists$territory_id[match(paste(g3$prov, key(g3$shapeName)), paste(dists$parent_id, key(dists$name)))]
        g3 <- g3[!is.na(g3$territory_id) & !duplicated(g3$territory_id), ]; g3$name <- dists$name[match(g3$territory_id, dists$territory_id)]
        layers$municipality <- g3[, c("territory_id", "name", attr(g3, "sf_column"))]
        PE_STATE$unmatched <- dists[!dists$territory_id %in% g3$territory_id, c("territory_id", "name")]
        message(sprintf("  boundaries: geoBoundaries (CC BY 4.0) matched by name: %d of %d provincias, %d of %d distritos", nrow(g2), nrow(provs), nrow(g3), nrow(dists)))
      }
    }
  }
  save_country_territory(con, "PE", tbl, scheme = "Ubigeo (INEI)")
  if (length(layers)) save_country_gpkg(con, "PE", layers, tolerance_m = 100) else message("  no boundaries: tables only")
  invisible(tbl)
}

# --- Complaints ----------------------------------------------------------------------------------

pe_ingest <- function(con) {
  f <- pe_complaints_file()
  if (is.na(f)) stop("No complaints file in ", pe_dir(), ".\n  ", pe_manual_note("CSV", pe_source("page_denuncias")))
  d <- PE_STATE$complaints %||% pe_read_complaints(f)
  deps <- pe_departamentos(); d <- d[d$dpto_code %in% deps$department_code, ]
  types <- read_reference("pe_offence_types.csv")
  d$offence_code <- types$source_code[match(normalize_text(d$type), normalize_text(types$source_code))]
  unknown <- unique(d$type[is.na(d$offence_code)])
  if (length(unknown)) message("  modalidades not in pe_offence_types.csv (ignored until mapped): ", paste(unknown, collapse = " / "))
  d <- d[!is.na(d$offence_code), ]
  monthly <- function(x, id, lv) { x$territory_id <- as.character(id); x$level <- rep(lv, nrow(x))
    x |> dplyr::group_by(territory_id, level, offence_code, year, month) |> dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop") }
  m_dist <- monthly(d, pe_ids(d$ubigeo), "municipality")
  m_prov <- monthly(d, pe_prov_ids(d$prov_code), "province")
  m_dep  <- monthly(d, paste0("PE-", d$dpto_code), "region")
  m_nat  <- monthly(d, "PE", "country")
  all <- dplyr::bind_rows(m_dist, m_prov, m_dep, m_nat)
  # Total = sum of the published modalidades (the file holds every complaint, "Otros" included)
  counted <- types$source_code[types$offence_type_id != "unmapped" & types$offence_type_id != "total"]
  tot <- all[all$offence_code %in% counted, ] |> dplyr::group_by(territory_id, level, year, month) |> dplyr::summarise(n = sum(n), .groups = "drop")
  tot$offence_code <- rep("TOTAL_DENUNCIAS", nrow(tot))
  all <- dplyr::bind_rows(all, tot[, names(all)]); all$country <- "PE"
  crime <- monthly_to_cumulative(all[, c("country", "territory_id", "level", "offence_code", "year", "month", "n")])
  if (nrow(crime) == 0) stop("No complete quarter in ", basename(f))
  crime$file <- basename(f)
  # Territories named in the file but missing (file newer than the territory step)
  ensure_territories(con, "PE", unique(pe_prov_ids(d$prov_code)), "province", paste0("PE-", substr(unique(d$prov_code), 1, 2)), pe_title(d$prov_name[match(unique(d$prov_code), d$prov_code)]))
  ensure_territories(con, "PE", unique(pe_ids(d$ubigeo)), "municipality", pe_prov_ids(substr(unique(d$ubigeo), 1, 4)), pe_title(d$dist_name[match(unique(d$ubigeo), d$ubigeo)]))
  replace_country_rows(con, "stg_country_crime", "PE", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_PE", basename(f), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  PE: %s rows, %d territories (%d distritos), %d-%dQ%d (complete quarters only)", fmt_int(nrow(crime)), length(unique(crime$territory_id)), length(unique(m_dist$territory_id)),
                  min(crime$year), max(crime$year), max(crime$quarter[crime$year == max(crime$year)])))
  invisible(crime)
}

# --- Population ----------------------------------------------------------------------------------

# MININTER indicators file: POBLACION_PROYECTADA repeated for every indicator; one value per
# (ámbito, ubigeo, year). Ámbito 0 = national, 1 = departamento (Lima split into 1501 Lima
# Metropolitana and 1599 Región Lima: departamento 15 = their sum), 2 = provincia, 3 = distrito.
pe_population <- function(con) {
  f <- pe_indicators_file()
  if (is.na(f)) stop("No indicators file (population) in ", pe_dir(), ": counts are shown without rates.\n  ", pe_manual_note("zip", pe_source("page_indicadores")))
  d <- pe_read_csv(f); nm <- names(d); col <- lapply(PE_COLUMNS, pe_pick, nm = nm)
  if (any(is.na(unlist(col[c("ind_ambito", "ind_ubigeo", "ind_year", "ind_pop")])))) stop("Unexpected columns in ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun pe_inspect() and adjust PE_COLUMNS (ind_*) in R/93_pe.R")
  p <- data.frame(ambito = suppressWarnings(as.integer(d[[col$ind_ambito]])), ubigeo = gsub("\\D", "", d[[col$ind_ubigeo]]), year = suppressWarnings(as.integer(d[[col$ind_year]])),
                  population = suppressWarnings(as.numeric(gsub("[^0-9.]", "", d[[col$ind_pop]]))), stringsAsFactors = FALSE)
  p <- p[!is.na(p$ambito) & !is.na(p$year) & !is.na(p$population) & p$population > 0, ]
  p <- p[!duplicated(p[, c("ambito", "ubigeo", "year")]), ]
  # Ubigeo of each ámbito, whatever its spelling: "1"/"15"/"1501"/"150000" -> departamento "15"; "101"/"150100" -> provincia "0101"/"1501"
  norm <- function(code, width) { d <- gsub("\\D", "", code); d[!nzchar(d)] <- NA
    five <- !is.na(d) & nchar(d) == 5; d[five] <- paste0("0", d[five])            # a 6-digit ubigeo that lost its leading zero ("10500" = 010500 = provincia 0105)
    short <- !is.na(d) & nchar(d) < width; d[short] <- formatC(as.integer(d[short]), width = width, flag = "0", format = "d")
    long <- !is.na(d) & nchar(d) > width & (grepl(paste0("0{", 6 - width, "}$"), d) | (width == 2 & nchar(d) == 4)); d[long] <- substr(d[long], 1, width)   # 1501/1599 (Lima split) -> 15
    d[!is.na(d) & nchar(d) != width] <- NA; d }
  dist <- p[p$ambito == 3, ]; dist$ubigeo <- norm(dist$ubigeo, 6); dist <- dist[!is.na(dist$ubigeo), ]
  prov <- p[p$ambito == 2, ]; prov$ubigeo <- norm(prov$ubigeo, 4); prov <- prov[!is.na(prov$ubigeo), ]
  dep  <- p[p$ambito == 1, ]; dep$ubigeo <- norm(dep$ubigeo, 2); dep <- dep[!is.na(dep$ubigeo), ]   # 1501 Lima Metropolitana + 1599 Región Lima -> 15
  nat  <- p[p$ambito == 0, ]
  deps <- pe_departamentos()
  regions <- if (nrow(dep)) dep |> dplyr::group_by(ubigeo, year) |> dplyr::summarise(population = sum(population), .groups = "drop") |>
                              dplyr::transmute(country = "PE", territory_id = paste0("PE-", ubigeo), level = "region", year, population)
             else prov |> dplyr::group_by(dep = substr(ubigeo, 1, 2), year) |> dplyr::summarise(population = sum(population), .groups = "drop") |>
                              dplyr::transmute(country = "PE", territory_id = paste0("PE-", dep), level = "region", year, population)
  out <- dplyr::bind_rows(
    data.frame(country = "PE", territory_id = pe_ids(dist$ubigeo), level = "municipality", year = dist$year, population = dist$population),
    data.frame(country = "PE", territory_id = pe_prov_ids(prov$ubigeo), level = "province", year = prov$year, population = prov$population),
    regions,
    if (nrow(nat)) data.frame(country = "PE", territory_id = "PE", level = "country", year = nat$year, population = nat$population) else NULL)
  message(sprintf("  population rows by ámbito: national %d, departamentos %d, provincias %d, distritos %d", nrow(nat), nrow(dep), nrow(prov), nrow(dist)))
  known <- DBI::dbGetQuery(con, "SELECT territory_id, level FROM dim_territory WHERE country = 'PE'")
  for (lv in c("region", "province", "municipality")) {
    ids <- unique(out$territory_id[out$level == lv]); k <- known$territory_id[known$level == lv]
    message(sprintf("    %s: %d with population, %d of the %d territories matched (e.g. population ids %s; raw codes %s)", lv, length(ids), sum(k %in% ids), length(k),
                    paste(utils::head(ids, 2), collapse = ", "), paste(utils::head(unique(p$ubigeo[p$ambito == c(region = 1, province = 2, municipality = 3)[[lv]]]), 3), collapse = ", ")))
  }
  out <- out[substr(out$territory_id, 4, 5) %in% c(deps$department_code, "") | out$level == "country", ]
  out <- out[!duplicated(out[, c("territory_id", "year")]), ]
  if (nrow(out) == 0) stop("No population rows recognised in ", basename(f), " (run pe_inspect())")
  replace_country_rows(con, "ref_country_population", "PE", as.data.frame(out))
  message(sprintf("  population: %d territories (%d distritos), %d-%d (MININTER indicators, INEI projections)", length(unique(out$territory_id)), sum(out$level == "municipality" & !duplicated(out$territory_id)), min(out$year), max(out$year)))
}

register_country(list(
  code = "PE", iso3 = "PER", name_en = "Peru", name_local = "Perú", lang = "es",
  levels = c("region", "province", "municipality"),
  level_labels = list(region = c("Departments", "Departamentos", "Department", "Departamento"), province = c("Provinces", "Provincias", "Province", "Provincia"),
                      municipality = c("Districts", "Distritos", "District", "Distrito")),
  source = list(name = "Denuncias Policiales (SIDPOL)", publisher = "Ministerio del Interior (MININTER) / Policía Nacional del Perú, Plataforma Nacional de Datos Abiertos",
                url = "https://www.datosabiertos.gob.pe/dataset/denuncias-policiales",
                licence = "Open Data Commons Attribution License (ODC-BY)",
                measure = "Complaints (denuncias) registered in SIDPOL by month, distrito and modalidad (robo, hurto, estafa, extorsión, secuestro, violencia contra la mujer, otros); no official total: the total shown is their sum. No homicides in this dataset."),
  population_source = "MININTER, Indicadores de seguridad ciudadana (población proyectada INEI por distrito)",
  boundaries_attribution = "OCHA COD / IGN Perú (CC BY-IGO) or geoBoundaries (CC BY 4.0)",
  measure_labels = list(kind = "complaints", title = c("Police complaints", "Denuncias policiales"), noun = c("complaints", "denuncias")),
  not_published = c("No complaints registered or not published", "Sin denuncias registradas o no publicado"),
  notes_en = "Complaints filed at police stations, not verified offences, counted where they are filed (commercial and transit districts look higher). 'Otros' (about a third of the complaints) is not broken down. Registration changes cause level shifts (robbery +52% Dec 2022, -27% Apr 2024, -28% Nov 2024 when virtual complaints stopped being counted): never compare periods across them. Lima Metropolitana = provincia de Lima (43 distritos); Callao is its own departamento.",
  default_region = "PE-15",
  download = pe_download, ingest = pe_ingest, territory = pe_territory, population = pe_population
))
