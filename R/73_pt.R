# Portugal: crimes recorded by the police by município, NUTS III and NUTS II ----------
# Source: INE (Statistics Portugal) indicator 0012261 "Crimes registados (N.º) pelas autoridades
# policiais por Localização geográfica (NUTS - 2024) e Categoria de crime; Anual", data produced by
# the DGPJ (Ministério da Justiça). Yearly, 2021 onwards, official total + 7 main DGPJ categories
# + 8 detailed sub-categories. Read through INE's key-less JSON API (CC BY 4.0).
# Population: INE indicator 0012917 (annual resident population estimates, same geography).
# Boundaries: DGT CAOP 2025 geopackages (CC BY 4.0); geoBoundaries ADM2 as fallback.
#
# Geography: INE codes are hierarchical: NUTS II = 2 chars ("11" Norte, "1A" Grande Lisboa...),
# NUTS III = 3 chars ("111" Alto Minho), município = NUTS III + DICO ("1111601" = Arcos de Valdevez,
# DICO 1601). NUTS 2024 has 9 NUTS II, 26 NUTS III and 308 municípios: reference/pt_territories.csv.
# Confidential cells arrive without "valor" and with sinal_conv "…" (confidential) or "x" (not
# available): they become NA (the app shows "< threshold").
#
# Written against the live API answers of 2026-11 (metadata, national, municipal and multi-code
# calls were checked) but not run end to end: `pt_inspect()` prints what was downloaded.

PT_STATE <- new.env()
PT_API <- list(
  base  = "https://www.ine.pt/ine/json_indicador",
  crime = "0012261",   # crimes recorded by the police, NUTS 2024, yearly (DGPJ)
  pop   = "0012917"    # resident population estimates, NUTS 2024, yearly (sex x age group)
)
# DGT CAOP 2025 (Carta Administrativa Oficial de Portugal), one geopackage zip per region
PT_CAOP <- c(
  continente = "https://geo2.dgterritorio.gov.pt/caop/CAOP_Continente_2025-gpkg.zip",
  acores     = "https://geo2.dgterritorio.gov.pt/caop/CAOP_RAA_2025-gpkg.zip",
  madeira    = "https://geo2.dgterritorio.gov.pt/caop/CAOP_RAM_2025-gpkg.zip"
)
# Field names of one API record (first match wins, case-insensitive)
PT_FIELDS <- list(
  geocod = c("^geocod$"), geodsg = c("^geodsg$"), code = c("^dim_3$"), label = c("^dim_3_t$"),
  value = c("^valor$"), flag = c("^sinal_conv$"), sex = c("^dim_3$"), age = c("^dim_4$")
)

pt_url_meta <- function(varcd) sprintf("%s/pindicaMeta.jsp?varcd=%s&lang=PT", PT_API$base, varcd)
pt_url_data <- function(varcd, year, extra = "") sprintf("%s/pindica.jsp?op=2&varcd=%s&Dim1=S7A%d%s&lang=PT", PT_API$base, varcd, year, extra)

pt_file <- function(pattern) {
  f <- newest_first(list.files(COUNTRY_MODULES$PT$dir_raw, pattern, full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}
pt_age_days <- function(f) if (is.na(f)) Inf else as.numeric(difftime(Sys.time(), file.mtime(f), units = "days"))

# NUTS II / NUTS III / município table (INE codes, DICO, names)
pt_territories <- function() read_reference("pt_territories.csv")

# Years published for an indicator, from its downloaded metadata (Dim_Num1_<S7Ayyyy> keys)
pt_meta_years <- function(varcd) {
  f <- pt_file(paste0("_meta_", varcd, "\\.json$"))
  if (is.na(f)) return(integer())
  j <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  cats <- tryCatch(j[[1]]$Dimensoes$Categoria_Dim[[1]], error = function(e) NULL)
  keys <- grep("^Dim_Num1_", names(cats), value = TRUE)
  years <- suppressWarnings(as.integer(sub("^Dim_Num1_S7A", "", keys)))
  sort(years[!is.na(years)])
}

pt_download <- function(days = 60) {
  m <- COUNTRY_MODULES$PT
  terr <- pt_territories()
  upper <- paste(c("PT", "1", unique(terr$ine_code[terr$level %in% c("region", "province")])), collapse = ",")
  for (what in c("crime", "pop")) {
    varcd <- PT_API[[what]]
    meta <- pt_file(paste0("_meta_", varcd, "\\.json$"))
    if (pt_age_days(meta) > days) download_versioned(pt_url_meta(varcd), m$dir_raw, name = paste0("meta_", varcd, ".json"))
    years <- pt_meta_years(varcd)
    if (length(years) == 0) { years <- 2021:(as.integer(format(Sys.Date(), "%Y")) - 1); message("  PT: no metadata for ", varcd, ", trying ", min(years), "-", max(years)) }
    for (y in years) {
      name <- sprintf("%s_%d.json", what, y)
      prev <- pt_file(paste0("_", name, "$"))
      # Years before the last two are final: one copy is enough; recent years are refreshed every `days`
      if (!is.na(prev) && (y < max(years) - 1 || pt_age_days(prev) < days)) { message("= PT ", name, " already downloaded"); next }
      extra <- if (what == "pop") "&Dim3=T&Dim4=T" else ""               # both sexes, all ages
      f <- download_versioned(pt_url_data(varcd, y, extra), m$dir_raw, name = name)
      # The full call returns every level (PT, NUTS I-III, municípios); if INE ever limits it to
      # municípios, fetch the upper levels in one multi-code call
      if (!grepl('"geocod"\\s*:\\s*"PT"', paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "")))
        download_versioned(pt_url_data(varcd, y, paste0("&Dim2=", upper, extra)), m$dir_raw, name = sprintf("%s_%d_upper.json", what, y))
    }
  }
  # Boundaries: CAOP geopackages (~100-200 MB in all; a new CAOP edition is a new URL)
  for (r in names(PT_CAOP)) {
    if (!is.na(pt_file(paste0("_", basename(PT_CAOP[[r]]), "$")))) next
    tryCatch(download_versioned(PT_CAOP[[r]], m$dir_raw), error = function(e) message("  CAOP ", r, " not downloaded: ", conditionMessage(e)))
  }
}

pt_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }

# One INE JSON answer -> data frame: year, geocod, geodsg, code, label, value (NA when suppressed), flag
pt_read_json <- function(f) {
  j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  if (is.list(j) && length(j) >= 1 && is.null(names(j))) j <- j[[1]]
  dados <- j$Dados
  if (is.null(dados) || length(dados) == 0) { message("  ", basename(f), ": no data (", paste(unlist(j$Sucesso), collapse = " "), ")"); return(NULL) }
  out <- lapply(names(dados), function(yr) {
    recs <- dados[[yr]]; if (length(recs) == 0) return(NULL)
    nm <- unique(unlist(lapply(recs[seq_len(min(50, length(recs)))], names)))
    col <- lapply(PT_FIELDS, pt_pick, nm = nm)
    if (is.na(col$geocod) || is.na(col$value)) stop("Unexpected fields in ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun pt_inspect() and adjust PT_FIELDS in R/73_pt.R")
    get <- function(fld) if (is.na(fld)) rep(NA_character_, length(recs)) else vapply(recs, function(r) if (is.null(r[[fld]])) NA_character_ else as.character(r[[fld]]), character(1))
    data.frame(year = as.integer(yr), geocod = trimws(get(col$geocod)), geodsg = get(col$geodsg), code = trimws(get(col$code)), label = get(col$label),
               value = suppressWarnings(as.numeric(gsub("[^0-9.-]", "", get(col$value)))), flag = get(col$flag),
               sex = get(col$sex), age = get(col$age), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Newest copy of every year file of one kind ("crime" or "pop"), including the "_upper" complements
pt_year_files <- function(what) {
  f <- list.files(COUNTRY_MODULES$PT$dir_raw, sprintf("_%s_[0-9]{4}(_upper)?\\.json$", what), full.names = TRUE)
  if (length(f) == 0) return(character())
  f <- newest_first(f); f[!duplicated(sub("^[0-9_]+_", "", basename(f)))]   # one copy per name (without the YYYYMMDD_ prefix)
}

# Print what was downloaded (to adjust PT_FIELDS / the CAOP reader if INE or DGT change something)
pt_inspect <- function() {
  for (what in c("crime", "pop")) for (f in pt_year_files(what)) {
    j <- jsonlite::fromJSON(f, simplifyVector = FALSE); if (is.null(names(j))) j <- j[[1]]
    recs <- unlist(j$Dados, recursive = FALSE)
    message(basename(f), ": ", length(recs), " records, years ", paste(names(j$Dados), collapse = ","), "\n  fields: ", paste(unique(unlist(lapply(recs[1:min(20, length(recs))], names))), collapse = " | "))
    print(utils::head(pt_read_json(f), 3))
  }
  for (z in list.files(COUNTRY_MODULES$PT$dir_raw, "gpkg\\.zip$", full.names = TRUE)) {
    message(basename(z)); for (g in pt_unzip_gpkg(z)) { ly <- sf::st_layers(g); message("  ", basename(g), ": ", paste(ly$name, collapse = " | ")) }
  }
}

# Level and identifier from the INE geographic code
pt_level <- function(code) ifelse(code == "PT", "country", ifelse(nchar(code) == 2, "region", ifelse(nchar(code) == 3, "province", ifelse(nchar(code) == 7, "municipality", NA_character_))))
pt_ids <- function(code) {
  lv <- pt_level(code)
  ifelse(lv == "country", "PT",
  ifelse(lv == "region", paste0("PT-", code),
  ifelse(lv == "province", paste("PT", substr(code, 1, 2), code, sep = "-"),
  ifelse(lv == "municipality", paste("PT", substr(code, 1, 2), substr(code, 1, 3), substr(code, 4, 7), sep = "-"), NA_character_))))
}
pt_parent <- function(id) ifelse(id == "PT", NA_character_, sub("-[^-]+$", "", id))

pt_ingest <- function(con) {
  files <- pt_year_files("crime")
  if (length(files) == 0) stop("No INE crime file yet in ", COUNTRY_MODULES$PT$dir_raw, " (run the download step)")
  d <- do.call(rbind, lapply(files, function(f) { x <- pt_read_json(f); if (!is.null(x)) x$file <- basename(f); x }))
  if (is.null(d) || nrow(d) == 0) stop("The INE crime files contain no records")
  d$level <- pt_level(d$geocod)
  d <- d[!is.na(d$level) & !is.na(d$code) & d$code != "", ]               # drops NUTS I rows ("1", "2", "3")
  d <- d[!duplicated(d[, c("geocod", "code", "year")]), ]                 # a year present in two files: newest copy wins
  d$territory_id <- pt_ids(d$geocod)
  # Suppressed cells: no "valor", sinal_conv "…" (confidential) or "x" (not available) -> NA
  d$value[!is.na(d$flag) & d$flag != ""] <- NA
  # Territories mentioned by the data but absent from dim_territory (e.g. a new município)
  for (lv in c("region", "province", "municipality")) {
    x <- unique(d[d$level == lv, c("territory_id", "geodsg")])
    ensure_territories(con, "PT", x$territory_id, lv, pt_parent(x$territory_id), x$geodsg)
  }
  known <- read_reference("pt_offence_types.csv")$source_code
  unknown <- setdiff(unique(d$code), known)
  if (length(unknown)) message("  PT: categories not in reference/pt_offence_types.csv (ignored until mapped): ",
                               paste(sprintf("%s = %s", unknown, d$label[match(unknown, d$code)]), collapse = "; "))
  crime <- data.frame(country = "PT", territory_id = d$territory_id, level = d$level, offence_code = d$code, year = d$year, n = d$value, file = d$file, stringsAsFactors = FALSE)
  # National row: published by INE (includes crimes without a known município); sum of NUTS II only if missing
  for (y in setdiff(unique(crime$year), crime$year[crime$level == "country"])) {
    nat <- crime[crime$level == "region" & crime$year == y, ] |> dplyr::group_by(country, offence_code, year, file) |>
      dplyr::summarise(n = if (all(is.na(n))) NA_real_ else sum(n, na.rm = TRUE), .groups = "drop")
    nat$territory_id <- "PT"; nat$level <- "country"
    crime <- rbind(crime, nat[, names(crime)])
    message("  PT ", y, ": national figure = sum of the NUTS II regions (not published in the file)")
  }
  crime <- yearly_to_cumulative(crime)
  replace_country_rows(con, "stg_country_crime", "PT", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_PT", paste(basename(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  crime: %s rows, %d territories, %d-%d, %d categories (%d suppressed cells)", fmt_int(nrow(crime)), length(unique(crime$territory_id)),
                  min(crime$year), max(crime$year), length(unique(crime$offence_code)), sum(is.na(crime$cumulative))))
  invisible(crime)
}

# Resident population estimates (31 December of each year) for every level
pt_population <- function(con) {
  files <- pt_year_files("pop")
  if (length(files) == 0) stop("No INE population file yet in ", COUNTRY_MODULES$PT$dir_raw)
  d <- do.call(rbind, lapply(files, pt_read_json))
  if (is.null(d) || nrow(d) == 0) stop("The INE population files contain no records")
  d <- d[(is.na(d$sex) | d$sex == "T") & (is.na(d$age) | d$age == "T"), ]   # both sexes, all ages
  d$level <- pt_level(d$geocod)
  d <- d[!is.na(d$level) & !is.na(d$value), ]
  d <- d[!duplicated(d[, c("geocod", "year")]), ]
  pop <- data.frame(country = "PT", territory_id = pt_ids(d$geocod), level = d$level, year = d$year, population = d$value, stringsAsFactors = FALSE)
  replace_country_rows(con, "ref_country_population", "PT", pop)
  message(sprintf("  population: %d territories, %d-%d (INE estimates, indicator %s)", length(unique(pop$territory_id)), min(pop$year), max(pop$year), PT_API$pop))
  invisible(pop)
}

# --- Boundaries ------------------------------------------------------------------------------

pt_unzip_gpkg <- function(zip) {
  tmp <- file.path(tempdir(), tools::file_path_sans_ext(basename(zip)))
  if (!dir.exists(tmp)) utils::unzip(zip, exdir = tmp)
  list.files(tmp, "\\.gpkg$", full.names = TRUE, recursive = TRUE)
}

# Municípios from the CAOP geopackages: sf with dico, name (WGS84). The layers are named
# <region>_municipios (cont_, ram_, raa_oci_, raa_cen_ori_) with fields dtmn (DICO) and municipio.
pt_caop_municipalities <- function() {
  zips <- newest_first(list.files(COUNTRY_MODULES$PT$dir_raw, "CAOP_.*gpkg\\.zip$", full.names = TRUE))
  zips <- zips[!duplicated(sub("^[0-9_]+_", "", basename(zips)))]
  if (length(zips) == 0) stop("no CAOP file downloaded")
  parts <- list()
  for (z in zips) for (g in pt_unzip_gpkg(z)) {
    layers <- sf::st_layers(g)$name
    for (ly in grep("municip|concelho", layers, value = TRUE, ignore.case = TRUE)) {
      x <- sf::st_read(g, layer = ly, quiet = TRUE)
      nm <- names(x)
      code <- pt_pick(nm, c("^dtmn$", "^dico$", "^cod_?mun", "^municipio_?cod"))
      if (is.na(code)) code <- nm[vapply(nm, function(cn) is.character(x[[cn]]) && all(grepl("^[0-9]{4}$", x[[cn]])), logical(1))][1]
      name <- pt_pick(nm, c("^municipio$", "^concelho$", "^nome", "^designacao", "^name"))
      if (is.na(code) || is.na(name)) stop("CAOP layer ", ly, " of ", basename(g), ": no DICO / name column among: ", paste(nm, collapse = " | "), "\nRun pt_inspect() and adjust pt_caop_municipalities() in R/73_pt.R")
      x <- sf::st_transform(sf::st_zm(x), 4326)
      x <- sf::st_sf(dico = sprintf("%04d", as.integer(x[[code]])), name = as.character(x[[name]]), geometry = sf::st_geometry(x))
      parts[[length(parts) + 1]] <- x
    }
  }
  if (length(parts) == 0) stop("no '*municipios' layer in the CAOP geopackages (", paste(basename(zips), collapse = ", "), "): run pt_inspect()")
  g <- do.call(rbind, parts)
  g[!duplicated(g$dico), ]
}

# Fallback: geoBoundaries PRT ADM2 (CC BY 4.0), matched by normalised name; the two Calhetas and
# the two Lagoas are told apart by their position (Açores / Madeira / mainland)
pt_geoboundaries_municipalities <- function(terr) {
  g <- geoboundaries("PRT", 2)
  nm_col <- pt_pick(names(g), c("^shapeName$", "^name", "^NAME"))
  if (is.na(nm_col)) stop("geoBoundaries ADM2: no name column among ", paste(names(g), collapse = " | "))
  g <- sf::st_transform(g, 4326)
  base <- normalize_text(sub(" \\(R\\.A\\.[AM]\\.\\)$", "", terr$name))
  ctr <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(g)))
  island <- ifelse(ctr[, 1] < -24, "ACORES", ifelse(ctr[, 2] < 34, "MADEIRA", "CONT"))
  terr_island <- ifelse(substr(terr$ine_code, 1, 2) == "20", "ACORES", ifelse(substr(terr$ine_code, 1, 2) == "30", "MADEIRA", "CONT"))
  key_g <- paste(normalize_text(g[[nm_col]]), island); key_t <- paste(base, terr_island)
  i <- match(key_g, key_t)
  message(sprintf("  geoBoundaries: %d of %d polygons matched to a município", sum(!is.na(i)), nrow(g)))
  sf::st_sf(dico = terr$dico[i], name = terr$name[i], geometry = sf::st_geometry(g))[!is.na(i), ]
}

# Territories: NUTS II / NUTS III / municípios from reference/pt_territories.csv; polygons from CAOP
pt_territory <- function(con) {
  ref <- pt_territories()
  ref$territory_id <- pt_ids(ref$ine_code); ref$parent_id <- pt_parent(ref$territory_id)
  tbl <- data.frame(territory_id = ref$territory_id, level = ref$level, parent_id = ref$parent_id,
                    official_code = ifelse(ref$level == "municipality", ref$dico, ref$ine_code), name = ref$name, stringsAsFactors = FALSE)
  save_country_territory(con, "PT", tbl, scheme = "INE NUTS 2024 / DICO")
  mun <- ref[ref$level == "municipality", ]
  g <- tryCatch(pt_caop_municipalities(), error = function(e) { message("  CAOP not usable (", conditionMessage(e), "): trying geoBoundaries"); NULL })
  if (is.null(g)) g <- tryCatch(pt_geoboundaries_municipalities(mun), error = function(e) { message("  geoBoundaries not available either: municípios without boundaries (", conditionMessage(e), ")"); NULL })
  if (is.null(g) || nrow(g) == 0) return(invisible(tbl))
  i <- match(g$dico, mun$dico)
  if (any(is.na(i))) message("  ", sum(is.na(i)), " polygons with a DICO unknown to pt_territories.csv (ignored): ", paste(utils::head(g$dico[is.na(i)], 10), collapse = ", "))
  g <- g[!is.na(i), ]; i <- i[!is.na(i)]
  g$territory_id <- mun$territory_id[i]; g$name <- mun$name[i]
  g <- g[, c("territory_id", "name", attr(g, "sf_column"))]
  if (nrow(g) < nrow(mun)) message("  ", nrow(mun) - nrow(g), " municípios without polygon")
  message("  building NUTS III and NUTS II polygons from the municípios")
  gp <- g; gp$territory_id <- pt_parent(gp$territory_id); gp <- gp |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop"); gp$name <- ref$name[match(gp$territory_id, ref$territory_id)]
  gr <- gp; gr$territory_id <- pt_parent(gr$territory_id); gr <- gr |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop"); gr$name <- ref$name[match(gr$territory_id, ref$territory_id)]
  save_country_gpkg(con, "PT", list(region = gr, province = gp, municipality = g), tolerance_m = 100)
  invisible(tbl)
}

register_country(list(
  code = "PT", iso3 = "PRT", name_en = "Portugal", name_local = "Portugal", lang = "pt",
  levels = c("region", "province", "municipality"),
  level_labels = list(region = c("Regions (NUTS II)", "Regiões (NUTS II)", "Region", "Região"),
                      province = c("Sub-regions (NUTS III)", "Sub-regiões (NUTS III)", "Sub-region", "Sub-região"),
                      municipality = c("Municipalities", "Municípios", "Municipality", "Município")),
  source = list(name = "Crimes registados pelas autoridades policiais (INE / DGPJ)", publisher = "INE - Instituto Nacional de Estatística, data from DGPJ (Ministério da Justiça)",
                url = "https://www.ine.pt/xurl/indx/0012261/PT",
                licence = "Creative Commons Attribution 4.0 (CC BY 4.0)",
                measure = "Crimes recorded by the police authorities (PJ, PSP, GNR, AT, PM, PJM, ASAE; SEF until 2023), yearly, by município of occurrence; DGPJ classification with an official total (the national total also counts crimes without a known location)."),
  population_source = "INE, annual resident population estimates (indicator 0012917, 31 December)",
  boundaries_attribution = "Direção-Geral do Território, CAOP 2025 (CC BY 4.0); geoBoundaries (CC BY 4.0) as fallback",
  not_published = c("Not published: confidential cell (statistical secrecy) or figure not available (INE)", "Não publicado: dado confidencial (segredo estatístico) ou não disponível (INE)"),
  notes_en = "Series starts in 2021 (NUTS 2024 geography; earlier years exist only under NUTS 2013 in indicator 0008073). Detailed sub-categories (homicide, assault, domestic violence, vehicle theft, street robbery, drink-driving, driving without a licence) are parts of the seven main DGPJ categories; the Portuguese Immigration Service (SEF) left the totals after 2023.",
  method = list(where = "Município where the crime occurred; crimes with no known place count in the national total only", date = "Year the crime was recorded by the police authority (participação)", revisions = "Yearly publication; the previous year may be revised with the next release"),
  default_region = "PT-1A",
  download = pt_download, ingest = pt_ingest, territory = pt_territory, population = pt_population
))
