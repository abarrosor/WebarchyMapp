# Brazil: recorded occurrences by município and UF -----------------------------------------
# Source: Ministério da Justiça e Segurança Pública (MJSP), SINESP VDE "Base de dados e notas
# metodológicas dos gestores estaduais" — one Excel workbook per year (2015 onwards), monthly
# rows by UF, município and "evento" (indicator), with `total` (occurrences) and
# `total_vitima` (victims). Figures are reported by each state's Polícia Civil / SSP
# ("gestor estadual"): definitions and coverage differ by state (see the notes on the page).
# No licence is stated on the page; gov.br content is published under CC BY-ND 3.0 BR.
# Municipalities come as NAMES (no IBGE code): they are matched, within the UF, to the IBGE
# "localidades" list. Population: IBGE SIDRA table 6579. Boundaries: IBGE malhas API.
#
# Written against the published structure (not run against the live files): the reader detects
# the columns by name; `br_inspect()` prints the headers if a workbook does not match.
# URLs live in reference/br_sources.csv so that they can be changed without touching the code.

BR_STATE <- new.env()
BR_COLUMNS <- list(
  uf        = c("^uf$", "^sigla_uf$", "^estado$", "^nome_uf$"),
  municipio = c("^munic[ií]pio$", "^nome_munic", "^municipio"),
  evento    = c("^evento$", "^indicador$", "^tipo_evento"),
  date      = c("^data_referencia$", "^data$", "^mes_ano$", "^referencia$"),
  year      = c("^ano$", "^ano_referencia$"),
  month     = c("^m[eê]s$", "^mes_referencia$"),
  total     = c("^total$", "^ocorr[eê]ncias?$", "^qtd_ocorr", "^quantidade$"),
  victims   = c("^total_vitimas?$", "^v[ií]timas?$"),
  scope     = c("^abrang[eê]ncia$")
)

br_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }
br_sources <- function() read_reference("br_sources.csv")
br_source <- function(key, field = "url") { s <- br_sources(); v <- s[[field]][s$key == key]; if (length(v) == 0) stop("br_sources.csv has no key ", key); v[1] }
br_ufs <- function() read_reference("br_ufs.csv")
br_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", as.character(x)))))

# Local files -------------------------------------------------------------------------------
br_file <- function(name) {
  f <- newest_first(list.files(COUNTRY_MODULES$BR$dir_raw, paste0("_", gsub("([.()+])", "\\\\\\1", name), "$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}
br_vde_files <- function() {
  f <- list.files(COUNTRY_MODULES$BR$dir_raw, "_bancovde-[0-9]{4}\\.xlsx$", full.names = TRUE)
  if (length(f) == 0) return(character())
  yr <- sub("^.*bancovde-([0-9]{4})\\.xlsx$", "\\1", f)
  unlist(lapply(split(f, yr), function(x) newest_first(x)[1]))   # newest version of each year
}

# Download ----------------------------------------------------------------------------------
# Yearly workbooks: closed years are downloaded once; the two latest years are refreshed every
# `days` days (states revise their figures). IBGE lists are refreshed yearly.
br_download <- function(days = 60) {
  m <- COUNTRY_MODULES$BR
  if (!requireNamespace("readxl", quietly = TRUE)) message("  package readxl is needed to read the SINESP VDE workbooks")
  first <- as.integer(br_source("vde_first_year")); this_year <- as.integer(format(Sys.Date(), "%Y"))
  tmpl <- br_source("vde_year"); tmpl_name <- br_source("vde_year", "name")
  for (y in first:this_year) {
    name <- sub("{year}", y, tmpl_name, fixed = TRUE); prev <- br_file(name)
    if (!is.na(prev) && (y < this_year - 1 || difftime(Sys.time(), file.mtime(prev), units = "days") < days)) { message("= BR ", name, " already downloaded"); next }
    tryCatch(download_versioned(sub("{year}", y, tmpl, fixed = TRUE), m$dir_raw, name = name),
             error = function(e) message("  BR ", y, ": not downloaded (", conditionMessage(e), ")"))
  }
  for (key in c("ibge_municipios", "sidra_ufs", "sidra_brasil")) {
    name <- br_source(key, "name"); prev <- br_file(name)
    if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < 365) next
    tryCatch(download_versioned(br_source(key), m$dir_raw, name = name), error = function(e) message("  BR ", key, ": not downloaded (", conditionMessage(e), ")"))
  }
  # SIDRA refuses the 5,570 municípios x many years in one call (HTTP 400): one call per year
  # (years without an estimate, 2022 and 2023 census years, answer with an error and are skipped)
  tmpl <- br_source("sidra_municipios"); tmpl_name <- br_source("sidra_municipios", "name")
  for (y in first:this_year) {
    name <- sub("{year}", y, tmpl_name, fixed = TRUE); prev <- br_file(name)
    if (!is.na(prev) && (y < this_year || difftime(Sys.time(), file.mtime(prev), units = "days") < 365)) next
    tryCatch(download_versioned(sub("{year}", y, tmpl, fixed = TRUE), m$dir_raw, name = name),
             error = function(e) message("  BR sidra_municipios ", y, ": not downloaded (", conditionMessage(e), ")"))
  }
}
# The yearly SIDRA municipal population files (newest version of each year)
br_sidra_muni_files <- function() {
  f <- list.files(COUNTRY_MODULES$BR$dir_raw, "_sidra_6579_municipios_[0-9]{4}\\.json$", full.names = TRUE)
  if (!length(f)) return(character())
  unlist(lapply(split(f, sub("^.*_([0-9]{4})\\.json$", "\\1", f)), function(x) newest_first(x)[1]))
}

# Print the header of each downloaded workbook (to adjust BR_COLUMNS or br_offence_types.csv)
br_inspect <- function() {
  for (f in br_vde_files()) { d <- br_read_xlsx(f); message(basename(f), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
  if (length(BR_STATE$events)) { message("Events seen and their measure:"); print(BR_STATE$events) }
  if (length(BR_STATE$unmatched)) { message("Municipality names not matched to IBGE (first 30):"); print(utils::head(BR_STATE$unmatched, 30)) }
}

# Read a workbook: the sheet with most rows, everything as text
br_read_xlsx <- function(f) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is needed to read ", basename(f))
  sheets <- readxl::excel_sheets(f)
  best <- NULL
  for (s in sheets) { d <- readxl::read_excel(f, sheet = s, col_types = "text", .name_repair = "minimal"); if (is.null(best) || nrow(d) > nrow(best)) best <- d }
  names(best) <- trimws(names(best))
  best
}

# Standardise one workbook to: uf_code, municipio, evento, year, month, n (and the measure used)
br_standardise <- function(d, f) {
  nm <- names(d); col <- lapply(BR_COLUMNS, br_pick, nm = nm)
  if (is.na(col$uf) || is.na(col$evento) || (is.na(col$date) && (is.na(col$year) || is.na(col$month))) || (is.na(col$total) && is.na(col$victims)))
    stop("Unexpected columns in ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun br_inspect() and adjust BR_COLUMNS in R/82_br.R")
  ufs <- br_ufs()
  uf_raw <- normalize_text(d[[col$uf]])
  uf_code <- ufs$uf_code[match(uf_raw, normalize_text(ufs$uf_sigla))]
  miss <- is.na(uf_code); uf_code[miss] <- ufs$uf_code[match(uf_raw[miss], normalize_text(ufs$uf_name))]
  miss <- is.na(uf_code); uf_code[miss] <- ufs$uf_code[match(uf_raw[miss], ufs$uf_code)]
  if (!is.na(col$date)) {
    as_date <- br_date(d[[col$date]])
    year <- as.integer(format(as_date, "%Y")); month <- as.integer(format(as_date, "%m"))
    if (!is.na(col$year)) year <- ifelse(is.na(year), as.integer(d[[col$year]]), year)
    if (!is.na(col$month)) month <- ifelse(is.na(month), br_month(d[[col$month]]), month)
  } else { year <- as.integer(d[[col$year]]); month <- br_month(d[[col$month]]) }
  total <- if (!is.na(col$total)) br_num(d[[col$total]]) else NA_real_
  victims <- if (!is.na(col$victims)) br_num(d[[col$victims]]) else NA_real_
  out <- data.frame(uf_code = uf_code, municipio = if (!is.na(col$municipio)) trimws(d[[col$municipio]]) else NA_character_,
                    evento = trimws(d[[col$evento]]), year = year, month = month, total = total, victims = victims, stringsAsFactors = FALSE)
  # One measure per event: occurrences (`total`) when the state reports them, else victims
  ev <- out |> dplyr::group_by(evento) |> dplyr::summarise(has_total = any(!is.na(total)), .groups = "drop")
  out$n <- ifelse(out$evento %in% ev$evento[ev$has_total], out$total, out$victims)
  BR_STATE$events <- unique(rbind(BR_STATE$events, data.frame(evento = ev$evento, measure = ifelse(ev$has_total, "total (ocorrências)", "total_vitima"), stringsAsFactors = FALSE)))
  out <- out[!is.na(out$uf_code) & !is.na(out$year) & !is.na(out$month) & out$evento != "", c("uf_code", "municipio", "evento", "year", "month", "n")]
  out
}
# Excel dates read as text: serial numbers ("45292") or "2023-01-01" / "01/01/2023"
br_date <- function(x) {
  x <- trimws(as.character(x)); out <- as.Date(rep(NA_character_, length(x)))
  num <- !is.na(x) & grepl("^[0-9]{5}(\\.0+)?$", x)
  if (any(num)) out[num] <- as.Date(as.numeric(x[num]), origin = "1899-12-30")
  for (fmt in c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d")) { miss <- is.na(out) & !num & !is.na(x); if (!any(miss)) break; out[miss] <- as.Date(x[miss], format = fmt) }
  out
}
br_month <- function(x) {
  m <- suppressWarnings(as.integer(x)); if (all(!is.na(m[!is.na(x)]))) return(m)
  pt <- c("JAN", "FEV", "MAR", "ABR", "MAI", "JUN", "JUL", "AGO", "SET", "OUT", "NOV", "DEZ")
  ifelse(is.na(m), match(substr(normalize_text(x), 1, 3), pt), m)
}

# IBGE municipality list (7-digit code, name, UF) from the localidades API
br_municipios <- function() {
  f <- br_file(br_source("ibge_municipios", "name"))
  if (is.na(f)) {   # fallback: the SIDRA population files name every município as "Name - UF" with its 7-digit code
    sf <- br_sidra_muni_files(); if (!length(sf)) return(NULL)
    j <- do.call(rbind, lapply(sf, function(x) { d <- jsonlite::fromJSON(x); if (nrow(d) < 2) NULL else d[-1, c("D1C", "D1N")] }))
    if (is.null(j) || nrow(j) == 0) return(NULL)
    j <- j[!duplicated(j$D1C), ]; code <- sprintf("%07d", as.integer(j$D1C))
    message("  municipality list taken from the SIDRA population files (IBGE localidades API not downloaded)")
    return(data.frame(code = code, name = sub("\\s+-\\s+[A-Z]{2}$", "", j$D1N), uf_code = substr(code, 1, 2), stringsAsFactors = FALSE))
  }
  j <- jsonlite::fromJSON(f, flatten = TRUE)
  nm <- names(j)
  id <- br_pick(nm, c("^municipio-id$", "^id$")); name <- br_pick(nm, c("^municipio-nome$", "^nome$"))
  uf <- br_pick(nm, c("^UF-id$", "UF\\.id$", "^uf-id$"))
  if (is.na(id) || is.na(name)) stop("Unexpected IBGE municipality JSON: ", paste(nm, collapse = " | "))
  code <- sprintf("%07d", as.integer(j[[id]]))
  data.frame(code = code, name = j[[name]], uf_code = if (!is.na(uf)) as.character(j[[uf]]) else substr(code, 1, 2), stringsAsFactors = FALSE)
}

# Match (UF, município name) to the IBGE code; NA when not found
br_match_municipio <- function(uf_code, name, mun) {
  if (is.null(mun)) return(rep(NA_character_, length(name)))
  key <- paste(uf_code, normalize_text(name)); ref <- paste(mun$uf_code, normalize_text(mun$name))
  code <- mun$code[match(key, ref)]
  # Second try without apostrophes / hyphens / extra words
  squash <- function(x) gsub("[^A-Z0-9 ]", "", gsub("-", " ", x))
  miss <- is.na(code) & !is.na(name)
  if (any(miss)) code[miss] <- mun$code[match(squash(key[miss]), squash(ref))]
  code
}

br_ingest <- function(con) {
  files <- br_vde_files()
  if (length(files) == 0) stop("No SINESP VDE workbook in ", COUNTRY_MODULES$BR$dir_raw, " (run the download first)")
  types <- read_reference("br_offence_types.csv")
  mun <- tryCatch(br_municipios(), error = function(e) { message("  IBGE municipality list not readable: ", conditionMessage(e)); NULL })
  if (is.null(mun)) message("  no IBGE municipality list: only UF and national figures will be loaded")
  BR_STATE$events <- NULL
  rows <- list()
  for (f in files) {
    d <- br_standardise(br_read_xlsx(f), f)
    # Source codes: the event label as written in the reference file (matched without accents/case)
    d$offence_code <- types$source_code[match(normalize_text(d$evento), normalize_text(types$source_code))]
    unknown <- unique(d$evento[is.na(d$offence_code)])
    if (length(unknown)) message("  ", basename(f), ": events not in br_offence_types.csv (ignored): ", paste(unknown, collapse = "; "))
    d <- d[!is.na(d$offence_code), ]
    d$muni_code <- br_match_municipio(d$uf_code, d$municipio, mun)
    rows[[f]] <- d
    message(sprintf("  %s: %s rows, %d UFs, %d-%02d to %d-%02d", basename(f), fmt_int(nrow(d)), length(unique(d$uf_code)), min(d$year), min(d$month[d$year == min(d$year)]), max(d$year), max(d$month[d$year == max(d$year)])))
  }
  d <- do.call(rbind, rows); d$file <- basename(rep(names(rows), vapply(rows, nrow, 1L)))
  unm <- d[is.na(d$muni_code) & !is.na(d$municipio) & !grepl("INFORMADO|^NA$", normalize_text(d$municipio)) & !is.na(d$n), ]
  BR_STATE$unmatched <- unm |> dplyr::count(uf_code, municipio, sort = TRUE) |> as.data.frame()
  if (nrow(BR_STATE$unmatched)) message(sprintf("  %d municipality names not matched to IBGE codes (their figures stay in the UF totals; see br_inspect())", nrow(BR_STATE$unmatched)))
  # Monthly figures by level: municipality (matched names), UF (every row of the UF), country (sum of UFs)
  monthly <- function(x, id, lv) { x$territory_id <- as.character(id); x$level <- rep(lv, nrow(x))   # rep(): x may have 0 rows (no IBGE list)
    x |> dplyr::group_by(territory_id, level, offence_code, year, month) |> dplyr::summarise(n = sum(n, na.rm = TRUE), file = file[1], .groups = "drop") }
  m_mun <- monthly(d[!is.na(d$muni_code), ], paste("BR", d$uf_code[!is.na(d$muni_code)], d$muni_code[!is.na(d$muni_code)], sep = "-"), "municipality")
  m_uf  <- monthly(d, paste0("BR-", d$uf_code), "region")
  m_nat <- monthly(d, "BR", "country")
  all <- rbind(m_mun, m_uf, m_nat); all$country <- "BR"
  # Synthetic total: sum of the events mapped to an offence type (never the unmapped ones)
  counted <- types$source_code[types$offence_type_id != "unmapped" & types$offence_type_id != "total"]
  tot <- all[all$offence_code %in% counted, ] |> dplyr::group_by(country, territory_id, level, year, month) |> dplyr::summarise(n = sum(n), file = file[1], .groups = "drop")
  tot$offence_code <- rep("TOTAL_EVENTOS", nrow(tot))
  all <- rbind(all, tot[, names(all)])
  crime <- monthly_to_cumulative(all[, c("country", "territory_id", "level", "offence_code", "year", "month", "n")])
  crime$file <- all$file[match(paste(crime$territory_id, crime$year), paste(all$territory_id, all$year))]
  if (nrow(crime) == 0) stop("No complete quarter in the SINESP VDE files")
  # Territories present in the data but not in dim_territory (e.g. without IBGE list)
  ufs <- br_ufs()
  ensure_territories(con, "BR", paste0("BR-", ufs$uf_code), "region", "BR", ufs$uf_name)
  mm <- unique(crime$territory_id[crime$level == "municipality"])
  if (length(mm)) ensure_territories(con, "BR", mm, "municipality", sub("-[^-]+$", "", mm), if (!is.null(mun)) mun$name[match(sub("^.*-", "", mm), mun$code)] else NULL)
  replace_country_rows(con, "stg_country_crime", "BR", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_BR", paste(basename(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  BR: %s rows, %d territories, %d-%d", fmt_int(nrow(crime)), length(unique(crime$territory_id)), min(crime$year), max(crime$year)))
  invisible(crime)
}

# Population: IBGE SIDRA table 6579 (estimated resident population on 1 July) ------------------
br_sidra <- function(f) {
  j <- jsonlite::fromJSON(f)
  if (nrow(j) < 2) return(NULL)
  j <- j[-1, ]   # the first record holds the labels
  data.frame(code = as.character(j$D1C), year = as.integer(j$D3C), population = br_num(j$V), stringsAsFactors = FALSE)
}
br_population <- function(con) {
  parts <- list()
  for (key in c("sidra_municipios", "sidra_ufs", "sidra_brasil")) {
    fs <- if (key == "sidra_municipios") br_sidra_muni_files() else br_file(br_source(key, "name"))
    fs <- fs[!is.na(fs)]; if (!length(fs)) { message("  no ", key, " file yet"); next }
    p <- do.call(rbind, Filter(Negate(is.null), lapply(fs, br_sidra))); if (is.null(p) || nrow(p) == 0) next
    p <- p[!is.na(p$population), ]
    if (key == "sidra_municipios") { p$territory_id <- paste("BR", substr(p$code, 1, 2), sprintf("%07d", as.integer(p$code)), sep = "-"); p$level <- "municipality" }
    else if (key == "sidra_ufs") { p$territory_id <- paste0("BR-", p$code); p$level <- "region" }
    else { p$territory_id <- "BR"; p$level <- "country" }
    parts[[key]] <- data.frame(country = "BR", territory_id = p$territory_id, level = p$level, year = p$year, population = p$population)
  }
  if (length(parts) == 0) stop("No SIDRA population file in ", COUNTRY_MODULES$BR$dir_raw)
  pop <- unique(do.call(rbind, parts))
  replace_country_rows(con, "ref_country_population", "BR", pop)
  message(sprintf("  population: %d territories, %d-%d (IBGE SIDRA 6579)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

# Territories: UFs from reference/br_ufs.csv, municipalities from the IBGE list; boundaries from
# the IBGE malhas API (GeoJSON, property `codarea`), else geoBoundaries ADM1 for the UFs only
br_territory <- function(con) {
  m <- COUNTRY_MODULES$BR; ufs <- br_ufs()
  tbl <- data.frame(territory_id = paste0("BR-", ufs$uf_code), level = "region", parent_id = "BR", official_code = ufs$uf_code, name = ufs$uf_name)
  mun <- tryCatch(br_municipios(), error = function(e) NULL)
  if (!is.null(mun)) tbl <- rbind(tbl, data.frame(territory_id = paste("BR", mun$uf_code, mun$code, sep = "-"), level = "municipality",
                                                   parent_id = paste0("BR-", mun$uf_code), official_code = mun$code, name = mun$name))
  save_country_territory(con, "BR", tbl, scheme = "IBGE")
  layers <- list()
  fetch_geo <- function(url, name) {
    f <- br_file(name)
    if (is.na(f)) f <- tryCatch(download_versioned(url, m$dir_raw, name = name), error = function(e) NA_character_)
    if (is.na(f)) return(NULL)
    tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL)
  }
  g_uf <- fetch_geo(br_source("malha_ufs"), br_source("malha_ufs", "name"))
  if (!is.null(g_uf) && "codarea" %in% names(g_uf)) {
    g_uf$territory_id <- paste0("BR-", g_uf$codarea); g_uf$name <- ufs$uf_name[match(g_uf$codarea, ufs$uf_code)]
    layers$region <- g_uf[!is.na(g_uf$name), c("territory_id", "name", attr(g_uf, "sf_column"))]
  } else {
    gb <- tryCatch(geoboundaries("BRA", 1), error = function(e) { message("  no UF boundaries (", conditionMessage(e), ")"); NULL })
    if (!is.null(gb)) {
      code <- ufs$uf_code[match(normalize_text(gb$shapeName), normalize_text(ufs$uf_name))]
      gb <- gb[!is.na(code), ]; gb$territory_id <- paste0("BR-", code[!is.na(code)]); gb$name <- ufs$uf_name[match(code[!is.na(code)], ufs$uf_code)]
      layers$region <- gb[, c("territory_id", "name", attr(gb, "sf_column"))]
      message("  UF boundaries from geoBoundaries (CC BY 4.0)")
    }
  }
  if (!is.null(mun)) {
    parts <- list(); failed <- character()
    for (uf in ufs$uf_code) {
      g <- fetch_geo(sub("{uf}", uf, br_source("malha_uf"), fixed = TRUE), sub("{uf}", uf, br_source("malha_uf", "name"), fixed = TRUE))
      if (is.null(g) || !"codarea" %in% names(g)) { failed <- c(failed, uf); if (length(failed) >= 3 && length(parts) == 0) break; next }
      g$territory_id <- paste("BR", uf, sprintf("%07d", as.integer(g$codarea)), sep = "-"); g$name <- mun$name[match(g$territory_id, paste("BR", mun$uf_code, mun$code, sep = "-"))]
      parts[[uf]] <- g[!is.na(g$name), c("territory_id", "name", attr(g, "sf_column"))]
    }
    if (length(failed)) message("  no municipality boundaries for UF ", paste(failed, collapse = ", "), if (length(parts) == 0) " (IBGE malhas API not reachable: municipalities shown as tables)")
    if (length(parts)) layers$municipality <- do.call(rbind, parts)
  }
  if (length(layers)) save_country_gpkg(con, "BR", layers, tolerance_m = 200)
  invisible(tbl)
}

register_country(list(
  code = "BR", iso3 = "BRA", name_en = "Brazil", name_local = "Brasil", lang = "pt",
  levels = c("region", "municipality"),
  level_labels = list(region = c("States", "Unidades da Federação", "State", "Unidade da Federação"),
                      municipality = c("Municipalities", "Municípios", "Municipality", "Município")),
  source = list(name = "SINESP VDE – base de dados dos gestores estaduais", publisher = "Ministério da Justiça e Segurança Pública (MJSP), SENASP",
                url = "https://www.gov.br/mj/pt-br/assuntos/sua-seguranca/seguranca-publica/estatistica/dados-nacionais-1/base-de-dados-e-notas-metodologicas-dos-gestores-estaduais-sinesp-vde-2022-e-2023",
                licence = "No licence stated on the dataset page (gov.br content: CC BY-ND 3.0 BR); public data, no registration",
                measure = "Police occurrences recorded by the state civil police forces (ocorrências, column `total`) by event and month; the lethal events and rape are reported as victims (`total_vitima`). No official total: the total shown is the sum of the mapped events."),
  population_source = "IBGE, estimativas da população residente (SIDRA, tabela 6579)",
  boundaries_attribution = "IBGE, malhas territoriais (API de malhas v3)",
  measure_labels = list(kind = "recorded_events", title = c("Recorded events (SINESP VDE)", "Ocorrências registradas (SINESP VDE)"), noun = c("recorded events", "ocorrências")),
  not_published = c("Not reported by the state for this month/event", "Não informado pelo gestor estadual neste mês/evento"),
  notes_en = "Figures are reported by each state (gestor estadual) under its own definitions; coverage gaps and revisions by state are listed on the MJSP page. Municipalities are matched by name to IBGE codes; unmatched names stay in the state totals only.",
  default_region = "BR-53",
  download = br_download, ingest = br_ingest, territory = br_territory, population = br_population
))
