# Chile: casos policiales by comuna, provincia and región -------------------------------------
# Source: CEAD (Centro de Estudios y Análisis del Delito, Ministerio de Seguridad Pública,
# formerly Subsecretaría de Prevención del Delito), "Estadísticas delictuales": casos
# policiales = denuncias + detenciones en flagrancia (Carabineros + PDI), by comuna, from 2005,
# monthly / quarterly / yearly. The site is a query tool with an export button and no download
# endpoint that could be verified: this module reads the table the USER exports and saves as
# data/raw/country_cl/cead_<anything>.csv (or .xlsx). `cl_download()` prints the instructions
# when no export is present (it never fails for that). The other inputs are downloaded:
# SUBDERE CUT codes (regiones/provincias/comunas), INE population projections by comuna
# (CC BY-SA 4.0) and the BCN comunas shapefile (free use with attribution).
#
# Expected export (long form, one row per comuna x period x delito): columns like
#   region | provincia | comuna [| cut/codigo] | año/anio | mes or trimestre | delito/indicador | casos/frecuencia
# A wide export (one column per year, year-month or year-quarter) is pivoted automatically.
# Columns are detected by name (CL_COLUMNS); `cl_inspect()` prints the headers of the files.

CL_STATE <- new.env()
CL_COLUMNS <- list(
  cut      = c("^cut$", "^c[oó]d(igo)?_?comuna", "^codigo$", "^cod_com"),
  comuna   = c("^comuna$", "^nombre_?comuna", "^unidad_?territorial", "^territorio$"),
  year     = c("^a[ñn]o$", "^anio$", "^year$"),
  month    = c("^mes$", "^month$"),
  quarter  = c("^trimestre$", "^quarter$"),
  period   = c("^per[ií]odo$", "^periodo$", "^fecha$"),
  offence  = c("^delito", "^indicador", "^grupo_?delictual", "^familia", "^categor[ií]a", "^tipo"),
  n        = c("^casos", "^frecuencia", "^valor", "^cantidad", "^total", "^n$")
)

cl_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }
cl_sources <- function() read_reference("cl_sources.csv")
cl_regions <- function() read_reference("cl_regions.csv")
cl_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", as.character(x)))))
cl_pad <- function(x, w) { v <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(x)))); ifelse(is.na(v), NA_character_, formatC(v, width = w, flag = "0")) }

cl_file <- function(name) {
  f <- newest_first(list.files(COUNTRY_MODULES$CL$dir_raw, paste0("_", gsub("([.()+])", "\\\\\\1", name), "$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}
cl_cead_files <- function() list.files(COUNTRY_MODULES$CL$dir_raw, "^cead.*\\.(csv|xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)

cl_instructions <- function() {
  message("  CEAD has no verified download endpoint. Export the table yourself:\n",
          "    1. Open ", cl_sources()$url[cl_sources()$key == "cead_page"][1], " > Estadísticas delictuales (Datos abiertos / Descarga).\n",
          "    2. Choose: Medida = Casos policiales; Unidad territorial = Comuna (all regions); Período = Mes (or Trimestre); Delitos = DMCS and its sub-types.\n",
          "    3. Export to CSV/XLSX and save it as ", file.path(COUNTRY_MODULES$CL$dir_raw, "cead_<descripcion>.csv"), " (one or several files; they are added up).\n",
          "    4. Run option 13 again; cl_inspect() prints what was read.")
}

# Download: CUT, INE population and BCN boundaries (versioned); then checks for a CEAD export
cl_download <- function(days = 365) {
  m <- COUNTRY_MODULES$CL; src <- cl_sources()
  for (key in c("cut", "ine_comunas", "bcn_comunas")) {
    rows <- src[src$key == key & !is.na(src$url) & src$url != "", ]; if (nrow(rows) == 0) next
    prev <- cl_file(rows$name[1])
    if (!is.na(prev) && file_age_days(prev) < days) { message("= CL ", rows$name[1], " downloaded less than ", days, " days ago"); next }
    ok <- FALSE
    for (i in seq_len(nrow(rows))) {
      ok <- tryCatch({ download_versioned(rows$url[i], m$dir_raw, name = rows$name[i]); TRUE }, error = function(e) { message("  CL ", key, " candidate ", i, " failed: ", conditionMessage(e)); FALSE })
      if (ok) break
    }
  }
  if (length(cl_cead_files()) == 0) cl_instructions() else message("  CEAD exports found: ", paste(basename(cl_cead_files()), collapse = ", "))
}

cl_read_any <- function(f) {
  if (grepl("\\.xlsx?$", f, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is needed to read ", basename(f))
    d <- readxl::read_excel(f, col_types = "text", .name_repair = "minimal")
  } else {
    first <- readLines(f, n = 1, warn = FALSE)
    sep <- if (lengths(regmatches(first, gregexpr(";", first))) >= lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
    d <- readr::read_delim(f, delim = sep, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
    if (any(grepl("�", names(d)))) d <- readr::read_delim(f, delim = sep, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE, locale = readr::locale(encoding = "latin1"))
  }
  names(d) <- trimws(gsub("^﻿", "", names(d)))
  d
}

cl_inspect <- function() {
  for (f in cl_cead_files()) { d <- cl_read_any(f); message(basename(f), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
  for (key in c("cut", "ine_comunas")) { f <- cl_file(cl_sources()$name[cl_sources()$key == key][1]); if (!is.na(f)) { d <- cl_read_any(f); message(key, ": ", basename(f), "\n  ", paste(names(d), collapse = " | ")) } }
  if (length(CL_STATE$unknown)) { message("Delito labels not in cl_offence_types.csv:"); print(CL_STATE$unknown) }
  if (length(CL_STATE$unmatched)) { message("Comuna names not matched to the CUT:"); print(CL_STATE$unmatched) }
}

# --- CUT: regiones, provincias, comunas --------------------------------------------------------
# From the SUBDERE workbook (header row found by the word "comuna"); else from the BCN shapefile
cl_cut <- function() {
  if (!is.null(CL_STATE$cut)) return(CL_STATE$cut)
  f <- cl_file(cl_sources()$name[cl_sources()$key == "cut"][1])
  out <- NULL
  if (!is.na(f) && requireNamespace("readxl", quietly = TRUE)) out <- tryCatch(cl_cut_from_xls(f), error = function(e) { message("  CUT workbook not understood: ", conditionMessage(e)); NULL })
  if (is.null(out)) { g <- cl_bcn(); if (!is.null(g)) out <- unique(sf::st_drop_geometry(g)[, c("region_code", "province_code", "province_name", "comuna_code", "comuna_name")]) }
  if (is.null(out)) stop("No CUT list: neither the SUBDERE workbook nor the BCN shapefile could be read")
  regs <- cl_regions(); out$region_name <- regs$region_name[match(out$region_code, regs$region_code)]
  out <- out[!is.na(out$comuna_code) & !is.na(out$region_name), ]
  CL_STATE$cut <- out
  out
}
cl_cut_from_xls <- function(f) {
  for (sh in readxl::excel_sheets(f)) {
    x <- as.data.frame(readxl::read_excel(f, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    h <- which(apply(x, 1, function(r) any(grepl("COMUNA", normalize_text(r)))))[1]
    if (is.na(h)) next
    nm <- normalize_text(as.character(x[h, ])); nm[is.na(nm)] <- ""
    d <- x[-(1:h), , drop = FALSE]; names(d) <- nm
    pick <- function(p) { i <- grep(p, nm); if (length(i)) d[[i[1]]] else NA }
    rc <- pick("^COD.*REGI"); pc <- pick("^COD.*PROVINCIA"); cc <- pick("^COD.*COMUNA")
    if (all(is.na(cc))) next
    out <- data.frame(region_code = cl_pad(rc, 2), province_code = cl_pad(pc, 3), province_name = trimws(pick("^NOMBRE.*PROVINCIA")),
                      comuna_code = cl_pad(cc, 5), comuna_name = trimws(pick("^NOMBRE.*COMUNA")), stringsAsFactors = FALSE)
    out <- out[!is.na(out$comuna_code), ]
    if (all(is.na(out$region_code))) out$region_code <- substr(out$comuna_code, 1, 2)
    if (all(is.na(out$province_code))) out$province_code <- substr(out$comuna_code, 1, 3)
    if (nrow(out) > 0) return(out)
  }
  stop("no sheet with comuna codes")
}

# BCN comunas shapefile with harmonised columns (codes detected by name, else by comuna name)
cl_bcn <- function() {
  if (!is.null(CL_STATE$bcn)) return(CL_STATE$bcn)
  f <- cl_file(cl_sources()$name[cl_sources()$key == "bcn_comunas"][1]); if (is.na(f)) return(NULL)
  g <- tryCatch(read_shp_zip(f), error = function(e) { message("  BCN shapefile not readable: ", conditionMessage(e)); NULL })
  if (is.null(g)) return(NULL)
  nm <- names(g)
  cc <- cl_pick(nm, c("^cod_?comuna", "^cut_?com", "^cut$", "^codcomuna", "^cod_com")); cn <- cl_pick(nm, c("^comuna$", "^nom_?com", "^nombre_?com"))
  pc <- cl_pick(nm, c("^cod_?prov", "^cut_?prov", "^codprov")); pn <- cl_pick(nm, c("^provincia$", "^nom_?prov", "^nombre_?prov"))
  rc <- cl_pick(nm, c("^cod_?regi", "^cut_?reg", "^codregion"))
  if (is.na(cc) && is.na(cn)) { message("  BCN shapefile: no comuna column among ", paste(nm, collapse = " | ")); return(NULL) }
  g$comuna_code <- if (!is.na(cc)) cl_pad(g[[cc]], 5) else NA_character_
  g$comuna_name <- if (!is.na(cn)) as.character(g[[cn]]) else NA_character_
  g$province_code <- if (!is.na(pc)) cl_pad(g[[pc]], 3) else substr(g$comuna_code, 1, 3)
  g$province_name <- if (!is.na(pn)) as.character(g[[pn]]) else NA_character_
  g$region_code <- if (!is.na(rc)) cl_pad(g[[rc]], 2) else substr(g$comuna_code, 1, 2)
  CL_STATE$bcn <- g
  g
}

# --- Ingest -------------------------------------------------------------------------------------
# Standardise one export to: comuna_code, year, month, quarter, label, n
cl_standardise <- function(d, f, cut) {
  nm <- names(d); col <- lapply(CL_COLUMNS, cl_pick, nm = nm)
  if (is.na(col$cut) && is.na(col$comuna)) stop("No comuna column in ", basename(f), ": ", paste(nm, collapse = " | "), "\nRun cl_inspect() and adjust CL_COLUMNS in R/85_cl.R")
  # Wide exports: one column per period
  per_cols <- nm[grepl("^(19|20)[0-9]{2}([-/_ .]?(0?[1-9]|1[0-2]|T[1-4]|[1-4]T|trim(estre)? ?[1-4]))?$", nm, ignore.case = TRUE)]
  if (length(per_cols) >= 1 && is.na(col$n)) {
    if (is.na(col$offence)) stop("Wide export without a delito column in ", basename(f), ": ", paste(nm, collapse = " | "))
    d <- tidyr_pivot_longer(d, per_cols); names(d)[names(d) == "year"] <- "periodo__"; names(d)[names(d) == "value"] <- "casos__"
    col$period <- "periodo__"; col$n <- "casos__"; col$year <- NA; col$month <- NA; col$quarter <- NA
  }
  if (is.na(col$n) || is.na(col$offence)) stop("Columns needed (delito, casos, año/mes or periodo) not found in ", basename(f), ": ", paste(nm, collapse = " | "))
  none <- rep(NA_integer_, nrow(d))
  year <- if (!is.na(col$year)) as.integer(cl_num(d[[col$year]])) else none
  month <- if (!is.na(col$month)) cl_month(d[[col$month]]) else none
  quarter <- if (!is.na(col$quarter)) suppressWarnings(as.integer(gsub("[^1-4]", "", d[[col$quarter]]))) else none
  if (!is.na(col$period)) {   # "2019", "2019-03", "2019 T1", "mar-2019"
    p <- trimws(as.character(d[[col$period]]))
    y2 <- suppressWarnings(as.integer(sub("^.*?((19|20)[0-9]{2}).*$", "\\1", p, perl = TRUE))); y2[!grepl("(19|20)[0-9]{2}", p)] <- NA
    rest <- trimws(gsub("(19|20)[0-9]{2}", "", p))
    q2 <- ifelse(grepl("T|TRIM", toupper(rest)), as.integer(gsub("[^1-4]", "", rest)), NA_integer_)
    m2 <- ifelse(is.na(q2) & nzchar(gsub("[^A-Za-z0-9]", "", rest)), cl_month(gsub("^[^A-Za-z0-9]+|[^A-Za-z0-9]+$", "", rest)), NA_integer_)
    year <- ifelse(is.na(year), y2, year); month <- ifelse(is.na(month), m2, month); quarter <- ifelse(is.na(quarter), q2, quarter)
  }
  code <- if (!is.na(col$cut)) cl_pad(d[[col$cut]], 5) else rep(NA_character_, nrow(d))
  if (!is.na(col$comuna)) {
    miss <- is.na(code) | !code %in% cut$comuna_code
    code[miss] <- cut$comuna_code[match(normalize_text(d[[col$comuna]][miss]), normalize_text(cut$comuna_name))]
    unm <- unique(d[[col$comuna]][is.na(code)]); unm <- unm[!is.na(unm) & !grepl("TOTAL|PA[IÍ]S|REGI[OÓ]N|PROVINCIA", toupper(unm))]
    if (length(unm)) { CL_STATE$unmatched <- unique(c(CL_STATE$unmatched, unm)); message("  ", basename(f), ": comuna names not matched to the CUT (ignored): ", paste(utils::head(unm, 20), collapse = "; ")) }
  }
  out <- data.frame(comuna_code = code, year = year, month = month, quarter = quarter, label = trimws(as.character(d[[col$offence]])), n = cl_num(d[[col$n]]), stringsAsFactors = FALSE)
  out[!is.na(out$comuna_code) & !is.na(out$year) & !is.na(out$label) & out$label != "", ]
}
cl_month <- function(x) {
  m <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(x)))); m[!is.na(m) & (m < 1 | m > 12)] <- NA
  es <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
  ifelse(is.na(m), match(substr(normalize_text(x), 1, 3), es), m)
}

cl_ingest <- function(con) {
  files <- cl_cead_files()
  if (length(files) == 0) { cl_instructions(); stop("No CEAD export in ", COUNTRY_MODULES$CL$dir_raw) }
  types <- read_reference("cl_offence_types.csv"); cut <- cl_cut(); CL_STATE$unknown <- NULL; CL_STATE$unmatched <- NULL
  parts <- list()
  for (f in files) {
    d <- cl_standardise(cl_read_any(f), f, cut)
    d$offence_code <- types$source_code[match(normalize_text(d$label), normalize_text(types$source_code))]
    unknown <- unique(d$label[is.na(d$offence_code)])
    if (length(unknown)) { CL_STATE$unknown <- unique(c(CL_STATE$unknown, unknown)); message("  ", basename(f), ": delitos not in cl_offence_types.csv (ignored): ", paste(unknown, collapse = "; ")) }
    d <- d[!is.na(d$offence_code), ]; d$file <- basename(f)
    message(sprintf("  %s: %s rows, %d comunas, %d-%d, periodicity: %s", basename(f), fmt_int(nrow(d)), length(unique(d$comuna_code)), min(d$year), max(d$year),
                    if (any(!is.na(d$month))) "monthly" else if (any(!is.na(d$quarter))) "quarterly" else "yearly"))
    parts[[f]] <- d
  }
  d <- do.call(rbind, parts)
  d$territory_id <- paste("CL", cut$region_code[match(d$comuna_code, cut$comuna_code)], cut$province_code[match(d$comuna_code, cut$comuna_code)], d$comuna_code, sep = "-")
  # Official DMCS total when exported; otherwise the sum of the exported DMCS components
  counted <- types$source_code[types$included_in == "total" & !is.na(types$included_in)]
  if (!"Delitos de mayor connotación social" %in% d$offence_code) {
    s <- d[d$offence_code %in% counted, ] |> dplyr::group_by(comuna_code, year, month, quarter, file, territory_id) |> dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop")
    if (nrow(s)) { s$offence_code <- "SUMA_DMCS"; s$label <- "SUMA_DMCS"; d <- rbind(d, s[, names(d)]); message("  no DMCS total in the export: the total is the sum of the exported DMCS components") }
  }
  # Levels: comuna, provincia, región, país (sums of the comunas)
  lvl <- list(municipality = d$territory_id, province = sub("-[^-]+$", "", d$territory_id), region = sub("-[^-]+$", "", sub("-[^-]+$", "", d$territory_id)), country = rep("CL", nrow(d)))
  all <- do.call(rbind, lapply(names(lvl), function(lv) { x <- d; x$territory_id <- lvl[[lv]]; x$level <- lv
    x |> dplyr::group_by(country = "CL", territory_id, level, offence_code, year, month, quarter, file) |> dplyr::summarise(n = sum(n, na.rm = TRUE), .groups = "drop") |> as.data.frame() }))
  # Periodicity -> cumulative at the end of each complete quarter
  to_cum <- function(x) {
    if (any(!is.na(x$month))) { x <- x[!is.na(x$month), ]; cum <- monthly_to_cumulative(x[, c("country", "territory_id", "level", "offence_code", "year", "month", "n")]) }
    else if (any(!is.na(x$quarter))) { x <- x[!is.na(x$quarter), ]; cum <- x |> dplyr::group_by(country, territory_id, level, offence_code, year) |> dplyr::arrange(quarter, .by_group = TRUE) |>
      dplyr::mutate(cumulative = cumsum(n)) |> dplyr::ungroup() |> dplyr::select(country, territory_id, level, offence_code, year, quarter, cumulative) |> as.data.frame() }
    else cum <- yearly_to_cumulative(x[, c("country", "territory_id", "level", "offence_code", "year", "n")])
    cum$file <- x$file[match(paste(cum$territory_id, cum$year), paste(x$territory_id, x$year))]
    cum
  }
  crime <- do.call(rbind, lapply(split(all, all$file), to_cum))
  # The same period from two exports is not added twice: keep the newest file's row
  crime <- crime[!duplicated(crime[, c("territory_id", "offence_code", "year", "quarter")], fromLast = TRUE), ]
  if (nrow(crime) == 0) stop("No complete period in the CEAD exports")
  ids <- unique(d[, c("territory_id", "comuna_code")])
  ensure_territories(con, "CL", ids$territory_id, "municipality", sub("-[^-]+$", "", ids$territory_id), cut$comuna_name[match(ids$comuna_code, cut$comuna_code)])
  replace_country_rows(con, "stg_country_crime", "CL", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_CL", paste(basename(files), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  CL: %s rows, %d territories, %d-%d", fmt_int(nrow(crime)), length(unique(crime$territory_id)), min(crime$year), max(crime$year)))
  invisible(crime)
}

# --- Population: INE projections by comuna (sum over sex and age), aggregated upwards ----------
cl_population <- function(con) {
  f <- cl_file(cl_sources()$name[cl_sources()$key == "ine_comunas"][1])
  if (is.na(f)) stop("No INE projections file in ", COUNTRY_MODULES$CL$dir_raw, " (download it from the INE page 'Cuadros estadísticos' and save it as <date>_ine_proyecciones_comunas.xlsx, or fix the URL in reference/cl_sources.csv)")
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is needed")
  cut <- cl_cut(); pop <- NULL
  for (sh in readxl::excel_sheets(f)) {
    x <- readxl::read_excel(f, sheet = sh, col_types = "text", .name_repair = "minimal"); nm <- trimws(names(x))
    yc <- grep("(19|20)[0-9]{2}$", nm); cc <- cl_pick(nm, c("^comuna$", "^c[oó]d(igo)?_?comuna", "^cut", "^codigo$"))
    if (length(yc) < 2 || is.na(cc)) next
    code <- cl_pad(x[[cc]], 5); ok <- code %in% cut$comuna_code
    if (sum(ok) == 0) { nc <- cl_pick(nm, c("^nombre_?comuna", "^comuna_?nombre")); if (!is.na(nc)) { code <- cut$comuna_code[match(normalize_text(x[[nc]]), normalize_text(cut$comuna_name))]; ok <- !is.na(code) } }
    if (sum(ok) == 0) next
    long <- do.call(rbind, lapply(yc, function(j) data.frame(comuna_code = code[ok], year = as.integer(sub("^.*((19|20)[0-9]{2})$", "\\1", nm[j])), population = cl_num(x[[j]][ok]))))
    pop <- long |> dplyr::group_by(comuna_code, year) |> dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") |> as.data.frame()
    break
  }
  if (is.null(pop)) stop("No sheet with comuna codes and year columns in ", basename(f), " (run cl_inspect())")
  i <- match(pop$comuna_code, cut$comuna_code)
  pop$territory_id <- paste("CL", cut$region_code[i], cut$province_code[i], pop$comuna_code, sep = "-")
  up <- function(ids, lv) { x <- pop; x$territory_id <- ids; x$level <- lv; x |> dplyr::group_by(territory_id, level, year) |> dplyr::summarise(population = sum(population), .groups = "drop") |> as.data.frame() }
  out <- rbind(up(pop$territory_id, "municipality"), up(sub("-[^-]+$", "", pop$territory_id), "province"),
               up(sub("-[^-]+$", "", sub("-[^-]+$", "", pop$territory_id)), "region"), up(rep("CL", nrow(pop)), "country"))
  out <- data.frame(country = "CL", out[, c("territory_id", "level", "year", "population")])
  replace_country_rows(con, "ref_country_population", "CL", out)
  message(sprintf("  population: %d territories, %d-%d (INE Chile, proyecciones base 2017)", length(unique(out$territory_id)), min(out$year), max(out$year)))
}

# --- Territories: CUT list + BCN comunas; provinces and regions as unions of the comunas --------
cl_territory <- function(con) {
  regs <- cl_regions(); cut <- cl_cut()
  provs <- unique(cut[, c("region_code", "province_code", "province_name")])
  provs$province_name[is.na(provs$province_name)] <- provs$province_code[is.na(provs$province_name)]
  tbl <- rbind(
    data.frame(territory_id = paste0("CL-", regs$region_code), level = "region", parent_id = "CL", official_code = regs$region_code, name = regs$region_name),
    data.frame(territory_id = paste("CL", provs$region_code, provs$province_code, sep = "-"), level = "province", parent_id = paste0("CL-", provs$region_code), official_code = provs$province_code, name = provs$province_name),
    data.frame(territory_id = paste("CL", cut$region_code, cut$province_code, cut$comuna_code, sep = "-"), level = "municipality", parent_id = paste("CL", cut$region_code, cut$province_code, sep = "-"),
               official_code = cut$comuna_code, name = cut$comuna_name))
  save_country_territory(con, "CL", tbl, scheme = "CUT")
  g <- cl_bcn()
  if (is.null(g)) {
    gb <- tryCatch(geoboundaries("CHL", 3), error = function(e) { message("  no comuna boundaries (", conditionMessage(e), ")"); NULL })
    if (!is.null(gb)) { gb$comuna_code <- cut$comuna_code[match(normalize_text(gb$shapeName), normalize_text(cut$comuna_name))]; g <- gb[!is.na(gb$comuna_code), ]; message("  comuna boundaries from geoBoundaries ADM3 (CC BY 4.0)") }
  }
  if (is.null(g)) return(invisible(tbl))
  code <- g$comuna_code
  if (all(is.na(code)) && "comuna_name" %in% names(g)) code <- cut$comuna_code[match(normalize_text(g$comuna_name), normalize_text(cut$comuna_name))]
  i <- match(code, cut$comuna_code); ok <- !is.na(i)
  g <- g[ok, ]; g$territory_id <- paste("CL", cut$region_code[i[ok]], cut$province_code[i[ok]], cut$comuna_code[i[ok]], sep = "-"); g$name <- cut$comuna_name[i[ok]]
  g <- g[, c("territory_id", "name", attr(g, "sf_column"))]
  message("  building provincia and región polygons from the comunas")
  nm <- setNames(tbl$name, tbl$territory_id)
  gp <- dissolve_territories(g, sub("-[^-]+$", "", g$territory_id), nm)        # provincias (repairs invalid BCN rings first)
  gr <- dissolve_territories(gp, sub("-[^-]+$", "", gp$territory_id), nm)      # regiones
  save_country_gpkg(con, "CL", list(region = gr, province = gp, municipality = g), tolerance_m = 150)
  invisible(tbl)
}

register_country(list(
  code = "CL", iso3 = "CHL", name_en = "Chile", name_local = "Chile", lang = "es",
  levels = c("region", "province", "municipality"),
  level_labels = list(region = c("Regions", "Regiones", "Region", "Región"), province = c("Provinces", "Provincias", "Province", "Provincia"),
                      municipality = c("Comunas", "Comunas", "Comuna", "Comuna")),
  source = list(name = "CEAD – Estadísticas delictuales (casos policiales)", publisher = "Centro de Estudios y Análisis del Delito (CEAD), Ministerio de Seguridad Pública",
                url = "https://cead.minsegpublica.gob.cl/",
                licence = "No licence stated (public statistics, free access without registration); table exported by hand from the CEAD tool",
                measure = "Casos policiales = denuncias formales + detenciones en flagrancia (Carabineros and PDI) by comuna and delito; the total is the official 'Delitos de mayor connotación social' (DMCS) aggregate when exported, otherwise the sum of the exported DMCS components."),
  population_source = "INE Chile, estimaciones y proyecciones de población 2002-2035 (base 2017) por comuna (CC BY-SA 4.0)",
  boundaries_attribution = "Biblioteca del Congreso Nacional de Chile (BCN), mapas vectoriales – comunas",
  not_published = c("Not in the exported table", "No incluido en la tabla exportada"),
  notes_en = "Casos policiales mix complaints and flagrancy arrests and are attributed to the comuna of registration; figures are provisional and revised by CEAD. The data must be exported by hand from the CEAD tool (no download endpoint).",
  method = list(where = "Comuna of the police unit that registered the complaint or the flagrancy arrest, not necessarily where the facts happened", date = "Month the case was registered (complaint filed or arrest made)", revisions = "Provisional figures revised by CEAD; the table is exported by hand, so the loaded version is as recent as the export"),
  default_region = "CL-13",
  download = cl_download, ingest = cl_ingest, territory = cl_territory, population = cl_population
))
