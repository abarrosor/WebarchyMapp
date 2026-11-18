# Colombia: recorded crime by municipio and departamento ----------------------------------
# Source: Policía Nacional de Colombia (DIJIN), record-level "Estadística delictiva" extracted
# from SIEDCO and published on the Portal Nacional de Datos Abiertos (datos.gov.co, Socrata)
# under CC BY-SA 4.0, one dataset per offence family. Only the six open-licensed families are
# used: vehicle/motorcycle theft, residential/commercial theft, lesiones personales (traffic
# injuries excluded), delitos sexuales, violencia intrafamiliar and amenazas. Hurto a personas
# and homicidio intencional are NOT on the portal (only in un-licensed Excel files on
# policia.gov.co), so the "total" is the sum of the six families and not an official total.
# Rows are events with a daily `fecha_hecho` (text dd/mm/yyyy) and a `codigo_dane` of 8 digits
# (5-digit DANE municipio + 3-digit centro poblado; "00000000" = unknown). The download asks
# the SODA API for MONTHLY sums per codigo_dane (and sub-type), 50,000 rows per page, so the
# raw files hold aggregates (no gender / age group). Names: DANE DIVIPOLA (datos.gov.co,
# CC BY-SA 4.0). Population: DANE municipal projections (xlsx). Boundaries: geoBoundaries ADM2
# (DANE-derived, CC BY 4.0); departamentos are unions of their municipios.
#
# Written against the live API (columns and labels checked on 2026-11-03) but not run: readers
# detect columns by name and fail naming the file. `co_inspect()` prints the headers.

CO_BASE <- "https://www.datos.gov.co/resource"
CO_PAGE <- 50000L
# One entry per dataset family: Socrata id, sub-type column (NA = none) and the fixed code
# used when the family has no sub-type column (or when the sub-types are all one type)
CO_DATASETS <- list(
  vehicle_theft  = list(id = "9vha-vh9n", type_col = "tipo_de_hurto",        label = "Hurto de automotores y motocicletas"),
  property_theft = list(id = "6sqw-8cg5", type_col = "tipo_de_hurto",        label = "Hurto a residencias y entidades comerciales"),
  injuries       = list(id = "72sg-cybi", type_col = "descripci_n_conducta", label = "Lesiones personales (y en accidente de tránsito)"),
  sexual         = list(id = "fpe5-yrmw", type_col = NA, code = "DELITOS SEXUALES",        label = "Delitos sexuales"),
  domestic       = list(id = "vuyt-mqpw", type_col = NA, code = "VIOLENCIA INTRAFAMILIAR", label = "Violencia intrafamiliar"),
  threats        = list(id = "meew-mguv", type_col = NA, code = "AMENAZAS",                label = "Amenazas")
)
CO_DIVIPOLA <- "gdxc-w37w"   # DANE "DIVIPOLA - Códigos municipios" (cod_dpto, dpto, cod_mpio, nom_mpio, tipo_municipio, longitud, latitud)
CO_POPULATION <- c(          # DANE "Serie municipal de población por área" (xlsx; area = Total)
  "https://www.dane.gov.co/files/censo2018/proyecciones-de-poblacion/Municipal/DCD-area-proypoblacion-Mun-2005-2017_VP.xlsx",
  "https://www.dane.gov.co/files/censo2018/proyecciones-de-poblacion/Municipal/PPED-AreaMun-2018-2042_VP.xlsx"
)
# Accepted column names (first match wins, case-insensitive)
CO_COLUMNS <- list(
  crime = list(code = c("^codigo_dane$", "^cod.*dane"), dpto = c("^departamento$"), muni = c("^municipio$"), month = c("^mes$"),
               n = c("^n$", "^sum_cantidad$", "^cantidad$")),
  divipola = list(dpto_code = c("^cod_dpto$", "^c[oó]d.*d(e)?pto", "^codigo_departamento$"), dpto_name = c("^dpto$", "^nombre_departamento$", "^departamento$"),
                  muni_code = c("^cod_mpio$", "^c[oó]d.*mpio", "^codigo_municipio$"), muni_name = c("^nom_mpio$", "^nombre_municipio$", "^municipio$"),
                  type = c("^tipo"), lon = c("^longitud$", "^lon"), lat = c("^latitud$", "^lat")),
  population = list(code = c("^DPMP$", "^MPIO$", "^C[oó]d.*mun", "^codigo_municipio$", "^C.{1,3}digo[ _]?DANE", "^Divipola"), year = c("^A[ÑN]O$", "^A.{1,2}O$", "^year$", "^Vigencia$"),
                    area = c("^[ÁA]REA", "^A.{1,2}REA", "^area_geografica$"), pop = c("^Poblaci[oó]n", "^Poblaci", "^Total", "^Personas", "Poblaci[oó]n"))
)

co_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }
co_dir <- function() COUNTRY_MODULES$CO$dir_raw
# Newest raw file whose name ends in `_<name>`; NA when none
co_file <- function(name) {
  f <- newest_first(list.files(co_dir(), paste0("_", gsub("([.()+])", "\\\\\\1", name), "$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}
co_read_csv <- function(f) readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE)

# --- Download ----------------------------------------------------------------------------------

# SODA query (SoQL) for monthly sums per codigo_dane (+ names + sub-type) of one dataset, one page.
# `substring(fecha_hecho, 4, 7)` turns the text date dd/mm/yyyy into "mm/yyyy"; `cantidad` is text
# in some datasets, hence the cast.
co_soda_url <- function(ds, offset = 0L, limit = CO_PAGE) {
  dims <- c("codigo_dane", "departamento", "municipio", if (!is.na(ds$type_col)) ds$type_col)
  select <- paste(c(dims, "substring(fecha_hecho,4,7) as mes", "sum(cantidad::number) as n"), collapse = ",")
  q <- sprintf("$select=%s&$group=%s&$order=%s&$limit=%d&$offset=%d", select, paste(c(dims, "mes"), collapse = ","),
               paste(c("codigo_dane", "mes", dims[-1]), collapse = ","), limit, offset)
  utils::URLencode(sprintf("%s/%s.csv?%s", CO_BASE, ds$id, q))
}
co_page_name <- function(family, page) sprintf("co_%s_p%02d.csv", family, page)

# Pages 1..n of one family (newest version of each; stops at the first missing page)
co_pages <- function(family) {
  out <- character(); for (p in 1:200) { f <- co_file(co_page_name(family, p)); if (is.na(f)) break; out <- c(out, f) }
  out
}

co_download <- function(days = 30, ref_days = 365) {
  dir <- co_dir()
  # Crime: one family at a time, 50,000 aggregated rows per page (a few pages per family)
  for (fam in names(CO_DATASETS)) {
    ds <- CO_DATASETS[[fam]]; first <- co_file(co_page_name(fam, 1))
    if (!is.na(first) && file_age_days(first) < days) { message("= CO ", fam, " downloaded less than ", days, " days ago"); next }
    message("CO ", fam, " (", ds$id, "): monthly sums per municipio from the SODA API")
    for (p in 1:200) {
      f <- download_versioned(co_soda_url(ds, offset = (p - 1L) * CO_PAGE), dir, name = co_page_name(fam, p))
      rows <- length(readLines(f, warn = FALSE)) - 1L
      if (rows < CO_PAGE) break
      Sys.sleep(1)
    }
  }
  # DIVIPOLA names and centroids (DANE)
  f <- co_file("co_divipola.csv")
  if (is.na(f) || file_age_days(f) > ref_days)
    download_versioned(utils::URLencode(sprintf("%s/%s.csv?$limit=5000&$order=cod_mpio", CO_BASE, CO_DIVIPOLA)), dir, name = "co_divipola.csv")
  # Population projections (DANE xlsx)
  for (u in CO_POPULATION) {
    f <- co_file(basename(u))
    if (is.na(f) || file_age_days(f) > ref_days) download_versioned(u, dir)
  }
}

# Print the header and first rows of every downloaded file (to adjust CO_COLUMNS / CO_DATASETS)
co_inspect <- function() {
  for (fam in names(CO_DATASETS)) { p <- co_pages(fam); if (!length(p)) next; d <- co_read_csv(p[1]); message(fam, ": ", length(p), " page(s), ", basename(p[1]), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
  f <- co_file("co_divipola.csv"); if (!is.na(f)) { d <- co_read_csv(f); message("divipola: ", basename(f), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
  for (u in CO_POPULATION) { f <- co_file(basename(u)); if (is.na(f)) next; message("population: ", basename(f)); for (s in readxl::excel_sheets(f)) { message("  sheet ", s); print(utils::head(as.data.frame(readxl::read_excel(f, sheet = s, col_names = FALSE, n_max = 15, .name_repair = "minimal")), 15)) } }
}

# --- Territories --------------------------------------------------------------------------------

co_departments <- function() read_reference("co_departments.csv")

# DIVIPOLA: region_code, region_name, municipality_code, municipality_name, type, lon, lat (NULL when not downloaded)
co_divipola <- function() {
  f <- co_file("co_divipola.csv"); if (is.na(f)) return(NULL)
  d <- co_read_csv(f); col <- lapply(CO_COLUMNS$divipola, co_pick, nm = names(d))
  if (is.na(col$muni_code) || is.na(col$muni_name)) stop("Unexpected columns in ", basename(f), ": ", paste(names(d), collapse = " | "), "\nRun co_inspect() and adjust CO_COLUMNS$divipola in R/78_co.R")
  code <- gsub("\\D", "", d[[col$muni_code]]); code <- ifelse(nchar(code) == 4, paste0("0", code), code)
  num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
  out <- data.frame(region_code = if (!is.na(col$dpto_code)) sprintf("%02d", as.integer(d[[col$dpto_code]])) else substr(code, 1, 2),
                    region_name = if (!is.na(col$dpto_name)) co_title(d[[col$dpto_name]]) else NA_character_,
                    municipality_code = code, municipality_name = co_title(d[[col$muni_name]]),
                    type = if (!is.na(col$type)) d[[col$type]] else NA_character_,
                    lon = if (!is.na(col$lon)) num(d[[col$lon]]) else NA_real_, lat = if (!is.na(col$lat)) num(d[[col$lat]]) else NA_real_, stringsAsFactors = FALSE)
  out <- out[nchar(out$municipality_code) == 5 & !duplicated(out$municipality_code), ]
  deps <- co_departments(); out$region_name <- ifelse(out$region_code %in% deps$region_code, deps$region_name[match(out$region_code, deps$region_code)], out$region_name)
  # Keep a readable copy of the names actually used (built at run time from the DANE file)
  tryCatch(readr::write_csv(out, file.path(CONFIG$dir_ref, "co_territories.csv")), error = function(e) message("  could not write reference/co_territories.csv: ", conditionMessage(e)))
  out
}

# "BOGOTÁ, D.C." -> "Bogotá, D.C."; "SAN JOSÉ DE LA MONTAÑA" -> "San José de la Montaña"
co_title <- function(x) {
  small <- c("de", "del", "la", "las", "los", "y", "el", "en")
  out <- vapply(strsplit(tolower(trimws(x)), " ", fixed = TRUE), function(w) {
    if (!length(w) || is.na(w[1])) return(NA_character_)
    cap <- paste0(toupper(substring(w, 1, 1)), substring(w, 2)); keep <- w %in% small & seq_along(w) > 1
    cap[keep] <- w[keep]; paste(cap, collapse = " ") }, "")
  trimws(gsub("D\\.c\\.", "D.C.", gsub("\\(ct\\)", "", out, ignore.case = TRUE)))
}

co_territory <- function(con) {
  deps <- co_departments(); div <- co_divipola()
  tbl <- data.frame(territory_id = paste0("CO-", deps$region_code), level = "region", parent_id = "CO", official_code = deps$region_code, name = deps$region_name)
  if (!is.null(div)) {
    tbl <- rbind(tbl, data.frame(territory_id = co_ids("CO", div$region_code, div$municipality_code), level = "municipality",
                                 parent_id = paste0("CO-", div$region_code), official_code = div$municipality_code, name = div$municipality_name))
  } else message("  DIVIPOLA not downloaded yet: municipality names will come from the crime files")
  save_country_territory(con, "CO", tbl, scheme = "DANE")
  # Boundaries: geoBoundaries ADM2 (1,122 units, DANE-derived). The file has names but no DANE
  # code: codes are attached by locating the DIVIPOLA centroid of each municipio inside a polygon,
  # then by name for the rest. Departamentos = union of their municipios.
  g <- tryCatch(geoboundaries("COL", 2), error = function(e) { message("  geoBoundaries not available: tables only (", conditionMessage(e), ")"); NULL })
  if (!is.null(g) && !is.null(div)) {
    g <- sf::st_make_valid(sf::st_transform(g, 4326)); g$code <- NA_character_
    pts <- div[!is.na(div$lon) & !is.na(div$lat), ]
    if (nrow(pts)) {
      p <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
      hit <- sf::st_within(p, g); n_hit <- lengths(hit)
      poly_of_pt <- ifelse(n_hit == 1, vapply(hit, function(i) if (length(i) == 1) i else NA_integer_, 1L), NA_integer_)
      ok <- !is.na(poly_of_pt) & !duplicated(poly_of_pt) & !poly_of_pt %in% poly_of_pt[duplicated(poly_of_pt)]   # one point per polygon
      g$code[poly_of_pt[ok]] <- pts$municipality_code[ok]
    }
    name_col <- co_pick(names(g), c("^shapeName$", "^name$", "^NOMBRE"))
    if (!is.na(name_col)) {   # the rest by normalised name, among the codes still free
      free <- div[!div$municipality_code %in% g$code, ]; todo <- which(is.na(g$code))
      m <- match(normalize_text(g[[name_col]][todo]), normalize_text(free$municipality_name))
      dup <- duplicated(m, incomparables = NA) | duplicated(m, fromLast = TRUE, incomparables = NA)
      g$code[todo[!is.na(m) & !dup]] <- free$municipality_code[m[!is.na(m) & !dup]]
    }
    message(sprintf("  boundaries: %d of %d polygons matched to a DANE code (%d by centroid)", sum(!is.na(g$code)), nrow(g), if (nrow(pts)) sum(ok) else 0))
    g <- g[!is.na(g$code), ]
    g$territory_id <- co_ids("CO", substr(g$code, 1, 2), g$code); g$name <- div$municipality_name[match(g$code, div$municipality_code)]
    g <- g[, c("territory_id", "name", attr(g, "sf_column"))]
    gr <- g; gr$territory_id <- sub("-[^-]+$", "", gr$territory_id)
    gr <- gr |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop"); gr$name <- deps$region_name[match(sub("^CO-", "", gr$territory_id), deps$region_code)]
    save_country_gpkg(con, "CO", list(region = gr, municipality = g), tolerance_m = 150)
  }
  invisible(tbl)
}
co_ids <- function(cc, ...) { a <- list(...); if (!length(a[[1]])) return(character()); unname(do.call(mapply, c(list(FUN = function(...) territory_id(cc, ...), USE.NAMES = FALSE), a))) }

# --- Ingest -------------------------------------------------------------------------------------

# One family -> monthly rows: code8, dpto_name, muni_name, offence_code, year, month, n
co_read_family <- function(fam) {
  ds <- CO_DATASETS[[fam]]; pages <- co_pages(fam); if (!length(pages)) return(NULL)
  d <- do.call(rbind, lapply(pages, co_read_csv))
  col <- lapply(CO_COLUMNS$crime, co_pick, nm = names(d))
  if (is.na(col$code) || is.na(col$month) || is.na(col$n))
    stop("Unexpected columns in ", basename(pages[1]), ": ", paste(names(d), collapse = " | "), "\nRun co_inspect() and adjust CO_COLUMNS$crime in R/78_co.R")
  type <- if (!is.na(ds$type_col)) { tc <- co_pick(names(d), paste0("^", ds$type_col, "$")); if (is.na(tc)) stop("Column ", ds$type_col, " not found in ", basename(pages[1]), ": ", paste(names(d), collapse = " | ")); gsub("\\s+", " ", trimws(d[[tc]])) } else ds$code
  mes <- trimws(d[[col$month]]); bad <- !grepl("^\\d{2}/\\d{4}$", mes)
  if (any(bad)) message(sprintf("  %s: %d rows with an unexpected month value (e.g. '%s') dropped", fam, sum(bad), mes[bad][1]))
  out <- data.frame(code8 = gsub("\\D", "", d[[col$code]]), dpto_name = if (!is.na(col$dpto)) d[[col$dpto]] else NA_character_,
                    muni_name = if (!is.na(col$muni)) d[[col$muni]] else NA_character_, offence_code = type,
                    year = as.integer(substr(mes, 4, 7)), month = as.integer(substr(mes, 1, 2)),
                    n = suppressWarnings(as.numeric(d[[col$n]])), family = fam, file = basename(pages[1]), stringsAsFactors = FALSE)
  out <- out[!bad & !is.na(out$n), ]
  # Stale pages from an older, longer download would repeat keys: keep the first occurrence
  out[!duplicated(out[, c("code8", "dpto_name", "muni_name", "offence_code", "year", "month")]), ]
}

co_ingest <- function(con) {
  deps <- co_departments(); types <- read_reference("co_offence_types.csv")
  fams <- lapply(names(CO_DATASETS), co_read_family); names(fams) <- names(CO_DATASETS)
  fams <- fams[!vapply(fams, is.null, TRUE)]
  if (!length(fams)) stop("No Colombian files found in ", co_dir(), " (run the download first)")
  message("  families loaded: ", paste(names(fams), collapse = ", "), if (length(fams) < length(CO_DATASETS)) paste0(" (missing: ", paste(setdiff(names(CO_DATASETS), names(fams)), collapse = ", "), ")") else "")
  d <- do.call(rbind, fams)
  unknown <- setdiff(unique(d$offence_code), types$source_code)
  if (length(unknown)) message("  codes not in reference/co_offence_types.csv (not shown until mapped): ", paste(unknown, collapse = " ; "))
  # Municipio (5 digits) and departamento (2 digits) from the 8-digit code; unknown codes
  # ("00000000") fall back to the departamento name
  muni <- ifelse(nchar(d$code8) == 8 & !grepl("^00", d$code8), substr(d$code8, 1, 5), NA_character_)
  dpto <- ifelse(!is.na(muni), substr(muni, 1, 2), deps$region_code[match(normalize_text(d$dpto_name), normalize_text(deps$region_name))])
  dpto[!is.na(dpto) & !dpto %in% deps$region_code] <- NA
  muni[is.na(dpto)] <- NA
  message(sprintf("  %s monthly rows; %s cases without municipio, %s without departamento", fmt_int(nrow(d)), fmt_int(sum(d$n[is.na(muni)])), fmt_int(sum(d$n[is.na(dpto)]))))
  base <- data.frame(country = "CO", offence_code = d$offence_code, year = d$year, month = d$month, n = d$n, file = d$file, stringsAsFactors = FALSE)
  m_muni <- base[!is.na(muni), ]; m_muni$territory_id <- co_ids("CO", dpto[!is.na(muni)], muni[!is.na(muni)]); m_muni$level <- "municipality"
  m_reg  <- base[!is.na(dpto), ]; m_reg$territory_id <- paste0("CO-", dpto[!is.na(dpto)]); m_reg$level <- "region"
  m_nat  <- base; m_nat$territory_id <- "CO"; m_nat$level <- "country"          # national = every published case
  monthly <- rbind(m_muni, m_reg, m_nat) |>
    dplyr::group_by(country, territory_id, level, offence_code, year, month, file) |> dplyr::summarise(n = sum(n), .groups = "drop")
  # Municipios present in the data but absent from DIVIPOLA (names from the files)
  nm <- d[!is.na(muni), ]; nm$tid <- m_muni$territory_id; nm <- nm[!duplicated(nm$tid), ]
  ensure_territories(con, "CO", nm$tid, "municipality", sub("-[^-]+$", "", nm$tid), co_title(nm$muni_name))
  # Synthetic total = sum of the mapped families (never an official total)
  mapped <- types$source_code[types$offence_type_id != "unmapped" & types$source_code != "TOTAL_FAMILIAS"]
  tot <- monthly[monthly$offence_code %in% mapped, ] |> dplyr::group_by(country, territory_id, level, year, month) |>
    dplyr::summarise(n = sum(n), .groups = "drop")
  tot$offence_code <- "TOTAL_FAMILIAS"; tot$file <- "co_total (sum of families)"
  monthly <- rbind(as.data.frame(monthly), as.data.frame(tot)[, names(monthly)])
  crime <- monthly_to_cumulative(monthly, keys = c("country", "territory_id", "level", "offence_code", "file"))
  replace_country_rows(con, "stg_country_crime", "CO", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  files <- unlist(lapply(names(fams), function(f) basename(co_pages(f))))
  log_ingest(con, "country_CO", paste(files, collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  message(sprintf("  CO: %s rows, %d territories, %d-%d (complete quarters only)", fmt_int(nrow(crime)), length(unique(crime$territory_id)), min(crime$year), max(crime$year)))
  invisible(crime)
}

# --- Population ---------------------------------------------------------------------------------

# One DANE xlsx -> municipality_code, year, population (area "Total"). The header row is found by
# scanning the first rows of every sheet for the year and population columns.
co_read_population <- function(f) {
  for (s in readxl::excel_sheets(f)) {
    all <- as.data.frame(readxl::read_excel(f, sheet = s, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    hdr <- NA_integer_
    for (i in seq_len(min(40, nrow(all)))) { v <- trimws(as.character(unlist(all[i, ]))); v <- v[!is.na(v)]
      if (!is.na(co_pick(v, CO_COLUMNS$population$year)) && !is.na(co_pick(v, CO_COLUMNS$population$pop)) && !is.na(co_pick(v, CO_COLUMNS$population$code))) { hdr <- i; break } }
    if (is.na(hdr)) next
    d <- all[-seq_len(hdr), , drop = FALSE]; names(d) <- trimws(as.character(unlist(all[hdr, ]))); names(d)[is.na(names(d))] <- ""
    col <- lapply(CO_COLUMNS$population, co_pick, nm = names(d))
    # DANE swaps DPMP / MPIO between editions (one holds the 5-digit code, the other the name): take the column with most 5-digit values
    cands <- unique(c(col$code, names(d)[grepl("^(DPMP|MPIO)$", names(d), ignore.case = TRUE)])); cands <- cands[!is.na(cands)]
    score <- vapply(cands, function(cn) mean(grepl("^0?[0-9]{4,5}$", trimws(as.character(d[[cn]]))), na.rm = TRUE), numeric(1))
    if (length(score)) col$code <- cands[which.max(score)]
    code <- gsub("\\D", "", as.character(d[[col$code]])); code <- ifelse(nchar(code) == 4, paste0("0", code), code)
    area <- if (!is.na(col$area)) normalize_text(d[[col$area]]) else "TOTAL"
    pv <- trimws(as.character(d[[col$pop]])); pop <- suppressWarnings(as.numeric(pv)); i <- is.na(pop) & !is.na(pv)
    pop[i] <- suppressWarnings(as.numeric(gsub("[^0-9]", "", pv[i])))                     # "2.600.000" written as text
    out <- data.frame(municipality_code = code, year = suppressWarnings(as.integer(d[[col$year]])), population = pop, area = area, stringsAsFactors = FALSE)
    out <- out[nchar(out$municipality_code) == 5 & !is.na(out$year) & !is.na(out$population) & (is.na(out$area) | grepl("^TOTAL", out$area)), ]
    if (nrow(out)) return(out[, c("municipality_code", "year", "population")])
  }
  # Not readable: say what the sheets look like (so the patterns can be adjusted) and let the other file through
  message("  population file not readable: ", basename(f), " (no sheet with municipality code, year and population columns; adjust CO_COLUMNS$population in R/78_co.R). First rows seen:")
  for (s in readxl::excel_sheets(f)) {
    all <- as.data.frame(readxl::read_excel(f, sheet = s, col_names = FALSE, col_types = "text", n_max = 15, .name_repair = "minimal"))
    rows <- apply(all, 1, function(r) { r <- trimws(as.character(r)); r <- r[!is.na(r) & nzchar(r)]; paste(utils::head(r, 10), collapse = " | ") })
    rows <- rows[nzchar(rows)]
    message("    sheet ", s, ": ", paste(utils::head(rows, 4), collapse = "  //  "))
  }
  NULL
}

co_population <- function(con, since = 2010) {
  files <- vapply(CO_POPULATION, function(u) co_file(basename(u)), ""); files <- files[!is.na(files)]
  if (!length(files)) stop("No DANE population file in ", co_dir(), " (run the download first)")
  pop <- do.call(rbind, Filter(Negate(is.null), lapply(files, co_read_population)))
  if (is.null(pop) || nrow(pop) == 0) stop("No DANE population file could be read (see the messages above)")
  pop <- pop[pop$year >= since & !duplicated(pop[, c("municipality_code", "year")]), ]
  deps <- co_departments(); reg <- substr(pop$municipality_code, 1, 2); pop <- pop[reg %in% deps$region_code, ]; reg <- reg[reg %in% deps$region_code]
  muni <- data.frame(country = "CO", territory_id = co_ids("CO", reg, pop$municipality_code), level = "municipality", year = pop$year, population = pop$population)
  regs <- muni |> dplyr::mutate(territory_id = sub("-[^-]+$", "", territory_id), level = "region") |> dplyr::group_by(country, territory_id, level, year) |> dplyr::summarise(population = sum(population), .groups = "drop")
  nat <- regs |> dplyr::group_by(country, year) |> dplyr::summarise(population = sum(population), .groups = "drop"); nat$territory_id <- "CO"; nat$level <- "country"
  out <- rbind(muni, as.data.frame(regs)[, names(muni)], as.data.frame(nat)[, names(muni)])
  replace_country_rows(con, "ref_country_population", "CO", out)
  message(sprintf("  population: %d territories, %d-%d (DANE projections, %s)", length(unique(out$territory_id)), min(out$year), max(out$year), paste(basename(files), collapse = " + ")))
}

register_country(list(
  code = "CO", iso3 = "COL", name_en = "Colombia", name_local = "Colombia", lang = "es",
  levels = c("region", "municipality"),
  level_labels = list(region = c("Departments", "Departamentos", "Department", "Departamento"),
                      municipality = c("Municipalities", "Municipios", "Municipality", "Municipio")),
  source = list(name = "Estadística delictiva de la Policía Nacional (SIEDCO) - datasets abiertos", publisher = "Policía Nacional de Colombia, DIJIN (via datos.gov.co, MinTIC)",
                url = "https://www.datos.gov.co/browse?q=DIJIN%20Polic%C3%ADa%20Nacional&sortBy=relevance",
                licence = "Creative Commons Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)",
                measure = "Offences (casos) recorded by the Policía Nacional in SIEDCO, by month of the event, for six offence families only: vehicle theft, residential and commercial theft, personal injury (traffic excluded), sexual offences, domestic violence and threats. No official total: the total shown is their sum. Hurto a personas and homicide are not published as open data."),
  population_source = "DANE, proyecciones de población municipales por área (series 2005-2017 and 2018-2042)",
  boundaries_attribution = "geoBoundaries (CC BY 4.0), derived from DANE MGN",
  measure_labels = list(kind = "reported_cases", title = c("Reported cases (SIEDCO)", "Casos reportados (SIEDCO)"), noun = c("reported cases", "casos reportados")),
  not_published = c("Every recorded case is published (no suppression); a municipality without a row had no recorded case", "Se publican todos los casos registrados (sin supresión); un municipio sin fila no tuvo casos registrados"),
  notes_en = "Figures are provisional until March of the following year and are revised retroactively. Hurto a residencias exists in the open data only from 2024, so the summed total jumps that year; the commercial-theft series doubles in 2017 (new complaint-validation method with the Fiscalía). Hurto a personas, homicide, extortion and kidnapping are missing from the open-data portal. Bogotá (20 localidades, women victims only) and Medellín (comunas) have separate city datasets not loaded here.",
  method = list(where = "Municipio where the event happened, as registered by the Policía Nacional in SIEDCO", date = "Month of the event (fecha del hecho)", revisions = "Provisional until March of the following year; revised retroactively as complaints are validated with the Fiscalía"),
  default_region = "CO-11",
  download = co_download, ingest = co_ingest, territory = co_territory, population = co_population
))
