# Costa Rica: OIJ police statistics by provincia, cantón and distrito ---------------------
# Source: Poder Judicial de Costa Rica, Organismo de Investigación Judicial (OIJ),
# "Estadísticas Policiales", Portal de Datos Abiertos del Poder Judicial, CC BY.
# One CSV per year (2015 onwards, refreshed monthly), one row per record of a complaint
# (denuncia) received by the OIJ: a complaint with several victims or objects gives several
# rows. The CSV has NO header row, 11 comma-separated fields padded with trailing spaces
# and, in some years, "Ñ" corrupted to "?". Column order assumed (verified on the 2015 and
# 2026 files and against the XML version, which names the fields):
#   1 Delito, 2 SubDelito, 3 Fecha (YYYY-MM-DD), 4 Victima, 5 SubVictima, 6 Edad,
#   7 Genero (always empty), 8 Nacionalidad, 9 Provincia, 10 Canton, 11 Distrito
# (the XML has 10 fields without Genero: a 10-field CSV is read with that order).
# Levels: region = provincia (7), province = cantón (84, official DTA codes in
# reference/cr_cantones.csv), municipality = distrito (~490). The files carry NO codes:
# provincias and cantones are matched by name (with aliases for renamed cantones); distritos
# have no verified official code list, so their identifier is the normalised name inside
# the cantón (CR-<prov>-<cantón>-<NAME>). Set CR_DISTRICTS <- FALSE to drop that level.
# Population: INEC projections (no open URL could be verified: the file is read from
# data/raw/country_cr/ if the user downloads it). Boundaries: geoBoundaries ADM1/2/3.
#
# Written against the published files (not run against them): `cr_inspect()` prints the
# first rows of every downloaded file so that CR_COLUMNS can be adjusted.

CR_DISTRICTS <- TRUE
CR_URL <- "https://pjcrdatosabiertos.blob.core.windows.net/datosabiertos/PJCROD_POLICIALES_V1/PJCROD_POLICIALES_V1-%d.csv"
CR_FIRST_YEAR <- 2015L
CR_COLUMNS <- c("delito", "subdelito", "fecha", "victima", "subvictima", "edad", "genero", "nacionalidad", "provincia", "canton", "distrito")
CR_STATE <- new.env()

cr_years <- function() CR_FIRST_YEAR:as.integer(format(Sys.Date(), "%Y"))
cr_file <- function(year) {
  f <- newest_first(list.files(COUNTRY_MODULES$CR$dir_raw, sprintf("_cr_policiales_%d\\.csv$", year), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}
# Matching key for names: upper case, no accents, "?" (corrupted Ñ) and Ñ -> N, one space
cr_key <- function(x) { k <- normalize_text(x); k <- gsub("[?Ñ]", "N", k); k <- gsub("[^A-Z0-9 ]", "", k); k[k == ""] <- NA; k }

# Closed years are refreshed once a year, the current and previous year every `days` days
cr_download <- function(days = 30) {
  m <- COUNTRY_MODULES$CR; this <- as.integer(format(Sys.Date(), "%Y"))
  for (y in cr_years()) {
    prev <- cr_file(y); limit <- if (y >= this - 1) days else 365
    if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < limit) { message("= CR ", y, " downloaded less than ", limit, " days ago"); next }
    tryCatch(download_versioned(sprintf(CR_URL, y), m$dir_raw, name = sprintf("cr_policiales_%d.csv", y)),
             error = function(e) message("  CR ", y, ": ", conditionMessage(e)))
  }
}

# Read one yearly file: no header, explicit names, trimmed fields, encoding detected
cr_read <- function(f) {
  probe <- readLines(f, n = 5000, warn = FALSE)
  enc <- if (all(validUTF8(probe))) "UTF-8" else "latin1"
  d <- readr::read_csv(f, col_names = FALSE, col_types = readr::cols(.default = "c"), locale = readr::locale(encoding = enc),
                       trim_ws = TRUE, na = character(), progress = FALSE, show_col_types = FALSE)
  names(d) <- if (ncol(d) == 11) CR_COLUMNS else if (ncol(d) == 10) setdiff(CR_COLUMNS, "genero") else
    stop("Unexpected number of fields (", ncol(d), ") in ", basename(f), ": first line = ", probe[1], "\nRun cr_inspect() and adjust CR_COLUMNS in R/81_cr.R")
  for (cn in names(d)) d[[cn]] <- trimws(iconv(d[[cn]], "UTF-8", "UTF-8", sub = "?"))
  d$fecha <- as.Date(substr(d$fecha, 1, 10), format = "%Y-%m-%d")
  if (mean(is.na(d$fecha)) > 0.05) stop("Field 3 of ", basename(f), " does not look like a YYYY-MM-DD date: ", probe[1])
  d
}

cr_inspect <- function() {
  for (y in cr_years()) { f <- cr_file(y); if (is.na(f)) next; d <- cr_read(f)
    message(y, ": ", basename(f), " (", fmt_int(nrow(d)), " rows)\n  delito: ", paste(sort(unique(d$delito)), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
}

cr_cantones <- function() read_reference("cr_cantones.csv")

# province code (1-7) and cantón code (101-706) from the names in the files
cr_match <- function(prov, canton, ref) {
  pk <- cr_key(prov); pcode <- rep_len(ref$province_code[match(pk, cr_key(ref$province_name))], max(length(prov), length(canton)))
  keys <- lapply(seq_len(nrow(ref)), function(i) unique(c(cr_key(ref$canton_name[i]), cr_key(strsplit(ref$aliases[i] %||% "", ";")[[1]]))))
  lookup <- data.frame(key = unlist(keys), code = rep(ref$canton_code, lengths(keys)), prov = rep(ref$province_code, lengths(keys)), stringsAsFactors = FALSE)
  lookup <- lookup[!is.na(lookup$key), ]
  ck <- cr_key(canton)
  i <- match(paste(pcode, ck), paste(lookup$prov, lookup$key))                   # by name inside the province
  j <- match(ck, lookup$key); i[is.na(i)] <- j[is.na(i)]                         # or by name alone (province unknown)
  ccode <- lookup$code[i]; pcode[is.na(pcode)] <- lookup$prov[i][is.na(pcode)]
  list(prov = pcode, canton = ccode)
}

cr_ingest <- function(con) {
  ref <- cr_cantones(); types <- read_reference("cr_offence_types.csv")
  files <- stats::na.omit(vapply(cr_years(), cr_file, character(1)))
  if (length(files) == 0) stop("No OIJ file found in ", COUNTRY_MODULES$CR$dir_raw)
  rows <- list(); dist_names <- list()
  for (f in files) {
    d <- cr_read(f); fy <- as.integer(sub("^.*_(\\d{4})\\.csv$", "\\1", basename(f)))
    d$offence_code <- normalize_text(d$delito); d <- d[!is.na(d$offence_code) & !is.na(d$fecha), ]
    d$year <- as.integer(format(d$fecha, "%Y")); d$month <- as.integer(format(d$fecha, "%m"))
    d <- d[d$year == fy, ]                                                 # one file = one year of events
    # The month in which the file was downloaded cannot be complete: leave it out
    dl <- as.Date(sub("^(\\d{8}).*$", "\\1", basename(f)), format = "%Y%m%d")
    if (!is.na(dl)) d <- d[d$year * 100 + d$month < as.integer(format(dl, "%Y")) * 100 + as.integer(format(dl, "%m")), ]
    mt <- cr_match(d$provincia, d$canton, ref)
    d$prov_id <- ifelse(is.na(mt$prov), NA, paste0("CR-", mt$prov))
    d$canton_id <- ifelse(is.na(mt$canton), NA, paste(d$prov_id, mt$canton, sep = "-"))
    dk <- cr_key(d$distrito)
    d$dist_id <- ifelse(is.na(d$canton_id) | is.na(dk), NA, paste(d$canton_id, gsub(" ", "_", dk), sep = "-"))
    unknown_c <- unique(d$canton[is.na(d$canton_id) & !is.na(cr_key(d$canton))])
    if (length(unknown_c)) message("  ", basename(f), ": cantones not in reference/cr_cantones.csv (counted at provincia/national level only): ", paste(unknown_c, collapse = ", "))
    count <- function(id, level) {
      x <- d[!is.na(id), ]; x$territory_id <- id[!is.na(id)]
      x |> dplyr::count(territory_id, offence_code, year, month, name = "n") |> dplyr::mutate(country = "CR", level = level, file = basename(f))
    }
    rows[[f]] <- dplyr::bind_rows(count(rep("CR", nrow(d)), "country"), count(d$prov_id, "region"), count(d$canton_id, "province"),
                                  if (CR_DISTRICTS) count(d$dist_id, "municipality"))
    if (CR_DISTRICTS) { dn <- unique(d[!is.na(d$dist_id), c("dist_id", "canton_id", "distrito")]); dn$distrito <- gsub("?", "Ñ", dn$distrito, fixed = TRUE); dist_names[[f]] <- dn }   # "?" = corrupted Ñ
    message(sprintf("  %s: %s records, %d cantones, %d distritos, %d-%02d to %d-%02d", basename(f), fmt_int(nrow(d)), length(unique(stats::na.omit(d$canton_id))),
                    length(unique(stats::na.omit(d$dist_id))), min(d$year), min(d$month[d$year == min(d$year)]), max(d$year), max(d$month[d$year == max(d$year)])))
  }
  crime <- dplyr::bind_rows(rows)
  new_codes <- setdiff(unique(crime$offence_code), types$source_code)
  if (length(new_codes)) message("  [!!] Delito values not in reference/cr_offence_types.csv (not shown until mapped): ", paste(new_codes, collapse = ", "))
  # "total" = sum of every category except those mapped to `unmapped` (NOT an official total)
  keep <- !crime$offence_code %in% types$source_code[types$offence_type_id == "unmapped"]
  tot <- crime[keep, ] |> dplyr::group_by(country, territory_id, level, year, month, file) |> dplyr::summarise(n = sum(n), .groups = "drop") |> dplyr::mutate(offence_code = "TOTAL_DELITOS")
  crime <- dplyr::bind_rows(crime, tot[, names(crime)])
  # Several versions of the same year never coexist (newest file per year), so files are not a key
  file_of <- crime |> dplyr::distinct(territory_id, level, offence_code, year, file)
  cum <- monthly_to_cumulative(crime[, c("country", "territory_id", "level", "offence_code", "year", "month", "n")])
  cum <- dplyr::left_join(cum, file_of, by = c("territory_id", "level", "offence_code", "year"))
  if (nrow(cum) == 0) stop("No complete quarter in the OIJ files")
  if (CR_DISTRICTS) { dn <- dplyr::bind_rows(dist_names) |> dplyr::distinct(dist_id, .keep_all = TRUE)
    ensure_territories(con, "CR", dn$dist_id, "municipality", dn$canton_id, dn$distrito) }
  replace_country_rows(con, "stg_country_crime", "CR", cum[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_CR", paste(basename(files), collapse = ";"), NA, paste(range(cum$year), collapse = "-"), nrow(cum))
  invisible(cum)
}

# Territories: provincias and cantones from the reference; boundaries from geoBoundaries
# (ADM1 provincias, ADM2 cantones, ADM3 distritos, matched by name; distritos are assigned
# to their cantón by location). Unmatched polygons are skipped (those units are table-only).
cr_territory <- function(con) {
  ref <- cr_cantones()
  provs <- unique(ref[, c("province_code", "province_name")])
  tbl <- rbind(data.frame(territory_id = paste0("CR-", provs$province_code), level = "region", parent_id = "CR", official_code = provs$province_code, name = provs$province_name),
               data.frame(territory_id = paste("CR", ref$province_code, ref$canton_code, sep = "-"), level = "province", parent_id = paste0("CR-", ref$province_code),
                          official_code = ref$canton_code, name = ref$canton_name))
  gb <- function(adm) tryCatch(geoboundaries("CRI", adm), error = function(e) { message("  geoBoundaries ADM", adm, " not available: ", conditionMessage(e)); NULL })
  layers <- list()
  g1 <- gb(1)
  if (!is.null(g1)) { pc <- provs$province_code[match(cr_key(g1$shapeName), cr_key(provs$province_name))]; g1$territory_id <- ifelse(is.na(pc), NA, paste0("CR-", pc)); g1$name <- provs$province_name[match(pc, provs$province_code)]
    layers$region <- g1[!is.na(g1$territory_id), c("territory_id", "name", attr(g1, "sf_column"))] }
  g2 <- gb(2)
  if (!is.null(g2)) { mt <- cr_match(NA_character_, g2$shapeName, ref); g2$territory_id <- ifelse(is.na(mt$canton), NA, paste("CR", mt$prov, mt$canton, sep = "-"))
    g2$name <- ref$canton_name[match(mt$canton, ref$canton_code)]
    if (any(is.na(g2$territory_id))) message("  cantones in geoBoundaries without match: ", paste(g2$shapeName[is.na(g2$territory_id)], collapse = ", "))
    layers$province <- g2[!is.na(g2$territory_id), c("territory_id", "name", attr(g2, "sf_column"))] }
  g3 <- if (CR_DISTRICTS && !is.null(layers$province)) gb(3) else NULL
  if (!is.null(g3)) {
    g3 <- sf::st_make_valid(sf::st_transform(g3, 4326)); cant <- sf::st_transform(layers$province, 4326)
    inside <- sf::st_within(suppressWarnings(sf::st_point_on_surface(g3)), cant, sparse = TRUE)
    canton_id <- vapply(inside, function(i) if (length(i)) cant$territory_id[i[1]] else NA_character_, character(1))
    dk <- cr_key(g3$shapeName)
    g3$territory_id <- ifelse(!is.na(canton_id) & !is.na(dk), paste(canton_id, gsub(" ", "_", dk), sep = "-"), NA)
    g3$parent_id <- canton_id; g3$name <- g3$shapeName
    g3 <- g3[!is.na(g3$territory_id) & !duplicated(g3$territory_id), ]
    tbl <- rbind(tbl, data.frame(territory_id = g3$territory_id, level = "municipality", parent_id = g3$parent_id, official_code = sub("^.*-", "", g3$territory_id), name = g3$name))
    layers$municipality <- g3[, c("territory_id", "name", attr(g3, "sf_column"))]
    message(sprintf("  %d distritos with boundaries (geoBoundaries ADM3); distritos only in the OIJ files are added without boundaries", nrow(g3)))
  }
  save_country_territory(con, "CR", tbl, scheme = "DTA (INEC); distritos by name")
  if (length(layers)) save_country_gpkg(con, "CR", layers, tolerance_m = 100)
  invisible(tbl)
}

# Population: INEC "Estimaciones y proyecciones de población" by provincia, cantón and distrito.
# No open download URL could be verified from the sandbox: save the INEC Excel file in
# data/raw/country_cr/ with a name containing "inec" (e.g. 20260101_inec_proyecciones.xlsx).
# Three layouts are recognised in each sheet (a header row holds the years as columns):
#   A. named columns PROVINCIA / CANTON / DISTRITO (one unit per row);
#   B. a DTA code column: 1 digit = provincia, 3 digits = cantón, 5 digits = distrito;
#   C. names nested in one column (provincia, then its cantones in code order; distritos ignored).
cr_population <- function(con) {
  files <- newest_first(list.files(COUNTRY_MODULES$CR$dir_raw, "inec.*\\.xlsx?$", full.names = TRUE, ignore.case = TRUE))
  if (length(files) == 0) stop("No INEC population file (data/raw/country_cr/*inec*.xlsx): see modules/cr.md; counts are shown without rates")
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl is needed to read the INEC file")
  ref <- cr_cantones(); provs <- unique(ref[, c("province_code", "province_name")])
  out <- list()
  for (sh in readxl::excel_sheets(files[1])) {
    s <- as.data.frame(suppressMessages(readxl::read_excel(files[1], sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal")), stringsAsFactors = FALSE)
    if (nrow(s) < 3) next
    digits <- function(x) gsub("[^0-9]", "", ifelse(is.na(x), "", x))
    is_year <- function(x) nchar(digits(x)) == 4 & suppressWarnings(as.integer(digits(x))) %in% 1990:2080
    hdr <- which(apply(s, 1, function(r) sum(is_year(r))) >= 2)[1]; if (is.na(hdr)) next
    ycols <- which(is_year(unlist(s[hdr, ]))); years <- as.integer(digits(unlist(s[hdr, ycols]))); other <- setdiff(seq_len(ncol(s)), ycols)
    hk <- cr_key(unlist(s[hdr, other])); body <- s[-(1:hdr), , drop = FALSE]
    values <- function(i) { v <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", gsub(",", "", unlist(body[i, ycols]))))); v }
    add <- function(i, id, level) { v <- values(i); if (any(!is.na(v))) out[[length(out) + 1]] <<- data.frame(country = "CR", territory_id = id, level = level, year = years[!is.na(v)], population = round(v[!is.na(v)])) }
    cp <- other[match(TRUE, grepl("^PROVINCIA", hk))]; cc <- other[match(TRUE, grepl("^CANTON", hk))]; cd <- other[match(TRUE, grepl("^DISTRITO", hk))]
    codes <- if (length(other)) other[which.max(colSums(sapply(other, function(j) nchar(digits(body[[j]])) %in% c(1, 3, 5) & !is.na(body[[j]]))))] else NA
    code_rows <- if (!is.na(codes)) mean(nchar(digits(body[[codes]])) %in% c(1, 3, 5) & !is_year(body[[codes]])) else 0
    if (!is.na(cp) && !is.na(cc)) {                                   # A. named columns
      for (i in seq_len(nrow(body))) {
        mt <- cr_match(body[i, cp], body[i, cc], ref); dk <- if (!is.na(cd)) cr_key(body[i, cd]) else NA
        if (is.na(mt$prov)) next
        if (is.na(mt$canton) || cr_key(body[i, cc]) %in% c(NA, "TOTAL")) add(i, paste0("CR-", mt$prov), "region")
        else if (is.na(dk) || dk == "TOTAL") add(i, paste("CR", mt$prov, mt$canton, sep = "-"), "province")
        else if (CR_DISTRICTS) add(i, paste("CR", mt$prov, mt$canton, gsub(" ", "_", dk), sep = "-"), "municipality")
      }
    } else if (code_rows > 0.5) {                                     # B. DTA codes
      lab <- apply(body[, setdiff(other, codes), drop = FALSE], 1, function(r) { r <- r[!is.na(r) & nzchar(trimws(r))]; if (length(r)) r[1] else NA })
      for (i in seq_len(nrow(body))) {
        cd0 <- digits(body[i, codes]); if (is_year(body[i, codes])) next
        if (nchar(cd0) == 1) add(i, paste0("CR-", cd0), "region")
        else if (nchar(cd0) == 3) add(i, paste("CR", substr(cd0, 1, 1), cd0, sep = "-"), "province")
        else if (nchar(cd0) == 5 && CR_DISTRICTS && !is.na(cr_key(lab[i]))) add(i, paste("CR", substr(cd0, 1, 1), substr(cd0, 1, 3), gsub(" ", "_", cr_key(lab[i])), sep = "-"), "municipality")
      }
    } else {                                                          # C. nested names (provincia + cantones)
      lab <- apply(body[, other, drop = FALSE], 1, function(r) { r <- r[!is.na(r) & nzchar(trimws(r)) & is.na(suppressWarnings(as.numeric(gsub("[ ,.]", "", r))))]; if (length(r)) r[1] else NA })
      prov <- NA; expect <- NA
      for (i in seq_len(nrow(body))) {
        k <- cr_key(lab[i]); if (is.na(k)) next
        last <- if (is.na(prov)) NA else max(as.integer(ref$canton_code[ref$province_code == prov]))
        if (k %in% cr_key(provs$province_name) && (is.na(prov) || expect > last)) { prov <- provs$province_code[match(k, cr_key(provs$province_name))]; expect <- as.integer(prov) * 100 + 1; add(i, paste0("CR-", prov), "region"); next }
        if (is.na(prov)) next
        mt <- cr_match(provs$province_name[match(prov, provs$province_code)], lab[i], ref)
        if (!is.na(mt$canton) && as.integer(mt$canton) == expect) { add(i, paste("CR", prov, mt$canton, sep = "-"), "province"); expect <- expect + 1 }
      }
    }
  }
  if (length(out) == 0) stop("No population rows recognised in ", basename(files[1]), ": check the layout (see R/81_cr.R, cr_population)")
  pop <- dplyr::bind_rows(out) |> dplyr::distinct(territory_id, year, .keep_all = TRUE)
  nat <- pop[pop$level == "region", ] |> dplyr::group_by(country, year) |> dplyr::summarise(population = sum(population), .groups = "drop") |> dplyr::mutate(territory_id = "CR", level = "country")
  pop <- dplyr::bind_rows(pop, nat[, names(pop)])
  replace_country_rows(con, "ref_country_population", "CR", pop)
  by_level <- tapply(pop$territory_id, pop$level, function(x) length(unique(x)))
  message(sprintf("  population: %d territories (%s), %d-%d (%s)", length(unique(pop$territory_id)), paste(names(by_level), by_level, collapse = ", "), min(pop$year), max(pop$year), basename(files[1])))
}

register_country(list(
  code = "CR", iso3 = "CRI", name_en = "Costa Rica", name_local = "Costa Rica", lang = "es",
  levels = if (CR_DISTRICTS) c("region", "province", "municipality") else c("region", "province"),
  level_labels = list(region = c("Provinces", "Provincias", "Province", "Provincia"), province = c("Cantons", "Cantones", "Canton", "Cantón"),
                      municipality = c("Districts", "Distritos", "District", "Distrito")),
  source = list(name = "Estadísticas Policiales (OIJ)", publisher = "Poder Judicial de Costa Rica, Organismo de Investigación Judicial",
                url = "https://datosabiertospj.poder-judicial.go.cr/dataset/estadisticas-policiales",
                licence = "Creative Commons Attribution (CC BY)",
                measure = "Records of complaints (denuncias) received by the OIJ, by month of the event; one complaint with several victims or objects gives several records. Six categories, no official total (the total shown is their sum)."),
  population_source = "INEC Costa Rica, estimaciones y proyecciones de población (file supplied by the user)",
  boundaries_attribution = "geoBoundaries (CC BY 4.0 / CC0; OpenStreetMap contributors for ADM1 and ADM3)",
  measure_labels = list(kind = "complaint_records", title = c("Complaints (OIJ)", "Denuncias (OIJ)"), noun = c("complaint records", "registros de denuncias")),
  not_published = c("Not published", "No publicado"),
  notes_en = "OIJ complaints only (not Fuerza Pública incidents); records, not incidents; distritos identified by name (no official code list), cantones created after 2015 (Río Cuarto, Monteverde, Puerto Jiménez) appear from their creation.",
  method = list(where = "Cantón or distrito where the event occurred, as stated in the complaint to the OIJ", date = "Month of the event (fecha del hecho)", revisions = "The dataset is republished with the whole series; recent months fill in later"),
  default_region = "CR-1",
  download = cr_download, ingest = cr_ingest, territory = cr_territory, population = cr_population
))
