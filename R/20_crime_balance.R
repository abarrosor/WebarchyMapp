# F3 · Crime Balance of the Spanish Ministry of the Interior ---------------------------
# Statistical crime portal: https://estadisticasdecriminalidad.ses.mir.es
#
# What it measures: KNOWN CRIMINAL OFFENCES (crimes and minor offences recorded
# by the National Police, Guardia Civil, regional and local police forces). It is
# a different measure from the Madrid police calls (F1): they are never added up.
#
# What is downloaded: every quarter, the table "Municipalities above 20,000
# inhabitants and islands" (above 30,000 in 2019). It covers the whole country:
# regions, provinces, municipalities and islands, with ~20 offence types. Figures
# are CUMULATIVE since January (Jan-Mar, Jan-Jun, ...), and each file repeats the
# same period of the previous year (sometimes revised).
#
# How: for each quarter its index page is read, the municipalities table is
# located (its number changes by year) and the CSV is requested. It is saved as
# data/raw/country_es/crime_balance/crime_balance_municipalities_YYYYQn.csv and never
# downloaded again. A CSV can also be placed by hand in that folder (downloaded
# from the portal with "Download > CSV separated by ;").
#
# Known formats:
#   - Since ~2024: municipalities with INE code in front ("28079 Madrid").
#   - Until ~2023: without code ("-Municipio de Madrid"). The code is looked up
#     by name within its province using the files that do carry it.
#   - Offence types: two schemes (until 2021 and since 2022). Equivalences in
#     reference/es_offence_types.csv (source_code = key of the published label).
#
# Spain is a country module (R/71_es.R): these readers fill the Spanish tables and the
# module copies the result into the shared stg_country_crime.

CB_QUARTER_END <- c(MARZO = 1L, JUNIO = 2L, SEPTIEMBRE = 3L, DICIEMBRE = 4L)

# --- Helpers ---------------------------------------------------------------------

# Key to compare names: upper case, no accents or punctuation, sorted words
# ("CORUÑA (A)" and "Coruña, A" give the same key)
cb_key <- function(x) {
  x <- normalize_text(x)
  x <- gsub("[^A-Z0-9Ñ]+", " ", x)
  vapply(strsplit(trimws(x), " +"), function(p) paste(sort(unique(p[nzchar(p)])), collapse = " "), "")
}

# Key of an offence type: without numbering ("5.1.-", "III.") nor "e indemnidad"
cb_type_key <- function(x) {
  x <- normalize_text(x)
  x <- sub("^[IVX]+\\.\\s+", "", x)
  x <- sub("^[0-9]+(\\.[0-9]+)*\\.?\\s*-?\\s*", "", x)
  x <- gsub(" E INDEMNIDAD", "", x, fixed = TRUE)
  x <- gsub("[^A-Z0-9Ñ]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# "enero-junio 2026" -> year 2026, quarter 2 ; "Variación %..." -> NA
# (the 2024Q4 file writes "enero--diciembre 2023", with two hyphens)
cb_period <- function(x) {
  m <- regmatches(x, regexec("^\\s*[Ee]nero\\s*-+\\s*(\\S+)\\s+([0-9]{4})\\s*$", x))
  year <- vapply(m, function(v) if (length(v) == 3) as.integer(v[3]) else NA_integer_, 1L)
  mon  <- vapply(m, function(v) if (length(v) == 3) normalize_text(v[2]) else NA_character_, "")
  data.frame(year = year, quarter = unname(CB_QUARTER_END[mon]))
}

cb_provinces <- function() es_provinces()

# --- Download ---------------------------------------------------------------------

# Locate the municipalities table in the index page of a quarter.
# Past years live under DatosBalanceAnt, the current year under DatosBalanceAct.
cb_find_table <- function(year, quarter, portal = es_source("crime_balance_portal")) {
  html <- NULL
  for (folder in c("DatosBalanceAnt", "DatosBalanceAct")) {
    url <- sprintf("%s/sec/dynPx/inebase/index.htm?type=pcaxis&path=/%s/%d%d/&file=pcaxis", portal, folder, year, quarter)
    html <- get_text(url)
    if (!is.null(html) && grepl("\\.px", html)) break
    html <- NULL
  }
  if (is.null(html)) return(NULL)
  html <- gsub("&amp;", "&", html, fixed = TRUE)
  links <- regmatches(html, gregexpr("<a[^>]*href=[\"'][^\"']*\\.px[^\"']*[\"'][^>]*>.*?</a>", html, perl = TRUE))[[1]]
  if (length(links) > 0) {
    href  <- sub("^.*?href=[\"']([^\"']*)[\"'].*$", "\\1", links, perl = TRUE)
    title <- trimws(gsub("\\s+", " ", gsub("<[^>]+>", " ", links)))
  } else {                       # in case the HTML changes: any reference to a .px
    href  <- regmatches(html, gregexpr("path=/[^&\"']+&file=[^&\"']+\\.px", html))[[1]]
    title <- rep(NA_character_, length(href))
  }
  ok <- grepl("path=", href) & grepl("file=", href)
  href <- href[ok]; title <- title[ok]
  if (length(href) == 0) return(NULL)
  i <- grep("municipio", title, ignore.case = TRUE)
  i <- if (length(i) > 0) i[1] else length(href)     # the municipalities table is the last one
  path <- sub("^.*path=([^&]+).*$", "\\1", href[i])
  file <- sub("^.*file=([^&]+\\.px).*$", "\\1", href[i])
  if (!endsWith(path, "/")) path <- paste0(path, "/")
  list(title = title[i], file = file,
       csv_url = paste0(portal, "/sec/jaxiPx/files/_px/es/csv_bdsc", path, sub("\\.px$", ".csv_bdsc", file), "?nocab=1"))
}

# Check that the download is the expected CSV (and not an error page)
cb_validate_download <- function(path, year, quarter) {
  lines <- cb_lines(path, n_bytes = 60000)
  if (length(lines) < 10 || !grepl("Geograf", lines[1]) || !grepl("Tipolog", lines[1])) return(FALSE)
  per <- cb_period(vapply(strsplit(lines[-1], ";", fixed = TRUE), function(v) v[3] %||% NA_character_, ""))
  any(per$year == year & per$quarter == quarter, na.rm = TRUE)
}

crime_balance_download <- function(from = CONFIG$sources$crime_balance$from, today = Sys.Date()) {
  dir <- CONFIG$sources$crime_balance$dir_raw
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  this_year <- as.integer(format(today, "%Y"))
  new <- character(0); unpublished <- character(0)
  for (year in from:this_year) for (quarter in 1:4) {
    quarter_end <- as.Date(sprintf("%d-%02d-01", year + (quarter == 4), if (quarter == 4) 1 else quarter * 3 + 1)) - 1
    if (quarter_end + 30 > today) next                 # cannot be published yet
    name <- sprintf("crime_balance_municipalities_%dQ%d.csv", year, quarter)
    dest <- file.path(dir, name)
    old_name <- file.path(dir, sprintf("balance_municipios_%dT%d.csv", year, quarter))   # files from the Spanish version
    if (file.exists(dest) || file.exists(old_name)) next
    table <- cb_find_table(year, quarter)
    if (is.null(table)) { unpublished <- c(unpublished, sprintf("%dQ%d", year, quarter)); next }
    tmp <- file.path(tempdir(), name)
    if (file.exists(tmp)) file.remove(tmp)
    ok <- tryCatch({ download_safely(table$csv_url, tmp, timeout = 300); TRUE },
                   error = function(e) { message("x ", name, ": ", conditionMessage(e)); FALSE })
    if (ok && cb_validate_download(tmp, year, quarter)) {
      if (!file.copy(tmp, dest, overwrite = FALSE)) stop("Could not save ", dest)
      message("+ ", name, "  (", table$file, ")")
      new <- c(new, dest)
    } else if (ok) {
      message("x ", name, ": the portal did not return the expected CSV (", table$file, "). Will try again another day.")
    }
    if (file.exists(tmp)) file.remove(tmp)
    Sys.sleep(0.5)
  }
  if (length(unpublished) > 0) message("- Not published or no answer: ", paste(unpublished, collapse = ", "))
  message(length(new), " new Crime Balance file(s).")
  invisible(new)
}

# --- Reading -----------------------------------------------------------------------

# Text lines of the file, without BOM and in UTF-8 (also if it came in latin1)
cb_lines <- function(path, n_bytes = file.size(path)) {
  b <- readBin(path, "raw", n = n_bytes)
  if (length(b) >= 3 && identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) b <- b[-(1:3)]
  txt <- rawToChar(b[b != as.raw(0)])
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) txt <- iconv(txt, "latin1", "UTF-8")
  lines <- strsplit(txt, "\r?\n")[[1]]
  lines[nzchar(trimws(lines))]
}

cb_read <- function(path) {
  lines <- cb_lines(path)
  if (length(lines) == 0 || !grepl("^\"?Geograf", lines[1]))
    stop(basename(path), ": does not look like a Crime Balance CSV (starts with: ", substr(lines[1] %||% "", 1, 60), ")")
  n_col <- length(strsplit(lines[1], ";", fixed = TRUE)[[1]])
  if (n_col != 4) stop(basename(path), ": expected 4 columns, found ", n_col)
  df <- readr::read_delim(I(paste(lines[-1], collapse = "\n")), delim = ";",
                          col_names = c("geography", "offence_type", "period", "value"),
                          col_types = readr::cols(.default = "c"), trim_ws = TRUE, progress = FALSE)
  attr(df, "header") <- lines[1]
  df
}

# --- Territories --------------------------------------------------------------------

# Classify the geographies of a file (in their order) and assign INE codes.
# dict: list(mun = c("<prov> <name key>" = "28079"), prov = c("<key>" = "28"), region = c("<key>" = "13"))
cb_geographies <- function(geos, dict, ref) {
  g <- unique(geos)
  n <- normalize_text(g)
  level <- dplyr::case_when(
    grepl("^[0-9]{5}\\s", g)            ~ "municipality",
    grepl("^-?\\s*MUNICIPI?O DE ", n)   ~ "municipality",   # also the typo "Municipo" (2023Q4)
    grepl("^-?\\s*ISLA\\b", n)          ~ "island",
    grepl("^PROVINCIA DE ", n)          ~ "province",
    grepl("NACIONAL|^ESPANA$", n)       ~ "country",
    grepl("EXTRANJER", n)               ~ "abroad",
    g == toupper(g)                     ~ "region",
    TRUE                                ~ "other")
  name <- g
  name <- sub("^[0-9]{5}\\s+", "", name)
  name <- sub("^-?\\s*[Mm]unicipi?o de\\s+", "", name)
  name <- sub("^-?\\s*[Ii]sla(\\s+de)?\\s+", "", name)
  name <- sub("^[Pp]rovincia de\\s+", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  key <- cb_key(name)

  prov_to_region <- stats::setNames(ref$region_code, ref$province_code)
  # Single-province regions (Madrid, Asturias, Balearic Islands, Murcia...) have
  # no "Provincia de" row: their municipalities follow the region row
  n_prov <- table(ref$region_code)
  single <- stats::setNames(ref$province_code, ref$region_code)[ref$region_code %in% names(n_prov)[n_prov == 1]]

  mun  <- ifelse(grepl("^[0-9]{5}\\s", g), substr(g, 1, 5), NA_character_)
  prov <- ifelse(!is.na(mun), substr(mun, 1, 2), NA_character_)
  region <- rep(NA_character_, length(g))
  cut <- level %in% c("province", "region", "country")

  # 1. Provinces: from the first coded municipality that follows; otherwise by name
  for (i in which(level == "province")) {
    j <- i + 1
    while (j <= length(g) && !cut[j]) { if (!is.na(mun[j])) { prov[i] <- substr(mun[j], 1, 2); break }; j <- j + 1 }
    if (is.na(prov[i])) prov[i] <- unname(dict$prov[key[i]])
  }
  # 2. Regions: from the first known province that follows; otherwise by name
  for (i in which(level == "region")) {
    j <- i + 1
    while (j <= length(g) && !(level[j] %in% c("region", "country"))) {
      if (!is.na(prov[j])) { region[i] <- unname(prov_to_region[prov[j]]); break }
      j <- j + 1
    }
    if (is.na(region[i])) region[i] <- unname(dict$region[key[i]])
  }
  # 3. Municipalities without code and islands: inherit the province, looked up by name
  current <- NA_character_
  for (i in seq_along(g)) {
    if (level[i] == "country") current <- NA_character_
    if (level[i] == "region") current <- if (is.na(region[i])) NA_character_ else unname(single[region[i]])
    if (level[i] == "province" || (level[i] == "municipality" && !is.na(mun[i]))) current <- prov[i]
    if (level[i] %in% c("municipality", "island") && is.na(prov[i])) prov[i] <- current
    if (level[i] == "municipality" && is.na(mun[i]) && !is.na(prov[i])) mun[i] <- unname(dict$mun[paste(prov[i], key[i])])
  }
  row <- level != "region" & !is.na(prov)
  region[row] <- unname(prov_to_region[prov[row]])

  id <- dplyr::case_when(
    level == "country"                                  ~ "ES",
    level == "region" & !is.na(region)                  ~ paste0("ES-", region),
    level == "province" & !is.na(region)                ~ paste0("ES-", region, "-", prov),
    level == "municipality" & !is.na(mun) & !is.na(region) ~ paste0("ES-", region, "-", prov, "-", mun),
    level == "island" & !is.na(region)                  ~ paste0("ES-", region, "-", prov, "-I-", gsub(" ", "", key)),
    TRUE ~ NA_character_)
  code <- dplyr::case_when(level == "municipality" ~ mun, level == "province" ~ prov, level == "region" ~ region, TRUE ~ NA_character_)
  data.frame(geography = g, level = level, name = name, key = key, prov = prov, ine_code = code,
             coded = grepl("^[0-9]{5}\\s", g), territory_id = id, stringsAsFactors = FALSE)
}

# Initial dictionary: INE province and region names, plus municipalities spelt
# differently in old files (reference/es_municipality_aliases.csv)
cb_dictionary <- function(ref) {
  al <- ref[!is.na(ref$region_alias) & nzchar(ref$region_alias), ]
  rg <- rbind(data.frame(k = cb_key(ref$region_name), v = ref$region_code),
              data.frame(k = cb_key(al$region_alias), v = al$region_code))
  rg <- rg[nzchar(rg$k) & !duplicated(rg$k), ]
  alias_path <- file.path(CONFIG$dir_ref, "es_municipality_aliases.csv")
  eq <- if (file.exists(alias_path)) read_reference("es_municipality_aliases.csv") else
    data.frame(source_name = character(), ine_code = character())
  eq <- eq[!is.na(eq$ine_code) & nchar(eq$ine_code) == 5, ]
  list(mun    = stats::setNames(eq$ine_code, paste(substr(eq$ine_code, 1, 2), cb_key(eq$source_name))),
       prov   = stats::setNames(ref$province_code, cb_key(ref$province_name)),
       region = stats::setNames(rg$v, rg$k))
}

# Add to the dictionary what the coded files teach
cb_dictionary_learn <- function(dict, geo) {
  m <- geo[geo$level == "municipality" & geo$coded, ]
  parts <- strsplit(m$name, "/", fixed = TRUE)          # bilingual names ("Almazora/Almassora"): each part too
  m <- rbind(m, do.call(rbind, lapply(which(lengths(parts) > 1), function(i) {
    x <- m[rep(i, length(parts[[i]])), ]; x$key <- cb_key(parts[[i]]); x })))
  m <- m[!duplicated(paste(m$prov, m$key)), ]
  p <- geo[geo$level == "province" & !is.na(geo$prov), ]
  p <- p[!duplicated(p$key), ]
  dict$mun  <- c(dict$mun,  stats::setNames(m$ine_code, paste(m$prov, m$key)))
  dict$prov <- c(dict$prov, stats::setNames(p$prov, p$key))
  dict$mun  <- dict$mun[!duplicated(names(dict$mun))]
  dict$prov <- dict$prov[!duplicated(names(dict$prov))]
  dict
}

# Register in dim_territory the regions, provinces, municipalities and islands
# that are missing (existing rows, e.g. Madrid and its neighbourhoods, are untouched)
cb_register_territories <- function(con, geo, ref) {
  geo <- geo[!is.na(geo$territory_id) & geo$level %in% c("country", "region", "province", "municipality", "island"), ]
  geo <- geo[order(-geo$pub), ]                        # most recent name wins
  geo <- geo[!duplicated(geo$territory_id), ]
  # Single-province regions have no province row: create it so municipalities have a parent
  children <- geo[geo$level %in% c("municipality", "island"), ]
  prov_ids <- setdiff(unique(sub("-(I-)?[^-]+$", "", children$territory_id)), geo$territory_id)
  if (length(prov_ids) > 0) {
    geo <- dplyr::bind_rows(geo, data.frame(level = "province", ine_code = sub("^.*-", "", prov_ids), territory_id = prov_ids,
                                            file = children$file[1], pub = max(children$pub)))
  }
  existing <- DBI::dbGetQuery(con, "SELECT territory_id FROM dim_territory")$territory_id
  geo <- geo[!geo$territory_id %in% existing, ]
  if (nrow(geo) == 0) return(invisible(0))
  prov_name <- stats::setNames(ref$province_name, ref$province_code)
  region_name <- stats::setNames(ref$region_name, ref$region_code)
  name <- dplyr::case_when(geo$level == "country" ~ "Spain",
                           geo$level == "region" ~ unname(region_name[geo$ine_code]),
                           geo$level == "province" ~ unname(prov_name[geo$ine_code]),
                           TRUE ~ geo$name)
  name[is.na(name)] <- geo$name[is.na(name)]
  parent <- ifelse(geo$level == "country", NA_character_, ifelse(geo$level == "region", "ES", sub("-[^-]+$", "", geo$territory_id)))
  parent[geo$level == "island"] <- sub("-I-[^-]+$", "", geo$territory_id[geo$level == "island"])
  DBI::dbAppendTable(con, "dim_territory", data.frame(
    territory_id = geo$territory_id, level = geo$level, parent_id = parent, country = "ES",
    official_code = ifelse(geo$level == "country", "ES", geo$ine_code),
    code_scheme = dplyr::case_when(geo$level == "country" ~ "ISO 3166-1", geo$level == "island" ~ "Ministry of the Interior", TRUE ~ "INE"),
    name = name, name_norm = normalize_text(name), area_m2 = NA_real_,
    valid_from = as.Date(NA), valid_to = as.Date(NA), file = geo$file, loaded_at = Sys.time()))
  invisible(nrow(geo))
}

# --- Database -----------------------------------------------------------------------

crime_balance_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_crime_balance (
      geography VARCHAR, level VARCHAR, territory_id VARCHAR, name VARCHAR,
      offence_type_source VARCHAR, offence_type_key VARCHAR, year INTEGER, quarter INTEGER, value DOUBLE,
      pub_year INTEGER, pub_quarter INTEGER, file VARCHAR)")
  invisible(TRUE)
}

# Load ALL files of the folder if any is new or changed (they are few and small;
# this way uncoded municipalities are always resolved with everything learnt)
crime_balance_ingest <- function(con, force = FALSE) {
  src <- CONFIG$sources$crime_balance
  files <- sort(list.files(src$dir_raw, pattern = "\\.(csv|csv_bdsc)$", full.names = TRUE))
  if (length(files) == 0) { message("No files in ", src$dir_raw, " (the Crime Balance has not been downloaded)."); return(invisible(NULL)) }
  hashes <- vapply(files, sha256_file, "")
  done <- logged_hashes(con, "crime_balance")
  has_rows <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM stg_crime_balance")$n > 0
  # If this code or the territory references change, everything is reloaded
  pieces <- c("R/20_crime_balance.R", file.path(CONFIG$dir_ref, c("es_provinces.csv", "es_municipality_aliases.csv")))
  pieces <- pieces[file.exists(pieces)]
  signature <- digest::digest(paste(vapply(pieces, sha256_file, ""), collapse = ""), algo = "sha256")
  signature_ok <- signature %in% logged_hashes(con, "crime_balance_process")
  if (!force && has_rows && signature_ok && all(hashes %in% done)) {
    message("= ", length(files), " Crime Balance file(s) already loaded, no changes.")
    return(invisible(NULL))
  }
  ref <- cb_provinces()
  notes <- list()
  note <- function(file, check, n, detail = "")
    notes[[length(notes) + 1]] <<- data.frame(file = file, check = check, rows_affected = n, detail = detail)

  # 1. Read and periods
  data <- list(); headers <- list()
  for (i in seq_along(files)) {
    name <- basename(files[i])
    df <- tryCatch(cb_read(files[i]), error = function(e) { note(name, "unreadable file", 1, conditionMessage(e)); NULL })
    if (is.null(df)) next
    headers[[name]] <- attr(df, "header")
    per <- cb_period(df$period)
    variation <- grepl("^\\s*Variaci", df$period, ignore.case = TRUE)
    odd <- unique(df$period[is.na(per$year) & !variation])
    if (length(odd) > 0) note(name, "unrecognised period", sum(df$period %in% odd), paste(odd, collapse = " | "))
    df <- cbind(df, per)[!is.na(per$year), ]
    if (nrow(df) == 0) { note(name, "no data", 1); next }
    pub <- max(df$year * 10L + df$quarter)
    df$pub_year <- pub %/% 10L; df$pub_quarter <- pub %% 10L
    df$file <- name; df$hash <- hashes[i]
    data[[name]] <- df
  }
  if (length(data) == 0) stop("No Crime Balance file could be read (see the quality report).")

  # 2. Territories in two passes: first learn from the files with INE codes
  dict <- cb_dictionary(ref)
  for (gg in lapply(data, function(d) cb_geographies(d$geography, dict, ref))) dict <- cb_dictionary_learn(dict, gg)
  geo <- lapply(names(data), function(nm) {
    gg <- cb_geographies(data[[nm]]$geography, dict, ref)
    gg$file <- nm; gg$pub <- data[[nm]]$pub_year[1] * 10L + data[[nm]]$pub_quarter[1]
    gg
  })
  names(geo) <- names(data)

  # 3. Staging table
  stg <- dplyr::bind_rows(lapply(names(data), function(nm) {
    d <- data[[nm]]; gg <- geo[[nm]]; k <- match(d$geography, gg$geography)
    data.frame(geography = d$geography, level = gg$level[k], territory_id = gg$territory_id[k], name = gg$name[k],
               offence_type_source = d$offence_type, offence_type_key = cb_type_key(d$offence_type),
               year = d$year, quarter = d$quarter, value = parse_es_number(d$value),
               pub_year = d$pub_year, pub_quarter = d$pub_quarter, file = nm)
  }))

  # 4. Quality checks
  keys <- read_reference("es_offence_types.csv")$source_code   # the shared type file of the Spain module (source_code = cb_type_key(label))
  for (nm in names(data)) {
    s <- stg[stg$file == nm, ]
    unmapped <- unique(s$offence_type_source[!s$offence_type_key %in% keys])
    note(nm, "offence type without equivalence", sum(!s$offence_type_key %in% keys), paste(unmapped, collapse = " | "))
    unknown <- unique(s$geography[is.na(s$territory_id) & s$level != "abroad"])
    note(nm, "unidentified territory", length(unknown), paste(utils::head(unknown, 20), collapse = " | "))
    note(nm, "non-numeric value", sum(is.na(s$value)))
    note(nm, "municipalities", length(unique(s$geography[s$level == "municipality"])),
         if (any(geo[[nm]]$coded)) "INE code in source" else "no code in source: matched by name")
  }

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM stg_crime_balance")
    DBI::dbAppendTable(con, "stg_crime_balance", stg)
    cb_register_territories(con, dplyr::bind_rows(geo), ref)
    for (nm in names(data)) {
      h <- data[[nm]]$hash[1]
      if (h %in% done) next
      s <- stg[stg$file == nm, ]
      log_ingest(con, "crime_balance", nm, h, paste(sort(unique(sprintf("%dQ%d", s$year, s$quarter))), collapse = ","),
                 nrow(s), header = headers[[nm]] %||% NA_character_, action = "added")
      message("+ ", nm, ": ", fmt_int(nrow(s)), " rows, ", length(unique(s$geography[s$level == "municipality"])), " municipalities")
    }
  })
  if (!signature_ok) log_ingest(con, "crime_balance_process", paste(basename(pieces), collapse = ","), signature, rows = nrow(stg), action = "reload")
  message(sprintf("Crime Balance loaded: %s files, %s rows.", length(data), fmt_int(nrow(stg))))

  report <- dplyr::bind_rows(notes)
  path <- write_quality_report("crime_balance", report)
  problems <- report[report$rows_affected > 0 & report$check != "municipalities", ]
  message("Quality report: ", path)
  if (nrow(problems) > 0) print(problems, row.names = FALSE)
  invisible(report)
}
