# Other countries: shared framework ----------------------------------------------------
# Every country is a module (R/7x_<iso2>_*.R) that registers itself with
# `register_country()` and fills three shared tables with its own data:
#
#   stg_country_crime        country, source, territory_id, level, offence_code, year, quarter, cumulative, file
#                            `cumulative` is the figure from 1 January to the end of `quarter`
#                            (yearly sources: quarter = 4; monthly sources: complete quarters only);
#                            `source` is 'main' for the country's source and the id of a second source
#                            (a regional police, say) registered with register_source(): its rows have
#                            their own offence types and levels and are never added to the main ones
#   ref_country_population   country, territory_id, level, year, population (a territory's, whatever the source)
#   dim_territory            rows of the country (country, region, province, municipality)
#   data/territory_<cc>.gpkg layers region / province / municipality (WGS84) with the columns
#                            territory_id, name, region_id, province_id
#
# `level_labels`: for each level c(English plural, local plural[, English singular, local singular]).
# Offence codes are mapped to the project's offence types in
# reference/<cc>_offence_types.csv (source_code, offence_type_id, name_local, name_en,
# included_in, sensitive). The marts built here (mart_country_*) are what the app reads.
# Spain is one more module (R/71_es.R): the app treats every country alike.
#
# Optional fields of a module: population_source, boundaries_attribution, measure_labels (kind, title,
# noun), source$name_local, source$publisher_local, population_source_local, not_published (English, local) or not_published_key / not_published_legend_key (translation
# keys when the texts exist in every language), notes_en, default_region, zoom_widths (c(region, province):
# view width in degrees under which the map changes level; else proportional to the country).
# method = list(where, date, revisions): short English sentences on where an event is counted (where it
# happened / where it was reported), which date (occurrence / report / registration) and how the source
# revises its figures; shown in the About page.
# rules: a short mark ("2", "3"...) raised when the way the module reads its files changes: a country whose mark
# differs from the one of its last load is loaded again at the next full run (country_rules_*, below).
# freshness_months: months after the end of a period within which the source is expected to publish it, when the
# defaults (8 for figures by quarter, 20 for yearly figures) do not fit (freshness(), R/05_journal.R).
# A second source of a country (register_source(cc, list(id, name, ..., levels, level_labels, covers,
# download, ingest, territory, population))) is shown in the app as a switch of source when the reader
# is inside the territory it covers (`covers`: a region id, or the country code).
#
# Rules (the same as for Spain): raw files are never modified or deleted; sensitive types
# only count inside totals; figures of different countries are never added or compared;
# the threshold is applied in the app after summing types and periods.

COUNTRY_MODULES <- list()

# Identifier scheme: <ISO2>[-<region code>[-<province code>[-<municipality code>]]]
# Official codes as published (hyphens inside a code become underscores).
territory_id <- function(cc, ...) {
  parts <- lapply(list(...), function(x) { x <- gsub("-", "_", as.character(x)); x[is.na(x)] <- ""; x })
  out <- rep(cc, max(1, lengths(parts)))
  for (p in parts) out <- ifelse(nzchar(p), paste(out, p, sep = "-"), out)
  out
}

register_country <- function(m) {
  needed <- c("code", "iso3", "name_en", "name_local", "lang", "levels", "level_labels", "source", "download", "ingest", "territory", "population")
  missing <- setdiff(needed, names(m))
  if (length(missing) > 0) stop("Country module ", m$code %||% "?", " lacks: ", toString(missing))
  m$dir_raw <- m$dir_raw %||% file.path(CONFIG$dir_raw, paste0("country_", tolower(m$code)))
  m$gpkg <- m$gpkg %||% file.path("data", paste0("territory_", tolower(m$code), ".gpkg"))
  COUNTRY_MODULES[[m$code]] <<- m
  invisible(m)
}

# A second source of a registered country (its module file is loaded after the country's)
register_source <- function(cc, s) {
  if (is.null(COUNTRY_MODULES[[cc]])) stop("register_source: country ", cc, " is not registered")
  needed <- c("id", "name", "levels", "level_labels", "covers", "ingest", "territory", "population")
  missing <- setdiff(needed, names(s)); if (length(missing)) stop("Source ", s$id %||% "?", " of ", cc, " lacks: ", toString(missing))
  if (s$id == "main") stop("'main' is the country's own source")
  COUNTRY_MODULES[[cc]]$sources[[s$id]] <<- s
  invisible(s)
}

country_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_country_crime (
      country VARCHAR, territory_id VARCHAR, level VARCHAR, offence_code VARCHAR,
      year INTEGER, quarter INTEGER, cumulative DOUBLE, file VARCHAR, source VARCHAR DEFAULT 'main')")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_country_population (
      country VARCHAR, territory_id VARCHAR, level VARCHAR, year INTEGER, population DOUBLE)")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_country_offence_types (
      country VARCHAR, source_code VARCHAR, offence_type_id VARCHAR, name_local VARCHAR, name_en VARCHAR,
      included_in VARCHAR, sensitive BOOLEAN, source VARCHAR DEFAULT 'main')")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_country_territory_info (
      country VARCHAR, territory_id VARCHAR, info_en VARCHAR, info_local VARCHAR)")
  # Databases created before the `source` column existed (P-029)
  for (tb in c("stg_country_crime", "ref_country_offence_types")) {
    DBI::dbExecute(con, paste0("ALTER TABLE ", tb, " ADD COLUMN IF NOT EXISTS source VARCHAR DEFAULT 'main'"))
    DBI::dbExecute(con, paste0("UPDATE ", tb, " SET source = 'main' WHERE source IS NULL"))
  }
  invisible(TRUE)
}

# --- Helpers shared by the modules ---------------------------------------------------------

# Replace the rows of one country (and, in the tables that have it, one source) in a shared table
replace_country_rows <- function(con, table, cc, df, source = "main") {
  has_source <- "source" %in% DBI::dbListFields(con, table)
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ", table, " WHERE country = ", DBI::dbQuoteString(con, cc),
                               if (has_source) paste0(" AND source = ", DBI::dbQuoteString(con, source)) else ""))
    if (nrow(df) > 0) { df <- as.data.frame(df); if (has_source && !"source" %in% names(df)) df$source <- source; DBI::dbAppendTable(con, table, df) }
  })
  invisible(nrow(df))
}

# Load reference/<cc>_offence_types.csv (or the file of a second source) into ref_country_offence_types
load_country_types <- function(con, cc, file = paste0(tolower(cc), "_offence_types.csv"), source = "main") {
  f <- file
  if (!file.exists(file.path(CONFIG$dir_ref, f))) stop("Missing ", f, " in ", CONFIG$dir_ref)
  t <- read_reference(f, bool_cols = "sensitive")
  needed <- c("source_code", "offence_type_id", "name_local", "name_en", "included_in")
  if (!all(needed %in% names(t))) stop(f, ": columns needed: ", toString(needed))
  t$sensitive <- t$sensitive %||% FALSE
  t$included_in[is.na(t$included_in) | t$included_in == ""] <- NA
  replace_country_rows(con, "ref_country_offence_types", cc,
                       data.frame(country = cc, t[, c("source_code", "offence_type_id", "name_local", "name_en", "included_in", "sensitive")], source = source), source)
  invisible(t)
}

# A line of context about a territory (the municipalities inside a police area, say), shown in the app
# under the territory selector. info_local may be NA.
save_territory_info <- function(con, cc, df) {
  ids <- paste(DBI::dbQuoteString(con, df$territory_id), collapse = ", ")
  DBI::dbWithTransaction(con, {
    if (nrow(df)) DBI::dbExecute(con, paste0("DELETE FROM ref_country_territory_info WHERE country = ", DBI::dbQuoteString(con, cc), " AND territory_id IN (", ids, ")"))
    if (nrow(df)) DBI::dbAppendTable(con, "ref_country_territory_info", data.frame(country = cc, territory_id = df$territory_id, info_en = df$info_en, info_local = df$info_local %||% NA_character_))
  })
  invisible(nrow(df))
}

# Yearly figures -> the shared shape (quarter 4 = the whole year)
yearly_to_cumulative <- function(df) {
  df$quarter <- 4L; df$cumulative <- as.numeric(df$n); df$n <- NULL
  df
}

# Monthly figures (columns year, month, n) -> cumulative at the end of each COMPLETE quarter.
# The months of an unfinished quarter are left out (the app shows complete quarters only).
monthly_to_cumulative <- function(df, keys = c("country", "territory_id", "level", "offence_code")) {
  df$quarter <- ceiling(df$month / 3)
  complete <- df |> dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "year", "quarter")))) |>
    dplyr::summarise(months = dplyr::n_distinct(month[!is.na(n)]), n = sum(n, na.rm = TRUE), .groups = "drop")
  # A quarter is complete when its three months are present for the country as a whole
  present <- complete |> dplyr::group_by(year, quarter) |> dplyr::summarise(m = max(months), .groups = "drop")
  present <- present[present$m == 3, c("year", "quarter")]
  complete <- dplyr::inner_join(complete, present, by = c("year", "quarter"))
  # A key with no row in some quarter of a year it appears in (nothing recorded those months) gets a zero
  # there, so that the cumulative carries on to the end of the year (P-029: before, the year total of a
  # sparse type stopped at its last quarter with rows)
  grid <- dplyr::inner_join(dplyr::distinct(complete[, c(keys, "year")]), present, by = "year", relationship = "many-to-many")
  complete <- dplyr::left_join(grid, complete, by = c(keys, "year", "quarter")); complete$n[is.na(complete$n)] <- 0
  complete |> dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "year")))) |> dplyr::arrange(quarter, .by_group = TRUE) |>
    dplyr::mutate(cumulative = cumsum(n)) |> dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(keys, "year", "quarter", "cumulative"))) |> as.data.frame()
}

# Rows of dim_territory for a country from a data frame (territory_id, level, parent_id, official_code, name)
save_country_territory <- function(con, cc, tbl, scheme = "official") {
  tbl <- as.data.frame(tbl)
  tbl$country <- cc; tbl$code_scheme <- scheme; tbl$name_norm <- normalize_text(tbl$name)
  for (col in c("area_m2", "valid_from", "valid_to", "file")) if (!col %in% names(tbl)) tbl[[col]] <- NA
  tbl$loaded_at <- Sys.time()
  if (!cc %in% tbl$territory_id) tbl <- rbind(tbl[0, ], data.frame(territory_id = cc, level = "country", parent_id = NA, official_code = cc, name = COUNTRY_MODULES[[cc]]$name_en,
                                                                     country = cc, code_scheme = "ISO 3166-1", name_norm = normalize_text(COUNTRY_MODULES[[cc]]$name_en),
                                                                     area_m2 = NA, valid_from = NA, valid_to = NA, file = NA, loaded_at = Sys.time()), tbl)
  tbl <- tbl[!duplicated(tbl$territory_id), ]
  # The rows of the city modules of this country belong to the city framework and are not touched: their districts and
  # neighbourhoods, and a city node of their own (PE-LIMACALLAO: a territory this country's table does not hold).
  # A city that is a real municipality of the country (Paris, Amsterdam) is in the table: it is deleted and written
  # again with the rest, as the country sees it. Only the nodes the table does NOT hold are protected from the DELETE:
  # protecting the others while writing them is a duplicate key, and every reload of France and of the Netherlands
  # failed on it (10-15 November 2026).
  city_ids <- vapply(Filter(function(m) identical(m$country, cc), CITY_MODULES), function(m) m$id, "")
  tbl <- tbl[!tbl$territory_id %in% city_ids | tbl$level == "municipality", ]
  own_nodes <- setdiff(city_ids, tbl$territory_id)
  keep <- if (length(own_nodes)) paste0(" AND territory_id NOT IN (", paste(DBI::dbQuoteString(con, own_nodes), collapse = ", "), ")") else ""
  # What stays in dim_territory after the DELETE cannot be written again. Said here in plain words, with the
  # territories: the database would only answer "Current transaction is aborted", which hides the duplicate key.
  stay <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE country IS DISTINCT FROM ", DBI::dbQuoteString(con, cc),
                                      " OR level IS NULL OR level IN ('district', 'neighbourhood')",
                                      if (length(own_nodes)) paste0(" OR territory_id IN (", paste(DBI::dbQuoteString(con, own_nodes), collapse = ", "), ")") else ""))$territory_id
  twice <- intersect(tbl$territory_id, stay)
  if (length(twice)) stop(sprintf("%d territories of %s are already in dim_territory under another country or as part of a city and cannot be written twice: %s",
                                  length(twice), cc, toString(utils::head(twice, 5))))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM dim_territory WHERE country = ", DBI::dbQuoteString(con, cc), " AND level NOT IN ('district', 'neighbourhood')", keep))
    DBI::dbAppendTable(con, "dim_territory", tbl[, c("territory_id", "level", "parent_id", "country", "official_code", "code_scheme", "name",
                                                     "name_norm", "area_m2", "valid_from", "valid_to", "file", "loaded_at")])
  })
  message(sprintf("%s: %d territories (%s)", cc, nrow(tbl), paste(names(table(tbl$level)), table(tbl$level), sep = " ", collapse = ", ")))
  invisible(tbl)
}

# Add to dim_territory the territories of a country that the data mention but the territory
# step did not create (e.g. communes when no boundary file could be downloaded): name = code
ensure_territories <- function(con, cc, ids, level, parent_id = NA, names = NULL) {
  have <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE country = ", DBI::dbQuoteString(con, cc)))$territory_id
  new <- !duplicated(ids) & !ids %in% have & !is.na(ids)
  if (!any(new)) return(invisible(0))
  tbl <- data.frame(territory_id = ids[new], level = level, parent_id = if (length(parent_id) == length(ids)) parent_id[new] else parent_id,
                    country = cc, official_code = sub("^.*-", "", ids[new]), code_scheme = "official",
                    name = if (!is.null(names)) ifelse(is.na(names[new]) | names[new] == "", sub("^.*-", "", ids[new]), names[new]) else sub("^.*-", "", ids[new]),
                    stringsAsFactors = FALSE)
  tbl$name_norm <- normalize_text(tbl$name); tbl$area_m2 <- NA; tbl$valid_from <- as.Date(NA); tbl$valid_to <- as.Date(NA); tbl$file <- NA; tbl$loaded_at <- Sys.time()
  DBI::dbAppendTable(con, "dim_territory", tbl[, c("territory_id", "level", "parent_id", "country", "official_code", "code_scheme", "name", "name_norm", "area_m2", "valid_from", "valid_to", "file", "loaded_at")])
  message(sprintf("  %s: %d %s territories added without boundaries", cc, sum(new), level))
  invisible(sum(new))
}

# Save the boundary layers of a country (sf objects in any CRS) as data/territory_<cc>.gpkg.
# Each layer needs territory_id and name; region_id / province_id are added from dim_territory.
# Dissolve polygons into their parents (one feature per parent id). Geometries are repaired first and a
# group whose union still fails is unioned after a zero buffer (invalid rings in some national files).
dissolve_territories <- function(g, parent_ids, names = NULL) {
  geom <- tryCatch(sf::st_make_valid(sf::st_geometry(g)), error = function(e) sf::st_geometry(g))
  parts <- split(seq_along(geom), parent_ids)
  # the union on the sphere (s2) refuses rings it cannot rebuild: then the union is done the planar way, in the
  # layer's UTM zone, and brought back
  planar <- function(x) { epsg <- utm_epsg(x); sf::st_transform(sf::st_union(sf::st_make_valid(sf::st_transform(x, epsg))), sf::st_crs(x)) }
  one <- function(i) tryCatch(sf::st_union(geom[i]), error = function(e) tryCatch(sf::st_union(sf::st_buffer(sf::st_make_valid(geom[i]), 0)), error = function(e2) planar(geom[i])))
  u <- do.call(c, lapply(parts, one))
  out <- sf::st_sf(territory_id = names(parts), geometry = u, crs = sf::st_crs(g))
  if (!is.null(names)) out$name <- names[match(out$territory_id, names(names))]
  out
}

save_country_gpkg <- function(con, cc, layers, tolerance_m = 150) {
  gpkg <- COUNTRY_MODULES[[cc]]$gpkg
  terr <- DBI::dbGetQuery(con, paste0("SELECT territory_id, level, parent_id FROM dim_territory WHERE country = ", DBI::dbQuoteString(con, cc)))
  parent_of <- setNames(terr$parent_id, terr$territory_id); level_of <- setNames(terr$level, terr$territory_id)
  up_to <- function(ids, lvl) {
    r <- ids
    for (i in 1:5) { lv <- unname(level_of[r]); done <- is.na(r) | is.na(lv) | lv == lvl; if (all(done)) break; r[!done] <- unname(parent_of[r[!done]]) }
    lv <- unname(level_of[r]); ifelse(!is.na(lv) & lv == lvl, r, NA_character_)
  }
  if (file.exists(gpkg)) file.remove(gpkg)
  for (lv in names(layers)) {
    g <- layers[[lv]]; if (is.null(g) || nrow(g) == 0) next
    g <- sf::st_make_valid(g)
    if (tolerance_m > 0) { crs <- sf::st_crs(g); g <- sf::st_transform(g, 3857); g <- sf::st_simplify(g, dTolerance = tolerance_m, preserveTopology = TRUE); g <- sf::st_transform(g, 4326) }
    else g <- sf::st_transform(g, 4326)
    g$region_id <- up_to(g$territory_id, "region"); g$province_id <- up_to(g$territory_id, "province")
    # `current` (optional, logical): FALSE marks a polygon of a classification that no longer exists; the app draws
    # it only for the periods in which it has figures. Layers without the column are all current.
    g <- g[!sf::st_is_empty(g), c("territory_id", "name", "region_id", "province_id", if ("current" %in% names(g)) "current", attr(g, "sf_column"))]
    sf::st_write(g, gpkg, layer = lv, quiet = TRUE, append = FALSE)
    message(sprintf("  %s layer: %d polygons", lv, nrow(g)))
  }
  invisible(gpkg)
}

# --- Boundaries from open sources ------------------------------------------------------------

# Eurostat GISCO: NUTS regions (levels 1-3) and LAU (municipalities) of EU/EFTA/candidates.
# Licence: free reuse with the source statement "© EuroGeographics for the administrative boundaries".
gisco_nuts <- function(level, year = CONFIG$gisco$nuts_year, scale = "03M") {
  url <- sprintf("%s/nuts/geojson/NUTS_RG_%s_%d_4326_LEVL_%d.geojson", CONFIG$gisco$base, scale, year, level)
  dir <- CONFIG$shared$gisco
  f <- newest_first(list.files(dir, basename(url), full.names = TRUE))
  if (length(f) == 0 || file_age_days(f[1]) > 365) f <- download_versioned(url, dir)
  sf::st_read(f[1], quiet = TRUE)
}
gisco_lau <- function(cc, year = CONFIG$gisco$lau_year) {
  url <- sprintf("%s/lau/gpkg/LAU_RG_01M_%d_4326.gpkg", CONFIG$gisco$base, year)
  dir <- CONFIG$shared$gisco
  f <- newest_first(list.files(dir, basename(url), full.names = TRUE))
  if (length(f) == 0) f <- download_versioned(url, dir)
  g <- sf::st_read(f[1], quiet = TRUE, query = sprintf("SELECT * FROM \"%s\" WHERE CNTR_CODE = '%s'", sf::st_layers(f[1])$name[1], cc))
  g
}

# geoBoundaries (CC BY 4.0): ADM0-ADM3 of every country, through its API
geoboundaries <- function(iso3, adm) {
  dir <- CONFIG$shared$geoboundaries
  name <- sprintf("geoBoundaries-%s-ADM%d.geojson", iso3, adm)
  f <- newest_first(list.files(dir, name, full.names = TRUE))
  if (length(f) == 0) {
    meta <- get_text(sprintf("https://www.geoboundaries.org/api/current/gbOpen/%s/ADM%d/", iso3, adm))
    if (is.null(meta)) stop("geoBoundaries API not reachable for ", iso3, " ADM", adm)
    url <- regmatches(meta, regexpr('"gjDownloadURL"\\s*:\\s*"[^"]+"', meta))
    url <- sub('^"gjDownloadURL"\\s*:\\s*"', "", sub('"$', "", url))
    if (!nzchar(url)) stop("No download URL in the geoBoundaries answer for ", iso3)
    f <- download_versioned(url, dir, name = name)
  }
  sf::st_read(f[1], quiet = TRUE)
}

# Natural Earth (public domain): first-level divisions of every country
natural_earth_admin1 <- function(iso2) {
  dir <- CONFIG$shared$natural_earth
  url <- "https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_1_states_provinces.zip"
  f <- newest_first(list.files(dir, "ne_10m_admin_1_states_provinces\\.zip$", full.names = TRUE))
  if (length(f) == 0) f <- download_versioned(url, dir)
  g <- read_shp_zip(f[1])
  g[g$iso_a2 == iso2, ]
}

# --- Population denominators --------------------------------------------------------------

# Eurostat: population on 1 January by NUTS3 (demo_r_pjangrp3) through the dissemination API (no key)
# Population on 1 January by NUTS 0-3 for one country (geo, year, value), from the shared Eurostat
# demo_r_pjangrp3 JSON-stat file that the Eurostat-regional module downloads (R/92_eurostat_regions.R);
# downloaded here when missing. (The SDMX 3.0 TSV filter used before answered HTTP 400.)
eurostat_population_nuts3 <- function(cc, since = 2015) {
  if (!exists("er_population")) stop("R/92_eurostat_regions.R not loaded")
  if (is.na(er_file(paste0(EUROSTAT_REGIONS$population, "_T.json")))) er_download()
  p <- er_population(); geo <- if (cc == "GR") "EL" else cc
  p <- p[p$cntr == geo & p$year >= since & !grepl("Z$", p$geo), c("geo", "year", "value")]
  if (geo != cc) p$geo <- sub(paste0("^", geo), cc, p$geo)
  p
}
# Eurostat TSV (dimensions in the first column separated by commas, years as columns)
parse_eurostat_tsv <- function(f) {
  raw <- readr::read_tsv(f, col_types = readr::cols(.default = "c"), progress = FALSE)
  key <- names(raw)[1]; dims <- strsplit(sub("\\\\.*$", "", key), ",")[[1]]
  long <- raw |> tidyr_pivot_longer(names(raw)[-1]) |>
    dplyr::mutate(year = as.integer(trimws(year)), value = suppressWarnings(as.numeric(gsub("[^0-9.]", "", value))))
  keys <- do.call(rbind, strsplit(long[[key]], ","))
  for (i in seq_along(dims)) long[[dims[i]]] <- keys[, i]
  long[[key]] <- NULL
  long
}
tidyr_pivot_longer <- function(df, cols) {   # without depending on tidyr
  out <- do.call(rbind, lapply(cols, function(cn) data.frame(df[, setdiff(names(df), cols), drop = FALSE], year = cn, value = df[[cn]], stringsAsFactors = FALSE)))
  out
}

# --- Marts (what the app reads) ----------------------------------------------------------------

country_marts <- function(con) {
  has <- function(tb) tb %in% DBI::dbListTables(con)
  # Figures by project offence type: the source codes that map to one type are added up FIRST, and the
  # quarterly figure (the difference between two consecutive cumulative quarters) is computed on the sum,
  # so that a type fed by several codes (two naming schemes, two sub-categories) gets the right quarter.
  # Names and flags of a type: the first row of its codes in the reference table; sensitive if any code is.
  other_crime <- "
    WITH typed AS (
      SELECT s.country, s.source, s.territory_id, s.level, o.offence_type_id, s.year, s.quarter, sum(s.cumulative) AS cumulative
      FROM stg_country_crime s
      JOIN ref_country_offence_types o ON o.country = s.country AND o.source = s.source AND o.source_code = s.offence_code
      WHERE o.offence_type_id <> 'unmapped' AND s.cumulative IS NOT NULL
      GROUP BY ALL),
    names AS (
      SELECT country, source, offence_type_id, first(included_in) AS included_in, bool_or(coalesce(sensitive, FALSE)) AS sensitive,
             first(name_local) AS name_local, first(name_en) AS name_en
      FROM ref_country_offence_types WHERE offence_type_id <> 'unmapped' GROUP BY ALL)
    SELECT c.country, c.source, c.territory_id, c.level, coalesce(t.name, c.territory_id) AS name, t.parent_id,
           c.offence_type_id, n.included_in, n.sensitive, n.name_local, n.name_en, c.year, c.quarter, c.cumulative,
           CASE WHEN c.quarter = 1 THEN c.cumulative
                WHEN lag(c.quarter) OVER w = c.quarter - 1 THEN c.cumulative - lag(c.cumulative) OVER w END AS quarterly
    FROM typed c
    JOIN names n ON n.country = c.country AND n.source = c.source AND n.offence_type_id = c.offence_type_id
    LEFT JOIN dim_territory t ON t.territory_id = c.territory_id
    WINDOW w AS (PARTITION BY c.country, c.source, c.territory_id, c.offence_type_id, c.year ORDER BY c.quarter)"
  DBI::dbExecute(con, paste0("CREATE OR REPLACE TABLE mart_country_crime AS ", other_crime, " ORDER BY country, source, territory_id, offence_type_id, year, quarter"))
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE mart_country_territory_info AS SELECT country, territory_id, info_en, info_local FROM ref_country_territory_info ORDER BY ALL")
  DBI::dbExecute(con, "CREATE OR REPLACE TABLE mart_country_population AS
                       SELECT country, level, territory_id, year, population FROM ref_country_population WHERE population > 0 ORDER BY ALL")   # uninhabited communes / empty NUTS codes: no rate

  # One row per country and source with data: names, levels, source and population metadata for the app
  # ('main' = the country's source; a second source repeats the country columns and carries its own name,
  # levels, measure, texts and the territory it covers)
  meta_row <- function(m, s = NULL) data.frame(
    country = m$code, source = s$id %||% "main", name_en = m$name_en, name_local = m$name_local, lang = m$lang,
    covers = s$covers %||% m$code, short_name_en = (s$short_name %||% m$short_name)[1] %||% "", short_name_local = (s$short_name %||% m$short_name)[2] %||% "",
    short_name_key = (s$short_name_key %||% m$short_name_key) %||% "",
    levels = paste(s$levels %||% m$levels, collapse = ";"),
    level_labels_en = paste(vapply(s$levels %||% m$levels, function(l) (s$level_labels %||% m$level_labels)[[l]][1], ""), collapse = ";"),
    level_labels_local = paste(vapply(s$levels %||% m$levels, function(l) (s$level_labels %||% m$level_labels)[[l]][2], ""), collapse = ";"),
    level_singular_en = paste(vapply(s$levels %||% m$levels, function(l) { x <- (s$level_labels %||% m$level_labels)[[l]]; if (length(x) >= 3) x[3] else x[1] }, ""), collapse = ";"),
    level_singular_local = paste(vapply(s$levels %||% m$levels, function(l) { x <- (s$level_labels %||% m$level_labels)[[l]]; if (length(x) >= 4) x[4] else if (length(x) >= 3) x[3] else x[2] }, ""), collapse = ";"),
    source_name = (s %||% m$source)$name, publisher = (s %||% m$source)$publisher %||% "", url = (s %||% m$source)$url %||% "", licence = (s %||% m$source)$licence %||% "", measure = (s %||% m$source)$measure %||% "",
    # the same names in the country's language, when the module gives them (shown in the sidebar in that language)
    source_local = (s %||% m$source)$name_local %||% (s %||% m$source)$name, publisher_local = (s %||% m$source)$publisher_local %||% (s %||% m$source)$publisher %||% "",
    population_source = (s$population_source %||% m$population_source) %||% "", population_source_local = (s$population_source_local %||% s$population_source %||% m$population_source_local %||% m$population_source) %||% "",
    boundaries_attribution = (s$boundaries_attribution %||% m$boundaries_attribution) %||% "",
    not_published_en = (s$not_published %||% m$not_published)[1] %||% "", not_published_local = (s$not_published %||% m$not_published)[2] %||% "",
    # What is counted, when it is not "recorded offences" (complaints, reported cases...): title and noun, English and local
    measure_kind = (s$measure_labels %||% m$measure_labels)$kind %||% "",
    measure_title_en = (s$measure_labels %||% m$measure_labels)$title[1] %||% "", measure_title_local = (s$measure_labels %||% m$measure_labels)$title[2] %||% "",
    count_noun_en = (s$measure_labels %||% m$measure_labels)$noun[1] %||% "", count_noun_local = (s$measure_labels %||% m$measure_labels)$noun[2] %||% "",
    # translation keys that override the English / local texts when the app has them in every language
    not_published_key = (s$not_published_key %||% m$not_published_key) %||% "", not_published_legend_key = (s$not_published_legend_key %||% m$not_published_legend_key) %||% "",
    title_key = s$title_key %||% "", help_key = s$help_key %||% "", notes_key = s$notes_key %||% "", source_text_key = s$source_text_key %||% "", breakdown_note_key = s$breakdown_note_key %||% "",
    zoom_region = unname(m$zoom_widths["region"]) %||% NA_real_, zoom_province = unname(m$zoom_widths["province"]) %||% NA_real_,
    notes_en = (s$notes_en %||% m$notes_en) %||% "", notes_local = (s$notes_local %||% m$notes_local) %||% "", default_region = m$default_region %||% "",
    financial_year = isTRUE(m$financial_year),                      # quarters of a financial year (April-March; year = the one it ends in): the app labels them "2025/26"
    # How the source counts (About page, "How each source counts"): where the event is counted, which date, revisions policy
    method_where = (s$method %||% m$method)$where %||% "", method_date = (s$method %||% m$method)$date %||% "", method_revisions = (s$method %||% m$method)$revisions %||% "",
    stringsAsFactors = FALSE)
  rows <- unlist(lapply(COUNTRY_MODULES, function(m) c(list(meta_row(m)), lapply(m$sources %||% list(), function(s) meta_row(m, s)))), recursive = FALSE)
  meta <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  loaded <- DBI::dbGetQuery(con, "SELECT country, source, min(year * 10 + quarter) AS first_period, max(year * 10 + quarter) AS last_period, count(*) AS rows FROM stg_country_crime GROUP BY 1, 2")
  if (nrow(meta) > 0) meta <- merge(meta, loaded, by = c("country", "source"), all.x = TRUE)
  # The day the source was last read (for "Cite this data"): the last load of the country in the log
  if (nrow(meta) > 0) {
    when <- DBI::dbGetQuery(con, "SELECT source AS log_source, max(loaded_at) AS loaded_at FROM ingest_log WHERE source LIKE 'country_%' GROUP BY 1")
    own <- ifelse(meta$source == "main", paste0("country_", meta$country), paste0("country_", meta$country, "_", meta$source))   # a second source logs its own loads
    meta$loaded_at <- when$loaded_at[match(own, when$log_source)]
    miss <- is.na(meta$loaded_at); meta$loaded_at[miss] <- when$loaded_at[match(paste0("country_", meta$country[miss]), when$log_source)]
  }
  meta <- if (nrow(meta) > 0) meta[!is.na(meta$rows) & meta$rows > 0, ] else meta
  DBI::dbWriteTable(con, "mart_country_meta", meta, overwrite = TRUE)
  invisible(TRUE)
}

# --- Pipeline ----------------------------------------------------------------------------------

# TRUE when the country has rows and nothing it is built from (raw files, its reference CSVs) changed since the last load
country_unchanged <- function(con, m) {
  if (!all(c("stg_country_crime", "ingest_log") %in% DBI::dbListTables(con))) return(FALSE)
  last <- DBI::dbGetQuery(con, paste0("SELECT max(loaded_at) AS t FROM ingest_log WHERE source = ", DBI::dbQuoteString(con, paste0("country_", m$code))))$t
  if (length(last) == 0 || is.na(last)) return(FALSE)
  rows <- DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM stg_country_crime WHERE country = ", DBI::dbQuoteString(con, m$code)))$n
  if (rows == 0) return(FALSE)
  # A module that reads files kept in a shared folder (the Eurostat tables that 23 countries use) names them with
  # `watch`: a new copy of one of them is a change too. Without it those countries were kept for ever, since their
  # own folder is empty, and a new year published by Eurostat never arrived unless the country was reloaded by hand.
  shared <- if (is.function(m$watch)) tryCatch(as.character(m$watch()), error = function(e) character()) else character()
  shared <- shared[!is.na(shared) & file.exists(shared)]
  files <- c(list.files(m$dir_raw, full.names = TRUE, recursive = TRUE), shared,
             list.files(CONFIG$dir_ref, paste0("^", tolower(m$code), "_.*\\.csv$"), full.names = TRUE))
  if (!country_rules_current(con, m)) return(FALSE)             # the rules that read the source changed (below)
  length(files) > 0 && max(file.mtime(files)) < as.POSIXct(last)
}

# The rules that read a source. A module (or a second source) whose way of reading its files changes raises its
# `rules` mark ("2", "3"...): the mark of the last load is kept in country_rules, and a country whose mark differs is
# loaded again at the next full run, by itself. Without it a correction of a reader only reached the data when the
# country was reloaded by hand (option 14). No mark stored = the first mark ("1"): nothing is reloaded for nothing.
country_rules_mark <- function(m) {
  second <- vapply(m$sources %||% list(), function(s) paste0(s$id, "=", s$rules %||% "1"), "")
  paste(c(as.character(m$rules %||% "1"), second), collapse = ";")
}
country_rules_stored <- function(con, m) {
  if (!"country_rules" %in% DBI::dbListTables(con)) return(NA_character_)
  r <- DBI::dbGetQuery(con, paste0("SELECT rules FROM country_rules WHERE country = ", DBI::dbQuoteString(con, m$code)))$rules
  if (length(r) == 0) NA_character_ else r[1]
}
country_rules_current <- function(con, m) {
  stored <- country_rules_stored(con, m)
  if (is.na(stored)) stored <- paste(c("1", vapply(m$sources %||% list(), function(s) paste0(s$id, "=1"), "")), collapse = ";")
  identical(stored, country_rules_mark(m))
}
country_rules_save <- function(con, m) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS country_rules (country VARCHAR PRIMARY KEY, rules VARCHAR, loaded_at TIMESTAMP)")
  DBI::dbExecute(con, paste0("DELETE FROM country_rules WHERE country = ", DBI::dbQuoteString(con, m$code)))
  DBI::dbAppendTable(con, "country_rules", data.frame(country = m$code, rules = country_rules_mark(m), loaded_at = Sys.time(), stringsAsFactors = FALSE))
  invisible(TRUE)
}

# force = FALSE (full run): a country whose raw and reference files (and the shared files it names with `watch`)
# did not change since its last load is not parsed again (Canada's 50 GB of CSV, the French communes...). A run limited with `only`
# always reloads those countries (that is how a code fix is applied).
country_steps <- function(con, download = TRUE, only = NULL, force = !is.null(only)) {
  mods <- COUNTRY_MODULES; if (!is.null(only)) mods <- mods[intersect(names(mods), only)]
  if (length(mods) == 0) { message("No country modules registered."); return(invisible(FALSE)) }
  fetch <- function(label, expr) try_download(label, expr, download)
  for (m in mods) {
    step(sprintf("%s (%s): %s", m$name_en, m$code, m$source$name), place = m$code, label = sprintf("%s (%s)", m$name_en, m$code))
    dir.create(m$dir_raw, recursive = TRUE, showWarnings = FALSE)
    fetch(m$code, m$download())
    attempt(paste(m$code, "offence types not loaded"), load_country_types(con, m$code))
    unchanged <- !force && country_unchanged(con, m)
    if (unchanged) { message("= ", m$code, ": files unchanged since the last load, kept (update_countries(only = \"", m$code, "\") reloads it)"); journal_keep(m$code) }
    else {
      loaded_before <- DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM stg_country_crime WHERE country = ", DBI::dbQuoteString(con, m$code)))$n > 0
      if (!force && loaded_before && !country_rules_current(con, m)) message("  ", m$code, ": the rules that read this source changed since its last load (now ", country_rules_mark(m), "): loaded again")
      ok_t <- attempt(paste(m$code, "territory not loaded"), m$territory(con))
      ok_i <- attempt(paste(m$code, "data not loaded"), m$ingest(con))
      if (isTRUE(ok_t) && isTRUE(ok_i)) country_rules_save(con, m)
    }
    has_pop <- "ref_country_population" %in% DBI::dbListTables(con) && DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM ref_country_population WHERE country = ", DBI::dbQuoteString(con, m$code)))$n > 0
    if (!unchanged || !has_pop) attempt(paste(m$code, "population not loaded (no rates)"), m$population(con))   # a failed population is retried
    # Second sources of the country (a regional police...): same four steps, their own offence types
    for (s in m$sources %||% list()) {
      step(sprintf("%s (%s) · %s: %s", m$name_en, m$code, s$id, s$name), place = paste0(m$code, "/", s$id), label = sprintf("%s (%s) · %s", m$name_en, m$code, s$id))
      if (!is.null(s$download)) fetch(paste(m$code, s$id), s$download())
      attempt(paste(m$code, s$id, "offence types not loaded"), load_country_types(con, m$code, s$offence_types_file %||% paste0(tolower(m$code), "_", s$id, "_offence_types.csv"), s$id))
      if (!unchanged) {
        attempt(paste(m$code, s$id, "territory not loaded"), s$territory(con))
        attempt(paste(m$code, s$id, "data not loaded"), s$ingest(con))
        attempt(paste(m$code, s$id, "population not loaded (no rates)"), s$population(con))
      } else { message("= ", m$code, " ", s$id, ": kept"); journal_keep(paste0(m$code, "/", s$id)) }
    }
  }
  invisible(TRUE)
}

# `only` takes country codes (Spain is ES) and city codes (MAD, BCN: modules of R/60_cities.R)
# refresh = TRUE asks every portal again even for files downloaded recently (a broken or truncated copy is
# then replaced by a new versioned copy; identical content is not saved twice)
update_countries <- function(download = TRUE, only = NULL, force = !is.null(only), refresh = FALSE) {
  old <- options(webarchy.refresh = isTRUE(refresh)); on.exit(options(old), add = TRUE)
  with_journal(if (is.null(only)) "countries" else paste("chosen:", paste(only, collapse = " ")), download = download, with_db(function(con) {
    city_codes <- vapply(CITY_MODULES, function(m) m$code, ""); cities <- intersect(only, city_codes); others <- setdiff(only, city_codes)
    if (length(cities)) city_steps(con, download, cities, after_countries = FALSE)
    if (is.null(only) || length(others)) country_steps(con, download, if (is.null(only)) NULL else others, force)
    if (length(cities)) city_steps(con, download, cities, after_countries = TRUE)    # cities built on a country's data run after it
    step("Tables for the app")
    build_marts(con)
    if (length(cities)) city_status(con)
    country_status(con)
  }))
}

# The last period loaded of every country and source, with its age against the rhythm of the source (freshness(),
# R/05_journal.R). One row per country and source: country, source, name, to_year, to_quarter, yearly, months, limit, old, text
country_freshness <- function(con, today = Sys.Date()) {
  out <- data.frame(country = character(), source = character(), name = character(), to_year = integer(), to_quarter = integer(), yearly = logical(), months = numeric(), limit = numeric(), old = logical(), text = character(), stringsAsFactors = FALSE)
  if (!"stg_country_crime" %in% DBI::dbListTables(con)) return(out)
  x <- DBI::dbGetQuery(con, "SELECT country, source, max(year * 10 + quarter) AS to_p, count(DISTINCT quarter) AS quarters FROM stg_country_crime GROUP BY 1, 2 ORDER BY 1, 2")
  for (i in seq_len(nrow(x))) {
    m <- COUNTRY_MODULES[[x$country[i]]]; if (is.null(m)) next
    s <- if (x$source[i] == "main") m else m$sources[[x$source[i]]]
    y <- as.integer(x$to_p[i] %/% 10); q <- as.integer(x$to_p[i] %% 10); yearly <- x$quarters[i] == 1 && q == 4L
    f <- freshness(period_end(y, q, isTRUE(m$financial_year)), yearly, s$freshness_months, today)
    out <- rbind(out, data.frame(country = x$country[i], source = x$source[i], name = m$name_en, to_year = y, to_quarter = q, yearly = yearly, months = f$months, limit = f$limit, old = f$old, text = f$text, stringsAsFactors = FALSE))
  }
  out
}

country_status <- function(con) {
  if (!"stg_country_crime" %in% DBI::dbListTables(con)) return(invisible(NULL))
  x <- DBI::dbGetQuery(con, "
    SELECT c.country, c.source, count(DISTINCT c.territory_id) AS territories, min(c.year) AS from_y, max(c.year * 10 + c.quarter) AS to_p,
           (SELECT count(*) FROM ref_country_population p WHERE p.country = c.country) AS pop_rows
    FROM stg_country_crime c GROUP BY 1, 2 ORDER BY 1, 2")
  fr <- country_freshness(con)
  journal_mute({
  message("Countries")
  for (m in COUNTRY_MODULES) {
    title <- sprintf("%s (%s)", m$name_en, m$code)
    journal_done(m$code, "status", title)
    r <- x[x$country == m$code & x$source == "main", ]
    if (nrow(r) == 0) { message("[!!] ", m$name_en, ": no data loaded yet"); journal_add("attention", "no data loaded yet", place = m$code, phase = "status"); next }
    extra <- x[x$country == m$code & x$source != "main", ]
    fy <- isTRUE(m$financial_year); yt <- function(y) if (fy) sprintf("%d/%02d", y - 1, y %% 100) else as.character(y)
    f <- fr[fr$country == m$code & fr$old, ]
    message(sprintf("%-5s %s %d territories, %s to %s Q%d%s%s%s%s", if (nrow(f)) "[old]" else "[ok]", format(m$name_en, width = 18), r$territories, yt(r$from_y), yt(r$to_p %/% 10), r$to_p %% 10, if (fy) " (financial years)" else "",
                    if (r$pop_rows > 0) "" else " (no population: no rates)", if (file.exists(m$gpkg)) "" else " (no boundaries: table only)",
                    if (nrow(extra)) paste0(" + ", paste(sprintf("%s: %d territories, %d to %dQ%d", extra$source, extra$territories, extra$from_y, extra$to_p %/% 10, extra$to_p %% 10), collapse = "; ")) else ""))
    for (i in seq_len(nrow(f))) {
      what <- if (f$source[i] == "main") "" else paste0(f$source[i], ": ")
      message("      ", what, f$text[i])
      journal_add("stale", paste0(what, sub("ended .* ago", sprintf("was %s Q%d", yt(f$to_year[i]), f$to_quarter[i]), f$text[i])), place = m$code, phase = "status")
    }
  }
  })
  invisible(x)
}
