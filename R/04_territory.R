# Territory table (dim_territory) --------------------------------------------
# Identifier scheme (decision P-006), built from official codes:
#   ES                         country (ISO 3166-1)
#   ES-13                      autonomous region (INE code)
#   ES-13-28                   province (INE code)
#   ES-13-28-28079             municipality (INE code, 5 digits)
#   ES-13-28-28079-D01         district (city council code)
#   ES-13-28-28079-D01-B011    neighbourhood (city council code)
#   ES-09-ABP-34               Mossos basic police area (Catalonia)
#   ES-04-07-I-MALLORCA        island (Ministry crime balance)
# The identifier is for reading. Code uses `level`, `parent_id` and
# `official_code`. All codes are text.

# Country / region / province / municipality rows for a city. Region and
# province names are the official INE names (reference/es_provinces.csv).
upper_levels <- function(municipality_id, city_name) {
  p <- strsplit(municipality_id, "-")[[1]]          # ES, 13, 28, 28079
  ref <- read_reference("es_provinces.csv")
  region_name <- ref$region_name[match(p[2], ref$region_code)][1] %||% p[2]
  province_name <- ref$province_name[match(p[3], ref$province_code)][1] %||% p[3]
  data.frame(
    territory_id  = c("ES", paste(p[1:2], collapse = "-"), paste(p[1:3], collapse = "-"), municipality_id),
    level         = c("country", "region", "province", "municipality"),
    parent_id     = c(NA, "ES", paste(p[1:2], collapse = "-"), paste(p[1:3], collapse = "-")),
    country       = "ES",
    official_code = c("ES", p[2], p[3], p[4]),
    code_scheme   = c("ISO 3166-1", "INE", "INE", "INE"),
    name          = c("Spain", region_name, province_name, city_name),
    stringsAsFactors = FALSE)
}

# The UTM zone (EPSG code) under a layer, for planar operations in metres
utm_epsg <- function(g) {
  bb <- sf::st_bbox(suppressWarnings(sf::st_transform(g, 4326)))
  lon <- mean(bb[c("xmin", "xmax")]); lat <- mean(bb[c("ymin", "ymax")])
  as.integer((if (lat >= 0) 32600 else 32700) + floor((lon + 180) / 6) + 1)
}

# Polygons from a portal made valid the planar way (GEOS, in the layer's UTM zone), their area in m² computed
# there (column area_m2), and returned in WGS84. The spherical engine of sf (s2) refuses to repair a ring that
# crosses itself ("Loop 0 is not valid: Edge 2589 crosses edge 2594", the Stadtkreise of Zürich) while GEOS
# repairs it; a repair that leaves lines or points behind keeps its polygon parts only
clean_polygons <- function(g) {
  g <- sf::st_zm(g, drop = TRUE, what = "ZM")
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  p <- sf::st_transform(g, utm_epsg(g))
  geom <- sf::st_geometry(sf::st_make_valid(p))
  geom <- sf::st_sfc(lapply(geom, function(x) {
    if (inherits(x, "GEOMETRYCOLLECTION")) { parts <- sf::st_collection_extract(sf::st_sfc(x), "POLYGON"); x <- if (length(parts)) sf::st_union(parts)[[1]] else sf::st_polygon() }
    x }), crs = sf::st_crs(p))
  sf::st_geometry(p) <- geom
  p$area_m2 <- as.numeric(sf::st_area(p))
  sf::st_transform(p, 4326)
}

# Read the shapefile inside a zip (unzipped to a temporary folder)
read_shp_zip <- function(zip) {
  tmp <- file.path(tempdir(), tools::file_path_sans_ext(basename(zip)))
  unlink(tmp, recursive = TRUE)
  utils::unzip(zip, exdir = tmp)
  shp <- list.files(tmp, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp) != 1) stop("Expected one .shp inside ", basename(zip))
  sf::st_read(shp, quiet = TRUE, options = "ENCODING=UTF-8")
}

# Save districts and neighbourhoods of a city: rows in dim_territory (without
# geometry) and geometries in a GeoPackage in WGS84 (EPSG:4326)
save_city_territory <- function(con, districts, neighbourhoods, upper, gpkg) {
  drop_geo <- function(x) sf::st_drop_geometry(x)
  tbl <- dplyr::bind_rows(upper, drop_geo(districts), drop_geo(neighbourhoods)) |>
    dplyr::mutate(name_norm = normalize_text(name), valid_from = as.Date(NA), valid_to = as.Date(NA),
                  loaded_at = Sys.time())
  DBI::dbWithTransaction(con, {
    ids <- paste(DBI::dbQuoteString(con, tbl$territory_id), collapse = ", ")
    DBI::dbExecute(con, paste0("DELETE FROM dim_territory WHERE territory_id IN (", ids, ")"))
    # districts and neighbourhoods of an earlier build that no longer exist (a changed boundaries file) go too
    for (u in unique(upper$territory_id)) DBI::dbExecute(con, paste0("DELETE FROM dim_territory WHERE level IN ('district', 'neighbourhood') AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(u, "-%"))))
    DBI::dbAppendTable(con, "dim_territory", as.data.frame(
      tbl[, c("territory_id", "level", "parent_id", "country", "official_code", "code_scheme", "name",
              "name_norm", "area_m2", "valid_from", "valid_to", "file", "loaded_at")]))
  })
  geo <- rbind(districts, neighbourhoods)[, c("territory_id", "level", "parent_id", "name")]
  geo <- sf::st_transform(geo, 4326)
  if (file.exists(gpkg)) file.remove(gpkg)
  sf::st_write(geo, gpkg, layer = "territory", quiet = TRUE)
  message("Territory saved: ", nrow(districts), " districts and ", nrow(neighbourhoods), " neighbourhoods. Geometries in ", gpkg)
  invisible(tbl)
}

# --- Madrid ----------------------------------------------------------------------
GPKG_MADRID <- "data/territory_madrid.gpkg"

madrid_territory_download <- function() {
  for (s in c("madrid_districts", "madrid_neighbourhoods")) {
    src <- CONFIG$sources[[s]]
    for (u in src$urls) download_versioned(u, src$dir_raw)
  }
}

madrid_territory_build <- function() {
  zip_dis <- latest_file(CONFIG$sources$madrid_districts$dir_raw, "Distritos\\.zip$")
  zip_bar <- latest_file(CONFIG$sources$madrid_neighbourhoods$dir_raw, "Barrios\\.zip$")
  dis <- read_shp_zip(zip_dis); bar <- read_shp_zip(zip_bar)
  missing <- function(x, cols) setdiff(cols, names(x))
  if (length(f <- missing(dis, c("COD_DIS_TX", "NOMBRE")))) stop("Districts: missing columns ", toString(f))
  if (length(f <- missing(bar, c("COD_DIS_TX", "COD_BAR", "NOMBRE")))) stop("Neighbourhoods: missing columns ", toString(f))
  if (nrow(dis) != 21)  warning("Expected 21 districts, found ", nrow(dis))
  if (nrow(bar) != 131) warning("Expected 131 neighbourhoods, found ", nrow(bar))
  dis_code <- sprintf("%02d", as.integer(dis$COD_DIS_TX))
  bar_dis  <- sprintf("%02d", as.integer(bar$COD_DIS_TX))
  bar_code <- sprintf("%03d", as.integer(bar$COD_BAR))
  if (anyDuplicated(bar_code)) stop("Duplicated neighbourhood codes")
  if (!all(substr(bar_code, 1, 2) == bar_dis)) stop("Some neighbourhood codes do not start with their district code")
  districts <- sf::st_sf(
    territory_id = paste0(ID_MADRID, "-D", dis_code), level = "district", parent_id = ID_MADRID, country = "ES",
    official_code = dis_code, code_scheme = "Madrid City Council", name = dis$NOMBRE,
    area_m2 = as.numeric(sf::st_area(dis)), file = basename(zip_dis), geometry = sf::st_geometry(dis))
  neighbourhoods <- sf::st_sf(
    territory_id = paste0(ID_MADRID, "-D", bar_dis, "-B", bar_code), level = "neighbourhood",
    parent_id = paste0(ID_MADRID, "-D", bar_dis), country = "ES", official_code = bar_code,
    code_scheme = "Madrid City Council", name = bar$NOMBRE, area_m2 = as.numeric(sf::st_area(bar)),
    file = basename(zip_bar), geometry = sf::st_geometry(bar))
  if (!all(neighbourhoods$parent_id %in% districts$territory_id)) stop("Neighbourhoods with a missing district")
  list(districts = districts, neighbourhoods = neighbourhoods)
}

madrid_territory_save <- function(con) {
  t <- madrid_territory_build()
  save_city_territory(con, t$districts, t$neighbourhoods,
                      upper_levels(ID_MADRID, "Madrid"), GPKG_MADRID)
}

# --- Barcelona ------------------------------------------------------------------
# Open Data BCN CSVs with WKT geometries. Neighbourhood codes are unique across
# the city (1-73), districts 1-10.
GPKG_BARCELONA <- "data/territory_barcelona.gpkg"

# Read an Open Data BCN CSV: detects separator and encoding; lower-case
# accent-free column names (Codi_Barri -> codi_barri)
bcn_read_csv <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE)
  enc <- if (validUTF8(header)) "UTF-8" else "latin1"
  header <- iconv(header, enc, "UTF-8")
  sep <- if (lengths(regmatches(header, gregexpr(";", header))) > lengths(regmatches(header, gregexpr(",", header)))) ";" else ","
  df <- readr::read_delim(path, delim = sep, col_types = readr::cols(.default = "c"),
                          locale = readr::locale(encoding = enc), trim_ws = TRUE, progress = FALSE)
  names(df) <- tolower(normalize_text(gsub("[\"']", "", sub("^﻿", "", names(df)))))
  df
}

bcn_territory_download <- function() {
  src <- CONFIG$sources$bcn_boundaries
  ckan_download_new(src$ckan_id, src$dir_raw, "csv", base = CONFIG$ckan_barcelona, include = src$include)
}

bcn_territory_build <- function() {
  src <- CONFIG$sources$bcn_boundaries
  f_dis <- latest_file(src$dir_raw, "Districtes\\.csv$"); f_bar <- latest_file(src$dir_raw, "Barris\\.csv$")
  dis <- bcn_read_csv(f_dis); bar <- bcn_read_csv(f_bar)
  missing <- function(x, cols) setdiff(cols, names(x))
  if (length(f <- missing(dis, c("codi_districte", "nom_districte", "geometria_wgs84")))) stop("Districts: missing columns ", toString(f))
  if (length(f <- missing(bar, c("codi_districte", "codi_barri", "nom_barri", "geometria_wgs84")))) stop("Neighbourhoods: missing columns ", toString(f))
  if (nrow(dis) != 10) warning("Expected 10 districts in Barcelona, found ", nrow(dis))
  if (nrow(bar) != 73) warning("Expected 73 neighbourhoods in Barcelona, found ", nrow(bar))
  dis_code <- sprintf("%02d", as.integer(dis$codi_districte))
  bar_dis  <- sprintf("%02d", as.integer(bar$codi_districte))
  bar_code <- sprintf("%03d", as.integer(bar$codi_barri))
  if (anyDuplicated(bar_code)) stop("Duplicated Barcelona neighbourhood codes")
  g_dis <- sf::st_as_sfc(dis$geometria_wgs84, crs = 4326); g_bar <- sf::st_as_sfc(bar$geometria_wgs84, crs = 4326)
  districts <- sf::st_sf(
    territory_id = paste0(ID_BARCELONA, "-D", dis_code), level = "district", parent_id = ID_BARCELONA, country = "ES",
    official_code = dis_code, code_scheme = "Barcelona City Council", name = trimws(dis$nom_districte),
    area_m2 = as.numeric(sf::st_area(g_dis)), file = basename(f_dis), geometry = g_dis)
  neighbourhoods <- sf::st_sf(
    territory_id = paste0(ID_BARCELONA, "-D", bar_dis, "-B", bar_code), level = "neighbourhood",
    parent_id = paste0(ID_BARCELONA, "-D", bar_dis), country = "ES", official_code = bar_code,
    code_scheme = "Barcelona City Council", name = trimws(bar$nom_barri), area_m2 = as.numeric(sf::st_area(g_bar)),
    file = basename(f_bar), geometry = g_bar)
  if (!all(neighbourhoods$parent_id %in% districts$territory_id)) stop("Barcelona neighbourhoods with a missing district")
  list(districts = districts, neighbourhoods = neighbourhoods)
}

bcn_territory_save <- function(con) {
  t <- bcn_territory_build()
  save_city_territory(con, t$districts, t$neighbourhoods,
                      upper_levels(ID_BARCELONA, "Barcelona"), GPKG_BARCELONA)
}

# --- Barcelona register (population by neighbourhood) ------------------------------------
# Open Data BCN dataset pad_mdbas_sexe: residents by census section and sex at 1 January, one file per year.
# The files from the first year of the incidents on are downloaded (a few hundred KB each) and all are kept: the
# rate of a period stands on the residents of its own years (mart_population_years). A year that cannot be
# downloaded is reported and left out; the newest file is the one that must be there.
bcn_census_years <- function(names, from = NULL) {
  y <- suppressWarnings(as.integer(sub("^([0-9]{4})_.*$", "\\1", names)))
  if (is.null(from) || is.na(from) || !any(y >= from, na.rm = TRUE)) return(!is.na(y) & y == max(y, na.rm = TRUE))   # no first year known: the newest only
  !is.na(y) & y >= from
}
bcn_census_download <- function() {
  src <- CONFIG$sources$bcn_census
  res <- ckan_resources(src$ckan_id, base = CONFIG$ckan_barcelona)
  res <- res[res$format == "csv" & grepl("^[0-9]{4}_", res$name), ]
  if (nrow(res) == 0) stop("No Barcelona register CSV in Open Data BCN")
  res <- res[order(res$name, decreasing = TRUE), ]
  res <- res[bcn_census_years(res$name, CONFIG$sources$bcn_incidents$from), ]
  dir.create(src$dir_raw, recursive = TRUE, showWarnings = FALSE)
  res$file <- file.path(src$dir_raw, ckan_file_name(res))
  got <- character()
  for (i in seq_len(nrow(res))) {
    dest <- res$file[i]
    if (file.exists(dest)) { message("= ", basename(dest), " already downloaded"); got <- c(got, dest); next }
    ok <- tryCatch({ download_safely(res$url[i], dest); TRUE }, error = function(e) {
      if (i == 1) stop(e)                                    # the newest register: without it there is no "latest" figure
      message("x ", basename(dest), " not downloaded (", conditionMessage(e), "): the rates of that year will stand on the nearest register"); FALSE })
    if (ok) { message("+ ", basename(dest)); got <- c(got, dest); Sys.sleep(1) }
  }
  invisible(got)
}

bcn_census_ingest <- function(con) {
  src <- CONFIG$sources$bcn_census
  files <- list.files(src$dir_raw, "\\.csv$", full.names = TRUE)
  if (length(files) == 0) { message("No Barcelona register in ", src$dir_raw); return(invisible(NULL)) }
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE '",
                                       ID_BARCELONA, "-%'"))$territory_id
  if (length(known) == 0) stop("Load the Barcelona neighbourhoods first")
  done <- DBI::dbGetQuery(con, "SELECT DISTINCT file FROM ref_population")$file
  # One file per year. A file that cannot be read (another structure in an old year) is reported and left out: the
  # rates of that year then stand on the nearest register. The newest file is the one that must load.
  files <- files[order(basename(files), decreasing = TRUE)]
  load_one <- function(path) {
    df <- bcn_read_csv(path)
    missing <- setdiff(c("data_referencia", "codi_districte", "codi_barri", "valor"), names(df))
    if (length(missing) > 0) stop(basename(path), ": missing columns ", toString(missing))
    ref_date <- unique(as.Date(substr(df$data_referencia, 1, 10)))
    if (length(ref_date) != 1 || is.na(ref_date)) stop(basename(path), ": reference date not recognised")
    id <- paste0(ID_BARCELONA, "-D", sprintf("%02d", as.integer(df$codi_districte)), "-B", sprintf("%03d", as.integer(df$codi_barri)))
    pop <- tapply(suppressWarnings(as.numeric(df$valor)), id, sum, na.rm = TRUE)
    unknown <- setdiff(names(pop), known)
    if (length(unknown) > 0) warning("Barcelona register neighbourhoods that do not exist: ", toString(unknown))
    pop <- pop[names(pop) %in% known]
    if (length(pop) == 0) stop(basename(path), ": none of its neighbourhoods is in the territory")
    ids_sql <- paste(DBI::dbQuoteString(con, names(pop)), collapse = ", ")
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE ref_date = DATE '", ref_date, "' AND territory_id IN (", ids_sql, ")"))
      DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = names(pop), ref_date = ref_date,
                                                           population = as.integer(pop), file = basename(path)))
    })
    message(sprintf("+ %s: Barcelona population at %s = %s residents in %d neighbourhoods", basename(path),
                    format(ref_date, "%Y-%m-%d"), fmt_int(sum(pop)), length(pop)))
  }
  for (i in seq_along(files)) {
    path <- files[i]
    if (basename(path) %in% done) { message("= ", basename(path), " (already loaded)"); next }
    if (i == 1) load_one(path)
    else tryCatch(load_one(path), error = function(e) message("x ", conditionMessage(e), " (this year is left out: its rates stand on the nearest register)"))
  }
  invisible(TRUE)
}

# Number of neighbourhoods of a city already in the database
city_neighbourhood_count <- function(con, city_id) {
  DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE '",
                              city_id, "-%'"))$n
}
