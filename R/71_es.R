# Spain (ES): Crime Balance of the Ministry of the Interior, in the shared framework ------------------
# Spain is one more country module (decision Q-005 / P-027). Its three sources keep their own
# readers (R/20_crime_balance.R, R/21_ine_population.R, R/22_municipal_boundaries.R) and this
# module feeds the shared tables with them:
#   stg_country_crime       from stg_crime_balance (figure of the most recent publication, cumulative
#                           January to quarter end; every type of the latest publication that covers the
#                           territory and period, so that the old and new type schemes never mix), offence_code = normalised type key
#   ref_country_population  INE register figures by municipality, added up to province, region, country
#   dim_territory           country, 19 regions, 52 provinces (reference/es_provinces.csv), every
#                           municipality of the IGN boundaries with its INE name; islands and old
#                           municipalities that only the Balance mentions are added at ingest
#   data/territory_es.gpkg  layers region / province / municipality (IGN CartoBase ANE, CC BY 4.0)
# Offence types: reference/es_offence_types.csv (source_code = key of the published label, kept in
# source_text; two schemes, until 2021 and since 2022, map to the same project types).

es_sources <- function() read_reference("es_sources.csv")
es_source <- function(key) { s <- es_sources(); u <- s$url[s$key == key]; if (length(u) && !is.na(u) && nzchar(trimws(u))) trimws(u) else NA_character_ }
es_provinces <- function() read_reference("es_provinces.csv")

# --- Download: the three sources, each on its own ----------------------------------------------------
es_download <- function() {
  attempt("Crime Balance download", crime_balance_download())
  attempt("INE population download", ine_population_download())
  attempt("IGN boundaries download", municipal_boundaries_download())
  invisible(TRUE)
}

# --- Territory: dim_territory rows and data/territory_es.gpkg -----------------------------------------
es_territory <- function(con) {
  ref <- es_provinces()
  g <- municipal_boundaries_build()                 # stops when no IGN file is there (attempt() reports it)
  mun <- sf::st_drop_geometry(g$municipalities)
  # Names: the INE register file when loaded (official spelling, what the Balance uses since 2024), else the IGN label
  ine <- newest_first(list.files(CONFIG$sources$ine_population$dir_raw, "29005\\.csv$", full.names = TRUE))
  if (length(ine)) {
    p <- tryCatch(ine_population_read(ine[1]), error = function(e) NULL)
    if (!is.null(p)) { p <- p[order(-p$year), ]; p <- p[!duplicated(p$territory_id), ]; k <- match(mun$territory_id, p$territory_id); mun$name[!is.na(k)] <- p$name[k[!is.na(k)]] }
  }
  rows <- rbind(
    data.frame(territory_id = "ES", level = "country", parent_id = NA_character_, official_code = "ES", code_scheme = "ISO 3166-1", name = "Spain", stringsAsFactors = FALSE),
    data.frame(territory_id = paste0("ES-", unique(ref$region_code)), level = "region", parent_id = "ES", official_code = unique(ref$region_code), code_scheme = "INE",
               name = ref$region_name[!duplicated(ref$region_code)], stringsAsFactors = FALSE),
    data.frame(territory_id = paste0("ES-", ref$region_code, "-", ref$province_code), level = "province", parent_id = paste0("ES-", ref$region_code), official_code = ref$province_code,
               code_scheme = "INE", name = ref$province_name, stringsAsFactors = FALSE),
    data.frame(territory_id = mun$territory_id, level = "municipality", parent_id = mun$province_id, official_code = sub("^.*-", "", mun$territory_id), code_scheme = "INE",
               name = mun$name, stringsAsFactors = FALSE))
  rows$country <- "ES"; rows$name_norm <- normalize_text(rows$name); rows$area_m2 <- NA_real_; rows$valid_from <- as.Date(NA); rows$valid_to <- as.Date(NA)
  rows$file <- NA_character_; rows$loaded_at <- Sys.time()
  # The city rows (districts, neighbourhoods of Madrid and Barcelona) and the Mossos police areas are not touched
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM dim_territory WHERE country = 'ES' AND level IN ('country', 'region', 'province', 'municipality', 'island')")
    DBI::dbAppendTable(con, "dim_territory", rows[, c("territory_id", "level", "parent_id", "country", "official_code", "code_scheme", "name", "name_norm", "area_m2", "valid_from", "valid_to", "file", "loaded_at")])
  })
  message(sprintf("ES: %d territories (country 1, region %d, province %d, municipality %d)", nrow(rows), sum(rows$level == "region"), sum(rows$level == "province"), sum(rows$level == "municipality")))
  layers <- list(region = g$regions[, c("territory_id", "name")], province = g$provinces[, c("territory_id", "name")], municipality = g$municipalities[, c("territory_id", "name")])
  layers$municipality$name <- mun$name
  save_country_gpkg(con, "ES", layers, tolerance_m = 0)   # the IGN 1:3,000,000 lines are already light: no extra simplification
  invisible(rows)
}

# --- Crime: the Balance into stg_country_crime ---------------------------------------------------------
es_ingest <- function(con) {
  crime_balance_ingest(con, force = TRUE)            # fills stg_crime_balance (and registers islands and old municipalities)
  if (!"stg_crime_balance" %in% DBI::dbListTables(con)) stop("stg_crime_balance missing: the Crime Balance was not loaded")
  n <- DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM stg_country_crime WHERE country = 'ES' AND source = 'main'")
    DBI::dbExecute(con, "
      INSERT INTO stg_country_crime (country, territory_id, level, offence_code, year, quarter, cumulative, file, source)
      SELECT 'ES' AS country, s.territory_id, s.level, s.offence_type_key AS offence_code, s.year, s.quarter, s.value AS cumulative, s.file, 'main' AS source
      FROM stg_crime_balance s
      WHERE s.territory_id IS NOT NULL AND s.level IN ('country', 'region', 'province', 'municipality', 'island') AND s.value IS NOT NULL
      -- One publication per territory and period, the most recent one, with every type it carries (P-028):
      -- a publication that repeats an earlier year uses the current type scheme, so the earlier
      -- scheme's aggregates ('Resto de infracciones penales') of that period are left out, never added
      QUALIFY s.pub_year * 10 + s.pub_quarter = max(s.pub_year * 10 + s.pub_quarter) OVER (PARTITION BY s.territory_id, s.year, s.quarter)")
  })
  per <- DBI::dbGetQuery(con, "SELECT min(year) AS y0, max(year * 10 + quarter) AS p1, count(DISTINCT territory_id) AS t, count(DISTINCT territory_id) FILTER (level = 'municipality') AS m FROM stg_country_crime WHERE country = 'ES' AND source = 'main'")
  log_ingest(con, "country_ES", "stg_crime_balance", NA, sprintf("%d-%dQ%d", per$y0, per$p1 %/% 10, per$p1 %% 10), n)
  message(sprintf("  ES: %s rows, %d territories (%d municipalities), %d-%dQ%d (figure of the latest publication)", fmt_int(n), per$t, per$m, per$y0, per$p1 %/% 10, per$p1 %% 10))
  invisible(n)
}

# --- Population: INE register by municipality, added up by level (R/21_ine_population.R writes the four levels)
es_population <- function(con) {
  ine_population_ingest(con)
  n <- DBI::dbGetQuery(con, "SELECT count(DISTINCT territory_id) AS t, min(year) AS y0, max(year) AS y1 FROM ref_country_population WHERE country = 'ES' AND level <> 'police_area'")
  if (n$t == 0) stop("no INE population loaded")
  message(sprintf("  population: %d territories, %d-%d (INE municipal register, 1 January)", n$t, n$y0, n$y1))
  invisible(TRUE)
}

register_country(list(
  code = "ES", iso3 = "ESP", name_en = "Spain", name_local = "España", lang = "es",
  levels = c("region", "province", "municipality", "island"),
  level_labels = list(region = c("Autonomous communities", "Comunidades autónomas", "Autonomous community", "Comunidad autónoma"),
                      province = c("Provinces", "Provincias", "Province", "Provincia"),
                      municipality = c("Municipalities", "Municipios", "Municipality", "Municipio"),
                      island = c("Islands", "Islas", "Island", "Isla")),
  source = list(name = "Crime Balance (Balance de Criminalidad)", name_local = "Balance de Criminalidad",
                publisher = "Ministry of the Interior, Statistical Crime Portal", publisher_local = "Ministerio del Interior, Portal Estadístico de Criminalidad",
                url = "https://estadisticasdecriminalidad.ses.mir.es/publico/portalestadistico/balances",
                licence = "Open reuse citing the Statistical Crime Portal (Law 37/2007)",
                measure = "Criminal offences recorded by the police forces that report to the Crime Statistics System (National Police, Guardia Civil, Mossos d'Esquadra, Ertzaintza, Policía Foral and the local police that take part), cumulative from January to the end of each quarter; the figure of the most recent publication is kept."),
  population_source = "INE, official municipal register figures at 1 January", population_source_local = "INE, cifras oficiales del padrón municipal a 1 de enero",
  boundaries_attribution = "IGN, CartoBase ANE (CC BY 4.0)",
  not_published = c("Fewer than 20,000 inhabitants: the Ministry does not publish its figures", "Menos de 20.000 habitantes: el Ministerio no publica sus cifras"),
  not_published_key = "sp_no_data_small", not_published_legend_key = "sp_legend_small",   # translated in every language of the app
  short_name = c("Ministry (municipalities > 20,000 inh.)", "Ministerio (municipios > 20.000 hab.)"), short_name_key = "sp_source_ministry",   # label of the source switch (R/72_es_mossos.R)
  method = list(where = "Where the offence was committed, as recorded by the police force", date = "Date the offence became known to the police; cumulative from 1 January to the end of each quarter", revisions = "Every quarterly publication revises the previous year; the most recent publication of each period is kept"),
  notes_en = "Municipalities above 20,000 inhabitants and islands only (above 30,000 in 2019). The Ministry revises the previous year in each publication. The Mossos d'Esquadra and Ertzaintza figures are inside the regional totals.",
  default_region = "ES-13", zoom_widths = c(region = 11, province = 7),
  download = es_download, ingest = es_ingest, territory = es_territory, population = es_population
))
