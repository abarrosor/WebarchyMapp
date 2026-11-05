# Spain (ES): Crime Balance of the Ministry of the Interior, in the shared framework ------------------
# Spain is one more country module (decision Q-005 / P-027). Its three sources keep their own
# readers (R/20_crime_balance.R, R/21_ine_population.R, R/22_municipal_boundaries.R) and this
# module feeds the shared tables with them:
#   stg_country_crime       from v_crime_balance (figure of the most recent publication, cumulative
#                           January to quarter end; every type of the latest publication that covers the
#                           territory and period, so that the old and new type schemes never mix), offence_code = normalised type key
#   ref_country_population  INE register figures by municipality, added up to province, region, country
#   dim_territory           country, 19 regions, 52 provinces (reference/es_provinces.csv), every
#                           municipality of the IGN boundaries with its INE name; islands and old
#                           municipalities that only the Balance mentions are added at ingest
#   data/territory_es.gpkg  layers region / province / municipality (IGN CartoBase ANE, CC BY 4.0)
# Offence types: reference/es_offence_types.csv (source_code = key of the published label, kept in
# source_text; two schemes, until 2021 and since 2022, map to the same project types).
# Until the old Spanish tables are dropped (phase 4 of Q-005) the readers also fill them, and
# es_parity() compares both paths.

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
  crime_balance_ingest(con, force = TRUE)            # fills stg_crime_balance / v_crime_balance (and registers islands and old municipalities)
  if (!"v_crime_balance" %in% c(DBI::dbListTables(con))) stop("v_crime_balance missing: the Crime Balance was not loaded")
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

# --- Population: INE register by municipality, added up by level -------------------------------------
es_population <- function(con) {
  ine_population_ingest(con)                          # ref_population_municipality (kept for the Mossos police areas and the parity check)
  if (DBI::dbGetQuery(con, "SELECT count(*) AS n FROM ref_population_municipality")$n == 0) stop("no INE population loaded")
  pop <- DBI::dbGetQuery(con, "
    WITH m AS (SELECT territory_id, year, population FROM ref_population_municipality)
    SELECT 'municipality' AS level, territory_id, year, population::DOUBLE AS population FROM m
    UNION ALL SELECT 'province', regexp_extract(territory_id, '^ES-[0-9]+-[0-9]+'), year, sum(population) FROM m GROUP BY 2, 3
    UNION ALL SELECT 'region', regexp_extract(territory_id, '^ES-[0-9]+'), year, sum(population) FROM m GROUP BY 2, 3
    UNION ALL SELECT 'country', 'ES', year, sum(population) FROM m GROUP BY 3")
  pop$country <- "ES"
  replace_country_rows(con, "ref_country_population", "ES", pop[, c("country", "territory_id", "level", "year", "population")])
  message(sprintf("  population: %d territories, %d-%d (INE municipal register, 1 January)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
  invisible(pop)
}

# --- Parity: the old Spanish tables against the shared ones (decision D4 of Q-005) --------------------
# Prints what differs and saves reports/parity_es_<date>.csv. Read-only.
es_parity <- function(save = TRUE) {
  with_db(function(con) {
    has <- function(tb) tb %in% DBI::dbListTables(con)
    out <- list(); add <- function(check, n, detail = "") out[[length(out) + 1]] <<- data.frame(check = check, n = as.integer(n), detail = detail, stringsAsFactors = FALSE)
    if (!has("mart_crime_balance") || !has("mart_country_crime")) { message("Both mart_crime_balance and mart_country_crime are needed (run option 14 with ES first)."); return(invisible(NULL)) }
    old <- DBI::dbGetQuery(con, "SELECT territory_id, level, name, offence_type_id, year, quarter, cumulative FROM mart_crime_balance")
    new <- DBI::dbGetQuery(con, paste0("SELECT territory_id, level, name, offence_type_id, year, quarter, cumulative FROM mart_country_crime WHERE country = 'ES'", if ("source" %in% DBI::dbListFields(con, "mart_country_crime")) " AND source = 'main'" else ""))
    key <- function(d) paste(d$territory_id, d$offence_type_id, d$year, d$quarter)
    # the old mart may hold two rows for one type and period (two published labels of the same type): add them up like the new one
    old <- old |> dplyr::group_by(territory_id, level, offence_type_id, year, quarter) |> dplyr::summarise(name = dplyr::first(name), cumulative = sum(cumulative), .groups = "drop") |> as.data.frame()
    ko <- key(old); kn <- key(new)
    add("crime_rows_old", nrow(old)); add("crime_rows_new", nrow(new))
    only_old <- old[!ko %in% kn, ]; only_new <- new[!kn %in% ko, ]
    # Expected: the old type scheme's aggregate in the periods republished with the new scheme (P-028)
    new_scheme <- paste(new$territory_id, new$year, new$quarter)[new$offence_type_id == "conventional_total"]
    p028 <- only_old$offence_type_id == "other_offences" & paste(only_old$territory_id, only_old$year, only_old$quarter) %in% new_scheme
    add("crime_only_in_old_expected_p028", sum(p028), "old-scheme aggregate of a period republished with the new scheme: left out on purpose")
    only_old <- only_old[!p028, ]
    add("crime_only_in_old", nrow(only_old), paste(utils::head(unique(only_old$territory_id), 5), collapse = ", "))
    add("crime_only_in_new", nrow(only_new), paste(utils::head(unique(only_new$territory_id), 5), collapse = ", "))
    j <- merge(old, new, by = c("territory_id", "offence_type_id", "year", "quarter"), suffixes = c("_old", "_new"))
    diff <- j[is.na(j$cumulative_old) != is.na(j$cumulative_new) | (!is.na(j$cumulative_old) & abs(j$cumulative_old - j$cumulative_new) > 0.5), ]
    add("crime_value_differences", nrow(diff), paste(utils::head(sprintf("%s %s %dQ%d: %s vs %s", diff$territory_id, diff$offence_type_id, diff$year, diff$quarter, diff$cumulative_old, diff$cumulative_new), 5), collapse = " | "))
    lvl <- j[j$level_old != j$level_new, ]; add("crime_level_differences", nrow(lvl), paste(utils::head(unique(lvl$territory_id), 5), collapse = ", "))
    nm <- unique(j[j$name_old != j$name_new, c("territory_id", "name_old", "name_new")])
    add("territory_name_differences", nrow(nm), paste(utils::head(sprintf("%s: '%s' -> '%s'", nm$territory_id, nm$name_old, nm$name_new), 8), collapse = " | "))
    add("territories_old", length(unique(old$territory_id))); add("territories_new", length(unique(new$territory_id)))
    add("offence_types_old", length(unique(old$offence_type_id))); add("offence_types_new", length(unique(new$offence_type_id)))
    if (has("mart_population_municipal") && has("mart_country_population")) {
      po <- DBI::dbGetQuery(con, "SELECT level, territory_id, year, population FROM mart_population_municipal")
      pn <- DBI::dbGetQuery(con, "SELECT level, territory_id, year, population FROM mart_country_population WHERE country = 'ES' AND level <> 'police_area'")
      pj <- merge(po, pn, by = c("level", "territory_id", "year"), all = TRUE, suffixes = c("_old", "_new"))
      add("population_only_in_old", sum(is.na(pj$population_new))); add("population_only_in_new", sum(is.na(pj$population_old)))
      add("population_differences", sum(!is.na(pj$population_old) & !is.na(pj$population_new) & abs(pj$population_old - pj$population_new) > 0.5))
    }
    # Mossos by police area (second source, R/72_es_mossos.R) against the old yearly mart: full years, every group
    if (has("mart_mossos_abp_year") && "source" %in% DBI::dbListFields(con, "mart_country_crime")) {
      ao <- DBI::dbGetQuery(con, "SELECT territory_id, year, group_id AS offence_type_id, cumulative FROM mart_mossos_abp_year WHERE to_month = 12")
      an <- DBI::dbGetQuery(con, "SELECT territory_id, year, offence_type_id, cumulative FROM mart_country_crime WHERE country = 'ES' AND source = 'mossos' AND quarter = 4 AND offence_type_id <> 'sensitive'")   # the old mart kept sensitive types inside the total only
      aj <- merge(ao, an, by = c("territory_id", "year", "offence_type_id"), all = TRUE, suffixes = c("_old", "_new"))
      add("mossos_rows_old", nrow(ao)); add("mossos_rows_new", nrow(an))
      add("mossos_only_in_old", sum(is.na(aj$cumulative_new)), paste(utils::head(unique(aj$offence_type_id[is.na(aj$cumulative_new)]), 5), collapse = ", "))
      add("mossos_only_in_new", sum(is.na(aj$cumulative_old)), paste(utils::head(unique(aj$offence_type_id[is.na(aj$cumulative_old)]), 5), collapse = ", "))
      dd <- aj[!is.na(aj$cumulative_old) & !is.na(aj$cumulative_new) & abs(aj$cumulative_old - aj$cumulative_new) > 0.5, ]
      add("mossos_value_differences", nrow(dd), paste(utils::head(sprintf("%s %s %d: %s vs %s", dd$territory_id, dd$offence_type_id, dd$year, dd$cumulative_old, dd$cumulative_new), 4), collapse = " | "))
      if (has("mart_population_abp")) {
        bo <- DBI::dbGetQuery(con, "SELECT territory_id, year, population FROM mart_population_abp")
        bn <- DBI::dbGetQuery(con, "SELECT territory_id, year, population FROM mart_country_population WHERE country = 'ES' AND level = 'police_area'")
        bj <- merge(bo, bn, by = c("territory_id", "year"), all = TRUE, suffixes = c("_old", "_new"))
        add("mossos_population_differences", sum(is.na(bj$population_old) | is.na(bj$population_new) | abs(bj$population_old - bj$population_new) > 0.5))
      }
    }
    for (lay in c("region", "province", "municipality")) {
      a <- if (file.exists(GPKG_SPAIN_OLD)) tryCatch(sf::st_read(GPKG_SPAIN_OLD, layer = lay, quiet = TRUE)$territory_id, error = function(e) character()) else character()
      b <- if (file.exists(COUNTRY_MODULES$ES$gpkg)) tryCatch(sf::st_read(COUNTRY_MODULES$ES$gpkg, layer = lay, quiet = TRUE)$territory_id, error = function(e) character()) else character()
      add(paste0("polygons_", lay, "_old"), length(a)); add(paste0("polygons_", lay, "_new"), length(b))
      add(paste0("polygons_", lay, "_only_old"), length(setdiff(a, b)), paste(utils::head(setdiff(a, b), 5), collapse = ", "))
      add(paste0("polygons_", lay, "_only_new"), length(setdiff(b, a)), paste(utils::head(setdiff(b, a), 5), collapse = ", "))
    }
    res <- do.call(rbind, out)
    ok <- res$n[res$check %in% c("crime_only_in_old", "crime_only_in_new", "crime_value_differences", "crime_level_differences", "population_only_in_old", "population_only_in_new", "population_differences",
                                 "mossos_only_in_old", "mossos_only_in_new", "mossos_value_differences", "mossos_population_differences")]
    message("Spain parity (old Spanish tables vs shared country tables): ", if (all(ok == 0)) "IDENTICAL figures" else "DIFFERENCES found")
    for (i in seq_len(nrow(res))) message(sprintf("  %-32s %8s  %s", res$check[i], fmt_int(res$n[i]), substr(res$detail[i], 1, 140)))
    if (save) { f <- file.path(CONFIG$dir_reports, paste0("parity_es_", format(Sys.Date(), "%Y%m%d"), ".csv")); utils::write.csv(res, f, row.names = FALSE, fileEncoding = "UTF-8"); message("Saved: ", f) }
    invisible(res)
  })
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
  notes_en = "Municipalities above 20,000 inhabitants and islands only (above 30,000 in 2019). The Ministry revises the previous year in each publication. The Mossos d'Esquadra and Ertzaintza figures are inside the regional totals.",
  default_region = "ES-13", zoom_widths = c(region = 11, province = 7),
  download = es_download, ingest = es_ingest, territory = es_territory, population = es_population
))
