# Spain · second source "mossos": Mossos d'Esquadra by basic police area (ABP) --------------------------
# The Catalan police publish the offences KNOWN to them by month and basic police area (qnyt-emjc);
# the Ministry's Balance already contains them inside the regional totals, so this is an alternative
# view of Catalonia, never added to the Balance (P-029: a second source of one country in the shared
# contract, with its own offence types, level and texts). The readers stay in R/30_mossos.R (they also
# serve the Barcelona city view); this module copies the result into the shared tables:
#   stg_country_crime        source 'mossos': level 'police_area' (ES-09-ABP-<code>) and the region
#                            ES-09 (Catalonia without the online reports of the "ABP Virtual"),
#                            complete quarters only (monthly_to_cumulative), offence_code = offence key,
#                            plus TOTAL_MOSSOS = every offence known, sensitive ones included
#   ref_country_offence_types   reference/es_mossos_offence_types.csv (Criminal Code offence -> 13 groups)
#   dim_territory            the 60 ABP as 'police_area' under ES-09
#   data/territory_es.gpkg   layer 'police_area' (Generalitat open data), written by abp_save() in R/30_mossos.R
#   ref_country_population   each ABP by year: INE municipalities added up; the Barcelona districts
#                            share the city's figure by the city register
#   ref_country_territory_info  the municipalities inside each ABP (shown under the territory selector)

ES_MOSSOS_REGION <- "ES-09"

es_mossos_download <- function() {
  attempt("Mossos download", mossos_download())
  attempt("Police areas (ABP) download", abp_download())
  invisible(TRUE)
}

# ABP rows in dim_territory, the police_area layer and the municipalities of each ABP
es_mossos_territory <- function(con) {
  abp_save(con)                                      # ref_abp, ref_abp_municipality and the police_area layer of data/territory_es.gpkg
  ref <- DBI::dbGetQuery(con, "SELECT abp_code, name FROM ref_abp ORDER BY abp_code")
  if (nrow(ref) == 0) stop("no ABP list (ref_abp is empty: boundaries file missing?)")
  rows <- data.frame(territory_id = paste0(ES_MOSSOS_REGION, "-ABP-", ref$abp_code), level = "police_area", parent_id = ES_MOSSOS_REGION, country = "ES",
                     official_code = ref$abp_code, code_scheme = "Mossos d'Esquadra", name = ref$name, stringsAsFactors = FALSE)
  rows$name_norm <- normalize_text(rows$name); rows$area_m2 <- NA_real_; rows$valid_from <- as.Date(NA); rows$valid_to <- as.Date(NA); rows$file <- NA_character_; rows$loaded_at <- Sys.time()
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM dim_territory WHERE country = 'ES' AND level = 'police_area'")
    DBI::dbAppendTable(con, "dim_territory", rows[, c("territory_id", "level", "parent_id", "country", "official_code", "code_scheme", "name", "name_norm", "area_m2", "valid_from", "valid_to", "file", "loaded_at")])
  })
  # Context line: the municipalities inside each ABP (most populated first), or "a district of Barcelona"
  info <- DBI::dbGetQuery(con, "
    WITH u AS (SELECT max(year) AS y FROM ref_country_population WHERE country = 'ES' AND level = 'municipality'),
    mu AS (SELECT DISTINCT m.abp_code, m.ine_code, m.district_code, t.name, i.population
           FROM ref_abp_municipality m
           LEFT JOIN dim_territory t ON t.country = 'ES' AND t.level = 'municipality' AND right(t.territory_id, 5) = m.ine_code
           LEFT JOIN ref_country_population i ON i.country = 'ES' AND i.level = 'municipality' AND right(i.territory_id, 5) = m.ine_code AND i.year = (SELECT y FROM u))
    SELECT abp_code, count(DISTINCT ine_code) AS n, string_agg(name, ', ' ORDER BY population DESC) AS municipalities,
           bool_or(district_code IS NOT NULL) AS barcelona_district
    FROM mu GROUP BY abp_code")
  if (nrow(info)) {
    save_territory_info(con, "ES", data.frame(
      territory_id = paste0(ES_MOSSOS_REGION, "-ABP-", info$abp_code),
      info_en = ifelse(info$barcelona_district, "A district of Barcelona.", sprintf("%d municipalities: %s", info$n, info$municipalities)),
      info_local = ifelse(info$barcelona_district, "Un distrito de Barcelona.", sprintf("%d municipios: %s", info$n, info$municipalities)), stringsAsFactors = FALSE))
  }
  message(sprintf("ES mossos: %d police areas", nrow(rows)))
  invisible(rows)
}

# Monthly known offences -> complete quarters, by ABP and for Catalonia (without the ABP Virtual)
es_mossos_ingest <- function(con) {
  mossos_ingest(con)                                 # stg_mossos / ref_mossos_abp_map
  m <- DBI::dbGetQuery(con, "
    SELECT year(s.month) AS year, month(s.month) AS month, a.abp_code, upper(s.abp) = 'ABP VIRTUAL' AS virtual, s.offence_key, sum(s.known) AS n
    FROM stg_mossos s LEFT JOIN ref_mossos_abp_map a ON a.abp = s.abp
    WHERE s.known IS NOT NULL GROUP BY ALL")
  if (nrow(m) == 0) stop("no Mossos rows loaded")
  abp <- m[!is.na(m$abp_code), ]
  abp <- data.frame(territory_id = paste0(ES_MOSSOS_REGION, "-ABP-", abp$abp_code), level = "police_area", offence_code = abp$offence_key, year = abp$year, month = abp$month, n = abp$n)
  reg <- m[!m$virtual, ] |> dplyr::group_by(year, month, offence_key) |> dplyr::summarise(n = sum(n), .groups = "drop")
  reg <- data.frame(territory_id = ES_MOSSOS_REGION, level = "region", offence_code = reg$offence_key, year = reg$year, month = reg$month, n = reg$n)
  d <- rbind(abp, reg)
  tot <- d |> dplyr::group_by(territory_id, level, year, month) |> dplyr::summarise(n = sum(n), .groups = "drop") |> as.data.frame()
  tot$offence_code <- "TOTAL_MOSSOS"
  d <- rbind(d, tot[, names(d)]); d$country <- "ES"; d$source <- "mossos"
  q <- monthly_to_cumulative(d, keys = c("country", "source", "territory_id", "level", "offence_code"))
  q$file <- "stg_mossos"
  replace_country_rows(con, "stg_country_crime", "ES", q[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file", "source")], source = "mossos")
  per <- range(q$year * 10 + q$quarter)
  log_ingest(con, "country_ES_mossos", "stg_mossos", NA, sprintf("%dQ%d-%dQ%d", per[1] %/% 10, per[1] %% 10, per[2] %/% 10, per[2] %% 10), nrow(q))
  message(sprintf("  ES mossos: %s rows, %d police areas, %dQ%d-%dQ%d (complete quarters only)", fmt_int(nrow(q)), length(unique(abp$territory_id)), per[1] %/% 10, per[1] %% 10, per[2] %/% 10, per[2] %% 10))
  invisible(q)
}

# Population of each ABP by year: INE municipalities added up; Barcelona districts share the city's
# figure in proportion to the city register (mart_population). Level 'police_area' only is replaced.
es_mossos_population <- function(con) {
  need <- c("ref_country_population", "ref_abp_municipality", "ref_population", "dim_territory")
  if (!all(need %in% DBI::dbListTables(con))) stop("needs ", toString(setdiff(need, DBI::dbListTables(con))))
  if (DBI::dbGetQuery(con, "SELECT count(*) AS n FROM ref_abp_municipality")$n == 0) stop("no municipality-to-ABP table")
  pop <- DBI::dbGetQuery(con, paste0("
    WITH m AS (SELECT abp_code, ine_code, district_code FROM ref_abp_municipality),
    ine AS (SELECT right(territory_id, 5) AS ine_code, year, population FROM ref_country_population WHERE country = 'ES' AND level = 'municipality' AND territory_id LIKE 'ES-09-%'),
    nbp AS (SELECT p.territory_id, t.parent_id AS district_id, p.population, p.ref_date FROM ref_population p
            JOIN dim_territory t ON t.territory_id = p.territory_id AND t.level = 'neighbourhood' AND t.territory_id LIKE '", ID_BARCELONA, "-%'),
    lastd AS (SELECT max(ref_date) AS d FROM nbp),
    dpop AS (SELECT district_id, sum(population) AS population FROM nbp WHERE ref_date = (SELECT d FROM lastd) GROUP BY 1),
    dis AS (SELECT right(district_id, 2) AS district_code, population * 1.0 / sum(population) OVER () AS share FROM dpop),
    p AS (
      SELECT m.abp_code, i.year, i.population * 1.0 AS pop FROM m JOIN ine i USING (ine_code) WHERE m.district_code IS NULL
      UNION ALL
      SELECT m.abp_code, i.year, i.population * d.share FROM m JOIN ine i USING (ine_code) JOIN dis d USING (district_code) WHERE m.district_code IS NOT NULL)
    SELECT 'ES' AS country, '", ES_MOSSOS_REGION, "-ABP-' || abp_code AS territory_id, 'police_area' AS level, year, round(sum(pop)) AS population
    FROM p GROUP BY abp_code, year ORDER BY 2, 4"))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM ref_country_population WHERE country = 'ES' AND level = 'police_area'")
    if (nrow(pop)) DBI::dbAppendTable(con, "ref_country_population", pop)
  })
  message(sprintf("  population: %d police areas, %d-%d", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
  invisible(pop)
}

register_source("ES", list(
  id = "mossos",
  name = "Mossos d'Esquadra: offences known by basic police area", name_local = "Mossos d'Esquadra: hechos conocidos por área básica policial",
  short_name = c("Mossos (police areas, all of Catalonia)", "Mossos (áreas policiales, toda Cataluña)"), short_name_key = "sp_source_mossos",
  publisher = "Government of Catalonia, Department of the Interior (open data)", publisher_local = "Generalitat de Catalunya, Departament d'Interior (dades obertes)",
  url = CONFIG$sources$mossos$page, licence = "Open data of the Government of Catalonia (see the terms of use of the portal)",
  measure = "Criminal offences known to the Mossos d'Esquadra by month and basic police area (ABP: a county, a group of municipalities or a district of Barcelona), grouped in 13 groups; complete quarters only. Already inside the Ministry's regional totals: an alternative view of Catalonia, never added to the Balance.",
  levels = "police_area",
  level_labels = list(police_area = c("Police areas (ABP)", "Áreas básicas policiales (ABP)", "Police area (ABP)", "Área policial (ABP)")),
  covers = ES_MOSSOS_REGION,
  measure_labels = list(kind = "known_offences", title = c("Recorded offences (Mossos)", "Hechos conocidos (Mossos)"), noun = c("recorded offences", "hechos conocidos")),
  title_key = "sp_title_mossos", help_key = "sp_source_help", notes_key = "sp_mossos_note", source_text_key = "sp_source_mossos_text", breakdown_note_key = "sp_breakdown_mossos_note",
  method = list(where = "Basic police area (ABP) that handled the case; online reports without a place go to the 'ABP Virtual' (left out of the Catalonia total)", date = "Month the offence became known to the Mossos (fet conegut); complete quarters only", revisions = "Yearly files republished; the most recent file is kept"),
  notes_en = "An ABP is a county or a group of municipalities (in Barcelona, a district): the figure does not belong to any single municipality. Online reports with no place (ABP Virtual) are left out of the Catalonia total.",
  population_source = "INE register by municipality, added up by police area (Barcelona districts share the city's figure by the city register)",
  population_source_local = "Padrón INE por municipio, sumado por área policial (los distritos de Barcelona reparten la cifra de la ciudad según el padrón municipal)",
  boundaries_attribution = "Generalitat de Catalunya (open data)",
  download = es_mossos_download, territory = es_mossos_territory, ingest = es_mossos_ingest, population = es_mossos_population
))
