# Tables for the app (marts) ------------------------------------------------------
# The app ONLY reads these tables. Rules (see Claude/02_data/analysis-madrid-calls.md):
#   1. Never day or hour: at least month x neighbourhood.                 -> here
#   2. Sensitive types out (`sensitive` in reference/).                   -> here
#   3. Only blocks that can be read by area: public safety and
#      coexistence (traffic, ambulance/fire support, 092 closures... out). -> here
#   4. Figures below the threshold -> "fewer than N".                     -> in the app,
#      AFTER adding up the chosen range of months (`apply_threshold()`).
#   5. Totals never mix blocks.                                           -> in the app
#   6. Different measures or sources are never added together.           -> everywhere

MARTS <- list(
  threshold = 10L,
  blocks    = c("public_safety", "coexistence")
)

# Rule 4, shared with the app: NA when n < threshold
apply_threshold <- function(n, threshold = MARTS$threshold) ifelse(is.na(n) | n < threshold, NA, n)

build_marts <- function(con, threshold = MARTS$threshold) {
  n_terr <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM dim_territory WHERE level = 'neighbourhood'")$n
  if (n_terr == 0) {
    message("Marts not built: load the territories first (option 7 of the menu).")
    return(invisible(FALSE))
  }
  blocks_sql <- paste(DBI::dbQuoteString(con, MARTS$blocks), collapse = ", ")

  # 1. Monthly base: month x neighbourhood x type, no day/hour nor sensitive types.
  #    Includes rows without neighbourhood (with or without district) for the
  #    district and city totals; never shown by neighbourhood.
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE mart_madrid_calls_month AS
    SELECT month, neighbourhood_id, district_id, block, call_type, sum(events)::INTEGER AS events
    FROM v_madrid_calls WHERE NOT sensitive AND block IN (", blocks_sql, ")
    GROUP BY ALL ORDER BY ALL"))

  # 2. Most recent resident population of each city (Madrid, Barcelona):
  #    neighbourhood, district and city (sum of its neighbourhoods)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_population AS
    WITH b AS (
      SELECT p.territory_id, t.parent_id AS district_id, d.parent_id AS city_id, p.population, p.ref_date
      FROM ref_population p
      JOIN dim_territory t ON t.territory_id = p.territory_id AND t.level = 'neighbourhood'
      JOIN dim_territory d ON d.territory_id = t.parent_id),
    last AS (SELECT city_id, max(ref_date) AS d FROM b GROUP BY 1),
    nb AS (SELECT b.* FROM b JOIN last ON b.city_id = last.city_id AND b.ref_date = last.d)
    SELECT 'neighbourhood' AS level, territory_id, population, ref_date FROM nb
    UNION ALL
    SELECT 'district', district_id, sum(population)::INTEGER, max(ref_date) FROM nb GROUP BY district_id
    UNION ALL
    SELECT 'municipality', city_id, sum(population)::INTEGER, max(ref_date) FROM nb GROUP BY city_id")

  # 3. Type names for the app (name_es / name_en of reference/madrid_call_types.csv)
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE mart_madrid_call_types AS
    SELECT call_type, block, name_es, name_en FROM ref_madrid_call_types
    WHERE NOT sensitive AND block IN (", blocks_sql, ")"))

  # 4. Metadata for the source card (origin, dates, coverage)
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE mart_madrid_calls_meta AS
    SELECT 'Calls received by the Municipal Police central dispatch' AS source,
           'Madrid City Council - Municipal Police' AS publisher,
           'https://datos.madrid.es/dataset/837676-0-incidencias-recibidas-en-la-emisora-central-de-policia-municipal' AS url,
           'CC BY 4.0' AS licence,
           (SELECT min(month) FROM stg_madrid_calls) AS first_month, (SELECT max(month) FROM stg_madrid_calls) AS last_month,
           (SELECT max(loaded_at) FROM ingest_log WHERE source = 'madrid_calls') AS loaded_at,
           (SELECT round(100.0 * sum(events) FILTER (neighbourhood_id IS NULL) / sum(events), 1) FROM v_madrid_calls) AS pct_without_neighbourhood,
           ", as.integer(threshold), " AS threshold,
           (SELECT max(ref_date) FROM ref_population WHERE territory_id LIKE '", ID_MADRID, "-%') AS population_date,
           current_timestamp AS generated_at"))

  # 5. Territory names for the app (every level, no geometry)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_territory AS
    SELECT territory_id, level, parent_id, name FROM dim_territory ORDER BY territory_id")

  # 6. Other sources, each in its own R file
  for (f in c("crime_balance_marts", "ine_population_marts", "mossos_marts", "bcn_incidents_marts", "madrid_activity_marts", "country_marts")) {
    if (exists(f, mode = "function")) do.call(f, list(con))
  }

  n <- DBI::dbGetQuery(con, "SELECT count(*) AS rows, count(DISTINCT month) AS months FROM mart_madrid_calls_month")
  p <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM mart_population")$n
  message(sprintf("Marts updated: %d months of Madrid calls%s.", n$months,
                  if (p > 0) " and population by neighbourhood" else " (no population loaded yet)"))
  invisible(TRUE)
}
