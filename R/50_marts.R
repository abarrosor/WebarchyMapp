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

  # 1. Most recent resident population of each city: neighbourhood, district and city (sum of its neighbourhoods)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_population AS
    WITH b AS (
      SELECT p.territory_id, t.parent_id AS district_id, d.parent_id AS city_id, p.population, p.ref_date
      FROM ref_population p
      JOIN dim_territory t ON t.territory_id = p.territory_id AND t.level = 'neighbourhood'
      JOIN dim_territory d ON d.territory_id = t.parent_id),
    last AS (SELECT city_id, max(ref_date) AS d FROM b GROUP BY 1),
    nb AS (SELECT b.* FROM b JOIN last ON b.city_id = last.city_id AND b.ref_date = last.d),
    -- a city whose register exists by district only (Mexico City's alcaldías): district rows given directly
    dd AS (SELECT p.territory_id AS district_id, t.parent_id AS city_id, p.population, p.ref_date
           FROM ref_population p JOIN dim_territory t ON t.territory_id = p.territory_id AND t.level = 'district'
           WHERE p.territory_id NOT IN (SELECT DISTINCT district_id FROM nb)
             AND p.ref_date = (SELECT max(ref_date) FROM ref_population p2 WHERE p2.territory_id = p.territory_id)),
    di AS (SELECT district_id, city_id, sum(population)::INTEGER AS population, max(ref_date) AS ref_date FROM nb GROUP BY 1, 2
           UNION ALL SELECT district_id, city_id, population::INTEGER, ref_date FROM dd)
    SELECT 'neighbourhood' AS level, territory_id, population, ref_date FROM nb
    UNION ALL
    SELECT 'district', district_id, population, ref_date FROM di
    UNION ALL
    SELECT 'municipality', city_id, sum(population)::INTEGER, max(ref_date) FROM di GROUP BY city_id")

  # 2. Territory names for the app (every level, no geometry)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_territory AS
    SELECT territory_id, level, parent_id, name FROM dim_territory ORDER BY territory_id")

  # The shared marts of the cities (events, surveys, panels) and the countries
  for (f in c("city_marts", "country_marts")) {
    if (exists(f, mode = "function")) do.call(f, list(con))
  }
  nc <- DBI::dbGetQuery(con, "SELECT count(DISTINCT city) AS n FROM mart_city_events")$n
  nk <- DBI::dbGetQuery(con, "SELECT count(DISTINCT country) AS n FROM mart_country_crime")$n
  p <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM mart_population")$n
  message(sprintf("Marts updated: %d cities, %d countries%s.", nc, nk, if (p > 0) "" else " (no city register loaded yet)"))
  invisible(TRUE)
}
