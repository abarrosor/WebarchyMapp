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
  # The table of national rates of Eurostat fed the "Countries table" of the app, retired on 2026-11-17 (P-063):
  # nothing writes it or reads it any more, so the one an earlier run left is dropped (the downloaded file stays)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS mart_country_rates")

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

  # 1b. The same by year, so that the rate of a period stands on the residents of its own years. One register per
  #     city and year: the first of the year, the nearest to 1 January, when a city publishes one a month (and so a
  #     figure that does not move while the year goes on). A district or a city is added up only in a year that
  #     holds exactly the parts of the latest register: a short sum would give a rate that is too high. A year
  #     without a figure is not invented here: the app takes the nearest one and marks it.
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_population_years AS
    WITH b AS (
      SELECT p.territory_id, t.level, t.parent_id, CASE WHEN t.level = 'neighbourhood' THEN d.parent_id ELSE t.parent_id END AS city_id,
             p.population, p.ref_date, year(p.ref_date)::INTEGER AS year
      FROM ref_population p
      JOIN dim_territory t ON t.territory_id = p.territory_id AND t.level IN ('neighbourhood', 'district')
      LEFT JOIN dim_territory d ON d.territory_id = t.parent_id),
    first AS (SELECT city_id, year, min(ref_date) AS d FROM b GROUP BY 1, 2),
    r AS (SELECT b.* FROM b JOIN first f ON b.city_id = f.city_id AND b.year = f.year AND b.ref_date = f.d),
    nb AS (SELECT territory_id, parent_id AS district_id, year, population, ref_date FROM r WHERE level = 'neighbourhood'),
    -- the parts a sum needs: the neighbourhoods of each district and the districts of each city in the latest register
    need_nb AS (SELECT t.parent_id AS district_id, count(*) AS n FROM mart_population m JOIN dim_territory t ON t.territory_id = m.territory_id
                WHERE m.level = 'neighbourhood' GROUP BY 1),
    di_sum AS (SELECT nb.district_id, nb.year, sum(nb.population)::INTEGER AS population, max(nb.ref_date) AS ref_date, count(*) AS n_all, count(m.territory_id) AS n_known
               FROM nb LEFT JOIN mart_population m ON m.territory_id = nb.territory_id AND m.level = 'neighbourhood' GROUP BY 1, 2),
    di AS (SELECT s.district_id, s.year, s.population, s.ref_date FROM di_sum s JOIN need_nb k ON k.district_id = s.district_id AND k.n = s.n_all AND k.n = s.n_known
           UNION ALL
           SELECT territory_id, year, population::INTEGER, ref_date FROM r WHERE level = 'district' AND territory_id NOT IN (SELECT district_id FROM need_nb)),
    need_di AS (SELECT t.parent_id AS city_id, count(*) AS n FROM mart_population m JOIN dim_territory t ON t.territory_id = m.territory_id
                WHERE m.level = 'district' GROUP BY 1),
    city_sum AS (SELECT t.parent_id AS city_id, di.year, sum(di.population)::INTEGER AS population, max(di.ref_date) AS ref_date, count(*) AS n_all, count(m.territory_id) AS n_known
                 FROM di JOIN dim_territory t ON t.territory_id = di.district_id
                 LEFT JOIN mart_population m ON m.territory_id = di.district_id AND m.level = 'district' GROUP BY 1, 2)
    SELECT 'neighbourhood' AS level, territory_id, year, population::INTEGER AS population, ref_date FROM nb
    UNION ALL
    SELECT 'district', district_id, year, population, ref_date FROM di
    UNION ALL
    SELECT 'municipality', c.city_id, c.year, c.population, c.ref_date FROM city_sum c JOIN need_di k ON k.city_id = c.city_id AND k.n = c.n_all AND k.n = c.n_known
    ORDER BY 2, 3")

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
