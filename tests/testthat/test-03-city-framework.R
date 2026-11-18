# The city framework on synthetic Zürich, Toronto, Buenos Aires and Utrecht files (helper-fixtures.R) -----------
# Expected figures are recomputed from the fixtures: Zürich title i of Kreis k has 100 + 10k + i offences
# (the "Total StGB" rows 1000 + 10k), Toronto category c of neighbourhood id has 50 + 7 id + c + (year - 2024)
# (homicides id %% 3), Buenos Aires every barrio b has 10 + b of each sub-type every month (homicides 1) plus
# 7 offences without barrio nor comuna and 5 without barrio in comuna 15 in January, Utrecht wijk w has 10 + w + t
# crimes of type t every month of 2025.
use_test_project()

test_that("the marts never hold a sensitive type, an aggregate or a block the app does not show", {
  load_fixtures()
  t <- db_query("SELECT city, event_type, block FROM mart_city_event_types WHERE city IN ('zurich', 'toronto', 'buenosaires', 'utrecht')")
  expect_true(all(t$block %in% MARTS$blocks))
  expect_false("HOMICIDE" %in% t$event_type[t$city == "toronto"])
  expect_false(any(c("Total StGB", "Total gegen die sexuelle Integrität") %in% t$event_type[t$city == "zurich"]))
  expect_false("Homicidios dolosos" %in% t$event_type[t$city == "buenosaires"])
  expect_false("Zedenmisdrijf" %in% t$event_type[t$city == "utrecht"])
  raw <- db_query("SELECT city, event_type, sensitive, block FROM ref_city_event_types WHERE city IN ('zurich', 'toronto', 'buenosaires', 'utrecht')")
  expect_true(any(raw$sensitive[raw$city == "toronto"]) && any(raw$sensitive[raw$city == "utrecht"]))      # loaded, flagged, kept out of the marts
  e <- db_query("SELECT DISTINCT e.city, e.event_type FROM mart_city_events e LEFT JOIN mart_city_event_types t ON t.city = e.city AND t.event_type = e.event_type WHERE t.event_type IS NULL")
  expect_equal(nrow(e), 0)                                     # every event row has a visible type
})

test_that("Zürich: 12 Stadtkreise under one district, a Gemeinde row ignored, an unknown title kept, residents per year", {
  load_fixtures()
  terr <- db_query("SELECT level, count(*) AS n FROM dim_territory WHERE territory_id LIKE 'CH-ZH-261%' GROUP BY level ORDER BY level")
  expect_equal(setNames(terr$n, terr$level), c(district = 1, municipality = 1, neighbourhood = 12))
  types <- db_query("SELECT event_type, name_local FROM mart_city_event_types WHERE city = 'zurich' ORDER BY event_type")
  expect_equal(nrow(types), 5)
  expect_equal(types$name_local[types$event_type == "Total Neuer Titel"], "Neuer Titel")
  s <- db_query("SELECT sum(events) AS n, count(DISTINCT neighbourhood_id) AS k FROM mart_city_events WHERE city = 'zurich' AND month = DATE '2025-01-01'")
  expect_equal(s$n, 10164); expect_equal(s$k, 12)
  expect_equal(db_query("SELECT count(*) AS n FROM mart_city_events WHERE city = 'zurich' AND neighbourhood_id IS NULL")$n, 0)
  pop <- db_query("SELECT level, population, ref_date FROM mart_population WHERE territory_id LIKE 'CH-ZH-261%' ORDER BY level, territory_id")
  expect_equal(unique(as.character(pop$ref_date)), "2025-01-01")                       # the latest register wins
  expect_equal(pop$population[pop$level == "municipality"], 324000)
  expect_equal(sum(pop$population[pop$level == "neighbourhood"]), 324000)
  meta <- db_query("SELECT time_unit, measure_kind FROM mart_city_meta WHERE city = 'zurich'")
  expect_equal(meta$time_unit, "year"); expect_equal(meta$measure_kind, "offences")
})

test_that("Toronto: the paged layer gives 12 neighbourhoods, eight visible categories and the portal's population", {
  load_fixtures()
  expect_equal(db_query("SELECT count(*) AS n FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE 'CA-35-TORONTO%'")$n, 12)
  expect_equal(nrow(db_query("SELECT * FROM mart_city_event_types WHERE city = 'toronto'")), 8)
  s <- db_query("SELECT sum(events) AS n FROM mart_city_events WHERE city = 'toronto' AND month = DATE '2025-01-01'")$n
  expect_equal(s, 9744)
  pop <- db_query("SELECT population FROM mart_population WHERE level = 'municipality' AND territory_id = 'CA-35-TORONTO'")$population
  expect_equal(pop, 159000)
  implied <- db_query("SELECT count(*) AS n FROM ref_population WHERE territory_id LIKE 'CA-35-TORONTO%' AND ref_date = DATE '2024-01-01'")$n
  expect_equal(implied, 12)                                                               # earlier years implied by the rates
})

test_that("Buenos Aires: barrios matched by name, rows without barrio kept at comuna or city level, homicides out of the breakdown", {
  load_fixtures()
  nb <- db_query("SELECT name, parent_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE 'AR-02-CABA%' ORDER BY name")
  expect_equal(nrow(nb), 6); expect_true("Nuñez" %in% nb$name); expect_true("La Boca" %in% nb$name)   # "BOCA" in the data matches "La Boca" in the layer
  expect_equal(db_query("SELECT count(*) AS n FROM dim_territory WHERE level = 'district' AND territory_id LIKE 'AR-02-CABA%'")$n, 4)   # comunas 4, 13, 14, 15
  per_year <- db_query("SELECT year(month) AS y, sum(events) AS n FROM mart_city_events WHERE city = 'buenosaires' GROUP BY 1 ORDER BY 1")
  expect_equal(per_year$n, c(2928, 2928))
  no_nb <- db_query("SELECT sum(events) AS n, count(*) FILTER (district_id IS NULL) AS no_district FROM mart_city_events WHERE city = 'buenosaires' AND neighbourhood_id IS NULL")
  expect_equal(no_nb$n, 24); expect_equal(no_nb$no_district, 2)                        # 7 + 5 a year; the 7 have no comuna either
  expect_true(db_query("SELECT bool_or(sensitive) AS s FROM ref_city_event_types WHERE city = 'buenosaires' AND event_type = 'Homicidios dolosos'")$s)
  pop <- db_query("SELECT level, count(*) AS n, sum(population) AS pop FROM mart_population WHERE territory_id LIKE 'AR-02-CABA%' GROUP BY level ORDER BY level")
  expect_equal(pop$level, c("district", "municipality"))                                   # a register by comuna only: no barrio rates
  expect_equal(pop$n, c(4, 1)); expect_equal(pop$pop, c(870000, 870000))
})

test_that("Buenos Aires: the INDEC census cuadro (labels over a line of years, a Total row, notes) is read as a population table of the latest census", {
  skip_if_not_installed("writexl")
  # The layout of c2022_caba_est_c1_1.xlsx (checked on the live file): a title block, "Código | Comuna | Población |
  # . | Variación absoluta | Variación relativa (%)", the years one row below, the Total row first, notes and source
  f <- tempfile(pattern = "20261110_cuadro_comunas_", fileext = ".xlsx")                   # a dated copy: 2026 is not the census year
  cuadro <- data.frame(a = c("Censo Nacional de Población, Hogares y Viviendas 2022", "Cuadro 1.1. Ciudad Autónoma de Buenos Aires. Total de población, variación absoluta y variación relativa, por comuna. Años 2010 y 2022",
                             "Código", NA, "02", sprintf("%05d", 2000 + 7 * 1:15), "Notas: de acuerdo con la Ley Orgánica de Comunas...", "Fuente: INDEC, Censo Nacional de Población, Hogares y Viviendas 2022."),
                       b = c(NA, NA, "Comuna", NA, "Total", paste("Comuna", 1:15), NA, NA),
                       c = c(NA, NA, "Población", "2010", "2890151", as.character(100000 + 1:15), NA, NA),
                       d = c(NA, NA, NA, "2022", "3121707", as.character(200000 + 1:15), NA, NA),
                       e = c(NA, NA, "Variación absoluta", NA, "231556", rep("100000", 15), NA, NA),
                       f = c(NA, NA, "Variación relativa (%)", NA, "8", c("4.5999999999999996", rep("8", 14)), NA, NA), stringsAsFactors = FALSE)
  writexl::write_xlsx(list(`Carátula` = data.frame(x = "Censo 2022"), `Cuadro 1.1` = cuadro), f, col_names = FALSE)
  d <- bue_read_population(f)
  expect_equal(d$unit[1], "comuna"); expect_equal(nrow(d), 15); expect_equal(d$year[1], 2022L)   # the year of the column, not 2026 from the file's date prefix
  expect_equal(d$population[d$key == "Comuna 15"], 200015)                                 # the 2022 column, not 2010, not the variation; no Total row
  expect_equal(sum(d$population), sum(200000 + 1:15))
  # A one-line header with the years in the labels works too
  g <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(Comunas = data.frame(a = c("Comuna", paste("Comuna", 1:15), "Total"), b = c("Población 2010", as.character(100000 + 1:15), "2890151"), c = c("Población 2022", as.character(200000 + 1:15), "3121707"), stringsAsFactors = FALSE)), g, col_names = FALSE)
  d2 <- bue_read_population(g)
  expect_equal(d2$year[1], 2022L); expect_equal(d2$population[d2$key == "Comuna 1"], 200001); expect_equal(nrow(d2), 15)
})

# Messages of a step, with the step run against the test database
step_messages <- function(expr) { msgs <- character(); withCallingHandlers(expr, message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }); msgs }

test_that("Utrecht (Dutch generator): the wijken of the gemeente only, water parts and aggregates out, residents of the layer", {
  load_fixtures()
  terr <- db_query("SELECT level, count(*) AS n FROM dim_territory WHERE territory_id LIKE 'NL-PV26-GM0344%' GROUP BY level ORDER BY level")
  expect_equal(setNames(terr$n, terr$level), c(district = 1, municipality = 1, neighbourhood = 3))       # WK034409 is water, WK036301 is Amsterdam's
  types <- db_query("SELECT event_type FROM mart_city_event_types WHERE city = 'utrecht' ORDER BY 1")$event_type
  expect_equal(types, c("Diefstal uit/vanaf motorvoertuigen", "Diefstal/inbraak woning"))               # Zedenmisdrijf is sensitive, "Totaal misdrijven" an aggregate
  s <- db_query("SELECT sum(events) AS n, count(DISTINCT neighbourhood_id) AS k, count(DISTINCT month) AS m FROM mart_city_events WHERE city = 'utrecht'")
  expect_equal(s$k, 3); expect_equal(s$m, 12); expect_equal(s$n, 12 * sum(outer(1:3, 1:2, function(w, t) 10 + w + t)))   # WK034404 is not in the layer; the yearly row is out
  expect_equal(db_query("SELECT sum(events) AS n FROM stg_city_events WHERE city = 'utrecht'")$n, 12 * sum(outer(1:3, 1:3, function(w, t) 10 + w + t)))
  pop <- db_query("SELECT level, population, ref_date FROM mart_population WHERE territory_id LIKE 'NL-PV26-GM0344%' ORDER BY level")
  expect_equal(pop$population[pop$level == "municipality"], 150000); expect_equal(unique(as.character(pop$ref_date)), "2025-01-01")
})

test_that("Utrecht: the population step on a kept territory reads the layer without rebuilding the boundaries, and a newer wijken file rebuilds them once", {
  load_fixtures()
  if (exists("utrecht", envir = NL_CITY_STATE)) rm("utrecht", envir = NL_CITY_STATE)                   # as in a later run, where the territory step was skipped
  msgs <- step_messages(with_db(function(con) { DBI::dbExecute(con, "DELETE FROM ref_population WHERE territory_id LIKE 'NL-PV26-GM0344%'"); CITY_MODULES$utrecht$population(con) }))
  expect_false(any(grepl("Territory saved", msgs))); expect_true(any(grepl("population: 3 wijken, CBS Wijk- en buurtkaart 2025 \\(150,000 residents\\)", msgs)))
  expect_equal(db_query("SELECT sum(population) AS p, count(*) AS n FROM ref_population WHERE territory_id LIKE 'NL-PV26-GM0344-%'"), data.frame(p = 150000, n = 3L))
  expect_true(any(grepl("territory kept \\(3 wijken\\)", step_messages(with_db(function(con) CITY_MODULES$utrecht$territory(con))))))
  f1 <- file.path(nl_city_dir("utrecht"), "20260101_wijken_2025_p01.geojson"); f2 <- file.path(nl_city_dir("utrecht"), "20260102_wijken_2025_p01.geojson")
  file.copy(f1, f2); Sys.setFileTime(f2, file.mtime(CITY_MODULES$utrecht$gpkg) + 1); on.exit(unlink(f2), add = TRUE)   # a new edition arrives after the boundaries were saved
  msgs <- step_messages(with_db(function(con) CITY_MODULES$utrecht$territory(con)))
  expect_true(any(grepl("newer than the saved boundaries", msgs))); expect_true(any(grepl("Territory saved: 1 districts and 3 neighbourhoods", msgs)))
  expect_true(any(grepl("territory kept", step_messages(with_db(function(con) CITY_MODULES$utrecht$territory(con))))))   # and kept again once rebuilt
})

test_that("the registers by year: one per city and year (the first of the year), and a sum only when every part is there", {
  load_fixtures()
  y <- db_query("SELECT level, territory_id, year, population, ref_date FROM mart_population_years WHERE territory_id LIKE 'CH-ZH-261%' ORDER BY year, level, territory_id")
  expect_equal(nrow(y), 28)                                                                # 12 Kreise, their district and the city, in 2024 and in 2025
  expect_equal(y$population[y$level == "municipality"], c(318000, 324000))
  expect_equal(y$population[y$level == "district"], c(318000, 324000))
  expect_equal(unique(as.character(y$ref_date)), c("2024-01-01", "2025-01-01"))
  expect_equal(db_query("SELECT year FROM mart_population_years WHERE level = 'municipality' AND territory_id = 'CA-35-TORONTO' ORDER BY 1")$year, 2024:2025)   # 2025 given by the portal, 2024 implied by its rates
  kreise <- db_query("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE 'CH-ZH-261%' ORDER BY 1")$territory_id
  with_db(function(con) {
    on.exit({ DBI::dbExecute(con, "DELETE FROM ref_population WHERE territory_id LIKE 'CH-ZH-261%' AND ref_date IN (DATE '2023-01-01', DATE '2025-06-01')"); quiet(build_marts(con)) }, add = TRUE)
    # a second register inside 2025 (a city that publishes one a month) and a register of 2023 that lacks a Kreis
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = kreise, ref_date = as.Date("2025-06-01"), population = 30000L, file = "test"))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = kreise[-12], ref_date = as.Date("2023-01-01"), population = 10000L, file = "test"))
    quiet(build_marts(con))
    q <- function(sql) DBI::dbGetQuery(con, sql)
    expect_equal(q("SELECT population FROM mart_population WHERE level = 'municipality' AND territory_id LIKE 'CH-ZH-261%'")$population, 360000)                       # "the register" of the city: the latest
    expect_equal(q("SELECT population FROM mart_population_years WHERE level = 'municipality' AND territory_id LIKE 'CH-ZH-261%' AND year = 2025")$population, 324000)  # the register of 2025: its first
    y23 <- q("SELECT level, count(*) AS n FROM mart_population_years WHERE territory_id LIKE 'CH-ZH-261%' AND year = 2023 GROUP BY 1")
    expect_equal(setNames(as.numeric(y23$n), y23$level), c(neighbourhood = 11))            # 11 Kreise, and no district or city: a short sum is not a population
  })
  expect_equal(db_query("SELECT count(*) AS n FROM mart_population_years WHERE territory_id LIKE 'CH-ZH-261%'")$n, 28)   # and as before once the extra registers are gone
})

test_that("Berlin, Lima, Mexico City and the French cities keep the residents of every year, not the latest only", {
  load_fixtures()
  paris <- CITY_MODULES$paris; par_d <- paste0(paris$id, "-D00"); ber_d <- paste0(ID_BERLIN, "-D01"); this_year <- as.integer(format(Sys.Date(), "%Y"))
  terr <- data.frame(
    territory_id = c(ID_BERLIN, ber_d, paste0(ber_d, "-B", c("010111", "010112")), ID_LIMA, lima_district_id("1501"), lima_neighbourhood_id(c("150101", "150102")),
                     ID_CDMX, cdmx_district_id(c("002", "003")), paris$id, par_d, paste0(par_d, "-B", c("75101", "75102"))),
    level = c("municipality", "district", "neighbourhood", "neighbourhood", "municipality", "district", "neighbourhood", "neighbourhood",
              "municipality", "district", "district", "municipality", "district", "neighbourhood", "neighbourhood"),
    parent_id = c("DE", ID_BERLIN, ber_d, ber_d, "PE", ID_LIMA, rep(lima_district_id("1501"), 2), "MX", ID_CDMX, ID_CDMX, "FR", paris$id, par_d, par_d),
    official_code = c("11", "01", "010111", "010112", "1501", "1501", "150101", "150102", "09", "002", "003", "75056", "00", "75101", "75102"), stringsAsFactors = FALSE)
  terr$country <- substr(terr$territory_id, 1, 2); terr$name <- terr$territory_id
  like <- paste(sprintf("territory_id LIKE '%s%%'", c(ID_BERLIN, ID_LIMA, ID_CDMX, paris$id)), collapse = " OR ")
  by_date <- function(con, id) DBI::dbGetQuery(con, sprintf("SELECT ref_date, sum(population) AS p, count(*) AS n FROM ref_population WHERE territory_id LIKE '%s-%%' GROUP BY 1 ORDER BY 1", id))
  with_db(function(con) {
    on.exit({ for (tb in c("dim_territory", "ref_population")) DBI::dbExecute(con, paste0("DELETE FROM ", tb, " WHERE ", like))
              DBI::dbExecute(con, "DELETE FROM ref_country_population WHERE country IN ('PE', 'MX')")
              BERLIN_STATE$pop <- NULL; if (exists("paris_pop", envir = FR_CITY_STATE)) rm("paris_pop", envir = FR_CITY_STATE)
              quiet(build_marts(con)) }, add = TRUE)
    DBI::dbAppendTable(con, "dim_territory", terr)
    # Berlin: the residents implied by the atlas, year by year
    BERLIN_STATE$pop <- data.frame(key = rep(c("010111", "010112"), 2), year = rep(2023:2024, each = 2), population = c(1000, 2000, 1100, 2100))
    quiet(berlin_population(con)); r <- by_date(con, ID_BERLIN)
    expect_equal(as.character(r$ref_date), c("2023-12-31", "2024-12-31")); expect_equal(r$p, c(3000, 3200)); expect_equal(as.numeric(r$n), c(2, 2))
    # Lima: the projections up to this year (next year's is a projection of the future: left out)
    DBI::dbAppendTable(con, "ref_country_population", data.frame(country = "PE", territory_id = rep(c("PE-15-150101", "PE-15-150102"), 3), level = "municipality",
                                                                 year = rep(c(this_year - 1L, this_year, this_year + 1L), each = 2), population = c(500, 600, 510, 610, 520, 620)))
    quiet(lima_population(con)); r <- by_date(con, ID_LIMA)
    expect_equal(as.character(r$ref_date), sprintf("%d-01-01", c(this_year - 1L, this_year))); expect_equal(r$p, c(1100, 1120))
    # Mexico City: the alcaldías of every year the country module holds (district rows: colonias have no population)
    DBI::dbAppendTable(con, "ref_country_population", data.frame(country = "MX", territory_id = rep(c("MX-09-002", "MX-09-003"), 2), level = "municipality",
                                                                 year = rep(c(2020L, this_year), each = 2), population = c(400, 300, 410, 310)))
    quiet(cdmx_population(con)); r <- by_date(con, ID_CDMX)
    expect_equal(as.character(r$ref_date), sprintf("%d-07-01", c(2020L, this_year))); expect_equal(r$p, c(700, 720))
    # Paris: the population the SSMSI file sets beside each year
    assign("paris_pop", data.frame(code = rep(c("75101", "75102"), 2), year = rep(2022:2023, each = 2), pop = c(16000, 21000, 15900, 20900)), envir = FR_CITY_STATE)
    quiet(fr_city_population(paris, con)); r <- by_date(con, paris$id)
    expect_equal(as.character(r$ref_date), c("2022-01-01", "2023-01-01")); expect_equal(r$p, c(37000, 36800))
    # and the yearly table adds them up: by district only in Mexico City, by neighbourhood elsewhere
    quiet(build_marts(con))
    m <- DBI::dbGetQuery(con, paste0("SELECT territory_id, level, year, population FROM mart_population_years WHERE ", like, " ORDER BY territory_id, year"))
    expect_equal(m$population[m$territory_id == ID_CDMX], c(700, 720)); expect_equal(sum(m$level == "district" & startsWith(m$territory_id, ID_CDMX)), 4)
    expect_equal(m$population[m$territory_id == ID_BERLIN], c(3000, 3200)); expect_equal(m$population[m$territory_id == paris$id], c(37000, 36800))
    expect_equal(m$year[m$territory_id == ID_LIMA], c(this_year - 1L, this_year))
  })
})

test_that("Barcelona: the yearly registers from the first year of the incidents are wanted, and a year that cannot be read is left out", {
  load_fixtures()
  nm <- c("2026_pad_mdbas_sexe.csv", "2025_pad_mdbas_sexe.csv", "2019_pad_mdbas_sexe.csv", "2018_pad_mdbas_sexe.csv", "notes.csv")
  expect_equal(nm[bcn_census_years(nm, 2019)], nm[1:3])
  expect_equal(nm[bcn_census_years(nm, NULL)], nm[1])                                      # no first year known: the newest only
  expect_equal(nm[bcn_census_years(nm, 2030)], nm[1])                                      # a first year after the newest file: the newest only
  dir <- CONFIG$sources$bcn_census$dir_raw; dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  d01 <- paste0(ID_BARCELONA, "-D01"); nb <- paste0(d01, "-B00", 1:2)
  reg <- function(year, values) data.frame(Data_Referencia = sprintf("%d-01-01", year), Codi_Districte = 1, Nom_Districte = "Ciutat Vella", Codi_Barri = rep(1:2, each = 2),
                                           Nom_Barri = rep(c("el Raval", "el Gotic"), each = 2), Valor = values, SEXE = rep(1:2, 2))
  readr::write_csv(reg(2026, c(100, 110, 200, 210)), file.path(dir, "2026_pad_mdbas_sexe.csv"))
  readr::write_csv(reg(2025, c(90, 100, 190, 200)), file.path(dir, "2025_pad_mdbas_sexe.csv"))
  readr::write_csv(data.frame(Any = 2024, Barri = "x", Poblacio = 1), file.path(dir, "2024_pad_mdbas_sexe.csv"))      # a year with another structure
  on.exit(unlink(file.path(dir, sprintf("%d_pad_mdbas_sexe.csv", 2024:2026))), add = TRUE)
  msgs <- step_messages(with_db(function(con) {
    on.exit(for (tb in c("dim_territory", "ref_population")) DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE territory_id LIKE '%s%%'", tb, ID_BARCELONA)), add = TRUE)
    DBI::dbAppendTable(con, "dim_territory", data.frame(territory_id = c(ID_BARCELONA, d01, nb), level = c("municipality", "district", "neighbourhood", "neighbourhood"),
                                                        parent_id = c("ES-09-08", ID_BARCELONA, d01, d01), country = "ES", official_code = c("08019", "01", "001", "002"),
                                                        name = c("Barcelona", "Ciutat Vella", "el Raval", "el Gotic"), stringsAsFactors = FALSE))
    bcn_census_ingest(con)
    r <- DBI::dbGetQuery(con, sprintf("SELECT ref_date, sum(population) AS p FROM ref_population WHERE territory_id LIKE '%s-%%' GROUP BY 1 ORDER BY 1", ID_BARCELONA))
    expect_equal(as.character(r$ref_date), c("2025-01-01", "2026-01-01")); expect_equal(r$p, c(580, 620))
  }))
  expect_true(any(grepl("^x 2024_pad_mdbas_sexe.csv: missing columns", msgs)))
  expect_true(any(grepl("^\\+ 2026_pad_mdbas_sexe.csv", msgs))); expect_true(any(grepl("^\\+ 2025_pad_mdbas_sexe.csv", msgs)))
})

# A yearly file that its publisher cut short (Barcelona, 2024 and 2025: 30,000 lines exactly, decision P-056) -----------
test_that("a file cut by its publisher: a round thousand of lines written code by code, and its last code is the incomplete one", {
  code <- rep(c("400", "500", "510"), c(400, 400, 199))
  expect_equal(bcn_incidents_cut(code), "510")                                             # 999 rows + the header = 1,000 lines
  expect_true(is.na(bcn_incidents_cut(code[-1])))                                          # 999 lines: nothing says it is cut
  expect_true(is.na(bcn_incidents_cut(sample(code))))                                      # a round thousand, but the codes are mixed: a complete file of the old kind
  expect_equal(bcn_incidents_cut(rep(c("510", "400", "500"), c(400, 400, 199))), "500")    # written code by code in another order: still its last block
  expect_true(is.na(bcn_incidents_cut(character(0)))); expect_true(is.na(bcn_incidents_cut("400", n_rows = 999)))
})

test_that("Barcelona: the incomplete code of a cut year is not shown, the types the year lacks are listed, and a complete file ends it", {
  load_fixtures()
  dir <- CONFIG$sources$bcn_incidents$dir_raw; dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  grid <- function(year, codes) { g <- expand.grid(Mes_any = 1:12, Codi_barri = 1:3, Codi_districte = 1:10, Codi_Incident = codes, stringsAsFactors = FALSE)
    data.frame(Codi_Incident = g$Codi_Incident, Descripcio_Incident = "x", Codi_districte = sprintf("%02d", g$Codi_districte), Nom_districte = "d", Codi_barri = g$Codi_districte * 10 + g$Codi_barri,
               Nom_barri = "b", NK_Any = year, Mes_any = g$Mes_any, Nom_mes = "m", Numero_incidents_GUB = 2L, stringsAsFactors = FALSE) }
  whole <- grid(2023, c("400", "500", "510", "610"))                                       # 1,440 rows: every code in the ten districts
  cut <- grid(2024, c("400", "500", "510", "610"))[1:999, ]                                # the same table stopped at line 1,000: inside code 510, in district 08
  files <- file.path(dir, c("20990101_gub_2023.csv", "20990101_gub_2024.csv", "20990102_gub_2024.csv"))
  readr::write_csv(whole[sample(nrow(whole)), ], files[1]); readr::write_csv(cut, files[2])
  d <- bcn_incidents_read(files[2])
  expect_equal(attr(d, "cut_code"), "510"); expect_true(attr(d, "cut_sorted")); expect_equal(attr(d, "cut_lines"), 1000L)
  expect_null(attr(bcn_incidents_read(files[1]), "cut_code"))
  with_db(function(con) {
    on.exit({ for (tb in c("stg_bcn_incidents", "stg_bcn_incidents_cut")) DBI::dbExecute(con, paste("DELETE FROM", tb))
              DBI::dbExecute(con, "DELETE FROM ingest_log WHERE source = 'bcn_incidents'"); DBI::dbExecute(con, "DELETE FROM stg_city_gaps WHERE city = 'barcelona'"); unlink(files) }, add = TRUE)
    q <- function(sql) DBI::dbGetQuery(con, sql)
    said <- step_messages(bcn_incidents_ingest(con))
    expect_true(any(grepl("20990101_gub_2024.csv is cut by the publisher: 1,000 lines exactly, written code by code and ending inside code 510", said)))
    expect_false(any(grepl("gub_2023.csv is cut", said)))
    k <- q("SELECT year, last_code, sorted, lines FROM stg_bcn_incidents_cut"); expect_equal(k$year, 2024L); expect_equal(k$last_code, "510"); expect_true(k$sorted); expect_equal(k$lines, 1000L)
    expect_equal(q("SELECT count(*) AS n FROM stg_bcn_incidents WHERE year(month) = 2024")$n, 999)       # the table keeps what the file says
    seen <- q("SELECT year(month) AS y, code, count(DISTINCT district_id) AS districts FROM v_bcn_incidents GROUP BY ALL ORDER BY 1, 2")
    expect_equal(seen$code[seen$y == 2023], c("400", "500", "510", "610")); expect_true(all(seen$districts[seen$y == 2023] == 10))
    expect_equal(seen$code[seen$y == 2024], c("400", "500"))                               # 510 reached eight districts only: not shown; 610 is not in the file
    g <- bcn_incidents_gaps(con)
    expect_true(all(c("510", "610", "670") %in% g$event_type)); expect_false(any(c("400", "500") %in% g$event_type))   # the last code and the codes after it, whether or not another year holds them
    expect_equal(unique(as.character(g$first_month)), "2024-01-01"); expect_equal(unique(as.character(g$last_month)), "2024-12-01"); expect_equal(unique(g$reason), "cut_file")
    expect_equal(bcn_incidents_gaps(con)$event_type, sort(g$event_type, method = "radix"))
    said <- step_messages(bcn_incidents_ingest(con)); expect_true(all(grepl("already loaded", said)))   # nothing is read twice
    # the publisher puts the whole year up: the newer copy is read, the year is no longer cut and nothing is missing
    readr::write_csv(grid(2024, c("400", "500", "510", "610"))[sample(1440), ], files[3])
    quiet(bcn_incidents_ingest(con))
    expect_equal(nrow(q("SELECT * FROM stg_bcn_incidents_cut")), 0)
    expect_equal(q("SELECT DISTINCT code FROM v_bcn_incidents WHERE year(month) = 2024 ORDER BY 1")$code, c("400", "500", "510", "610"))
    expect_equal(nrow(bcn_incidents_gaps(con)), 0)
    expect_equal(quiet(publish_city_gaps(con, "barcelona", g)), nrow(g)); expect_equal(quiet(publish_city_gaps(con, "barcelona", bcn_incidents_gaps(con))), 0)
    expect_equal(q("SELECT count(*) AS n FROM stg_city_gaps WHERE city = 'barcelona'")$n, 0)             # an empty table clears the city's gaps
  })
})

test_that("the gaps of a city reach the app for the types it shows only (no sensitive type, no hidden category)", {
  load_fixtures()
  ty <- db_query("SELECT event_type, block, sensitive FROM ref_city_event_types WHERE city = 'zurich'")
  shown <- ty$event_type[!ty$sensitive & ty$block == "public_safety"][1:2]; hidden <- ty$event_type[ty$sensitive][1]
  expect_false(anyNA(c(shown, hidden)))
  gaps <- data.frame(event_type = c(shown, hidden, "not a type of this city"), first_month = as.Date("2025-01-01"), last_month = as.Date("2025-12-01"), reason = "cut_file", file = "test.csv", stringsAsFactors = FALSE)
  with_db(function(con) {
    on.exit({ quiet(publish_city_gaps(con, "zurich", NULL)); quiet(build_marts(con)) }, add = TRUE)
    said <- step_messages(publish_city_gaps(con, "zurich", gaps)); expect_true(any(grepl("zurich: 4 types missing from the source in part of the series \\(01/2025 to 12/2025\\)", said)))
    quiet(build_marts(con))
    m <- DBI::dbGetQuery(con, "SELECT city, block, event_type, first_month, last_month, reason FROM mart_city_gaps ORDER BY event_type")
    expect_equal(nrow(m), 2); expect_setequal(m$event_type, shown); expect_equal(unique(m$block), "public_safety"); expect_equal(unique(m$city), "zurich")
  })
  with_db(function(con) quiet(publish_city_gaps(con, "zurich", gaps)))                     # the checks of option 15 name it too
  out <- quiet(check_countries(save = FALSE)); ck <- out[out$check == "types_missing_from_source", ]
  expect_equal(ck$country, "ZRH"); expect_equal(ck$n, 4L); expect_equal(ck$severity, "warning"); expect_true(grepl("01/2025 to 12/2025 (cut file)", ck$detail, fixed = TRUE))
  with_db(function(con) quiet(publish_city_gaps(con, "zurich", NULL)))
  expect_false("types_missing_from_source" %in% quiet(check_countries(save = FALSE))$check)
  expect_equal(db_query("SELECT count(*) AS n FROM mart_city_gaps")$n, 0)                  # and none once the city says it has none
})

test_that("the status and the checks run on the city tables", {
  load_fixtures()
  expect_no_error(quiet(with_db(function(con) city_status(con))))
  out <- quiet(check_countries(save = FALSE))
  expect_false(any(out$country %in% c("ZRH", "TOR", "BUE", "UTR") & out$severity == "error"))
  yr <- out[out$check == "years_without_register", ]
  expect_false("ZRH" %in% yr$country)                                                      # Zürich has the residents of both its years
  expect_equal(yr$n[yr$country == "BUE"], 2L)                                              # Buenos Aires: 2024 and 2025 on the census of 2022
  expect_true(grepl("figures of 2024-2025 stand on the nearest register (registers of 2022)", yr$detail[yr$country == "BUE"], fixed = TRUE))
})
