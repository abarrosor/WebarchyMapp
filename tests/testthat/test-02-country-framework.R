# The country framework on the synthetic country XT (helper-fixtures.R) ----------------------------------------
use_test_project()

test_that("a registered module loads its territory, figures, types and population through update_countries()", {
  load_fixtures()
  terr <- db_query("SELECT territory_id, level, parent_id, name FROM dim_territory WHERE country = 'XT' ORDER BY territory_id")
  expect_equal(terr$territory_id, c("XT", "XT-R1", "XT-R2")); expect_equal(terr$level, c("country", "region", "region")); expect_equal(terr$name[2], "Region One")
  stg <- db_query("SELECT count(*) AS n FROM stg_country_crime WHERE country = 'XT'")$n
  expect_equal(stg, nrow(XT_CRIME))
  types <- db_query("SELECT source_code, offence_type_id, sensitive FROM ref_country_offence_types WHERE country = 'XT' ORDER BY source_code")
  expect_equal(types$offence_type_id, c("a", "a", "s", "total")); expect_equal(types$sensitive, c(FALSE, FALSE, TRUE, FALSE))
  pop <- db_query("SELECT territory_id, population FROM mart_country_population WHERE country = 'XT' ORDER BY territory_id")
  expect_equal(pop$population, c(300000, 200000, 100000))
  meta <- db_query("SELECT * FROM mart_country_meta WHERE country = 'XT'")
  expect_equal(nrow(meta), 1); expect_false(meta$financial_year)                    # calendar quarters (the GB modules set TRUE)
})

test_that("the quarterly figure of a type is computed on the sum of its codes (P-037 C2), with no row lost", {
  load_fixtures()
  a <- db_query("SELECT quarter, cumulative, quarterly FROM mart_country_crime WHERE country = 'XT' AND territory_id = 'XT-R1' AND offence_type_id = 'a' ORDER BY quarter")
  expect_equal(a$quarter, 1:4)
  expect_equal(a$cumulative, c(10, 25, 42, 60))                 # A1 + A2 (A2 starts in quarter 2)
  expect_equal(a$quarterly, c(10, 15, 17, 18))                  # differences of the SUMMED cumulatives
  tot <- db_query("SELECT quarterly FROM mart_country_crime WHERE country = 'XT' AND territory_id = 'XT' AND offence_type_id = 'total' ORDER BY quarter")$quarterly
  expect_equal(tot, c(15, 21, 22, 24))
  # a region with one code only is unchanged by the fix
  r2 <- db_query("SELECT quarterly FROM mart_country_crime WHERE country = 'XT' AND territory_id = 'XT-R2' AND offence_type_id = 'a' ORDER BY quarter")$quarterly
  expect_equal(r2, c(4, 4, 4, 4))
})

test_that("sensitive types reach the mart flagged, inside the total, so that the app can keep them out of the breakdown", {
  load_fixtures()
  s <- db_query("SELECT DISTINCT offence_type_id, sensitive, included_in FROM mart_country_crime WHERE country = 'XT' ORDER BY offence_type_id")
  expect_equal(s$sensitive[s$offence_type_id == "s"], TRUE)
  expect_equal(s$sensitive[s$offence_type_id == "a"], FALSE)
  expect_equal(s$included_in[s$offence_type_id == "s"], "total")
})

test_that("the consistency checks run on the test database and raise nothing about the synthetic country", {
  load_fixtures()
  out <- quiet(check_countries(save = FALSE))
  expect_s3_class(out, "data.frame")
  expect_true(all(c("country", "check", "severity") %in% names(out)))
  expect_false(any(out$country == "XT" & out$severity == "error"))
})

test_that("the Eurostat population of the years before the main table comes in only where both tables agree on the years they share", {
  tab <- function(geo, years, value) { d <- data.frame(geo = rep(geo, each = length(years)), geo_label = "x", time = rep(as.character(years), length(geo)), value = value, stringsAsFactors = FALSE)
    d$year <- as.integer(d$time); d$cntr <- substr(d$geo, 1, 2); d }
  main <- tab(c("AA", "AA11", "BB", "BB11"), 2014:2015, c(1000, 1010,  400, 410,  2000, 2020,  900, 905))
  before <- tab(c("AA", "AA11", "AA99", "BB", "BB11"), 2012:2014, c(980, 990, 1000,  380, 390, 401,  50, 50, 50,  1900, 1950, 2600,  880, 890, 700))
  out <- er_merge_population(main, before)
  expect_equal(sort(unique(out$year[out$cntr == "AA"])), 2012:2015)         # AA: 2014 is the same in both (401 against 400 is within 0.5 %): 2012 and 2013 come in
  expect_equal(out$value[out$geo == "AA11" & out$year == 2012], 380)
  expect_false("AA99" %in% out$geo)                                          # a region the main table does not know (an older NUTS code) stays out
  expect_equal(sort(unique(out$year[out$cntr == "BB"])), 2014:2015)          # BB: 2014 differs (2600 against 2000): nothing is taken, its early years stay marked in the app
  expect_equal(attr(out, "added"), "AA 2012-2013"); expect_equal(attr(out, "refused"), "BB")
  expect_equal(anyDuplicated(paste(out$geo, out$year)), 0)
  expect_equal(out$value[out$geo == "AA" & out$year == 2014], 1000)          # the shared year keeps the figure of the main table
  expect_equal(nrow(er_merge_population(main, before[0, ])), nrow(main)); expect_equal(nrow(er_merge_population(main, NULL)), nrow(main))
  expect_equal(EUROSTAT_REGIONS$population_before, "demo_r_pjanaggr3")
})

# A minimal JSON-stat 2.0 file (geo x time), as the Eurostat API answers: values in row-major order
write_jsonstat <- function(path, geo, years, values) {
  idx <- function(k) as.list(stats::setNames(seq_along(k) - 1L, k)); lab <- function(k) as.list(stats::setNames(k, k)); yrs <- as.character(years)
  js <- list(version = "2.0", class = "dataset", id = list("geo", "time"), size = list(length(geo), length(yrs)),
             dimension = list(geo = list(category = list(index = idx(geo), label = lab(geo))), time = list(category = list(index = idx(yrs), label = lab(yrs)))),
             value = as.list(values))
  writeLines(jsonlite::toJSON(js, auto_unbox = TRUE), path)
}

test_that("the Eurostat population is read again when a newer copy of either table arrives during the session", {
  use_test_project()
  dir <- EUROSTAT_REGIONS$dir_raw; dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  main <- paste0(EUROSTAT_REGIONS$population, "_T.json"); before <- paste0(EUROSTAT_REGIONS$population_before, "_T.json")
  expect_length(list.files(dir, "_T\\.json$"), 0)                                          # the test copy starts without Eurostat files
  f1 <- file.path(dir, paste0("20260101_", main)); f2 <- file.path(dir, paste0("20260201_", before)); f3 <- file.path(dir, paste0("20260301_", main))
  clear <- function() rm(list = ls(ER_STATE), envir = ER_STATE)
  on.exit({ unlink(c(f1, f2, f3)); clear() }, add = TRUE); clear()
  write_jsonstat(f1, c("AT11", "AT12"), 2014:2015, c(100, 110, 200, 210))
  p <- er_population(); expect_equal(sort(unique(p$year)), 2014:2015); expect_equal(nrow(p), 4)
  # the table of the earlier years arrives later in the same run (another module had read the population first): it is read
  write_jsonstat(f2, c("AT11", "AT12"), 2012:2014, c(90, 95, 100, 190, 195, 200))
  p <- quiet(er_population()); expect_equal(sort(unique(p$year)), 2012:2015); expect_equal(p$value[p$geo == "AT11" & p$year == 2012], 90)
  expect_identical(quiet(er_population()), p)                                               # nothing new: the copy kept in memory
  write_jsonstat(f3, c("AT11", "AT12"), 2014:2016, c(100, 110, 120, 200, 210, 220))         # a newer copy of the main table
  Sys.setFileTime(f3, Sys.time() + 60)
  p <- quiet(er_population()); expect_equal(max(p$year), 2016); expect_equal(min(p$year), 2012)
  expect_equal(basename(er_files()[2:3]), basename(c(f3, f2)))                              # what the countries watch: the newest copy of each
})

test_that("a new copy of a shared file is a change for the countries that read it; their own empty folder alone would keep them for ever", {
  load_fixtures()
  expect_true(is.function(COUNTRY_MODULES$AT$watch)); expect_true(is.function(COUNTRY_MODULES$CZ$watch)); expect_null(COUNTRY_MODULES$XT$watch)
  w <- tempfile(fileext = ".json"); writeLines("{}", w); on.exit(unlink(w), add = TRUE)
  with_db(function(con) {
    m <- COUNTRY_MODULES$XT; base <- country_unchanged(con, m)
    expect_true(base)                                                                       # loaded, and nothing touched since
    m$watch <- function() w
    Sys.setFileTime(w, Sys.time() + 3600); expect_false(country_unchanged(con, m))          # a shared file newer than the last load: load again
    Sys.setFileTime(w, as.POSIXct("2000-01-01 12:00:00", tz = "UTC")); expect_true(country_unchanged(con, m))   # an old one changes nothing
    m$watch <- function() c(NA_character_, "no/such/file.json"); expect_true(country_unchanged(con, m))         # a file not downloaded yet
    m$watch <- function() stop("not there"); expect_true(country_unchanged(con, m))
  })
})

test_that("a country is loaded again when a city is one of its municipalities (Paris in France) or a node of its own (Lima in Peru)", {
  load_fixtures()
  old <- CITY_MODULES
  cm <- old; cm$xta <- list(id = "XT-R1-M1", country = "XT", key = "xta"); cm$xtb <- list(id = "XT-OWN", country = "XT", key = "xtb")
  assign("CITY_MODULES", cm, envir = globalenv())
  tbl <- data.frame(territory_id = c("XT-R1", "XT-R2", "XT-R1-M1", "XT-R1-M2"), level = c("region", "region", "municipality", "municipality"),
                    parent_id = c("XT", "XT", "XT-R1", "XT-R1"), official_code = c("R1", "R2", "M1", "M2"), name = c("Region One", "Region Two", "City A", "Town"), stringsAsFactors = FALSE)
  # what the city framework writes: the city's own row, its districts and its neighbourhoods
  city <- data.frame(territory_id = c("XT-R1-M1", "XT-R1-M1-D01", "XT-R1-M1-D01-B01", "XT-OWN", "XT-OWN-D01", "XT-OWN-D01-B01"),
                     level = rep(c("municipality", "district", "neighbourhood"), 2), parent_id = c("XT-R1", "XT-R1-M1", "XT-R1-M1-D01", "XT", "XT-OWN", "XT-OWN-D01"),
                     country = "XT", official_code = c("M1", "01", "01", "OWN", "01", "01"), name = c("City A (as the city wrote it)", "District", "Neighbourhood", "Metropolitan area", "District", "Neighbourhood"), stringsAsFactors = FALSE)
  with_db(function(con) {
    on.exit({ assign("CITY_MODULES", old, envir = globalenv())
              DBI::dbExecute(con, "DELETE FROM dim_territory WHERE territory_id LIKE 'XT-R1-M%' OR territory_id LIKE 'XT-OWN%'")
              quiet(COUNTRY_MODULES$XT$territory(con)) }, add = TRUE)
    DBI::dbAppendTable(con, "dim_territory", city)
    xt <- function() DBI::dbGetQuery(con, "SELECT territory_id, level, name FROM dim_territory WHERE country = 'XT' ORDER BY territory_id")
    for (i in 1:2) {                                                                        # the first load over the city's rows, and a reload
      expect_no_error(quiet(save_country_territory(con, "XT", tbl, scheme = "test")))
      d <- xt()
      expect_equal(d$territory_id, c("XT", "XT-OWN", "XT-OWN-D01", "XT-OWN-D01-B01", "XT-R1", "XT-R1-M1", "XT-R1-M1-D01", "XT-R1-M1-D01-B01", "XT-R1-M2", "XT-R2"))
      expect_equal(d$name[d$territory_id == "XT-R1-M1"], "City A")                          # the municipality that is a city: once, as the country sees it
      expect_equal(d$name[d$territory_id == "XT-OWN"], "Metropolitan area")                 # a node of its own: left as the city wrote it
    }
    # a territory that stays in the table (here a neighbourhood of the city) cannot be written again: said with its name, not "transaction is aborted"
    again <- rbind(tbl, data.frame(territory_id = "XT-R1-M1-D01-B01", level = "municipality", parent_id = "XT-R1", official_code = "X", name = "Clash", stringsAsFactors = FALSE))
    expect_error(quiet(save_country_territory(con, "XT", again, scheme = "test")), "cannot be written twice: XT-R1-M1-D01-B01")
    expect_equal(nrow(xt()), 10)                                                            # and nothing was changed by the refused load
  })
  expect_equal(db_query("SELECT territory_id FROM dim_territory WHERE country = 'XT' ORDER BY 1")$territory_id, c("XT", "XT-R1", "XT-R2"))   # the test country as the other tests expect it
})

# Two readers that the first full run on real files showed to be wrong (16 November 2026, decision P-055) ----------------
test_that("Costa Rica: a nationality written with a comma does not move the record to another cantón, and the file is named", {
  f <- file.path(tempdir(), "20990101_cr_policiales_2099.csv")
  usual <- sprintf("HURTO,POR DESCUIDO,2099-01-%02d,PERSONA,PEATON [PERSONA],Mayor de edad ,,COSTA RICA,SAN JOSE,SAN JOSE,CARMEN", 1:6)
  writeLines(c(usual,
    "HURTO,CON LLAVE,2099-04-14,PERSONA,TURISTA/EXTRANJERO [PERSONA],Mayor de edad ,,CONGO, REPUBLICA DEMOCRATICA DEL,PUNTARENAS,GARABITO,JACO",                # 12 fields
    "ROBO,BOQUETE,2099-07-08,EDIFICACION,HOTEL/MOTEL/PENSION [EDIFICACION],Desconocido,,COREA, REPUBLICA POPULAR, DEMOCRATICA DE,GUANACASTE,LIBERIA,LIBERIA", # 13 fields
    "ASALTO,ARMA BLANCA,2099-02-25,PERSONA,PEATON [PERSONA],Mayor de edad ,,COSTA RICA,HEREDIA,SAN PABLO,SAN PABLO, RINCON",                                    # a comma elsewhere: no provincia where it should be
    "ASALTO,ARREBATO,2099-03-01,PERSONA,PEATON [PERSONA],Mayor de edad ,,COSTA RICA,ALAJUELA,ALAJUELA"), f)                                                     # a field missing
  said <- character()
  expect_no_warning(d <- withCallingHandlers(cr_read(f), message = function(m) { said <<- c(said, conditionMessage(m)); invokeRestart("muffleMessage") }))
  expect_equal(nrow(d), 10)
  expect_equal(unlist(d[7, c("nacionalidad", "provincia", "canton", "distrito")], use.names = FALSE), c("CONGO, REPUBLICA DEMOCRATICA DEL", "PUNTARENAS", "GARABITO", "JACO"))
  expect_equal(unlist(d[8, c("nacionalidad", "provincia", "canton", "distrito")], use.names = FALSE), c("COREA, REPUBLICA POPULAR, DEMOCRATICA DE", "GUANACASTE", "LIBERIA", "LIBERIA"))
  expect_equal(d$delito[7:8], c("HURTO", "ROBO")); expect_equal(format(d$fecha[7:8]), c("2099-04-14", "2099-07-08"))
  mt <- cr_match(d$provincia, d$canton, cr_cantones())
  expect_equal(paste(mt$prov, mt$canton)[7:8], c("6 611", "5 501"))                        # Garabito and Liberia: before the fix, the cantón of Puntarenas and no place at all
  expect_true(all(is.na(d[9:10, c("provincia", "canton", "distrito")])))                   # rows that cannot be rebuilt: no place (national total only), never a wrong one
  expect_true(all(is.na(mt$prov[9:10])) && all(is.na(mt$canton[9:10])))
  expect_equal(unlist(d[1, c("provincia", "canton", "distrito")], use.names = FALSE), c("SAN JOSE", "SAN JOSE", "CARMEN"))   # the usual rows as they were
  expect_true(any(grepl("20990101_cr_policiales_2099.csv: 2 record\\(s\\) with a comma inside the nationality", said)))
  expect_true(any(grepl("1 row\\(s\\) with more fields than expected could not be rebuilt \\(rows 9\\)", said)))
  expect_true(any(grepl("1 row\\(s\\) with fewer fields than expected \\(rows 10\\)", said)))
  # a file whose very first line is one of those rows still has eleven columns
  writeLines(c("HURTO,CON LLAVE,2099-04-14,PERSONA,TURISTA/EXTRANJERO [PERSONA],Mayor de edad ,,CAIMAN, ISLAS,ALAJUELA,SAN CARLOS,FORTUNA", usual), f)
  d <- quiet(cr_read(f))
  expect_equal(ncol(d), 11); expect_equal(unlist(d[1, c("nacionalidad", "provincia", "canton", "distrito")], use.names = FALSE), c("CAIMAN, ISLAS", "ALAJUELA", "SAN CARLOS", "FORTUNA"))
  unlink(f)
})

test_that("Hungary: the wide table is put in long form without one warning per row", {
  m <- rbind(c("11.1.2.1. Registered crimes by county and region", rep("", 6)),
             c("Name of the territorial unit", "Level of the territorial unit", "2021", "2022", "2023", "2024", "2025"),
             c("Total number of crimes", rep("", 6)),
             c("Budapest", "capital, region", "36 280", "42 344", "43 019", "54 949", "43 910"),
             c("Pest", "county", "10 000", "11 000", "12 000", "13 000", "14 000"),
             c("Country total", "", "154 012", "167 774", "180 000", "190 000", "200 000"))
  colnames(m) <- paste0("X", 1:7)                                                          # as hu_read() gives it: every cell carries its column name
  expect_no_warning(d <- hu_standardise(m, hu_counties()))
  expect_equal(nrow(d), 15); expect_equal(unique(d$block), "total number of crimes"); expect_null(names(d$block))
  expect_equal(sort(unique(d$code)), c("BU", "HU", "PE"))
  expect_equal(d$n[d$code == "BU" & d$year == 2025], 43910); expect_equal(d$n[d$code == "HU" & d$year == 2021], 154012)
})
