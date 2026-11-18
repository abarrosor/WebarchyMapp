# Synthetic sources for the tests ---------------------------------------------------------------------------------
# A synthetic country ("XT") registered with the country framework like any module, and small files in the
# layout the Zürich, Toronto and Buenos Aires modules expect. Values are chosen so that the expected results
# can be written down in the tests (see test-country-framework.R and test-city-framework.R).

# --- A synthetic country: two regions, one source, quarterly cumulative figures, population ------------------------
XT_CRIME <- rbind(
  # offence type "a" is made of two codes: A1 has the four quarters, A2 starts in quarter 2 (a code that a
  # source publishes late): the mart must compute the quarter on the SUM of the codes (P-037, finding C2)
  data.frame(territory_id = "XT-R1", level = "region", offence_code = "A1", year = 2025L, quarter = 1:4, cumulative = c(10, 20, 30, 40)),
  data.frame(territory_id = "XT-R1", level = "region", offence_code = "A2", year = 2025L, quarter = 2:4, cumulative = c(5, 12, 20)),
  data.frame(territory_id = "XT-R2", level = "region", offence_code = "A1", year = 2025L, quarter = 1:4, cumulative = c(4, 8, 12, 16)),
  data.frame(territory_id = "XT-R1", level = "region", offence_code = "S1", year = 2025L, quarter = 1:4, cumulative = c(1, 2, 3, 4)),      # sensitive
  data.frame(territory_id = "XT-R2", level = "region", offence_code = "S1", year = 2025L, quarter = 1:4, cumulative = c(0, 1, 1, 2)),
  data.frame(territory_id = "XT-R1", level = "region", offence_code = "TOT", year = 2025L, quarter = 1:4, cumulative = c(11, 27, 45, 64)),
  data.frame(territory_id = "XT-R2", level = "region", offence_code = "TOT", year = 2025L, quarter = 1:4, cumulative = c(4, 9, 13, 18)),
  data.frame(territory_id = "XT", level = "country", offence_code = "TOT", year = 2025L, quarter = 1:4, cumulative = c(15, 36, 58, 82)),
  data.frame(territory_id = "XT", level = "country", offence_code = "A1", year = 2025L, quarter = 1:4, cumulative = c(14, 28, 42, 56)),
  data.frame(territory_id = "XT", level = "country", offence_code = "A2", year = 2025L, quarter = 2:4, cumulative = c(5, 12, 20)),
  data.frame(territory_id = "XT", level = "country", offence_code = "S1", year = 2025L, quarter = 1:4, cumulative = c(1, 3, 4, 6)))

xt_dir <- function() file.path(CONFIG$dir_raw, "country_xt")
xt_write_fixtures <- function() {
  dir.create(xt_dir(), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(XT_CRIME, file.path(xt_dir(), "crime.csv"), row.names = FALSE)
  utils::write.csv(data.frame(territory_id = c("XT", "XT-R1", "XT-R2"), level = c("country", "region", "region"), year = 2025L, population = c(300000, 200000, 100000)),
                   file.path(xt_dir(), "population.csv"), row.names = FALSE)
  writeLines(c("source_code,offence_type_id,name_local,name_en,included_in,sensitive",
               "TOT,total,Total,Total,,no",
               "A1,a,Type A (first code),Type A (first code),total,no",
               "A2,a,Type A (second code),Type A (second code),total,no",
               "S1,s,Sensitive type,Sensitive type,total,yes"), file.path(CONFIG$dir_ref, "xt_offence_types.csv"))
}

register_test_country <- function() {
  if (!is.null(COUNTRY_MODULES$XT)) return(invisible(COUNTRY_MODULES$XT))
  register_country(list(
    code = "XT", iso3 = "XTT", name_en = "Testland", name_local = "Testland", lang = "en",
    levels = c("region"), level_labels = list(region = c("Regions", "Regions", "Region", "Region")),
    source = list(name = "Synthetic test source", publisher = "WebarchyMapp tests", url = "", licence = "none", measure = "Synthetic cumulative figures"),
    population_source = "synthetic", boundaries_attribution = "",
    method = list(where = "where it happened", date = "quarter of recording", revisions = "none"),
    download = function() invisible(TRUE),
    territory = function(con) {
      save_country_territory(con, "XT", data.frame(territory_id = c("XT-R1", "XT-R2"), level = "region", parent_id = "XT", official_code = c("R1", "R2"), name = c("Region One", "Region Two")), scheme = "test")
    },
    ingest = function(con) {
      d <- utils::read.csv(file.path(xt_dir(), "crime.csv"), stringsAsFactors = FALSE); d$country <- "XT"; d$file <- "crime.csv"
      replace_country_rows(con, "stg_country_crime", "XT", d[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
      log_ingest(con, "country_XT", "crime.csv", NA, "2025", nrow(d))
    },
    population = function(con) {
      p <- utils::read.csv(file.path(xt_dir(), "population.csv"), stringsAsFactors = FALSE); p$country <- "XT"
      replace_country_rows(con, "ref_country_population", "XT", p[, c("country", "territory_id", "level", "year", "population")])
    }))
}

# --- Zürich: the PKS file (12 Kreise, one Gemeinde row to be ignored, an unknown title) and the WFS layer ------------
ZRH_TITLES <- c("Total StGB", "Total gegen Leib und Leben", "Total gegen das Vermögen", "Total gegen die Freiheit", "Total gegen die sexuelle Integrität", "Total gegen die Ehre und den Geheim- oder Privatbereich", "Total Neuer Titel")
zurich_write_fixtures <- function() {
  d1 <- file.path(CONFIG$dir_raw, "city_zurich", "pks"); d2 <- file.path(CONFIG$dir_raw, "city_zurich", "boundaries")
  dir.create(d1, recursive = TRUE, showWarnings = FALSE); dir.create(d2, recursive = TRUE, showWarnings = FALSE)
  rows <- list()
  for (y in 2024:2025) for (k in 1:12) for (i in seq_along(ZRH_TITLES)) {
    n <- if (i == 1) 1000 + k * 10 else 100 + k * 10 + i                        # deterministic: tests can recompute them
    rows[[length(rows) + 1]] <- data.frame(Ausgangsjahr = y, Gemeinde_BFS_Nr = 261, Gemeindename = "Zürich", Stadtkreis_BFS_Nr = 26100 + k, Stadtkreis_Name = paste("Kreis", k),
                                           Gesetz_Nummer = "311.0", Gesetz_Abk = "StGB", Reihenfolge_Haupttitel = i - 1, Haupttitel = ZRH_TITLES[i],
                                           Straftaten_total = n, Straftaten_vollendet = n, Straftaten_versucht = 0, Einwohner = 20000 + k * 1000 + (y - 2024) * 500, Häufigkeitszahl = 1)
  }
  rows[[length(rows) + 1]] <- data.frame(Ausgangsjahr = 2025, Gemeinde_BFS_Nr = 131, Gemeindename = "Adliswil", Stadtkreis_BFS_Nr = NA, Stadtkreis_Name = NA, Gesetz_Nummer = "311.0", Gesetz_Abk = "StGB",
                                         Reihenfolge_Haupttitel = 1, Haupttitel = "Total gegen Leib und Leben", Straftaten_total = 48, Straftaten_vollendet = 48, Straftaten_versucht = 0, Einwohner = 16052, Häufigkeitszahl = 3)
  readr::write_csv(do.call(rbind, rows), file.path(d1, "20260101_pks_gemeinden_stadtkreise.csv"), na = "")
  bowtie <- function(x0, y0, s = 0.02) sf::st_polygon(list(matrix(c(x0, y0, x0 + s, y0 + s, x0 + s, y0, x0, y0 + s, x0, y0), ncol = 2, byrow = TRUE)))   # a ring that crosses itself, as the live WFS has
  g <- sf::st_sf(bezeichnung = paste("Kreis", 1:12), name = as.numeric(1:12), geometry = sf::st_sfc(lapply(1:12, function(k) (if (k == 7) bowtie else square)(8.50 + (k - 1) %% 4 * 0.025, 47.35 + (k - 1) %/% 4 * 0.025)), crs = 4326))
  sf::st_write(g, file.path(d2, "20260101_stadtkreise.geojson"), quiet = TRUE, delete_dsn = TRUE)
}

# --- Toronto: the layer in two pages of 6 neighbourhoods, counts and rates 2024-2025, population 2025 ---------------
TOR_CATS <- c("ASSAULT", "AUTOTHEFT", "BIKETHEFT", "BREAKENTER", "HOMICIDE", "ROBBERY", "SHOOTING", "THEFTFROMMV", "THEFTOVER")
toronto_write_fixtures <- function() {
  d <- file.path(CONFIG$dir_raw, "city_toronto", "layer"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  page <- function(ids, file) {
    df <- data.frame(OBJECTID_1 = ids, AREA_NAME = paste("Hood", ids), HOOD_ID = ids)
    for (ct in TOR_CATS) for (y in 2024:2025) { pop <- 10000 + ids * 500; n <- if (ct == "HOMICIDE") ids %% 3 else 50 + ids * 7 + match(ct, TOR_CATS) + (y - 2024)
      df[[paste0(ct, "_", y)]] <- n; df[[paste0(ct, "_RATE_", y)]] <- round(n / pop * 1e5, 1) }
    df$POPULATION_2025 <- 10000 + ids * 500
    g <- sf::st_sf(df, geometry = sf::st_sfc(lapply(ids, function(k) square(-79.5 + (k - 1) %% 4 * 0.03, 43.65 + (k - 1) %/% 4 * 0.03, 0.025)), crs = 4326))
    sf::st_write(g, file, quiet = TRUE, delete_dsn = TRUE)
  }
  page(1:6, file.path(d, "20260101_crime_rates_p01.geojson")); page(7:12, file.path(d, "20260101_crime_rates_p02.geojson"))
}

# --- Buenos Aires: two yearly CSVs with the live columns (a NULL barrio, a NULL comuna) and the barrios layer -------
BUE_BARRIOS <- data.frame(nombre = c("Chacarita", "Villa Crespo", "Palermo", "Nuñez", "La Boca", "Agronomia"), comuna = c(15, 15, 14, 13, 4, 15))   # the data say "BOCA"
buenosaires_write_fixtures <- function() {
  d1 <- file.path(CONFIG$dir_raw, "city_buenosaires", "delitos"); d2 <- file.path(CONFIG$dir_raw, "city_buenosaires", "boundaries")
  dir.create(d1, recursive = TRUE, showWarnings = FALSE); dir.create(d2, recursive = TRUE, showWarnings = FALSE)
  mk <- function(year) {
    rows <- list(); id <- 0
    for (m in 1:12) for (b in seq_len(nrow(BUE_BARRIOS))) for (sub in c("Robo total", "Hurto total", "Homicidios dolosos", "Amenazas")) {
      id <- id + 1; n <- if (sub == "Homicidios dolosos") 1 else 10 + b
      rows[[length(rows) + 1]] <- data.frame(`id-mapa` = id, anio = year, mes = m, dia = 1, fecha = sprintf("%d-%02d-01", year, m), franja = 10, tipo = sub("s? .*$", "", sub), subtipo = sub,
                                             uso_arma = "NO", uso_moto = "NO", barrio = toupper(sub("^La ", "", BUE_BARRIOS$nombre[b])), comuna = BUE_BARRIOS$comuna[b], latitud = -34.6, longitud = -58.4, cantidad = n, check.names = FALSE)
    }
    rows[[length(rows) + 1]] <- data.frame(`id-mapa` = id + 1, anio = year, mes = 1, dia = 1, fecha = sprintf("%d-01-01", year), franja = 10, tipo = "Robo", subtipo = "Robo total", uso_arma = "NO", uso_moto = "NO",
                                           barrio = "NULL", comuna = "NULL", latitud = "NULL", longitud = "NULL", cantidad = 7, check.names = FALSE)     # no barrio, no comuna: city level
    rows[[length(rows) + 1]] <- data.frame(`id-mapa` = id + 2, anio = year, mes = 1, dia = 1, fecha = sprintf("%d-01-01", year), franja = 10, tipo = "Robo", subtipo = "Robo total", uso_arma = "NO", uso_moto = "NO",
                                           barrio = "NULL", comuna = 15, latitud = "NULL", longitud = "NULL", cantidad = 5, check.names = FALSE)          # no barrio: comuna level
    readr::write_csv(do.call(rbind, rows), file.path(d1, sprintf("20260101_delitos_%d.csv", year)), na = "")
  }
  mk(2024); mk(2025)
  d3 <- file.path(CONFIG$dir_raw, "city_buenosaires", "population"); dir.create(d3, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(data.frame(comuna = c(4, 13, 14, 15), poblacion = c(220000, 230000, 240000, 180000)), file.path(d3, "censo_2022_comunas.csv"), row.names = FALSE)
  g <- sf::st_sf(BUE_BARRIOS, geometry = sf::st_sfc(lapply(seq_len(nrow(BUE_BARRIOS)), function(k) square(-58.5 + (k - 1) %% 3 * 0.03, -34.65 + (k - 1) %/% 3 * 0.03, 0.025)), crs = 4326))
  sf::st_write(g, file.path(d2, "20260101_barrios.geojson"), quiet = TRUE, delete_dsn = TRUE)
}

# --- Utrecht (the Dutch generator): one wijken page of the whole country, one year of OData rows, the SoortMisdrijf
# metadata. Wijk w of Utrecht has 10 + w + t crimes of type t every month; WK034404 is in the data but not in the
# layer, WK034409 is a water-only part, WK036301 belongs to Amsterdam, "0.0.0" is the aggregate "Totaal misdrijven".
UTR_TYPES <- c("1.1.1 " = "1.1.1 Diefstal/inbraak woning", "1.2.1 " = "1.2.1 Diefstal uit/vanaf motorvoertuigen", "1.4.5 " = "1.4.5 Zedenmisdrijf", "0.0.0 " = "Totaal misdrijven")
utrecht_write_fixtures <- function() {
  d <- nl_city_dir("utrecht"); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  w <- data.frame(id = 1:5, wijkcode = c("WK034401", "WK034402", "WK034403", "WK034409", "WK036301"), wijknaam = c("West", "Noordwest", "Overvecht", "Water", "Burgwallen-Oude Zijde"),
                  gemeentecode = c(rep("GM0344", 4), "GM0363"), gemeentenaam = c(rep("Utrecht", 4), "Amsterdam"), water = c("NEE", "NEE", "NEE", "JA", "NEE"), aantalInwoners = c(40000, 50000, 60000, -99999999, 7000), stringsAsFactors = FALSE)
  g <- sf::st_sf(w, geometry = sf::st_sfc(lapply(1:5, function(k) square(5.05 + (k - 1) * 0.03, 52.08, 0.025)), crs = 4326))
  sf::st_write(g, file.path(d, "20260101_wijken_2025_p01.geojson"), quiet = TRUE, delete_dsn = TRUE)
  rows <- expand.grid(wijk = c("WK034401", "WK034402", "WK034403", "WK034404"), type = names(UTR_TYPES), month = 1:12, stringsAsFactors = FALSE)
  rows$n <- ifelse(rows$type == "0.0.0 ", 999, 10 + as.integer(substr(rows$wijk, 8, 8)) + match(rows$type, names(UTR_TYPES)))
  v <- data.frame(ID = seq_len(nrow(rows) + 1), SoortMisdrijf = c(rows$type, "1.1.1 "), WijkenEnBuurten = paste0(c(rows$wijk, "WK034401"), "  "), Perioden = c(sprintf("2025MM%02d", rows$month), "2025JJ00"),
                  GeregistreerdeMisdrijven_1 = c(rows$n, 5000), stringsAsFactors = FALSE)                        # CBS pads the codes; a yearly row is left out
  writeLines(jsonlite::toJSON(list(odata.metadata = "https://dataderden.cbs.nl/ODataFeed/odata/47022NED/$metadata#Cbs.OData.WebAPI.TypedDataSet", value = v), dataframe = "rows", auto_unbox = TRUE), file.path(d, "20260101_47022NED_2025_p01.json"))
  writeLines(jsonlite::toJSON(list(value = data.frame(Key = names(UTR_TYPES), Title = unname(UTR_TYPES), stringsAsFactors = FALSE)), dataframe = "rows", auto_unbox = TRUE), file.path(d, "20260101_47022NED_SoortMisdrijf.json"))
}

# Everything loaded once per test session: the fixtures written, the five modules run with download = FALSE
load_fixtures <- function() {
  use_test_project()
  if (isTRUE(getOption("webarchy.fixtures_loaded"))) return(invisible(TRUE))
  register_test_country(); xt_write_fixtures(); zurich_write_fixtures(); toronto_write_fixtures(); buenosaires_write_fixtures(); utrecht_write_fixtures()
  quiet(update_countries(download = FALSE, only = c("XT", "ZRH", "TOR", "BUE", "UTR")))
  options(webarchy.fixtures_loaded = TRUE)
  invisible(TRUE)
}
