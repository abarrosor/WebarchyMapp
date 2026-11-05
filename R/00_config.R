# Project configuration ------------------------------------------------------
# All paths are relative to the RStudio project root (the folder that holds
# WebarchyMapp.Rproj). Every data source is registered here once.

CONFIG <- list(
  dir_raw     = "data/raw",
  dir_reports = "reports",
  dir_ref     = "reference",
  db_path     = "data/webarchymapp.duckdb",

  # CKAN API endpoints of the open-data portals
  ckan_madrid    = "https://datos.madrid.es/api/3/action",
  ckan_barcelona = "https://opendata-ajuntament.barcelona.cat/data/api/3/action",

  sources = list(

    # --- Madrid -----------------------------------------------------------
    # F1 · Calls received by the Municipal Police central dispatch (monthly CSV,
    # by neighbourhood, since 2026). Madrid City Council, CC BY 4.0.
    madrid_calls = list(
      ckan_id = "837676-0-incidencias-recibidas-en-la-emisora-central-de-policia-municipal",
      format  = "csv",
      dir_raw = "data/raw/madrid_calls"
    ),
    # F2 · Municipal Police activity statistics (one Excel per month, by
    # district, since 2014). Madrid City Council, CC BY 4.0.
    madrid_activity = list(
      ckan_id = "212616-0-policia-estadisticas",
      dir_raw = "data/raw/madrid_activity"
    ),
    # Official district and neighbourhood boundaries (Madrid City Council, CC BY 4.0)
    madrid_districts = list(
      urls    = "https://geoportal.madrid.es/fsdescargas/IDEAM_WBGEOPORTAL/LIMITES_ADMINISTRATIVOS/Distritos/Distritos.zip",
      dir_raw = "data/raw/madrid_districts"
    ),
    madrid_neighbourhoods = list(
      urls    = "https://geoportal.madrid.es/fsdescargas/IDEAM_WBGEOPORTAL/LIMITES_ADMINISTRATIVOS/Barrios/Barrios.zip",
      dir_raw = "data/raw/madrid_neighbourhoods"
    ),
    # Monthly municipal register (population by neighbourhood). All files are kept.
    madrid_census = list(
      ckan_id = "200076-0-padron",
      format  = "csv",
      dir_raw = "data/raw/madrid_census"
    ),

    # --- Spain ------------------------------------------------------------
    # F3 · Crime Balance of the Ministry of the Interior (quarterly). Table
    # "Municipalities above 20,000 inhabitants and islands": the whole of
    # Spain with regions, provinces and municipalities. See R/crime_balance.R.
    crime_balance = list(
      portal  = "https://estadisticasdecriminalidad.ses.mir.es",
      from    = 2019,                       # first year downloaded
      dir_raw = "data/raw/crime_balance"
    ),
    # Official population of every Spanish municipality by year (INE table 29005)
    ine_population = list(
      urls    = "https://www.ine.es/jaxiT3/files/t/csv_bdsc/29005.csv",
      dir_raw = "data/raw/ine_population"
    ),
    # Municipal boundaries of Spain (IGN CartoBase ANE 1:3,000,000, CC BY 4.0),
    # from the copy kept by the open-source mapSpain project on GitHub.
    # "_x" = mainland and Balearic Islands, "_y" = Canary Islands.
    municipal_boundaries = list(
      urls    = c("https://github.com/rOpenSpain/mapSpain/raw/sianedata/dist/se89_3_admin_muni_a_x.gpkg",
                  "https://github.com/rOpenSpain/mapSpain/raw/sianedata/dist/se89_3_admin_muni_a_y.gpkg"),
      dir_raw = "data/raw/municipal_boundaries"
    ),

    # --- Catalonia and Barcelona ---------------------------------------------
    # F4 · Mossos d'Esquadra: offences known, solved and arrests (monthly, by
    # basic police area). Government of Catalonia, Socrata open-data portal.
    mossos = list(
      url     = "https://analisi.transparenciacatalunya.cat/resource/qnyt-emjc.csv",
      page    = "https://analisi.transparenciacatalunya.cat/d/qnyt-emjc",
      from    = 2019,
      dir_raw = "data/raw/mossos",
      # Basic police areas (ABP): municipalities of each one, and boundaries
      abp_municipalities_url = "https://analisi.transparenciacatalunya.cat/resource/dtpq-6fvw.csv?%24select=codi_abp_mossos,codi_municipi,codi_districte,nom_districte&%24limit=5000",
      abp_boundaries_url     = "https://analisi.transparenciacatalunya.cat/resource/i2dd-kfpa.geojson?%24limit=100",
      dir_abp = "data/raw/abp"
    ),
    # F5 · Barcelona City Police (Guardia Urbana): incidents handled (monthly by
    # neighbourhood, one file per year). Open Data BCN (CKAN), CC BY 4.0.
    bcn_incidents = list(
      ckan_id = "incidents-gestionats-gub",
      from    = 2019,
      dir_raw = "data/raw/bcn_incidents"
    ),
    # Barcelona district and neighbourhood boundaries (Open Data BCN, CC BY 4.0)
    bcn_boundaries = list(
      ckan_id = "20170706-districtes-barris",
      include = "^BarcelonaCiutat_(Districtes|Barris)\\.csv$",
      dir_raw = "data/raw/bcn_boundaries"
    ),
    # Barcelona register by census section and sex (yearly, 1 January)
    bcn_census = list(
      ckan_id = "pad_mdbas_sexe",
      dir_raw = "data/raw/bcn_census"
    )
  )
)

# Raw-data layout -------------------------------------------------------------------------------
# Target layout (one folder per country, city or shared source, like data/raw/country_<cc>):
#   data/raw/country_es/{crime_balance, ine_population, municipal_boundaries, mossos, abp}
#   data/raw/city_madrid/{calls, activity, districts, neighbourhoods, census}
#   data/raw/city_barcelona/{incidents, boundaries, census}
#   data/raw/shared/{gisco, geoboundaries, natural_earth, eurostat}
# The legacy flat folders (data/raw/crime_balance, data/raw/madrid_calls...) keep working: a path
# resolves to the new folder when it exists, to the legacy one while only that exists. The one-off
# script scripts/maintenance/migrate_raw_layout.R moves the folders (files are never edited or
# deleted). Decision pending (Q-004 in the decision register).
raw_dir <- function(new, legacy) if (!dir.exists(new) && dir.exists(legacy)) legacy else new
RAW_LAYOUT <- c(   # new path = legacy path
  "data/raw/country_es/crime_balance" = "data/raw/crime_balance", "data/raw/country_es/ine_population" = "data/raw/ine_population",
  "data/raw/country_es/municipal_boundaries" = "data/raw/municipal_boundaries", "data/raw/country_es/mossos" = "data/raw/mossos", "data/raw/country_es/abp" = "data/raw/abp",
  "data/raw/city_madrid/calls" = "data/raw/madrid_calls", "data/raw/city_madrid/activity" = "data/raw/madrid_activity", "data/raw/city_madrid/districts" = "data/raw/madrid_districts",
  "data/raw/city_madrid/neighbourhoods" = "data/raw/madrid_neighbourhoods", "data/raw/city_madrid/census" = "data/raw/madrid_census",
  "data/raw/city_barcelona/incidents" = "data/raw/bcn_incidents", "data/raw/city_barcelona/boundaries" = "data/raw/bcn_boundaries", "data/raw/city_barcelona/census" = "data/raw/bcn_census",
  "data/raw/shared/gisco" = "data/raw/gisco", "data/raw/shared/geoboundaries" = "data/raw/geoboundaries", "data/raw/shared/natural_earth" = "data/raw/natural_earth", "data/raw/shared/eurostat" = "data/raw/eurostat")
raw_resolve <- function(new) raw_dir(new, unname(RAW_LAYOUT[new]))
local({
  map <- c(crime_balance = "data/raw/country_es/crime_balance", ine_population = "data/raw/country_es/ine_population", municipal_boundaries = "data/raw/country_es/municipal_boundaries",
           mossos = "data/raw/country_es/mossos", madrid_calls = "data/raw/city_madrid/calls", madrid_activity = "data/raw/city_madrid/activity", madrid_districts = "data/raw/city_madrid/districts",
           madrid_neighbourhoods = "data/raw/city_madrid/neighbourhoods", madrid_census = "data/raw/city_madrid/census", bcn_incidents = "data/raw/city_barcelona/incidents",
           bcn_boundaries = "data/raw/city_barcelona/boundaries", bcn_census = "data/raw/city_barcelona/census")
  for (k in names(map)) CONFIG$sources[[k]]$dir_raw <<- raw_resolve(map[[k]])
  CONFIG$sources$mossos$dir_abp <<- raw_resolve("data/raw/country_es/abp")
})
CONFIG$shared <- list(gisco = raw_resolve("data/raw/shared/gisco"), geoboundaries = raw_resolve("data/raw/shared/geoboundaries"),
                      natural_earth = raw_resolve("data/raw/shared/natural_earth"), eurostat = raw_resolve("data/raw/shared/eurostat"))
# TRUE when at least one source still lives in a legacy folder
raw_layout_legacy <- function() any(vapply(names(RAW_LAYOUT), function(n) !dir.exists(n) && dir.exists(RAW_LAYOUT[[n]]), logical(1)))

# Shared open sources used by the country modules (R/70_countries.R and R/7x_*.R)
CONFIG$gisco <- list(base = "https://gisco-services.ec.europa.eu/distribution/v2", nuts_year = 2024, lau_year = 2023)
CONFIG$eurostat <- list(base = "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT")

# Territory identifiers (see R/territory.R for the scheme)
ID_SPAIN     <- "ES"
ID_MADRID    <- "ES-13-28-28079"
ID_BARCELONA <- "ES-09-08-08019"

# Create the working folders if they do not exist
prepare_folders <- function() {
  dirs <- c(CONFIG$dir_raw, CONFIG$dir_reports,
            unlist(lapply(CONFIG$sources, function(s) c(s$dir_raw, s$dir_abp))))
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  invisible(dirs)
}
