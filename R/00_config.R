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
      dir_raw = "data/raw/city_madrid/calls"
    ),
    # F2 · Municipal Police activity statistics (one Excel per month, by
    # district, since 2014). Madrid City Council, CC BY 4.0.
    madrid_activity = list(
      ckan_id = "212616-0-policia-estadisticas",
      dir_raw = "data/raw/city_madrid/activity"
    ),
    # Official district and neighbourhood boundaries (Madrid City Council, CC BY 4.0)
    madrid_districts = list(
      urls    = "https://geoportal.madrid.es/fsdescargas/IDEAM_WBGEOPORTAL/LIMITES_ADMINISTRATIVOS/Distritos/Distritos.zip",
      dir_raw = "data/raw/city_madrid/districts"
    ),
    madrid_neighbourhoods = list(
      urls    = "https://geoportal.madrid.es/fsdescargas/IDEAM_WBGEOPORTAL/LIMITES_ADMINISTRATIVOS/Barrios/Barrios.zip",
      dir_raw = "data/raw/city_madrid/neighbourhoods"
    ),
    # Monthly municipal register (population by neighbourhood). All files are kept.
    madrid_census = list(
      ckan_id = "200076-0-padron",
      format  = "csv",
      dir_raw = "data/raw/city_madrid/census"
    ),

    # --- Spain ------------------------------------------------------------
    # F3 · Crime Balance of the Ministry of the Interior (quarterly). Table
    # "Municipalities above 20,000 inhabitants and islands": the whole of
    # Spain with regions, provinces and municipalities. See R/crime_balance.R.
    crime_balance = list(
      from    = 2019,                       # first year downloaded (URLs: reference/es_sources.csv)
      dir_raw = "data/raw/country_es/crime_balance"
    ),
    # Official population of every Spanish municipality by year (INE table 29005)
    ine_population = list(
      dir_raw = "data/raw/country_es/ine_population"
    ),
    # Municipal boundaries of Spain (IGN CartoBase ANE 1:3,000,000, CC BY 4.0),
    # from the copy kept by the open-source mapSpain project on GitHub.
    # "_x" = mainland and Balearic Islands, "_y" = Canary Islands.
    municipal_boundaries = list(
      dir_raw = "data/raw/country_es/municipal_boundaries"
    ),

    # --- Catalonia and Barcelona ---------------------------------------------
    # F4 · Mossos d'Esquadra: offences known, solved and arrests (monthly, by
    # basic police area). Government of Catalonia, Socrata open-data portal.
    mossos = list(
      url     = "https://analisi.transparenciacatalunya.cat/resource/qnyt-emjc.csv",
      page    = "https://analisi.transparenciacatalunya.cat/d/qnyt-emjc",
      from    = 2019,
      dir_raw = "data/raw/country_es/mossos",
      # Basic police areas (ABP): municipalities of each one, and boundaries
      abp_municipalities_url = "https://analisi.transparenciacatalunya.cat/resource/dtpq-6fvw.csv?%24select=codi_abp_mossos,codi_municipi,codi_districte,nom_districte&%24limit=5000",
      abp_boundaries_url     = "https://analisi.transparenciacatalunya.cat/resource/i2dd-kfpa.geojson?%24limit=100",
      dir_abp = "data/raw/country_es/abp"
    ),
    # F5 · Barcelona City Police (Guardia Urbana): incidents handled (monthly by
    # neighbourhood, one file per year). Open Data BCN (CKAN), CC BY 4.0.
    bcn_incidents = list(
      ckan_id = "incidents-gestionats-gub",
      from    = 2019,
      dir_raw = "data/raw/city_barcelona/incidents"
    ),
    # Barcelona district and neighbourhood boundaries (Open Data BCN, CC BY 4.0)
    bcn_boundaries = list(
      ckan_id = "20170706-districtes-barris",
      include = "^BarcelonaCiutat_(Districtes|Barris)\\.csv$",
      dir_raw = "data/raw/city_barcelona/boundaries"
    ),
    # Barcelona register by census section and sex (yearly, 1 January)
    bcn_census = list(
      ckan_id = "pad_mdbas_sexe",
      dir_raw = "data/raw/city_barcelona/census"
    )
  )
)

# Raw-data layout -------------------------------------------------------------------------------
# One folder per country, city or shared source, like data/raw/country_<cc> (layout unified 2026-11-05, P-026):
#   data/raw/country_es/{crime_balance, ine_population, municipal_boundaries, mossos, abp}
#   data/raw/city_madrid/{calls, activity, districts, neighbourhoods, census}
#   data/raw/city_barcelona/{incidents, boundaries, census}
#   data/raw/shared/{gisco, geoboundaries, natural_earth, eurostat}
CONFIG$shared <- list(gisco = "data/raw/shared/gisco", geoboundaries = "data/raw/shared/geoboundaries", natural_earth = "data/raw/shared/natural_earth", eurostat = "data/raw/shared/eurostat")

# GDAL refuses GeoJSON features above 200 MB by default ("GeoJSON object too complex/large"): no limit
Sys.setenv(OGR_GEOJSON_MAX_OBJ_SIZE = "0")

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
