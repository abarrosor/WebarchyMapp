# European countries without a national module: Eurostat regional crime data ----------------
# Dataset crim_gen_reg "Police-recorded offences by NUTS 3 region": 7 ICCS offences
# (intentional homicide, assault, robbery, burglary, burglary of private residential premises,
# theft, theft of a motorized land vehicle), yearly 2008 onwards, counts (unit NR), supplied by
# each national authority. Eurostat statistical data: CC BY 4.0. Population: demo_r_pjangrp3
# (1 January, NUTS 0-3). Boundaries: Eurostat GISCO NUTS 2024 (© EuroGeographics for the
# administrative boundaries; non-commercial use).
#
# One module registers every country listed in reference/eurostat_regions_countries.csv that
# has no dedicated module (R/7x_*.R, R/8x_*.R): levels region = NUTS 2, province = NUTS 3.
# No municipalities. Coverage differs by country (some report NUTS 2 or the country only,
# some offences start later): what Eurostat has is what is shown; the rest stays grey.
# There is no official total in this dataset: the "total" is the sum of the 7 offences.

EUROSTAT_REGIONS <- list(
  dataset = "crim_gen_reg", population = "demo_r_pjangrp3",
  api = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data",
  dir_raw = "data/raw/eurostat",
  types = data.frame(
    code = c("ICCS0101", "ICCS02011", "ICCS0401", "ICCS0501", "ICCS05012", "ICCS0502", "ICCS050211"),
    offence_type_id = c("homicide", "assault", "robbery_violence", "burglary", "residential_burglary", "theft", "vehicle_theft"),
    name_en = c("Intentional homicide", "Assault", "Robbery", "Burglary", "Burglary of private residential premises", "Theft", "Theft of a motorized land vehicle"),
    included_in = c("total", "total", "total", "total", "burglary", "total", "theft"),
    sensitive = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), stringsAsFactors = FALSE)
)

er_file <- function(name) { f <- newest_first(list.files(EUROSTAT_REGIONS$dir_raw, paste0("_", name, "$"), full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

er_download <- function(days = 30) {
  x <- EUROSTAT_REGIONS
  get <- function(name, url) {
    prev <- er_file(name)
    if (!is.na(prev) && difftime(Sys.time(), file.mtime(prev), units = "days") < days) { message("= Eurostat ", name, " downloaded less than ", days, " days ago"); return(invisible(prev)) }
    download_versioned(url, x$dir_raw, name = name)
  }
  get(paste0(x$dataset, "_NR.json"), paste0(x$api, "/", x$dataset, "?format=JSON&lang=EN&unit=NR"))
  get(paste0(x$population, "_T.json"), paste0(x$api, "/", x$population, "?format=JSON&lang=EN&sex=T&age=TOTAL&unit=NR&sinceTimePeriod=2008"))
  for (lv in 2:3) {   # NUTS 2 and 3 boundaries, 1:3M, WGS84
    url <- sprintf("%s/nuts/geojson/NUTS_RG_03M_%d_4326_LEVL_%d.geojson", CONFIG$gisco$base, CONFIG$gisco$nuts_year, lv)
    prev <- newest_first(list.files(file.path(CONFIG$dir_raw, "gisco"), basename(url), full.names = TRUE))
    if (length(prev) == 0 || difftime(Sys.time(), file.mtime(prev[1]), units = "days") > 365) download_versioned(url, file.path(CONFIG$dir_raw, "gisco"))
  }
}

# The whole dataset as a long data frame (cached per R session)
ER_STATE <- new.env()
er_data <- function() {
  if (!is.null(ER_STATE$crime)) return(ER_STATE$crime)
  f <- er_file(paste0(EUROSTAT_REGIONS$dataset, "_NR.json")); if (is.na(f)) stop("No Eurostat crim_gen_reg file yet: run the download")
  d <- jsonstat_to_df(jsonlite::fromJSON(f, simplifyVector = FALSE))
  d <- d[!is.na(d$value), c("geo", "geo_label", "iccs", "time", "value")]
  d$year <- as.integer(d$time); d$cntr <- substr(d$geo, 1, 2)
  ER_STATE$crime <- d; d
}
er_population <- function() {
  if (!is.null(ER_STATE$pop)) return(ER_STATE$pop)
  f <- er_file(paste0(EUROSTAT_REGIONS$population, "_T.json")); if (is.na(f)) stop("No Eurostat population file yet")
  d <- jsonstat_to_df(jsonlite::fromJSON(f, simplifyVector = FALSE))
  d <- d[!is.na(d$value), c("geo", "geo_label", "time", "value")]
  d$year <- as.integer(d$time); d$cntr <- substr(d$geo, 1, 2)
  ER_STATE$pop <- d; d
}
er_nuts <- function(level) {
  f <- newest_first(list.files(file.path(CONFIG$dir_raw, "gisco"), sprintf("NUTS_RG_03M_%d_4326_LEVL_%d\\.geojson$", CONFIG$gisco$nuts_year, level), full.names = TRUE))
  if (length(f) == 0) return(NULL)
  sf::st_read(f[1], quiet = TRUE)
}

# Eurostat country code of a module code (Greece is EL in Eurostat)
er_geo_code <- function(cc) if (cc == "GR") "EL" else cc
# Territory id of a NUTS code: <cc>-<NUTS2>-<NUTS3> (NUTS 0 -> cc)
er_id <- function(cc, nuts) { n <- nchar(nuts); ifelse(n <= 2, cc, ifelse(n == 3, paste(cc, nuts, sep = "-"), ifelse(n == 4, paste(cc, nuts, sep = "-"), paste(cc, substr(nuts, 1, 4), nuts, sep = "-")))) }
er_level <- function(nuts) c("country", "nuts1", "region", "province")[pmin(nchar(nuts) - 1, 4)]

er_make <- function(row) {
  cc <- row$code; geo <- er_geo_code(cc)
  list(
    code = cc, iso3 = row$iso3, name_en = row$name_en, name_local = row$name_local, lang = row$lang,
    levels = c("region", "province"),
    level_labels = list(region = c(row$nuts2_en, row$nuts2_local, row$nuts2_en_s, row$nuts2_local_s), province = c(row$nuts3_en, row$nuts3_local, row$nuts3_en_s, row$nuts3_local_s)),
    source = list(name = "Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region)", publisher = "Eurostat, from the national authorities",
                  url = "https://ec.europa.eu/eurostat/databrowser/view/crim_gen_reg/default/table?lang=en", licence = "CC BY 4.0 (Eurostat)",
                  measure = "Police-recorded offences for 7 ICCS categories, yearly; national definitions mapped to ICCS by each country (no official total: the total shown is the sum of the 7 categories)."),
    population_source = "Eurostat demo_r_pjangrp3 (1 January)", boundaries_attribution = "© EuroGeographics for the administrative boundaries (Eurostat GISCO NUTS)",
    not_published = c("Not reported to Eurostat for this region", "Not reported to Eurostat for this region"),
    notes_en = "Regional detail and starting years differ by country; some countries report NUTS 2 or the country only.",
    default_region = if (nzchar(row$default_region)) paste(cc, row$default_region, sep = "-") else NULL,
    download = function() er_download(),
    territory = function(con) {
      d <- er_data(); d <- d[d$cntr == geo, ]; p <- er_population(); p <- p[p$cntr == geo, ]
      codes <- unique(rbind(d[, c("geo", "geo_label")], p[, c("geo", "geo_label")]))
      codes <- codes[!grepl("Z$", codes$geo) & nchar(codes$geo) %in% c(2, 4, 5), ]           # NUTS 0, 2, 3 (no extra-regio)
      ids <- er_id(cc, codes$geo); lvl <- er_level(codes$geo)
      parent <- ifelse(lvl == "country", NA, ifelse(lvl == "region", cc, er_id(cc, substr(codes$geo, 1, 4))))
      tbl <- data.frame(territory_id = ids, level = lvl, parent_id = parent, official_code = codes$geo, name = codes$geo_label)
      tbl$name[tbl$level == "country"] <- row$name_en
      save_country_territory(con, cc, tbl, scheme = "NUTS 2024")
      g2 <- er_nuts(2); g3 <- er_nuts(3)
      if (!is.null(g2) && !is.null(g3)) {
        g2 <- g2[g2$CNTR_CODE == geo, ]; g3 <- g3[g3$CNTR_CODE == geo, ]
        g2$territory_id <- er_id(cc, g2$NUTS_ID); g2$name <- g2$NAME_LATN
        g3$territory_id <- er_id(cc, g3$NUTS_ID); g3$name <- g3$NAME_LATN
        save_country_gpkg(con, cc, list(region = g2[, c("territory_id", "name")], province = g3[, c("territory_id", "name")]), tolerance_m = 0)
      } else message("  NUTS boundaries not downloaded: tables only")
    },
    ingest = function(con) {
      d <- er_data(); d <- d[d$cntr == geo & !grepl("Z$", d$geo) & nchar(d$geo) %in% c(2, 4, 5), ]
      if (nrow(d) == 0) stop("Eurostat has no regional crime data for ", cc)
      out <- data.frame(country = cc, territory_id = er_id(cc, d$geo), level = er_level(d$geo), offence_code = d$iccs, year = d$year, n = d$value, file = basename(er_file(paste0(EUROSTAT_REGIONS$dataset, "_NR.json"))))
      # Total = sum of the top-level categories (sub-categories excluded so nothing counts twice)
      top <- EUROSTAT_REGIONS$types$code[EUROSTAT_REGIONS$types$included_in == "total"]
      tot <- out[out$offence_code %in% top, ] |> dplyr::group_by(country, territory_id, level, year, file) |> dplyr::summarise(n = sum(n), k = dplyr::n(), .groups = "drop")
      tot <- tot[tot$k >= 3, ]; tot$offence_code <- "TOTAL_7"; tot$k <- NULL   # only when at least 3 of the 5 top categories exist
      out <- yearly_to_cumulative(rbind(out, tot[, names(out)]))
      replace_country_rows(con, "stg_country_crime", cc, out)
      log_ingest(con, paste0("country_", cc), out$file[1], NA, paste(range(out$year), collapse = "-"), nrow(out))
      message(sprintf("  %s: %s rows, %d territories, %d-%d", cc, fmt_int(nrow(out)), length(unique(out$territory_id)), min(out$year), max(out$year)))
    },
    population = function(con) {
      p <- er_population(); p <- p[p$cntr == geo & !grepl("Z$", p$geo) & nchar(p$geo) %in% c(2, 4, 5), ]
      replace_country_rows(con, "ref_country_population", cc, data.frame(country = cc, territory_id = er_id(cc, p$geo), level = er_level(p$geo), year = p$year, population = p$value))
    }
  )
}

# Offence-type reference shared by every Eurostat-regional country (written once per country code)
er_write_types <- function(cc) {
  f <- file.path(CONFIG$dir_ref, paste0(tolower(cc), "_offence_types.csv"))
  if (file.exists(f)) return(invisible(f))
  t <- EUROSTAT_REGIONS$types
  df <- rbind(data.frame(source_code = "TOTAL_7", offence_type_id = "total", name_local = "Sum of the 7 Eurostat categories (not an official total)", name_en = "Sum of the 7 Eurostat categories (not an official total)", included_in = "", sensitive = "no"),
              data.frame(source_code = t$code, offence_type_id = t$offence_type_id, name_local = t$name_en, name_en = t$name_en, included_in = t$included_in, sensitive = ifelse(t$sensitive, "yes", "no")))
  utils::write.csv(df, f, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(f)
}

local({
  reg <- read_reference("eurostat_regions_countries.csv")
  for (i in seq_len(nrow(reg))) {
    cc <- reg$code[i]
    if (!is.null(COUNTRY_MODULES[[cc]])) next            # a national module exists: it wins
    er_write_types(cc)
    register_country(er_make(reg[i, ]))
  }
})
