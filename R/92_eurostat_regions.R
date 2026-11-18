# European countries without a national module: Eurostat regional crime data ----------------
# Dataset crim_gen_reg "Police-recorded offences by NUTS 3 region": 7 ICCS offences
# (intentional homicide, assault, robbery, burglary, burglary of private residential premises,
# theft, theft of a motorized land vehicle), yearly 2008 onwards, counts (unit NR), supplied by
# each national authority. Eurostat statistical data: CC BY 4.0. Population: demo_r_pjangrp3
# (1 January, NUTS 0-3), which begins in 2014; the years before it, back to the first year of the
# crime figures, come from demo_r_pjanaggr3 (the same population by broad age group, from 1990), so
# that a rate of 2010 stands on the people of 2010. Boundaries: Eurostat GISCO NUTS, every
# edition since 2006 (© EuroGeographics for the administrative boundaries; non-commercial use):
# the figures of a year carry the codes of the classification of that time, so each edition's
# polygons are kept for the codes it alone has, and the newest one is the "current" map. The
# list of editions is read from GISCO's own index, so NUTS 2027 comes in by itself (P-057).
#
# One module registers every country listed in reference/eurostat_regions_countries.csv that
# has no dedicated module (R/7x_*.R, R/8x_*.R): levels region = NUTS 2, province = NUTS 3.
# No municipalities. Coverage differs by country (some report NUTS 2 or the country only,
# some offences start later): what Eurostat has is what is shown; the rest stays grey.
# There is no official total in this dataset: the "total" is the sum of the 7 offences.

EUROSTAT_REGIONS <- list(
  dataset = "crim_gen_reg", population = "demo_r_pjangrp3",
  population_before = "demo_r_pjanaggr3", population_before_until = 2014L,     # the earlier years, and one year shared with the main table to check that both agree
  api = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data",
  dir_raw = CONFIG$shared$eurostat,
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
    if (!is.na(prev) && file_age_days(prev) < days) { message("= Eurostat ", name, " downloaded less than ", days, " days ago"); return(invisible(prev)) }
    download_versioned(url, x$dir_raw, name = name)
  }
  get(paste0(x$dataset, "_NR.json"), paste0(x$api, "/", x$dataset, "?format=JSON&lang=EN&unit=NR"))
  get(paste0(x$population, "_T.json"), paste0(x$api, "/", x$population, "?format=JSON&lang=EN&sex=T&age=TOTAL&unit=NR&sinceTimePeriod=2008"))
  # The population of the years before the main table: never a reason to stop (the rates of those years then stand on the first year there is, marked in the app)
  tryCatch(get(paste0(x$population_before, "_T.json"), paste0(x$api, "/", x$population_before, "?format=JSON&lang=EN&sex=T&age=TOTAL&unit=NR&sinceTimePeriod=2008&untilTimePeriod=", x$population_before_until)),
           error = function(e) message("  Eurostat ", x$population_before, " (population before 2014) not downloaded: ", conditionMessage(e)))
  # NUTS 2 and 3 boundaries, 1:3M, WGS84, of every edition GISCO lists (crim_gen_reg still carries the codes of
  # older editions, e.g. Italian provinces or Norwegian counties before their reforms). The index first: a new
  # edition (NUTS 2027 is valid from 1 January 2027) is then downloaded the first time it is listed.
  idx <- newest_first(list.files(CONFIG$shared$gisco, "_nuts_datasets\\.json$", full.names = TRUE))
  if (length(idx) == 0 || file_age_days(idx[1]) > days)
    tryCatch(download_versioned(paste0(CONFIG$gisco$base, "/nuts/datasets.json"), CONFIG$shared$gisco, name = "nuts_datasets.json"),
             error = function(e) message("  GISCO index of NUTS editions not downloaded (", conditionMessage(e), "): the editions already known are used"))
  for (yr in er_editions()) for (lv in 2:3) {
    url <- sprintf("%s/nuts/geojson/NUTS_RG_03M_%d_4326_LEVL_%d.geojson", CONFIG$gisco$base, yr, lv)
    prev <- newest_first(list.files(CONFIG$shared$gisco, basename(url), full.names = TRUE))
    if (length(prev) == 0 || file_age_days(prev[1]) > 365) tryCatch(download_versioned(url, CONFIG$shared$gisco), error = function(e) message("  GISCO NUTS ", yr, " level ", lv, " not downloaded (", conditionMessage(e), ")"))
  }
}

# The NUTS editions to keep, newest first: those of GISCO's index (datasets.json, keys "nuts-2024"...) from 2006 on,
# together with the ones this code has always known; the built-in list alone when the index was never downloaded.
ER_EDITIONS <- c(2024L, 2021L, 2016L, 2013L, 2010L, 2006L)
er_editions <- function() {
  idx <- newest_first(list.files(CONFIG$shared$gisco, "_nuts_datasets\\.json$", full.names = TRUE))
  listed <- if (length(idx)) tryCatch({ k <- names(jsonlite::fromJSON(idx[1], simplifyVector = FALSE)); as.integer(sub("^nuts-", "", k[grepl("^nuts-[0-9]{4}$", k)])) }, error = function(e) integer()) else integer()
  y <- sort(unique(c(ER_EDITIONS, CONFIG$gisco$nuts_year, listed)), decreasing = TRUE)
  y[!is.na(y) & y >= 2006]
}

# The files of the shared folder that every Eurostat-regional country reads (the newest copy of each): a new copy of
# any of them is a reason to load those countries again (`watch` of the module, country_unchanged())
er_files <- function() { x <- EUROSTAT_REGIONS; c(er_file(paste0(x$dataset, "_NR.json")), er_file(paste0(x$population, "_T.json")), er_file(paste0(x$population_before, "_T.json"))) }

# The whole dataset as a long data frame, kept for the R session while the file it was read from is still the newest
# (a copy downloaded later in the same run is read again: another module may have read the old one first)
ER_STATE <- new.env()
er_data <- function() {
  f <- er_file(paste0(EUROSTAT_REGIONS$dataset, "_NR.json")); if (is.na(f)) stop("No Eurostat crim_gen_reg file yet: run the download")
  if (!is.null(ER_STATE$crime) && identical(ER_STATE$crime_file, f)) return(ER_STATE$crime)
  d <- jsonstat_to_df(jsonlite::fromJSON(f, simplifyVector = FALSE))
  d <- d[!is.na(d$value), c("geo", "geo_label", "iccs", "time", "value")]
  d$year <- as.integer(d$time); d$cntr <- substr(d$geo, 1, 2)
  ER_STATE$crime <- d; ER_STATE$crime_file <- f; d
}
er_population_file <- function(f) {
  d <- jsonstat_to_df(jsonlite::fromJSON(f, simplifyVector = FALSE))
  d <- d[!is.na(d$value), c("geo", "geo_label", "time", "value")]
  d$year <- as.integer(d$time); d$cntr <- substr(d$geo, 1, 2)
  d
}
# The years of the earlier table that the main one lacks, added country by country. A country takes them only when
# the two tables agree on the years they share (within 0.5 % for at least 95 % of its regions): the earlier table
# is a different extraction, and figures that do not match would put a step in every rate. Returns the main table
# with the added rows; `attr(, "added")` lists the countries and years added, `attr(, "refused")` the countries left out.
er_merge_population <- function(main, before) {
  if (is.null(before) || nrow(before) == 0) return(main)
  key <- function(d) paste(d$geo, d$year)
  added <- character(); refused <- character(); extra <- list()
  for (cn in intersect(unique(main$cntr), unique(before$cntr))) {
    m <- main[main$cntr == cn, ]; b <- before[before$cntr == cn, ]
    new <- b[!key(b) %in% key(m) & b$year < min(m$year), ]; if (nrow(new) == 0) next
    both <- merge(m[, c("geo", "year", "value")], b[, c("geo", "year", "value")], by = c("geo", "year"), suffixes = c("", ".before"))
    agree <- nrow(both) > 0 && mean(abs(both$value - both$value.before) <= 0.005 * pmax(both$value, 1)) >= 0.95
    if (!agree) { refused <- c(refused, cn); next }
    extra[[cn]] <- new[new$geo %in% m$geo, ]                    # only the regions the main table knows: the codes of an older NUTS version stay out
    if (nrow(extra[[cn]])) added <- c(added, sprintf("%s %d-%d", cn, min(extra[[cn]]$year), max(extra[[cn]]$year)))
  }
  out <- rbind(main, do.call(rbind, extra))
  attr(out, "added") <- added; attr(out, "refused") <- refused
  out
}
er_population <- function() {
  f <- er_file(paste0(EUROSTAT_REGIONS$population, "_T.json")); if (is.na(f)) stop("No Eurostat population file yet")
  fb <- er_file(paste0(EUROSTAT_REGIONS$population_before, "_T.json"))
  key <- paste(f, fb)                                           # read again when either file has a newer copy
  if (!is.null(ER_STATE$pop) && identical(ER_STATE$pop_key, key)) return(ER_STATE$pop)
  d <- er_population_file(f)
  if (!is.na(fb)) {
    d <- tryCatch({
      out <- er_merge_population(d, er_population_file(fb))
      if (length(attr(out, "added"))) message("  Eurostat population before ", min(d$year), " (", EUROSTAT_REGIONS$population_before, "): ", length(attr(out, "added")), " countries, e.g. ", paste(utils::head(attr(out, "added"), 3), collapse = ", "))
      if (length(attr(out, "refused"))) message("  Eurostat population before ", min(d$year), " not used for ", paste(attr(out, "refused"), collapse = " "), ": the two tables do not agree on the years they share")
      out
    }, error = function(e) { message("  Eurostat ", EUROSTAT_REGIONS$population_before, " not read (", conditionMessage(e), "): population from ", min(d$year), " only"); d })
  }
  ER_STATE$pop <- d; ER_STATE$pop_key <- key; d
}
er_nuts <- function(level, year = CONFIG$gisco$nuts_year) {
  f <- newest_first(list.files(CONFIG$shared$gisco, sprintf("NUTS_RG_03M_%d_4326_LEVL_%d\\.geojson$", year, level), full.names = TRUE))
  if (length(f) == 0) return(NULL)
  sf::st_read(f[1], quiet = TRUE)
}
# The polygons of every edition on disk, one per code: the newest edition that has the code wins. `current` marks
# those of the newest edition, the map of today; the others are codes that no longer exist, drawn only for the
# years in which they have figures (the app draws a polygon without figures only when it is current). `edition`
# is the year of the edition the polygon comes from, and the attribute "current_edition" the newest one found.
er_nuts_all <- function(level) {
  g <- NULL; newest <- NA_integer_
  for (yr in er_editions()) {
    one <- er_nuts(level, yr); if (is.null(one) || nrow(one) == 0) next
    one$edition <- as.integer(yr)
    if (is.null(g)) { g <- one; newest <- as.integer(yr); next }
    keep <- intersect(names(g), names(one)); one <- one[!one$NUTS_ID %in% g$NUTS_ID, keep]
    if (nrow(one)) g <- rbind(g[, keep], one)
  }
  if (is.null(g)) return(NULL)
  g$current <- g$edition == newest; attr(g, "current_edition") <- newest
  g
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
    watch = er_files,                                           # its figures and population live in the shared Eurostat folder
    rules = "2",                                                # 2 (16 Nov 2026): polygons of every NUTS edition, the old ones marked `current = FALSE`
    freshness_months = 36,                                      # the regional table arrives about two years after the year it describes
    level_labels = list(region = c(row$nuts2_en, row$nuts2_local, row$nuts2_en_s, row$nuts2_local_s), province = c(row$nuts3_en, row$nuts3_local, row$nuts3_en_s, row$nuts3_local_s)),
    source = list(name = "Eurostat crim_gen_reg (police-recorded offences by NUTS 3 region)", publisher = "Eurostat, from the national authorities",
                  url = "https://ec.europa.eu/eurostat/databrowser/view/crim_gen_reg/default/table?lang=en", licence = "CC BY 4.0 (Eurostat)",
                  measure = "Police-recorded offences for 7 ICCS categories, yearly; national definitions mapped to ICCS by each country (no official total: the total shown is the sum of the 7 categories)."),
    population_source = "Eurostat demo_r_pjangrp3 and, before 2014, demo_r_pjanaggr3 (1 January)", boundaries_attribution = "© EuroGeographics for the administrative boundaries (Eurostat GISCO NUTS)",
    not_published = c("Not reported to Eurostat for this region", "Not reported to Eurostat for this region"),
    notes_en = "Regional detail and starting years differ by country; some countries report NUTS 2 or the country only.",
    method = list(where = "NUTS 2 / NUTS 3 region as defined by each national authority (usually where the offence was committed; some countries use the place of registration)", date = "Calendar year under each country's own counting date (report, recording or closure of the case)", revisions = "Yearly Eurostat update; countries revise their earlier years and Eurostat replaces the whole series"),
    default_region = if (nzchar(row$default_region)) paste(cc, row$default_region, sep = "-") else NULL,
    download = function() er_download(),
    territory = function(con) {
      d <- er_data(); d <- d[d$cntr == geo, ]; p <- er_population(); p <- p[p$cntr == geo, ]
      codes <- unique(rbind(d[, c("geo", "geo_label")], p[, c("geo", "geo_label")]))
      codes <- codes[!grepl("Z$", codes$geo) & nchar(codes$geo) %in% c(2, 4, 5), ]           # NUTS 0, 2, 3 (no extra-regio)
      if (nrow(codes) == 0) stop("Eurostat has no regional data for ", cc, " in the files on disk")
      ids <- er_id(cc, codes$geo); lvl <- er_level(codes$geo)
      parent <- ifelse(lvl == "country", NA, ifelse(lvl == "region", cc, er_id(cc, substr(codes$geo, 1, 4))))
      tbl <- data.frame(territory_id = ids, level = lvl, parent_id = parent, official_code = codes$geo, name = codes$geo_label)
      tbl$name[tbl$level == "country"] <- row$name_en
      g2 <- er_nuts_all(2); g3 <- er_nuts_all(3)
      save_country_territory(con, cc, tbl, scheme = paste("NUTS", if (is.null(g3)) CONFIG$gisco$nuts_year else attr(g3, "current_edition")))
      if (!is.null(g2) && !is.null(g3)) {
        g2 <- g2[g2$CNTR_CODE == geo, ]; g3 <- g3[g3$CNTR_CODE == geo, ]
        g2$territory_id <- er_id(cc, g2$NUTS_ID); g2$name <- g2$NAME_LATN
        g3$territory_id <- er_id(cc, g3$NUTS_ID); g3$name <- g3$NAME_LATN
        # A code of an older edition is kept only if the figures use it: one that nothing uses would lie on the map
        # over the current one (Lithuania's ten counties of NUTS 2013, P-057)
        g2 <- g2[g2$current | g2$territory_id %in% tbl$territory_id, ]; g3 <- g3[g3$current | g3$territory_id %in% tbl$territory_id, ]
        save_country_gpkg(con, cc, list(region = g2[, c("territory_id", "name", "current")], province = g3[, c("territory_id", "name", "current")]), tolerance_m = 0)
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
