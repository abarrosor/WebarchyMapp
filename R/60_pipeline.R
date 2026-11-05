# Project operations (what WEBARCHYMAPP.R and scripts/ run) -----------------------------
# Every function opens and closes the database on its own: they can be run in
# any order and repeated safely.

# --- Single steps -----------------------------------------------------------------

test_api <- function() {
  res <- tryCatch(ckan_resources(CONFIG$sources$madrid_calls$ckan_id), error = function(e) {
    message("The API did NOT answer as expected:\n  ", conditionMessage(e)); NULL })
  if (!is.null(res)) {
    message("The API works. Resources of the Madrid calls dataset: ", nrow(res), " (", sum(res$format == "csv"), " CSV).")
    print(res[, c("description", "format", "modified")], n = Inf)
  }
  invisible(!is.null(res))
}

# Try an expression and report instead of stopping the whole update
attempt <- function(label, expr) tryCatch(expr, error = function(e) message(label, ": ", conditionMessage(e), "\n(The rest continues.)"))

update_territory <- function(download = TRUE) {
  with_db(function(con) {
    step("Territories (official district and neighbourhood boundaries)")
    if (download) attempt("Madrid boundaries download not available", madrid_territory_download())
    madrid_territory_save(con)
    if (download) attempt("Barcelona boundaries download not available", bcn_territory_download())
    attempt("Barcelona neighbourhoods not loaded", bcn_territory_save(con))
    build_marts(con)
  })
}

# Steps for the Madrid sources. Each step fails on its own without stopping the others.
madrid_steps <- function(con, download = TRUE) {
  fetch <- function(label, expr) if (download) tryCatch(expr, error = function(e)
    message("Download not available (", label, "): ", conditionMessage(e), "\nContinuing with the files already in data/raw."))

  if (city_neighbourhood_count(con, ID_MADRID) == 0) {
    step("Madrid territory (first load)")
    have_zip <- length(list.files(CONFIG$sources$madrid_neighbourhoods$dir_raw, "Barrios\\.zip$")) > 0 &&
                length(list.files(CONFIG$sources$madrid_districts$dir_raw, "Distritos\\.zip$")) > 0
    if (download || !have_zip) fetch("Madrid boundaries", madrid_territory_download())
    madrid_territory_save(con)
  }
  step("Madrid Municipal Police calls (F1)")
  fetch("Madrid calls", madrid_calls_download())
  madrid_calls_ingest(con)

  step("Madrid population by neighbourhood (municipal register)")
  fetch("Madrid register", madrid_census_download())
  attempt("Register not loaded (the app works without rates per 1,000 residents)", madrid_census_ingest(con))

  step("Madrid Municipal Police activity by district (F2)")
  fetch("Madrid activity", madrid_activity_download())
  attempt("Activity not loaded", madrid_activity_ingest(con))
}

# Steps for Barcelona and the Mossos d'Esquadra. (The Crime Balance, the INE population and the
# IGN boundaries are the Spain country module, R/71_es.R, run by country_steps() like every country.)
other_steps <- function(con, download = TRUE) {
  fetch <- function(label, expr) if (download) tryCatch(expr, error = function(e)
    message("Download not available (", label, "): ", conditionMessage(e)))

  step("Barcelona: neighbourhoods, register and city police incidents (F5)")
  if (city_neighbourhood_count(con, ID_BARCELONA) == 0 || !file.exists(GPKG_BARCELONA)) {
    fetch("Barcelona boundaries", bcn_territory_download())
    attempt("Barcelona neighbourhoods not loaded", bcn_territory_save(con))
  }
  fetch("Barcelona register", bcn_census_download())
  attempt("Barcelona register not loaded", bcn_census_ingest(con))
  fetch("Barcelona incidents", bcn_incidents_download())
  attempt("Barcelona incidents not loaded", bcn_incidents_ingest(con))

  step("Mossos d'Esquadra (F4)")
  fetch("Mossos", mossos_download())
  attempt("Mossos not loaded", mossos_ingest(con))
  fetch("Mossos police areas", abp_download())
  attempt("Police areas (ABP) not saved", abp_save(con))
}

update_other_sources <- function(download = TRUE) {
  with_db(function(con) {
    other_steps(con, download)
    step("Tables for the app")
    build_marts(con)
    data_status(con)
  })
}

# --- Full routine -----------------------------------------------------------------

update_all <- function(download = TRUE) {
  check_packages()
  t0 <- Sys.time()
  with_db(function(con) {
    madrid_steps(con, download)
    other_steps(con, download)
    if (length(COUNTRY_MODULES) > 0) country_steps(con, download)
    if (exists("eurostat_rates_update", mode = "function")) eurostat_rates_update(con, download)
    step("Tables for the app")
    build_marts(con)
    data_status(con)
  })
  message(sprintf("\nFinished in %.0f seconds. You can open the app now.", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(TRUE)
}

# --- Data status --------------------------------------------------------------------

data_status <- function(con = NULL) {
  if (is.null(con)) return(invisible(with_db(data_status)))
  q <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
  mon <- function(d) if (is.null(d) || is.na(d)) "-" else format(d, "%Y-%m")
  line <- function(cond, label, text) message(if (isTRUE(cond)) "[ok] " else "[!!] ", label, strrep(" ", max(1, 20 - nchar(label))), text)
  pop_of <- function(id) q(paste0("SELECT max(ref_date) AS d, sum(population) FILTER (ref_date = (SELECT max(ref_date) FROM ref_population WHERE territory_id LIKE '",
                                  id, "-%')) AS n FROM ref_population WHERE territory_id LIKE '", id, "-%'"))
  terr_of <- function(id) q(paste0("SELECT count(*) FILTER (level = 'neighbourhood') AS nb, count(*) FILTER (level = 'district') AS di FROM dim_territory WHERE territory_id LIKE '", id, "-%'"))
  pop_text <- function(p) if (is.null(p) || is.na(p$d)) "not loaded (no rates per 1,000 residents)" else paste0(fmt_int(p$n), " residents at ", format(p$d, "%Y-%m-%d"))

  step("Data status")
  message("Madrid")
  f1 <- q("SELECT min(month) AS from_m, max(month) AS to_m, count(DISTINCT month) AS months, sum(events) AS n FROM stg_madrid_calls")
  unmapped <- q("SELECT count(DISTINCT call_type) AS n FROM v_madrid_calls WHERE block = 'unmapped'")$n
  pm <- pop_of(ID_MADRID); tm <- terr_of(ID_MADRID)
  line(isTRUE(f1$months > 0), "Calls (F1):", if (is.null(f1) || is.na(f1$months) || f1$months == 0) "not loaded" else
    sprintf("%s months (%s to %s), %s calls", f1$months, mon(f1$from_m), mon(f1$to_m), fmt_int(f1$n)))
  line(!is.null(pm) && !is.na(pm$d), "Population:", pop_text(pm))
  line(isTRUE(tm$nb == 131), "Neighbourhoods:", sprintf("%s districts and %s neighbourhoods", tm$di, tm$nb))
  line(isTRUE(unmapped == 0), "Call types:", if (isTRUE(unmapped == 0)) "all mapped" else
    paste(unmapped, "without equivalence: add them to reference/madrid_call_types.csv (GUIDE.md, section 4)"))
  f2 <- q("SELECT min(month) AS from_m, max(month) AS to_m, count(DISTINCT month) AS months FROM stg_madrid_activity")
  f2_missing <- length(list.files(CONFIG$sources$madrid_activity$dir_raw, "\\.xlsx?$")) - (f2$months %||% 0)
  line(isTRUE(f2$months > 0) && f2_missing <= 0, "Activity (F2):", if (is.null(f2) || is.na(f2$months) || f2$months == 0) "not loaded" else
    paste0(f2$months, " months (", mon(f2$from_m), " to ", mon(f2$to_m), ")",
           if (f2_missing > 0) paste0("; ", f2_missing, " Excel file(s) without a recognisable month (run option 1 again to rebuild the index)") else ""))

  message("Barcelona")
  f5 <- q("SELECT min(month) AS from_m, max(month) AS to_m, count(DISTINCT month) AS months, sum(incidents) AS n,
                  count(DISTINCT code) FILTER (block = 'unmapped') AS unmapped FROM v_bcn_incidents")
  f4 <- q(paste0("SELECT min(month) AS from_m, max(month) AS to_m, count(DISTINCT month) AS months, sum(known) AS n,
                  (SELECT count(DISTINCT offence) FROM v_mossos WHERE group_id = 'unmapped') AS unmapped
                  FROM v_mossos WHERE territory_id LIKE '", ID_BARCELONA, "%'"))
  pb <- pop_of(ID_BARCELONA); tb <- terr_of(ID_BARCELONA)
  line(isTRUE(f5$months > 0) && isTRUE(f5$unmapped == 0), "City police (F5):", if (is.null(f5) || is.na(f5$months) || f5$months == 0) "not loaded" else
    paste0(sprintf("%s months (%s to %s), %s incidents", f5$months, mon(f5$from_m), mon(f5$to_m), fmt_int(f5$n)),
           if (f5$unmapped > 0) paste0("; ", f5$unmapped, " type(s) without equivalence (reference/bcn_incident_types.csv)") else ""))
  line(isTRUE(f4$months > 0) && isTRUE(f4$unmapped == 0), "Mossos (F4):", if (is.null(f4) || is.na(f4$months) || f4$months == 0) "not loaded" else
    paste0(sprintf("%s months (%s to %s), %s known offences in the city", f4$months, mon(f4$from_m), mon(f4$to_m), fmt_int(f4$n)),
           if (f4$unmapped > 0) paste0("; ", f4$unmapped, " type(s) without equivalence (reference/mossos_types.csv)") else ""))
  line(!is.null(pb) && !is.na(pb$d), "Population:", pop_text(pb))
  line(isTRUE(tb$nb == 73), "Neighbourhoods:", sprintf("%s districts and %s neighbourhoods", tb$di, tb$nb))

  message("Spain")
  f3 <- q("SELECT count(DISTINCT year * 10 + quarter) AS n, min(year * 10 + quarter) AS from_q, max(year * 10 + quarter) AS to_q,
                  count(DISTINCT offence_type_source) FILTER (offence_type_id = 'unmapped') AS unmapped_types,
                  count(DISTINCT geography) FILTER (territory_id IS NULL AND level <> 'abroad') AS unknown_terr
           FROM v_crime_balance")
  f3m <- q("SELECT count(DISTINCT territory_id) AS n FROM mart_crime_balance WHERE level = 'municipality'")$n %||% 0
  ine <- q("SELECT min(year) AS from_y, max(year) AS to_y, count(DISTINCT territory_id) AS n FROM ref_population_municipality")
  qtr <- function(x) if (is.null(x) || is.na(x)) "-" else sprintf("%dQ%d", x %/% 10, x %% 10)
  line(isTRUE(f3$n > 0) && isTRUE(f3$unmapped_types == 0) && isTRUE(f3$unknown_terr == 0), "Crime Balance (F3):",
       if (is.null(f3) || f3$n == 0) "not loaded" else
         paste0(f3$n, " quarters (", qtr(f3$from_q), " to ", qtr(f3$to_q), "), ", fmt_int(f3m), " municipalities in the app tables",
                if (f3$unmapped_types > 0) paste0("; ", f3$unmapped_types, " type(s) without equivalence (reference/es_offence_types.csv)") else "",
                if (f3$unknown_terr > 0) paste0("; ", f3$unknown_terr, " unidentified territory(ies) (see reports/quality_crime_balance)") else ""))
  n_geo <- tryCatch(nrow(sf::st_read(GPKG_SPAIN, layer = "municipality", quiet = TRUE)), error = function(e) 0L)
  line(n_geo > 0, "Spain map:", if (n_geo == 0) "no municipal boundaries (the Spain tab works without a map)" else
    sprintf("%s municipalities with boundaries (IGN)", fmt_int(n_geo)))
  abp <- q("SELECT count(DISTINCT territory_id) AS n FROM mart_population_abp")
  abp_missing <- q("SELECT count(*) AS n FROM ref_mossos_abp_map WHERE abp_code IS NULL AND territory_id IS NULL AND upper(abp) NOT IN ('ABP VIRTUAL', 'ABP BARCELONA')")
  line(isTRUE(abp$n >= 60) && isTRUE(abp_missing$n == 0), "Mossos by ABP:",
       if (is.null(abp) || abp$n == 0) "no police areas (municipality-to-ABP table missing)" else
         paste0(abp$n, " police areas with population", if (file.exists(GPKG_ABP)) " and boundaries" else " (no boundaries)",
                if (isTRUE(abp_missing$n > 0)) paste0("; ", abp_missing$n, " ABP name(s) without code (reference/mossos_abp.csv)") else ""))
  line(isTRUE(ine$n > 0), "INE population:", if (is.null(ine) || is.na(ine$n) || ine$n == 0) "not loaded (no rates by municipality)" else
    sprintf("%s municipalities, years %s to %s", fmt_int(ine$n), ine$from_y, ine$to_y))

  if (length(COUNTRY_MODULES) > 0 && exists("country_status", mode = "function")) country_status(con)

  rep <- newest_first(list.files(CONFIG$dir_reports, "^quality_.*\\.csv$", full.names = TRUE))
  if (length(rep) > 0) message("  Quality reports in ", CONFIG$dir_reports, "/ (latest: ", basename(rep[1]), ")")
  invisible(TRUE)
}

# --- App and rebuild ---------------------------------------------------------------

open_app <- function(lang = "en") {
  check_packages()
  shiny::runApp("app", launch.browser = function(url) utils::browseURL(paste0(url, "?lang=", lang)))
}

rebuild_database <- function(ask = interactive()) {
  if (ask) {
    r <- readline(paste0(CONFIG$db_path, " will be deleted and rebuilt from data/raw/ (originals are not touched). Continue? (y/n): "))
    if (!tolower(trimws(r)) %in% c("y", "yes", "s", "si")) return(message("Cancelled."))
  }
  if (file.exists(CONFIG$db_path) && !file.remove(CONFIG$db_path)) {
    stop("The database cannot be deleted: close it (stop the app and do Session > Restart R) and try again.", call. = FALSE)
  }
  update_all(download = FALSE)
}
