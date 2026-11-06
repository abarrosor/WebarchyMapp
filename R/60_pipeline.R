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

# Districts and neighbourhoods of every city, rebuilt from the official files (option 7)
update_territory <- function(download = TRUE) {
  with_db(function(con) {
    step("Territories (official district and neighbourhood boundaries)")
    for (m in CITY_MODULES) {
      if (download) attempt(paste(m$name_en, "boundaries download not available"), m$download())
      attempt(paste(m$name_en, "territory not loaded"), m$territory(con, force = TRUE))
    }
    build_marts(con)
  })
}

# --- Full routine -----------------------------------------------------------------
# Cities (R/60_cities.R modules), then countries (R/70_countries.R modules, Spain included), then the
# Eurostat national rates and the tables for the app. Each step fails on its own without stopping the others.
update_all <- function(download = TRUE) {
  check_packages()
  t0 <- Sys.time()
  with_db(function(con) {
    if (length(CITY_MODULES) > 0) city_steps(con, download, after_countries = FALSE)
    if (length(COUNTRY_MODULES) > 0) country_steps(con, download)
    if (length(CITY_MODULES) > 0) city_steps(con, download, after_countries = TRUE)   # cities built on a country's data (Lima and Callao on Peru)
    if (exists("eurostat_rates_update", mode = "function")) eurostat_rates_update(con, download)
    step("Tables for the app")
    build_marts(con)
    data_status(con)
  })
  message(sprintf("\nFinished in %.0f seconds. You can open the app now.", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(TRUE)
}

# --- Data status --------------------------------------------------------------------

# One status for everything: the database, the cities, the countries, the national rates table and
# the last checks report. Details per source: option 15 (checks) and the quality reports.
data_status <- function(con = NULL) {
  if (is.null(con)) return(invisible(with_db(data_status)))
  q <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
  step("Data status")
  last <- q("SELECT max(loaded_at) AS t, count(*) AS n FROM ingest_log")
  message(sprintf("Database: %s (%.0f MB); %s loads logged, the last one %s", CONFIG$db_path, file.size(CONFIG$db_path) / 1e6, fmt_int(last$n %||% 0),
                  if (is.null(last) || is.na(last$t)) "never" else format(last$t, "%Y-%m-%d %H:%M")))
  if (exists("city_status", mode = "function")) city_status(con)
  if (exists("country_status", mode = "function")) country_status(con)
  if ("mart_country_rates" %in% DBI::dbListTables(con)) {
    r <- q("SELECT count(DISTINCT country) AS n, max(year) AS y FROM mart_country_rates")
    if (!is.null(r) && isTRUE(r$n > 0)) message(sprintf("Countries table: national rates of %d countries, up to %d (Eurostat)", r$n, r$y))
  }
  chk <- newest_first(list.files(CONFIG$dir_reports, "^country_checks_.*\\.csv$", full.names = TRUE))
  message(if (length(chk)) paste0("Checks: option 15 (last report ", basename(chk[1]), ")") else "Checks: option 15 (no report yet)")
  rep <- newest_first(list.files(CONFIG$dir_reports, "^quality_.*\\.csv$", full.names = TRUE))
  if (length(rep) > 0) message("Quality reports of the loads in ", CONFIG$dir_reports, "/ (latest: ", basename(rep[1]), ")")
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
