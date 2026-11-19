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

# attempt() (try a step and report instead of stopping the whole update) and try_download() live in
# R/05_journal.R: what they report is also collected for the summary at the end of the run.

# Districts and neighbourhoods of every city, rebuilt from the official files (option 7)
update_territory <- function(download = TRUE) {
  with_journal("territories", download = download, with_db(function(con) {
    step("Territories (official district and neighbourhood boundaries)")
    for (m in CITY_MODULES) {
      if (download) attempt(paste(m$name_en, "boundaries download not available"), m$download())
      attempt(paste(m$name_en, "territory not loaded"), m$territory(con, force = TRUE))
    }
    build_marts(con)
  }))
}

# --- Full routine -----------------------------------------------------------------
# Cities (R/60_cities.R modules), then countries (R/70_countries.R modules, Spain included), then the
# tables for the app. Each step fails on its own without stopping the others.
# The run journal (R/05_journal.R) listens to the whole run: what did not go as planned is said together at the end.
update_all <- function(download = TRUE) {
  check_packages()
  t0 <- Sys.time()
  with_journal("everything", download = download, with_db(function(con) {
    if (length(CITY_MODULES) > 0) city_steps(con, download, after_countries = FALSE)
    if (length(COUNTRY_MODULES) > 0) country_steps(con, download)
    if (length(CITY_MODULES) > 0) city_steps(con, download, after_countries = TRUE)   # cities built on a country's data (Lima and Callao on Peru)
    step("Tables for the app")
    build_marts(con)
    data_status(con)
  }))
  message(sprintf("\nFinished in %.0f seconds. You can open the app now.", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  invisible(TRUE)
}

# --- Data status --------------------------------------------------------------------

# One status for everything: the database, the cities, the countries and
# the last checks report. Details per source: option 15 (checks) and the quality reports.
data_status <- function(con = NULL) {
  if (is.null(con)) return(invisible(with_db(data_status)))
  q <- function(sql) tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
  step("Data status")
  last <- q("SELECT max(loaded_at) AS t, count(*) AS n FROM ingest_log")
  message(sprintf("Database: %s (%.0f MB); %s loads logged, the last one %s", CONFIG$db_path, file.size(CONFIG$db_path) / 1e6, fmt_int(last$n %||% 0),
                  if (is.null(last) || is.na(last$t)) "never" else format(last$t, "%Y-%m-%d %H:%M", tz = "")))       # the database keeps UTC: said in the time of this computer
  if (exists("city_status", mode = "function")) city_status(con)
  if (exists("country_status", mode = "function")) country_status(con)
  # The automatic update, from its logs alone (no question to Windows here: option 11 asks it). Said only when there is
  # a log: a run that failed or did not finish must not wait for someone to look at option 11.
  lg <- tryCatch(last_update_log(), error = function(e) NULL)
  if (!is.null(lg)) journal_mute(message(sprintf("%s Automatic update: last log %s (%s): %s", if (identical(lg$result, "unfinished") || grepl("^error", lg$result)) "[!!]" else "    ",
                                                 basename(lg$file), format(lg$date, "%Y-%m-%d %H:%M"), lg$result)))
  chk <- newest_first(list.files(CONFIG$dir_reports, "^country_checks_.*\\.csv$", full.names = TRUE))
  message(if (length(chk)) paste0("Checks: option 15 (last report ", basename(chk[1]), ")") else "Checks: option 15 (no report yet)")
  rep <- newest_first(list.files(CONFIG$dir_reports, "^quality_.*\\.csv$", full.names = TRUE))
  if (length(rep) > 0) message("Quality reports of the loads in ", CONFIG$dir_reports, "/ (latest: ", basename(rep[1]), ")")
  invisible(TRUE)
}

# --- App and rebuild ---------------------------------------------------------------

# The app at a fixed address, http://127.0.0.1:<CONFIG$app_port> (P-068): the same link works from one day to the
# next, where shiny alone picks a port at random each time. When that port is taken (the app is already open in
# another R session, or another program uses it) the app opens on a free one and says so: nothing is closed or
# stopped to make room.
port_taken <- function(e) grepl("failed to create server|address already in use|EADDRINUSE", conditionMessage(e), ignore.case = TRUE)
run_app_at <- function(port, go, run = shiny::runApp) {
  if (is.null(port) || is.na(port)) return(run("app", launch.browser = go))
  tryCatch(run("app", port = as.integer(port), launch.browser = go), error = function(e) {
    if (!port_taken(e)) stop(e)
    message("Port ", port, " is in use (is the app already open in another R session?). Opening the app on a free port this time: its address is the one R prints below.")
    run("app", launch.browser = go)
  })
}
open_app <- function(lang = "en", port = CONFIG$app_port) {
  check_packages()
  # Opened from the menu, the app stops by itself some seconds after its last page is closed in the browser (app/app.R)
  old <- options(webarchy.stop_when_closed = 15); on.exit(options(old), add = TRUE)
  run_app_at(port, function(url) utils::browseURL(paste0(url, "?lang=", lang)))
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
