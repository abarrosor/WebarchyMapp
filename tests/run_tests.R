# Automated tests of WebarchyMapp -----------------------------------------------------------------------------
# Runs the testthat suite in tests/testthat/. The tests work in a TEMPORARY COPY of the code (R/, reference/,
# app/, scripts/, data_seed/) with their own synthetic files and their own DuckDB file under tempdir():
# nothing in data/, reports/ or the real database is read or written, and the app can stay open.
#
# From the menu: option 16 (a separate R process, so nothing of the tests stays in the session). From a console at
# the project root: source("tests/run_tests.R").
# One file: run_tests("city-framework") (the part of the file name after "test-").
# What is covered: the helpers (normalize_text, file_age_days, apply_threshold, the cumulative conversions,
# the download layer against a local web server), the country framework on a synthetic country (the quarterly
# figures of the mart, sensitive types, population), the city framework on synthetic Zürich, Toronto, Buenos
# Aires and Utrecht files (the marts never hold sensitive types, the latest register wins, the threshold, the
# Dutch generator's kept territory and its population), the
# reference and translation tables (the 32 languages: placeholders, names quoted exactly, one word per concept),
# the consistency checks, the app (every view renders in every language; the time series, its period bar and
# the chart by rate; the country table ordered by its column titles; the box and the card over a map; the citation
# of a source), and what keeps an update from failing in silence (the summary at the end of a run and its memory of
# the last one, the freshness of the sources, the rules mark of a country, the versions, the notes on the scheduled
# task, the listing of old copies, the guards of Madrid's historical register).

run_tests <- function(filter = NULL, reporter = "progress") {
  if (!file.exists("WebarchyMapp.Rproj")) stop("Run the tests from the project root (the folder of WebarchyMapp.Rproj)")
  for (pkg in c("testthat", "writexl"))                                   # writexl writes the synthetic INDEC workbook of the Buenos Aires test
    if (!requireNamespace(pkg, quietly = TRUE)) { message("Installing ", pkg, " (free, CRAN)..."); utils::install.packages(pkg) }
  root <- normalizePath(".")
  options(webarchy.project_root = root)
  on.exit({
    setwd(root)
    # the synthetic test country must not survive in an interactive session (option 13 would try to load it)
    cm <- get0("COUNTRY_MODULES", envir = globalenv()); if (!is.null(cm$XT)) { cm$XT <- NULL; assign("COUNTRY_MODULES", cm, envir = globalenv()) }
    options(webarchy.fixtures_loaded = FALSE)
  }, add = TRUE)
  t0 <- Sys.time()
  res <- testthat::test_dir(file.path(root, "tests", "testthat"), filter = filter, reporter = reporter, stop_on_failure = FALSE, stop_on_warning = FALSE)
  df <- as.data.frame(res)
  failed <- sum(df$failed) + sum(df$error)
  message(sprintf("\nTests: %d passed, %d failed, %d skipped in %.0f s (temporary copy in %s)",
                  sum(df$passed), failed, sum(df$skipped), as.numeric(difftime(Sys.time(), t0, units = "secs")), getOption("webarchy.test_root", "tempdir()")))
  if (failed > 0) message("Something changed in the code or the tables: read the failures above (each names its test file).")
  invisible(res)
}

# options(webarchy.run_tests_now = FALSE) before sourcing this file only defines run_tests() without running it
if (!isFALSE(getOption("webarchy.run_tests_now"))) {
  res <- run_tests()
  df <- as.data.frame(res)
  if (!interactive() && sum(df$failed) + sum(df$error) > 0) quit(save = "no", status = 1)   # Rscript / the menu see a failure as status 1
}
