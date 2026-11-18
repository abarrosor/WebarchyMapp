# Test harness: a temporary copy of the project -------------------------------------------------------------------
# testthat sources this file before the tests. It copies the code (R/, reference/, app/, scripts/, data_seed/,
# the .Rproj) into a folder under tempdir(), moves there and loads the project the way the menu does
# (scripts/_bootstrap.R). Every relative path of the project (data/raw, data/webarchymapp.duckdb, reports/,
# data/territory_<key>.gpkg) then resolves inside the copy: the real data folder is never touched.

TEST_ROOT <- file.path(tempdir(), "webarchymapp-tests")

project_root <- function() {
  r <- getOption("webarchy.project_root")
  if (!is.null(r) && file.exists(file.path(r, "WebarchyMapp.Rproj"))) return(r)
  d <- normalizePath(".", mustWork = FALSE)
  for (i in 0:3) { if (file.exists(file.path(d, "WebarchyMapp.Rproj"))) return(d); d <- dirname(d) }
  stop("project root not found: run the tests with tests/run_tests.R from the project folder")
}

test_project <- function() {
  if (identical(getOption("webarchy.test_root"), TEST_ROOT) && file.exists(file.path(TEST_ROOT, "WebarchyMapp.Rproj"))) { setwd(TEST_ROOT); return(invisible(TEST_ROOT)) }
  src <- project_root()
  unlink(TEST_ROOT, recursive = TRUE); dir.create(TEST_ROOT, recursive = TRUE, showWarnings = FALSE)
  for (d in c("R", "reference", "app", "scripts", "data_seed")) if (dir.exists(file.path(src, d))) file.copy(file.path(src, d), TEST_ROOT, recursive = TRUE)
  file.copy(file.path(src, "WebarchyMapp.Rproj"), TEST_ROOT)
  dir.create(file.path(TEST_ROOT, "data"), showWarnings = FALSE); dir.create(file.path(TEST_ROOT, "reports"), showWarnings = FALSE)
  setwd(TEST_ROOT)
  suppressMessages(source("scripts/_bootstrap.R", encoding = "UTF-8"))   # sources every R/*.R into the global environment
  options(webarchy.test_root = TEST_ROOT, webarchy.fixtures_loaded = FALSE)
  invisible(TEST_ROOT)
}

test_project()
# testthat moves to tests/testthat/ before every file: each file starts with use_test_project() to come back
use_test_project <- function() { setwd(TEST_ROOT); invisible(TEST_ROOT) }

# One square polygon (WGS84) for synthetic boundaries
square <- function(x0, y0, s = 0.02) sf::st_polygon(list(matrix(c(x0, y0, x0 + s, y0, x0 + s, y0 + s, x0, y0 + s, x0, y0), ncol = 2, byrow = TRUE)))
quiet <- function(expr) suppressMessages(suppressWarnings(expr))

# Helpers that read the test database (the project's own helpers, with its CONFIG paths inside the copy)
db_query <- function(sql) { con <- db_connect_readonly(); on.exit(db_disconnect(con), add = TRUE); DBI::dbGetQuery(con, sql) }
