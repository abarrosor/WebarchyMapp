# The small helpers every module relies on --------------------------------------------------------------------
use_test_project()

test_that("normalize_text folds case, accents and spaces and turns the null words into NA", {
  expect_equal(normalize_text(c("  Nuñez ", "VILLA GRAL. MITRE", "Chacarita", "Agronomía")), c("NUÑEZ", "VILLA GRAL. MITRE", "CHACARITA", "AGRONOMIA"))
  expect_equal(normalize_text(c("NULL", "", "#NULL", "DESCONOCIDO", NA)), rep(NA_character_, 5))
  expect_equal(normalize_text("a   b"), "A B")
})

test_that("file_age_days: missing files are infinitely old and the refresh option makes every file old", {
  expect_equal(file_age_days(character()), Inf)
  expect_equal(file_age_days(NA_character_), Inf)
  expect_equal(file_age_days(file.path(tempdir(), "does-not-exist.csv")), Inf)
  f <- tempfile(fileext = ".csv"); writeLines("a", f)
  expect_lt(file_age_days(f), 1)
  old <- options(webarchy.refresh = TRUE); on.exit(options(old), add = TRUE)
  expect_equal(file_age_days(f), Inf)
})

test_that("apply_threshold hides every figure under the project threshold (10) and keeps the others", {
  expect_equal(MARTS$threshold, 10L)
  expect_equal(apply_threshold(c(0, 3, 9, 10, 11, NA)), c(NA, NA, NA, 10, 11, NA))
})

test_that("yearly and monthly figures become the shared cumulative shape", {
  y <- yearly_to_cumulative(data.frame(country = "XT", territory_id = "XT", level = "country", offence_code = "A", year = 2024:2025, n = c(10, 12)))
  expect_equal(y$quarter, c(4L, 4L)); expect_equal(y$cumulative, c(10, 12)); expect_null(y$n)
  m <- data.frame(country = "XT", territory_id = "XT", level = "country", offence_code = "A", year = 2025L, month = 1:8, n = 1:8)
  cum <- monthly_to_cumulative(m)
  expect_true(all(cum$quarter %in% 1:2))                     # months 7-8 are an unfinished quarter: left out
  expect_equal(cum$cumulative[order(cum$quarter)], c(sum(1:3), sum(1:6)))
})

test_that("clean_polygons repairs a ring that crosses itself and gives areas the spherical engine agrees with", {
  bow <- sf::st_polygon(list(matrix(c(8.50, 47.35, 8.52, 47.37, 8.52, 47.35, 8.50, 47.37, 8.50, 47.35), ncol = 2, byrow = TRUE)))
  g <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(bow, square(8.55, 47.35), crs = 4326))
  expect_error(sf::st_area(g), "not valid")                          # what the live Zürich layer did
  c2 <- clean_polygons(g)
  expect_true(all(sf::st_is_valid(c2)))
  expect_equal(sf::st_crs(c2)$epsg, 4326)
  expect_true(all(c2$area_m2 > 0))
  expect_equal(as.numeric(sf::st_area(c2)) / c2$area_m2, c(1, 1), tolerance = 0.01)
  expect_s3_class(dissolve_territories(c2, c("a", "a")), "sf")
})

test_that("read_reference turns yes/no columns into logicals and keeps the rest as text", {
  t <- read_reference("toronto_event_types.csv", bool_cols = "sensitive")
  expect_type(t$sensitive, "logical"); expect_true(t$sensitive[t$source_code == "HOMICIDE"]); expect_false(t$sensitive[t$source_code == "ASSAULT"])
  expect_type(t$block, "character")
})

# A tiny web server in a second R process (httpuv in this process could not answer while curl blocks R):
# it serves the content of a file, so that the test changes the answer by rewriting the file, and it stops
# when a stop file appears or after 90 s
local_web_server <- function() {
  skip_if_not_installed("httpuv")
  dir <- tempfile("srv"); dir.create(dir)
  body <- file.path(dir, "body.txt"); type <- file.path(dir, "type.txt"); stop_file <- file.path(dir, "stop")
  writeLines("a,b\n1,2", body, sep = ""); writeLines("text/csv", type)
  port <- httpuv::randomPort()
  script <- file.path(dir, "server.R")
  writeLines(sprintf('srv <- httpuv::startServer("127.0.0.1", %d, list(call = function(req) list(status = 200L, headers = list("Content-Type" = readLines("%s", warn = FALSE)[1]), body = paste0(paste(readLines("%s", warn = FALSE), collapse = "\\n"), "\\n"))))
t0 <- Sys.time(); while (!file.exists("%s") && difftime(Sys.time(), t0, units = "secs") < 90) httpuv::service(100); httpuv::stopServer(srv)',
                    port, gsub("\\\\", "/", type), gsub("\\\\", "/", body), gsub("\\\\", "/", stop_file)), script)
  system2(file.path(R.home("bin"), "Rscript"), shQuote(script), wait = FALSE, stdout = FALSE, stderr = FALSE)
  url <- sprintf("http://127.0.0.1:%d/file.csv", port)
  up <- FALSE
  for (i in 1:50) { up <- tryCatch({ httr2::req_perform(httr2::req_timeout(httr2::request(url), 2)); TRUE }, error = function(e) FALSE); if (up) break; Sys.sleep(0.2) }
  if (!up) { file.create(stop_file); skip("the local web server did not start") }
  list(url = url, set = function(content, content_type = "text/csv") { writeLines(content, body, sep = ""); writeLines(content_type, type) }, stop = function() file.create(stop_file))
}

test_that("a connection that fails is tried once more over IPv4 only, and the error is kept when that fails too", {
  msgs <- character()
  err <- tryCatch(withCallingHandlers(download_safely("http://127.0.0.1:1/none.csv", tempfile(fileext = ".csv"), timeout = 5), message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }),
                  error = function(e) conditionMessage(e))
  expect_true(any(grepl("IPv4", msgs)), info = paste(msgs, collapse = " / "))
  expect_true(grepl("connect", err, ignore.case = TRUE))
})

test_that("the download layer keeps files it completes, refuses HTML pages and never saves an identical copy twice", {
  srv <- local_web_server(); on.exit(srv$stop(), add = TRUE)
  url <- srv$url
  dir <- tempfile("dl"); dir.create(dir)
  f1 <- quiet(download_versioned(url, dir, name = "file.csv"))
  expect_true(file.exists(f1)); expect_equal(readLines(f1), c("a,b", "1,2"))
  f2 <- quiet(download_versioned(url, dir, name = "file.csv"))        # same content: the first copy is kept, no second file
  expect_equal(normalizePath(f1), normalizePath(f2)); expect_length(list.files(dir), 1)
  srv$set("a,b\n1,3")
  f3 <- quiet(download_versioned(url, dir, name = "file.csv"))        # new content: a new dated copy next to the old one
  expect_length(list.files(dir), 2); expect_equal(readLines(f3), c("a,b", "1,3"))
  srv$set("<!DOCTYPE html><html><body>Please log in</body></html>", "text/html")
  expect_error(download_safely(url, file.path(dir, "page.csv")), "HTML page")
  expect_false(file.exists(file.path(dir, "page.csv")))
  dest <- file.path(dir, "exists.csv"); writeLines("x", dest)
  srv$set("a,b\n1,2")
  expect_error(download_safely(url, dest), "never overwritten")
})

test_that("de_count reads the whole numbers of the German tables and never takes a thousands mark for a decimal", {
  expect_equal(de_count(c("572,173", "1,282,441", "5,555,520", "401", "0")), c(572173, 1282441, 5555520, 401, 0))
  expect_equal(de_count(c("572.173", "1.282.441", " 74 524 ", "186894")), c(572173, 1282441, 74524, 186894))
  expect_equal(de_count(c("", "-", "...", NA, "n/a")), rep(NA_real_, 5))
  d <- data.frame(Schluessel = "------", Straftat = "Straftaten insgesamt", Bundesland = "Nordrhein-Westfalen", check.names = FALSE, stringsAsFactors = FALSE)
  d[["erfasste Faelle"]] <- "1,282,441"
  expect_equal(de_standardise(d, "land")$value, 1282441)
})

test_that("de_standardise reads a Kreise table as the workbooks of 2024 write it and leaves out the row that numbers the columns", {
  d <- data.frame(a = c("1", "------", "100000"), b = c("2", "Straftaten insgesamt", "Mord"), c = c("3", "01001", "01001"), d = c("4", "Flensburg", "Flensburg"),
                  e = c("6", "8692", "3"), stringsAsFactors = FALSE)
  names(d) <- c("Schl\u00fcssel", "Straftat", "Gemeinde-schl\u00fcssel", "Stadt-/Landkreis", "Anzahl erfasste F\u00e4lle")
  out <- de_standardise(d, "kreis")
  expect_equal(out$key, c("------", "100000"))
  expect_equal(out$code, c("01001", "01001"))
  expect_equal(out$value, c(8692, 3))
  # a key written without its leading zeros in the CSV ("0" for 000000, "10000" for 010000) is kept and completed
  d <- data.frame(Schluessel = c("0", "10000", "------", "892500"), Straftat = c("Straftaten gegen das Leben", "Mord", "Straftaten insgesamt", "x"), Bundesland = "Bayern", stringsAsFactors = FALSE)
  d[["erfasste Faelle"]] <- c("648", "276", "635,421", "5")
  expect_equal(de_standardise(d, "land")$key, c("000000", "010000", "------", "892500"))
})
