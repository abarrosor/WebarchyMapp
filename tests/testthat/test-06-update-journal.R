# What an update says at its end, and the tools that keep it from failing in silence -------------------------
# The run journal (R/05_journal.R), the freshness of the sources, the rules mark of a country, the versions written
# down, the notes on the scheduled task, the listing of old copies and the historical register of Madrid (P-057).
use_test_project()

# A reports folder of its own for a test: the journal compares a run with the last one it finds there
with_reports <- function(code) {
  old <- CONFIG$dir_reports; tmp <- tempfile("reports_"); dir.create(tmp)
  assign("CONFIG", utils::modifyList(CONFIG, list(dir_reports = tmp)), envir = globalenv())
  on.exit({ assign("CONFIG", utils::modifyList(CONFIG, list(dir_reports = old)), envir = globalenv()); JOURNAL$active <- FALSE; unlink(tmp, recursive = TRUE) }, add = TRUE)
  force(code)
}
state_of <- function() journal_read_state()

test_that("the journal knows the project's wording for what did not go as planned, and leaves ordinary lines alone", {
  kinds <- c(
    "  [!!] 20261103_cr_policiales_2015.csv: 2 other reading problem(s) reported by readr" = "attention",
    "x 2019_pad_mdbas_sexe.csv not downloaded (HTTP 404): the rates of that year will stand on the nearest register" = "failed",
    "  20261103_cr_policiales_2016.csv: cantones not in reference/cr_cantones.csv (counted at provincia/national level only): GUANACASTE" = "unmapped",
    "  categories without a row in reference/pl_offence_types.csv (left out): X | Y" = "unmapped",
    "  SoortMisdrijf titles not in nl_city_event_types.csv (left out): Nieuwe categorie" = "unmapped",
    "  6 municipality names not matched to IBGE codes (their figures stay in the UF totals; see br_inspect())" = "unmapped",
    "  PKS 2019 land not downloaded (HTTP 404 Not Found.): check the URL in reference/de_sources.csv" = "download",
    "  AR departamentos candidate 2 failed: HTTP 403 Forbidden." = "download",
    "Download not available (CMX): Timeout was reached [datos.cdmx.gob.mx]" = "download",
    "  GISCO LAU not available: communes without boundaries (Failed to perform HTTP request.)" = "download",
    "  KML not readable: cannot open" = "other",
    "  CUT workbook not understood: no sheet" = "other")
  for (line in names(kinds)) expect_equal(journal_kind(line), unname(kinds[[line]]), info = line)
  ordinary <- c("", "= fr_communal.csv.gz unchanged (keeping 20261103_fr_communal.csv.gz)", "+ 20261116_file.csv", ". 200076_20261006_220058.csv (a newer file has the same reference date: skipped)",
                "\n-- Germany (DE): Polizeiliche Kriminalstatistik (not available yet) ---", "Buenos Aires 2026: not published yet (the portal adds the year once it is complete)",
                "  communal: 5,238,000 rows, 34920 territories, 2016-2025", "FR: 35075 territories (country 1, municipality 34955, province 101, region 18)",
                "  CR: 53 municipality territories added without boundaries", "Marts updated: 16 cities, 45 countries.")
  for (line in ordinary) expect_true(is.na(journal_kind(line)), info = line)
})

test_that("two lines are the same line whatever their numbers, the dates of their files and the names of temporary files", {
  a <- journal_key("CR", "load", "attention", "20261103_cr_policiales_2015.csv: 2 record(s) with a comma inside the nationality")
  b <- journal_key("CR", "load", "attention", "20261201_cr_policiales_2021.csv: 13 record(s) with a comma inside the nationality")
  expect_equal(a, b)
  expect_equal(journal_key("X", "download", "download", "Timeout after 10002 ms"), journal_key("X", "download", "download", "Timeout after 21,045 ms"))
  expect_equal(journal_key("X", "load", "warning", "cannot open file '/tmp/RtmpAbC123/file1a2b3c4d'"), journal_key("X", "load", "warning", "cannot open file '/tmp/RtmpZz9/file99ffee00'"))
  expect_false(journal_key("X", "load", "warning", "a") == journal_key("Y", "load", "warning", "a"))            # the place is part of it
  expect_false(journal_key("X", "load", "warning", "a") == journal_key("X", "download", "warning", "a"))        # and the phase
})

test_that("an update collects its failures, its warnings and its downloads under the place that raised them, and still returns its value", {
  with_reports({
    as_int <- function(x) as.integer(x)
    out <- NULL
    msgs <- testthat::capture_messages(expect_no_warning(
      out <- with_journal("test", {
        step("Alpha (AA): a source", place = "AA", label = "Alpha (AA)")
        try_download("AA", stop("Timeout was reached [example.org] after 10002 ms"))
        for (i in 1:5) as_int("a")                                                      # the same warning five times
        as_int("b")
        message("  20261103_x.csv: categories not in reference/x_types.csv (left out): A")
        ok <- attempt("AA data not loaded", stop("boom"))
        expect_false(ok); expect_true(attempt("fine", 1 + 1))
        step("Beta")
        42
      })))
    expect_equal(out, 42)
    expect_false(JOURNAL$active)
    all <- paste(msgs, collapse = "")
    expect_equal(lengths(regmatches(all, gregexpr("warning: NAs introduced by coercion [in as_int(\"a\")]", all, fixed = TRUE))), 1)   # printed once where it happened...
    expect_true(grepl("NAs introduced by coercion [in as_int(\"a\")] (5 times)", all, fixed = TRUE))                                     # ...and counted in the summary
    expect_true(grepl("AA data not loaded: boom\n(The rest continues.)", all, fixed = TRUE))
    expect_true(grepl("Summary of the run: what did not go as planned", all, fixed = TRUE))
    expect_true(grepl("First run with a summary: 5 line(s)", all, fixed = TRUE))
    s <- state_of()
    expect_setequal(s$kind, c("download", "warning", "warning", "unmapped", "failed"))
    expect_true(all(s$place == "AA")); expect_true(all(s$title == "Alpha (AA)"))
    expect_equal(s$phase[s$kind == "download"], "download"); expect_true(all(s$phase[s$kind != "download"] == "load"))
    expect_equal(s$n[grepl("as_int(\"a\")", s$text, fixed = TRUE)], "5")
    expect_match(journal_result(), "^ok, 5 line\\(s\\) in the summary \\(first summary")
    expect_length(list.files(CONFIG$dir_reports, "^run_[0-9]{8}_[0-9]{6}\\.csv$"), 1)
  })
})

test_that("a line that only describes what a module does is not taken for trouble, and readr prints no progress bars", {
  # Poland says which variables it leaves aside: "not downloaded" in that line was read as a failed download (16 November 2026)
  expect_true(is.na(journal_kind("  128 variables (category x period) in P3588, P4601; the 128 detection-rate variables (percentages, not counts) are left aside")))
  expect_equal(journal_kind("  PKS 2019 land not downloaded (HTTP 404 Not Found.): check the URL in reference/de_sources.csv"), "download")
  # In the RStudio console a progress bar of readr glues the next message to the line before it: the project turns them off
  expect_false(isTRUE(getOption("readr.show_progress")))
})

test_that("the journal never stops the step it listens to: bytes that are not text are read as they can be", {
  with_reports({
    bad <- "  [!!] Gu\xe0rdia Urbana: 3 names not in reference/x.csv"                              # a name read in the wrong encoding: Latin-1 bytes, not valid text
    expect_false(validUTF8(bad))
    expect_equal(journal_text(c(bad, "Guàrdia", NA)), c("  [!!] Guàrdia Urbana: 3 names not in reference/x.csv", "Guàrdia", NA))
    out <- NULL
    msgs <- testthat::capture_messages(expect_no_warning(
      out <- with_journal("test", { step("A", place = "AA", label = "Alpha (AA)"); message(bad); warning(bad); "the step went on" })))
    expect_equal(out, "the step went on")
    s <- state_of()
    expect_equal(nrow(s), 2); expect_setequal(s$kind, c("attention", "warning")); expect_true(all(validUTF8(s$text)))
    expect_true(all(grepl("Guàrdia Urbana: 3 names", s$text, fixed = TRUE)))
  })
})

test_that("a listener that fails leaves the run as it would be without a journal: the line is printed, the warning stays R's own", {
  old <- journal_key
  assign("journal_key", function(...) stop("the journal itself is broken"), envir = globalenv())
  on.exit(assign("journal_key", old, envir = globalenv()), add = TRUE)
  with_reports({
    out <- NULL
    msgs <- testthat::capture_messages(expect_warning(out <- with_journal("test", { step("A"); message("x something failed"); warning("kept by R"); 5 }), "kept by R"))
    expect_equal(out, 5)
    expect_true(any(grepl("x something failed", msgs, fixed = TRUE)))
    expect_true(any(grepl("Nothing to report", msgs, fixed = TRUE)))                              # nothing could be recorded, and the run still ends with its summary
  })
})

test_that("the next run says what is new, what stopped happening and what is the same; what did not run is not judged", {
  with_reports({
    run <- function(expr, download = TRUE) paste(testthat::capture_messages(with_journal("test", expr, download = download)), collapse = "")
    run({ step("A", place = "AA", label = "Alpha (AA)"); try_download("AA", stop("no answer")); warning("old warning"); attempt("AA step", stop("boom"))
          step("B", place = "BB", label = "Beta (BB)"); message("  [!!] BB looks wrong") })
    first <- state_of(); expect_equal(nrow(first), 4)
    # second run: the download answers, the old warning is gone, a new one comes, the failure stays; BB is not part of the run
    out <- run({ step("A", place = "AA", label = "Alpha (AA)"); try_download("AA", TRUE); warning("new warning"); attempt("AA step", stop("boom")) })
    expect_true(grepl("NEW in this run: 1 line(s)", out, fixed = TRUE)); expect_true(grepl("new warning", out, fixed = TRUE))
    expect_true(grepl("No longer happening: 2 line(s)", out, fixed = TRUE)); expect_true(grepl("Download not available (AA): no answer", out, fixed = TRUE))
    expect_true(grepl("The same as in earlier runs: 1 line(s) in 1 place(s)", out, fixed = TRUE))
    expect_true(grepl("Kept from earlier runs, not looked at in this one: 1 line(s)", out, fixed = TRUE))
    expect_match(journal_result(), "^ok, 2 line\\(s\\) in the summary, 1 new, 2 no longer happening$")
    s <- state_of()
    expect_true("[!!] BB looks wrong" %in% s$text)                                                # Beta keeps the line of its own last run
    expect_false(any(grepl("old warning|no answer", s$text)))
    expect_equal(s$first_seen[s$text == "AA step: boom"], first$first_seen[first$text == "AA step: boom"])
    # third run without downloads, and with the load kept: nothing of AA is judged
    Sys.sleep(1.1)
    out <- run({ step("A", place = "AA", label = "Alpha (AA)"); try_download("AA", stop("never asked"), download = FALSE); journal_keep("AA") }, download = FALSE)
    expect_true(grepl("Nothing to report: every step of this run did what it had to", out, fixed = TRUE)); expect_false(grepl("No longer happening", out, fixed = TRUE))
    expect_false(grepl("The same as in earlier runs", out, fixed = TRUE))
    expect_true(grepl("Kept from earlier runs, not looked at in this one: 3 line(s)", out, fixed = TRUE))    # the two of AA's load and the one of BB
    expect_equal(journal_result(), "ok")                                                                          # this run saw nothing itself
    expect_setequal(state_of()$text, s$text)
    # a run that stops: the error comes out, the summary is written, and the phase in progress is not called solved
    Sys.sleep(1.1)
    msgs <- testthat::capture_messages(expect_error(with_journal("test", { step("A", place = "AA", label = "Alpha (AA)"); stop("fatal thing") }), "fatal thing"))
    expect_true(grepl("the run stopped here: fatal thing", paste(msgs, collapse = ""), fixed = TRUE))
    expect_equal(journal_result(), "error: fatal thing")
    expect_setequal(state_of()$text, s$text)                                                      # the event is of that run only
    expect_false(JOURNAL$active)
  })
})

test_that("the comparison of two runs: every row has its own status (one value never decides for all)", {
  state <- data.frame(place = c("AT", "AT", "DE"), title = c("Austria (AT)", "Austria (AT)", "Germany (DE)"), phase = c("load", "status", "download"), kind = c("failed", "attention", "download"),
                      key = c("AT|load|failed|old", "AT|status|attention|none", "DE|download|download|pks"), text = c("old", "none", "pks"), n = "1", first_seen = "2026-11-01", last_seen = "2026-11-01", stringsAsFactors = FALSE)
  now <- data.frame(place = c("AT", "AT", "versions"), title = c("Austria (AT)", "Austria (AT)", "R and packages"), phase = c("load", "status", "status"), kind = c("failed", "attention", "versions"),
                    key = c("AT|load|failed|new", "AT|status|attention|none", "versions|status|versions|x"), text = c("new", "none", "x"), n = 1L, stringsAsFactors = FALSE)
  cmp <- journal_compare(now, state, done = c("AT|load", "AT|status", "DE|load"), today = as.Date("2026-11-16"))
  st <- setNames(cmp$now$status, cmp$now$text)
  expect_equal(st[["new"]], "new"); expect_equal(st[["none"]], "again"); expect_equal(st[["x"]], "event")
  expect_equal(st[["pks"]], "kept")                                                              # Germany was visited, its downloads did not run: carried
  expect_equal(cmp$solved$text, "old")
  expect_equal(cmp$now$first_seen[cmp$now$text == "none"], "2026-11-01"); expect_equal(cmp$now$first_seen[cmp$now$text == "new"], "2026-11-16")
  expect_setequal(cmp$state$text, c("pks", "new", "none"))                                       # an event is not carried to the next run
  first <- journal_compare(now, NULL, done = c("AT|load", "AT|status"))
  expect_equal(first$now$status, c("first", "first", "event")); expect_true(first$first_run)
})

test_that("warnings outside an update are R's own, and a journal that cannot write does not stop the update", {
  expect_warning(as.integer("a"), "NAs introduced")
  old <- CONFIG$dir_reports; blocked <- tempfile("blocked_"); writeLines("a file, not a folder", blocked)
  assign("CONFIG", utils::modifyList(CONFIG, list(dir_reports = file.path(blocked, "reports"))), envir = globalenv())
  on.exit({ assign("CONFIG", utils::modifyList(CONFIG, list(dir_reports = old)), envir = globalenv()); JOURNAL$active <- FALSE; unlink(blocked) }, add = TRUE)
  out <- NULL
  msgs <- testthat::capture_messages(suppressWarnings(out <- with_journal("test", { step("A"); warning("w"); 7 })))
  expect_equal(out, 7)
  expect_true(any(grepl("summary of the run could not be written|versions not written down", msgs)))
})

test_that("a summary that cannot be saved is still printed, and says what was not saved", {
  with_reports({
    dir.create(journal_state_file())                    # a folder where the file should be: it cannot be written (as run_state.csv open in a spreadsheet on Windows)
    out <- NULL
    msgs <- paste(testthat::capture_messages(out <- with_journal("test", { step("A", place = "AA", label = "Alpha (AA)"); message("x a.csv not loaded"); 7 })), collapse = "")
    expect_equal(out, 7)
    expect_true(grepl("First run with a summary: 1 line(s)", msgs, fixed = TRUE)); expect_true(grepl("failed    x a.csv not loaded", msgs, fixed = TRUE))
    expect_true(grepl("(run_state.csv is there but could not be read: this run had nothing to compare itself with", msgs, fixed = TRUE))
    expect_true(grepl("(Not saved: run_state.csv (", msgs, fixed = TRUE)); expect_true(grepl("The next run will compare itself with the last summary that was saved", msgs, fixed = TRUE))
    expect_match(journal_result(), "^ok, 1 line\\(s\\) in the summary")
    expect_length(list.files(CONFIG$dir_reports, "^run_[0-9]{8}_[0-9]{6}\\.csv$"), 1)             # the list of this run was saved; only the state was not
  })
})

# --- Freshness -----------------------------------------------------------------------------------------------------
test_that("the end of a period: calendar quarters and financial years", {
  expect_equal(period_end(2025, 4), as.Date("2025-12-31")); expect_equal(period_end(2026, 2), as.Date("2026-06-30")); expect_equal(period_end(2024, 1), as.Date("2024-03-31"))
  expect_equal(period_end(2027, 1, financial_year = TRUE), as.Date("2026-06-30"))                # 2026/27 Q1 = April to June 2026
  expect_equal(period_end(2026, 4, financial_year = TRUE), as.Date("2026-03-31"))                # 2025/26 Q4 ends in March 2026
  expect_equal(period_end(2026, 3, financial_year = TRUE), as.Date("2025-12-31"))
  expect_equal(month_end(as.Date("2024-02-01")), as.Date("2024-02-29"))
  expect_equal(age_text(0.4), "0 months"); expect_equal(age_text(1.9), "1 month"); expect_equal(age_text(23.5), "23 months"); expect_equal(age_text(24), "2 years"); expect_equal(age_text(94.6), "7 years 10 months")
})

test_that("a source is old when its last period ended longer ago than its rhythm allows; a module can state its own rhythm", {
  f <- freshness(as.Date("2025-12-31"), yearly = TRUE, today = as.Date("2026-11-16")); expect_false(f$old); expect_equal(f$limit, 20)
  expect_true(freshness(as.Date("2018-12-31"), yearly = TRUE, today = as.Date("2026-11-16"))$old)             # Germany on the founder's computer
  expect_false(freshness(as.Date("2026-06-30"), yearly = FALSE, today = as.Date("2026-11-16"))$old)           # a quarterly source, two quarters behind
  expect_true(freshness(as.Date("2026-02-28"), yearly = FALSE, today = as.Date("2026-11-16"))$old)
  expect_false(freshness(as.Date("2024-12-31"), yearly = TRUE, limit = 36, today = as.Date("2026-11-16"))$old)   # the Eurostat regional table
  expect_true(freshness(as.Date("2022-12-31"), yearly = TRUE, limit = 36, today = as.Date("2026-11-16"))$old)
  expect_match(freshness(as.Date("2018-12-31"), TRUE, today = as.Date("2026-11-16"))$text, "ended 7 years 10 months ago \\(this source is expected within 20 months\\)")
  expect_equal(COUNTRY_MODULES$AT$freshness_months, 36); expect_equal(CITY_MODULES$barcelona$freshness_months, 20); expect_equal(CITY_MODULES$buenosaires$freshness_months, 20)
  load_fixtures()
  with_db(function(con) {
    xt <- function(today) { f <- country_freshness(con, as.Date(today)); f[f$country == "XT", ] }
    expect_equal(xt("2026-06-01")$to_year, 2025L); expect_equal(xt("2026-06-01")$to_quarter, 4L); expect_false(xt("2026-06-01")$yearly)     # four quarters in the file: a quarterly source
    expect_false(xt("2026-06-01")$old); expect_true(xt("2026-10-01")$old)                                                                    # 2025 Q4 against the 8 months of a quarterly source
    m <- COUNTRY_MODULES$XT; m$freshness_months <- 24; COUNTRY_MODULES$XT <<- m
    on.exit({ m$freshness_months <- NULL; COUNTRY_MODULES$XT <<- m }, add = TRUE)
    expect_false(xt("2026-10-01")$old); expect_equal(xt("2026-10-01")$limit, 24)
    ct <- function(today) city_freshness(con, as.Date(today))
    z <- function(today) { f <- ct(today); f[f$city == "zurich" & f$what == "", ] }
    expect_false(z("2027-06-01")$old); expect_true(z("2027-10-01")$old)                                                                      # a yearly city: 2025 against 20 months
    b <- function(today) { f <- ct(today); f[f$city == "buenosaires" & f$what == "", ] }
    expect_equal(b("2026-11-16")$last, as.Date("2025-12-01")); expect_false(b("2026-11-16")$old); expect_true(b("2027-10-01")$old)           # monthly figures published once a year
    u <- function(today) { f <- ct(today); f[f$city == "utrecht" & f$what == "", ] }
    expect_false(u("2026-06-01")$old); expect_true(u("2026-10-01")$old)                                                                      # a monthly source: 8 months
    chk <- freshness_checks(con, today = as.Date("2026-10-01"))
    expect_true(all(chk$check == "last_period_old")); expect_true(all(chk$severity == "warning"))
    expect_true("UTR" %in% chk$country); expect_false("ZRH" %in% chk$country); expect_false("XT" %in% chk$country)                            # XT has its own 24 months here
    expect_equal(nrow(freshness_checks(con, only = "ZRH", today = as.Date("2028-01-01"))), 1)
  })
})

test_that("the status lines mark an old source and the summary of the run carries it, once", {
  load_fixtures()
  with_reports({
    msgs <- paste(testthat::capture_messages(with_journal("test", with_db(function(con) { city_status(con); country_status(con) }))), collapse = "")
    old_now <- with_db(function(con) { f <- country_freshness(con); f$old[f$country == "XT"] })
    expect_true(grepl(if (old_now) "[old] Testland" else "[ok]  Testland", msgs, fixed = TRUE))            # the two marks take the same room: the names stay in one column
    s <- state_of()
    expect_equal(sum(s$place == "XT" & s$kind == "stale"), as.integer(old_now))
    expect_true(!old_now || grepl("the last period was 2025 Q4", paste(s$text[s$place == "XT" & s$kind == "stale"], collapse = " "), fixed = TRUE))
    # a country registered and never loaded is said under its own place, in the status phase
    expect_true(any(s$kind == "attention" & s$phase == "status" & s$text == "no data loaded yet"))
    expect_false(any(grepl("[!!]", s$text[s$phase == "status"], fixed = TRUE)))                    # the printed line is not recorded a second time
  })
})

# --- The rules that read a source ------------------------------------------------------------------------------------
test_that("a country whose reading rules changed is loaded again by a full run, and only once", {
  load_fixtures()
  m0 <- COUNTRY_MODULES$XT; on.exit(COUNTRY_MODULES$XT <<- m0, add = TRUE)
  expect_equal(country_rules_mark(m0), "1")
  expect_equal(country_rules_mark(COUNTRY_MODULES$PL), "2"); expect_equal(country_rules_mark(COUNTRY_MODULES$AT), "2")       # the corrections of 16 November 2026
  expect_equal(country_rules_mark(COUNTRY_MODULES$ES), "1;mossos=1")                                                         # a second source has its own mark
  with_db(function(con) {
    expect_true(country_rules_current(con, m0)); expect_true(country_unchanged(con, m0))
    m <- m0; m$rules <- "2"; COUNTRY_MODULES$XT <<- m
    expect_false(country_rules_current(con, m)); expect_false(country_unchanged(con, m))                                     # the files did not change: the rules did
    msgs <- paste(testthat::capture_messages(country_steps(con, download = FALSE, only = "XT", force = FALSE)), collapse = "")
    expect_true(grepl("XT: the rules that read this source changed since its last load (now 2): loaded again", msgs, fixed = TRUE))
    expect_equal(country_rules_stored(con, m), "2")
    Sys.sleep(1.1)                                                                                                           # the load is newer than the files
    expect_true(country_unchanged(con, m))
    msgs <- paste(testthat::capture_messages(country_steps(con, download = FALSE, only = "XT", force = FALSE)), collapse = "")
    expect_true(grepl("= XT: files unchanged since the last load, kept", msgs, fixed = TRUE))
    # a load that fails does not store the mark: the country is tried again at the next run
    m$rules <- "3"; m$ingest <- function(con) stop("reader broken"); COUNTRY_MODULES$XT <<- m
    quiet(country_steps(con, download = FALSE, only = "XT", force = FALSE))
    expect_equal(country_rules_stored(con, m), "2"); expect_false(country_unchanged(con, m))
    DBI::dbExecute(con, "DELETE FROM country_rules WHERE country = 'XT'")                                                     # as it was: no mark stored = the first mark
  })
})

test_that("Poland: the detection rates are percentages and are never read as counts", {
  vars <- data.frame(label = c("przestępstwa stwierdzone przez Policję ogółem", "wskaźnik wykrywalności sprawców przestępstw ogółem", "przestępstwa o charakterze kryminalnym", "Wskaźnik wykrywalności - kryminalne", "inne"),
                     unit = c("szt.", "%", "szt.", NA, " % "), stringsAsFactors = FALSE)
  expect_equal(pl_is_rate(vars), c(FALSE, TRUE, FALSE, TRUE, TRUE))
})

test_that("the NUTS editions come from the index GISCO publishes, the ones known to the code when there is none", {
  dir <- CONFIG$shared$gisco; dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  old <- list.files(dir, "_nuts_datasets\\.json$", full.names = TRUE); expect_length(old, 0)
  expect_equal(er_editions(), c(2024L, 2021L, 2016L, 2013L, 2010L, 2006L))
  f <- file.path(dir, "20270115_nuts_datasets.json"); on.exit(unlink(f), add = TRUE)
  writeLines('{"nuts-2003": {}, "nuts-2006": {}, "nuts-2021": {}, "nuts-2024": {}, "nuts-2027": {}, "something-else": {}}', f)
  expect_equal(er_editions(), c(2027L, 2024L, 2021L, 2016L, 2013L, 2010L, 2006L))                 # the new edition first; nothing before 2006
  writeLines("not json", f)
  expect_equal(er_editions(), c(2024L, 2021L, 2016L, 2013L, 2010L, 2006L))
})

# --- Versions ---------------------------------------------------------------------------------------------------------
test_that("the versions of R and the packages are written down the first time and again only when one changes", {
  with_reports({
    snap <- versions_snapshot()
    expect_true(all(c("R", "platform", "duckdb", "sf", "shiny") %in% snap$item)); expect_false(anyNA(snap$version))
    expect_equal(snap$version[snap$item == "R"], paste(R.version$major, R.version$minor, sep = "."))
    expect_null(versions_last())
    msgs <- testthat::capture_messages(ch <- versions_check())
    expect_length(ch, 0); expect_true(any(grepl("written down for the first time", msgs)))
    expect_length(list.files(CONFIG$dir_reports, "^versions_"), 1)
    expect_silent(versions_check())                                                                # nothing changed: nothing written, nothing said
    expect_length(list.files(CONFIG$dir_reports, "^versions_"), 1)
    # an older snapshot with another duckdb and a package since removed
    past <- snap; past$version[past$item == "duckdb"] <- "0.0.1"; past <- rbind(past, data.frame(item = "gonepkg", version = "1.0"))
    unlink(list.files(CONFIG$dir_reports, "^versions_", full.names = TRUE))
    readr::write_csv(past, file.path(CONFIG$dir_reports, "versions_20260101.csv"))
    expect_setequal(versions_diff(past, snap), c(sprintf("duckdb 0.0.1 -> %s", snap$version[snap$item == "duckdb"]), "gonepkg 1.0 -> (removed)"))
    msgs <- paste(testthat::capture_messages(with_journal("test", 1)), collapse = "")
    expect_true(grepl("R or packages changed since 2026-01-01: duckdb 0.0.1 -> ", msgs, fixed = TRUE))
    expect_true(grepl("NEW in this run: 1 line(s)", msgs, fixed = TRUE))
    expect_length(list.files(CONFIG$dir_reports, "^versions_"), 2)
    expect_equal(nrow(state_of()), 0)                                                              # said in that run only
  })
})

test_that("of two version snapshots of one day the later is the last one, whatever the alphabet of the computer does with dots and underscores", {
  with_reports({
    put <- function(name, v) writeLines(c("item,version", paste0("R,", v)), file.path(CONFIG$dir_reports, name))
    put("versions_20260228_235959.csv", "0"); put("versions_20260301.csv", "1"); put("versions_20260301_101500.csv", "2")
    expect_equal(basename(versions_last()$file), "versions_20260301_101500.csv"); expect_equal(versions_last()$data$version, "2")
    put("versions_20260302.csv", "3")
    expect_equal(basename(versions_last()$file), "versions_20260302.csv")
  })
})

# --- The scheduled task -------------------------------------------------------------------------------------------------
test_that("the time limit of a Windows task is read in hours", {
  expect_equal(schedule_limit_hours("PT2H"), 2); expect_equal(schedule_limit_hours("PT72H"), 72); expect_equal(schedule_limit_hours("P3D"), 72)
  expect_equal(schedule_limit_hours("PT1H30M"), 1.5); expect_equal(schedule_limit_hours("P1DT6H"), 30); expect_equal(schedule_limit_hours("PT0S"), Inf)
  expect_true(is.na(schedule_limit_hours("two hours"))); expect_true(is.na(schedule_limit_hours(NA))); expect_true(is.na(schedule_limit_hours("")))
  expect_equal(SCHEDULE_HOURS, 6L)
})

test_that("the state of the scheduled task says what deserves a look: another R, a short limit, a run that failed, did not finish or did not happen", {
  now <- as.POSIXct("2026-11-16 12:00:00", tz = "UTC")
  ok <- list(scheduled = TRUE, state = "Ready", next_run = "2026-11-22 21:00", last_run = "2026-11-15 21:00", result = "0", missed = "0",
             exe = "C:\\Program Files\\R\\R-4.6.1\\bin\\x64\\Rscript.exe", limit = "PT6H",
             log = list(file = "reports/update_20261115_210000.log", date = now - 15 * 3600, result = "ok, 41 line(s) in the summary, 2 new"))
  mine <- "C:/Program Files/R/R-4.6.1/bin/x64/Rscript.exe"
  expect_length(schedule_notes(ok, mine, now), 0)
  expect_length(schedule_notes(utils::modifyList(ok, list(exe = "\"c:/program files/r/R-4.6.1/bin/x64/rscript.exe\"")), mine, now), 0)      # the same file, written another way
  # Windows gives R.home() in the short names of a folder with spaces, and the task holds the long ones: still the same R
  win <- function(p) sub("C:/PROGRA~1/R/R-46~1.1", "C:/Program Files/R/R-4.6.1", gsub("\\\\", "/", gsub("\"", "", trimws(p))), fixed = TRUE)
  short <- "C:/PROGRA~1/R/R-46~1.1/bin/x64/Rscript.exe"
  expect_length(schedule_notes(ok, short, now, long = win), 0)
  n <- schedule_notes(utils::modifyList(ok, list(exe = "C:\\Program Files\\R\\R-4.5.2\\bin\\x64\\Rscript.exe")), short, now, long = win)
  expect_length(n, 1); expect_true(grepl("the R in use is C:\\Program Files\\R\\R-4.6.1\\bin\\x64\\Rscript.exe.", n, fixed = TRUE))   # named in full
  n <- schedule_notes(utils::modifyList(ok, list(exe = "C:\\Program Files\\R\\R-4.5.2\\bin\\x64\\Rscript.exe")), mine, now)
  expect_length(n, 1); expect_match(n, "The task runs C:\\\\Program Files\\\\R\\\\R-4.5.2"); expect_match(n, "Option 9 creates the task again with the R in use")
  n <- schedule_notes(utils::modifyList(ok, list(limit = "PT2H")), mine, now); expect_length(n, 1); expect_match(n, "stops this task after 2 hour\\(s\\); tasks created now get 6")
  expect_length(schedule_notes(utils::modifyList(ok, list(limit = "PT0S")), mine, now), 0)                                                  # no limit at all
  n <- schedule_notes(utils::modifyList(ok, list(result = "1")), mine, now); expect_length(n, 1); expect_match(n, "ended with code 1")
  n <- schedule_notes(utils::modifyList(ok, list(result = "267014")), mine, now); expect_match(n, "the run was stopped")
  expect_length(schedule_notes(utils::modifyList(ok, list(result = "267011", log = NULL)), mine, now), 0)                                    # created and not run yet
  log <- function(...) utils::modifyList(ok$log, list(...))
  n <- schedule_notes(utils::modifyList(ok, list(log = log(date = now - 23 * 86400))), mine, now); expect_length(n, 1); expect_match(n, "The last log is 23 days old")
  n <- schedule_notes(utils::modifyList(ok, list(log = log(result = "unfinished"))), mine, now); expect_length(n, 1); expect_match(n, "did not reach its end")
  expect_length(schedule_notes(utils::modifyList(ok, list(result = "267009", log = log(result = "unfinished"))), mine, now), 0)              # it is running now
  n <- schedule_notes(utils::modifyList(ok, list(log = log(result = "error: disk full"))), mine, now); expect_length(n, 1); expect_match(n, "ended with an error")
  n <- schedule_notes(utils::modifyList(ok, list(state = "Disabled")), mine, now); expect_match(n, "disabled in Windows")
  ok$log <- NULL; n <- schedule_notes(ok, mine, now); expect_length(n, 1); expect_match(n, "has run but left no log")
  expect_length(schedule_notes(list(scheduled = FALSE), mine, now), 0)
})

test_that("the last log of a scheduled run is the newest one that is not being written", {
  with_reports({
    expect_null(last_update_log())
    a <- file.path(CONFIG$dir_reports, "update_20261108_210000.log"); b <- file.path(CONFIG$dir_reports, "update_20261115_210000.log")
    writeLines(c("Scheduled update", "", "RESULT: ok, 3 line(s) in the summary, 0 new "), a); Sys.setFileTime(a, Sys.time() - 7 * 86400)
    writeLines(c("Scheduled update", "..."), b)
    expect_equal(last_update_log()$result, "unfinished")
    expect_equal(last_update_log(skip = b)$result, "ok, 3 line(s) in the summary, 0 new ")
    old <- options(webarchy.scheduled_log = b); on.exit(options(old), add = TRUE)
    expect_equal(basename(last_update_log()$file), basename(a))                                    # the run in progress does not read its own log
  })
})

# --- Old copies of downloaded files ---------------------------------------------------------------------------------------
test_that("a dated copy is told from a plain file name", {
  p <- old_copies_parse(c("a/20261103_file.csv", "a/20261110_081933_file.csv", "a/200076_20261006_220058.csv", "a/balance_municipios_2025T2.csv", "a/20269999_x.csv", "a/20261103.csv",
                          "a/20261103_200076_padron.csv"))                                         # six digits that cannot be a time belong to the name
  expect_equal(p$name, c("file.csv", "file.csv", NA, NA, NA, NA, "200076_padron.csv"))
  expect_equal(p$stamp[c(1:2, 7)], c("20261103000000", "20261110081933", "20261103000000"))
})

test_that("the listing of old copies names only the copies a newer one replaced, and never one the database still reads", {
  raw <- tempfile("raw_"); dir.create(file.path(raw, "country_aa", "sub"), recursive = TRUE); dir.create(file.path(raw, "city_bb"))
  on.exit(unlink(raw, recursive = TRUE), add = TRUE)
  put <- function(path, text, days_ago) { f <- file.path(raw, path); writeLines(text, f); Sys.setFileTime(f, Sys.time() - days_ago * 86400); f }
  put("country_aa/20261001_series.csv", "old content", 40); put("country_aa/20261101_series.csv", "newer content", 10); put("country_aa/20261110_series.csv", "newest", 2)
  put("country_aa/sub/20261001_layer.geojson", "same", 40); put("country_aa/sub/20261105_layer.geojson", "same", 5)             # downloaded again, identical
  put("country_aa/20261001_single.csv", "one copy", 40); put("country_aa/plain_name.csv", "no date", 40); put("country_aa/plain_name_2.csv", "no date", 40)
  put("city_bb/20261001_window.csv", "months only here", 40); put("city_bb/20261101_window.csv", "later months", 3)
  put("city_bb/20261001_clock.csv", "a", 1); put("city_bb/20261101_clock.csv", "b", 30)                                        # the newer name is the older file
  db <- tempfile(fileext = ".duckdb"); con <- DBI::dbConnect(duckdb::duckdb(dbdir = db)); on.exit({ db_disconnect(con); unlink(db) }, add = TRUE)
  DBI::dbWriteTable(con, "stg_city_events", data.frame(city = "bb", file = "20261001_window.csv;20261101_window.csv"))        # a series built from both copies
  DBI::dbWriteTable(con, "ingest_log", data.frame(source = "x", file = "20261001_series.csv"))                                # the log remembers every load: it does not count
  x <- old_copies(raw, con)
  expect_setequal(x$file, c("20261001_series.csv", "20261101_series.csv", "20261001_layer.geojson", "20261001_window.csv", "20261001_clock.csv"))
  v <- setNames(x$verdict, x$file)
  expect_equal(unname(v[c("20261001_series.csv", "20261101_series.csv", "20261001_layer.geojson")]), rep("can be moved", 3))
  expect_match(v[["20261001_window.csv"]], "^keep: rows of the database come from it"); expect_match(v[["20261001_clock.csv"]], "^keep: the dates in the names and the times of the files disagree")
  expect_equal(x$replaced_by[x$file == "20261001_series.csv"], "20261110_series.csv")
  expect_true(x$identical[x$file == "20261001_layer.geojson"]); expect_false(x$identical[x$file == "20261001_series.csv"])
  expect_equal(x$folder[x$file == "20261001_layer.geojson"], "country_aa/sub")
  expect_true(all(old_copies(raw, NULL)$verdict[!grepl("clock", old_copies(raw, NULL)$file)] == "not checked: no database to ask"))   # without a database nothing is recommended
  before <- sort(list.files(raw, recursive = TRUE)); sizes <- file.size(file.path(raw, before))
  old <- CONFIG; assign("CONFIG", utils::modifyList(CONFIG, list(dir_raw = raw, dir_reports = file.path(raw, "..", basename(tempfile("rep_"))), db_path = db)), envir = globalenv())
  on.exit({ unlink(CONFIG$dir_reports, recursive = TRUE); assign("CONFIG", old, envir = globalenv()) }, add = TRUE)
  db_disconnect(con); con <- NULL
  msgs <- paste(testthat::capture_messages(y <- list_old_copies()), collapse = "")
  expect_true(grepl("5 older copies of 4 files", msgs, fixed = TRUE)); expect_true(grepl("3 of them can be moved", msgs, fixed = TRUE)); expect_true(grepl("2 must stay", msgs, fixed = TRUE))
  expect_equal(sort(list.files(raw, recursive = TRUE)), before); expect_equal(file.size(file.path(raw, before)), sizes)       # a listing: nothing moved, nothing changed
  rep <- list.files(CONFIG$dir_reports, "^old_copies_")
  expect_true(any(grepl("\\.csv$", rep))); expect_true(any(grepl("\\.txt$", rep)))
  txt <- readLines(file.path(CONFIG$dir_reports, rep[grepl("\\.txt$", rep)]))
  expect_length(txt, 3); expect_true(all(grepl("series|layer", txt)))
  con <- DBI::dbConnect(duckdb::duckdb(dbdir = db))                                                                            # for the on.exit above
})

# --- Madrid: the historical register -------------------------------------------------------------------------------------
test_that("the month a file of the portal describes is read from its own words, never guessed", {
  x <- c("Rango_Edades_Seccion_201401.csv", "Padrón a 1 de enero de 2015", "Enero 2016", "2017-01", "01/2018", "200076_20261006_220058.csv", "FEBRERO DE 2021", "padron_20220101.csv", "poblacion_2023_07.csv",
         "209163-12-padron-municipal-historico", "Padron 2019", "datos a 1-1-2020", "", NA)
  expect_equal(format(madrid_history_month(x)), c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2026-10-01", "2021-02-01", "2022-01-01", "2023-07-01", NA, NA, NA, NA, NA))
  # What the real dataset writes (its list, received on 16 November 2026): the year, then the month, in the description
  expect_equal(format(madrid_history_month(c("Padrón municipal. Histórico. 2024. Diciembre", "Padrón municipal. Histórico. 2014. Enero", "Histórico. 2026. Septiembre", "Padrón municipal. Histórico. 2024."))),
               c("2024-12-01", "2014-01-01", "2026-09-01", NA))
  real <- data.frame(name = c("209163-0-padron-municipal-historico-csv", "209163-351-padron-municipal-historico", "209163-999-sin-descripcion", "padron"),
                     description = c("Padrón municipal. Histórico. 2024. Diciembre", "Padrón municipal. Histórico. 2026. Septiembre", "", NA),
                     url = c("https://x/download/209163-0-padron-municipal-historico-csv.csv", "https://x/download/209163_20261029_160951.csv", "https://x/download/209163_20261029_160951.csv", "https://x/download/padron_201401.csv"),
                     stringsAsFactors = FALSE)
  expect_equal(format(madrid_history_months(real)), c("2024-12-01", "2026-09-01", NA, "2014-01-01"))      # the description wins; the date of an export in the address is not a month
  res <- data.frame(name = c("Febrero 2014", "Enero 2014", "Marzo 2015", "Enero 2016", "sin fecha", "Enero 2030"), stringsAsFactors = FALSE); res$month <- madrid_history_month(res$name)
  pick <- madrid_history_pick(res, 2014:2016)
  expect_equal(pick$name, c("Enero 2014", "Marzo 2015", "Enero 2016"))                              # the first month the portal has of each year wanted
})

# A register file in today's layout: one row per neighbourhood (district d, neighbourhood b), 2 x n residents
write_reg <- function(rows, file = tempfile(fileext = ".csv"), date = "2019-01-01", fx = TRUE) {
  d <- data.frame(COD_DISTRITO = rows$d, DESC_DISTRITO = "X", COD_DIST_BARRIO = rows$d * 100 + rows$b, DESC_BARRIO = "Y", COD_BARRIO = rows$b, COD_DIST_SECCION = 1, COD_SECCION = 1, COD_EDAD_INT = 30,
                  ESPANOLESHOMBRES = rows$n, ESPANOLESMUJERES = rows$n, EXTRANJEROSHOMBRES = 0, EXTRANJEROSMUJERES = 0)
  if (fx) { d$FX_CARGA <- "2019-02-01"; d$FX_DATOS_INI <- date; d$FX_DATOS_FIN <- date }
  utils::write.table(d, file, sep = ";", row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8"); file
}

test_that("a historical register is loaded only when it is plainly the register of Madrid; a district with other neighbourhoods is left out", {
  known <- paste0(ID_MADRID, c("-D01-B011", "-D01-B012", "-D02-B021", "-D02-B022", "-D19-B191", "-D19-B192", "-D19-B193"))
  today <- data.frame(d = c(1, 1, 2, 2, 19, 19, 19), b = c(1, 2, 1, 2, 1, 2, 3), n = 230000)        # 3.22 million residents in all
  r <- quiet(madrid_history_read(write_reg(today), known))
  expect_true(r$ok); expect_equal(r$ref_date, as.Date("2019-01-01")); expect_equal(nrow(r$pop), 7); expect_length(r$left_out, 0); expect_equal(sum(r$pop$population), 7 * 460000)
  then <- today[!(today$d == 19 & today$b == 3), ]; then$n <- 270000                                # the district had two neighbourhoods that year
  r <- quiet(madrid_history_read(write_reg(then), known))
  expect_true(r$ok); expect_equal(r$left_out, paste0(ID_MADRID, "-D19")); expect_setequal(r$pop$territory_id, known[1:4])
  # no date inside: the month the portal states is used; neither: not loaded
  f <- write_reg(today, fx = FALSE)
  r <- quiet(madrid_history_read(f, known, listed = as.Date("2016-01-01"))); expect_true(r$ok); expect_equal(r$ref_date, as.Date("2016-01-01"))
  r <- quiet(madrid_history_read(f, known)); expect_false(r$ok); expect_match(r$reason, "no reference date")
  r <- quiet(madrid_history_read(write_reg(today, date = "2019-06-01"), known, listed = as.Date("2014-01-01"))); expect_false(r$ok); expect_match(r$reason, "the file says 2019-06 and the portal's list says 2014-01")
  half <- today; half$n <- 100000
  r <- quiet(madrid_history_read(write_reg(half), known)); expect_false(r$ok); expect_match(r$reason, "residents in all: not the register of Madrid")
  other <- tempfile(fileext = ".csv"); writeLines(c("distrito;barrio;total", "1;1;100"), other)
  r <- quiet(madrid_history_read(other, known)); expect_false(r$ok); expect_match(r$reason, "columns missing")
  elsewhere <- today; elsewhere$d <- elsewhere$d + 30
  r <- quiet(madrid_history_read(write_reg(elsewhere), known)); expect_false(r$ok); expect_match(r$reason, "none of its districts")
  expect_equal(CONFIG$sources$madrid_census_history$dir_raw, "data/raw/city_madrid/census_history")
})

test_that("the historical registers on disk are loaded once, and a month that has its own register keeps it", {
  load_fixtures()
  src <- CONFIG$sources$madrid_census_history
  ids <- paste0(ID_MADRID, c("-D01-B011", "-D01-B012", "-D02-B021", "-D02-B022"))
  dir.create(src$dir_raw, recursive = TRUE, showWarnings = FALSE); on.exit(unlink(src$dir_raw, recursive = TRUE), add = TRUE)
  rows <- data.frame(d = c(1, 1, 2, 2), b = c(1, 2, 1, 2), n = 400000)                              # 3.2 million residents in all
  write_reg(rows, file.path(src$dir_raw, "padron_2019.csv"), date = "2019-01-01"); write_reg(rows, file.path(src$dir_raw, "padron_2020.csv"), date = "2020-01-01")
  with_db(function(con) {
    mine <- paste0("territory_id LIKE '", ID_MADRID, "-%'")
    expect_equal(DBI::dbGetQuery(con, paste("SELECT count(*) AS n FROM dim_territory WHERE", mine))$n, 0)                       # the test database has no Madrid of its own
    on.exit({ DBI::dbExecute(con, paste("DELETE FROM ref_population WHERE", mine)); DBI::dbExecute(con, paste("DELETE FROM dim_territory WHERE", mine)) }, add = TRUE)
    DBI::dbAppendTable(con, "dim_territory", data.frame(territory_id = ids, level = "neighbourhood", parent_id = sub("-B[0-9]+$", "", ids), country = "ES", name = paste("Neighbourhood", 1:4)))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = ids, ref_date = as.Date("2020-01-01"), population = 1L, file = "200076_monthly.csv"))   # January 2020 has its own register
    msgs <- paste(testthat::capture_messages(madrid_history_ingest(con)), collapse = "")
    expect_true(grepl("+ padron_2019.csv: historical register at 2019-01-01 = 3,200,000 residents in the file, 4 neighbourhoods loaded", msgs, fixed = TRUE))
    expect_true(grepl("= padron_2020.csv (historical register of 2020-01): that month already has its own register, which is kept", msgs, fixed = TRUE))
    p <- DBI::dbGetQuery(con, paste("SELECT ref_date, file, sum(population) AS n FROM ref_population WHERE", mine, "GROUP BY 1, 2 ORDER BY 1"))
    expect_equal(p$file, c("history:padron_2019.csv", "200076_monthly.csv")); expect_equal(as.numeric(p$n), c(3200000, 4))
    msgs <- paste(testthat::capture_messages(madrid_history_ingest(con)), collapse = "")
    expect_true(grepl("= padron_2019.csv (historical register, already loaded)", msgs, fixed = TRUE))                           # the second time nothing is read again
    expect_equal(madrid_history_years(con), 2014:2019)                                                                          # wanted: from the first year of the police activity (2014 when none is loaded) to the year before the first monthly register
  })
})


# The app at a fixed address (P-068) -----------------------------------------------------------------------------
test_that("the app opens at the same address every day, and on a free port only when its own is taken", {
  use_test_project()
  expect_true(is.integer(CONFIG$app_port) && length(CONFIG$app_port) == 1 && CONFIG$app_port > 1023L && CONFIG$app_port < 65536L)
  refused <- c(1719, 1720, 1723, 2049, 3659, 4045, 4190, 5060, 5061, 6000, 6566, 6665:6669, 6679, 6697, 10080)   # ports above 1023 that a browser will not open
  expect_false(CONFIG$app_port %in% refused)
  expect_true(port_taken(simpleError("Failed to create server"))); expect_true(port_taken(simpleError("listen EADDRINUSE: address already in use 127.0.0.1:7175")))
  expect_false(port_taken(simpleError("object 'x' not found")))
  # What open_app() does with the port, on a stand-in for shiny::runApp that records how it was called
  calls <- list(); go <- function(url) url
  free <- function(dir, ...) { calls[[length(calls) + 1]] <<- list(dir = dir, ...); "running" }
  expect_equal(run_app_at(CONFIG$app_port, go, run = free), "running")
  expect_length(calls, 1); expect_equal(calls[[1]]$dir, "app"); expect_identical(calls[[1]]$port, CONFIG$app_port); expect_identical(calls[[1]]$launch.browser, go)
  # The port is taken: said in a message, and the app opens on a port shiny picks (none is named); nothing else is tried
  calls <- list(); taken <- function(dir, ...) { a <- list(dir = dir, ...); calls[[length(calls) + 1]] <<- a; if (!is.null(a$port)) stop("Failed to create server"); "running elsewhere" }
  expect_message(out <- run_app_at(7175L, go, run = taken), "Port 7175 is in use")
  expect_equal(out, "running elsewhere"); expect_length(calls, 2); expect_null(calls[[2]]$port); expect_identical(calls[[2]]$launch.browser, go)
  # Any other failure is the app's own and is not swallowed, nor tried a second time
  calls <- list(); broken <- function(dir, ...) { calls[[length(calls) + 1]] <<- list(dir = dir, ...); stop("could not find function \"ui\"") }
  expect_error(run_app_at(7175L, go, run = broken), "could not find function"); expect_length(calls, 1)
  # No port set (NULL): shiny picks one, as before
  calls <- list(); expect_equal(run_app_at(NULL, go, run = free), "running"); expect_null(calls[[1]]$port)
  expect_true(grepl("run_app_at(port,", paste(deparse(open_app), collapse = " "), fixed = TRUE)); expect_identical(formals(open_app)$port, quote(CONFIG$app_port))
  # Opened from the menu, the app is told to stop by itself once its pages are closed (app/app.R, stop_if_closed)
  expect_true(grepl("webarchy.stop_when_closed = 15", paste(deparse(open_app), collapse = " "), fixed = TRUE))
})
