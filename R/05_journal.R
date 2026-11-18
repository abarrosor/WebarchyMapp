# The run journal: what did not go as planned in an update, said together at the end ------------------
# (proposals F2, F13, F1 and F7 of Claude/06_technical/future-proof-analysis-2026-11.md; decision P-057)
#
# An update prints hundreds of lines. The few that matter (a portal that did not answer, a step that
# failed, a category the reference table does not know, an R warning) were lost among them, and R keeps
# only the first 50 warnings of a run without saying where they came from. The journal collects those
# lines under the place that raised them (a country, a city, a shared step), prints them together at the
# end of the run and compares them with the last run of the same place:
#   - what is NEW is said first;
#   - what stopped happening is said too (a fix is seen to work);
#   - what has been the same for weeks is one count per place, and what was not looked at in this run (a country
#     kept because its files did not change, a place that was not part of the run) is one number.
# The whole list is saved in reports/run_<date>_<time>.csv, and reports/run_state.csv keeps, place by
# place, what the last run of that place found and since when (it is how "new" is told from "again").
#
# How a line gets into the journal:
#   - attempt() and try_download() record the step that failed and the portal that did not answer;
#   - a message of a module is recorded when it uses the project's wording for "this did not go as
#     planned" (journal_kind(), below). The modules need no change: they keep printing. A new module
#     only has to keep to that wording; tests/testthat/test-01-helpers.R holds the wording to account;
#   - every R warning raised during the run, with the call that raised it. Each different warning is
#     printed once, where it happens, and the summary says how many times it came;
#   - the status lines add the sources whose last period is old (freshness, below).
#
# Places, phases and what "no longer happening" means. A place is a module (its code: DE, MAD, ES/mossos)
# or a shared step (its title). A place has three phases: "download", "load" and "status". A line of the
# last run is "no longer happening" only when the same phase of the same place ran again and did not say
# it: a run without downloads says nothing about downloads, a country that was kept because its files did
# not change says nothing about its load, and a run of two countries says nothing about the others.
#
# Nothing here changes what an update does: the journal listens. A line it cannot read (bytes that are not
# text, from a file read in the wrong encoding) is read as it can be, and if listening fails all the same the
# step that printed the line goes on as if nobody listened. If the files of the journal cannot be written (the
# reports folder is not there, run_state.csv is open in a spreadsheet) the update goes on, the summary is still
# printed and a line says what was not saved.

JOURNAL <- new.env()
JOURNAL$active <- FALSE
JOURNAL_EVENTS <- c("stopped", "versions")       # kinds that belong to one run: said once, never carried to the next

journal_reset <- function(run = "update", download = TRUE) {
  JOURNAL$active <- TRUE; JOURNAL$run <- run; JOURNAL$started <- Sys.time(); JOURNAL$download <- isTRUE(download)
  JOURNAL$place <- "run"; JOURNAL$phase <- "load"; JOURNAL$titles <- list(run = "The run itself")
  JOURNAL$done <- character(); JOURNAL$kept <- character(); JOURNAL$lines <- new.env(hash = TRUE); JOURNAL$order <- 0L; JOURNAL$muted <- 0L
  JOURNAL$result <- NULL; JOURNAL$file <- NULL
  invisible(TRUE)
}

journal_pair <- function(place, phase) paste(place, phase, sep = "|")

# The place the next lines belong to (step() calls it) and the phase inside it
journal_place <- function(id, title = id, phase = "load") {
  if (!isTRUE(JOURNAL$active)) return(invisible(FALSE))
  JOURNAL$place <- id; JOURNAL$titles[[id]] <- title
  journal_phase(phase)
}
journal_phase <- function(phase) {
  if (!isTRUE(JOURNAL$active)) return(invisible(FALSE))
  JOURNAL$phase <- phase; JOURNAL$done <- union(JOURNAL$done, journal_pair(JOURNAL$place, phase))
  invisible(TRUE)
}
# A phase of a place ran (said by the status lines, which go through every module)
journal_done <- function(place, phase, title = NULL) {
  if (!isTRUE(JOURNAL$active)) return(invisible(FALSE))
  if (!is.null(title)) JOURNAL$titles[[place]] <- title
  JOURNAL$done <- union(JOURNAL$done, journal_pair(place, phase)); invisible(TRUE)
}
# The load of this place did not run in this run (a country kept because its files did not change): what its
# last load said stays as it was
journal_keep <- function(place = JOURNAL$place, phase = "load") {
  if (!isTRUE(JOURNAL$active)) return(invisible(FALSE))
  JOURNAL$kept <- union(JOURNAL$kept, journal_pair(place, phase)); invisible(TRUE)
}

# Text that a regular expression can read. Bytes that are not valid text (a name read in the wrong encoding) make
# R's text functions stop with an error in a UTF-8 session, and the step that only wanted to print the line would
# stop with them: such bytes are read as Latin-1, the commonest cause.
journal_text <- function(x) {
  x <- as.character(x)
  x <- tryCatch(enc2utf8(x), error = function(e) x)
  bad <- !is.na(x) & (!validUTF8(x) | Encoding(x) == "bytes")
  if (any(bad)) x[bad] <- iconv(x[bad], "latin1", "UTF-8", sub = "?")
  x
}

# Record a line. The same line again (journal_key()) only raises its count: a warning that comes a hundred thousand
# times costs a hundred thousand additions, not a list of that length. TRUE when the line is new in this run.
journal_add <- function(kind, text, place = NULL, phase = NULL, title = NULL) {
  if (!isTRUE(JOURNAL$active)) return(invisible(FALSE))
  place <- place %||% JOURNAL$place; phase <- phase %||% JOURNAL$phase
  if (!is.null(title)) JOURNAL$titles[[place]] <- title
  text <- trimws(gsub("\\s+", " ", paste(journal_text(text), collapse = " ")))
  if (!nzchar(text)) return(invisible(FALSE))
  key <- journal_key(place, phase, kind, text)
  seen <- JOURNAL$lines[[key]]
  if (is.null(seen)) {
    JOURNAL$order <- JOURNAL$order + 1L
    JOURNAL$lines[[key]] <- list(place = place, phase = phase, kind = kind, text = text, key = key, n = 1L, order = JOURNAL$order)
    JOURNAL$done <- union(JOURNAL$done, journal_pair(place, phase))
  } else { seen$n <- seen$n + 1L; JOURNAL$lines[[key]] <- seen }
  invisible(is.null(seen))
}

# Evaluate `expr` without the journal listening to its messages (for lines already recorded by hand)
journal_mute <- function(expr) {
  JOURNAL$muted <- (JOURNAL$muted %||% 0L) + 1L
  on.exit(JOURNAL$muted <- max(0L, JOURNAL$muted - 1L), add = TRUE)
  expr
}
# Record a line and print it (once: the journal does not read it again)
journal_say <- function(kind, text, tail = "", ...) {
  journal_add(kind, text, ...)
  journal_mute(message(text, tail))
  invisible(TRUE)
}

# --- Which messages are "this did not go as planned" -----------------------------------------------------
# The wording the modules already use. NA = an ordinary line. Kinds:
#   attention  the module's own alarm: the line holds [!!]
#   failed     a file or a step failed: the line starts with "x "
#   unmapped   the source has a category, a code or a place that the reference table does not know
#   download   a file could not be fetched (the copies on disk are used)
#   other      a file could not be read or understood
# A line that says the source has simply not published yet is ordinary.
journal_kind <- function(line) {
  x <- trimws(line)
  if (!nzchar(x) || grepl("^([=+.] |-- )", x)) return(NA_character_)              # "= kept", "+ new file", ". skipped", a step title: ordinary
  if (grepl("not published yet", x, fixed = TRUE)) return(NA_character_)
  if (grepl("[!!]", x, fixed = TRUE)) return("attention")
  if (grepl("^x ", x)) return("failed")
  if (grepl("not in (the )?(reference/)?[A-Za-z0-9_]+\\.csv|without a row in reference|absent from reference|unknown to [A-Za-z0-9_]+\\.csv|not in the cube|unknown districts|not matched to|without match", x)) return("unmapped")
  if (grepl("not downloaded|download failed|Download not available|not available|candidate [0-9]+ failed|refused the automated download|not reachable|link failed|HDX resource", x)) return("download")
  if (grepl("not readable|not usable|not understood|cannot read|could not|not read \\(|not recognised|not found|not loaded", x)) return("other")
  NA_character_
}

# What makes two lines "the same line": the place, the phase, the kind and the text without its numbers (dates in
# file names, counts and waiting times change from run to run) nor the names R gives to temporary files
journal_key <- function(place, phase, kind, text) {
  t <- gsub("Rtmp[A-Za-z0-9]+", "Rtmp#", text)
  t <- gsub("file[0-9a-f]{4,}", "file#", t)
  t <- gsub("[0-9]+([.,][0-9]+)*", "#", t)
  paste(place, phase, kind, substr(t, 1, 160), sep = "|")
}

# The two listeners. Whatever happens inside them stays inside them: a line that cannot be recorded is printed as it
# would be without a journal, and a warning that cannot be recorded stays one of R's own (warnings() keeps it).
journal_on_message <- function(m) {
  if (!isTRUE(JOURNAL$active) || (JOURNAL$muted %||% 0L) > 0L) return(invisible())
  tryCatch({
    # A message is one line of the journal even when it runs over several (an error of the download library does): its
    # kind is the one of its first line that has a kind
    lines <- strsplit(journal_text(conditionMessage(m)), "\n", fixed = TRUE)[[1]]
    k <- stats::na.omit(vapply(lines, journal_kind, ""))
    if (length(k)) journal_add(k[[1]], substr(paste(trimws(lines[nzchar(trimws(lines))]), collapse = " "), 1, 500))
  }, error = function(e) NULL)
  invisible()
}
journal_on_warning <- function(w) {
  if (!isTRUE(JOURNAL$active)) return(invisible())
  taken <- tryCatch({
    text <- journal_text(conditionMessage(w))
    call <- conditionCall(w)
    where <- if (is.null(call)) "" else tryCatch(journal_text(paste0(" [in ", substr(paste(deparse(call, nlines = 1L), collapse = " "), 1, 90), "]")), error = function(e) "")
    if (grepl("^ \\[in (withCallingHandlers|doTryCatch|with_journal)\\(", where)) where <- ""     # raised at the top of the run: the wrapper is not a place
    first <- journal_add("warning", paste0(text, where))
    if (isTRUE(first)) journal_mute(message("  warning: ", strsplit(text, "\n", fixed = TRUE)[[1]][1], where))   # once here; the summary counts how many times it came
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(taken)) tryInvokeRestart("muffleWarning")
  invisible()
}

# Try an expression and report instead of stopping the whole update. TRUE when it worked.
attempt <- function(label, expr) {
  ok <- TRUE
  tryCatch(expr, error = function(e) { ok <<- FALSE; journal_say("failed", paste0(label, ": ", conditionMessage(e)), "\n(The rest continues.)") })
  invisible(ok)
}
# The same for a download: the copies already on disk are used. Nothing is asked when download = FALSE.
try_download <- function(label, expr, download = TRUE, tail = "") {
  if (!isTRUE(download)) return(invisible(FALSE))
  before <- JOURNAL$phase; journal_phase("download")
  on.exit(if (isTRUE(JOURNAL$active)) JOURNAL$phase <- before, add = TRUE)
  ok <- TRUE
  tryCatch(expr, error = function(e) { ok <<- FALSE; journal_say("download", paste0("Download not available (", label, "): ", conditionMessage(e)), tail) })
  invisible(ok)
}

# Run an update under the journal. A run started inside another shares its journal.
with_journal <- function(run, expr, download = TRUE) {
  if (isTRUE(JOURNAL$active)) return(expr)
  journal_reset(run, download)
  on.exit(JOURNAL$active <- FALSE, add = TRUE)
  tryCatch(versions_check(), error = function(e) message("(versions not written down: ", conditionMessage(e), ")"))
  out <- tryCatch(
    withCallingHandlers(expr, message = journal_on_message, warning = journal_on_warning),
    error = function(e) {
      journal_add("stopped", paste0("the run stopped here: ", conditionMessage(e)))
      journal_keep(JOURNAL$place, JOURNAL$phase)        # the phase in progress did not finish: what it said last time is not "no longer happening"
      journal_close(error = conditionMessage(e))
      stop(e)
    })
  journal_close()
  invisible(out)
}

# The result of the last run in one line (the scheduled update writes it in its log)
journal_result <- function() JOURNAL$result %||% "ok"

# --- The end of the run: compare with the last time, print, save ---------------------------------------
journal_state_file <- function() file.path(CONFIG$dir_reports, "run_state.csv")
JOURNAL_COLS <- c("place", "title", "phase", "kind", "key", "text", "n", "first_seen", "last_seen")

journal_read_state <- function(file = journal_state_file()) {
  if (!file.exists(file)) return(NULL)
  s <- tryCatch(suppressWarnings(as.data.frame(readr::read_csv(file, col_types = readr::cols(.default = "c"), progress = FALSE), stringsAsFactors = FALSE)), error = function(e) NULL)
  if (is.null(s) || !all(JOURNAL_COLS %in% names(s))) return(NULL)        # a state that cannot be read is no state (journal_close() says so)
  s[, JOURNAL_COLS]
}

# The lines of this run, one per different line, in the order they first came, with how many times each came
journal_rows <- function() {
  empty <- data.frame(place = character(), title = character(), phase = character(), kind = character(), key = character(), text = character(), n = integer(), stringsAsFactors = FALSE)
  keys <- ls(JOURNAL$lines, all.names = TRUE)
  if (length(keys) == 0) return(empty)
  items <- mget(keys, envir = JOURNAL$lines)
  field <- function(f, type) vapply(items, function(r) r[[f]], type, USE.NAMES = FALSE)
  out <- data.frame(place = field("place", ""), phase = field("phase", ""), kind = field("kind", ""), text = field("text", ""), key = field("key", ""), n = field("n", 1L), stringsAsFactors = FALSE)
  out <- out[order(field("order", 1L)), ]
  out$title <- vapply(out$place, function(p) JOURNAL$titles[[p]] %||% p, "", USE.NAMES = FALSE)
  rownames(out) <- NULL
  out[, names(empty)]
}

# Compare the lines of this run with the state: list(now, solved, state, first_run).
# now$status: "first" (no earlier run to compare with), "new", "again", "event"; rows carried from a phase that did
# not run are added with status "kept".
journal_compare <- function(now, state, done, kept = character(), today = Sys.Date()) {
  today <- format(today)
  first_run <- is.null(state)
  if (first_run) state <- data.frame(matrix(character(), 0, length(JOURNAL_COLS), dimnames = list(NULL, JOURNAL_COLS)), stringsAsFactors = FALSE)
  event <- now$kind %in% JOURNAL_EVENTS
  now$first_seen <- ifelse(now$key %in% state$key, state$first_seen[match(now$key, state$key)], today)
  known <- now$key %in% state$key
  now$status <- if (first_run) rep("first", nrow(now)) else ifelse(known, "again", "new")     # (first_run is one value: not inside ifelse)
  now$status[event] <- "event"
  pair <- journal_pair(state$place, state$phase)
  ran <- pair %in% setdiff(done, kept)                         # the phases that really ran again
  solved <- state[ran & !state$key %in% now$key, , drop = FALSE]
  carried <- state[!ran & !state$key %in% now$key, , drop = FALSE]
  visited <- unique(sub("\\|.*$", "", done))
  carried_here <- carried[carried$place %in% visited, , drop = FALSE]   # of the places of this run: shown as kept
  keep_now <- now[!event, , drop = FALSE]
  new_state <- rbind(carried,
                     data.frame(place = keep_now$place, title = keep_now$title, phase = keep_now$phase, kind = keep_now$kind, key = keep_now$key, text = keep_now$text,
                                n = as.character(keep_now$n), first_seen = keep_now$first_seen, last_seen = rep(today, nrow(keep_now)), stringsAsFactors = FALSE))
  if (nrow(carried_here)) now <- rbind(now, data.frame(place = carried_here$place, title = carried_here$title, phase = carried_here$phase, kind = carried_here$kind, key = carried_here$key,
                                                       text = carried_here$text, n = as.integer(carried_here$n), first_seen = carried_here$first_seen, status = "kept", stringsAsFactors = FALSE))
  list(now = now, solved = solved, state = new_state, first_run = first_run, elsewhere = nrow(carried) - nrow(carried_here))
}

journal_line <- function(kind, text, n, since = NULL, width = 170) {
  times <- if (!is.na(n) && n > 1) sprintf(" (%s times)", fmt_int(n)) else ""
  when <- if (!is.null(since) && !is.na(since)) paste0(" (since ", since, ")") else ""
  t <- if (nchar(text) > width) paste0(substr(text, 1, width - 1), "…") else text
  sprintf("    %-9s %s%s%s", kind, t, times, when)
}

journal_print <- function(cmp, file = NULL) {
  now <- cmp$now; solved <- cmp$solved
  step_line <- function(title) message("\n-- ", title, " ", strrep("-", max(3, 64 - nchar(title))))
  by_place <- function(d, since = FALSE) for (p in unique(d$place)) {
    r <- d[d$place == p, ]; message("  ", r$title[1])
    for (i in seq_len(nrow(r))) message(journal_line(r$kind[i], r$text[i], r$n[i], if (since) r$first_seen[i] else NULL))
  }
  step_line("Summary of the run: what did not go as planned")
  kept <- sum(now$status == "kept") + (cmp$elsewhere %||% 0)
  kept_line <- function() if (kept > 0) message(sprintf("Kept from earlier runs, not looked at in this one: %d line(s) (steps that did not run: files unchanged, no download, or a place that was not part of the run)", kept))
  if (sum(now$status != "kept") == 0 && nrow(solved) == 0) { message("Nothing to report: every step of this run did what it had to."); kept_line(); return(invisible(TRUE)) }
  fresh <- now[now$status %in% c("new", "event"), ]; first <- now[now$status == "first", ]; old <- now[now$status == "again", ]
  if (cmp$first_run && nrow(first)) {
    message(sprintf("First run with a summary: %d line(s). They are the reference: from the next run on, only what is new is listed in full.", nrow(first)))
    by_place(first)
  }
  if (nrow(fresh)) { message(sprintf("NEW in this run: %d line(s)", nrow(fresh))); by_place(fresh) }
  else if (!cmp$first_run) message("NEW in this run: nothing.")
  if (nrow(solved)) { message(sprintf("No longer happening: %d line(s) of the last run of these places", nrow(solved))); by_place(solved, since = TRUE) }
  if (nrow(old)) {
    per <- table(old$title)[unique(old$title)]
    message(sprintf("The same as in earlier runs: %d line(s) in %d place(s), the oldest since %s", nrow(old), length(per), min(old$first_seen)))
    message(paste(strwrap(paste(sprintf("%s: %d", names(per), as.integer(per)), collapse = "; "), width = 150, indent = 2, exdent = 2), collapse = "\n"))
  }
  kept_line()
  if (!is.null(file)) message("Every line, with the date it was first seen: ", file)
  invisible(TRUE)
}

journal_close <- function(error = NULL) {
  ok <- tryCatch({
    now <- journal_rows()
    state <- journal_read_state()
    unread <- is.null(state) && file.exists(journal_state_file())
    cmp <- journal_compare(now, state, JOURNAL$done, JOURNAL$kept)
    dir.create(CONFIG$dir_reports, showWarnings = FALSE, recursive = TRUE)
    # Writing comes around the printing, never before it: a file that cannot be written (the folder is not there,
    # run_state.csv is open in a spreadsheet, which locks it on Windows) must not take the summary away
    save <- function(data, path) tryCatch({ suppressWarnings(readr::write_csv(data, path)); NULL }, error = function(e) paste0(basename(path), " (", gsub("\\s+", " ", conditionMessage(e)), ")"))
    file <- NULL; unsaved <- character()
    if (nrow(cmp$now) || nrow(cmp$solved)) {                  # every line of the places of this run, the kept ones included
      file <- file.path(CONFIG$dir_reports, paste0("run_", format(JOURNAL$started, "%Y%m%d_%H%M%S"), ".csv"))
      cols <- c("status", "title", "place", "phase", "kind", "n", "first_seen", "text")
      all <- cmp$now[, cols]
      if (nrow(cmp$solved)) all <- rbind(all, data.frame(status = "solved", title = cmp$solved$title, place = cmp$solved$place, phase = cmp$solved$phase, kind = cmp$solved$kind,
                                                         n = as.integer(cmp$solved$n), first_seen = cmp$solved$first_seen, text = cmp$solved$text, stringsAsFactors = FALSE))
      all <- cbind(run = JOURNAL$run, all)
      w <- save(all, file); if (!is.null(w)) { unsaved <- c(unsaved, w); file <- NULL }
    }
    n_new <- sum(cmp$now$status %in% c("new", "event")); n_all <- sum(cmp$now$status != "kept")     # the lines this run saw
    JOURNAL$file <- file
    JOURNAL$result <- if (!is.null(error)) paste("error:", error)
                      else if (n_all == 0) "ok"
                      else if (cmp$first_run) sprintf("ok, %d line(s) in the summary (first summary: all of them are the reference)", n_all)
                      else sprintf("ok, %d line(s) in the summary, %d new%s", n_all, n_new, if (nrow(cmp$solved)) sprintf(", %d no longer happening", nrow(cmp$solved)) else "")
    journal_mute(journal_print(cmp, file))
    if (unread) journal_mute(message("(", basename(journal_state_file()), " is there but could not be read: this run had nothing to compare itself with, as a first run.)"))
    w <- save(cmp$state[, JOURNAL_COLS], journal_state_file()); if (!is.null(w)) unsaved <- c(unsaved, w)
    if (length(unsaved)) journal_mute(message("(Not saved: ", paste(unsaved, collapse = "; "), ". Is the file open in another program? ",
                                              "The next run will compare itself with the last summary that was saved.)"))
    TRUE
  }, error = function(e) { JOURNAL$result <- if (!is.null(error)) paste("error:", error) else "ok (the summary could not be written)"; journal_mute(message("(The summary of the run could not be written: ", conditionMessage(e), ")")); FALSE })
  invisible(ok)
}

# --- Freshness (F1): the last period loaded against the rhythm of its source -----------------------------
# A source that stops publishing, or whose address changed, fails without an error: the update "works" and
# the last period never moves. The check measures the months between the end of the last period loaded and
# today. The limits are generous on purpose (an alarm means that about two publications were missed): 8 months
# for figures by month or quarter, 20 for yearly figures; a module states its own with `freshness_months`
# (the Eurostat regional table arrives about two years after the year it describes: 36).
FRESHNESS <- list(sub_yearly = 8, yearly = 20)

# Last day of a period. Calendar years: Q1 ends 31 March ... Q4 31 December. Financial year y (April y-1 to
# March y, as in England and Wales and Scotland): Q1 ends 30 June y-1 ... Q4 31 March y.
period_end <- function(year, quarter = 4L, financial_year = FALSE) {
  quarter <- as.integer(quarter); year <- as.integer(year)
  month <- if (isTRUE(financial_year)) c(6L, 9L, 12L, 3L)[quarter] else c(3L, 6L, 9L, 12L)[quarter]
  if (isTRUE(financial_year) && quarter < 4L) year <- year - 1L
  month_end(as.Date(sprintf("%d-%02d-01", year, month)))
}
month_end <- function(d) { d <- as.Date(format(as.Date(d), "%Y-%m-01")); seq(d, by = "month", length.out = 2)[2] - 1 }

age_text <- function(months) {
  months <- max(0, floor(months))
  if (months < 24) sprintf("%d month%s", months, if (months == 1) "" else "s") else sprintf("%d years%s", months %/% 12, if (months %% 12) sprintf(" %d month%s", months %% 12, if (months %% 12 == 1) "" else "s") else "")
}

# list(months, limit, old, text) for a period that ended on `end`
freshness <- function(end, yearly = FALSE, limit = NULL, today = Sys.Date()) {
  months <- as.numeric(difftime(as.Date(today), as.Date(end), units = "days")) / 30.4375
  limit <- as.numeric(limit %||% if (isTRUE(yearly)) FRESHNESS$yearly else FRESHNESS$sub_yearly)
  old <- isTRUE(months > limit)
  list(months = months, limit = limit, old = old,
       text = sprintf("nothing newer has arrived: the last period ended %s ago (this source is expected within %.0f months)", age_text(months), limit))
}

# --- Versions (F7): R and the packages, written down when they change ------------------------------------
# reports/versions_<date>.csv holds the versions an update ran with; a new file is written only when something
# differs from the last one, and the run says what changed. When something breaks after an upgrade, the last
# set that worked is known. (Lighter than renv, which would add a tool and a library per project.)
versions_snapshot <- function() {
  pk <- unique(c(PACKAGES, names(PACKAGES_OPTIONAL)))
  v <- vapply(pk, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed"), "")
  ext <- tryCatch(sf::sf_extSoftVersion(), error = function(e) character())
  ext <- ext[intersect(c("GEOS", "GDAL", "PROJ"), names(ext))]
  data.frame(item = c("R", "platform", pk, names(ext)), version = c(paste(R.version$major, R.version$minor, sep = "."), R.version$platform, unname(v), unname(ext)), stringsAsFactors = FALSE)
}
versions_last <- function() {
  f <- list.files(CONFIG$dir_reports, "^versions_[0-9]{8}(_[0-9]{6})?\\.csv$", full.names = TRUE)
  if (length(f) == 0) return(NULL)
  # The newest by the date and time in its name, read as digits: sorted as text, "versions_20261116.csv" and
  # "versions_20261116_101500.csv" change places with the language of the computer (a dot and an underscore have no
  # fixed order), and the first snapshot of a day would pass for the last
  stamp <- gsub("[^0-9]", "", basename(f)); stamp <- paste0(stamp, strrep("0", pmax(0, 14 - nchar(stamp))))
  f <- f[order(stamp, decreasing = TRUE, method = "radix")][1]
  d <- tryCatch(suppressWarnings(as.data.frame(readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE), stringsAsFactors = FALSE)), error = function(e) NULL)
  if (is.null(d) || !all(c("item", "version") %in% names(d))) return(NULL)
  list(file = f, data = d)
}
# What differs between two snapshots, as text: "duckdb 1.1.3 -> 1.2.0"
versions_diff <- function(old, new) {
  items <- union(old$item, new$item)
  a <- old$version[match(items, old$item)]; b <- new$version[match(items, new$item)]
  ch <- is.na(a) | is.na(b) | a != b
  sprintf("%s %s -> %s", items[ch], ifelse(is.na(a[ch]), "(not recorded)", a[ch]), ifelse(is.na(b[ch]), "(removed)", b[ch]))
}
versions_check <- function() {
  now <- versions_snapshot(); last <- versions_last()
  changes <- if (is.null(last)) character() else versions_diff(last$data, now)
  if (!is.null(last) && length(changes) == 0) return(invisible(character()))
  dir.create(CONFIG$dir_reports, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(CONFIG$dir_reports, paste0("versions_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  if (file.exists(f)) f <- file.path(CONFIG$dir_reports, paste0("versions_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  readr::write_csv(now, f)
  if (is.null(last)) message("Versions of R and the packages written down for the first time: ", f)
  else journal_say("versions", sprintf("R or packages changed since %s: %s (now in %s)", sub("^versions_([0-9]{4})([0-9]{2})([0-9]{2}).*$", "\\1-\\2-\\3", basename(last$file)), paste(changes, collapse = "; "), basename(f)),
                   place = "versions", phase = "status", title = "R and packages")
  invisible(changes)
}
