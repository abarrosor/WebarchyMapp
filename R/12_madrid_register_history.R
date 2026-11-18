# Madrid: the historical register, one file per year (proposal F4) -------------------------------------
#
# HALF VERIFIED (16 November 2026). datos.madrid.es cannot be read from the place this code was written in.
# The founder's first run of option 18 sent back the portal's list of files, and that part is now checked
# against the real thing: 354 files, 153 of them CSV, one per month from January 2014 to September 2026, each
# described as "Padrón municipal. Histórico. 2024. Diciembre" (the month is in the description, not in the
# name; the address of the newest ones carries the date of the export, which is not the month of the file).
# The shape of the old files themselves has still not been seen. The project's own rule applies ("a reader is
# not verified until it has read the real file"): until a run that downloads them has been read, the loading
# is a careful attempt, built so that a wrong guess loads nothing instead of loading something wrong:
#   - the first thing it does is to save the list of files the portal gives (reports/), whatever comes next;
#   - a file is loaded only if it has the columns of today's register (the same reader, R/10_madrid_calls.R),
#     a reference date that is known (inside the file, or in the portal's own name for it), a total between
#     2.8 and 3.8 million residents, and neighbourhoods that are today's;
#   - a district whose neighbourhoods were others in that year (Vicálvaro and Villa de Vallecas before the
#     2017 reform, as far as is known) is left out for that year: a code that meant another area must not
#     be stored under today's name. Its rates keep standing on the nearest register, marked † in the app;
#   - anything that does not fit is reported, left out and listed in the run summary.
#
# Why. Madrid's register dataset (200076-0-padron) is one snapshot a month, kept from October 2026 on. The
# calls exist since January 2026 only and have their register; the police activity by district goes back to
# 2014, and its months before 2026 stand on the register of 2026 (†). With one register per year (the first
# of each year) those months stand on the residents of their own year.
#
# Use: menu option 18. It lists, proposes one file per year, says how many it would download (about 35 MB
# each today) and asks before downloading. Loading again later needs nothing: madrid_census_ingest() reads
# the folder at every update and after a rebuild.

CONFIG$sources$madrid_census_history <- list(
  ckan_id = "209163-0-padron-municipal-historico",       # NOT VERIFIED: the id as remembered from the portal's catalogue
  format  = "csv",
  dir_raw = "data/raw/city_madrid/census_history"
)

MADRID_MONTHS_ES <- c(enero = 1, febrero = 2, marzo = 3, abril = 4, mayo = 5, junio = 6, julio = 7, agosto = 8, septiembre = 9, setiembre = 9, octubre = 10, noviembre = 11, diciembre = 12)

# The month a resource of the portal describes, from its own words (description, name or address): "2024. Diciembre"
# (what the real dataset writes, seen on 16 November 2026: "Padrón municipal. Histórico. 2024. Diciembre"), "enero 2014",
# "1 de enero de 2014", "201401", "2014-01", "01/2014". NA when none of them says it. Never from a download date.
madrid_history_month <- function(text) {
  one <- function(x) {
    x <- tolower(enc2utf8(as.character(x))); if (is.na(x) || !nzchar(x)) return(as.Date(NA))
    mk <- function(y, m) { y <- as.integer(y); m <- as.integer(m); if (is.na(y) || is.na(m) || y < 1996 || y > 2100 || m < 1 || m > 12) as.Date(NA) else as.Date(sprintf("%d-%02d-01", y, m)) }
    names_re <- paste(names(MADRID_MONTHS_ES), collapse = "|")
    r <- regmatches(x, regexec(paste0("(", names_re, ")\\s*(?:de|del)?\\s*((?:19|20)[0-9]{2})"), x, perl = TRUE))[[1]]
    if (length(r) == 3) return(mk(r[3], MADRID_MONTHS_ES[[r[2]]]))
    r <- regmatches(x, regexec(paste0("(?<![0-9])((?:19|20)[0-9]{2})[\\s.,;:/_-]{1,4}(", names_re, ")(?![a-z])"), x, perl = TRUE))[[1]]      # "2024. diciembre": the year, then the month
    if (length(r) == 3) return(mk(r[2], MADRID_MONTHS_ES[[r[3]]]))
    r <- regmatches(x, regexec("(?<![0-9])((?:19|20)[0-9]{2})(0[1-9]|1[0-2])(?:0[1-9]|[12][0-9]|3[01])(?![0-9])", x, perl = TRUE))[[1]]     # 20140101
    if (length(r) == 3) return(mk(r[2], r[3]))
    r <- regmatches(x, regexec("(?<![0-9])((?:19|20)[0-9]{2})[-_/. ]?(0[1-9]|1[0-2])(?![0-9])", x, perl = TRUE))[[1]]
    if (length(r) == 3) return(mk(r[2], r[3]))
    r <- regmatches(x, regexec("(?<![0-9])(0[1-9]|1[0-2])[-_/. ]((?:19|20)[0-9]{2})(?![0-9])", x, perl = TRUE))[[1]]
    if (length(r) == 3) return(mk(r[3], r[2]))
    as.Date(NA)
  }
  as.Date(unname(vapply(as.character(text), function(x) as.numeric(one(x)), numeric(1))), origin = "1970-01-01")
}

# One resource per year: the earliest month the portal has of each year (January when it is there)
madrid_history_pick <- function(res, years) {
  res <- res[!is.na(res$month), , drop = FALSE]
  res$year <- as.integer(format(res$month, "%Y"))
  res <- res[res$year %in% years, , drop = FALSE]
  res <- res[order(res$month, res$name), , drop = FALSE]
  res[!duplicated(res$year), , drop = FALSE]
}

# The month of each file of the portal's list (columns description, name, url). The description first: it is where
# the portal writes the month ("... Histórico. 2024. Diciembre"). Then the name. The file name in the address comes
# last, and never when it carries the date and time of an export ("209163_20261029_160951.csv" is the file of
# September 2026, written out on 29 October).
madrid_history_months <- function(res) {
  text <- function(x) if (is.null(x)) rep(NA_character_, nrow(res)) else as.character(x)
  month <- madrid_history_month(text(res$description))
  miss <- is.na(month); if (any(miss)) month[miss] <- madrid_history_month(text(res$name)[miss])
  address <- basename(sub("\\?.*$", "", text(res$url))); stamped <- grepl("(?<![0-9])(?:19|20)[0-9]{6}_[0-9]{6}(?![0-9])", address, perl = TRUE)
  miss <- is.na(month) & !stamped; if (any(miss)) month[miss] <- madrid_history_month(address[miss])
  month
}

# The list of files the portal gives for the dataset, with the month each one describes; saved in reports/
madrid_history_list <- function(save = TRUE) {
  src <- CONFIG$sources$madrid_census_history
  res <- as.data.frame(ckan_resources(src$ckan_id), stringsAsFactors = FALSE)
  res$month <- madrid_history_months(res)
  res$file <- ckan_file_name(res)
  if (save) {
    dir.create(CONFIG$dir_reports, showWarnings = FALSE, recursive = TRUE)
    f <- file.path(CONFIG$dir_reports, paste0("madrid_register_history_", format(Sys.Date(), "%Y%m%d"), ".csv"))
    readr::write_csv(res, f); message("The portal's list of files, as received: ", f)
  }
  res
}

# The years the historical register is wanted for: from the first year of the police activity to the year before
# the first register already on disk
madrid_history_years <- function(con) {
  first_activity <- tryCatch(DBI::dbGetQuery(con, "SELECT min(year(month))::INTEGER AS y FROM stg_madrid_activity")$y, error = function(e) NA_integer_)
  first_register <- tryCatch(DBI::dbGetQuery(con, paste0("SELECT min(year(ref_date))::INTEGER AS y FROM ref_population WHERE territory_id LIKE '", ID_MADRID, "-%' AND file NOT LIKE 'history:%'"))$y, error = function(e) NA_integer_)
  from <- if (is.na(first_activity)) 2014L else first_activity
  to <- (if (is.na(first_register)) as.integer(format(Sys.Date(), "%Y")) else first_register) - 1L
  if (to < from) integer() else seq.int(from, to)
}

# Read one historical file and decide what of it can be loaded. Returns list(ok, reason, ref_date, pop, left_out).
# `known`: the neighbourhood ids of today (dim_territory); `listed`: the month the portal's list gives for the file (or NA)
madrid_history_read <- function(path, known, listed = as.Date(NA)) {
  fail <- function(reason) list(ok = FALSE, reason = reason)
  raw <- tryCatch(madrid_census_read_strict(path), error = function(e) e)
  if (inherits(raw, "error")) return(fail(conditionMessage(raw)))
  inside <- attr(raw, "ref_date")
  if (!is.na(inside) && !is.na(listed) && abs(as.numeric(inside - listed)) > 45) return(fail(sprintf("the file says %s and the portal's list says %s: not loaded until it is clear which", format(inside, "%Y-%m"), format(listed, "%Y-%m"))))
  ref_date <- if (!is.na(inside)) inside else listed
  if (is.na(ref_date)) return(fail("no reference date inside the file (FX_DATOS_INI) nor in the portal's name for it"))
  ref_date <- as.Date(format(ref_date, "%Y-%m-01"))
  pop <- madrid_census_by_neighbourhood(raw)
  total <- sum(pop$population)
  if (total < 2.8e6 || total > 3.8e6) return(fail(sprintf("%s residents in all: not the register of Madrid as this reader understands it", fmt_int(total))))
  # Districts whose neighbourhoods are exactly today's: the others are left out for this year
  district_of <- function(id) sub("-B[0-9]+$", "", id)
  today <- split(known, district_of(known)); then <- split(pop$territory_id, district_of(pop$territory_id))
  same <- vapply(names(today), function(d) setequal(today[[d]], then[[d]] %||% character()), logical(1))
  left <- names(today)[!same]
  keep <- pop[district_of(pop$territory_id) %in% names(today)[same], , drop = FALSE]
  if (nrow(keep) == 0) return(fail("none of its districts has today's neighbourhoods"))
  list(ok = TRUE, reason = "", ref_date = ref_date, pop = keep, left_out = left, total = total)
}

# Today's reader, without its fallback to the date in the file name (the names of the historical files are not known)
madrid_census_read_strict <- function(path) {
  header_raw <- readLines(path, n = 1, warn = FALSE)
  enc <- if (validUTF8(header_raw)) "UTF-8" else "latin1"
  header <- sub("^﻿", "", iconv(header_raw, enc, "UTF-8"))
  sep <- if (grepl(";", header, fixed = TRUE)) ";" else ","
  df <- readr::read_delim(path, delim = sep, col_types = readr::cols(.default = readr::col_character()), locale = readr::locale(encoding = enc), trim_ws = TRUE, progress = FALSE)
  names(df) <- chartr("ÁÉÍÓÚÑ", "AEIOUN", toupper(trimws(sub("^﻿", "", names(df)))))
  missing <- setdiff(CENSUS_REQUIRED, names(df))
  if (length(missing) > 0) stop("columns missing (", toString(missing), "); the file has: ", paste(utils::head(names(df), 20), collapse = " | "))
  num <- function(x) { x <- suppressWarnings(as.numeric(trimws(x))); x[is.na(x)] <- 0; x }
  df$people <- num(df$ESPANOLESHOMBRES) + num(df$ESPANOLESMUJERES) + num(df$EXTRANJEROSHOMBRES) + num(df$EXTRANJEROSMUJERES)
  dates <- if ("FX_DATOS_INI" %in% names(df)) unique(stats::na.omit(as.Date(substr(df$FX_DATOS_INI, 1, 10)))) else as.Date(character())
  attr(df, "ref_date") <- if (length(dates) == 1) dates else as.Date(NA)
  df
}

# Load the historical files on disk (called by madrid_census_ingest() at every update, and by option 18).
# The index written by the download (resources.csv in the folder) gives the month the portal states for each file.
madrid_history_ingest <- function(con) {
  src <- CONFIG$sources$madrid_census_history
  files <- list.files(src$dir_raw, pattern = "\\.csv$", full.names = TRUE)
  files <- files[basename(files) != "resources.csv"]
  if (length(files) == 0) return(invisible(NULL))
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE '", ID_MADRID, "-%'"))$territory_id
  if (length(known) == 0) stop("Load the Madrid territory first")
  idx <- file.path(src$dir_raw, "resources.csv")
  index <- if (file.exists(idx)) tryCatch(as.data.frame(readr::read_csv(idx, col_types = readr::cols(.default = "c"), progress = FALSE)), error = function(e) NULL) else NULL
  done <- DBI::dbGetQuery(con, "SELECT DISTINCT file FROM ref_population")$file
  for (path in sort(files)) {
    tag <- paste0("history:", basename(path))
    if (tag %in% done) { message("= ", basename(path), " (historical register, already loaded)"); next }
    listed <- if (!is.null(index) && basename(path) %in% index$file) as.Date(index$month[match(basename(path), index$file)]) else as.Date(NA)
    r <- madrid_history_read(path, known, listed)
    if (!r$ok) { message("  [!!] historical register ", basename(path), " not loaded: ", r$reason); next }
    # A month that already has its own register (a monthly file of the current dataset) keeps it: the historical file
    # may hold fewer districts, and the two readers would otherwise replace each other's rows at every update
    own <- DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM ref_population WHERE ref_date = DATE '", r$ref_date, "' AND territory_id LIKE '", ID_MADRID, "-%' AND (file IS NULL OR file NOT LIKE 'history:%')"))$n
    if (own > 0) { message("= ", basename(path), " (historical register of ", format(r$ref_date, "%Y-%m"), "): that month already has its own register, which is kept"); next }
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE ref_date = DATE '", r$ref_date, "' AND territory_id LIKE '", ID_MADRID, "-%'"))
      DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = r$pop$territory_id, ref_date = r$ref_date, population = as.integer(r$pop$population), file = tag))
    })
    message(sprintf("+ %s: historical register at %s = %s residents in the file, %d neighbourhoods loaded%s", basename(path), format(r$ref_date, "%Y-%m-%d"), fmt_int(r$total), nrow(r$pop),
                    if (length(r$left_out)) sprintf("; %d district(s) left out because their neighbourhoods were not today's: %s", length(r$left_out), paste(sub("^.*-D", "", r$left_out), collapse = ", ")) else ""))
  }
  invisible(TRUE)
}

# Menu option 18: list, propose one file per year, ask, download, load, rebuild the tables for the app
madrid_register_history <- function(ask = interactive()) {
  step("Madrid: the historical register (one file per year)")
  src <- CONFIG$sources$madrid_census_history
  message("The list of files of this dataset was read on 16 November 2026 (one file per month since January 2014, the month in its description); ",
          "the files themselves have not been seen yet: each one is loaded only if it passes every check, and what is left out is said.")
  res <- tryCatch(madrid_history_list(), error = function(e) {
    message("[!!] The portal did not give the list of files of '", src$ckan_id, "': ", conditionMessage(e),
            "\n     If the dataset has another id, it is the last part of its address in datos.madrid.es; it is set in R/12_madrid_register_history.R (ckan_id).")
    NULL })
  if (is.null(res)) return(invisible(FALSE))
  csv <- res[res$format == "csv" | grepl("\\.csv$", res$url, ignore.case = TRUE), , drop = FALSE]
  message(sprintf("%d files in the dataset, %d of them CSV; the portal says the month of %d of them (from %s to %s).", nrow(res), nrow(csv), sum(!is.na(csv$month)),
                  if (any(!is.na(csv$month))) format(min(csv$month, na.rm = TRUE), "%m/%Y") else "?", if (any(!is.na(csv$month))) format(max(csv$month, na.rm = TRUE), "%m/%Y") else "?"))
  if (nrow(csv) == 0 || all(is.na(csv$month))) { message("[!!] No CSV file with a recognisable month: nothing is downloaded. The list saved in reports/ shows what the portal has."); return(invisible(FALSE)) }
  years <- with_db(madrid_history_years)
  pick <- madrid_history_pick(csv, years)
  if (nrow(pick) == 0) { message("No file for the years wanted (", if (length(years)) paste(range(years), collapse = "-") else "none", "): nothing to do."); return(invisible(FALSE)) }
  dir.create(src$dir_raw, recursive = TRUE, showWarnings = FALSE)
  pick$dest <- file.path(src$dir_raw, pick$file)
  new <- pick[!file.exists(pick$dest), , drop = FALSE]
  message(sprintf("One register per year, %d to %d: %d file(s), %d already on disk.", min(pick$year), max(pick$year), nrow(pick), nrow(pick) - nrow(new)))
  said <- ifelse(is.na(pick$description) | !nzchar(pick$description), pick$name, pick$description)        # the portal's own words for the file
  for (i in seq_len(nrow(pick))) message(sprintf("  %d  %s  %s%s", pick$year[i], format(pick$month[i], "%m/%Y"), said[i], if (file.exists(pick$dest[i])) "  (on disk)" else ""))
  missing_years <- setdiff(years, pick$year); if (length(missing_years)) message("  years without a file in the portal's list: ", paste(missing_years, collapse = ", "))
  if (nrow(new) > 0) {
    if (!ask) { message("Nothing is downloaded without a yes: run this option from the menu."); return(invisible(FALSE)) }
    r <- readline(sprintf("Download these %d file(s) now? Each is about 35 MB today; older ones may be smaller. (y/N): ", nrow(new)))
    if (!tolower(trimws(r)) %in% c("y", "yes", "s", "si", "sí")) { message("Nothing downloaded."); return(invisible(FALSE)) }
    for (i in seq_len(nrow(new))) {
      ok <- tryCatch({ download_safely(new$url[i], new$dest[i]); TRUE }, error = function(e) { message("x ", basename(new$dest[i]), " not downloaded (", conditionMessage(e), ")"); FALSE })
      if (ok) { message("+ ", basename(new$dest[i])); Sys.sleep(1) }
    }
  }
  # The index: the month the portal states for each file (the reader uses it when the file itself does not say)
  have <- pick[file.exists(pick$dest), , drop = FALSE]
  idx <- file.path(src$dir_raw, "resources.csv")
  old <- if (file.exists(idx)) tryCatch(as.data.frame(readr::read_csv(idx, col_types = readr::cols(.default = "c"), progress = FALSE)), error = function(e) NULL) else NULL
  index <- data.frame(file = have$file, month = format(have$month), name = have$name, url = have$url, stringsAsFactors = FALSE)
  if (!is.null(old) && all(c("file", "month", "name", "url") %in% names(old))) index <- rbind(old[!old$file %in% index$file, c("file", "month", "name", "url")], index)
  readr::write_csv(index, idx)
  with_journal("Madrid historical register", download = FALSE, with_db(function(con) {
    step("Madrid: the historical register (one file per year)", place = "MAD/history", label = "Madrid (MAD) · historical register")
    attempt("Madrid historical register not loaded", madrid_history_ingest(con))
    step("Tables for the app")
    build_marts(con)
    y <- DBI::dbGetQuery(con, paste0("SELECT year(ref_date)::INTEGER AS year, count(*) AS neighbourhoods, sum(population) AS residents, min(file) AS file FROM ref_population WHERE territory_id LIKE '", ID_MADRID, "-%' GROUP BY 1 ORDER BY 1"))
    journal_mute({ message("Registers of Madrid in the database, by year:"); for (i in seq_len(nrow(y))) message(sprintf("  %d  %3d neighbourhoods  %s residents  (%s)", y$year[i], y$neighbourhoods[i], fmt_int(y$residents[i]), y$file[i])) })
  }))
  invisible(TRUE)
}
