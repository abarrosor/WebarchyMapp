# Old copies of downloaded files: a listing, nothing else -----------------------------------------------
# (proposal F8 of Claude/06_technical/future-proof-analysis-2026-11.md, option B; decision P-057)
#
# A file that a portal republishes whole is saved again under a new date (data/raw/<source>/YYYYMMDD_<name>,
# R/02_download.R) whenever its content changes, and the older copies stay: raw files are never deleted. The
# disk therefore grows by the size of every republished series, for ever. This file answers one question:
# which copies has a newer copy of the same file replaced, how much room do they take, and is it safe to
# move them to another disk?
#
# It only reads and lists. It never moves, renames or deletes a file: moving is done by hand, in the
# file explorer, with the listing in front (reports/old_copies_<date>.csv and .txt).
#
# A copy is listed as "can be moved" only when all of this holds:
#   - another copy of the same name (the name without its date) exists in the same folder and is newer, both by
#     the date in its name and by the time of the file (the loaders take the newest by the time of the file: when
#     the two orders disagree nothing is recommended);
#   - no row of the database says it came from that copy (the staging tables keep the name of the file of every
#     row; a series built from several copies, as London's, keeps them all).
# A copy whose content is identical to the newer one (same SHA-256) is marked: it holds nothing the newer lacks.
# Files without a date in their name (Madrid's monthly files, the Crime Balance...) are single copies and are
# never listed.

# The date-and-time stamp and the plain name of a versioned file: "20261103_file.csv", "20261110_081933_file.csv".
# Six digits after the date are a time only when they can be one: "20261103_200076_padron.csv" is the file
# "200076_padron.csv" saved on 3 November, not a file saved at 20:00:76.
old_copies_parse <- function(file) {
  b <- basename(file)
  m <- regmatches(b, regexec("^([0-9]{8})(?:_((?:[01][0-9]|2[0-3])[0-5][0-9][0-5][0-9]))?_(.+)$", b, perl = TRUE))
  ok <- lengths(m) == 4
  stamp <- rep(NA_character_, length(b)); name <- rep(NA_character_, length(b))
  stamp[ok] <- vapply(m[ok], function(x) paste0(x[2], if (nzchar(x[3])) x[3] else "000000"), "")
  name[ok] <- vapply(m[ok], function(x) x[4], "")
  valid <- ok & !is.na(as.Date(substr(stamp, 1, 8), "%Y%m%d"))                       # "20269999_x" is not a date: not a versioned copy
  data.frame(file = file, stamp = ifelse(valid, stamp, NA_character_), name = ifelse(valid, name, NA_character_), stringsAsFactors = FALSE)
}

# Names of the files the rows of the database came from (every table with a `file` column, the log of loads left
# out: it remembers every file ever loaded). NULL when there is no database to ask.
old_copies_in_database <- function(con) {
  if (is.null(con)) return(NULL)
  cols <- tryCatch(DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.columns WHERE column_name = 'file' AND table_schema = 'main'")$table_name, error = function(e) NULL)
  if (is.null(cols)) return(NULL)
  views <- tryCatch(DBI::dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_type = 'VIEW'")$table_name, error = function(e) character())
  out <- character()
  for (t in setdiff(cols, c("ingest_log", views))) {
    v <- tryCatch(DBI::dbGetQuery(con, paste0("SELECT DISTINCT file FROM ", DBI::dbQuoteIdentifier(con, t)))$file, error = function(e) character())
    out <- c(out, basename(trimws(unlist(strsplit(as.character(v[!is.na(v)]), ";", fixed = TRUE)))))   # "a.csv;b.csv": a series built from several files
  }
  unique(out)
}

# The copies that a newer copy of the same file replaced: one row per old copy.
# verdict: "can be moved", "keep: rows of the database come from it", "keep: dates disagree", "not checked: no database"
old_copies <- function(dir = CONFIG$dir_raw, con = NULL) {
  empty <- data.frame(folder = character(), file = character(), size_mb = numeric(), replaced_by = character(), identical = logical(), in_database = logical(), verdict = character(), path = character(), stringsAsFactors = FALSE)
  f <- list.files(dir, recursive = TRUE, full.names = TRUE)
  if (length(f) == 0) return(empty)
  p <- old_copies_parse(f); p <- p[!is.na(p$stamp), ]
  if (nrow(p) == 0) return(empty)
  p$folder <- dirname(p$file); p$group <- paste(p$folder, p$name, sep = "/")
  p <- p[p$group %in% p$group[duplicated(p$group)], ]
  if (nrow(p) == 0) return(empty)
  info <- file.info(p$file); p$size <- info$size; p$mtime <- info$mtime
  used <- old_copies_in_database(con)
  root <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  relative <- function(d) { d <- normalizePath(d, winslash = "/", mustWork = FALSE); if (startsWith(d, root)) sub("^/", "", substring(d, nchar(root) + 1)) else d }
  rows <- list()
  for (g in unique(p$group)) {
    x <- p[p$group == g, ]; x <- x[order(x$stamp, x$mtime), ]
    newest <- x[nrow(x), ]; old <- x[-nrow(x), ]
    agree <- newest$mtime >= max(x$mtime)                                   # the newest by its name is also the newest file
    hash_new <- NULL
    for (i in seq_len(nrow(old))) {
      same <- FALSE
      if (isTRUE(old$size[i] == newest$size)) { hash_new <- hash_new %||% sha256_file(newest$file); same <- identical(sha256_file(old$file[i]), hash_new) }
      in_db <- if (is.null(used)) NA else basename(old$file[i]) %in% used
      verdict <- if (!agree) "keep: the dates in the names and the times of the files disagree"
                 else if (is.na(in_db)) "not checked: no database to ask"
                 else if (in_db) "keep: rows of the database come from it"
                 else "can be moved"
      rows[[length(rows) + 1]] <- data.frame(folder = relative(old$folder[i]), file = basename(old$file[i]), size_mb = round(old$size[i] / 1e6, 2), replaced_by = basename(newest$file),
                                             identical = same, in_database = in_db,
                                             verdict = verdict, path = normalizePath(old$file[i], winslash = "/", mustWork = FALSE), stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  out[order(out$folder, out$file), ]
}

# Menu option 17: print the listing by folder and save it. Nothing is moved.
list_old_copies <- function(save = TRUE) {
  step("Old copies of downloaded files (a listing: nothing is moved or deleted)")
  con <- if (file.exists(CONFIG$db_path)) tryCatch(db_connect_readonly(), error = function(e) NULL) else NULL
  if (!is.null(con)) on.exit(db_disconnect(con), add = TRUE)
  if (is.null(con)) message("No database to ask (", CONFIG$db_path, "): nothing will be recommended.")
  all_files <- list.files(CONFIG$dir_raw, recursive = TRUE, full.names = TRUE)
  total <- sum(file.size(all_files), na.rm = TRUE)
  x <- old_copies(CONFIG$dir_raw, con)
  message(sprintf("data/raw holds %s files, %s MB.", fmt_int(length(all_files)), fmt_int(round(total / 1e6))))
  if (nrow(x) == 0) { message("No file has an older copy: nothing to move."); return(invisible(x)) }
  move <- x[x$verdict == "can be moved", ]
  message(sprintf("%d older copies of %d files, %s MB; %d of them can be moved (%s MB), %d must stay.",
                  nrow(x), length(unique(paste(x$folder, x$replaced_by))), fmt_int(round(sum(x$size_mb))), nrow(move), fmt_int(round(sum(move$size_mb))), nrow(x) - nrow(move)))
  for (fo in unique(x$folder)) {
    r <- x[x$folder == fo, ]
    message(sprintf("  %-44s %3d copies %8s MB   can be moved: %d (%s MB)%s", fo, nrow(r), fmt_int(round(sum(r$size_mb), 1)), sum(r$verdict == "can be moved"),
                    fmt_int(round(sum(r$size_mb[r$verdict == "can be moved"]), 1)), if (any(r$identical)) sprintf("; identical to the newer copy: %d", sum(r$identical)) else ""))
  }
  stay <- x[x$verdict != "can be moved", ]
  if (nrow(stay)) for (v in unique(stay$verdict)) message(sprintf("  %d %s", sum(stay$verdict == v), v))
  if (save) {
    dir.create(CONFIG$dir_reports, showWarnings = FALSE, recursive = TRUE)
    f <- file.path(CONFIG$dir_reports, paste0("old_copies_", format(Sys.Date(), "%Y%m%d"), ".csv"))
    readr::write_csv(x, f)
    message("Listing: ", f)
    if (nrow(move)) {
      t <- sub("\\.csv$", ".txt", f)
      writeLines(gsub("/", if (.Platform$OS.type == "windows") "\\\\" else "/", move$path), t, useBytes = TRUE)
      message("The ", nrow(move), " that can be moved, one path per line: ", t)
      message("To free the room: move those files to another disk by hand, keeping the folder names (so that they can come back). The project never deletes a download.")
    }
  }
  invisible(x)
}
