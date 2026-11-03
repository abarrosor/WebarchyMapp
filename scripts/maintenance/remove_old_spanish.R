# Final clean-up after the move to the English version (November 2026). Asks before acting.
# It deletes ONLY leftovers that the new version never reads:
#   - reports/calidad_*.csv            quality reports written by the old Spanish code
#   - .RData                           workspace saved by an old RStudio session (stale objects)
#   - _old_spanish/, data/werbarchy.duckdb, data/territorio_*.gpkg   (if still there)
#   - the Spanish documents in ../../Claude/ (00_gestion, 01_descubrimiento, 02_datos/*.md, 06_tecnico)
# It also offers to MOVE (never delete) your own copies of the Madrid CSV/PDF from
# ../../Claude/02_datos/ to ../../Claude/02_data/original-downloads/. Raw data in data/raw/ is never touched.
# When everything is done the script removes itself: nothing of the migration remains.

# --- Go to the project folder (see scripts/_bootstrap.R) --------------------------------
local({
  if (file.exists("WebarchyMapp.Rproj")) return(invisible())
  from <- c(tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA),
            tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) NA),
            sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)))
  for (d in normalizePath(from[!is.na(from) & nzchar(from)], mustWork = FALSE)) for (i in 0:3) {
    if (file.exists(file.path(d, "WebarchyMapp.Rproj"))) { setwd(d); return(invisible()) }
    d <- dirname(d)
  }
  stop("Project not found. In RStudio: File > Open Project... > WebarchyMapp.Rproj")
})
# -----------------------------------------------------------------------------------------

remove_old_spanish <- function(ask = interactive()) {
  yes <- function(q) { if (!ask) return(TRUE); tolower(trimws(readline(q))) %in% c("y", "yes", "s", "si") }
  docs <- normalizePath(file.path("..", "..", "Claude"), mustWork = FALSE)
  targets <- c(list.files("reports", "^calidad_.*\\.csv$", full.names = TRUE), ".RData",
               "_old_spanish", "data/werbarchy.duckdb", list.files("data", "^territorio_.*\\.gpkg$", full.names = TRUE))
  if (dir.exists(docs)) targets <- c(targets, file.path(docs, c("00_gestion", "01_descubrimiento", "06_tecnico")),
                                     list.files(file.path(docs, "02_datos"), "\\.md$", full.names = TRUE))
  targets <- targets[file.exists(targets)]
  all_ok <- TRUE
  if (length(targets) == 0) message("Nothing left to delete.") else {
    message("To be deleted:\n", paste("  -", targets, collapse = "\n"))
    if (yes("Delete these files and folders? (y/n): ")) {
      all_ok <- unlink(targets, recursive = TRUE, force = TRUE) == 0
      message(if (all_ok) "[ok] Removed." else "[!!] Some items could not be removed (a file in use? close the app and RStudio's viewer and try again).")
    } else { message("Cancelled."); all_ok <- FALSE }
  }
  # Your own copies of the Madrid CSV/PDF (the same files are in data/raw/madrid_calls): moved, never deleted
  old_docs <- file.path(docs, "02_datos")
  if (dir.exists(old_docs)) {
    files <- list.files(old_docs, full.names = TRUE)
    if (length(files) == 0) unlink(old_docs, recursive = TRUE)
    else {
      dest <- file.path(docs, "02_data", "original-downloads")
      message(length(files), " file(s) in ", old_docs, " (your own downloads of the Madrid calls).")
      if (yes(paste0("Move them to ", dest, "? Nothing is deleted. (y/n): "))) {
        dir.create(dest, recursive = TRUE, showWarnings = FALSE)
        ok <- file.rename(files, file.path(dest, basename(files)))
        if (all(ok) && length(list.files(old_docs, all.files = TRUE, no.. = TRUE)) == 0) unlink(old_docs, recursive = TRUE)
        message(if (all(ok)) "[ok] Moved." else "[!!] Some files could not be moved."); all_ok <- all_ok && all(ok)
      } else all_ok <- FALSE
    }
  }
  if (all_ok) {
    me <- "scripts/maintenance/remove_old_spanish.R"
    if (file.exists(me) && file.remove(me)) message("[ok] Clean-up finished; this script has removed itself (", me, ").")
  }
  invisible(all_ok)
}
remove_old_spanish()
