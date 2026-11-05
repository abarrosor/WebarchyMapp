# One-off: move the legacy raw-data folders of Spain, Madrid, Barcelona and the shared sources into
# the layout used by the other countries (see RAW_LAYOUT in R/00_config.R):
#   data/raw/crime_balance  -> data/raw/country_es/crime_balance      data/raw/madrid_calls -> data/raw/city_madrid/calls   ...
# Folders are MOVED with their files (file.rename on the folder): no file is edited, copied or deleted,
# and a legacy folder is left alone when its new location already exists (nothing is ever merged).
# Run it from the project root with the app closed and after Session > Restart R:
#   source("scripts/maintenance/migrate_raw_layout.R")
# Then option 4 (data status) must show the same sources as before. Dry run first with DRY_RUN <- TRUE.

if (!exists("DRY_RUN")) DRY_RUN <- FALSE     # set DRY_RUN <- TRUE in the console before sourcing to see the plan only

source("R/00_config.R")
log <- character()
say <- function(...) { m <- paste0(...); message(m); log <<- c(log, m) }
say("Raw-data layout migration · ", format(Sys.time(), "%Y-%m-%d %H:%M"), if (DRY_RUN) " · DRY RUN (nothing moved)" else "")
moved <- 0; skipped <- 0
for (new in names(RAW_LAYOUT)) {
  legacy <- RAW_LAYOUT[[new]]
  if (!dir.exists(legacy)) { next }
  if (dir.exists(new)) { say("  kept   ", legacy, "  (", new, " already exists: not merged, decide by hand)"); skipped <- skipped + 1; next }
  n <- length(list.files(legacy, recursive = TRUE))
  if (DRY_RUN) { say("  would move ", legacy, " -> ", new, "  (", n, " files)"); next }
  dir.create(dirname(new), recursive = TRUE, showWarnings = FALSE)
  ok <- file.rename(legacy, new)
  if (!isTRUE(ok)) { say("  FAILED ", legacy, " -> ", new, " (folder in use? close RStudio's Files pane and the app, then run again)"); skipped <- skipped + 1; next }
  say("  moved  ", legacy, " -> ", new, "  (", n, " files)"); moved <- moved + 1
}
say(moved, " folder(s) moved, ", skipped, " kept.")
if (!DRY_RUN) {
  dir.create(CONFIG$dir_reports, recursive = TRUE, showWarnings = FALSE)
  f <- file.path(CONFIG$dir_reports, paste0("raw_layout_migration_", format(Sys.Date(), "%Y%m%d"), ".txt"))
  writeLines(log, f); message("Log: ", f)
  message("Restart R (Session > Restart R) so that the paths are resolved again, then menu option 4.")
}
