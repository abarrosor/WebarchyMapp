# Run by the Windows Task Scheduler (see R/61_schedule.R).
# No need to run it by hand: to test it, use the WEBARCHYMAPP.R menu.
# Does the same as option 1 (update everything) and writes a log in
# reports/update_YYYYMMDD_hhmmss.log

# --- Project folder: the one of this script, one level up ---
args <- commandArgs(trailingOnly = FALSE)
this <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
if (length(this) == 1) setwd(dirname(dirname(normalizePath(this))))
if (!file.exists("WebarchyMapp.Rproj")) stop("Project not found from ", getwd())
source("scripts/_bootstrap.R", encoding = "UTF-8")

dir.create(CONFIG$dir_reports, showWarnings = FALSE)
log_file <- file.path(CONFIG$dir_reports, paste0("update_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
log_con <- file(log_file, open = "wt", encoding = "UTF-8")
sink(log_con); sink(log_con, type = "message")
cat("Scheduled update -", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
result <- tryCatch({ update_all(); "ok" },
                   error = function(e) { message("ERROR: ", conditionMessage(e)); paste("error:", conditionMessage(e)) })
cat("\nRESULT:", result, "\n")
sink(type = "message"); sink(); close(log_con)

# Logs of the last 12 months are kept
old <- list.files(CONFIG$dir_reports, "^update_.*\\.log$", full.names = TRUE)
old <- old[file.mtime(old) < Sys.time() - 365 * 24 * 3600]
if (length(old) > 0) file.remove(old)
