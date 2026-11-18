# LIST THE OLD COPIES of downloaded files that a newer copy of the same file replaced (menu option 17).
# It only lists: nothing is moved, renamed or deleted. The listing is saved in reports/old_copies_<date>.csv, and the
# paths of the copies that can be moved to another disk, one per line, in reports/old_copies_<date>.txt.
# A copy is called movable only when a newer copy exists (by the date in its name and by the time of the file) and
# no row of the database comes from it. See R/95_old_copies.R and GUIDE.md, section 5c.

# --- Go to the project folder and load the project code (see scripts/_bootstrap.R) ---
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
source("scripts/_bootstrap.R", encoding = "UTF-8")
# -----------------------------------------------------------------------------------------

list_old_copies()
