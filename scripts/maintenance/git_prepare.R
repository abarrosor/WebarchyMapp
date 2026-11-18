# PREPARE A COMMIT of the code (menu option 19): stages every change (git add -A), then checks every file the
# repository would hold: only the published folders, no data file, no e-mail address, no folder of a user, no drive
# path, no key. If a file fails, everything is unstaged again and the reasons are listed; no file is ever changed.
# It never commits and never pushes. See R/96_git_prepare.R and GUIDE.md, "Publishing the code on GitHub".

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

git_prepare()
