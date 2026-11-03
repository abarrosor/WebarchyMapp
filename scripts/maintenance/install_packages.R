# Installs the missing R packages and prints their versions.

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

install_packages()
