# Shared start-up for WEBARCHYMAPP.R and every script -------------------------------------
# 1. Moves to the project folder (works even if the .Rproj was not opened).
# 2. Sources every R/*.R file.
# Nothing here downloads or deletes anything. (The one-off migration from the
# Spanish version ran in November 2026 and was removed from this file.)

local({
  if (!file.exists("WebarchyMapp.Rproj")) {
    from <- c(tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NA),
              tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) NA),
              sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)))
    found <- FALSE
    for (d in from[!is.na(from) & nzchar(from)]) {
      d <- normalizePath(d, mustWork = FALSE)
      for (i in 0:3) {
        if (file.exists(file.path(d, "WebarchyMapp.Rproj"))) { setwd(d); found <- TRUE; break }
        d <- dirname(d)
      }
      if (found) break
    }
    if (!found) stop("Project not found. In RStudio: File > Open Project... > WebarchyMapp.Rproj")
  }
})

for (f in sort(list.files("R", full.names = TRUE, pattern = "\\.R$"))) source(f, encoding = "UTF-8")
