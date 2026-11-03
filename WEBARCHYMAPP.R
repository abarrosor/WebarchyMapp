# WEBARCHYMAPP - Main menu
# Open this file and press Source (Ctrl + Shift + S). Choose an option by typing its number in the console.
# Everything here is also available in scripts/ if you prefer to run one step.

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

options_menu <- c(
  "Update everything (monthly routine)",
  "Open the app (English)",
  "Open the app in another language",
  "Show the data status",
  "Install or complete the R packages",
  "Test the automatic download (datos.madrid.es API)",
  "Update the district and neighbourhood boundaries (Madrid and Barcelona)",
  "Update only Spain, Barcelona and Mossos sources (not Madrid)",
  "Schedule the automatic update (weekly, without opening RStudio)",
  "Remove the automatic update",
  "Check whether the automatic update is scheduled and how the last run went",
  "Rebuild the database from scratch",
  "Update only the other countries (Country menu of the app)"
)
choice <- utils::menu(options_menu, title = "\nWebarchyMapp - What do you want to do?")
switch(as.character(choice),
  "1" = update_all(),
  "2" = open_app("en"),
  "3" = {
    langs <- c(es = "Spanish", ca = "Catalan", gl = "Galician", fr = "French", it = "Italian", de = "German",
               pt = "Portuguese", zh = "Chinese", ja = "Japanese", ko = "Korean")
    i <- utils::menu(paste0(langs, " (", names(langs), ")"), title = "Language")
    if (i > 0) open_app(names(langs)[i])
  },
  "4" = data_status(),
  "5" = install_packages(),
  "6" = test_api(),
  "7" = update_territory(),
  "8" = update_other_sources(),
  "9" = schedule_update_menu(),
  "10" = unschedule_update(),
  "11" = show_schedule_status(),
  "12" = rebuild_database(),
  "13" = update_countries(),
  message("Nothing chosen.")
)
