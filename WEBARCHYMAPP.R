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
  "Update the district and neighbourhood boundaries of every city module",
  "Update only the cities (every city module)",
  "Schedule the automatic update (weekly, without opening RStudio)",
  "Remove the automatic update",
  "Check whether the automatic update is scheduled and how the last run went",
  "Rebuild the database from scratch",
  "Update only the other countries (Country menu of the app)",
  "Reload some countries or cities (type their codes; forces the reload after a fix, and can download again)",
  "Check the consistency of the data (cities and countries; report in reports/)",
  "Run the automated tests (in a temporary copy of the code: data/ and the app are not touched)",
  "List the old copies of downloaded files that a newer copy replaced (a listing in reports/: nothing is moved or deleted)",
  "Madrid: look at the historical register and load one file per year (asks before downloading)",
  "Prepare a commit of the code for GitHub (git add, then a check of every file; never commits or pushes)"
)
choice <- utils::menu(options_menu, title = "\nWebarchyMapp - What do you want to do?")
switch(as.character(choice),
  "1" = update_all(),
  "2" = open_app("en"),
  "3" = {
    # Spanish first, then by English name: the 31 languages of app/translations.csv besides English
    langs <- c(es = "Spanish", sq = "Albanian", bg = "Bulgarian", ca = "Catalan", zh = "Chinese", hr = "Croatian", cs = "Czech",
               da = "Danish", nl = "Dutch", et = "Estonian", fi = "Finnish", fr = "French", gl = "Galician", de = "German",
               el = "Greek", hu = "Hungarian", is = "Icelandic", it = "Italian", ja = "Japanese", ko = "Korean", lv = "Latvian",
               lt = "Lithuanian", no = "Norwegian", pl = "Polish", pt = "Portuguese", ro = "Romanian", sr = "Serbian",
               sk = "Slovak", sl = "Slovenian", sv = "Swedish", tr = "Turkish")
    i <- utils::menu(paste0(langs, " (", names(langs), ")"), title = "Language")
    if (i > 0) open_app(names(langs)[i])
  },
  "4" = data_status(),
  "5" = install_packages(),
  "6" = test_api(),
  "7" = update_territory(),
  "8" = update_cities(),
  "9" = schedule_update_menu(),
  "10" = unschedule_update(),
  "11" = show_schedule_status(),
  "12" = rebuild_database(),
  "13" = update_countries(),
  "14" = {
    message("Countries: ", paste(names(COUNTRY_MODULES), collapse = " "))
    message("Cities: ", paste(vapply(CITY_MODULES, function(m) paste0(m$code, " (", m$name_en, ")"), ""), collapse = "  "))
    codes <- toupper(unlist(strsplit(trimws(readline("Codes separated by spaces (e.g. ES GBE FI, or MAD BCN): ")), "[ ,;]+")))
    codes <- codes[nzchar(codes)]
    if (length(codes) == 0) message("Nothing chosen.")
    else if (length(bad <- setdiff(codes, c(names(COUNTRY_MODULES), vapply(CITY_MODULES, function(m) m$code, ""))))) message("Unknown codes: ", paste(bad, collapse = " "))
    else {
      again <- tolower(trimws(readline("Download again the files already on disk, even if recent? (y/N): ")))
      update_countries(only = codes, refresh = again %in% c("y", "yes", "s", "si"))
    }
  },
  "15" = check_countries(),
  "16" = {   # a separate R process: the tests load their own copy of the code and leave nothing in this session
    rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    status <- system2(rscript, c("--no-save", "--no-restore", "tests/run_tests.R"))
    if (!identical(status, 0L)) message("The test process ended with status ", status, " (see the lines above; tests/run_tests.R can also be sourced in this console)")
  },
  "17" = list_old_copies(),
  "18" = madrid_register_history(),
  "19" = git_prepare(),
  message("Nothing chosen.")
)
