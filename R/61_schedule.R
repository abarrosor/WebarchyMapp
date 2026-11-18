# Automatic update with the Windows Task Scheduler ---------------------------------
# Creates (or removes) a task that runs scripts/scheduled_update.R with Rscript,
# without opening RStudio. No password needed: the task runs under your user
# while your Windows session is open. If the computer is off or asleep at that
# time, it runs as soon as it is available again.
#
# Everything is done with PowerShell (Register-ScheduledTask), which ships with
# Windows. Every run leaves a log in reports/update_YYYYMMDD_hhmmss.log.

TASK_NAME <- "WebarchyMapp - data update"
# Windows stops a run after this many hours. Two until 16 November 2026: a first full run, or one after a rebuild of
# the database, can take longer, and a run stopped by the limit ends without a word. A task keeps the limit it was
# created with: option 9 creates it again with this one (the code never changes an existing task by itself).
SCHEDULE_HOURS <- 6L
WEEKDAYS <- c(monday = "Monday", tuesday = "Tuesday", wednesday = "Wednesday", thursday = "Thursday",
              friday = "Friday", saturday = "Saturday", sunday = "Sunday")

is_windows <- function() identical(.Platform$OS.type, "windows")

# Run a PowerShell script (written to a temporary file to avoid quoting trouble)
powershell <- function(lines) {
  if (!is_windows()) stop("Automatic updates only work on Windows.", call. = FALSE)
  ps1 <- tempfile(fileext = ".ps1")
  on.exit(unlink(ps1), add = TRUE)
  writeLines(c("$ErrorActionPreference = 'Stop'", lines), ps1, useBytes = TRUE)
  out <- suppressWarnings(system2("powershell.exe", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(ps1)),
                                  stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status") %||% 0L
  if (!identical(as.integer(status), 0L)) stop("PowerShell returned an error:\n", paste(out, collapse = "\n"), call. = FALSE)
  out
}

ps_quote <- function(x) paste0("'", gsub("'", "''", x, fixed = TRUE), "'")

# day: one (or several) of names(WEEKDAYS); time: "HH:MM"
schedule_update <- function(day = "sunday", time = "21:00") {
  day <- tolower(day)
  if (!all(day %in% names(WEEKDAYS))) stop("Day not recognised: ", toString(day), ". Use: ", toString(names(WEEKDAYS)))
  if (!grepl("^[0-2][0-9]:[0-5][0-9]$", time)) stop("Invalid time (format HH:MM): ", time)
  rscript <- normalizePath(file.path(R.home("bin"), "Rscript.exe"), winslash = "\\", mustWork = TRUE)
  script  <- normalizePath("scripts/scheduled_update.R", winslash = "\\", mustWork = TRUE)
  folder  <- normalizePath(".", winslash = "\\", mustWork = TRUE)
  powershell(c(
    sprintf("$action = New-ScheduledTaskAction -Execute %s -Argument %s -WorkingDirectory %s",
            ps_quote(rscript), ps_quote(paste0('"', script, '"')), ps_quote(folder)),
    sprintf("$trigger = New-ScheduledTaskTrigger -Weekly -DaysOfWeek %s -At %s", paste(unname(WEEKDAYS[day]), collapse = ","), ps_quote(time)),
    sprintf("$settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -ExecutionTimeLimit (New-TimeSpan -Hours %d)", SCHEDULE_HOURS),
    sprintf("Register-ScheduledTask -TaskName %s -Action $action -Trigger $trigger -Settings $settings -Description %s -Force | Out-Null",
            ps_quote(TASK_NAME), ps_quote("Downloads and loads new data for the WebarchyMapp project (R)"))
  ))
  message(sprintf("[ok] Automatic update scheduled: every %s at %s.", paste(day, collapse = " and "), time))
  message("  Runs under your user while the Windows session is open; if the computer was off, when it is switched on.")
  message("  Log of every run: reports/update_*.log")
  invisible(schedule_status())
}

unschedule_update <- function() {
  powershell(sprintf("Unregister-ScheduledTask -TaskName %s -Confirm:$false -ErrorAction SilentlyContinue", ps_quote(TASK_NAME)))
  message("[ok] Automatic update removed.")
  invisible(TRUE)
}

# State of the task (if it exists) and of the last logged run. Every property is asked on its own line and inside
# its own try: a task without a next run (disabled, say) must not hide the rest.
schedule_status <- function() {
  info <- if (is_windows()) tryCatch(powershell(c(
    sprintf("$t = Get-ScheduledTask -TaskName %s -ErrorAction SilentlyContinue", ps_quote(TASK_NAME)),
    "if ($t) {",
    "  Write-Output ('state=' + $t.State)",
    "  $i = $null",
    "  try { $i = $t | Get-ScheduledTaskInfo } catch { }",
    "  try { Write-Output ('next=' + $i.NextRunTime.ToString('yyyy-MM-dd HH:mm')) } catch { }",
    "  try { Write-Output ('last=' + $i.LastRunTime.ToString('yyyy-MM-dd HH:mm')) } catch { }",
    "  try { Write-Output ('result=' + $i.LastTaskResult) } catch { }",
    "  try { Write-Output ('missed=' + $i.NumberOfMissedRuns) } catch { }",
    "  try { Write-Output ('exe=' + $t.Actions[0].Execute) } catch { }",
    "  try { Write-Output ('limit=' + $t.Settings.ExecutionTimeLimit) } catch { }",
    "} else { Write-Output 'state=none' }"
  )), error = function(e) c("state=unknown", paste0("problem=", gsub("\\s+", " ", conditionMessage(e))))) else "state=not-windows"
  info <- info[grepl("=", info, fixed = TRUE)]
  kv <- stats::setNames(sub("^[^=]*=", "", info), sub("=.*$", "", info))
  get <- function(k) if (k %in% names(kv) && nzchar(kv[[k]])) kv[[k]] else NA_character_
  state <- get("state") %||% "unknown"
  list(scheduled = !state %in% c("none", "unknown", "not-windows"), state = state,
       next_run = get("next"), last_run = get("last"), result = get("result"), missed = get("missed"), exe = get("exe"), limit = get("limit"),
       problem = get("problem"), log = last_update_log())
}

# The newest log of a scheduled run: list(file, date, result). `skip`: a log being written right now (the scheduled
# run reads the status at its end, and its own log has no RESULT line yet)
last_update_log <- function(skip = getOption("webarchy.scheduled_log")) {
  f <- newest_first(list.files(CONFIG$dir_reports, "^update_.*\\.log$", full.names = TRUE))
  if (!is.null(skip)) f <- f[basename(f) != basename(skip)]
  if (length(f) == 0) return(NULL)
  lines <- readLines(f[1], warn = FALSE, encoding = "UTF-8")
  res <- grep("^RESULT:", lines, value = TRUE)
  list(file = f[1], date = file.mtime(f[1]), result = if (length(res)) sub("^RESULT:\\s*", "", res[length(res)]) else "unfinished")
}

# Hours of a Windows time limit as the task stores it ("PT2H", "PT72H", "P3D"; "PT0S" = no limit). NA when not understood.
schedule_limit_hours <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA_real_)
  if (grepl("^PT0S$", x)) return(Inf)
  m <- regmatches(x, regexec("^P(?:([0-9]+)D)?(?:T(?:([0-9]+)H)?(?:([0-9]+)M)?(?:([0-9]+)S)?)?$", x, perl = TRUE))[[1]]
  if (length(m) == 0) return(NA_real_)
  v <- suppressWarnings(as.numeric(m[-1])); v[is.na(v)] <- 0
  v[1] * 24 + v[2] + v[3] / 60 + v[4] / 3600
}

# The path of a program as Windows itself writes it. R.home() gives the short names of a folder with spaces
# ("C:/PROGRA~1/R/R-46~1.1") and the task holds the long ones ("C:\Program Files\R\R-4.6.1"): one file must not look
# like two. A path that does not exist (an R since removed) comes back as it was written.
long_path <- function(p) suppressWarnings(normalizePath(gsub('"', "", trimws(p)), winslash = "/", mustWork = FALSE))

# What deserves a look in the state of the task (F6): lines starting with [!!], or none. Kept apart from PowerShell
# so that it can be tested: `s` is the list of schedule_status(), `rscript` the Rscript of the R in use, `long` what
# gives a path its full form (long_path(); a test passes its own, since only Windows has short names).
schedule_notes <- function(s, rscript = file.path(R.home("bin"), "Rscript.exe"), now = Sys.time(), long = long_path) {
  out <- character()
  if (!isTRUE(s$scheduled)) return(out)
  shape <- function(p) tolower(gsub("[/\\\\]+", "/", long(p)))            # the same file whatever the case, the slashes and the quotes
  rscript <- long(rscript)
  if (!is.na(s$exe %||% NA) && !identical(shape(s$exe), shape(rscript)))
    out <- c(out, paste0("[!!] The task runs ", s$exe, ", and the R in use is ", gsub("/", "\\\\", rscript), ".\n",
                         "     After an R upgrade the task keeps the old R: it does not see the packages you update, and it stops the day that R is removed.\n",
                         "     Option 9 creates the task again with the R in use."))
  if (identical(s$state, "Disabled")) out <- c(out, "[!!] The task exists but is disabled in Windows: it does not run (Task Scheduler > enable it, or option 9).")
  hours <- schedule_limit_hours(s$limit %||% NA)
  if (!is.na(hours) && hours < SCHEDULE_HOURS)
    out <- c(out, sprintf("[!!] Windows stops this task after %s hour(s); tasks created now get %d. A long run (the first one, or after a rebuild) can be cut without a word.\n     Option 9 creates the task again with the new limit.", format(hours), SCHEDULE_HOURS))
  code <- suppressWarnings(as.numeric(s$result %||% NA))
  if (!is.na(code) && !code %in% c(0, 267009, 267011))
    out <- c(out, sprintf("[!!] Windows reports that the last run ended with code %s (0 means finished): %s", format(code, scientific = FALSE),
                          if (code == 267014) "the run was stopped (time limit, or by hand)" else "R did not start or ended with an error; see the last log"))
  if (is.null(s$log)) {
    if (!is.na(code) && code == 0) out <- c(out, "[!!] The task has run but left no log in reports/: R did not reach the update (see the path of Rscript above).")
  } else {
    age <- as.numeric(difftime(now, s$log$date, units = "days"))
    if (age > 9) out <- c(out, sprintf("[!!] The last log is %.0f days old although the task is weekly: the update is not running (computer off at that time and not started later, or the task fails before R starts).", age))
    running <- !is.na(code) && code == 267009
    if (identical(s$log$result, "unfinished") && !running) out <- c(out, "[!!] The last run did not reach its end (no RESULT line in its log): stopped by the time limit, the computer was switched off, or R closed.")
    else if (grepl("^error", s$log$result)) out <- c(out, "[!!] The last run ended with an error (the line above): the steps after it were not done.")
  }
  out
}

# --- For the WEBARCHYMAPP.R menu ----------------------------------------------------------
schedule_update_menu <- function() {
  day <- readline("Which day of the week? (monday, tuesday, ..., sunday) [sunday]: ")
  if (!nzchar(trimws(day))) day <- "sunday"
  time <- readline("At what time? (HH:MM) [21:00]: ")
  if (!nzchar(trimws(time))) time <- "21:00"
  schedule_update(trimws(day), trimws(time))
}

show_schedule_status <- function() {
  s <- schedule_status()
  code <- suppressWarnings(as.numeric(s$result %||% NA))
  never <- is.na(s$last_run %||% NA) || grepl("^(1999|1601)", s$last_run) || (!is.na(code) && code == 267011)
  if (identical(s$state, "unknown")) message("[!!] Windows did not say whether the automatic update is scheduled (the question to the Task Scheduler failed", if (!is.na(s$problem %||% NA)) paste0(": ", substr(s$problem, 1, 300)) else "", "). The task itself is not touched; send this line.")
  else if (!s$scheduled) message("[!!] No automatic update scheduled (option 9 of the menu).") else
    message("[ok] Automatic update scheduled. Next: ", s$next_run %||% "not set", " - Last: ", if (never) "it has not run yet" else s$last_run,
            if (!is.na(code) && code == 267009) " (running now)" else "")
  if (is.null(s$log)) message("  No logs yet.") else
    message("  Last log: ", basename(s$log$file), " (", format(s$log$date, "%Y-%m-%d %H:%M"), "): ", s$log$result)
  hours <- schedule_limit_hours(s$limit %||% NA)
  if (s$scheduled && !is.na(hours)) message("  Time limit of a run: ", if (is.infinite(hours)) "none" else paste(format(hours), "hour(s)"))
  for (n in schedule_notes(s)) message(n)
  invisible(s)
}
