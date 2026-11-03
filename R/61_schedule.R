# Automatic update with the Windows Task Scheduler ---------------------------------
# Creates (or removes) a task that runs scripts/scheduled_update.R with Rscript,
# without opening RStudio. No password needed: the task runs under your user
# while your Windows session is open. If the computer is off or asleep at that
# time, it runs as soon as it is available again.
#
# Everything is done with PowerShell (Register-ScheduledTask), which ships with
# Windows. Every run leaves a log in reports/update_YYYYMMDD_hhmmss.log.

TASK_NAME <- "WebarchyMapp - data update"
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
    "$settings = New-ScheduledTaskSettingsSet -StartWhenAvailable -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -ExecutionTimeLimit (New-TimeSpan -Hours 2)",
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

# State of the task (if it exists) and of the last logged run
schedule_status <- function() {
  info <- if (is_windows()) tryCatch(powershell(c(
    sprintf("$t = Get-ScheduledTask -TaskName %s -ErrorAction SilentlyContinue", ps_quote(TASK_NAME)),
    "if ($t) { $i = $t | Get-ScheduledTaskInfo; ",
    "  Write-Output ('state=' + $t.State); ",
    "  Write-Output ('next=' + $i.NextRunTime.ToString('yyyy-MM-dd HH:mm')); ",
    "  Write-Output ('last=' + $i.LastRunTime.ToString('yyyy-MM-dd HH:mm')) } else { Write-Output 'state=none' }"
  )), error = function(e) "state=unknown") else "state=not-windows"
  kv <- stats::setNames(sub("^[^=]*=", "", info), sub("=.*$", "", info))
  list(scheduled = !kv[["state"]] %in% c("none", "unknown", "not-windows"),
       next_run = kv["next"], last_run = kv["last"], log = last_update_log())
}

last_update_log <- function() {
  f <- newest_first(list.files(CONFIG$dir_reports, "^update_.*\\.log$", full.names = TRUE))
  if (length(f) == 0) return(NULL)
  lines <- readLines(f[1], warn = FALSE, encoding = "UTF-8")
  res <- grep("^RESULT:", lines, value = TRUE)
  list(file = f[1], date = file.mtime(f[1]), result = if (length(res)) sub("^RESULT:\\s*", "", res[length(res)]) else "unfinished")
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
  if (!s$scheduled) message("[!!] No automatic update scheduled (option 9 of the menu).") else
    message("[ok] Automatic update scheduled. Next: ", s$next_run, " - Last: ", s$last_run)
  if (is.null(s$log)) message("  No logs yet.") else
    message("  Last log: ", basename(s$log$file), " (", format(s$log$date, "%Y-%m-%d %H:%M"), "): ", s$log$result)
  invisible(s)
}
