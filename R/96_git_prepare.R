# The code on GitHub: what is published, what never is, and a check before every commit ---------------------------
# (decisions P-034 and P-069)
#
# The repository holds the code and the tables that describe the sources: R/, app/, reference/, scripts/, tests/ and
# the files at the root listed below. It never holds data: nothing downloaded (data/), nothing built from it (the
# database, the reports), nor the table transcribed by hand in data_seed/. .gitignore says so to git; this file says
# it a second time, in R, and looks inside every file that would be published for what must not leave this computer:
# an e-mail address, the folder of a user, a drive path, a key or a password.
#
# git_prepare() (menu option 19) does what "git add" does and then checks the result:
#   1. stages every change (git add -A: new, modified and deleted files, as .gitignore allows);
#   2. checks every file the repository would hold; if one fails, everything is unstaged again (the files themselves
#      are never touched) and the reasons are listed;
#   3. says what is staged, who would sign the commit, and writes a report and a draft of the message in reports/.
# It NEVER commits and NEVER pushes: both stay in the founder's hands (GUIDE.md, "Publishing the code on GitHub").
# The same check runs on the project's files in the automated tests (option 16), with or without git.

PUBLISH <- list(
  root_files = c(".gitignore", ".gitattributes", "LICENSE", "README.md", "GUIDE.md", "WEBARCHYMAPP.R", "WebarchyMapp.Rproj"),
  folders    = c("R", "app", "reference", "scripts", "tests"),
  # data by its kind, wherever it is: a database, a map layer, an R object, a workbook, an archive, a document
  never_ext  = c("duckdb", "wal", "db", "sqlite", "gpkg", "shp", "rds", "rdata", "rda", "xlsx", "xls", "ods", "parquet", "feather", "zip", "7z", "gz", "tar", "pdf", "docx"),
  never_name = c(".Renviron", ".Rprofile", ".Rhistory", ".RData", ".env"),
  max_bytes  = 2 * 1024^2,                                  # the largest file of the project is the table of translations (1.4 MB)
  text_ext   = c("r", "md", "csv", "txt", "rproj", "json", "yml", "yaml", "js", "css", "html", ""))

# What must not be inside a published file. Each pattern is written so that this file does not match itself.
publish_patterns <- function() c(
  "an e-mail address"                  = "[A-Za-z0-9._%+-]+@[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)*\\.[A-Za-z]{2,}",
  "the folder of a user on a computer" = paste0("([A-Za-z]:[/\\\\]+", "Users", "[/\\\\]+|(^|[\\s\"'(=])/(", "home", "|", "Users", ")/)[A-Za-z0-9._-]+"),
  "a path on a drive of a computer"    = "(?<![A-Za-z0-9:/])[A-Za-z]:[/\\\\](?![/\\\\])[^\\s\"'<>|]*",       # not the "/:u:/" inside a web address
  "a private key"                      = "-----BEGIN [A-Z ]*PRIVATE KEY-----",
  "an access token"                    = "(gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{30,}|AKIA[0-9A-Z]{16}|xox[baprs]-[A-Za-z0-9-]{10,})",
  "a key or a password written out"    = "(?i)(api[_-]?key|secret|passw(or)?d|token)\\s*(=|<-|:)\\s*[\"'][^\"'\\s]{8,}[\"']")
# Text that looks like one of the above and is not: where a program is installed (every computer has it), an address
# that names no person, and the examples of the documentation
publish_allowed <- function() c(
  "(?i)^[A-Za-z]:[/\\\\]+(Program Files( \\(x86\\))?|PROGRA~[0-9]|Windows|ProgramData)([/\\\\]|$)",
  "(?i)@users\\.noreply\\.github\\.com$", "(?i)^noreply@", "(?i)@example\\.(org|com|net)$")

# The check itself. files: paths from the root of the project, with "/". read(path) gives the lines of a file and
# size(path) its size in bytes (both can be replaced, so that the rule is tested without files on disk).
# Returns a data frame of problems (path, problem, found): no rows means the set can be published.
publish_check <- function(files, read = function(p) readLines(p, warn = FALSE, encoding = "UTF-8"), size = function(p) file.size(p)) {
  out <- list(); add <- function(path, problem, found = "") out[[length(out) + 1]] <<- data.frame(path = path, problem = problem, found = found, stringsAsFactors = FALSE)
  pat <- publish_patterns(); ok <- publish_allowed()
  for (f in unique(gsub("\\\\", "/", files))) {
    parts <- strsplit(f, "/", fixed = TRUE)[[1]]; name <- parts[length(parts)]; ext <- tolower(if (grepl(".", name, fixed = TRUE)) sub("^.*\\.", "", name) else "")
    if (length(parts) == 1) { if (!f %in% PUBLISH$root_files) add(f, "not one of the files the repository holds at its root") }
    else if (!parts[1] %in% PUBLISH$folders) add(f, sprintf("the folder %s/ is not published", parts[1]))
    if (name %in% PUBLISH$never_name) add(f, "a file of a session or of a computer, never of the project")
    if (ext %in% PUBLISH$never_ext) add(f, sprintf("a .%s file is data or a document, not code", ext))
    bytes <- tryCatch(size(f), error = function(e) NA_real_)
    if (is.na(bytes)) { add(f, "the file cannot be read"); next }
    if (bytes > PUBLISH$max_bytes) add(f, sprintf("%.1f MB: larger than anything the project's code holds", bytes / 1024^2))
    if (!ext %in% PUBLISH$text_ext || ext %in% PUBLISH$never_ext) next
    lines <- tryCatch(read(f), error = function(e) NULL); if (is.null(lines)) { add(f, "the file cannot be read as text"); next }
    lines <- suppressWarnings(iconv(lines, "UTF-8", "UTF-8", sub = "?")); lines[is.na(lines)] <- ""
    for (what in names(pat)) {
      hit <- grep(pat[[what]], lines, perl = TRUE); if (!length(hit)) next
      for (i in hit) {
        m <- gregexpr(pat[[what]], lines[i], perl = TRUE)[[1]]; found <- regmatches(lines[i], list(m))[[1]]
        rest <- substring(lines[i], m + nchar(found) - nchar(trimws(found, "left")), m + 80)        # what follows too: the folder of the programs of Windows has a space in its name
        fine <- vapply(seq_along(found), function(k) any(vapply(ok, function(a) grepl(a, trimws(found[k]), perl = TRUE) || grepl(a, rest[k], perl = TRUE), TRUE)), TRUE)
        if (any(!fine)) add(f, sprintf("line %d holds %s", i, what), mask_found(found[!fine][1]))
      }
    }
  }
  if (length(out)) do.call(rbind, out) else data.frame(path = character(), problem = character(), found = character(), stringsAsFactors = FALSE)
}
# What was found is shown in part only: a report must not repeat an address or a key in full
mask_found <- function(x) { x <- trimws(x); n <- nchar(x); if (n <= 6) return(strrep("*", n)); paste0(substr(x, 1, 3), strrep("*", min(n - 5, 12)), substr(x, n - 1, n)) }
mask_email <- function(x) ifelse(grepl("@", x, fixed = TRUE), paste0(substr(x, 1, 2), "***@", sub("^.*@", "", x)), x)
is_noreply <- function(x) grepl("@users\\.noreply\\.github\\.com$", x, ignore.case = TRUE)

# The files of the project that would be published, read from the disk (no git needed): the root files and the
# published folders, less what .gitignore leaves out by its kind
publish_files <- function(root = ".") {
  inside <- unlist(lapply(PUBLISH$folders, function(d) if (dir.exists(file.path(root, d))) file.path(d, list.files(file.path(root, d), recursive = TRUE, all.files = TRUE, no.. = TRUE))))
  inside <- inside[!tolower(sub("^.*\\.", "", basename(inside))) %in% PUBLISH$never_ext & !basename(inside) %in% PUBLISH$never_name]
  c(PUBLISH$root_files[file.exists(file.path(root, PUBLISH$root_files))], inside)
}

# Where git is: on the search path, or where Git for Windows installs it (RStudio finds it there without the path)
git_find <- function() {
  g <- unname(Sys.which("git")); if (nzchar(g)) return(g)
  base <- Sys.getenv(c("ProgramFiles", "ProgramW6432", "ProgramFiles(x86)", "LOCALAPPDATA"), unset = "")
  for (p in file.path(base[nzchar(base)], rep(c("Git/cmd/git.exe", "Programs/Git/cmd/git.exe"), each = sum(nzchar(base))))) if (file.exists(p)) return(normalizePath(p, winslash = "/"))
  NA_character_
}

# Windows refuses a file for a moment when another program has just opened it: an antivirus that reads every new
# file, a search index, a sync client, the watcher of an editor. Git for Windows tries again for about 70 ms and,
# where it can ask nobody (git gui asks: "should I try again?"), gives up: "unable to write file ...: Permission
# denied". Nothing is wrong and nothing is lost: what was written stays written, and the next try goes on from there.
# It happened on the founder's first run of option 19 (18 November 2026): 40 new files written in one second, the
# 41st refused. So a command that fails that way is tried again, a few times, a moment apart; any other failure is
# returned at once.
git_busy <- function(out) any(grepl("Permission denied|unable to write file|failed to insert into database|unable to index file|unable to unlink|index\\.lock|Unable to create .*\\.lock|Resource temporarily unavailable", out, ignore.case = TRUE))
git_again <- function(once, tries = 8, pause = 1.5, wait = Sys.sleep) {
  for (i in seq_len(tries)) {
    r <- once(); r$tries <- i
    if (isTRUE(r$ok) || !git_busy(r$out)) return(r)
    if (i < tries) wait(pause)
  }
  r
}

# R on Windows calls the user's Documents folder HOME, and git looks in HOME for the user's own settings, the name and
# the address that sign a commit among them. Run from R, git did not find them: on the founder's run of 18 November
# 2026 option 19 said "(no e-mail set in git)" although every commit of his is signed. RStudio gives its own git tools
# the user's profile as HOME for the same reason, and so does git_prepare(), for as long as it runs: what it reports is
# then what a commit made from git gui or from a terminal carries. Nothing is changed outside this R session.
git_home <- function(os = .Platform$OS.type, profile = Sys.getenv("USERPROFILE")) if (identical(os, "windows") && nzchar(profile) && dir.exists(profile)) profile else NA_character_

git_prepare <- function(root = ".", say = message, home = git_home()) {
  git <- git_find()
  if (is.na(git)) { say("Git was not found on this computer (neither on the search path nor where Git for Windows installs it). Nothing was done."); return(invisible(NULL)) }
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  if (!is.na(home)) { was <- Sys.getenv("HOME", unset = NA); Sys.setenv(HOME = home); on.exit(if (is.na(was)) Sys.unsetenv("HOME") else Sys.setenv(HOME = was), add = TRUE) }
  run <- function(...) { out <- suppressWarnings(system2(git, c("-c", "core.quotepath=false", ...), stdout = TRUE, stderr = TRUE)); list(out = as.character(out), ok = is.null(attr(out, "status")) || identical(attr(out, "status"), 0L)) }
  top <- run("rev-parse", "--show-toplevel")
  if (!top$ok || !file.exists(".git")) {                                                   # .git is in the root of a repository and nowhere else
    say("This folder is not the root of a git repository (the folder of WebarchyMapp.Rproj must be the one that holds .git). Nothing was done."); return(invisible(NULL)) }
  born <- run("rev-parse", "--verify", "-q", "HEAD")$ok                                   # FALSE in a repository without a commit yet
  unstage <- function() git_again(function() if (born) run("reset", "-q") else run("rm", "-r", "-q", "--cached", "--ignore-unmatch", "."))
  staged <- git_again(function() run("add", "-A"))
  if (!staged$ok) {
    say("git add failed and nothing was staged", if (staged$tries > 1) sprintf(" (tried %d times)", staged$tries) else "", ":\n  ", paste(staged$out, collapse = "\n  "),
        if (git_busy(staged$out)) paste0("\n\nAnother program keeps a file of the repository open (an antivirus, a sync client, git gui or another git tool).",
                                         "\nNo file was changed. Close git gui and any other git tool, wait a moment and run this option again.") else "")
    return(invisible(NULL))
  }
  retried <- if (staged$tries > 1) sprintf("git add needed %d tries: another program held a new file open for a moment each time (usual on Windows: an antivirus, a search index, a sync client). Nothing was lost.", staged$tries)
  files <- run("ls-files")$out; files <- files[nzchar(files)]
  problems <- publish_check(files)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S"); dir.create(CONFIG$dir_reports, showWarnings = FALSE, recursive = TRUE)
  report <- file.path(CONFIG$dir_reports, paste0("git_prepare_", stamp, ".txt"))
  if (nrow(problems)) {
    unstage()
    lines <- c(sprintf("WebarchyMapp - check before a commit, %s", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
               sprintf("NOT READY: %d problem(s) in %d file(s). Everything was unstaged again; no file was changed.", nrow(problems), length(unique(problems$path))), retried, "",
               sprintf("  %s: %s%s", problems$path, problems$problem, ifelse(nzchar(problems$found), paste0("  [", problems$found, "]"), "")), "",
               "A file that must stay out goes into .gitignore; a line that names an address, a folder or a key is rewritten.", "Then run this option again.")
    writeLines(lines, report, useBytes = TRUE); say(paste(lines, collapse = "\n")); say("\nReport: ", report)
    return(invisible(problems))
  }
  change <- run("diff", "--cached", "--name-status")$out; kind <- substr(change, 1, 1)
  branch <- if (born) run("rev-parse", "--abbrev-ref", "HEAD")$out[1] else run("symbolic-ref", "--short", "HEAD")$out[1]
  remote <- run("remote", "-v")$out; remote <- unique(sub("\\s+\\((fetch|push)\\)$", "", remote[nzchar(remote)]))
  email <- run("config", "user.email")$out[1]; if (is.na(email)) email <- ""
  name <- run("config", "user.name")$out[1]; if (is.na(name)) name <- ""
  authors <- if (born) unique(run("log", "--format=%ae")$out) else character()
  sizes <- file.size(files); big <- order(-sizes)[seq_len(min(3, length(files)))]
  folder <- ifelse(grepl("/", files, fixed = TRUE), paste0(sub("/.*$", "", files), "/"), "(root)")
  lines <- c(sprintf("WebarchyMapp - check before a commit, %s", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
    sprintf("READY: %d file(s) staged for the next commit (%d new, %d modified, %d deleted, %d other).", length(change), sum(kind == "A"), sum(kind == "M"), sum(kind == "D"), sum(!kind %in% c("A", "M", "D"))),
    sprintf("The repository would hold %d files, %.1f MB: %s.", length(files), sum(sizes, na.rm = TRUE) / 1024^2, paste(sprintf("%s %d", names(table(folder)), as.integer(table(folder))), collapse = ", ")),
    sprintf("The largest: %s.", paste(sprintf("%s (%.0f KB)", files[big], sizes[big] / 1024), collapse = ", ")),
    "Every one of them was checked: only published folders, no data file, no e-mail address, no folder of a user, no drive path, no key.", retried, "",
    sprintf("Branch: %s. Remote: %s.", branch, if (length(remote)) paste(remote, collapse = "; ") else "none yet (the repository has never been pushed)"),
    sprintf("The next commit would be signed with: %s%s", if (nzchar(email)) paste0(if (nzchar(name)) paste0(name, " ") else "", "<", mask_email(email), ">") else "(no e-mail set in git)",
            if (!nzchar(email)) "\n  -> git will not commit until a name and an address are set: in git gui, Edit > Options; or see GUIDE.md, section 14."
            else if (!is_noreply(email)) "\n  -> this address would be PUBLIC on GitHub. GitHub gives every account a private one (Settings > Emails): see GUIDE.md." else ""),
    if (length(authors)) sprintf("The %d commit(s) already in the history are signed with: %s%s", length(run("rev-list", "HEAD")$out), paste(mask_email(authors), collapse = ", "),
            if (any(!is_noreply(authors))) "\n  -> pushing the history publishes these addresses too. The three ways to deal with it are in GUIDE.md." else ""))
  writeLines(lines, report, useBytes = TRUE)
  # A draft of the message of the commit, unless one was written after the last commit (then it is for this one)
  msg <- file.path(CONFIG$dir_reports, "commit_message.txt")
  last <- if (born) suppressWarnings(as.numeric(run("log", "-1", "--format=%ct")$out[1])) else 0
  if (!file.exists(msg) || isTRUE(as.numeric(file.mtime(msg)) < last)) {
    writeLines(c(sprintf("Update of %s", format(Sys.Date(), "%Y-%m-%d")), "",
                 sprintf("%d files: %d new, %d modified, %d deleted.", length(change), sum(kind == "A"), sum(kind == "M"), sum(kind == "D")),
                 sprintf("- %s %d", names(table(sub("/.*$", "/", sub("^[A-Z][0-9]*\\s+", "", change)))), as.integer(table(sub("/.*$", "/", sub("^[A-Z][0-9]*\\s+", "", change)))))), msg, useBytes = TRUE)
    lines <- c(lines, "", sprintf("A draft of the message was written to %s: edit it before the commit.", msg))
  } else lines <- c(lines, "", sprintf("The message prepared in %s is kept (it is newer than the last commit).", msg))
  say(paste(lines, collapse = "\n"))
  say("\nNothing was committed or pushed. Next, in a terminal opened in this folder (RStudio: Tools > Terminal > New Terminal), or in git gui:\n",
      "  git status\n  git commit -F reports/commit_message.txt\n",
      if (!length(remote)) "The first time only, after creating the empty repository on github.com (GUIDE.md):\n  git remote add origin https://github.com/YOUR-USER/YOUR-REPOSITORY.git\n",
      sprintf("  git push -u origin %s\n", branch), "Report: ", report)
  invisible(problems)
}
