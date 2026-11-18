# The code on GitHub: what is published and what never is (P-034, P-069) ------------------------------------------
# R/96_git_prepare.R. The addresses, folders and keys of these tests are put together from pieces, so that this file
# passes the very check it tests.
use_test_project()

test_that("what would be published: only the folders of the code, never a data file, never an address, a folder of a user, a drive path or a key", {
  at <- "@"; user_dir <- paste0("C:", "/Users/", "someone/Documents"); drive <- paste0("E:", "/Projects/x/y.R")
  txt <- list("R/mail.R" = paste0("# write to someone", at, "gmail.com"), "R/userdir.R" = sprintf("setwd(\"%s\")", user_dir), "R/drive.R" = sprintf("source(\"%s\")", drive),
              "R/homedir.R" = paste0("x <- \"/", "home", "/someone/project\""), "R/macdir.R" = paste0("x <- '/", "Users", "/someone/project'"),
              "R/password.R" = paste0("pass", "word = \"", strrep("x9", 6), "\""), "R/token.R" = paste0("gh", "p_", strrep("A1b2C3", 6)), "R/key.R" = paste0("-----BEGIN RSA ", "PRIVATE KEY-----"),
              "R/apikey.R" = paste0("api", "_key <- '", strrep("k7", 8), "'"),
              "R/fine.R" = c("key <- \"stats_note@lima\"   # a key of the translations and its variant", "path <- \"reference/x.csv\"", paste0("git <- \"C:", "/Program Files/Git/cmd/git.exe\""),
                             paste0("r <- \"C:", "/PROGRA~1/R/R-46~1.1\""), paste0("who <- \"12345+someone", at, "users.noreply.github.com\""), "url <- \"https://host.example/:u:/r/personal/x/file.zip\"",
                             "site <- \"https://host.example/home/page\"", "ratio <- a:b / 2", "token <- input$token", "label <- \"password\""))
  read <- function(f) if (is.null(txt[[f]])) "x <- 1" else txt[[f]]; size <- function(f) if (f == "reference/huge.csv") 5 * 1024^2 else 1000
  files <- c(names(txt), "README.md", "LICENSE", ".gitignore", "app/app.R", "tests/testthat/test-01-helpers.R", "scripts/1_update_all.R", "reference/sources.csv",
             "notes.txt", ".Renviron", "data/raw/madrid/file.csv", "data/webarchymapp.duckdb", "reports/quality.csv", "data_seed/survey.csv", "Claude/00_management/decision-register.md",
             "_old_spanish/app.R", "R/model.rds", "app/map.gpkg", "reference/figures.xlsx", "reference/huge.csv", "R/archive.zip")
  p <- publish_check(files, read = read, size = size)
  why <- function(f) p$problem[p$path == f]
  for (f in c("R/fine.R", "README.md", "LICENSE", ".gitignore", "app/app.R", "tests/testthat/test-01-helpers.R", "scripts/1_update_all.R", "reference/sources.csv")) expect_length(why(f), 0)
  expect_match(why("R/mail.R"), "line 1 holds an e-mail address", fixed = TRUE)
  expect_true(any(grepl("the folder of a user", why("R/userdir.R")))); expect_true(any(grepl("the folder of a user", why("R/homedir.R")))); expect_true(any(grepl("the folder of a user", why("R/macdir.R"))))
  expect_match(why("R/drive.R"), "a path on a drive", fixed = TRUE)
  expect_match(why("R/password.R"), "a key or a password written out", fixed = TRUE); expect_match(why("R/apikey.R"), "a key or a password written out", fixed = TRUE)
  expect_match(why("R/token.R"), "an access token", fixed = TRUE); expect_match(why("R/key.R"), "a private key", fixed = TRUE)
  expect_match(why("notes.txt"), "not one of the files the repository holds at its root", fixed = TRUE); expect_true(any(grepl("a file of a session or of a computer", why(".Renviron"))))
  for (f in c("data/raw/madrid/file.csv", "data/webarchymapp.duckdb", "reports/quality.csv", "data_seed/survey.csv", "Claude/00_management/decision-register.md", "_old_spanish/app.R"))
    expect_true(any(grepl("is not published", why(f), fixed = TRUE)), info = f)
  for (f in c("R/model.rds", "app/map.gpkg", "reference/figures.xlsx", "R/archive.zip", "data/webarchymapp.duckdb")) expect_true(any(grepl("is data or a document, not code", why(f), fixed = TRUE)), info = f)
  expect_match(why("reference/huge.csv"), "5.0 MB: larger than anything", fixed = TRUE)
  # What was found is never repeated in full: a report is read by others too
  expect_false(any(grepl(paste0("someone", at, "gmail.com"), p$found, fixed = TRUE))); expect_true(all(grepl("\\*{3,}", p$found[nzchar(p$found)])))
  expect_equal(mask_email(paste0("someone", at, "gmail.com")), paste0("so***", at, "gmail.com")); expect_true(is_noreply(paste0("1+x", at, "users.noreply.github.com"))); expect_false(is_noreply(paste0("x", at, "gmail.com")))
  # A file that cannot be read is a problem, never a pass
  expect_match(publish_check("R/gone.R", read = function(f) stop("no"), size = function(f) 10)$problem, "cannot be read as text", fixed = TRUE)
  expect_match(publish_check("R/gone.R", read = function(f) "x", size = function(f) stop("no"))$problem, "cannot be read", fixed = TRUE)
  expect_equal(nrow(publish_check(character())), 0)
})

test_that("the project's own files pass the check, and .gitignore keeps the data out", {
  root <- getOption("webarchy.project_root"); skip_if(is.null(root) || !file.exists(file.path(root, "WebarchyMapp.Rproj")), "the project root is not known to this run")
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  files <- publish_files("."); expect_true(length(files) > 150)
  expect_true(all(c("LICENSE", "README.md", "GUIDE.md", "WEBARCHYMAPP.R", ".gitignore", ".gitattributes", "R/96_git_prepare.R", "app/app.R", "app/translations.csv", "tests/run_tests.R") %in% files))
  expect_false(any(grepl("^(data|reports|data_seed|_old_spanish|Claude)/", files)))
  p <- publish_check(files)
  expect_equal(paste(p$path, p$problem), character())                                           # a failure here names the file and the line
  ignore <- trimws(readLines(".gitignore", warn = FALSE)); ignore <- ignore[nzchar(ignore) & !startsWith(ignore, "#")]
  expect_true(all(c("data/", "reports/", "data_seed/", "_old_spanish/", ".Rproj.user/", ".Rhistory", ".RData", ".Renviron", "*.duckdb", "*.gpkg", "*.xlsx", "*.zip") %in% ignore))
  expect_false(any(c("R/", "app/", "reference/", "scripts/", "tests/", "*.R", "*.csv", "*.md") %in% ignore))   # ...and never the code or its tables
  attrs <- readLines(".gitattributes", warn = FALSE); expect_true("* text=auto eol=lf" %in% attrs); expect_true("*.Rproj text eol=crlf" %in% attrs)
  expect_true(any(grepl("Copyright (c) 2026", readLines("LICENSE", warn = FALSE), fixed = TRUE)))
  expect_true(any(grepl("\"19\" = git_prepare()", readLines("WEBARCHYMAPP.R", warn = FALSE), fixed = TRUE)))
})

test_that("a git command that Windows refuses for a moment is tried again; any other failure is returned at once", {
  # What git said on the founder's first run of option 19 (18 November 2026): 40 new files written in a second, the 41st
  # refused, because another program had just opened it. Git for Windows gives up after about 70 ms where it can ask nobody.
  refused <- c("error: unable to write file .git/objects/cd/ad33f04ba4a65a435cc2c418a4c46d2040a9db: Permission denied", "error: R/88_pl.R: failed to insert into database",
               "error: unable to index file 'R/88_pl.R'", "fatal: updating files failed")
  expect_true(git_busy(refused)); expect_true(git_busy("fatal: Unable to create 'x/.git/index.lock': File exists.")); expect_true(git_busy("error: unable to unlink old 'a.R': Permission denied"))
  expect_false(git_busy("fatal: not a git repository (or any of the parent directories): .git")); expect_false(git_busy("fatal: pathspec 'x' did not match any files")); expect_false(git_busy(character()))
  waits <- numeric(); wait <- function(s) waits <<- c(waits, s)
  # refused twice, then done: three tries, two waits, and the result says how many it took
  n <- 0; r <- git_again(function() { n <<- n + 1; if (n < 3) list(out = refused, ok = FALSE) else list(out = character(), ok = TRUE) }, wait = wait)
  expect_true(r$ok); expect_equal(r$tries, 3); expect_equal(n, 3); expect_equal(waits, c(1.5, 1.5))
  # done at once: one try, no wait
  waits <- numeric(); n <- 0; r <- git_again(function() { n <<- n + 1; list(out = character(), ok = TRUE) }, wait = wait); expect_equal(c(r$tries, n, length(waits)), c(1, 1, 0))
  # another kind of failure: not tried again
  n <- 0; r <- git_again(function() { n <<- n + 1; list(out = "fatal: not a git repository", ok = FALSE) }, wait = wait); expect_false(r$ok); expect_equal(c(r$tries, n, length(waits)), c(1, 1, 0))
  # refused every time: it stops, after the number of tries given and one wait fewer, and hands back what git said last
  n <- 0; r <- git_again(function() { n <<- n + 1; list(out = refused, ok = FALSE) }, tries = 4, pause = 0.5, wait = wait); expect_false(r$ok); expect_equal(c(r$tries, n), c(4, 4)); expect_equal(waits, rep(0.5, 3)); expect_identical(r$out, refused)
  body <- paste(deparse(git_prepare), collapse = " "); expect_true(grepl("git_again(function() run(\"add\", \"-A\"))", body, fixed = TRUE)); expect_true(grepl("needed %d tries", body, fixed = TRUE))
})

test_that("git add and the check together, on a repository made for the test: a clean set is staged, a file with an address unstages everything, nothing is committed", {
  git <- git_find(); skip_if(is.na(git), "git was not found on this computer: the rule itself is tested above, without git")
  repo <- file.path(tempdir(), paste0("wm-git-", as.integer(Sys.time()))); dir.create(file.path(repo, "R"), recursive = TRUE); dir.create(file.path(repo, "data")); on.exit(unlink(repo, recursive = TRUE, force = TRUE), add = TRUE)
  g <- function(...) suppressWarnings(system2(git, c("-C", shQuote(repo), ...), stdout = TRUE, stderr = TRUE))
  g("init", "-q"); g("config", "user.email", paste0("someone", "@", "gmail.com")); g("config", "user.name", "Someone"); g("config", "core.autocrlf", "false")
  writeLines(c("data/", "reports/", "*.duckdb"), file.path(repo, ".gitignore")); writeLines("x <- 1", file.path(repo, "R", "one.R")); writeLines("MIT", file.path(repo, "LICENSE"))
  writeLines("figures", file.path(repo, "data", "raw.csv")); writeLines("not a database", file.path(repo, "data", "db.duckdb"))
  was <- CONFIG$dir_reports; on.exit(CONFIG$dir_reports <<- was, add = TRUE)
  said <- character(); say <- function(...) said <<- c(said, paste0(...))
  # 1. A clean set: staged, a report and a draft of the message written in reports/ (which git ignores), no commit made
  out <- git_prepare(repo, say = say)
  expect_equal(nrow(out), 0); expect_setequal(g("diff", "--cached", "--name-only"), c(".gitignore", "LICENSE", "R/one.R"))      # the data stayed out: .gitignore
  expect_true(any(grepl("READY: 3 file(s) staged for the next commit (3 new, 0 modified, 0 deleted, 0 other)", said, fixed = TRUE)))
  expect_true(any(grepl("signed with: Someone <so***@gmail.com>", said, fixed = TRUE))); expect_true(any(grepl("this address would be PUBLIC on GitHub", said, fixed = TRUE)))   # who would sign it, the address in part
  expect_true(any(grepl("Nothing was committed or pushed", said, fixed = TRUE))); expect_true(any(grepl("git remote add origin", said, fixed = TRUE)))
  expect_length(list.files(file.path(repo, "reports"), "^git_prepare_.*\\.txt$"), 1); expect_true(file.exists(file.path(repo, "reports", "commit_message.txt")))
  expect_false(any(grepl("^[0-9a-f]{7,}", g("log", "--oneline"))))                               # no commit exists
  # 2. A file with an address in it: named with its line, and everything is unstaged again; the file is not changed
  bad <- file.path(repo, "R", "two.R"); writeLines(c("y <- 2", paste0("# ask someone", "@", "gmail.com")), bad); before <- readLines(bad)
  said <- character(); out <- git_prepare(repo, say = say)
  expect_equal(out$path, "R/two.R"); expect_match(out$problem, "line 2 holds an e-mail address", fixed = TRUE)
  expect_length(g("diff", "--cached", "--name-only"), 0); expect_identical(readLines(bad), before); expect_true(any(grepl("NOT READY: 1 problem(s) in 1 file(s)", said, fixed = TRUE)))
  # 3. Put right, and after a first commit: the changes since then, the commits of the history and who signed them
  writeLines("y <- 2", bad); said <- character(); expect_equal(nrow(git_prepare(repo, say = say)), 0)
  g("commit", "-q", "-m", "first"); writeLines("x <- 3", file.path(repo, "R", "one.R")); unlink(bad); Sys.sleep(1.1)
  said <- character(); expect_equal(nrow(git_prepare(repo, say = say)), 0)
  expect_true(any(grepl("READY: 2 file(s) staged for the next commit (0 new, 1 modified, 1 deleted, 0 other)", said, fixed = TRUE)))
  expect_true(any(grepl("The 1 commit(s) already in the history are signed with: so***@gmail.com", said, fixed = TRUE))); expect_true(any(grepl("pushing the history publishes these addresses too", said, fixed = TRUE)))
  expect_length(g("rev-list", "HEAD"), 1)                                                        # still the one commit made by the test itself
  # 4. Who signs when the repository itself names nobody: the user's own settings, which git looks for in HOME. R on
  #    Windows calls the Documents folder HOME, so git run from R found none there and option 19 said that no e-mail was
  #    set (the founder's run of 18 November 2026). git_prepare() gives git the folder it is told as HOME while it runs,
  #    and puts back what was there.
  g("config", "--unset", "user.email"); g("config", "--unset", "user.name")
  mine <- file.path(tempdir(), paste0("wm-home-", as.integer(Sys.time()))); nobody <- paste0(mine, "-empty"); dir.create(mine); dir.create(nobody); on.exit(unlink(c(mine, nobody), recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(c("[user]", "\tname = Somebody Else", paste0("\temail = else", "@", "gmail.com")), file.path(mine, ".gitconfig"))
  before <- Sys.getenv("HOME", unset = NA); xdg <- Sys.getenv("XDG_CONFIG_HOME", unset = NA); Sys.unsetenv("XDG_CONFIG_HOME"); on.exit(if (!is.na(xdg)) Sys.setenv(XDG_CONFIG_HOME = xdg), add = TRUE)
  said <- character(); expect_equal(nrow(git_prepare(repo, say = say, home = mine)), 0)
  expect_true(any(grepl("signed with: Somebody Else <el***@gmail.com>", said, fixed = TRUE))); expect_identical(Sys.getenv("HOME", unset = NA), before)
  said <- character(); expect_equal(nrow(git_prepare(repo, say = say, home = nobody)), 0)
  expect_true(any(grepl("signed with: (no e-mail set in git)", said, fixed = TRUE))); expect_true(any(grepl("git will not commit until a name and an address are set", said, fixed = TRUE)))
  expect_identical(Sys.getenv("HOME", unset = NA), before)
  # Which folder that is: the user's profile on Windows, where it exists; nothing to change anywhere else
  expect_identical(git_home("windows", mine), mine); expect_true(is.na(git_home("windows", ""))); expect_true(is.na(git_home("windows", file.path(mine, "not-there")))); expect_true(is.na(git_home("unix", mine)))
  expect_identical(formals(git_prepare)$home, quote(git_home()))
  # 5. A folder that is not the root of a repository: nothing is done
  said <- character(); expect_null(git_prepare(file.path(repo, "R"), say = say)); expect_true(any(grepl("not the root of a git repository", said, fixed = TRUE)))
})
