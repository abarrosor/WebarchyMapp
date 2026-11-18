# The editable tables: translations, reference files, module registrations ------------------------------------
use_test_project()

translations <- function() utils::read.csv(file.path(getOption("webarchy.project_root", "."), "app", "translations.csv"), encoding = "UTF-8", check.names = FALSE, colClasses = "character", na.strings = character())

APP_LANGUAGES <- c("en", "es", "ca", "gl", "fr", "it", "de", "pt", "zh", "ja", "ko", "nl", "da", "sv", "no", "fi", "is", "pl", "cs", "sk",
                   "hu", "ro", "bg", "el", "hr", "sl", "sr", "sq", "et", "lv", "lt", "tr")
# Text of a key in a language as the app shows it (an empty cell falls back to English)
cell <- function(tr, key, l) { i <- match(key, tr$key); x <- tr[[l]][i]; if (is.na(x) || !nzchar(x)) tr$en[i] else x }

test_that("translations.csv has the 32 languages, unique keys and an English text for every key", {
  tr <- translations()
  expect_equal(names(tr)[1], "key")
  expect_setequal(names(tr)[-1], APP_LANGUAGES)
  expect_equal(anyDuplicated(tr$key), 0)
  expect_true(all(nzchar(trimws(tr$en))))
  i18n <- paste(readLines(file.path(getOption("webarchy.project_root", "."), "app", "i18n.R"), encoding = "UTF-8", warn = FALSE), collapse = " ")
  block <- sub("DEFAULT_LANGUAGE.*$", "", sub("^.*LANGUAGES <- c\\(", "", i18n))
  listed <- sub(" = \"", "", regmatches(block, gregexpr("\\b[a-z]{2} = \"", block))[[1]], fixed = TRUE)
  expect_setequal(listed, APP_LANGUAGES)                                 # the selector offers every column of the table
  menu <- paste(readLines(file.path(getOption("webarchy.project_root", "."), "WEBARCHYMAPP.R"), encoding = "UTF-8", warn = FALSE), collapse = " ")
  block <- sub("utils::menu\\(paste0\\(langs.*$", "", sub("^.*langs <- c\\(", "", menu))
  offered <- sub(" = \"", "", regmatches(block, gregexpr("\\b[a-z]{2} = \"", block))[[1]], fixed = TRUE)
  expect_setequal(offered, setdiff(APP_LANGUAGES, "en"))                 # option 3 of the menu opens the app in any of them
})

test_that("every city or kind variant (key@x) has a base key", {
  tr <- translations()
  variants <- tr$key[grepl("@", tr$key)]
  base <- sub("@.*$", "", variants)
  expect_true(all(base %in% tr$key), info = paste("variants without base key:", paste(variants[!base %in% tr$key], collapse = ", ")))
})

test_that("placeholders of every text agree with English in type and position, numbered or not, and sprintf accepts them", {
  tr <- translations()
  specs <- function(x) { m <- regmatches(x, gregexpr("%(?:[0-9]+\\$)?[sd]", x, perl = TRUE))[[1]]
    list(type = sub("^%(?:[0-9]+\\$)?", "", m, perl = TRUE), pos = suppressWarnings(as.integer(sub("^%([0-9]+)\\$.*$", "\\1", ifelse(grepl("\\$", m), m, NA))))) }
  literal <- function(x) lengths(regmatches(x, gregexpr("%%", x, fixed = TRUE)))
  bad <- character()
  for (i in seq_len(nrow(tr))) {
    en <- specs(tr$en[i]); n <- length(en$type)
    for (l in APP_LANGUAGES[-1]) {
      x <- tr[[l]][i]; if (!nzchar(x)) next
      sp <- specs(x); where <- paste0(l, ":", tr$key[i])
      if (n == 0) { if (length(sp$type) > 0) bad <- c(bad, paste(where, "has placeholders, English has none")); next }
      numbered <- !is.na(sp$pos)
      if (any(numbered) && !all(numbered)) { bad <- c(bad, paste(where, "mixes numbered and plain placeholders")); next }
      if (all(numbered) && length(numbered) > 0) {
        if (!setequal(sp$pos, seq_len(n))) { bad <- c(bad, paste(where, "numbers", paste(sp$pos, collapse = ","), "instead of 1 to", n)); next }
        if (!all(sp$type == en$type[sp$pos])) { bad <- c(bad, paste(where, "changes the type of a numbered placeholder")); next }
      } else if (!identical(sp$type, en$type)) { bad <- c(bad, paste(where, "placeholders", paste(sp$type, collapse = ""), "instead of", paste(en$type, collapse = ""))); next }
      if (literal(x) != literal(tr$en[i])) { bad <- c(bad, paste(where, "another number of %%")); next }
      args <- lapply(en$type, function(t) if (t == "d") 7L else "X")
      out <- tryCatch(do.call(sprintf, c(list(x), args)), error = function(e) NA_character_, warning = function(w) NA_character_)
      if (is.na(out)) bad <- c(bad, paste(where, "sprintf fails"))
    }
  }
  expect_length(bad, 0)
  if (length(bad)) print(bad)                                       # which language and key, when it fails
})

test_that("HTML tags and list separators of every text are those of the English text", {
  tr <- translations()
  tags <- function(x) vapply(regmatches(x, gregexpr("</?[a-z]+>|&[a-z]+;", x)), function(t) paste(sort(t), collapse = " "), "")
  bars <- function(x) lengths(regmatches(x, gregexpr("|", x, fixed = TRUE)))
  bad <- character()
  for (l in APP_LANGUAGES[-1]) { filled <- nzchar(tr[[l]])
    d <- tags(tr$en[filled]) != tags(tr[[l]][filled]) | bars(tr$en[filled]) != bars(tr[[l]][filled])
    if (any(d)) bad <- c(bad, paste0(l, ":", tr$key[filled][d])) }
  expect_length(bad, 0)
  if (length(bad)) print(bad)
  for (l in APP_LANGUAGES) for (k in c("months", "months_long")) expect_length(strsplit(cell(tr, k, l), "|", fixed = TRUE)[[1]], 12)
})

test_that("texts that name another text of the app use its exact words in every language", {
  tr <- translations()
  pairs <- list(c("desc_public_safety@berlin", "block_coexistence"), c("about_more", "nav_about"), c("about_why_bcn", "country_ES"))
  bad <- character()
  for (l in APP_LANGUAGES) for (p in pairs) if (!grepl(cell(tr, p[2], l), cell(tr, p[1], l), fixed = TRUE)) bad <- c(bad, paste0(l, ":", p[1], " does not contain ", p[2]))
  expect_length(bad, 0)
  if (length(bad)) print(bad)
})

test_that("the templates that join a number or a rate to a noun work in every language", {
  tr <- translations()
  for (l in APP_LANGUAGES) {
    a <- sprintf(cell(tr, "n_noun", l), "1234", "NOUN"); b <- sprintf(cell(tr, "noun_rate", l), "NOUN", "RATE")
    expect_true(grepl("1234", a, fixed = TRUE) && grepl("NOUN", a, fixed = TRUE), info = l)
    expect_true(grepl("NOUN", b, fixed = TRUE) && grepl("RATE", b, fixed = TRUE), info = l)
  }
})

test_that("a language never uses one word for two things the screen shows side by side", {
  tr <- translations()
  groups <- list(c("shade_rate", "sp_level", "panel_col_indicator", "shade_number", "sp_col_type", "category", "period"),   # rate, level, indicator, number, type...
                 c("block_public_safety", "block_coexistence"),
                 c("k_calls", "k_calls@barcelona", "k_calls@lima", "k_calls@offences", "k_calls@cases"))                    # the unit of each kind of source
  bad <- character()
  for (l in APP_LANGUAGES) for (g in groups) { g <- g[g %in% tr$key]; w <- tolower(vapply(g, function(k) cell(tr, k, l), ""))
    if (anyDuplicated(w)) bad <- c(bad, paste0(l, ": ", paste(g[w %in% w[duplicated(w)]], collapse = " = "))) }
  expect_length(bad, 0)
  if (length(bad)) print(bad)
})

test_that("the two handles have one name in each language: in the help of the period slider and under the period bar of the series", {
  tr <- translations()
  word <- c(en = "handles", es = "selectores", ca = "selectors", gl = "selectores", fr = "poignées", it = "maniglie", de = "Griffe", pt = "alças", zh = "滑块", ja = "ハンドル", ko = "핸들",
            nl = "grepen", da = "håndtag", sv = "handtagen", no = "håndtakene", fi = "kahvaa", is = "handföngin", pl = "uchwyty", cs = "úchytů", sk = "úchytov", hu = "fogantyú", ro = "mânere",
            bg = "манипулатора", el = "λαβές", hr = "ručice", sl = "ročici", sr = "клизача", sq = "dorezat", et = "käepidet", lv = "rokturus", lt = "rankenėles", tr = "tutamac")
  expect_setequal(names(word), APP_LANGUAGES)
  bad <- character()
  for (l in APP_LANGUAGES) for (k in c("sp_period_help", "ts_nav_help")) if (!grepl(word[[l]], cell(tr, k, l), fixed = TRUE)) bad <- c(bad, paste0(l, ":", k))
  expect_length(bad, 0)
  if (length(bad)) print(bad)
})

test_that("category_names.csv gives every Eurostat (ICCS) category name in every language", {
  f <- file.path(getOption("webarchy.project_root", "."), "app", "category_names.csv")
  expect_true(file.exists(f))
  cn <- utils::read.csv(f, encoding = "UTF-8", check.names = FALSE, colClasses = "character", na.strings = character())
  expect_setequal(names(cn), APP_LANGUAGES)
  expect_equal(anyDuplicated(cn$en), 0)
  expect_true(all(vapply(cn, function(col) all(nzchar(trimws(col))), TRUE)))
  for (l in APP_LANGUAGES) expect_equal(anyDuplicated(cn[[l]]), 0, info = l)          # two categories never share a name
  # The names of the countries read from Eurostat only (their tables have no local name)
  eurostat_only <- Filter(function(m) m$code != "XT", COUNTRY_MODULES)
  for (m in eurostat_only) { t <- read_reference(paste0(tolower(m$code), "_offence_types.csv"))
    same <- t$name_local == t$name_en
    if (all(same) && !identical(m$lang, "en")) expect_true(all(t$name_en %in% cn$en), info = m$code) }
})

test_that("the levels of one country never share a name in a language (Regions and Provinces must stay apart)", {
  tr <- translations()
  key_of <- function(x) paste0("lvl_", gsub("[^a-z]", "", tolower(sub("\\s*\\(.*$", "", x))))
  bad <- character()
  for (m in COUNTRY_MODULES) {
    if (m$code == "XT" || length(m$level_labels) < 2) next
    en <- vapply(m$level_labels, function(x) x[1], "")
    for (l in setdiff(APP_LANGUAGES, c("en", m$lang))) {
      shown <- vapply(en, function(x) { k <- key_of(x); kc <- paste0(k, "@", tolower(m$code))             # the country's own word first
        if (kc %in% tr$key) cell(tr, kc, l) else if (k %in% tr$key) cell(tr, k, l) else x }, "")
      if (anyDuplicated(tolower(shown))) bad <- c(bad, paste0(m$code, " in ", l, ": ", paste(shown, collapse = " / ")))
    }
  }
  expect_length(bad, 0)
  if (length(bad)) print(bad)
})

test_that("the reference tables of the city modules have the columns the framework reads and known blocks", {
  files <- list.files(CONFIG$dir_ref, "_event_types\\.csv$", full.names = TRUE)
  expect_gt(length(files), 5)
  for (f in files) {
    t <- read_reference(basename(f), bool_cols = "sensitive")
    expect_true(all(c("source_code", "block", "sensitive", "name_local", "name_en") %in% names(t)), info = basename(f))
    expect_true(all(nzchar(t$block) & !is.na(t$block)), info = basename(f))            # a block the app does not show is loaded but hidden
    expect_true(any(t$block %in% MARTS$blocks), info = basename(f))                       # something to show by area
    expect_equal(anyDuplicated(t$source_code), 0, info = basename(f))
  }
})

test_that("every reference CSV parses cleanly (an unquoted comma shifts the columns of a row silently)", {
  # One check for the whole folder, not one per table: the total of the suite must be the same on every computer,
  # and the folder also holds tables a module writes when it loads (co_territories.csv) or that were left over.
  files <- list.files(CONFIG$dir_ref, "\\.csv$", full.names = TRUE)
  expect_gt(length(files), 50)                                      # the folder was found and has its tables
  bad <- character()
  for (f in files) {
    d <- suppressWarnings(readr::read_csv(f, col_types = readr::cols(.default = "c"), progress = FALSE, show_col_types = FALSE))
    n <- nrow(readr::problems(d)); if (n > 0) bad <- c(bad, sprintf("%s: %d row(s) with a problem", basename(f), n))
  }
  if (length(bad)) print(bad)                                       # which table, when it fails
  expect_length(bad, 0)
})

test_that("every country module has its offence-types table with the columns load_country_types() needs", {
  for (m in COUNTRY_MODULES) {
    if (m$code == "XT") next
    f <- file.path(CONFIG$dir_ref, paste0(tolower(m$code), "_offence_types.csv"))
    expect_true(file.exists(f), info = m$code)
    if (file.exists(f)) { t <- read_reference(basename(f)); expect_true(all(c("source_code", "offence_type_id", "name_local", "name_en", "included_in") %in% names(t)), info = m$code) }
  }
})

test_that("module registrations are consistent: unique codes, keys and ids; city nodes hang from their country", {
  cities <- Filter(function(m) m$key != "xt", CITY_MODULES)
  codes <- vapply(cities, function(m) m$code, ""); keys <- vapply(cities, function(m) m$key, ""); ids <- vapply(cities, function(m) m$id, "")
  expect_equal(anyDuplicated(codes), 0); expect_equal(anyDuplicated(keys), 0); expect_equal(anyDuplicated(ids), 0)
  expect_equal(anyDuplicated(vapply(COUNTRY_MODULES, function(m) m$code, "")), 0)
  expect_equal(anyDuplicated(c(codes, names(COUNTRY_MODULES))), 0)                    # a city code never clashes with a country code
  for (m in cities) {
    expect_true(startsWith(m$id, paste0(m$country, "-")), info = m$key)
    expect_true(m$country %in% names(COUNTRY_MODULES), info = m$key)
    expect_true(is.function(m$download) && is.function(m$territory) && is.function(m$ingest) && is.function(m$population), info = m$key)
    expect_true(is.null(m$time_unit) || m$time_unit %in% c("month", "year"), info = m$key)
  }
})
