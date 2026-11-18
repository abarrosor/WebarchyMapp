# App texts in 32 languages -------------------------------------------------------
# All texts live in app/translations.csv: one row per key, one column per
# language (en is the reference; an empty cell falls back to English).
# Keys ending in "@barcelona" override the base key in that city (the source is
# different: incidents handled by the City Police instead of calls).
# %s / %d are replaced by values (sprintf); a language that needs another order
# numbers them (%2$s ... %1$s), all of them or none. Texts with <b> are HTML.
# No text glues a number to a noun by itself: "n_noun" (number + noun) and
# "noun_rate" (noun + "per 1,000 inhabitants") let every language choose the order
# and the punctuation ("5 calls", "skambučiai: 5", "1.000 kişi başına çağrı").
#
# The selector offers at least one official language of every country of the app.
# Names of call types, offence types and arrest motives come from the reference
# CSVs (reference/*.csv): the source's own language and English; the other
# languages show the English name, except the tables made only of the Eurostat
# (ICCS) categories, which app/category_names.csv gives in every language.
# Neighbourhood, district, municipality and region names are always the
# official ones.

# In the order of the selector: Latin script by native name, then Greek, Cyrillic and the East Asian scripts
LANGUAGES <- c(ca = "Català", cs = "Čeština", da = "Dansk", de = "Deutsch", et = "Eesti", en = "English", es = "Español",
               fr = "Français", gl = "Galego", hr = "Hrvatski", is = "Íslenska", it = "Italiano", lv = "Latviešu",
               lt = "Lietuvių", hu = "Magyar", nl = "Nederlands", no = "Norsk bokmål", pl = "Polski", pt = "Português",
               ro = "Română", sq = "Shqip", sk = "Slovenčina", sl = "Slovenščina", fi = "Suomi", sv = "Svenska", tr = "Türkçe",
               el = "Ελληνικά", bg = "Български", sr = "Српски", zh = "中文", ja = "日本語", ko = "한국어")
DEFAULT_LANGUAGE <- "en"

TRANSLATIONS <- local({
  path <- if (file.exists("translations.csv")) "translations.csv" else "app/translations.csv"
  df <- utils::read.csv(path, encoding = "UTF-8", check.names = FALSE, colClasses = "character", na.strings = character())
  out <- lapply(names(LANGUAGES), function(l) stats::setNames(df[[l]], df$key))
  names(out) <- names(LANGUAGES)
  out
})

# Text of a key; with ... applies sprintf. `city`: its own version first ("key@city"), then the version of
# the city's kind of measure ("key@offences", "key@complaints"...), then the base key.
tr <- function(key, lang, ..., city = NULL, kind = NULL) {
  pick <- function(k) {
    x <- TRANSLATIONS[[lang]][k]
    if (is.na(x) || !nzchar(x)) x <- TRANSLATIONS$en[k]
    if (is.na(x) || !nzchar(x)) NULL else unname(x)
  }
  x <- (if (!is.null(city)) pick(paste0(key, "@", city))) %||% (if (!is.null(kind) && nzchar(kind)) pick(paste0(key, "@", kind))) %||% pick(key) %||% key
  if (...length() > 0) sprintf(x, ...) else x
}

# Month names of a language (short or long), as a vector of 12
month_names <- function(lang, long = FALSE) strsplit(tr(if (long) "months_long" else "months", lang), "|", fixed = TRUE)[[1]]

# First letter in capitals / in lower case, and a whole text in lower case, with the Turkish pair of i's (i-İ, ı-I)
upper_first <- function(x, lang = "en") {
  f <- substr(x, 1, 1); f <- if (identical(lang, "tr")) chartr("iı", "İI", f) else f
  paste0(toupper(f), substring(x, 2))
}
lower_first <- function(x, lang = "en") {
  f <- substr(x, 1, 1); f <- if (identical(lang, "tr")) chartr("İI", "iı", f) else f
  paste0(tolower(f), substring(x, 2))
}
lower_all <- function(x, lang = "en") tolower(if (identical(lang, "tr")) chartr("İI", "iı", x) else x)

# Names of the categories that are the same in every country (the Eurostat / ICCS ones): app/category_names.csv,
# one row per English name, one column per language. Any other name comes back unchanged.
CATEGORY_NAMES <- local({
  path <- if (file.exists("category_names.csv")) "category_names.csv" else "app/category_names.csv"
  if (!file.exists(path)) return(list())
  df <- utils::read.csv(path, encoding = "UTF-8", check.names = FALSE, colClasses = "character", na.strings = character())
  out <- lapply(intersect(names(LANGUAGES), names(df)), function(l) stats::setNames(df[[l]], df$en))
  names(out) <- intersect(names(LANGUAGES), names(df))
  out
})
tr_category <- function(name_en, lang) {
  tab <- CATEGORY_NAMES[[lang]]
  if (is.null(tab) || identical(lang, "en")) return(name_en)
  x <- unname(tab[as.character(name_en)])
  ifelse(is.na(x) | !nzchar(x), name_en, x)
}
# A whole list of names: translated only when every name is a shared one, so that a list is never half in one language
# and half in another (the national names of a source exist in its own language and in English only)
categories_known <- function(names_en, lang) {
  tab <- CATEGORY_NAMES[[lang]]
  !is.null(tab) && !identical(lang, "en") && length(names_en) > 0 && all(as.character(names_en) %in% names(tab))
}
tr_categories <- function(names_en, lang) if (categories_known(names_en, lang)) tr_category(names_en, lang) else names_en

# Notes on specific call types (shown with an info icon). Key: the type as it comes
# from the City Council (column call_type of the CSV) -> translation key.
TYPE_NOTES <- c("OTROS DELITOS..." = "type_note_other_offences")
type_note <- function(call_type, lang) {
  k <- TYPE_NOTES[call_type]
  if (is.na(k)) NA_character_ else tr(unname(k), lang)
}

# Language requested in the URL (?lang=fr); English by default
lang_from_url <- function(query_string) {
  l <- shiny::parseQueryString(query_string %||% "")$lang
  if (!is.null(l) && l %in% names(LANGUAGES)) l else DEFAULT_LANGUAGE
}

# View requested in the URL (?view=ES / FR / madrid / barcelona / about); the first
# available by default. "spain" (the old name of the Spain view) still works.
view_from_url <- function(query_string, available) {
  v <- shiny::parseQueryString(query_string %||% "")$view
  if (identical(v, "spain")) v <- "ES"
  if (!is.null(v) && toupper(v) %in% available) v <- toupper(v)
  if (!is.null(v) && v %in% available) v else available[1]
}
