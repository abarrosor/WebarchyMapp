# App texts in 11 languages -------------------------------------------------------
# All texts live in app/translations.csv: one row per key, one column per
# language (en is the reference; an empty cell falls back to English).
# Keys ending in "@barcelona" override the base key in that city (the source is
# different: incidents handled by the City Police instead of calls).
# %s / %d are replaced by values (sprintf). Texts with <b> are HTML.
#
# Names of call types, offence types and arrest motives come from the reference
# CSVs (reference/*.csv, columns name_es / name_en): they exist in Spanish and
# English only, so the other languages show the English name.
# Neighbourhood, district, municipality and region names are always the
# official ones (Spanish / Catalan).

LANGUAGES <- c(en = "English", es = "Español", ca = "Català", gl = "Galego", fr = "Français", it = "Italiano",
               de = "Deutsch", pt = "Português", zh = "中文", ja = "日本語", ko = "한국어")
DEFAULT_LANGUAGE <- "en"

TRANSLATIONS <- local({
  path <- if (file.exists("translations.csv")) "translations.csv" else "app/translations.csv"
  df <- utils::read.csv(path, encoding = "UTF-8", check.names = FALSE, colClasses = "character", na.strings = character())
  out <- lapply(names(LANGUAGES), function(l) stats::setNames(df[[l]], df$key))
  names(out) <- names(LANGUAGES)
  out
})

# Text of a key; with ... applies sprintf. `city`: its own version first ("key@city").
tr <- function(key, lang, ..., city = NULL) {
  pick <- function(k) {
    x <- TRANSLATIONS[[lang]][k]
    if (is.na(x) || !nzchar(x)) x <- TRANSLATIONS$en[k]
    if (is.na(x) || !nzchar(x)) NULL else unname(x)
  }
  x <- (if (!is.null(city)) pick(paste0(key, "@", city))) %||% pick(key) %||% key
  if (...length() > 0) sprintf(x, ...) else x
}

# Month names of a language (short or long), as a vector of 12
month_names <- function(lang, long = FALSE) strsplit(tr(if (long) "months_long" else "months", lang), "|", fixed = TRUE)[[1]]

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

# View requested in the URL (?view=ES / FR / madrid / barcelona / countries / about); the first
# available by default. "spain" (the old name of the Spain view) still works.
view_from_url <- function(query_string, available) {
  v <- shiny::parseQueryString(query_string %||% "")$view
  if (identical(v, "spain")) v <- "ES"
  if (!is.null(v) && toupper(v) %in% available) v <- toupper(v)
  if (!is.null(v) && v %in% available) v else available[1]
}
