# F2 · Madrid Municipal Police activity statistics --------------------------------------
# datos.madrid.es dataset 212616-0-policia-estadisticas (CC BY 4.0).
# One Excel file per month since January 2014, 16 sheets; several have one row
# per district (21 + "no district assigned" + total). The structure has been
# stable since 2014; only titles changed slightly.
#
# What it measures: police ACTIVITY (reports, arrests, inspections), not crime.
# More patrols or campaigns mean more actions without more incidents. The app
# therefore presents it as "Municipal Police activity in the district", never
# as a crime rate or a ranking.
#
# The month is not inside the Excel file: it is in the resource description on
# the portal, so the download also saves an index (resources.csv).

ACTIVITY_MONTHS <- c(enero = 1, febrero = 2, marzo = 3, abril = 4, mayo = 5, junio = 6, julio = 7,
                     agosto = 8, septiembre = 9, setiembre = 9, octubre = 10, noviembre = 11, diciembre = 12)

# District sheets that are loaded: sheet-name pattern -> indicators (column order after the district column)
ACTIVITY_SHEETS <- list(
  list(pattern = "^SEGURIDAD",        block = "losc_reports",   indicators = c("losc_people", "losc_property", "losc_weapons", "losc_drug_possession", "losc_drug_use")),
  list(pattern = "DETENID.*DISTRITO", block = "arrests",        indicators = "arrested_investigated"),
  list(pattern = "^ACCIDENTES",       block = "accidents",      indicators = c("accidents_with_casualties", "accidents_without_casualties")),
  list(pattern = "^CONSUMO ALCOHOL",  block = "coexistence",    indicators = c("drinking_in_public_adults", "drinking_in_public_minors")),
  list(pattern = "^LOCALES",          block = "coexistence",    indicators = c("venue_inspections", "venue_reports")),
  list(pattern = "^VENTA AMBULANTE",  block = "coexistence",    indicators = c("street_vending_counterfeit", "street_vending_food"))
)

madrid_activity_download <- function() {
  src <- CONFIG$sources$madrid_activity
  dir.create(src$dir_raw, recursive = TRUE, showWarnings = FALSE)
  res <- ckan_resources(src$ckan_id)
  is_table <- res$format %in% c("xls", "xlsx", "csv") | grepl("\\.(xlsx?|csv)$", res$url, ignore.case = TRUE)
  is_pdf <- res$format == "pdf" | grepl("\\.pdf$", res$url, ignore.case = TRUE)
  res <- res[is_table | is_pdf, ]
  res$file <- ckan_file_name(res)
  readr::write_csv(res[, c("file", "name", "description", "format", "url", "created", "modified")],
                   file.path(src$dir_raw, "resources.csv"))        # file -> description (holds the month)
  dest <- file.path(src$dir_raw, res$file)
  new <- which(!file.exists(dest))
  for (i in new) {
    ok <- tryCatch({ download_safely(res$url[i], dest[i]); TRUE },
                   error = function(e) { message("x ", res$file[i], ": ", conditionMessage(e)); FALSE })
    if (ok) message("+ ", res$file[i])
    Sys.sleep(0.5)
  }
  message(length(new), " new activity file(s); ", nrow(res) - length(new), " already downloaded.")
  invisible(dest[new])
}

# "enero 2014", "Enero de 2025", "enero-2026"... -> "2014-01-01"
activity_month_from_text <- function(x) {
  x <- tolower(chartr("ÁÉÍÓÚ", "AEIOU", x %||% ""))
  m <- regmatches(x, regexec("(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|setiembre|octubre|noviembre|diciembre)[^0-9]{0,15}(20[0-9]{2})", x))
  vapply(m, function(v) if (length(v) == 3) sprintf("%s-%02d-01", v[3], ACTIVITY_MONTHS[[v[2]]]) else NA_character_, "")
}

activity_index <- function() {
  src <- CONFIG$sources$madrid_activity
  path <- file.path(src$dir_raw, "resources.csv")
  idx <- if (file.exists(path)) readr::read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE) else
    data.frame(file = character(), name = character(), description = character())
  files <- list.files(src$dir_raw, pattern = "\\.xlsx?$")
  i <- match(files, idx$file)
  month <- activity_month_from_text(paste(idx$description[i], idx$name[i]))
  month[is.na(month)] <- activity_month_from_text(files[is.na(month)])
  data.frame(file = files, month = as.Date(month), stringsAsFactors = FALSE)
}

# Read one Excel: rows by district (ACTIVITY_SHEETS) plus, with district_code NA
# and block 'arrest_motives', city-wide arrests by motive (sheet "... X MOTIVOS")
madrid_activity_read <- function(path, ref_districts, ref_motives) {
  sheets <- readxl::excel_sheets(path)
  out <- list(); notes <- character()
  motive_sheet <- grep("MOTIVO", toupper(sheets), value = TRUE)
  if (length(motive_sheet) > 0) {
    d <- as.data.frame(readxl::read_excel(path, sheet = motive_sheet[1], col_names = FALSE, .name_repair = "minimal"))
    txt <- normalize_text(as.character(d[[1]])); v <- suppressWarnings(as.numeric(as.character(d[[2]])))
    id <- ref_motives$motive_id[match(txt, normalize_text(ref_motives$source_text))]
    unknown <- unique(txt[is.na(id) & !is.na(v) & !is.na(txt) & txt != "MOTIVOS"])
    if (length(unknown) > 0) notes <- c(notes, paste0("unmapped motives: ", paste(unknown, collapse = " | ")))
    ok <- !is.na(id) & !is.na(v)
    if (any(ok)) {
      agg <- stats::aggregate(v[ok], by = list(motive = id[ok]), FUN = sum)   # a motive may be repeated
      out[[length(out) + 1]] <- data.frame(district_code = NA_character_, district_txt = NA_character_, block = "arrest_motives",
                                           indicator = agg$motive, value = agg$x, stringsAsFactors = FALSE)
    }
  }
  for (h in ACTIVITY_SHEETS) {
    sheet <- grep(h$pattern, toupper(sheets), value = TRUE)
    if (length(sheet) == 0) { notes <- c(notes, paste0("no sheet ", h$pattern)); next }
    d <- as.data.frame(readxl::read_excel(path, sheet = sheet[1], col_names = FALSE, .name_repair = "minimal"))
    if (ncol(d) < 1 + length(h$indicators)) { notes <- c(notes, paste0(sheet[1], ": fewer columns than expected")); next }
    name <- normalize_text(as.character(d[[1]]))
    code <- ref_districts$district_code[match(name, normalize_text(ref_districts$district_name))]
    is_row <- !is.na(code) | name %in% c("SIN DISTRITO ASIGNADO", "OTRAS UNIDADES"); is_row[is.na(is_row)] <- FALSE
    if (sum(!is.na(code)) != 21) notes <- c(notes, paste0(sheet[1], ": ", sum(!is.na(code)), " districts recognised out of 21"))
    for (j in seq_along(h$indicators)) {
      v <- suppressWarnings(as.numeric(as.character(d[[1 + j]])))
      out[[length(out) + 1]] <- data.frame(district_code = code[is_row], district_txt = name[is_row], block = h$block,
                                           indicator = h$indicators[j], value = v[is_row], stringsAsFactors = FALSE)
    }
  }
  res <- do.call(rbind, out); attr(res, "notes") <- notes; res
}

madrid_activity_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS stg_madrid_activity (
      month DATE, district_code VARCHAR, district_txt VARCHAR, block VARCHAR, indicator VARCHAR, value DOUBLE, file VARCHAR)")
  invisible(TRUE)
}

madrid_activity_ingest <- function(con) {
  src <- CONFIG$sources$madrid_activity
  idx <- activity_index()
  if (nrow(idx) == 0) { message("No activity Excel files in ", src$dir_raw); return(invisible(NULL)) }
  no_month <- idx$file[is.na(idx$month)]
  if (length(no_month) > 0) message("  ", length(no_month), " file(s) without a recognisable month (skipped; resources.csv missing or description without month): ",
                                    paste(utils::head(no_month, 5), collapse = ", "), if (length(no_month) > 5) "..." else "")
  idx <- idx[!is.na(idx$month), ]; idx <- idx[order(idx$month, idx$file), ]
  ref_districts <- read_reference("madrid_districts.csv")
  ref_motives <- read_reference("madrid_arrest_motives.csv")
  # If the reader or the motive reference changes, every month is reloaded
  signature <- digest::digest(paste(sha256_file(file.path(CONFIG$dir_ref, "madrid_arrest_motives.csv")), "v1"), algo = "sha256")
  if (!signature %in% logged_hashes(con, "madrid_activity_process")) {
    DBI::dbExecute(con, "DELETE FROM ingest_log WHERE source IN ('madrid_activity', 'madrid_activity_process')")
    DBI::dbExecute(con, "DELETE FROM stg_madrid_activity")
    log_ingest(con, "madrid_activity_process", "madrid_arrest_motives.csv", signature, action = "reload")
  }
  done <- logged_hashes(con, "madrid_activity")
  n_new <- 0; notes <- list()
  for (i in seq_len(nrow(idx))) {
    path <- file.path(src$dir_raw, idx$file[i]); hash <- sha256_file(path)
    if (hash %in% done) next
    d <- tryCatch(madrid_activity_read(path, ref_districts, ref_motives), error = function(e) { notes[[idx$file[i]]] <<- conditionMessage(e); NULL })
    if (is.null(d) || nrow(d) == 0) next
    if (length(attr(d, "notes")) > 0) notes[[idx$file[i]]] <- paste(attr(d, "notes"), collapse = " | ")
    d$month <- idx$month[i]; d$file <- idx$file[i]
    DBI::dbWithTransaction(con, {
      previous <- DBI::dbGetQuery(con, paste0("SELECT count(*) AS n FROM stg_madrid_activity WHERE month = DATE '", idx$month[i], "'"))$n
      DBI::dbExecute(con, paste0("DELETE FROM stg_madrid_activity WHERE month = DATE '", idx$month[i], "'"))
      DBI::dbAppendTable(con, "stg_madrid_activity", d[, c("month", "district_code", "district_txt", "block", "indicator", "value", "file")])
      log_ingest(con, "madrid_activity", idx$file[i], hash, format(idx$month[i], "%Y-%m"), nrow(d),
                 action = if (previous > 0) "replaced" else "added")
    })
    n_new <- n_new + 1
  }
  message(n_new, " month(s) of police activity loaded", if (n_new == 0) " (nothing new)" else "", ".")
  if (length(notes) > 0) {
    path <- write_quality_report("madrid_activity", data.frame(file = names(notes), note = unlist(notes)))
    message("  Notes for ", length(notes), " file(s): ", path)
  }
  invisible(n_new)
}

# Marts: district x month x indicator (plus the city total, which includes rows
# without district) and city-wide arrests by motive
madrid_activity_marts <- function(con) {
  if (!DBI::dbExistsTable(con, "stg_madrid_activity")) return(invisible(FALSE))
  DBI::dbExecute(con, paste0("
    CREATE OR REPLACE TABLE mart_madrid_activity AS
    SELECT month, '", ID_MADRID, "-D' || district_code AS territory_id, block, indicator, value::INTEGER AS value
    FROM stg_madrid_activity WHERE district_code IS NOT NULL AND value IS NOT NULL
    UNION ALL
    SELECT month, '", ID_MADRID, "', block, indicator, sum(value)::INTEGER
    FROM stg_madrid_activity WHERE value IS NOT NULL AND block <> 'arrest_motives' GROUP BY ALL
    ORDER BY ALL"))
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_madrid_arrests AS
    SELECT month, indicator AS motive_id, value::INTEGER AS people
    FROM stg_madrid_activity WHERE block = 'arrest_motives' AND value IS NOT NULL ORDER BY ALL")
  motives <- read_reference("madrid_arrest_motives.csv")
  motives <- motives[!duplicated(motives$motive_id), c("motive_id", "name_en", "name_es")]
  DBI::dbWriteTable(con, "mart_madrid_arrest_motives", as.data.frame(motives), overwrite = TRUE)
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_madrid_activity_meta AS
    SELECT 'Municipal Police activity statistics' AS source, 'Madrid City Council' AS publisher,
           'https://datos.madrid.es/dataset/212616-0-policia-estadisticas' AS url, 'CC BY 4.0' AS licence,
           (SELECT min(month) FROM stg_madrid_activity) AS first_month, (SELECT max(month) FROM stg_madrid_activity) AS last_month,
           (SELECT max(loaded_at) FROM ingest_log WHERE source = 'madrid_activity') AS loaded_at")
  invisible(TRUE)
}
