# Official population by municipality (INE) --------------------------------------
# INEbase table 29005: "Official population figures of Spanish municipalities:
# revision of the municipal register". One figure per municipality and year, at
# 1 January. The whole of Spain in a single ";"-separated CSV (~25 MB):
#   Municipios;Sexo;Periodo;Total
#   28079 Madrid;Total;2025;3.422.416
# Only the total (no sex split) is kept. It is the denominator for the Crime
# Balance rates by municipality, province, region and country.

ine_population_download <- function(days = 30) {
  src <- CONFIG$sources$ine_population
  previous <- newest_first(list.files(src$dir_raw, "29005\\.csv$", full.names = TRUE))
  # The INE updates it once a year (December-January): no need to fetch it every run
  if (length(previous) > 0 && difftime(Sys.time(), file.mtime(previous[1]), units = "days") < days) {
    message("= INE population downloaded less than ", days, " days ago (", basename(previous[1]), ")")
    return(invisible(previous[1]))
  }
  download_versioned(src$urls, src$dir_raw)
}

ine_population_read <- function(path) {
  df <- readr::read_delim(path, delim = ";", col_types = readr::cols(.default = "c"),
                          locale = readr::locale(encoding = "UTF-8"), trim_ws = TRUE, progress = FALSE)
  names(df) <- normalize_text(sub("^﻿", "", names(df)))
  missing <- setdiff(c("MUNICIPIOS", "SEXO", "PERIODO", "TOTAL"), names(df))
  if (length(missing) > 0) stop(basename(path), ": missing columns ", toString(missing), " (has the INE table changed?)")
  df <- df[normalize_text(df$SEXO) == "TOTAL" & grepl("^[0-9]{5}\\s", df$MUNICIPIOS), ]
  ref <- read_reference("ine_provinces.csv")
  code <- substr(df$MUNICIPIOS, 1, 5)
  prov <- substr(code, 1, 2)
  region <- ref$region_code[match(prov, ref$province_code)]
  out <- data.frame(
    territory_id = paste0("ES-", region, "-", prov, "-", code),
    name         = trimws(sub("^[0-9]{5}\\s+", "", df$MUNICIPIOS)),
    year         = suppressWarnings(as.integer(substr(df$PERIODO, 1, 4))),
    population   = parse_es_number(df$TOTAL))
  out[!is.na(region) & !is.na(out$year) & !is.na(out$population), ]
}

ine_population_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ref_population_municipality (
      territory_id VARCHAR, name VARCHAR, year INTEGER, population INTEGER, file VARCHAR)")
}

ine_population_ingest <- function(con) {
  src <- CONFIG$sources$ine_population
  f <- newest_first(list.files(src$dir_raw, "29005\\.csv$", full.names = TRUE))
  if (length(f) == 0) { message("No INE population file in ", src$dir_raw); return(invisible(NULL)) }
  f <- f[1]
  hash <- sha256_file(f)
  if (hash %in% logged_hashes(con, "ine_population") &&
      DBI::dbGetQuery(con, "SELECT count(*) AS n FROM ref_population_municipality")$n > 0) {
    message("= ", basename(f), " (already loaded)")
    return(invisible(NULL))
  }
  pop <- ine_population_read(f)
  n_mun <- length(unique(pop$territory_id))
  if (n_mun < 8000) warning("Few municipalities in the INE table (", n_mun, "): check the file")
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM ref_population_municipality")
    DBI::dbAppendTable(con, "ref_population_municipality", data.frame(pop, file = basename(f)))
    log_ingest(con, "ine_population", basename(f), hash, paste(range(pop$year), collapse = "-"), nrow(pop))
  })
  message(sprintf("+ %s: %s municipalities, years %d to %d", basename(f), fmt_int(n_mun), min(pop$year), max(pop$year)))
  invisible(pop)
}

# Population by municipality, province, region and country, for the whole of Spain
ine_population_marts <- function(con) {
  DBI::dbExecute(con, "
    CREATE OR REPLACE TABLE mart_population_municipal AS
    WITH m AS (SELECT territory_id, year, population FROM ref_population_municipality),
    all_levels AS (
      SELECT 'municipality' AS level, territory_id, year, population FROM m
      UNION ALL
      SELECT 'province', regexp_extract(territory_id, '^ES-[0-9]+-[0-9]+'), year, sum(population) FROM m GROUP BY 2, 3
      UNION ALL
      SELECT 'region', regexp_extract(territory_id, '^ES-[0-9]+'), year, sum(population) FROM m GROUP BY 2, 3
      UNION ALL
      SELECT 'country', 'ES', year, sum(population) FROM m GROUP BY 3
    )
    SELECT level, territory_id, year, population::INTEGER AS population FROM all_levels ORDER BY ALL")
  invisible(TRUE)
}
