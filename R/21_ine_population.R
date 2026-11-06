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
  download_versioned(es_source("ine_population"), src$dir_raw)
}

ine_population_read <- function(path) {
  df <- readr::read_delim(path, delim = ";", col_types = readr::cols(.default = "c"),
                          locale = readr::locale(encoding = "UTF-8"), trim_ws = TRUE, progress = FALSE)
  names(df) <- normalize_text(sub("^﻿", "", names(df)))
  missing <- setdiff(c("MUNICIPIOS", "SEXO", "PERIODO", "TOTAL"), names(df))
  if (length(missing) > 0) stop(basename(path), ": missing columns ", toString(missing), " (has the INE table changed?)")
  df <- df[normalize_text(df$SEXO) == "TOTAL" & grepl("^[0-9]{5}\\s", df$MUNICIPIOS), ]
  ref <- read_reference("es_provinces.csv")
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

# The register figures go straight into the shared ref_country_population of the Spain module
# (municipality, province, region and country rows, country = 'ES'); nothing else keeps them.
ine_population_ingest <- function(con) {
  src <- CONFIG$sources$ine_population
  f <- newest_first(list.files(src$dir_raw, "29005\\.csv$", full.names = TRUE))
  if (length(f) == 0) { message("No INE population file in ", src$dir_raw); return(invisible(NULL)) }
  f <- f[1]
  hash <- sha256_file(f)
  loaded <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM ref_country_population WHERE country = 'ES' AND level = 'municipality'")$n
  if (hash %in% logged_hashes(con, "ine_population") && loaded > 0) {
    message("= ", basename(f), " (already loaded)")
    return(invisible(NULL))
  }
  pop <- ine_population_read(f)
  n_mun <- length(unique(pop$territory_id))
  if (n_mun < 8000) warning("Few municipalities in the INE table (", n_mun, "): check the file")
  m <- pop[, c("territory_id", "year", "population")]
  up <- function(level, id) m |> dplyr::group_by(territory_id = id, year) |> dplyr::summarise(population = sum(population), .groups = "drop") |> dplyr::mutate(level = level)
  all <- dplyr::bind_rows(data.frame(m, level = "municipality"),
                          up("province", sub("^(ES-[0-9]+-[0-9]+).*$", "\\1", m$territory_id)),
                          up("region", sub("^(ES-[0-9]+).*$", "\\1", m$territory_id)),
                          up("country", rep("ES", nrow(m))))
  all$country <- "ES"; all$population <- as.numeric(all$population)
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM ref_country_population WHERE country = 'ES' AND level IN ('municipality', 'province', 'region', 'country')")
    DBI::dbAppendTable(con, "ref_country_population", as.data.frame(all[, c("country", "territory_id", "level", "year", "population")]))
    log_ingest(con, "ine_population", basename(f), hash, paste(range(pop$year), collapse = "-"), nrow(pop))
  })
  message(sprintf("+ %s: %s municipalities, years %d to %d", basename(f), fmt_int(n_mun), min(pop$year), max(pop$year)))
  invisible(pop)
}
