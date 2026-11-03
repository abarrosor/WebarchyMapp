# Countries table: national rates per 100,000 inhabitants from Eurostat -------------------
# Dataset crim_off_cat "Police-recorded offences by offence category" (ICCS), unit P_HTHAB,
# EU-27 + EFTA + candidates + UK nations, 2008 onwards. Eurostat statistical data: CC BY 4.0.
# Read through the key-less JSON-stat API. This is the ONLY place where the app puts
# several countries side by side, and it does so with a warning: each country reports
# under its own criminal law and recording rules.
#
# UNODC (data.unodc.org) would add the American countries, but its terms allow personal,
# non-commercial use only and forbid redistribution and derivative compilations, so it is
# not loaded here (see Claude/02_data/source-catalogue.md).

EUROSTAT_RATES <- list(
  dataset = "crim_off_cat",
  api = "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data",
  # ICCS code -> project offence id, English name, Spanish name
  iccs = data.frame(
    code = c("ICCS0101", "ICCS0102", "ICCS020111", "ICCS0301", "ICCS03011", "ICCS0401", "ICCS0501", "ICCS05012", "ICCS0502", "ICCS05021", "ICCS0601", "ICCS0701"),
    offence_id = c("intentional_homicide", "attempted_homicide", "serious_assault", "sexual_violence", "rape", "robbery", "burglary", "residential_burglary", "theft", "vehicle_theft", "drug_offences", "fraud"),
    offence_en = c("Intentional homicide", "Attempted intentional homicide", "Serious assault", "Sexual violence", "Rape", "Robbery", "Burglary", "Burglary of private residential premises", "Theft", "Theft of a motorized vehicle or parts", "Unlawful acts involving controlled drugs", "Fraud"),
    offence_local = c("Homicidio intencional", "Tentativa de homicidio", "Agresión grave", "Violencia sexual", "Violación", "Robo con violencia", "Robo con fuerza", "Robo con fuerza en domicilio", "Hurto", "Robo de vehículo o de sus partes", "Delitos de drogas", "Estafa"),
    stringsAsFactors = FALSE),
  dir_raw = "data/raw/eurostat"
)

# JSON-stat 2.0 (one dataset) -> long data frame with one column per dimension and `value`
jsonstat_to_df <- function(js) {
  # JSON-stat 1.x (PxWeb "json-stat"): one dataset under $dataset, ids and sizes inside $dimension
  if (is.null(js$id) && !is.null(js$dataset)) { ds <- js$dataset; js <- list(id = ds$dimension$id, size = ds$dimension$size, dimension = ds$dimension[setdiff(names(ds$dimension), c("id", "size", "role"))], value = ds$value) }
  ids <- unlist(js$id); size <- unlist(js$size)
  if (length(ids) == 0) stop("Not a JSON-stat dataset (top-level fields: ", paste(utils::head(names(js), 12), collapse = ", "), if (!is.null(js$error) || !is.null(js$message)) paste0("; message: ", js$error %||% js$message) else "", ")")
  cats <- lapply(ids, function(d) { idx <- js$dimension[[d]]$category$index; lab <- js$dimension[[d]]$category$label
    keys <- if (is.list(idx)) names(idx)[order(unlist(idx))] else if (!is.null(names(idx))) names(idx)[order(unlist(idx))] else unlist(idx)
    list(keys = keys, labels = unlist(lab)[keys]) })
  names(cats) <- ids
  n <- prod(size)
  grid <- expand.grid(rev(lapply(cats, function(c) seq_along(c$keys))), KEEP.OUT.ATTRS = FALSE)[, rev(seq_along(ids))]   # row-major order
  names(grid) <- ids
  vals <- rep(NA_real_, n)
  v <- js$value
  if (is.list(v) && !is.null(names(v))) { i <- as.integer(names(v)) + 1L; vals[i] <- vapply(v, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1)) }
  else if (!is.null(v)) vals[seq_along(v)] <- vapply(v, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1))
  out <- as.data.frame(lapply(ids, function(d) cats[[d]]$keys[grid[[d]]]), stringsAsFactors = FALSE); names(out) <- ids
  for (d in ids) out[[paste0(d, "_label")]] <- unname(cats[[d]]$labels[out[[d]]])
  out$value <- vals
  out
}

eurostat_rates_download <- function(days = 30) {
  x <- EUROSTAT_RATES
  name <- paste0(x$dataset, "_P_HTHAB.json")
  prev <- newest_first(list.files(x$dir_raw, paste0("_", name, "$"), full.names = TRUE))
  if (length(prev) > 0 && difftime(Sys.time(), file.mtime(prev[1]), units = "days") < days) { message("= Eurostat ", x$dataset, " downloaded less than ", days, " days ago"); return(invisible(prev[1])) }
  url <- paste0(x$api, "/", x$dataset, "?format=JSON&lang=EN&unit=P_HTHAB", paste0("&iccs=", x$iccs$code, collapse = ""))
  download_versioned(url, x$dir_raw, name = name)
}

eurostat_rates_build <- function(con) {
  x <- EUROSTAT_RATES
  f <- newest_first(list.files(x$dir_raw, paste0("_", x$dataset, "_P_HTHAB\\.json$"), full.names = TRUE))
  if (length(f) == 0) { message("No Eurostat file yet (", x$dir_raw, ")"); return(invisible(FALSE)) }
  js <- jsonlite::fromJSON(f[1], simplifyVector = FALSE)
  d <- jsonstat_to_df(js)
  d <- d[!is.na(d$value), ]
  m <- match(d$iccs, x$iccs$code)
  # Eurostat country codes: EL -> GR, UKC-L etc. stay as published; EU aggregates are dropped
  geo <- d$geo; geo[geo == "EL"] <- "GR"
  keep <- !grepl("^EU|^EA", geo)
  out <- data.frame(country = geo[keep], country_name = d$geo_label[keep], offence_id = x$iccs$offence_id[m][keep],
                    offence_en = x$iccs$offence_en[m][keep], offence_local = x$iccs$offence_local[m][keep],
                    year = as.integer(d$time[keep]), rate = d$value[keep], source = "Eurostat", note = "", stringsAsFactors = FALSE)
  out <- out[!is.na(out$offence_id), ]
  DBI::dbWriteTable(con, "mart_country_rates", out, overwrite = TRUE)
  log_ingest(con, "eurostat_rates", basename(f[1]), sha256_file(f[1]), paste(range(out$year), collapse = "-"), nrow(out))
  message(sprintf("Eurostat rates: %d countries, %d offence categories, %d-%d (%s)", length(unique(out$country)), length(unique(out$offence_id)), min(out$year), max(out$year), basename(f[1])))
  invisible(TRUE)
}

# Called by update_countries() / update_all(): download then rebuild the table
eurostat_rates_update <- function(con, download = TRUE) {
  step("Countries table: Eurostat national rates (crim_off_cat)")
  if (download) tryCatch(eurostat_rates_download(), error = function(e) message("Download not available (Eurostat): ", conditionMessage(e)))
  attempt("Eurostat rates not built", eurostat_rates_build(con))
}
