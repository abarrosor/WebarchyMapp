# Consistency checks of the other countries' tables ---------------------------------------------
# Read-only: looks at stg_country_crime, ref_country_population, ref_country_offence_types,
# dim_territory and data/territory_<cc>.gpkg and reports what does not add up. Nothing is changed.
# Severity: "error" = the app would show something wrong (fix before trusting the country);
# "warning" = worth a look; "info" = a known property of the source, recorded for the About page.
# Result: a data frame (country, check, severity, n, detail), printed and saved as
# reports/country_checks_<date>.csv. Menu option 15, or check_countries() in the console.

check_countries <- function(only = NULL, save = TRUE) {
  with_db(function(con) {
    out <- country_checks(con, only)
    if (save && nrow(out)) {
      f <- file.path(CONFIG$dir_reports, paste0("country_checks_", format(Sys.Date(), "%Y%m%d"), ".csv"))
      utils::write.csv(out, f, row.names = FALSE, fileEncoding = "UTF-8"); message("Saved: ", f)
    }
    invisible(out)
  })
}

country_checks <- function(con, only = NULL) {
  need <- c("stg_country_crime", "ref_country_population", "ref_country_offence_types", "dim_territory")
  if (!all(need %in% DBI::dbListTables(con))) { message("Country tables not created yet."); return(invisible(data.frame())) }
  pairs <- DBI::dbGetQuery(con, "SELECT DISTINCT country, source FROM stg_country_crime ORDER BY 1, 2")
  if (!is.null(only)) pairs <- pairs[pairs$country %in% only, ]
  rows <- list()
  add <- function(cc, check, severity, n, detail = "") rows[[length(rows) + 1]] <<- data.frame(country = cc, check = check, severity = severity, n = as.integer(n), detail = detail, stringsAsFactors = FALSE)
  q <- function(sql, ...) DBI::dbGetQuery(con, sprintf(sql, ...))
  # One pass per country and source ('ES', then 'ES/mossos'): a second source has its own types and levels
  for (i in seq_len(nrow(pairs))) {
    cc0 <- pairs$country[i]; sid <- pairs$source[i]; m0 <- COUNTRY_MODULES[[cc0]]; ccq <- DBI::dbQuoteString(con, cc0); sq <- DBI::dbQuoteString(con, sid)
    cc <- if (sid == "main") cc0 else paste0(cc0, "/", sid)
    m <- if (sid == "main") m0 else { s <- m0$sources[[sid]]; if (is.null(s)) NULL else list(levels = s$levels, gpkg = m0$gpkg, financial_year = m0$financial_year) }
    crime <- q("SELECT territory_id, level, offence_code, year, quarter, cumulative FROM stg_country_crime WHERE country = %s AND source = %s", ccq, sq)
    pop   <- q("SELECT territory_id, level, year, population FROM ref_country_population WHERE country = %s", ccq)
    types <- q("SELECT source_code, offence_type_id, included_in FROM ref_country_offence_types WHERE country = %s AND source = %s", ccq, sq)
    terr  <- q("SELECT territory_id, level, parent_id, name FROM dim_territory WHERE country = %s", ccq)
    # 1. Levels declared by the module vs levels with data
    if (!is.null(m)) { miss <- setdiff(m$levels, unique(crime$level)); if (length(miss)) add(cc, "levels_without_data", "warning", length(miss), paste(miss, collapse = ", ")) }
    extra <- setdiff(unique(crime$level), c("country", if (sid != "main") "region", m$levels %||% unique(crime$level))); if (length(extra)) add(cc, "levels_not_declared", "warning", length(extra), paste(extra, collapse = ", "))
    # 2. Duplicate keys
    dup <- sum(duplicated(crime[, c("territory_id", "offence_code", "year", "quarter")])); if (dup) add(cc, "duplicate_keys", "error", dup)
    # 3. Territories with data but no row in dim_territory (the app shows the id instead of a name)
    orphan <- setdiff(unique(crime$territory_id), terr$territory_id); if (length(orphan)) add(cc, "territories_without_name", "error", length(orphan), paste(utils::head(orphan, 5), collapse = ", "))
    # 4. Parents missing (region_of() cannot climb)
    par <- unique(terr$parent_id[!is.na(terr$parent_id)]); nop <- setdiff(par, terr$territory_id); if (length(nop)) add(cc, "parents_missing", "error", length(nop), paste(utils::head(nop, 5), collapse = ", "))
    # 5. Cumulative must not decrease inside a year (negative corrections exist in a few sources: warning)
    o <- crime[order(crime$territory_id, crime$offence_code, crime$year, crime$quarter), ]
    same <- c(FALSE, o$territory_id[-1] == o$territory_id[-nrow(o)] & o$offence_code[-1] == o$offence_code[-nrow(o)] & o$year[-1] == o$year[-nrow(o)])
    dec <- sum(same & o$cumulative < c(NA, o$cumulative[-nrow(o)]), na.rm = TRUE); if (dec) add(cc, "cumulative_decreases", "warning", dec, "quarterly figure negative (correction in the source?)")
    neg <- sum(crime$cumulative < 0, na.rm = TRUE); if (neg) add(cc, "negative_cumulative", "warning", neg)
    # 6. Offence codes in the data that the reference table does not know (left out of the app)
    unk <- setdiff(unique(crime$offence_code), types$source_code); if (length(unk)) add(cc, "offence_codes_unmapped", "warning", length(unk), paste(utils::head(unk, 6), collapse = " / "))
    # 7. Total vs parts: the categories that count in the total must not exceed it (same territory / period)
    tot_code <- types$source_code[types$offence_type_id == "total"]; parts <- types$source_code[!is.na(types$included_in) & types$included_in == "total"]
    if (length(tot_code) && length(parts)) {
      tt <- crime[crime$offence_code %in% tot_code, c("territory_id", "year", "quarter", "cumulative")]
      pp <- crime[crime$offence_code %in% parts, ] |> dplyr::group_by(territory_id, year, quarter) |> dplyr::summarise(parts = sum(cumulative, na.rm = TRUE), .groups = "drop")
      j <- dplyr::inner_join(tt, pp, by = c("territory_id", "year", "quarter"))
      # Sources that round small figures (CBS rounds municipal counts to 5) make the parts exceed the total slightly: tolerated up to 5 % + 10
      over_small <- sum(j$parts > j$cumulative * 1.001 + 1 & j$parts <= j$cumulative * 1.05 + 10, na.rm = TRUE); over <- sum(j$parts > j$cumulative * 1.05 + 10, na.rm = TRUE)
      if (over) add(cc, "parts_exceed_total", "error", over, "sum of the categories marked total > total by more than 5% (double counting?)")
      if (over_small) add(cc, "parts_exceed_total_rounding", "info", over_small, "parts slightly above the total (rounded source figures)")
      under <- sum(j$parts < j$cumulative * 0.5, na.rm = TRUE); if (under) add(cc, "parts_far_below_total", "info", under, "categories cover < 50% of the total (large 'other' remainder)")
    }
    # 8. Country total = sum of the regions (when both exist): within 1 %
    if (all(c("country", "region") %in% crime$level) && length(tot_code)) {
      nat <- crime[crime$level == "country" & crime$offence_code %in% tot_code, c("year", "quarter", "cumulative")]
      reg <- crime[crime$level == "region" & crime$offence_code %in% tot_code, ] |> dplyr::group_by(year, quarter) |> dplyr::summarise(s = sum(cumulative, na.rm = TRUE), .groups = "drop")
      j <- dplyr::inner_join(nat, reg, by = c("year", "quarter")); bad <- sum(abs(j$s - j$cumulative) > pmax(1, 0.01 * j$cumulative), na.rm = TRUE)
      if (bad) add(cc, "regions_vs_country", "info", bad, "regions do not add up to the national figure (unallocated cases, or the national row comes from another table)")
    }
    # 9. Population: years, zeros, coverage of the territories with data, sum of municipalities vs region
    #    (the country's population is checked once, with its main source; a second source only its own coverage)
    if (nrow(pop) && sid != "main") {
      for (lv in setdiff(unique(crime$level), c("country", "region"))) {
        ids <- unique(crime$territory_id[crime$level == lv]); nop <- setdiff(ids, unique(pop$territory_id)); if (length(nop)) add(cc, paste0("no_population_", lv), if (length(nop) > 0.5 * length(ids)) "warning" else "info", length(nop), paste(utils::head(nop, 4), collapse = ", "))
      }
    } else if (nrow(pop)) {
      z <- sum(pop$population <= 0, na.rm = TRUE); if (z) add(cc, "population_not_positive", "info", z, "zero population (uninhabited or discontinued units): no rate for them")
      cy <- range(crime$year); py <- range(pop$year)
      if (py[2] < cy[2] - 1) add(cc, "population_years_behind", "warning", cy[2] - py[2], sprintf("population to %d, crime to %d (the app uses the latest population year)", py[2], cy[2]))
      if (py[1] > cy[1]) add(cc, "population_starts_later", "info", py[1] - cy[1], sprintf("population from %d, crime from %d (early years use the first population year)", py[1], cy[1]))
      for (lv in setdiff(unique(crime$level), "country")) {
        ids <- unique(crime$territory_id[crime$level == lv]); nop <- setdiff(ids, unique(pop$territory_id)); if (length(nop)) add(cc, paste0("no_population_", lv), if (length(nop) > 0.5 * length(ids)) "warning" else "info", length(nop), paste(utils::head(nop, 4), collapse = ", "))
      }
      if (all(c("region", "municipality") %in% pop$level)) {
        mu <- pop[pop$level == "municipality", ]; mu$region <- terr$parent_id[match(mu$territory_id, terr$territory_id)]
        # climb to the region when the parent is a province
        lvp <- terr$level[match(mu$region, terr$territory_id)]; climb <- !is.na(lvp) & lvp != "region"
        if (any(climb)) mu$region[climb] <- terr$parent_id[match(mu$region[climb], terr$territory_id)]
        s <- mu |> dplyr::filter(!is.na(region)) |> dplyr::group_by(region, year) |> dplyr::summarise(s = sum(population), .groups = "drop")
        r <- pop[pop$level == "region", c("territory_id", "year", "population")]
        j <- dplyr::inner_join(s, r, by = c("region" = "territory_id", "year")); bad <- sum(abs(j$s - j$population) > 0.05 * j$population, na.rm = TRUE)
        if (bad) add(cc, "municipal_pop_vs_region", "warning", bad, "sum of the municipalities differs from the region by > 5% (missing municipalities or different reference dates)")
      }
    } else add(cc, "no_population", "info", 0, "counts only, no rates")
    # 10. Boundaries vs territories
    gp <- if (!is.null(m)) m$gpkg else file.path(CONFIG$dir_data %||% "data", paste0("territory_", tolower(cc), ".gpkg"))
    if (!is.na(gp) && file.exists(gp)) {
      for (lay in intersect(sf::st_layers(gp)$name, unique(crime$level))) {
        g <- sf::st_read(gp, layer = lay, quiet = TRUE); ids <- unique(crime$territory_id[crime$level == lay])
        nog <- setdiff(ids, g$territory_id); if (length(nog)) add(cc, paste0("no_polygon_", lay), if (length(nog) > 0.2 * length(ids)) "warning" else "info", length(nog), paste(utils::head(nog, 4), collapse = ", "))
        nod <- setdiff(g$territory_id, terr$territory_id); if (length(nod)) add(cc, paste0("polygon_without_territory_", lay), "warning", length(nod), paste(utils::head(nod, 4), collapse = ", "))
        inv <- sum(!sf::st_is_valid(g)); if (inv) add(cc, paste0("invalid_geometry_", lay), "warning", inv)
      }
    } else add(cc, "no_boundaries", "info", 0, "tables only")
    # 11. Last period: a quarter in the future, or the current quarter already complete, points to a reading problem
    last <- max(crime$year * 10 + crime$quarter); now <- as.integer(format(Sys.Date(), "%Y")) * 10 + ceiling(as.integer(format(Sys.Date(), "%m")) / 3)
    if (last >= now && !isTRUE(m$financial_year)) add(cc, "future_or_current_quarter_complete", "warning", 1, sprintf("last period %dQ%d (today is %dQ%d); unreported months read as zeros?", last %/% 10, last %% 10, now %/% 10, now %% 10))
  }
  out <- if (length(rows)) do.call(rbind, rows) else data.frame(country = character(), check = character(), severity = character(), n = integer(), detail = character())
  if (nrow(out) == 0) { message("Country checks: nothing to report for ", paste(unique(pairs$country), collapse = ", ")); return(invisible(out)) }
  out <- out[order(match(out$severity, c("error", "warning", "info")), out$country, out$check), ]
  message("Country checks (", sum(out$severity == "error"), " errors, ", sum(out$severity == "warning"), " warnings, ", sum(out$severity == "info"), " notes)")
  for (i in seq_len(nrow(out))) message(sprintf("  [%s] %-9s %-38s %6s  %s", c(error = "!!", warning = "! ", info = "i ")[out$severity[i]], out$country[i], out$check[i], fmt_int(out$n[i]), substr(out$detail[i], 1, 110)))
  invisible(out)
}
