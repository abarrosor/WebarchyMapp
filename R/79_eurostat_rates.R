# JSON-stat reader (the Eurostat API and the PxWeb portals answer in it) ---------------------------------
# Used by Germany (R/86_de.R), Switzerland (R/87_ch.R) and the Eurostat regional tables (R/92_eurostat_regions.R).
#
# Until 2026-11-17 this file also built the "Countries table" of the app: the national rates per 100,000
# inhabitants of Eurostat's crim_off_cat (ICCS categories), the one place that put several countries side by
# side. It was retired (P-063): the update no longer downloads that table, mart_country_rates is no longer
# built (R/50_marts.R drops the one an earlier run left) and the app has no such page. The copies already
# downloaded stay in data/raw/shared/eurostat/, as every downloaded file does. The file keeps its name because
# every R/*.R of the project is loaded and a file cannot be renamed on the founder's computer from outside.

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
