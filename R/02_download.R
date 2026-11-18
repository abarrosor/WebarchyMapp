# Downloads --------------------------------------------------------------------
# Rules that apply to every source:
#   - a download goes to a temporary file and is only kept if it completes;
#   - an existing original is never overwritten or deleted;
#   - versioned sources are saved as data/raw/<source>/YYYYMMDD_<name>, and no
#     copy is saved when the content is identical to the last one.

USER_AGENT <- "webarchymapp-personal-use (R httr2)"

# Age of a downloaded file in days, used by every module to decide whether to ask the portal again
# ("downloaded less than N days ago"). With options(webarchy.refresh = TRUE) every file counts as old,
# so that a file that came down broken or truncated is asked for again (menu option 14 offers it);
# versioned copies are only kept when the content differs, so asking again costs nothing on disk.
file_age_days <- function(f) {
  if (isTRUE(getOption("webarchy.refresh", FALSE))) return(Inf)
  if (length(f) == 0 || is.na(f[1]) || !file.exists(f[1])) return(Inf)
  as.numeric(difftime(Sys.time(), file.mtime(f[1]), units = "days"))
}

# Perform a request; when the connection itself fails (the server never answers, as opposed to answering an
# error), try once more over IPv4 only: a portal whose IPv6 address does not answer fails in curl, while a
# browser falls back to IPv4 without saying so (seen with datos.cdmx.gob.mx)
perform <- function(req, path = NULL) {
  tryCatch(httr2::req_perform(req, path = path), error = function(e) {
    if (!grepl("could not connect|failed to connect|couldn't connect|connection timed out", conditionMessage(e), ignore.case = TRUE)) stop(e)
    message("  no connection to the server; trying once more over IPv4 only")
    httr2::req_perform(httr2::req_options(req, ipresolve = 1L), path = path)
  })
}

# Download `url` to `dest`. Fails if `dest` already exists.
download_safely <- function(url, dest, timeout = 300) {
  tmp <- paste0(dest, ".part")
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  httr2::request(url) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(timeout) |>
    perform(path = tmp)
  if (!file.exists(tmp) || file.size(tmp) == 0) stop("Empty download: ", url)
  # A portal that answers an HTML page (a login, an error, a maintenance notice) instead of the file: not kept
  if (!grepl("\\.html?$", dest, ignore.case = TRUE)) {
    head <- tryCatch({ b <- readBin(tmp, what = "raw", n = 300L); rawToChar(b[b != as.raw(0L)]) }, error = function(e) "")   # binary files (xlsx, zip) hold nul bytes
    if (grepl("^\\s*(<!doctype html|<html)", head, ignore.case = TRUE, useBytes = TRUE)) stop("The server answered an HTML page instead of the file (changed URL, login or maintenance page): ", url)
  }
  if (file.exists(dest)) stop(dest, " already exists and is never overwritten")
  if (!file.rename(tmp, dest)) stop("Could not save ", dest)
  invisible(dest)
}

# Download a versioned file: keeps YYYYMMDD_<name> unless identical to the last copy
download_versioned <- function(url, dir, name = basename(sub("\\?.*$", "", url))) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tmp <- file.path(tempdir(), paste0("download_", name))
  if (file.exists(tmp)) file.remove(tmp)
  download_safely(url, tmp)
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  keep_version(tmp, dir, name)
}

# Save an already downloaded file as dir/YYYYMMDD_<name>, unless it is identical
# to the latest saved version of that name
keep_version <- function(tmp, dir, name) {
  pattern <- paste0("_", gsub("([.()+])", "\\\\\\1", name), "$")
  previous <- newest_first(list.files(dir, pattern = pattern, full.names = TRUE))
  hash <- sha256_file(tmp)
  if (length(previous) > 0 && identical(sha256_file(previous[1]), hash)) {
    message("= ", name, " unchanged (keeping ", basename(previous[1]), ")")
    return(invisible(previous[1]))
  }
  dest <- file.path(dir, paste0(format(Sys.Date(), "%Y%m%d"), "_", name))
  if (file.exists(dest)) dest <- file.path(dir, paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", name))
  if (!file.copy(tmp, dest)) stop("Could not save ", dest)
  message("+ ", basename(dest))
  invisible(dest)
}

# GET a URL as text; NULL on any error or HTTP >= 400
get_text <- function(url, timeout = 60) {
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent(USER_AGENT) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      perform(),
    error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400) return(NULL)
  tryCatch(httr2::resp_body_string(resp), error = function(e) rawToChar(httr2::resp_body_raw(resp)))
}

# --- CKAN (datos.madrid.es and Open Data BCN) --------------------------------------
# Resources (files) of a dataset
ckan_resources <- function(dataset_id, base = CONFIG$ckan_madrid) {
  resp <- httr2::request(base) |>
    httr2::req_url_path_append("package_show") |>
    httr2::req_url_query(id = dataset_id) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(60) |>
    perform()
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  if (!isTRUE(body$success)) stop("CKAN API returned success = FALSE for ", dataset_id)
  res <- body$result$resources
  field <- function(r, x) if (is.null(r[[x]])) NA_character_ else as.character(r[[x]])
  dplyr::tibble(
    id          = vapply(res, field, "", "id"),
    name        = vapply(res, field, "", "name"),
    description = vapply(res, field, "", "description"),
    format      = tolower(vapply(res, field, "", "format")),
    url         = vapply(res, field, "", "url"),
    created     = vapply(res, field, "", "created"),
    modified    = vapply(res, field, "", "last_modified")
  )
}

# Local file name of a resource: the end of the URL if it has an extension;
# otherwise (Open Data BCN URLs end in ".../download") the resource name
ckan_file_name <- function(res) {
  b <- basename(sub("\\?.*$", "", res$url))
  ifelse(grepl("\\.[A-Za-z0-9]{2,5}$", b), b, gsub("[^A-Za-z0-9._-]+", "_", res$name))
}

# Download the resources of one format that are not yet in `dir`
ckan_download_new <- function(dataset_id, dir, format = "csv", base = CONFIG$ckan_madrid, include = NULL) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  res <- ckan_resources(dataset_id, base = base)
  if (!is.null(include)) res <- res[grepl(include, res$name, ignore.case = TRUE), ]
  res <- res[res$format == format | grepl(paste0("\\.", format, "$"), res$url, ignore.case = TRUE), ]
  if (nrow(res) == 0) { message("No resources in format ", format); return(invisible(character())) }
  res$file <- file.path(dir, ckan_file_name(res))
  new <- res[!file.exists(res$file), ]
  for (i in seq_len(nrow(new))) {
    message("Downloading: ", new$description[i], " -> ", basename(new$file[i]))
    download_safely(new$url[i], new$file[i])
    Sys.sleep(1)
  }
  message(nrow(new), " new file(s); ", nrow(res) - nrow(new), " already downloaded.")
  invisible(new$file)
}
