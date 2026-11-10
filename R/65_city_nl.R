# Dutch municipalities (gemeenten): registered crimes by wijk, monthly (Politie / CBS 47022NED) -------------
# A generator for city modules of the shared city framework (R/60_cities.R): `nl_city_module(code, key, gm,
# name)` describes one gemeente and the file registers Amsterdam at the end. Any other gemeente is one more
# `register_city(nl_city_module(...))` line (Rotterdam "0599", Den Haag "0518", Utrecht "0344"...).
#
# Source: "Geregistreerde misdrijven; soort misdrijf, wijk, buurt, maandcijfers" (dataderden.cbs.nl, table
# 47022NED, data owner Politie, CC BY 4.0): crimes registered by the police by month, type and CBS wijk / buurt,
# all years in the wijk and buurt boundaries of 1 January of the latest edition, attempts included. Read
# through the CBS OData v3 services (JSON, no key) like the country module (R/72_nl.R, whose helpers this
# file reuses). Hierarchy: gemeente > districts > CBS wijken as neighbourhoods (buurten are too small for the
# threshold of 10 and five times the volume). Districts: Amsterdam's stadsdelen through the city's own
# "gebieden" API (wijk -> stadsdeel); a gemeente without a district mapping has one district, itself.
# Boundaries and residents: CBS "Wijk- en buurtkaart" (PDOK WFS, CC BY 4.0), field aantal_inwoners.
# Types: reference/nl_city_event_types.csv (SoortMisdrijf title -> block, sensitive, names); aggregates
# (Totaal misdrijven, Vermogensmisdrijven...) are not in the file and are left out; the total is the sum.
#
# Written against the OData metadata, not run against the live service: `nl_city_inspect("amsterdam")`
# prints what was downloaded and the SoortMisdrijf titles that have no equivalence.

NL_CITY_TABLE <- "47022NED"
NL_CITY_API  <- "https://dataderden.cbs.nl/ODataApi/odata"
NL_CITY_FEED <- "https://dataderden.cbs.nl/ODataFeed/odata"
# The PDOK service ignores attribute filters and answers 1,000 features at most: the whole country is fetched in
# pages (startIndex) and the gemeente is picked here
NL_CITY_WFS  <- paste0("https://service.pdok.nl/cbs/wijkenbuurten/%d/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0",
                       "&typeName=wijkenbuurten:%s&outputFormat=json&srsName=EPSG:4326&count=1000&startIndex=%d")
NL_CITY_WFS_PAGE <- 1000L
NL_CITY_WFS_YEAR <- 2025L
NL_CITY_PAGE <- "https://data.overheid.nl/dataset/5251-geregistreerde-misdrijven--soort-misdrijf--wijk--buurt--maandcijfers"

nl_city_dir <- function(key) file.path(CONFIG$dir_raw, paste0("city_", key))
nl_city_newest <- function(key, pattern) { f <- newest_first(list.files(nl_city_dir(key), pattern, full.names = TRUE)); if (length(f)) f[1] else NA_character_ }

# --- Download: metadata, one request per year (wijk rows of the gemeente), boundaries ----------------------
nl_city_download <- function(m, days = 20) {
  dir <- nl_city_dir(m$key); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  recent <- function(pattern, d) { f <- nl_city_newest(m$key, pattern); !is.na(f) && difftime(Sys.time(), file.mtime(f), units = "days") < d }
  if (recent(paste0("_", NL_CITY_TABLE, "_[0-9]{4}_p01\\.json$"), days)) message("= ", m$name_en, " crime table downloaded less than ", days, " days ago") else {
    for (meta in c("TableInfos", "Perioden", "SoortMisdrijf")) download_versioned(sprintf("%s/%s/%s", NL_CITY_API, NL_CITY_TABLE, meta), dir, name = sprintf("%s_%s.json", NL_CITY_TABLE, meta))
    # the WijkenEnBuurten dimension of the whole country is large: only the rows of this gemeente
    download_versioned(sprintf("%s/%s/WijkenEnBuurten?$format=json&$filter=substringof('%s',Key)", NL_CITY_API, NL_CITY_TABLE, m$gm), dir, name = sprintf("%s_WijkenEnBuurten.json", NL_CITY_TABLE))
    per <- nl_periods_file(nl_city_newest(m$key, paste0("_", NL_CITY_TABLE, "_Perioden\\.json$")))
    years <- sort(unique(per$year[!is.na(per$year) & per$year >= m$since])); have <- nl_years_local_dir(dir, NL_CITY_TABLE)
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    for (y in years) {
      if (y %in% have && y < this_year - 1) next                       # the current and previous year are refreshed (late registrations)
      message("  ", NL_CITY_TABLE, " ", y, " (wijken of gemeente ", m$gm, ")")
      url <- sprintf("%s/%s/TypedDataSet?$format=json&$filter=startswith(WijkenEnBuurten,'WK%s')%%20and%%20startswith(Perioden,'%d')&$select=ID,SoortMisdrijf,WijkenEnBuurten,Perioden,GeregistreerdeMisdrijven_1",
                     NL_CITY_FEED, NL_CITY_TABLE, m$gm, y)
      nl_odata_pages(url, paste0(NL_CITY_TABLE, "_", y), dir = dir)
    }
  }
  if (!recent(sprintf("_wijken_%d_p01\\.geojson$", NL_CITY_WFS_YEAR), 365)) {
    k <- 0L
    repeat {
      f <- download_versioned(sprintf(NL_CITY_WFS, NL_CITY_WFS_YEAR, "wijken", k * NL_CITY_WFS_PAGE), dir, name = sprintf("wijken_%d_p%02d.geojson", NL_CITY_WFS_YEAR, k + 1L))
      n <- tryCatch(nrow(sf::st_read(f, quiet = TRUE)), error = function(e) 0L)
      k <- k + 1L; if (n < NL_CITY_WFS_PAGE || k > 20L) break
    }
    message("  wijken layer: ", k, " page(s) of the whole country")
  }
  if (!is.null(m$districts_download)) attempt(paste(m$name_en, "districts download"), m$districts_download(dir))
  invisible(TRUE)
}

# Perioden metadata of a table file: Key, Title, Status, year
nl_periods_file <- function(f) {
  if (is.na(f)) stop("no Perioden metadata downloaded")
  p <- nl_trim(as.data.frame(nl_json(f)$value, stringsAsFactors = FALSE))
  p$year <- suppressWarnings(as.integer(substr(p$Key, 1, 4))); p
}
nl_years_local_dir <- function(dir, table) sort(unique(as.integer(sub(paste0(".*_", table, "_([0-9]{4})_p[0-9]+\\.json$"), "\\1", list.files(dir, paste0("_", table, "_[0-9]{4}_p[0-9]+\\.json$"))))))

# --- Territory: wijken (neighbourhoods) and districts from the CBS Wijk- en buurtkaart -------------------------
nl_city_territory <- function(m, con, force = FALSE) {
  if (!force && city_neighbourhood_count(con, m$id) > 1 && file.exists(CITY_MODULES[[m$key]]$gpkg)) { message("= ", m$name_en, " territory kept (", city_neighbourhood_count(con, m$id), " wijken)"); return(invisible(FALSE)) }
  files <- list.files(nl_city_dir(m$key), "_wijken_[0-9]{4}_p[0-9]+\\.geojson$", full.names = TRUE)
  if (!length(files)) stop("no wijken boundaries in ", nl_city_dir(m$key), " (download: option 14 with ", m$code, ")")
  pg <- sub(".*_p([0-9]+)\\.geojson$", "\\1", files); files <- unname(vapply(split(files, pg), function(x) newest_first(x)[1], ""))   # newest copy of every page
  g <- do.call(rbind, lapply(files, function(x) sf::st_read(x, quiet = TRUE))); f <- files[1]; nm <- names(g)
  pick <- function(p) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) nm[i[1]] else NA_character_ }
  col_code <- pick("^wijkcode$|^statcode$"); col_name <- pick("^wijknaam$|^statnaam$"); col_pop <- pick("^aantal_?inwoners$"); col_water <- pick("^water$")
  if (is.na(col_code)) stop("wijken file: no wijkcode column; fields are ", paste(nm, collapse = ", "))
  message(sprintf("  wijken file %s: %d features, fields %s", basename(f), nrow(g), paste(utils::head(nm, 8), collapse = ", ")))
  if (!is.na(col_water)) g <- g[is.na(g[[col_water]]) | toupper(trimws(as.character(g[[col_water]]))) != "JA", ]   # water-only parts of a wijk are not places
  code <- trimws(as.character(g[[col_code]])); code <- ifelse(grepl("^WK", code), code, paste0("WK", code))
  g <- g[grepl(paste0("^WK", m$gm), code), ]; code <- code[grepl(paste0("^WK", m$gm), code)]
  if (nrow(g) == 0) stop("wijken files: no wijk of gemeente ", m$gm, " among ", length(nm), " fields and the pages downloaded; run nl_city_inspect('", m$key, "')")
  if (nrow(g) < 3) message("  only ", nrow(g), " wijk(en) of the gemeente in the files: run nl_city_inspect('", m$key, "')")
  if (!is.na(col_pop)) { pop <- suppressWarnings(as.numeric(g[[col_pop]])); pop[!is.na(pop) & pop < 0] <- NA } else pop <- rep(NA_real_, nrow(g))
  name <- if (!is.na(col_name)) as.character(g[[col_name]]) else code
  g <- sf::st_make_valid(sf::st_transform(g, 4326))
  # Districts: the module's mapping (wijk code -> district code and name) or the gemeente itself
  map <- if (!is.null(m$districts)) tryCatch(m$districts(nl_city_dir(m$key), code, name), error = function(e) { message("  districts not available (", conditionMessage(e), "): one district"); NULL }) else NULL
  if (is.null(map) || !all(code %in% map$wijk_code)) { if (!is.null(map)) message("  ", sum(!code %in% map$wijk_code), " wijken without a district: one district for the whole gemeente"); map <- data.frame(wijk_code = code, district_code = "00", district_name = m$name_local, stringsAsFactors = FALSE) }
  dcode <- map$district_code[match(code, map$wijk_code)]
  district_id <- paste0(m$id, "-D", dcode)
  neighbourhoods <- sf::st_sf(
    territory_id = paste0(district_id, "-B", sub("^WK", "", code)), level = "neighbourhood", parent_id = district_id, country = "NL",
    official_code = code, code_scheme = "CBS wijk", name = name, area_m2 = as.numeric(sf::st_area(g)), file = basename(f), geometry = sf::st_geometry(g))
  dis <- dissolve_territories(g, district_id)
  dname <- map$district_name[match(sub("^.*-D", "", dis$territory_id), map$district_code)]
  districts <- sf::st_sf(territory_id = dis$territory_id, level = "district", parent_id = m$id, country = "NL", official_code = sub("^.*-D", "", dis$territory_id),
                         code_scheme = if (is.null(m$districts)) "WebarchyMapp (whole gemeente)" else "stadsdeel", name = dname, area_m2 = as.numeric(sf::st_area(dis)), file = basename(f), geometry = sf::st_geometry(dis))
  upper <- data.frame(territory_id = m$id, level = "municipality", parent_id = sub("-[^-]+$", "", m$id), country = "NL", official_code = paste0("GM", m$gm), code_scheme = "CBS", name = m$name_local, stringsAsFactors = FALSE)
  save_city_territory(con, districts, neighbourhoods, upper, CITY_MODULES[[m$key]]$gpkg)
  NL_CITY_STATE[[m$key]] <- list(pop = data.frame(territory_id = neighbourhoods$territory_id, population = pop, stringsAsFactors = FALSE), file = basename(f))
  invisible(neighbourhoods)
}
NL_CITY_STATE <- new.env()

# --- Ingest: monthly rows by wijk and SoortMisdrijf -> the shared city tables --------------------------------
nl_city_ingest <- function(m, con) {
  dir <- nl_city_dir(m$key)
  years <- nl_years_local_dir(dir, NL_CITY_TABLE); if (!length(years)) stop("no ", NL_CITY_TABLE, " pages in ", dir, " (option 14 with ", m$code, ")")
  d <- do.call(rbind, lapply(years, function(y) nl_read_pages(paste0(NL_CITY_TABLE, "_", y), dir)))
  if (is.null(d) || nrow(d) == 0) stop("the downloaded pages hold no rows")
  nm <- names(d); col <- list(type = nl_pick(nm, "^SoortMisdrijf$"), region = nl_pick(nm, "^WijkenEnBuurten$"), period = nl_pick(nm, "^Perioden$"), n = nl_pick(nm, c("^GeregistreerdeMisdrijven", "^TotaalGeregistreerdeMisdrijven")))
  if (anyNA(unlist(col))) stop("unexpected columns in ", NL_CITY_TABLE, ": ", paste(nm, collapse = ", "), "\nRun nl_city_inspect('", m$key, "')")
  d <- data.frame(type = d[[col$type]], region = d[[col$region]], period = d[[col$period]], n = suppressWarnings(as.numeric(d[[col$n]])), stringsAsFactors = FALSE)
  d <- d[grepl("^WK", d$region) & grepl("^[0-9]{4}MM[0-9]{2}$", d$period) & !is.na(d$n), ]
  if (nrow(d) == 0) stop("no monthly wijk rows (Perioden like 2024MM01) in the pages")
  # Type titles from the SoortMisdrijf metadata, matched to reference/nl_city_event_types.csv by title
  tf <- nl_city_newest(m$key, paste0("_", NL_CITY_TABLE, "_SoortMisdrijf\\.json$")); if (is.na(tf)) stop("no SoortMisdrijf metadata downloaded")
  sm <- nl_trim(as.data.frame(nl_json(tf)$value, stringsAsFactors = FALSE)); title_of <- setNames(nl_city_title(sm$Title), sm$Key)
  types <- read_reference("nl_city_event_types.csv", bool_cols = "sensitive")
  d$title <- unname(title_of[d$type]); d$event_type <- types$source_code[match(normalize_text(d$title), normalize_text(types$source_code))]
  unknown <- unique(d$title[is.na(d$event_type)]); unknown <- unknown[!is.na(unknown)]
  agg <- unknown[grepl("totaal|misdrijven$|^[0-9]\\.[0-9]\\.0", unknown, ignore.case = TRUE)]
  if (length(setdiff(unknown, agg))) message("  SoortMisdrijf titles not in nl_city_event_types.csv (left out): ", paste(utils::head(setdiff(unknown, agg), 12), collapse = " / "))
  d <- d[!is.na(d$event_type), ]
  if (nrow(d) == 0) stop("no SoortMisdrijf title matches reference/nl_city_event_types.csv: run nl_city_inspect('", m$key, "') and add the titles")
  known <- DBI::dbGetQuery(con, paste0("SELECT territory_id, parent_id, official_code FROM dim_territory WHERE level = 'neighbourhood' AND territory_id LIKE ", DBI::dbQuoteString(con, paste0(m$id, "-%"))))
  d$neighbourhood_id <- known$territory_id[match(d$region, known$official_code)]; d$district_id <- known$parent_id[match(d$region, known$official_code)]
  lost <- unique(d$region[is.na(d$neighbourhood_id)]); if (length(lost)) message("  ", length(lost), " wijk codes not in the boundaries (left out): ", paste(utils::head(lost, 6), collapse = ", "))
  d <- d[!is.na(d$neighbourhood_id), ]
  if (nrow(d) == 0) stop("none of the wijk codes of the data is in the territory: rebuild the territory (option 7, or option 14 after moving the wijken files aside)")
  d$month <- as.Date(sprintf("%s-%s-01", substr(d$period, 1, 4), substr(d$period, 7, 8)))
  ev <- d |> dplyr::group_by(month, neighbourhood_id, district_id, event_type) |> dplyr::summarise(events = sum(n), .groups = "drop") |> as.data.frame()
  ev$city <- m$key; ev$file <- NL_CITY_TABLE; ev$events <- as.integer(round(ev$events))
  replace_city_rows(con, "stg_city_events", m$key, ev[, c("city", "month", "neighbourhood_id", "district_id", "event_type", "events", "file")])
  seen <- types[types$source_code %in% ev$event_type, ]
  t <- data.frame(city = m$key, event_type = seen$source_code, block = seen$block, name_local = seen$name_local, name_en = seen$name_en, sensitive = seen$sensitive, stringsAsFactors = FALSE)
  replace_city_rows(con, "ref_city_event_types", m$key, t)
  log_ingest(con, paste0("city_", m$key), NL_CITY_TABLE, NA, paste(format(range(ev$month), "%Y-%m"), collapse = " to "), nrow(ev), sum(ev$events))
  message(sprintf("  %s: %s event rows, %d months (%s to %s), %d wijken, %d types", m$key, fmt_int(nrow(ev)), length(unique(ev$month)), format(min(ev$month), "%m/%Y"), format(max(ev$month), "%m/%Y"), length(unique(ev$neighbourhood_id)), nrow(t)))
  invisible(ev)
}

# --- Population: aantal_inwoners of the CBS wijken layer (the edition's 1 January) ---------------------------
nl_city_population <- function(m, con) {
  st <- NL_CITY_STATE[[m$key]]
  if (is.null(st)) { nl_city_territory(m, con, force = TRUE); st <- NL_CITY_STATE[[m$key]] }
  pop <- st$pop[!is.na(st$pop$population), ]
  if (nrow(pop) == 0) stop("the wijken layer has no aantal_inwoners values")
  yr <- suppressWarnings(as.integer(sub(".*_([0-9]{4})\\.geojson$", "\\1", st$file))); if (is.na(yr)) yr <- NL_CITY_WFS_YEAR
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0("DELETE FROM ref_population WHERE territory_id LIKE ", DBI::dbQuoteString(con, paste0(m$id, "-%"))))
    DBI::dbAppendTable(con, "ref_population", data.frame(territory_id = pop$territory_id, ref_date = as.Date(sprintf("%d-01-01", yr)), population = as.integer(round(pop$population)), file = st$file))
  })
  message(sprintf("  population: %d wijken, CBS Wijk- en buurtkaart %d (%s residents)", nrow(pop), yr, fmt_int(sum(pop$population))))
  invisible(TRUE)
}

# A SoortMisdrijf title without its code prefix ("1.1.1 Diefstal/inbraak woning" -> "Diefstal/inbraak woning")
nl_city_title <- function(x) trimws(sub("^[0-9]+(\\.[0-9]+)*\\s+", "", x))

# What is on disk for a gemeente: files, the wijken layer (features and fields), the data pages (wijk codes seen)
# and the SoortMisdrijf titles without equivalence
nl_city_inspect <- function(key = "amsterdam") {
  dir <- nl_city_dir(key); cat("Folder:", dir, "\n"); print(list.files(dir))
  for (f in list.files(dir, "_wijken_[0-9]{4}(_p[0-9]+)?\\.geojson$", full.names = TRUE)) { g <- tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL)
    if (is.null(g)) { cat("\n", basename(f), "could not be read as a layer; first lines:\n"); print(readLines(f, n = 3, warn = FALSE)) } else {
      cat("\n", basename(f), ":", nrow(g), "features; fields:", paste(utils::head(names(g), 8), collapse = ", "), "; gemeentecodes e.g.", paste(utils::head(unique(g$gemeentecode), 4), collapse = ", "), "\n") } }
  pages <- nl_pages(paste0(NL_CITY_TABLE, "_", max(nl_years_local_dir(dir, NL_CITY_TABLE), 0)), dir)
  if (length(pages)) { v <- nl_json(pages[1])$value; if (NROW(v)) { v <- nl_trim(as.data.frame(v)); cat("\nfirst page of the latest year:", nrow(v), "rows; fields:", paste(names(v), collapse = ", "), "\n"); cat("wijk codes seen:", paste(utils::head(sort(unique(v$WijkenEnBuurten)), 10), collapse = ", "), "...\n") } }
  tf <- nl_city_newest(key, paste0("_", NL_CITY_TABLE, "_SoortMisdrijf\\.json$"))
  if (!is.na(tf)) { sm <- nl_trim(as.data.frame(nl_json(tf)$value, stringsAsFactors = FALSE)); types <- read_reference("nl_city_event_types.csv")
    miss <- sm$Title[!normalize_text(nl_city_title(sm$Title)) %in% normalize_text(types$source_code)]; cat("\nSoortMisdrijf titles (", nrow(sm), "):\n", sep = ""); print(sm[, c("Key", "Title")])
    cat("\nNot in reference/nl_city_event_types.csv (aggregates are expected here):\n"); print(miss) }
  fw <- newest_first(list.files(dir, "_ams_wijken\\.json$", full.names = TRUE))
  if (length(fw)) { j <- jsonlite::fromJSON(fw[1], simplifyVector = TRUE); x <- j[["_embedded"]][["wijken"]] %||% j; cat("\ngebieden wijken fields:", paste(names(as.data.frame(x)), collapse = ", "), "\n") }
  invisible(TRUE)
}

# --- The module of one gemeente -------------------------------------------------------------------------------
# code: menu code ("AMS"); key: folder and table key ("amsterdam"); gm: CBS gemeente code without "GM" ("0363");
# id: the municipality id of the NL country module (NL-<province>-GM<gm>); districts(dir, wijk_codes, wijk_names):
# optional, returns data.frame(wijk_code, district_code, district_name); districts_download(dir): optional
nl_city_module <- function(code, key, gm, name, id, since = 2019L, districts = NULL, districts_download = NULL, stats_level = "neighbourhood", notes = NULL) {
  m <- list(code = code, key = key, gm = gm, id = id, name_en = name, name_local = name, lang = "nl", country = "NL", since = as.integer(since),
            measure_kind = "offences", stats_level = stats_level, districts = districts, districts_download = districts_download,
            boundaries_attribution = sprintf("CBS Wijk- en buurtkaart %d (CC BY 4.0)", NL_CITY_WFS_YEAR),
            method = list(where = "CBS wijk where the crime was committed, in the wijk boundaries of the latest edition for every year", date = "Month the crime was registered by the police", revisions = "The whole series is republished every month; the two latest years are refreshed on every update"),
            source = list(name = "Registered crimes by wijk, monthly (Politie / CBS 47022NED)", name_local = "Geregistreerde misdrijven; soort misdrijf, wijk, buurt, maandcijfers (Politie / CBS)",
                          publisher = "Politie, through Centraal Bureau voor de Statistiek (dataderden.cbs.nl)", url = NL_CITY_PAGE, licence = "CC BY 4.0",
                          measure = paste0("Crimes registered by the police (misdrijven, attempts included) by month, type and CBS wijk of ", name, "; aggregates of the classification are left out and the total shown is the sum of the listed types. Traffic types are loaded but not shown by area.")),
            about_source_key = paste0("about_src_", key), about_measures_key = paste0("about_", key, "_measures"), about_territory_key = paste0("about_", key, "_territory"))
  m$download <- function() nl_city_download(m)
  m$territory <- function(con, force = FALSE) nl_city_territory(m, con, force)
  m$ingest <- function(con) nl_city_ingest(m, con)
  m$population <- function(con) nl_city_population(m, con)
  m
}

# --- Amsterdam: stadsdelen as districts through the city's "gebieden" API (DSO, api.data.amsterdam.nl) ---------
AMS_GEBIEDEN <- list(wijken = "https://api.data.amsterdam.nl/v1/gebieden/wijken/?_format=json&_pageSize=1000",
                     stadsdelen = "https://api.data.amsterdam.nl/v1/gebieden/stadsdelen/?_format=json&_pageSize=100")
ams_districts_download <- function(dir) {
  for (k in names(AMS_GEBIEDEN)) {
    prev <- newest_first(list.files(dir, paste0("_ams_", k, "\\.json$"), full.names = TRUE))
    if (length(prev) && difftime(Sys.time(), file.mtime(prev[1]), units = "days") < 365) next
    download_versioned(AMS_GEBIEDEN[[k]], dir, name = paste0("ams_", k, ".json"))
  }
}
# wijk -> stadsdeel: the API gives each wijk its CBS code and the stadsdeel it lies in
ams_districts <- function(dir, wijk_codes, wijk_names) {
  fw <- newest_first(list.files(dir, "_ams_wijken\\.json$", full.names = TRUE)); fs <- newest_first(list.files(dir, "_ams_stadsdelen\\.json$", full.names = TRUE))
  if (!length(fw) || !length(fs)) stop("no gebieden files")
  flat <- function(f, what) { j <- jsonlite::fromJSON(f, simplifyVector = TRUE); x <- j[["_embedded"]][[what]] %||% j[[what]] %||% j; as.data.frame(x, stringsAsFactors = FALSE) }
  w <- flat(fw[1], "wijken"); s <- flat(fs[1], "stadsdelen"); nm <- names(w)
  pick <- function(d, p) { i <- grep(p, names(d), ignore.case = TRUE); if (length(i)) names(d)[i[1]] else NA_character_ }
  c_cbs <- pick(w, "^cbs_?code$"); c_sd <- pick(w, "^ligtInStadsdeelId$|^ligt_in_stadsdeel_id$|stadsdeel.*id"); c_sid <- pick(s, "^identificatie$|^id$"); c_sname <- pick(s, "^naam$"); c_scode <- pick(s, "^code$")
  if (anyNA(c(c_cbs, c_sd, c_sid, c_sname))) stop("unexpected gebieden fields: wijken ", paste(nm, collapse = ", "), "; stadsdelen ", paste(names(s), collapse = ", "))
  cbs <- trimws(as.character(w[[c_cbs]])); cbs <- ifelse(grepl("^WK", cbs), cbs, paste0("WK", cbs))
  sd_id <- as.character(w[[c_sd]]); s_id <- as.character(s[[c_sid]])
  dcode <- if (!is.na(c_scode)) as.character(s[[c_scode]])[match(sd_id, s_id)] else sprintf("%02d", match(sd_id, s_id))
  data.frame(wijk_code = cbs, district_code = gsub("[^A-Za-z0-9]", "", dcode), district_name = as.character(s[[c_sname]])[match(sd_id, s_id)], stringsAsFactors = FALSE)
}

register_city(nl_city_module(code = "AMS", key = "amsterdam", gm = "0363", name = "Amsterdam", id = "NL-PV27-GM0363",
                             districts = ams_districts, districts_download = ams_districts_download))
