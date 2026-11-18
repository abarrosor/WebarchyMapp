# France: recorded crime by commune, département and région ------------------------------
# Source: SSMSI (Ministère de l'Intérieur), "Bases statistiques communale, départementale et
# régionale de la délinquance enregistrée par la police et la gendarmerie nationales",
# data.gouv.fr, Licence Ouverte 2.0. Yearly, 2016 onwards, 15 indicators (not a total).
# A commune x indicator is published only when it has more than 5 facts in each of three
# consecutive years ("est_diffuse" = "ndiff" otherwise): rural maps are mostly grey.
# Population (INSEE) comes inside the files. Boundaries: Eurostat GISCO LAU (communes);
# départements and régions are unions of their communes.
#
# Written against the published documentation (not run against the live files):
# the reader detects the columns by name, so small changes in the headers are tolerated.
# If a file does not match, `fr_inspect()` prints its header to adjust FR_COLUMNS.

FR_STATE <- new.env()   # population read from the files, handed from fr_ingest to fr_population
FR_RESOURCES <- c(
  communal     = "https://www.data.gouv.fr/api/1/datasets/r/44ef4323-1097-48d5-8719-3c544b55d294",   # CSV.GZ, ~38 MB
  departmental = "https://www.data.gouv.fr/api/1/datasets/r/2b27a675-e3bf-41ef-a852-5fb9ab483967",   # CSV
  regional     = "https://www.data.gouv.fr/api/1/datasets/r/75753852-aca0-4c53-bfd4-a29513358d2f"    # CSV
)
# Accepted names for every field the loader needs (first match wins, case-insensitive)
FR_COLUMNS <- list(
  code      = c("^CODGEO", "^Code\\.?_?d[ée]partement$", "^Code\\.?_?r[ée]gion$", "^code_departement$", "^code_region$", "^dep$", "^reg$"),
  year      = c("^annee$", "^ann[ée]e$", "^year$"),
  indicator = c("^indicateur$", "^classe$"),
  value     = c("^nombre$", "^faits$", "^valeur$"),
  diffused  = c("^est_diffuse$", "^valeur\\.publi[ée]e$", "^diffusion$"),
  pop       = c("^insee_pop$", "^POP$", "^population$"),
  name      = c("^libelle", "^nom_", "^LIBGEO")
)

fr_pick <- function(nm, patterns) { for (p in patterns) { i <- grep(p, nm, ignore.case = TRUE); if (length(i)) return(nm[i[1]]) }; NA_character_ }

fr_read <- function(f) {
  sep <- if (grepl("\\.gz$", f)) ";" else ";"
  first <- readLines(if (grepl("\\.gz$", f)) gzfile(f) else f, n = 1, warn = FALSE)
  sep <- if (lengths(regmatches(first, gregexpr(";", first))) >= lengths(regmatches(first, gregexpr(",", first)))) ";" else ","
  readr::read_delim(f, delim = sep, col_types = readr::cols(.default = "c"), locale = readr::locale(decimal_mark = ","), progress = FALSE, show_col_types = FALSE)
}

# Print the header of each downloaded file (to adjust FR_COLUMNS if the Ministry renames fields)
fr_inspect <- function() {
  for (lv in names(FR_RESOURCES)) { f <- fr_file(lv); if (is.na(f)) next; d <- fr_read(f); message(lv, ": ", basename(f), "\n  ", paste(names(d), collapse = " | ")); print(utils::head(as.data.frame(d), 3)) }
}
fr_file <- function(level) {
  f <- newest_first(list.files(COUNTRY_MODULES$FR$dir_raw, paste0("_fr_", level, "\\.csv(\\.gz)?$"), full.names = TRUE))
  if (length(f) == 0) NA_character_ else f[1]
}

fr_download <- function(days = 60) {
  m <- COUNTRY_MODULES$FR
  for (lv in names(FR_RESOURCES)) {
    prev <- fr_file(lv)
    if (!is.na(prev) && file_age_days(prev) < days) { message("= FR ", lv, " base downloaded less than ", days, " days ago"); next }
    name <- paste0("fr_", lv, if (lv == "communal") ".csv.gz" else ".csv")
    download_versioned(FR_RESOURCES[[lv]], m$dir_raw, name = name)
  }
}

# Département -> région table (INSEE codes), with names
fr_departments <- function() read_reference("fr_departments.csv")

# Standardise one base to: code, year, indicator, value, diffused, pop, name
fr_standardise <- function(d, level) {
  nm <- names(d); col <- lapply(FR_COLUMNS, fr_pick, nm = nm)
  if (is.na(col$code) || is.na(col$year) || is.na(col$indicator) || is.na(col$value))
    stop("Unexpected columns in the French ", level, " base: ", paste(nm, collapse = " | "), "\nRun fr_inspect() and adjust FR_COLUMNS in R/71_fr.R")
  out <- data.frame(code = trimws(d[[col$code]]), year = as.integer(d[[col$year]]), indicator = trimws(d[[col$indicator]]),
                    value = suppressWarnings(as.numeric(gsub(",", ".", d[[col$value]]))), stringsAsFactors = FALSE)
  out$year <- ifelse(!is.na(out$year) & out$year < 100, 2000L + out$year, out$year)          # "16" -> 2016 in the oldest files
  out$diffused <- if (!is.na(col$diffused)) !grepl("^n", tolower(d[[col$diffused]])) else TRUE
  out$pop <- if (!is.na(col$pop)) suppressWarnings(as.numeric(gsub("[^0-9.]", "", gsub(",", ".", d[[col$pop]])))) else NA_real_
  out$name <- if (!is.na(col$name)) d[[col$name]] else NA_character_
  out$value[!out$diffused] <- NA                                                              # suppressed: unknown, shown as "< threshold"
  out[!is.na(out$code) & !is.na(out$year), ]
}

# Identifiers: FR-<région>-<département>-<commune>
fr_ids <- function(code, level, deps) {
  dep_of_commune <- function(c) ifelse(substr(c, 1, 2) %in% c("97", "98"), substr(c, 1, 3), substr(c, 1, 2))
  if (level == "communal") { dep <- dep_of_commune(code); reg <- deps$region_code[match(dep, deps$department_code)]; paste("FR", reg, dep, code, sep = "-") }
  else if (level == "departmental") { dep <- code; reg <- deps$region_code[match(dep, deps$department_code)]; paste("FR", reg, dep, sep = "-") }
  else paste("FR", code, sep = "-")
}

fr_ingest <- function(con) {
  deps <- fr_departments()
  rows <- list(); pops <- list()
  for (lv in names(FR_RESOURCES)) {
    f <- fr_file(lv); if (is.na(f)) { message("  no FR ", lv, " file yet"); next }
    d <- fr_standardise(fr_read(f), lv)
    d <- d[d$indicator != "", ]
    d$territory_id <- fr_ids(d$code, lv, deps)
    d <- d[!is.na(d$territory_id) & !grepl("-NA-", d$territory_id), ]
    level <- c(communal = "municipality", departmental = "province", regional = "region")[[lv]]
    rows[[lv]] <- data.frame(country = "FR", territory_id = d$territory_id, level = level, offence_code = d$indicator, year = d$year, n = d$value, file = basename(f))
    if (lv == "communal") ensure_territories(con, "FR", d$territory_id, "municipality", sub("-[^-]+$", "", d$territory_id), d$name)
    p <- unique(d[!is.na(d$pop), c("territory_id", "year", "pop")])
    if (nrow(p)) pops[[lv]] <- data.frame(country = "FR", territory_id = p$territory_id, level = level, year = p$year, population = p$pop)
    message(sprintf("  %s: %s rows, %d territories, %d-%d", lv, fmt_int(nrow(d)), length(unique(d$territory_id)), min(d$year), max(d$year)))
  }
  if (length(rows) == 0) stop("No French base found in ", COUNTRY_MODULES$FR$dir_raw)
  crime <- do.call(rbind, rows)
  # "total" = sum of the published indicators (NOT an official total: see reference/fr_offence_types.csv)
  tot <- crime |> dplyr::group_by(country, territory_id, level, year, file) |> dplyr::summarise(n = if (all(is.na(n))) NA_real_ else sum(n, na.rm = TRUE), .groups = "drop")
  tot$offence_code <- "TOTAL_INDICATEURS"
  crime <- yearly_to_cumulative(rbind(crime, tot[, names(crime)]))
  # National figures: sum of the régions
  nat <- crime[crime$level == "region", ] |> dplyr::group_by(country, offence_code, year, quarter, file) |> dplyr::summarise(cumulative = if (all(is.na(cumulative))) NA_real_ else sum(cumulative, na.rm = TRUE), .groups = "drop")
  nat$territory_id <- "FR"; nat$level <- "country"
  crime <- rbind(crime, nat[, names(crime)])
  replace_country_rows(con, "stg_country_crime", "FR", crime[, c("country", "territory_id", "level", "offence_code", "year", "quarter", "cumulative", "file")])
  log_ingest(con, "country_FR", paste(basename(unlist(lapply(names(FR_RESOURCES), fr_file))), collapse = ";"), NA, paste(range(crime$year), collapse = "-"), nrow(crime))
  # Population carried in the files (INSEE), plus national = sum of régions
  if (length(pops)) {
    pop <- do.call(rbind, pops)
    natp <- pop[pop$level == "region", ] |> dplyr::group_by(country, year) |> dplyr::summarise(population = sum(population), .groups = "drop")
    natp$territory_id <- "FR"; natp$level <- "country"
    pop <- unique(rbind(pop, natp[, names(pop)]))
    FR_STATE$pop <- pop
  }
  # Names of communes seen in the base (completed by the LAU names in fr_territory)
  invisible(crime)
}

fr_population <- function(con) {
  pop <- FR_STATE$pop
  if (is.null(pop)) { fr_ingest(con); pop <- FR_STATE$pop }
  if (is.null(pop)) stop("No population column found in the French files")
  replace_country_rows(con, "ref_country_population", "FR", pop)
  message(sprintf("  population: %d territories, %d-%d (INSEE, inside the SSMSI files)", length(unique(pop$territory_id)), min(pop$year), max(pop$year)))
}

# Territories: régions and départements from reference/fr_departments.csv; communes from GISCO LAU
fr_territory <- function(con) {
  deps <- fr_departments()
  regs <- unique(deps[, c("region_code", "region_name")])
  tbl <- rbind(
    data.frame(territory_id = paste0("FR-", regs$region_code), level = "region", parent_id = "FR", official_code = regs$region_code, name = regs$region_name),
    data.frame(territory_id = paste("FR", deps$region_code, deps$department_code, sep = "-"), level = "province", parent_id = paste0("FR-", deps$region_code),
               official_code = deps$department_code, name = deps$department_name))
  lau <- tryCatch(gisco_lau("FR"), error = function(e) { message("  GISCO LAU not available: communes without boundaries (", conditionMessage(e), ")"); NULL })
  layers <- list()
  if (!is.null(lau)) {
    code <- lau$LAU_ID; name <- lau$LAU_NAME
    dep <- ifelse(substr(code, 1, 2) %in% c("97", "98"), substr(code, 1, 3), substr(code, 1, 2))
    reg <- deps$region_code[match(dep, deps$department_code)]
    ok <- !is.na(reg)
    communes <- data.frame(territory_id = paste("FR", reg[ok], dep[ok], code[ok], sep = "-"), level = "municipality",
                           parent_id = paste("FR", reg[ok], dep[ok], sep = "-"), official_code = code[ok], name = name[ok])
    tbl <- rbind(tbl, communes)
    g <- lau[ok, ]; g$territory_id <- communes$territory_id; g$name <- communes$name
    g <- g[, c("territory_id", "name", attr(g, "sf_column"))]
    message("  building département and région polygons from the communes (a few minutes the first time)")
    gp <- g; gp$territory_id <- communes$parent_id; gp <- gp |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop"); gp$name <- deps$department_name[match(sub("^FR-[^-]+-", "", gp$territory_id), deps$department_code)]
    gr <- gp; gr$territory_id <- sub("-[^-]+$", "", gr$territory_id); gr <- gr |> dplyr::group_by(territory_id) |> dplyr::summarise(.groups = "drop"); gr$name <- regs$region_name[match(sub("^FR-", "", gr$territory_id), regs$region_code)]
    layers <- list(region = gr, province = gp, municipality = g)
  }
  save_country_territory(con, "FR", tbl, scheme = "INSEE")
  if (length(layers)) save_country_gpkg(con, "FR", layers, tolerance_m = 100)
  invisible(tbl)
}

register_country(list(
  code = "FR", iso3 = "FRA", name_en = "France", name_local = "France", lang = "fr",
  levels = c("region", "province", "municipality"),
  level_labels = list(region = c("Regions", "Régions", "Region", "Région"), province = c("Departments", "Départements", "Department", "Département"),
                      municipality = c("Communes", "Communes", "Commune", "Commune")),
  source = list(name = "Bases statistiques de la délinquance enregistrée (SSMSI)", publisher = "Ministère de l'Intérieur, SSMSI",
                url = "https://www.data.gouv.fr/datasets/bases-statistiques-communale-departementale-et-regionale-de-la-delinquance-enregistree-par-la-police-et-la-gendarmerie-nationales",
                licence = "Licence Ouverte 2.0", measure = "Offences recorded by the Police nationale and the Gendarmerie nationale, yearly, 15 indicators (no official total: the total shown is their sum)."),
  population_source = "INSEE (inside the SSMSI files)", boundaries_attribution = "© EuroGeographics for the administrative boundaries (Eurostat GISCO LAU)",
  not_published = c("Not published: 5 facts or fewer in one of the last three years (SSMSI rule)", "Non diffusé : 5 faits ou moins une des trois dernières années (règle SSMSI)"),
  method = list(where = "Commune where the offence was committed (lieu de commission)", date = "Year of recording by police or gendarmerie", revisions = "A new annual base replaces the previous one each year"),
  notes_en = "Communes with 5 facts or fewer in any of three consecutive years are not published; violence indicators were redefined in 2025.",
  default_region = "FR-11",
  download = fr_download, ingest = fr_ingest, territory = fr_territory, population = fr_population
))
