# Municipal, provincial and regional boundaries of Spain --------------------------
# Source: IGN - CartoBase ANE (scale 1:3,000,000), CC BY 4.0 ("CartoBase ANE
# CC BY 4.0 ign.es"), through the copy published by the open-source mapSpain
# project (github.com/rOpenSpain/mapSpain, branch sianedata). It carries every
# municipality with its INE code (id_ine) and history (fecha_alta, fecha_baja).
# Two files: "_x" (mainland, Balearic Islands, Ceuta, Melilla) and "_y" (Canary Islands).
#
# Output: data/territory_es.gpkg (written by the Spain module, R/71_es.R, through the
# shared save_country_gpkg()) with layers "municipality", "province" and "region"
# (provinces and regions are unions of their municipalities), in WGS84, with the
# project identifiers (ES-13-28-28079, ES-13-28, ES-13).


municipal_boundaries_download <- function(days = 365) {
  src <- CONFIG$sources$municipal_boundaries
  out <- character()
  for (u in c(es_source("ign_boundaries_mainland"), es_source("ign_boundaries_canary"))) {
    name <- basename(u)
    previous <- newest_first(list.files(src$dir_raw, paste0(name, "$"), full.names = TRUE))
    # Boundaries change very little: checked once a year
    if (length(previous) > 0 && file_age_days(previous[1]) < days) {
      message("= Boundaries already downloaded (", basename(previous[1]), ")")
      out <- c(out, previous[1]); next
    }
    out <- c(out, download_versioned(u, src$dir_raw))
  }
  invisible(out)
}

municipal_boundaries_files <- function() {
  dir <- CONFIG$sources$municipal_boundaries$dir_raw
  unlist(lapply(c("admin_muni_a_x\\.gpkg$", "admin_muni_a_y\\.gpkg$"), function(p) {
    f <- newest_first(list.files(dir, p, full.names = TRUE)); if (length(f) > 0) f[1] else character()
  }))
}

municipal_boundaries_build <- function() {
  files <- municipal_boundaries_files()
  if (length(files) == 0) stop("No boundary file in ", CONFIG$sources$municipal_boundaries$dir_raw)
  ref <- read_reference("es_provinces.csv")
  parts <- lapply(files, function(f) {
    m <- sf::st_read(f, quiet = TRUE)
    missing <- setdiff(c("id_ine", "rotulo", "fecha_baja"), names(m))
    if (length(missing) > 0) stop(basename(f), ": missing columns ", toString(missing))
    m <- m[is.na(m$fecha_baja), ]                         # current municipalities only
    m$region <- ref$region_code[match(substr(m$id_ine, 1, 2), ref$province_code)]
    m <- m[!is.na(m$region), ]
    m <- sf::st_transform(sf::st_make_valid(m), 4326)
    prov <- substr(m$id_ine, 1, 2)
    sf::st_sf(territory_id = paste0("ES-", m$region, "-", prov, "-", m$id_ine),
              name = m$rotulo, province_id = paste0("ES-", m$region, "-", prov),
              region_id = paste0("ES-", m$region), file = basename(f), geometry = sf::st_geometry(m))
  })
  mun <- do.call(rbind, parts)
  mun <- mun[!duplicated(mun$territory_id), ]
  if (nrow(mun) < 8000) warning("Only ", nrow(mun), " municipalities in the boundary files: check the download")
  message("  Building province and region outlines (this takes a minute)...")
  prov <- mun |> dplyr::group_by(territory_id = province_id, region_id) |> dplyr::summarise(.groups = "drop")
  prov$name <- ref$province_name[match(sub("^.*-", "", prov$territory_id), ref$province_code)]
  reg <- mun |> dplyr::group_by(territory_id = region_id) |> dplyr::summarise(.groups = "drop")
  reg$name <- ref$region_name[match(sub("^ES-", "", reg$territory_id), ref$region_code)]
  reg <- reg[!duplicated(reg$territory_id), ]
  list(municipalities = mun[, c("territory_id", "name", "province_id", "region_id", "file")],
       provinces = prov[, c("territory_id", "name", "region_id")], regions = reg[, c("territory_id", "name")])
}
