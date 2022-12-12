## Create address data to use w/ ArcGIS.
#' @export
gva_make_arcgis_address_file <- function(
  file_dir = ".",
  base_filename = "gva_addresses",
  ids = NULL # Can use 'gva_get_missing_geolocation_ids()' here.
)
{
  get_addresses_sql <- "SELECT id, address, city_or_county, state FROM incidents"
  conn <- gva_connect_db()
  a0 <- DBI::dbGetQuery(conn, get_addresses_sql)
  DBI::dbDisconnect(conn)

  if (!is.null(ids))
    a <- a0 %>% dplyr::filter(id %in% ids)
  else
    a <- a0

  a <- a %>%
    naniar::replace_with_na(replace = list(address = "N/A")) %>%
    dplyr::mutate(full_address = paste(ifelse(is.na(address), "", address), city_or_county, state, sep = ", ")) %>%
    dplyr::mutate(full_address = stringr::str_replace(full_address, "^\\s*,\\s*", ""))

  g <- a %>% dplyr::select(id, full_address)

  rio::export(g, sprintf("%s/%s_%s.csv", file_dir, base_filename,
    keystone::make_current_timestamp(fmt = "%Y%m%d", use_seconds = FALSE)))
}

## usage:
# gva_make_arcgis_address_file()


#' @export
gva_process_arcgis_geocodes <- function(
  geocodes_path,
  write_table = TRUE,
  ...
)
{
  g0 <- rio::import(geocodes_path)

  g <- g0 %>%
    dplyr::transmute(
      incident_id = as.integer(USER_id),
      lat = Y,
      lon = X,
      zip = dplyr::case_when(!is.na(Postal) ~ sprintf("%05d", Postal), TRUE ~ NA_character_)
    ) %>%
    dplyr::filter(!(lon == 0 & lat == 0)) # These are (US) missings from ArcGIS

  if (write_table) {
    conn <- gva_connect_db()
    dbx::dbxInsert(conn, "geolocation", g, ...)
    DBI::dbDisconnect(conn)

    return (g0)
  }

  return (g)
}

## usage:
# g0 <- process_arcgis_geocodes("./data/gva_addresses_geo_20210129.csv")
# g <- process_arcgis_geocodes("./data/gva_addresses_geo_20210129.csv", write_table = FALSE)
