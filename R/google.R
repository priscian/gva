#' @export
gva_update_missing_geolocations_google <- function(
  max_requests = 2500,
  chunk_size = 50,
  verbose = TRUE,
  ## Set global option to auto-fill Google API key, e.g. options(google_api_key = "your-key-here"):
  google_api_key = getOption("google_api_key")
)
{
  ids <- gva_get_missing_geolocation_ids()
  if (!is.null(max_requests) && length(ids) > max_requests)
    ids <- head(ids, max_requests)

  if (verbose) {
    cat(sprintf("Updating %i geolocations ...", length(ids)), fill = TRUE)
  }

  get_addresses_sql <- "SELECT id, address, city_or_county, state FROM incidents"
  conn <- gva_connect_db()
  a0 <- DBI::dbGetQuery(conn, get_addresses_sql)
  DBI::dbDisconnect(conn)

  a <- a0 %>% dplyr::filter(id %in% ids) %>%
    naniar::replace_with_na(replace = list(address = "N/A")) %>%
    dplyr::mutate(full_address = paste(ifelse(is.na(address), "", address), city_or_county, state, sep = ", ")) %>%
    dplyr::mutate(full_address = stringr::str_replace(full_address, "^\\s*,\\s*", "")) %>%
    dplyr::mutate(full_address = stringr::str_replace(full_address, "#", "")) # Octothorpes may cause geolocation failures

  ## Set up Google Geocoding interface.
  if (is.null(google_api_key))
    stop("Need Google API key to continue.")

  ggmap::register_google(key = google_api_key, account_type = "standard")
  ggmap::ggmap_show_api_key()

  ac <- keystone::chunk(a, chunk_size)

  ## Geocode each chunk of addresses & save the results to the DB.
  g <- sapply(ac,
    function(x)
    {
      g0 <- plyr::adply(x, 1,
        function(y)
        {
          #r0 <- ggmap::geocode(y$full_address, output = "latlon", source = "google", override_limit = TRUE)
          r0 <- ggmap::geocode(y$full_address, output = "all", source = "google", override_limit = TRUE)
          r <- keystone::dataframe(
            incident_id = y$id,
            lat = NA_real_,
            lon = NA_real_,
            zip = NA_character_
          )
          ## This is hacky but appears to work always; if Google can't find address, returns a tibble of NAs:
          if (!is.data.frame(r0)) {
            r$lat <- r0$results[[1]]$geometry$location$lat
            r$lon <- r0$results[[1]]$geometry$location$lng
            index <- (r0$results[[1]]$address_components %>%
              sapply(function(a) a$types[[1]], simplify = TRUE) == "postal_code") %>%
              which
            if (!is_invalid(index))
              r$zip <- r0$results[[1]]$address_components[[index]]$short_name
          } else {
            warning(sprintf("Address for Incident %s not found", r$incident_id), immediate. = FALSE)
          }

          utils::flush.console()

          r
        }, .expand = FALSE, .id = NULL)

      g <- g0 %>% dplyr::filter(!is.na(lat) & !is.na(lon))

      ## Write results to DB:
      conn <- gva_connect_db()
      dbx::dbxInsert(conn, "geolocation", g)
      DBI::dbDisconnect(conn)

      g
    }, simplify = FALSE)

  Reduce(rbind, g)
}

## usage:
# neo_geos <- gva_update_missing_geolocations_google(max_requests = 1000, chunk_size = 100)
# neo_geos <- gva_update_missing_geolocations_google(max_requests = 40000, chunk_size = 10)
