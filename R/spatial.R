#' @export
haversine_distance <- function(lat1, lon1, lat2, lon2)
{
  p <- 0.017453292519943295
  a <- 0.5 - cos((lat2 - lat1) * p)/2 + cos(lat1 * p) * cos(lat2 * p) *
    (1 - cos((lon2 - lon1) * p)) / 2

  return (12742 * asin(sqrt(a)))
}


#' @export
match_blank_zip_to_polygon_coords <- function(
  coords, # data.frame w/ cols "id", "lat", "lon" (leave missing to get from DB)
  update_db = FALSE,
  verbose = TRUE
)
{
  ## Load object 'zip_code_data'.
  load(system.file("extdata/zip-codes+demo.RData", package = "gva", mustWork = TRUE))

  zip_coords <- zip_code_data$zip_code_database_enterprise %>%
    dplyr::select(zip, approximate_latitude, approximate_longitude) %>%
    dplyr::rename(lat = 2, lon = 3)

  if (missing(coords)) {
    get_sql <- "SELECT * FROM geolocation where zip IS NULL OR TRIM(zip) = ''"
    conn <- gva_connect_db()
    flit <- DBI::dbGetQuery(conn, get_sql)
    DBI::dbDisconnect(conn)

    coords <- flit %>% dplyr::select(1:4) %>% tibble::as_tibble() %>%
      dplyr::rename(id = 1)
  }

  if (NROW(coords) == 0) {
    warning("No missing ZIP Codes in 'geolocation' table", immediate. = TRUE)

    return (coords)
  }

  if (verbose)
    { coords %>% print; utils::flush.console() }

  ### Use the US ZIP Codes shapefile to perform a spatial join w/ coordinates
  ### V. https://stackoverflow.com/questions/42337619/how-to-batch-reverse-geocode-in-r/42339199#42339199

  ## Import ZIP Code shapefile and transform CRS:
  # tl_2022_us_zcta520 <- rgdal::readOGR("./Downloads/geospatial/tl_2022_us_zcta520/tl_2022_us_zcta520.shp") %>%
  #   sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  # save(tl_2022_us_zcta520, file = system.file("inst/extdata/tl_2022_us_zcta520.RData", package = "gva"))

  load(system.file("extdata/sub-rosa/tl_2022_us_zcta520.RData", package = "gva"))

  ## Transform coordinates into 'SpatialPointsDataFrame'
  spdf <- sp::SpatialPointsDataFrame(coords = coords %>% dplyr::select(lon, lat), data = coords,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

  ## Keep only ZIP Codes where points are located
  zips_subset <- tl_2022_us_zcta520[spdf, ]

  ## N.B. The ZIP Code column in 'tl_2022_us_zcta520' is "ZCTA5CE20"
  ## Use 'sp::over()' to overlay coordinate points in polygons:
  coords <- coords %>%
    dplyr::mutate(zip = sp::over(x = spdf, y = zips_subset[, "ZCTA5CE20"]) %>% dplyr::pull())

  ## Check for missing ZIPs
  if (verbose)
    { coords %>% dplyr::filter(is.na(zip) | trimws(zip) == "") %>% print; utils::flush.console() }

  ## Brute-force the stragglers using Haversine distance
  flit <- coords %>% dplyr::filter(is.na(zip) | trimws(zip) == "")
  if (NROW(flit != 0)) {
    flit <- flit %>%
      dplyr::mutate(
        zip =
          plyr::alply(., 1,
            function(a)
            {
              #print(a); utils::flush.console()
              plyr::alply(zip_coords, 1,
                function(b)
                {
                  haversine_distance(a["lat"], a["lon"], b["lat"], b["lon"])
                })  %>% unlist(use.names = FALSE) %>% which.min %>% `[`(zip_coords$zip, .)
            }, .parallel = TRUE) %>% unlist(use.names = FALSE)
      )
  }
  if (verbose) print(flit %>% as.data.frame)
  coords <- dplyr::rows_patch(coords, flit %>% dplyr::select(id, zip))

  if (update_db) {
    if (verbose) { cat(sprintf("Updating %d geolocation records in DB...", NROW(coords)))}

    patch <- coords %>% dplyr::select(id, zip) %>% dplyr::rename(incident_id = id)
    conn <- gva_connect_db()
    dbx::dbxUpdate(conn, "geolocation", patch, where_cols = c("incident_id"))
    DBI::dbDisconnect(conn)

    if (verbose) { cat(". Done.", fill = TRUE); utils::flush.console() }
  }

  #browser()
  coords
}
