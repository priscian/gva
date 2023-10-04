# gva
Retrieve & analyze data from the Gun Violence Archive (GVA)

## Using *gva*
Here's how to set up and test-populate a local GVA database.

```r
## Prelims.
options(keystone_parallel = TRUE)
library(gva)
#keystone::reload_all("gva", redocument = FALSE)

data_dir <- tempdir(check = TRUE)
setwd(data_dir)

### Test the code on a subset of all GVA data.

## Create the database.
gva_make_new_db(overwrite = TRUE)

## Get some test data for incidents & their participants. Two days only:
download_dates <- rbind(
  keystone::dataframe(lubridate::dmy("15 Nov 2019"), lubridate::dmy("15 Nov 2019")),
  keystone::dataframe(lubridate::dmy("1 Dec 2019"), lubridate::dmy("1 Dec 2019"))
) %>% `colnames<-`(c("start_date", "end_date"))

## Connect to most recent GVA DB in the current directory:
conn <- gva_connect_db()
gva_augment_db(conn, download_dates)
DBI::dbDisconnect(conn)

ids <- gva_get_missing_geolocation_ids()
ids %>% length %>% print

## Set global option to auto-fill Google API key, e.g.
##   options(google_api_key = "your-key-here"), or set it directly:
google_api_key <- getOption("google_api_key")

## Update geolocations for all incidents in DB.
g <- gva_update_missing_geolocations_google(google_api_key = google_api_key)
g %>% print

## Examine data.
get_locations_sql <-
"SELECT incidents.id, incidents.date, incidents.city_or_county,
  incidents.state, geolocation.lat, geolocation.lon, geolocation.zip
  FROM incidents
INNER JOIN geolocation
  ON incidents.id = geolocation.incident_id
ORDER BY id;"
conn <- gva_connect_db()
ll <- DBI::dbGetQuery(conn, get_locations_sql) %>%
  dplyr::mutate(
    #id = seq(NROW(.)),
    date = lubridate::as_date(date)
  ) %>%
  dplyr::rename(city = city_or_county) %>%
  #naniar::replace_with_na(replace = list(address = "N/A")) %>%
  tibble::as_tibble()
DBI::dbDisconnect(conn)

ll %>% print
# # A tibble: 273 × 7
#         id date       city          state            lat   lon zip
#      <int> <date>     <chr>         <chr>          <dbl> <dbl> <chr>
#  1 1549559 2019-11-15 Chicago       Illinois        41.9 -87.8 60644
#  2 1549561 2019-11-15 Pinellas Park Florida         27.8 -82.7 33781
#  3 1549601 2019-11-15 Clarkston     Georgia         33.8 -84.2 30021
#  4 1549654 2019-11-15 Philadelphia  Pennsylvania    40.0 -75.1 19149
#  5 1549664 2019-11-15 Houston       Texas           29.7 -95.3 <NA>
#  6 1549681 2019-11-15 Spring        Texas           30.1 -95.4 77386
#  7 1549735 2019-11-15 Charlotte     North Carolina  35.2 -80.8 28202
#  8 1549759 2019-11-15 Westborough   Massachusetts   42.3 -71.6 01581
#  9 1549803 2019-11-15 Lexington     Kentucky        38.1 -84.5 40505
# 10 1549877 2019-11-15 Independence  Missouri        39.1 -94.5 64052
# # … with 263 more rows

## Create full GVA database: make new DB file & populate it.
## (N.B. Don't do this as a test!)
# gva_create_full_db()

```

```sql
--- Transferring geolocation data from an older version of the DB

-- You might need to recreate the full database at some point;
--   if so, transfer the 'geolocation' table w/ the following SQL,
--   after loading the older DB (e.g. in DB Browser for SQLite):
ATTACH DATABASE './gva_2023-10-02.db' AS other; -- Attach newer DB
PRAGMA foreign_keys = 0; -- Bypass "FOREIGN KEY constraint failed" error
INSERT INTO other.geolocation
  SELECT * FROM main.geolocation;
PRAGMA foreign_keys = 1;
DETACH other;
```


Once you've set up a complete database, you might want to update it every week or month or so.

```r
## Update DB & add any failed geolocations by hand.
## Use https://www.gunviolencearchive.org/incident/[incident_id] to get geolocation.
options(keystone_parallel = TRUE)
library(gva)
#keystone::reload_all("gva", redocument = FALSE)

data_dir <- getOption("gva_db_location")
if (is.null(data_dir)) data_dir <- "."
setwd(data_dir)

## Update DB.
gva_update_db()
ids <- gva_get_missing_geolocation_ids()
ids %>% length %>% print
## Update up to 2500 geolocations per day (the free Google limit):
g <- gva_update_missing_geolocations_google()
print(date())
g %>% print
## Fill in missing 'geolocation' table ZIP codes by cruder searches:
match_blank_zip_to_polygon_coords(update_db = TRUE)

### N.B. Most updates will end here, but see some error corrections below.

## Update failed geolocations by hand, e.g.:
ge <- read.table(text = '
  incident_id lat      lon      zip
  2011371     35.9696  -79.9895 NA
  2018501     35.6527  -80.5006 NA
  2028811     38.3498  -81.6326 NA
', header = TRUE, colClasses = c(zip = "character")) %>%
dplyr::filter(!is.na(lat) & !is.na(lon))
ge %>% print

conn <- gva_connect_db()
dbx::dbxInsert(conn, "geolocation", ge)
DBI::dbDisconnect(conn)

### Update existing records.

## Check record
get_sql <- "SELECT * FROM geolocation"
conn <- gva_connect_db()
a0 <- DBI::dbGetQuery(conn, get_sql)
DBI::dbDisconnect(conn)
a <- a0 %>% dplyr::filter(incident_id == "2403713")
a %>% print

## Update record in DB (e.g. ZIP + 4 to ZIP; was 890156028)
gu <- read.table(text = '
  incident_id zip
  2403713     89015
', header = TRUE, colClasses = c(zip = "character"))
gu %>% print

conn <- gva_connect_db()
dbx::dbxUpdate(conn, "geolocation", gu, where_cols = c("incident_id"))
DBI::dbDisconnect(conn)

## Check record again
get_sql <- "SELECT * FROM geolocation"
conn <- gva_connect_db()
b0 <- DBI::dbGetQuery(conn, get_sql)
DBI::dbDisconnect(conn)
b <- b0 %>% dplyr::filter(incident_id == "2403713")
b %>% print
```
