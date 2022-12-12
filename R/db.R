#' @export
gva_make_new_db <- function(
  data_dir = ".",
  base_name = "gva",
  suffix = keystone::make_current_timestamp(use_seconds = FALSE),
  overwrite = FALSE
)
{
  dbPath <- paste(data_dir, paste(base_name, suffix, sep = "_") %_% ".db", sep = "/")
  if (file.exists(dbPath)) {
    if (overwrite)
      file.remove(dbPath)
    else
      stop("Must set 'overwrite = TRUE' to replace an existing DB.")
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), dbPath)

  create_incidents_table_sql <-
    "CREATE TABLE incidents(
      id INTEGER PRIMARY KEY,
      date DATETIME,
      state TEXT,
      city_or_county TEXT,
      address TEXT,
      num_killed INTEGER,
      num_injured INTEGER,
      source TEXT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
    );"

  DBI::dbExecute(conn, create_incidents_table_sql)

  create_participants_table_sql <-
    "CREATE TABLE participants(
      incident_id INTEGER,
      gender TEXT,
      name TEXT,
      age_group TEXT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (incident_id) REFERENCES incidents(id)
    );"

  DBI::dbExecute(conn, create_participants_table_sql)

  create_geolocation_table_sql <-
    "CREATE TABLE geolocation(
      incident_id INTEGER,
      lat REAL,
      lon REAL,
      zip TEXT,
      timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (incident_id) REFERENCES incidents(id)
    );"

  DBI::dbExecute(conn, create_geolocation_table_sql)

  DBI::dbDisconnect(conn)
  #unlink(dbPath)

  dbPath
}


#' @export
gva_connect_db <- function(
<<<<<<< HEAD
  data_dir = getOption("gva_db_location"),
=======
  data_dir = ".",
>>>>>>> 1913027bcf308ac3e78fc84684c1af7bf6597b21
  file_path = "gva.db",
  timestamp... = list()
)
{
<<<<<<< HEAD
  if (is.null(data_dir))
    data_dir <- "."

=======
>>>>>>> 1913027bcf308ac3e78fc84684c1af7bf6597b21
  fileExt <- tools::file_ext(file_path)

  timestampArgs <- list(use_seconds = TRUE, seconds_sep = "+")
  timestampArgs <- utils::modifyList(timestampArgs, timestamp..., keep.null = TRUE)

  timestampRe <- "_\\d{4}-\\d{2}-\\d{2}(?:\\" %_% timestampArgs$seconds_sep %_% "\\d{5})?"
  filePaths <- sort(grep("^.*?" %_% timestampRe %_% "\\." %_% fileExt %_% "$",
    list.files(data_dir, pattern = "^" %_% Hmisc::escapeRegex(tools::file_path_sans_ext(basename(file_path)))
      %_% timestampRe %_% "\\." %_% fileExt %_% "$", full.names = FALSE),
    perl = TRUE, value = TRUE), decreasing = TRUE)
  filePaths <- paste(data_dir, filePaths, sep = "/")
  if (length(filePaths) > 0L)
    filePath <- filePaths[1L]

  conn <- DBI::dbConnect(RSQLite::SQLite(), filePath)

  conn
}


#' @export
gva_create_full_db <- function(overwrite = FALSE)
{
  ## Dates for full download
  full_download_dates <- seq(ymd("2013-01-01"), today() - 2, by = "day")
  full_download_range <- rbind(
    keystone::dataframe(ymd("1969-01-01"), ymd("2012-12-31")),
    keystone::dataframe(full_download_dates, full_download_dates)
  ) %>% (function(x) { colnames(x) <- c("start_date", "end_date"); x })

  gva_make_new_db(overwrite = overwrite)

  conn <- gva_connect_db()
  gva_augment_db(conn, full_download_range)
  DBI::dbDisconnect(conn)

  keystone::nop()
}


## N.B. This is a utility function to be called by other functions.
#' @export
gva_augment_db <- function(conn, download_range, .progress = "none")
{
  plyr::a_ply(download_range, 1,
    function(a)
    {
      startDate <- format(lubridate::ymd(a$start_date), "%m/%d/%Y")
      endDate <- format(lubridate::ymd(a$end_date), "%m/%d/%Y")

      cat(sprintf("Downloading GVA records %s to %s...", startDate, endDate))

      incidents <- gva_retrieve_by_date(startDate, endDate, "incidents")
      participants <- gva_retrieve_by_date(startDate, endDate, "participants")

      if (!is_invalid(incidents)) {
        records <- dbx::dbxUpsert(conn, "incidents", incidents, where_cols = c("id"))
      }
      if (!is_invalid(participants)) {
        dbx::dbxInsert(conn, "participants", participants[, c("incident_id", "gender", "name", "age_group")])
      }

      cat(". Done.", fill = TRUE); flush.console()
    },
  .progress = .progress) # '.progress = "text"' or '.progress = "none"'
}


#' @export
gva_update_db <- function()
{
  conn <- gva_connect_db()

  latest_incident_date_sql <-
    "select max(date) from incidents;"

  res <- DBI::dbSendQuery(conn, latest_incident_date_sql)
  maxDate <- DBI::dbFetch(res)
  DBI::dbClearResult(res)
  startDate <- format(lubridate::ymd(maxDate %>% unlist %>% stringr::str_extract("^.*?\\s") %>% trimws) + 1, "%m/%d/%Y")
  endDate <- format(lubridate::ymd(lubridate::today() - 2), "%m/%d/%Y")

  if (lubridate::mdy(startDate) > lubridate::mdy(endDate)) {
    cat("No update necessary.\n")

    return (nop())
  }

  partial_download_dates <- seq(lubridate::mdy(startDate), lubridate::mdy(endDate), by = "day")
  partial_download_range <- rbind(
    ## Add any other individual ranges here (probably not necessary).
    keystone::dataframe(partial_download_dates, partial_download_dates)
  ) %>% (function(x) { colnames(x) <- c("start_date", "end_date"); x })

  gva_augment_db(conn, partial_download_range)

  DBI::dbDisconnect(conn)
}
