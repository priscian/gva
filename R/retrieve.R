#' @export
gva_retrieve_by_date <- function(
  start_date,
  end_date,
  results_type,
  query_uri = "https://www.gunviolencearchive.org/query",
  cf_agent = NULL
)
{
  ## Use a valid browser user agent, but don't always use the same one.
  cfAgent <- cf_agent

  uri <- query_uri

  ## Make this GET first to retrieve name/value [query_id] & [form_build_id].
  if (is.null(cf_agent)) cfAgent <- unname(sample(.cf_agents, 1))
  g <- httr::GET(uri, httr::user_agent(cfAgent))

  x0 <- xml2::read_html(httr::content(g, "text"))
  `query[query_id]` <- xml2::xml_attr(xml2::xml_find_all(x0, '//*[@name="query[query_id]"]'), "value")
  form_build_id <- xml2::xml_attr(xml2::xml_find_all(x0, '//*[@name="form_build_id"]'), "value")

  guid <- uuid::UUIDgenerate()
  form_body <- list(
    `query[base_group][base_group_select]` = "And",
    `query[filters][%s][type]` = "IncidentDate",
    `query[filters][%s][outer_filter][weight]` = "0.001",
    `query[filters][%s][outer_filter][comparator]` = "is in",
    `query[filters][%s][outer_filter][filter][field][date-from]` = start_date,
    `query[filters][%s][outer_filter][filter][field][date-to]` = end_date,
    `query[filters][new][type]` = "",
    `query[filters][new][outer_filter][weight]` = "0.002",
    `query[query_id]` = `query[query_id]`,
    `query[results_type][select]` = results_type,
    `form_build_id` = form_build_id,
    `form_id` = "gva_entry_query",
    `op` = "Search"
  ) %>% (function(x) { names(x) <- sprintf(names(x), guid); x })

  ## Fill in [query_id] & [form_build_id] here; response will include a TBODY containing the data.
  if (is.null(cf_agent)) cfAgent <- unname(sample(.cf_agents, 1))
  p <- httr::POST(
    url = "https://www.gunviolencearchive.org/query",
    encode = "form",
    body = form_body,
    httr::user_agent(cfAgent)
  )

  x <- xml2::read_html(httr::content(p, "text"))
  no_results <- !is_invalid(xml2::xml_find_all(x, '//*[@class="empty message"]'))

  if (no_results)
    return (NULL)

  ## First page is 1, last = N is given in '<li class="pager-last last">';
  ## GET page sequence 1 ... N: https://www.gunviolencearchive.org/query/[query_id]?page=N
  ## Mine each page for its TBODY.

  last_page <- 1
  last_page_node <- xml2::xml_find_all(x, '//*[@title="Go to last page"]')
  if (!is_invalid(last_page_node)) {
    last_page <- stringr::str_match(xml2::url_parse(xml2::xml_attr(last_page_node, "href"))["query"], "^.*?(\\d+)$")[, 2]
  }
  pages <- 0
  if (last_page > 1)
    pages <- seq(0, as.numeric(last_page))

  r <- sapply(pages,
    function(a)
    {
      if (is.null(cf_agent)) cfAgent <- unname(sample(.cf_agents, 1))

      g <- httr::GET(paste(paste(uri, `query[query_id]`, sep = "/"), "page=", sep = "?") %_% a, httr::user_agent(cfAgent))
      x <- xml2::read_html(httr::content(g, "text"))
      tab <- rvest::html_node(x, "table") %>% rvest::html_table(fill = TRUE)
      if (results_type == "incidents") {
        hasSource <- !is.na(stringr::str_match(tab$Operations,
          stringr::regex("Source", ignore_case = TRUE))[, 1]) # At least one entry has no source URL given (e.g. 5 July 2017).
        temp <- rep(NA_character_, NROW(tab))
        Operations <- sapply(xml2::xml_find_all(x, '//*[@class="1 last"]'),
          function(b) xml2::xml_attr(xml2::xml_children(b)[[1]], "href"), simplify = TRUE)
        temp[hasSource] <- Operations
        Operations <- temp
      } else {
        Operations <- stringr::str_match(sapply(xml2::xml_find_all(x, '//*[@class="0 first last"]'),
          function(b) xml2::xml_attr(xml2::xml_children(b)[[1]], "href"), simplify = TRUE), "^.*?(\\d+)$")[, 2]
      }
      tab$Operations <- unlist(Operations)[1] # Should take care of various missing values.

      tab
    }, simplify = FALSE)

  d <- Reduce(rbind, r) %>%
  ## Get list of quoted variable names for extracting them from data frame:
  # op <- options("useFancyQuotes"); options(useFancyQuotes = FALSE); cat("  " %_% dQuote(names(d)) %_% ",", sep= "\n"); options(op)
    dplyr::rename(.,
      date = "Incident Date",
      state = "State",
      city_or_county = "City Or County",
      address = "Address",
    ) %>%
    mutate(
      #date = lubridate::mdy(date)
      date = lubridate::as_datetime(lubridate::mdy(date))
    )

  d <- if (results_type == "incidents") {
    dplyr::rename(d,
      id = "Incident ID",
      num_killed = "# Killed",
      num_injured = "# Injured",
      source = "Operations"
    )
  } else if (results_type == "participants") {
    dplyr::rename(d,
      gender = "Participant Gender",
      name = "Participant Name",
      age_group = "Participant Age Group",
      incident_id = "Operations"
    )
  }

  d
}


#' @export
gva_get_db_geolocations <- function(sort = TRUE)
{
  get_geolocation_table_sql <- "SELECT * FROM geolocation;"
  conn <- gva_connect_db()
  g <- DBI::dbGetQuery(conn, get_geolocation_table_sql)
  DBI::dbDisconnect(conn)

  if (sort)
    g <- g %>% dplyr::arrange(incident_id)

  g
}

## usage:
# g <- gva_get_db_geolocations()


#' @export
gva_get_geolocation_data <- function(
  ids,
  incidentUriBase = "https://www.gunviolencearchive.org/incident",
  get_fun = httr::GET, # Also 'cf_GET()' to bypass Cloudflare protection,
  verbose = FALSE
)
{
  rv <- Reduce(rbind, sapply(ids,
    function(a)
    {
      g <- get_fun(paste(incidentUriBase, a, sep = "/"))
      geo_loc_match <- stringr::str_match(httr::content(g, "text"),
        stringr::regex("geolocation\\:\\s*?(.*?)\\<", ignore_case = TRUE))
      geo_loc <- keystone::eval_js(paste0("c(", trimws(geo_loc_match[, 2]), ")"))

      r <- keystone::dataframe(incident_id = a, lat = NA, lon = NA)
      if (!all(is.na(geo_loc))) {
        r$lat <- geo_loc[1]; r$lon <- geo_loc[2]
      }

      if (verbose) {
        cat("incident ID:", r$incident_id, "lat:", r$lat, "lon:", r$lon, fill = TRUE); utils::flush.console()
      }

      r
    }, simplify = FALSE))

  rv
}


#' @export
gva_get_missing_geolocation_ids <- function()
{
  get_missing_geolocations_sql <-
    "SELECT id FROM incidents
      EXCEPT
      SELECT incident_id FROM geolocation;"

  conn <- gva_connect_db()
  ids <- DBI::dbGetQuery(conn, get_missing_geolocations_sql)$id
  DBI::dbDisconnect(conn)

  ids
}


#' @export
gva_update_missing_geolocations <- function(
  max_requests = 20,
  verbose = TRUE,
  chunk_size = 5
)
{
  ids <- gva_get_missing_geolocation_ids()
  if (!is.null(max_requests) && length(ids) > max_requests)
    ids <- head(ids, max_requests)

  if (verbose) {
    cat(sprintf("Updating %i geolocations ...", length(ids)), fill = TRUE)
  }

  g <- Reduce(rbind, sapply(keystone::chunk(ids, chunk_size),
    function(a)
    {
      d0 <- gva_get_geolocation_data(a, get_fun = cf_GET, verbose = verbose)
      d <- d0 %>% dplyr::filter(!is.na(lat) & !is.na(lon))

      if (keystone::is_invalid(d))
        return (NULL)

      conn <- gva_connect_db()
      DBI::dbWriteTable(conn, "geolocation", d, append = TRUE)
      DBI::dbDisconnect(conn)

      d
    }, simplify = FALSE))

  g
}
