get_statcube_info <- function(token = statcube_token()) {
  response <- httr::GET(
    url = fs::path(base_url, "info"),
    config = httr::add_headers(APIKey = token)
  )
  response
}


get_statcube_schema <- function(token = statcube_token()) {
  response <- httr::GET(
    url = fs::path(base_url, "schema"),
    config = httr::add_headers(APIKey = token)
  )
  response
}

get_statcube_rate_limit <- function(token = statcube_token()) {
  response <- httr::GET(
    url = fs::path(base_url, "rate_limit_table"),
    config = httr::add_headers(APIKey = token)
  )
  response
}
