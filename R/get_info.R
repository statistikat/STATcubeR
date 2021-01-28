sc_get_info <- function(key = sc_key()) {
  response <- httr::GET(
    url = paste0(base_url, "/info"),
    config = httr::add_headers(APIKey = key)
  )
  response
}

sc_get_schema <- function(key = sc_key(), ...) {
  response <- httr::GET(
    url = paste0(base_url, "/schema", ...),
    config = httr::add_headers(APIKey = key)
  )
  response
}

sc_get_rate_limit <- function(key = sc_key()) {
  response <- httr::GET(
    url = paste0(base_url, "/rate_limit_table"),
    config = httr::add_headers(APIKey = key)
  )
  response
}
