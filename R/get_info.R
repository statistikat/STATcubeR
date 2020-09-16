sc_get_info <- function(token = sc_token()) {
  response <- httr::GET(
    url = paste0(base_url, "/info"),
    config = httr::add_headers(APIKey = token)
  )
  response
}

sc_get_schema <- function(token = sc_token()) {
  response <- httr::GET(
    url = paste0(base_url, "/schema"),
    config = httr::add_headers(APIKey = token)
  )
  response
}

sc_get_rate_limit <- function(token = sc_token()) {
  response <- httr::GET(
    url = paste0(base_url, "/rate_limit_table"),
    config = httr::add_headers(APIKey = token)
  )
  response
}
