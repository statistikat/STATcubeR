#' @rdname info_and_rate_limit
#' @export
sc_rate_limit <- function(key = sc_key()) {
  response <- httr::GET(
    url = paste0(base_url, "/rate_limit_table"),
    config = httr::add_headers(APIKey = key)
  )
  rate_limit <- httr::content(response)
  class(rate_limit) <- "sc_rate_limit"
  rate_limit
}

#' @export
print.sc_rate_limit <- function(x, ...) {
  cat(
    "remaining: ", x$remaining, "/", x$limit, "\n",
    "reset:     ", as.character(sc_parse_time(x$reset)),sep = ""
  )
}
