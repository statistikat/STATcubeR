#' Other endpoints of the STATcube REST API
#'
#' Utilize the simple endpoints `/info` and `/table_rate_limit`. Those provide
#' information about available locales and the amount of requests available
#' for calls against the `/table` endpoint.
#' @examples
#' sc_info()
#' sc_rate_limit_table()
#' @name other_endpoints
#' @rdname other_endpoints
#' @inheritParams sc_key
#' @inheritParams sc_table
#' @return
#' * [sc_info()] returns information about all available database languages
#' * [sc_rate_limit_table()] returns a `3x1` dataframe with the following columns
#'     * `remaining` how much requests can be sent to the `/table` endpoint until
#'       the rate limit is reached.
#'     * `limit` the number of requests allowed per hour.
#'     * `reset` a tiestamp when the rate limit will be reset. Ususally, this
#'       should be less than one hour `after the current time.
#' @export
sc_info <- function(language = c("en", "de"), key = sc_key()) {
  response <- httr::GET(
    url = paste0(base_url, "/info"),
    config = sc_headers(language, key)
  )
  info_content <- httr::content(response)
  info_content$languages %>%
    lapply(function(x)
      data.frame(locale = x$locale, displayName = x$displayName)) %>%
    do.call(rbind, .)
}

#' @rdname other_endpoints
#' @export
sc_rate_limit_table <- function(language = c("en", "de"), key = sc_key()) {
  response <- httr::GET(
    url = paste0(base_url, "/rate_limit_table"),
    config = sc_headers(language, key)
  )
  rate_limit <- httr::content(response)
  class(rate_limit) <- "sc_rate_limit_table"
  rate_limit
}

#' @export
print.sc_rate_limit_table <- function(x, ...) {
  cat(
    "remaining: ", x$remaining, "/", x$limit, "\n",
    "reset:     ", as.character(sc_parse_time(x$reset)),sep = ""
  )
}

