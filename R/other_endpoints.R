#' Other endpoints of the STATcube REST API
#'
#' Utilize the simple endpoints `/info` and `/table_rate_limit`. Those provide
#' information about available locales and the amount of requests available
#' for calls against the `/table` endpoint.
#' @examplesIf sc_key_exists()
#' sc_info()
#' sc_rate_limit_table()
#' sc_rate_limit_schema()
#' sc_schema("str:group:deake005:X_B1") %>%
#'   sc_rate_limits()
#' @name other_endpoints
#' @inheritParams sc_key
#' @inheritParams sc_schema
NULL

#' @describeIn other_endpoints
#' returns information about all available database languages
#' @export
sc_info <- function(language = c("en", "de"), key = NULL, server = "ext") {
  response <- httr::GET(
    url = paste0(base_url(server), "/info"),
    config = sc_headers(language, key, server)
  ) %>% sc_check_response()
  info_content <- httr::content(response)
  info_content$languages %>%
    lapply(function(x)
      data_frame(locale = x$locale, displayName = x$displayName)) %>%
    do.call(rbind, .)
}

#' @describeIn other_endpoints
#' returns a `3x1` dataframe with the following columns
#' * `remaining` how much requests can be sent to the `/table`
#'   endpoint until the rate limit is reached.
#' * `limit` the number of requests allowed per hour.
#' * `reset` a timestamp when the rate limit will be reset.
#'   Usually, this should be less than one hour `after the current time.
#' @export
sc_rate_limit_table <- function(language = c("en", "de"), key = NULL, server = "ext") {
  response <- httr::GET(
    url = paste0(base_url(server), "/rate_limit_table"),
    config = sc_headers(language, key, server)
  ) %>% sc_check_response()
  rate_limit <- httr::content(response)
  class(rate_limit) <- "sc_rate_limit_table"
  rate_limit
}

#' @rdname other_endpoints
#' @export
sc_rate_limit_schema <- function(language = c("en", "de"), key = NULL, server = "ext") {
  response <- httr::GET(
    url = paste0(base_url(server), "/rate_limit_schema"),
    config = sc_headers(language, key, server)
  ) %>% sc_check_response()
  rate_limit <- httr::content(response)
  class(rate_limit) <- "sc_rate_limit_table"
  rate_limit
}

extract_rate_limits <- function(response) {
  header <- response$headers
  list(
    schema = list(
      limit = header[["x-ratelimit-schema"]],
      remaining = header[["x-ratelimit-remaining-schema"]],
      reset = header[["x-ratelimit-reset-schema"]]
    ) %>% `class<-`("sc_rate_limit_table"),
    table = list(
      limit = header[["x-ratelimit-table"]],
      remaining = header[["x-ratelimit-remaining-table"]],
      reset = header[["x-ratelimit-reset-table"]]
    ) %>% `class<-`("sc_rate_limit_table")
  )
}

#' @describeIn other_endpoints gets rate limits from response headers
#' @param x either a response-object (package `httr`), an object of class
#'   `sc_table` or an object of class `sc_schema`
#' @export
sc_rate_limits <- function(x) {
  if (inherits(x, "response"))
    return(extract_rate_limits(x))
  if (inherits(x, "sc_table"))
    return(extract_rate_limits(x$response))
  if (inherits(x, "sc_schema"))
    return(extract_rate_limits(attr(x, "response")))
  stop("sc_rate_limits() is only implemented for the classes
        'response', 'sc_table' and 'sc_schema'")
}


#' @export
print.sc_rate_limit_table <- function(x, ...) {
  cat(format(x), sep = "\n")
}

format.sc_rate_limit_table <- function(x, ...) {
  cli::format_inline(
    "{.field {x$remaining}} / {.field {x$limit}} (Resets at {.timestamp ",
    strftime(sc_parse_time(x$reset), "%H:%M:%S"), "})\n"
  )
}
