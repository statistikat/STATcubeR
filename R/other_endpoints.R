#' Other endpoints of the STATcube REST API
#'
#' Utilize the simple endpoints `/info` and `/table_rate_limit`. Those provide
#' information about available locales and the amount of requests available
#' for calls against the `/table` endpoint.
#' @examplesIf sc_key_exists()
#' sc_info()
#' sc_rate_limit_table()
#' sc_rate_limit_schema()
#' sc_rate_limits(sc_schema("str:group:deake005:X_B1"))
#' @name other_endpoints
#' @inheritParams sc_key
#' @inheritParams sc_schema
#' @return
#' - [sc_info()]: a `data.frame` with two columns identifying possible languages
#' - [sc_rate_limit_table()], [sc_rate_limit_schema()], [sc_rate_limits()]: a `list` with elements
#'   * `remaining`: how much requests can be sent until the rate limit is reached
#'   * `limit`: the number of requests allowed per hour
#'   * `reset`: a timestamp when the rate limit will be reset
#' - [sc_rate_limits()]:

NULL

#' @describeIn other_endpoints
#' returns information about all available database languages
#' @export
sc_info <- function(language = c("en", "de"), key = NULL, server = "ext") {
  response <- httr::GET(
    url = paste0(base_url(server), "/info"),
    config = sc_headers(language, key, server)
  )
  response <- sc_check_response(response)
  info_content <- httr::content(response)
  return(do.call(rbind,lapply(info_content$languages,
           function(x) data_frame(locale = x$locale,
                                  displayName = x$displayName))))
}

#' @describeIn other_endpoints
#' returns a `list` with information about current requests-limits with
#' respect to the `/table` endpoint. It
#' also shows when the limits reset which should be less than one hour
#' after the current time.
#' @export
sc_rate_limit_table <- function(language = c("en", "de"), key = NULL, server = "ext") {
  response <- sc_check_response(httr::GET(
    url = paste0(base_url(server), "/rate_limit_table"),
    config = sc_headers(language, key, server)
  ))
  rate_limit <- httr::content(response)
  class(rate_limit) <- "sc_rate_limit_table"
  rate_limit
}

#' @describeIn other_endpoints
#' returns a `list` with information about current requests-limits with
#' respect to the `/schema` endpoint. It
#' also shows when the limits reset which should be less than one hour
#' after the current time.
#' @export
sc_rate_limit_schema <- function(language = c("en", "de"), key = NULL, server = "ext") {
  response <- sc_check_response(httr::GET(
    url = paste0(base_url(server), "/rate_limit_schema"),
    config = sc_headers(language, key, server)
  ))
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
    ) |> `class<-`("sc_rate_limit_table"),
    table = list(
      limit = header[["x-ratelimit-table"]],
      remaining = header[["x-ratelimit-remaining-table"]],
      reset = header[["x-ratelimit-reset-table"]]
    ) |> `class<-`("sc_rate_limit_table")
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

#' @export
format.sc_rate_limit_table <- function(x, ...) {
  cli::format_inline(
    "{.field {x$remaining}} / {.field {x$limit}} (Resets at {.timestamp ",
    strftime(sc_parse_time(x$reset), "%H:%M:%S"), "})\n"
  )
}
