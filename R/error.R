#' Error handling for the STATcube REST API
#'
#' @description
#' In case API requests are unsuccessful, `STATcubeR` will throw errors
#' to summarize the httr error type and its meaning.
#' Requests are considered unsuccessful if one of the following applies
#' * The response returns `TRUE` for `httr::http_error()`.
#' * The response is not of type `"application/json"`
#'
#' In some cases it is useful to get direct access to a faulty response object.
#' For that purpose, it is possible to use [sc_last_error()] which will provide
#' the httr response object for the last unsuccessful request.
#' @return The return value from `httr::GET()` or `httr::POST()`.
#' @examplesIf sc_key_exists()
#' try(sc_table_saved("invalid_id"))
#' last_error <- sc_last_error()
#' httr::content(last_error)
#' str(sc_last_error_parsed())
#' @export
sc_last_error <- function() {
  sc_env$last_error
}

#' @describeIn sc_last_error returns the last error as a list containing
#'   the response content and the response status
#' @export
sc_last_error_parsed <- function() {
  last_error <- sc_last_error()
  list(
    request = list(
      method = last_error$request$method,
      url = last_error$request$url
    ),
    content = httr::content(last_error),
    status = httr::http_status(last_error)
  )
}

sc_env <- new.env(parent = emptyenv())

cli_function_link <- function(fun, pkg = "STATcubeR", topic = fun) {
  if (in_pkgdown() || !cli::ansi_has_hyperlink_support())
    return(cli::format_inline("{.fun {fun}}"))
  cli::format_inline("{.help [{.fun {fun}}]({pkg}::{topic})}")
}

cli_function_run <- function(fun, pkg = "STATcubeR") {
  cli::format_inline("{.run [{fun}()]({pkg}::{fun}())}")
}

message_sc_last_error <- function() { cli::col_silver(
  "Run ", cli_function_run("sc_last_error"), " or read the ",
  cli::style_hyperlink("online documentation",
    "https://statistikat.github.io/STATcubeR/articles/sc_last_error.html"),
  " for more details"
) }

sc_check_response <- function(response) {
  stopifnot(inherits(response, "response"))
  if (httr::http_error(response)) {
    sc_env$last_error <- response
    message <- paste0(httr::http_status(response)$message, "\n")
    if (httr::http_type(response) == "application/json")
      message <- paste0(message, jsonlite::prettify(httr::content(response, as = "text"),indent = 2))
    message <- paste0(message, message_sc_last_error())
    stop(message, call. = FALSE)
  }
  if (httr::http_type(response) != "application/json") {
    sc_env$last_error <- response
    stop(cli::format_error(c(
      cli::format_inline("expected an API response of type {.val application/json}
        but got {.val {httr::http_type(response)}}"),
      cli::style_italic("possible reasons:"),
      "*" = "rate limit exceeded, check with {cli_function_run('sc_rate_limit_table')}",
      "*" = "invalid json body (via {cli_function_link('sc_table')} or
         {cli_function_link('sc_table_custom')})",
      message_sc_last_error()
    )), call. = FALSE)
  }
  response$request$headers["APIKey"] <- "HIDDEN"
  invisible(response)
}
