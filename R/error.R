#' Error handling for the STATcube REST API
#'
#' @description
#' In case API requests are unsuccessfull, `STATcubeR` will throw errors
#' to summarize the httr error type and its meaning.
#' Requests are considered unsuccessfull if one of the following applies
#' * The response returns `TRUE` for `httr::http_error()`.
#' * The response is not of type `"application/json"`
#'
#' In some cases it is useful to get direct access to a faulty response object.
#' For that purpose, it is possible to use [sc_last_error()] which will provide
#' the httr response object for the last unsuccessfull request.
#' @return The return value from `httr::GET()` or `httr::POST()`.
#' @examplesIf sc_key_exists()
#' try(sc_table_saved("invalid_id"))
#' last_error <- sc_last_error()
#' httr::content(last_error)
#' sc_last_error_parsed() %>% str()
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
    content = httr::content(last_error),
    status = httr::http_status(last_error)
  )
}

sc_env <- new.env(parent = emptyenv())

message_sc_last_error <- "see \033[90msc_last_error()\033[39m for more details"

sc_check_response <- function(response) {
  stopifnot(inherits(response, "response"))
  if (httr::http_error(response)) {
    sc_env$last_error <- response
    message <- paste0(httr::http_status(response)$message, "\n")
    if (httr::http_type(response) == "application/json")
      message <- httr::content(response, as = "text") %>%
        jsonlite::prettify(indent = 2) %>% paste0(message, .)
    message <- paste0(message, message_sc_last_error)
    stop(message, call. = FALSE)
  }
  if (httr::http_type(response) != "application/json") {
    sc_env$last_error <- response
    stop("expected a response of type \"application/json\" but got \"",
         httr::http_type(response), "\"\n",
         "  possible reasons:\n  - rate limit exceeded. ",
         "Check with \033[90msc_rate_limit_table()\033[39m\n  ",
         "- invalid json body (sc_table, sc_table_custom)\n  ",
         message_sc_last_error, call. = FALSE)
  }
  response$request$headers["APIKey"] <- "HIDDEN"
  invisible(response)
}
