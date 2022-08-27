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
#' httr::http_status(last_error)
#' sc_last_error_parsed()
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

sc_check_response <- function(response) {
  stopifnot(inherits(response, "response"))
  if (httr::http_error(response) || (httr::http_type(response) != "application/json")) {
    sc_env$last_error <- response
    httr::stop_for_status(response)
    stop("API did not return json")
  }
  invisible(response)
}
