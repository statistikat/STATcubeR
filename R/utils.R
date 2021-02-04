#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Utility Functions
#'
#' Supporting functions for STATcubeR
#'
#' @rdname utils
#' @param response an object of class `sc_table`
#' @param file A filename, under which the json should be saved.
#' @description * `sc_write_json()` saves the api request for a table as
#'   a json file. The resulting json file can be passed to [sc_table()]
#' @export
sc_write_json <- function(response, file) {
  response$raw$query %>%
    jsonlite::write_json(path = file, auto_unbox = TRUE, pretty = TRUE)
}

sc_parse_time <- function(timestamp) {
  (as.numeric(timestamp)/1000) %>% as.POSIXct(origin = "1970-01-01")
}

sc_get_ratelimit <- function(x) {
  headers <- x$response$headers
  reset <- sc_parse_time(headers$`x-ratelimit-reset-table`)
  diff <- reset - Sys.time()
  ratelimit <- headers$`x-ratelimit-table`
  remaining <- headers$`x-ratelimit-remaining-table`
  cat(remaining, "/", ratelimit, ", reset in ", diff, " minutes", sep = "")
}

#' @rdname utils
#' @description * `sc_annotation_legend()` lists all annotations occurring in
#'   a table together with descriptions of the annotations.
#' @export
sc_annotation_legend <- function(response) {
  response$raw$annotationMap
}
