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

is_sc_response <- function(x) {
  inherits(x, "STATcube_response")
}

#' Hilfsfunktionen
#'
#' Unterstützende Funktionen in STATcubeR
#'
#' @rdname utils
#' @param response Ein Objekt der Klasse STATcube_response
#' @export
sc_content <- function(response) {
  stopifnot(is_sc_response(response))
  httr::content(response$response)
}

#' @rdname utils
#' @param file Ein Dateiname, unter welchem das json abgespeichert
#'   werden soll.
#' @export
sc_write_json <- function(response, file) {
    sc_content(response)$query %>%
    jsonlite::write_json(path = file, auto_unbox = TRUE, pretty = TRUE)
}

sc_get_ratelimit <- function(x) {
  headers <- x$response$headers
  reset <- (as.numeric(headers$`x-ratelimit-reset-table`)/1000) %>%
    as.POSIXct(origin = "1970-01-01")
  diff <- reset - Sys.time()
  ratelimit <- headers$`x-ratelimit-table`
  remaining <- headers$`x-ratelimit-remaining-table`
  cat(remaining, "/", ratelimit, ", reset in ", diff, " minutes", sep = "")
}

#' @rdname utils
#' @export
sc_annotation_legend <- function(response) {
  content <- sc_content(response)
  content$annotationMap
}
