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

#' @export
sc_content <- function(x) {
  stopifnot(is_sc_response(x))
  httr::content(x$response)
}
