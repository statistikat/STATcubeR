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

sc_parse_time <- function(timestamp) {
  (as.numeric(timestamp)/1000) %>% as.POSIXct(origin = "1970-01-01")
}

#' Utility Functions
#'
#' Supporting functions for STATcubeR
#'
#' @rdname utils
#' @name utils
#' @param response an object of class `sc_table`
#' @param file A filename, under which the json should be saved.
#' @family functions for /table
#' @description * `sc_table_rate_limit()` is similar to `sc_ratelimit()` but
#'   uses the header of the `/table` response rather than a seperate API
#'   call.
#' @keywords internal
sc_table_rate_limit <- function(response) {
  headers <- response$response$headers
  res <- data.frame(
    remaining = headers$`x-ratelimit-remaining-table`,
    limit     = headers$`x-ratelimit-table`,
    reset     = headers$`x-ratelimit-reset-table`
  )
  class(res) <- "sc_rate_limit"
  res
}

#' @rdname utils
#' @name utils
#' @description * `sc_annotation_legend()` lists all annotations occurring in
#'   a table together with descriptions of the annotations.
#' @keywords internal
sc_annotation_legend <- function(response) {
  am <- response$raw$annotationMap
  data.frame(annotation = names(am), label = unlist(am), row.names = NULL)
}

sc_headers <- function(language = c("en", "de"), key = sc_key(), ...) {
  httr::add_headers(APIKey = key, `Accept-Language` = match.arg(language), ...)
}
