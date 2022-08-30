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

sc_headers <- function(language = c("en", "de"), key, ...) {
  httr::add_headers(
    APIKey = key, `Accept-Language` = match.arg(language), ...,
    `User-Agent` = paste0("STATcubeR/", sc_version(FALSE),
                          " (http://github.com/statistikat/STATcubeR)",
                          " httr/", utils::packageVersion("httr"),
                          " R/", R.version$major, ".", R.version$minor))
}
