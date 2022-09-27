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
  (as.numeric(timestamp) / 1000) %>% as.POSIXct(origin = "1970-01-01")
}

sc_headers <- function(language = NULL, key = NULL, server = "ext", ...) {
  if (is.null(key))
    key <- sc_key(server)
  httr::add_headers(
    APIKey = key, `Accept-Language` = sc_language(language), ...,
    `User-Agent` = paste0("STATcubeR/", sc_version(FALSE),
                          " (http://github.com/statistikat/STATcubeR)",
                          " httr/", utils::packageVersion("httr"),
                          " R/", R.version$major, ".", R.version$minor))
}

sc_language <- function(language = NULL, options = c("en", "de")) {
  if (is.null(language))
    language <- getOption("STATcubeR.language")
  match.arg(language, options)
}
