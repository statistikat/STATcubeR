#' Links to important STATcube and OGD pages
#'
#' A collection of links, to browse important STATcube pages.
#'
#' @examples
#' # Show starting page
#' sc_browse()
#'
#' @export
sc_browse <- function() {
  utils::browseURL("http://sdbext:8081/statistik.at/ext/statcube/home")
}

#' @rdname sc_browse
#' @examples
#' # Show the preferences page with the API key
#' sc_browse_preferences()
#'
#' @export
sc_browse_preferences <- function() {
  utils::browseURL("http://sdbext:8081/statistik.at/ext/statcube/jsf/preferences/editPreferences.xhtml")
}

#' @rdname sc_browse
#' @examples
#' # Show the landing page for OGD datasets
#' sc_browse_ogd()
#' @export
sc_browse_ogd <- function() {
  utils::browseURL("https://data.statistik.gv.at/")
}
