#' Links to important STATcube pages
#'
#' A collection of links, to browse important STATcube pages.
#'
#' @examples
#' # Zeige Startseite
#' sc_browse()
#'
#' @export
sc_browse <- function() {
  utils::browseURL("http://sdbext:8081/statistik.at/ext/statcube/home")
}

#' @rdname sc_browse
#' @examples
#' # Zeige "Einstellungen"
#' sc_browse_preferences()
#' @export
sc_browse_preferences <- function() {
  utils::browseURL("http://sdbext:8081/statistik.at/ext/statcube/jsf/preferences/editPreferences.xhtml")
}
