#' Links zu wichtigen STATcube Seiten
#'
#' Eine Sammlung von links, um relevante STATcube Seiten anzuzeigen
#'
#' @examples
#' # Zeige Startseite
#' statcube_browse()
#' @export
statcube_browse <- function() {
  utils::browseURL("http://sdbext:8081/statistik.at/ext/statcube/home")
}

#' @rdname statcube_browse
#' @examples
#' # Zeige "Einstellungen"
#' statcube_browse_preferences()
#' @export
statcube_browse_preferences <- function() {
  utils::browseURL("http://sdbext:8081/statistik.at/ext/statcube/jsf/preferences/editPreferences.xhtml")
}
