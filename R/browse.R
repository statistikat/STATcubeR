#' Links to important STATcube and OGD pages
#'
#' A collection of links, to browse important STATcube pages.
#'
#' @examples
#' # Show starting page
#' sc_browse()
#'
#' @export
sc_browse <- function(server = "ext") {
  utils::browseURL(sprintf(
    "http://sdb%s:8081/statistik.at/%s/statcube/home", server, server
  ))
}

#' @rdname sc_browse
#' @examples
#' # Show the preferences page with the API key
#' sc_browse_preferences()
#'
#' @export
sc_browse_preferences <- function(server = "ext") {
  utils::browseURL(sprintf(
    "http://sdb%s:8081/statistik.at/%s/statcube/jsf/preferences/editPreferences.xhtml",
    server, server
  ))
}

#' @rdname sc_browse
#' @examples
#' # Show the landing page for OGD datasets
#' sc_browse_ogd()
#' @export
sc_browse_ogd <- function() {
  utils::browseURL("https://data.statistik.gv.at/")
}
