#' Links to important STATcube and OGD pages
#'
#' A collection of links, to browse important STATcube pages.
#'
#' @examples
#' # Show starting page
#' sc_browse()
#' @inheritParams sc_key
#' @export
sc_browse <- function(server = "ext") {
  sc_url(sc_url_gui(server), "home")
}

#' @rdname sc_browse
#' @examples
#'
#' # Show the preferences page with the API key
#' sc_browse_preferences()
#'
#' @export
sc_browse_preferences <- function(server = "ext") {
  sc_url(sc_url_gui(server), "jsf/preferences/editPreferences.xhtml")
}

#' @rdname sc_browse
#' @examples
#' # Show the landing page for OGD datasets
#' sc_browse_ogd()
#' @export
sc_browse_ogd <- function() {
  sc_url("https://data.statistik.gv.at/")
}

# check if STATcubeR is used inside the firewall of Statistics Austria
in_stat <- function() {
  Sys.info()["nodename"] %in% c("xlwt0012", "xlwp0017")
}

sc_url_gui <- function(server = "ext") {
  if (server == "ext" && !in_stat())
    return("https://portal.statistik.at/statistik.at/ext/statcube/")
  if (server == "test")
    return("http://sdbtest:8081/statistik.at/wdev/statcube/")
  sprintf("http://sdb%s:8081/statistik.at/%s/statcube/", server, server)
}

sc_url <- function(...) {
  url <- paste0(...)
  class(url) <- c("sc_url", class(url))
  url
}

#' @export
print.sc_url <- function(x, ...) {
  if (interactive())
    browseURL(unclass(x))
  else
    cat('STATcube url:', x)
}
