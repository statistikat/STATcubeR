#' Links to important 'STATcube' and 'OGD' pages
#'
#' A collection of links, to browse important 'STATcube' pages.
#'
#' @inheritParams sc_key
#' @name sc_browse
NULL

#' @describeIn sc_browse opens the home menu of 'STATcube'
#' @examples
#' sc_browse()
#' @return the URL of a specific webpage which is opened by default
#' in a web browser.
#' @export
sc_browse <- function(server = "ext") {
  sc_url(sc_url_gui(server), "home")
}

#' @describeIn sc_browse opens the preference menu with the API key
#' @examples
#' sc_browse_preferences()
#' @export
sc_browse_preferences <- function(server = "ext") {
  sc_url(sc_url_gui(server), "jsf/preferences/editPreferences.xhtml")
}

#' @describeIn sc_browse shows the info page for a table
#' @examples
#' sc_browse_table('defaulttable_deake005')
#' @param table a table id
#' @export
sc_browse_table <- function(table, server = "ext") {
  sc_url(sc_url_gui(server), "openinfopage?tableId=", table)
}

#' @describeIn sc_browse shows the info page for a database
#' @param database a database id
#' @param open If `FALSE` (the default), open the infopage for the database.
#'   Otherwise, open the table view.
#' @examples
#' sc_browse_database('deake005')
#' @export
sc_browse_database <- function(database, server = NULL, open = FALSE) {
  if (is.null(server))
    server <- sc_database_get_server(paste0("str:database:", database))
  action <- ifelse(open, "opendatabase", "openinfopage")
  sc_url(sc_url_gui(server), action, "?id=", database)
}

#' @describeIn sc_browse shows the data catalogue explorer
#' @examples
#' sc_browse_catalogue()
#' @export
sc_browse_catalogue <- function(server = "ext") {
  sc_url(sc_url_gui(server), "jsf/dataCatalogueExplorer.xhtml")
}

#' @describeIn sc_browse shows the landing page for OGD datasets
#' @examples
#' sc_browse_ogd()
#' @export
sc_browse_ogd <- function() {
  sc_url("https://data.statistik.gv.at/")
}

in_pkgdown <- function() {
  identical(Sys.getenv("IN_PKGDOWN"), "true")
}

# check if STATcubeR is used inside the firewall of Statistics Austria
in_stat <- function() {
  if (in_pkgdown())
    return(FALSE)
  if (Sys.getenv("STATCUBER_IN_STAT") != "")
    return(as.logical(Sys.getenv("STATCUBER_IN_STAT")))
  Sys.info()["nodename"] %in% c("xlwt0012", "xlwp0017")
}

sc_url_gui <- function(server = "ext") {
  if (server == "ext" && (!in_stat() || Sys.getenv("NOT_CRAN") != ""))
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
  if (in_pkgdown() || !interactive())
    return(cli::style_hyperlink(x, x))
  utils::browseURL(unclass(x))
}
