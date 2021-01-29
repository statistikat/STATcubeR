#' Obtain a catalogue of available tables and databases
#'
#' Invoke the /schema endpoint to recurse into all datasets and tables. The
#' return value contains a nested list with ids and labels for all
#' resources
#' @examples
#' my_catalogue <- sc_catalogue()
#'
#' ## print
#' my_catalogue
#'
#' ## access the parsed catalogue
#' arbeitsmarkt <- my_catalogue$parsed$Statistiken$Arbeitsmarkt
#' arbeitsmarkt$`Mikrozensus-Arbeitskräfteerhebung Jahresdaten`
#' @inheritParams sc_key
#' @export
sc_catalogue <- function(key = sc_key()) {
  catalogue <- sc_get_schema(key = sc_key(), "?depth=folder")
  catalogue_content <- httr::content(catalogue)
  tabulated <- tabulate_db_info(catalogue_content)
  x <- list(
    response = catalogue,
    parsed = data.tree::as.Node(tabulated) %>% data.tree::ToListSimple(),
    version = sc_version()
  )
  class(x) <- "sc_catalogue"
  x
}

#' @export
print.sc_catalogue <- function(x, limit = 100, ...) {
  data.tree::as.Node(x$parsed) %>% print("type", limit = limit)
}
