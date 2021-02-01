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
#' my_catalogue$Statistics$`Labour Market`
#' my_catalogue$Statistics$`Labour Market`$`Working hours (Labour Force Survey)`
#' @inheritParams sc_key
#' @export
sc_catalogue <- function(key = sc_key()) {
  catalogue <- sc_get_schema(key = sc_key(), "?depth=folder")
  catalogue_content <- httr::content(catalogue)
  x <- tabulate_db_info(catalogue_content)
  attr(x, "response") <- catalogue
  x
}

#' @export
print.sc_catalogue <- function(x, limit = 100, ...) {
  tree <- data.tree::as.Node(x$parsed)
  data.tree::Prune(tree, function(node) {!is.null(node$type)})
  print(tree, "type", limit = limit)
}
