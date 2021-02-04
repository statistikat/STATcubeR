#' Create a request against the /schema endpoint
#'
#' Invoke the /schema endpoint of the STATcube REST API. In case of
#' `sc_schema_catalogue()`, recurse into all datasets and tables and return a
#' nested list with ids and labels for all resources. For `sc_schema_db()`,
#' recurse into all valuesets and return a list of all resources available
#' tor the specific dataset. The return values can be displayed as a
#' tree object.
#' @examples
#' \dontrun{
#'
#' my_catalogue <- sc_schema_catalogue()
#'
#' ## print
#' my_catalogue
#'
#' ## access the parsed catalogue
#' my_catalogue$Statistics$`Labour Market`
#' my_catalogue$Statistics$`Labour Market`$`Working hours (Labour Force Survey)`
#' }
#' @rdname sc_schema
#' @name sc_schema
#' @inheritParams sc_key
#' @export
sc_schema_catalogue <- function(key = sc_key()) {
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
