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
#' @export
sc_schema_catalogue <- function(depth = "folder", key = sc_key()) {
  sc_schema(depth = depth, key = key)
}