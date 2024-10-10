#' @examplesIf sc_key_exists()
#' my_catalogue <- sc_schema_catalogue()
#'
#' ## print
#' my_catalogue
#'
#' ## access the parsed catalogue
#' my_catalogue$Statistics$`Labour Market`
#' my_catalogue$Statistics$`Labour Market`$`Working hours (Labour Force Survey)`
#'
#' db_schema <- sc_schema_db("deake005")
#'
#' # printing
#' db_schema
#'
#' # access child nodes
#' db_schema$`Demographic Characteristics`
#' db_schema$`Demographic Characteristics`$Gender$Gender
#' db_schema$`Demographic Characteristics`$Gender$Gender$male
#'
#' # access the raw response from httr::GET()
#' my_response <- attr(db_schema, "response")
#' my_response$headers$date
#' my_content <- httr::content(my_response)
#' my_content$label
#'
#' # print with data.tree
#'
#'  treeX_B1 <- sc_schema("str:group:deake005:X_B1", depth = "valueset")
#'  print(treeX_B1, tree = TRUE)
#' @describeIn sc_schema is similar to the
#'   [table view](`r sc_browse_database('deake005', open = TRUE)`)
#'   of the STATcube GUI and gives information about all measures and
#'   classification fields for a specific database
#' @export
sc_schema_db <- function(id, depth = "valueset", language = c("en", "de"),
                         key = NULL) {
  stopifnot(is.character(id) && length(id) == 1)
  if (substr(id, 1, 3) != "str")
    id <- paste0("str:database:", id)
  server <- sc_database_get_server(id)
  if (is.null(key))
    key <- sc_key(server)
  sc_schema(id, depth, language, key, server)
}
