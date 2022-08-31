#' @param db_id a database id
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
#' "str:group:deake005:X_B1" %>%
#'   sc_schema(depth = "valueset") %>%
#'   print(tree = TRUE)
#' @rdname sc_schema
#' @export
sc_schema_db <- function(db_id, depth = "valueset", language = c("en", "de"),
                         key = NULL) {
  stopifnot(is.character(db_id) && length(db_id) == 1)
  if (substr(db_id, 1, 3) != "str")
    db_id <- paste0("str:database:", db_id)
  server <- sc_database_get_server(db_id)
  if (is.null(key))
    key <- sc_key(server)
  sc_schema(resource_id = db_id, depth = depth, key = key, language = language,
            server = server)
}
