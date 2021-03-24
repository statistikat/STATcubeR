#' @param db_id a database id
#' @examples
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
#' @rdname sc_schema
#' @export
sc_schema_db <- function(db_id, depth = "valueset", language = c("en", "de"),
                         key = sc_key()) {
  stopifnot(is.character(db_id) && length(db_id) == 1)
  if (substr(db_id, 1, 3) != "str")
    db_id <- paste0("str:database:", db_id)
  sc_schema(resource_id = db_id, depth = depth, key = key, language = language)
}
