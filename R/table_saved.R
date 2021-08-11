#' @rdname sc_table
#' @export
sc_table_saved_list <- function() {
  schema <- sc_schema()
  schema <- schema %>% attr("response") %>% httr::content()
  schema <- schema$children

  tables <- schema %>% sapply(function(x) x$type == "TABLE")
  saved_tables <- schema[tables]
  data.frame(
    label = sapply(saved_tables, function(x) x$label),
    id = sapply(saved_tables, function(x) x$id)
  )
}

#' @param table_uri Identifier of a saved table as returned by
#'   [sc_table_saved_list()]
#' @rdname sc_table
#' @examples
#'
#' # get the ids and labels of all saved tables
#' (saved_tables <- sc_table_saved_list())
#' table_uri <- saved_tables$id[1]
#'
#' # get a table based on one of these ids
#' my_response <- sc_table_saved(table_uri)
#' as.data.frame(my_response) %>% head()
#' @export
sc_table_saved <- function(table_uri, language = c("en", "de"), key = sc_key()) {
  sc_with_cache(list(table_uri, language, key), function() { httr::GET(
    url = paste0(base_url, "/table/saved/", table_uri),
    config = sc_headers(language, key)
  )}) %>% sc_table_class$new()
}
