#' @rdname sc_table
#' @export
sc_table_saved_list <- function(key = NULL, server = "ext") {
  if (is.null(key))
    key <- sc_key(server)
  schema = sc_schema(key  = key, server = server)
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
#' @export
sc_table_saved <- function(table_uri, language = c("en", "de"), key = NULL, sever = 'ext') {
  if (is.null(key))
    key <- sc_key(server)
  sc_with_cache(list(table_uri, language, key), function() {
    httr::GET(
      url = paste0(base_url(server), "/table/saved/", table_uri),
      config = sc_headers(language, key)
    ) %>% sc_check_response()
  }) %>% sc_table_class$new()
}
