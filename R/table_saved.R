#' @rdname sc_table
#' @export
sc_table_saved_list <- function(key = NULL, server = "ext") {
  if (is.null(key))
    key <- sc_key(server)
  schema <- sc_schema(key  = key, server = server)
  schema <- attr(schema,"response")
  schema <- httr::content(schema)
  schema <- schema$children

  tables <- sapply(schema, function(x) x$type == "TABLE")
  saved_tables <- schema[tables]
  vctrs::new_data_frame(list(
    label = sapply(saved_tables, function(x) x$label),
    id = new_schema_uri(
      vapply(saved_tables, function(x) x$id, ""),
      vapply(saved_tables, function(x) x$id, "")
    )
  ), class = c("tbl", "tbl_df"))
}

#' @param table_uri Identifier of a saved table as returned by
#'   [sc_table_saved_list()]
#' @rdname sc_table
#' @export
sc_table_saved <- function(table_uri, language = NULL, key = NULL, server = "ext") {
  language <- sc_language(language)
  table_uri <- as.character(table_uri)
  if (substr(table_uri, 1, 3) != "str")
    table_uri <- paste0("str:table:", table_uri)
  sc_with_cache(c("sc_table_saved", table_uri, language, key), function() {
    httr::GET(
      url = paste0(base_url(server), "/table/saved/", table_uri),
      config = sc_headers(language, key, server)
    ) |> sc_check_response()
  }) |> sc_table_class$new()
}
