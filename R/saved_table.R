#' Import saved tables
#'
#' Functions for importing saved tables from STATcube
#' @rdname saved_table
#' @export
sc_saved_tables_list <- function() {
  schema <- httr::content(sc_get_schema())$children
  tables <- schema %>% sapply(function(x) x$type == "TABLE")
  saved_tables <- schema[tables]
  data.frame(
    label = sapply(saved_tables, function(x) x$label),
    id = sapply(saved_tables, function(x) x$id)
  )
}

#' @param table_uri Identifier of a saved table as returned by
#'   [sc_saved_tables_list()]
#' @param key An API key
#' @param language The language to be used for labeling. `"en"` or `"de"`
#' @rdname saved_table
#' @examples
#' \dontrun{
#'
#' (saved_tables <- sc_saved_tables_list())
#' table_uri <- saved_tables$id[1]
#' my_response <- sc_saved_table(table_uri)
#' as.data.frame(my_response)
#' }
#' @export
sc_saved_table <- function(table_uri, key = sc_key(), language = c("en", "de")) {
  response <- httr::GET(
    url = paste0(base_url, "/table/saved/", table_uri),
    config = httr::add_headers(
      APIKey = key,
      `Accept-Language` = match.arg(language)
    )
  ) %>% as_sc_response()
}
