#' @export
sc_saved_table <- function(table_uri, token = sc_token()) {
  response <- httr::GET(
    url = paste0(base_url, "/table/saved/", table_uri),
    config = httr::add_headers(APIKey = token)
  ) %>% as_sc_response()
}

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
