#' @param db The uid of a database
#' @param measures A character vector of uids for measures. Can be either of
#'   type `MEASURE` or of type `STAT_FUNCTION`
#' @param dimensions A character vector of dimensions for the cube. Can be
#'   either of type `FIELD` or type `VALUESET`. Those entries are referred to
#'   as `fields` in the parsed API response
#' @rdname sc_table
#' @export
sc_table_custom <- function(db, measures = c(), dimensions = c(), language = c("en", "de"),
                            add_totals = TRUE, key = NULL) {
  json_list <- list(database = db, measures = as.list(measures),
                    dimensions = lapply(dimensions, list))
  json <- jsonlite::toJSON(json_list, auto_unbox = TRUE, pretty = TRUE)
  response <- sc_table_json_post(json, language, add_totals, key)
  sc_table_class$new(response, toString(json))
}
