#' Create custom tables
#'
#' Define requests against the /table endpoint by providing
#' URIs to databases, measures and fields.
#' The URIs can be obtained using [sc_schema_db()].
#' See the [Custom tables article](https://statistikat.github.io/STATcubeR/articles/sc_table_custom.html)
#' for more details.
#'
#' @param db The uid of a database
#' @param measures A character vector of uids for measures. Can be either of
#'   type `MEASURE` or of type `STAT_FUNCTION`
#' @param dimensions A character vector of dimensions for the cube. Can be
#'   either of type `FIELD` or type `VALUESET`. Those entries are referred to
#'   as `fields` in the parsed API response
#' @keywords internal
#' @examples
#' sc_table_custom(
#'   db = "str:database:detouextregsai",
#'   measures = c(
#'     "str:statfn:detouextregsai:F-DATA1:F-ANK:SUM",
#'     "str:measure:detouextregsai:F-DATA1:F-UEB"
#'   ),
#'   dimensions = c(
#'     "str:field:detouextregsai:F-DATA1:C-SDB_TIT-0",
#'     "str:valueset:detouextregsai:F-DATA1:C-C93-2:C-C93SUM-0"
#'   )
#' )
#' @export
sc_table_custom <- function(db, measures = c(), dimensions = c(), language = c("en", "de"),
                            add_totals = TRUE, key = NULL) {
  json_list <- list(database = db, measures = as.list(measures),
                    dimensions = lapply(dimensions, list))
  json <- jsonlite::toJSON(json_list, auto_unbox = TRUE, pretty = TRUE)
  response <- sc_table_json_post(json, language, add_totals, key)
  sc_table_class$new(response, toString(json))
}
