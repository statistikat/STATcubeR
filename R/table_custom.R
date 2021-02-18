#' @param db The uid of a database
#' @param measures A character vector of uids for measures. Can be either of
#'   type `MEASURE` or of type `STAT_FUNCTION`
#' @param dimensions A character vector of dimensions for the cube. Can be
#'   either of type `FIELD` or type `VALUESET`. Those entries are referred to
#'   as `fields` in the parsed API response
#' @rdname sc_table
#' @examples
#'
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
sc_table_custom <- function(db, measures, dimensions, language = c("en", "de"),
                            key = sc_key()) {
  json_list <- list(database = db, measures = as.list(measures),
                    dimensions = lapply(dimensions, list))
  response <- httr::POST(
    url = paste0(base_url, "/table"),
    body = jsonlite::toJSON(json_list, auto_unbox = TRUE),
    encode = "raw",
    config = sc_headers(language, key)
  )

  sc_table_class$new(response)
}
