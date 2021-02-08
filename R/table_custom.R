#' @param db The uid of a database
#' @param measures A character vector of uids for measures. Can be either of
#'   type `MEASURE` or of type `STAT_FUNCTION`
#' @param fields A character vector of field variables. Can be either of
#'   type `FIELD` or type `VALUESET`
#' @rdname sc_table
#' @examples
#' \dontrun{
#'
#' sc_table_custom(
#'   db = "str:database:detouextregsai",
#'   measures = c(
#'     "str:statfn:detouextregsai:F-DATA1:F-ANK:SUM",
#'     "str:measure:detouextregsai:F-DATA1:F-UEB"
#'   ),
#'   fields = c(
#'     "str:field:detouextregsai:F-DATA1:C-SDB_TIT-0",
#'     "str:valueset:detouextregsai:F-DATA1:C-C93-2:C-C93SUM-0"
#'   )
#' )
#' }
#' @export
sc_table_custom <- function(db, measures, fields, language = c("en", "de"),
                            key = sc_key()) {
  json_list <- list(database = db, measures = as.list(measures),
                    dimensions = lapply(fields, list))
  response <- httr::POST(
    url = paste0(base_url, "/table"),
    body = jsonlite::toJSON(json_list, auto_unbox = TRUE),
    encode = "raw",
    config = sc_headers(language, key)
  )

  sc_table_class$new(response)
}
