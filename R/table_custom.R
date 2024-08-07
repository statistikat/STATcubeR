#' Create custom tables
#'
#' Define requests against the /table endpoint by providing
#' URIs to databases, measures and fields.
#' The URIs can be obtained using [sc_schema_db()].
#' See the [Custom tables article](https://statistikat.github.io/STATcubeR/articles/sc_table_custom.html)
#' for more details.
#'
#' @param db The uid of a database. Must be of type `DATASET`
#' @param measures A character vector of uids for measures. Each entry must be
#'   of type `MEASURE`, `STAT_FUNCTION` or `COUNT`.
#' @param dimensions A character vector of dimensions for the cube. Can be
#'   either of type `FIELD` or type `VALUESET`. Those entries are referred to
#'   as `fields` in the parsed API response
#' @param add_totals Should totals be added for each classification field in
#'   the json request? Ignored if `recodes` is used.
#' @param recodes One or more recodes that were generated via [sc_recode()].
#'   If more than one recode is supplied, recodes should be concatenated with
#'   [c()].
#' @param language The language to be used for labeling. "en"
#'   (the default) will use English. "de" uses German.
#' @param dry_run If `TRUE`, no request is sent to the API. Instead, type
#'   checks are performed and the json request is returned as a string.
#'   Defaults to `FALSE`.
#' @inheritParams sc_table
#' @section Schema objects in parameters:
#' it is possible to pass `sc_schema` objects (usually generated by
#' [sc_schema_db()]) instead of ids in [sc_table_custom()] and [sc_recode()].
#' If provided, the schema objects will be converted into ids via `$id`.
#' @section Error handling:
#' Unfortunately, the API gives fairly vague error messages in case a
#' custom table request is ill defined. For this reason, [sc_table_custom()]
#' applies some simple heuristics and throws warnings if inconsistencies
#' in the provided parameters are recognized. The following conditions are
#' currently checked
#' * the parameter `db` is of type `DATABASE`
#' * all entries in `measures` are of type `MEASURE`, `COUNT` or
#'   `STATFN`
#' * all entries in `dimensions` are of type `VALUESET` or `FIELD`
#' * all entries in `field` are of type `VALUESET` or `FIELD`
#' * all entries in `map` are of type `VALUE`
#' * all fields in `recodes` are also present in `dimensions`
#' * the first two arguments of `sc_recode()` are consistent, i.e.
#'   if the provided `VALUE`s belong to the `VALUESET/FIELD`
#' @examplesIf sc_key_exists()
#' sc_table_custom("str:database:detouextregsai")
#'
#' sc_table_custom(
#'   "str:database:detouextregsai",
#'   dimensions = "str:field:detouextregsai:F-DATA1:C-SDB_TIT-0"
#' )
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
#'
#' schema <- sc_schema_db("detouextregsai")
#' region <- schema$`Other Classifications`$`Tourism commune [ABO]`$
#'   `Regionale Gliederung (Ebene +1)`
#' month <- schema$`Mandatory fields`$`Season/Tourism Month`
#'
#' x <- sc_table_custom(
#'   schema,
#'   schema$Facts$Arrivals,
#'   list(month, region),
#'   recodes = c(
#'     sc_recode(region, total = FALSE, map = list(
#'       region$Achensee,
#'       list(region$Arlberg, region$`Ausseerland-Salzkammergut`)
#'     )),
#'     sc_recode(month, total = FALSE)
#'   )
#' )
#' x$tabulate()
#' @export
sc_table_custom <- function(db, measures = c(), dimensions = c(),
                            language = c("en", "de"),
                            add_totals = TRUE, key = NULL, recodes = NULL,
                            dry_run = FALSE) {
  db <- as_id(db)
  measures <- as_id(measures, TRUE)
  dimensions <- as_id(dimensions, TRUE)
  json_list <- list(database = db, measures = as.list(measures),
                    dimensions = lapply(dimensions, I))
  if (!is.null(recodes)) {
    json_list$recodes <- recodes
    add_totals <- FALSE
  }
  json <- jsonlite::toJSON(json_list, auto_unbox = TRUE, pretty = TRUE)
  if (!all(names(recodes) %in% dimensions))
    warning("`recodes` and `dimensions` might be inconsistent")
  if (!all(grepl("^str:valueset", dimensions) | grepl("^str:field", dimensions)))
    warning("parameter `dimensions` is not of type `FIELD` or `VALUESET`")
  if (!all(grepl("^str:measure", measures) | grepl("^str:statfn", measures) |
           grepl("^str:count", measures)))
    warning("parameter `measures` is not of type `MEASURE`, `STATFN` or `COUNT`")
  if (!grepl("^str:database", db))
    warning("parameter `db` is not of type `DATABASE`")
  if (dry_run)
    return(json)
  response <- sc_table_json_post(json, language, add_totals, key)
  sc_table_class$new(response, toString(json))
}

#' @describeIn sc_table_custom creates a recode object which can be used
#'   for the `recode` parameter of [sc_table_custom()]
#' @param field An uid of a classification field to be recoded. The provided
#'   uid should also be passed in the `dimensions` parameter of
#'   [sc_table_custom()].
#' @param map A list of ids for values (type `VALUE`) This can also be a nested
#'   list if items should be grouped. See examples
#' @param total Add totals to the field? If `map` is provided, the totals
#'   will correspond to the filtered data.
#' @export
sc_recode <- function(field, map = NULL, total = FALSE) {
  if (is.null(map))
    return(stats::setNames(list(list(total = total)), as_id(field)))
  if (inherits(map, "sc_schema"))
    map <- list(map)
  else
    map <- stats::setNames(map, NULL)
  recode <- stats::setNames(
    list(list(
      map = lapply(map, function(value) {
        I(as_id(value, TRUE))
      }),
      total = total
    )),
    as_id(field)
  )
  code_parent <- gsub("^.*:", "", names(recode))
  codes_children <- unlist(recode[[1]]$map)
  if (!all(grepl(code_parent, codes_children)))
    warning("parameters `field` and `map` might be inconsistent")
  if (!all(grepl("^str:value:", codes_children)))
    warning("some entries in `map` are not of type VALUE")
  if (!grepl("^str:valueset", names(recode)) && !grepl("^str:field", names(recode)))
    warning("parameter `field` is not of type `FIELD` or `VALUESET`")
  recode
}

as_id <- function(x, multiple = FALSE) {
  if (length(x) == 0)
    return(c())
  if (inherits(x, "sc_schema"))
    return(x$id)
  if (is.character(x) && length(x) == 1)
    return(x)
  if (!multiple)
    stop("invalid id")
  if (is.character(x))
    return(x)
  if (is.list(x))
    return(sapply(x, as_id))
  stop("invalid ids")
}
