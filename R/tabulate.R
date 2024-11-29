#' Turn sc_data objects into tidy data frames
#'
#' @description
#' [sc_tabulate()] extracts the data in the table and turns it into a tidy
#' data.frame. It applies labeling of the data and transforms time variables
#' into a `Date` format if they satisfy certain 'STATcube' standards.
#'
#' `sc_tabulate(table, ...)` is just an alias for `table$tabulate(...)` and
#' was added so this rather complicated method can have a separate documentation
#' page. It is recommended to use the `table$tabulate()` syntax
#'
#' the `...` argument decides which measures and/or fields should be included
#' in the output. If no measures are given, all measures are included. The same
#' is true for fields.
#'
#' @param table An object of class `sc_data`
#' @param ... Names of measures and/or fields
#' @param parse_time Should time variables be converted into a `Date` format?
#'   Ignored if `raw` is set to `TRUE`.
#' @param recode_zeros turn zero values into `NA`s
#' @param .list allows to define the arguments for `...` as a character vector.
#' @param raw If FALSE (the default), apply labeling to the dataset.
#'   Otherwise, return codes.
#' @param language The language to be used for labeling. By default, the
#'   dataset language (`table$language`) is used.
#' @param sort If `TRUE`, the resulting data will be sorted by all provided
#'   field values
#' @details
#' Aggregation is done as follows
#'
#' * First, all columns that provide a total code via `table$total_codes()`
#'   will be used to filter for `column == total_code` or `column != total_code`
#' * Then, the remaining data is aggregated using [rowsum()]
#'
#' The ellipsis (`...`) supports partial matching of codes and labels.
#' See Examples
#'
#' For objects of class `sc_table` two additional operations are performed.
#' * zeros are recoded to `NA`s
#' * rounding is done according to the precision of each measure. Rounding
#'   happens after the recoding to `NA` values
#' @seealso sc_table_class
#' @examples
#' ############################ OGD Data #######################################
#'
#' table <- od_table("OGD_veste309_Veste309_1")
#'
#' # no arguments -> same output as `table$data`
#' table$tabulate()
#'
#' # provide some fields -> aggregate to keep only these fields
#' table$tabulate("Sex", "Citizenship")
#'
#' # provide some measures -> drop all other measures from the output
#' table$tabulate("Arithmetic mean")
#'
#' # mixture of measures and fields  -> keep exactly those columns
#' table$tabulate("Sex", "Arithmetic mean")
#'
#' ## define total codes
#' table$total_codes(
#'   `C-A11-0` = "A11-1",
#'   `C-STAATS-0` = "STAATS-9",
#'   `C-VEBDL-0` = "VEBDL-10",
#'   `C-BESCHV-0` = "BESCHV-1"
#' )
#'
#' ## alternatively, use partial matching to define totals
#' table$total_codes(
#'   Sex = "Sum total",
#'   Citizenship = "Total",
#'   Region = "Total",
#'   `Form of employment` = "Total"
#' )
#'
#' # filter for totals in `Region (NUTS2)` and `Form of employment`. Drop totals
#' # in `Sex` and `Citizenship`.
#' table$tabulate("Sex", "Citizenship")
#'
#' ## switch language
#' table$language <- "de"
#'
#' ## `...` matches for codes and labels
#' table$tabulate("C-A11-0", "Staats", "2. Quartil (Median)")
#'
#' ## Keep totals in the output by removing total codes
#' table$tabulate("C-A11-0")      # -> 2 rows: "male" "female"
#' table$total_codes(`C-A11-0` = NA)
#' table$tabulate("C-A11-0")      # -> 3 rows: "total", "male", "female"
#'
#' ## table$tabulate(...) is an alias for sc_tabulate(table, ...)
#' sc_tabulate(table, "C-A11-0")
#'
#' ######################## 'STATcube' REST API ################################
#'
#' @examplesIf sc_key_exists()
#' table_tourism <- sc_table(sc_example("accomodation.json"), "de")
#'
#' table_tourism$tabulate()
#' table_tourism$tabulate("Saison/Tourismusmonat")
#' table_tourism$tabulate("Saison/Tourismusmonat", "Ankünfte")
#' table_tourism$tabulate("Ankünfte")
#' @return a `data.frame`
#' @export
sc_tabulate <- function(table, ..., .list = NULL, raw = FALSE,
                        parse_time = TRUE, recode_zeros = inherits(table, "sc_table"),
                        language = NULL, sort = FALSE) {
  table$tabulate(..., .list = .list, raw = raw, parse_time = parse_time,
                 recode_zeros = recode_zeros, language = language, sort = sort)
}

## implementation for class sc_table
sc_table_tabulate <- function(table, ..., .list = NULL, parse_time = TRUE,
                        round = TRUE, recode_zeros = TRUE, sort = FALSE,
                        annotations = FALSE, raw = FALSE, language = NULL) {
  ## use the generic implementation and apply some post-processing
  data <- sc_data_tabulate(table, ..., .list = .list, parse_time = parse_time,
                           recode_zeros = recode_zeros, raw = raw, language = language)

  codes <- od_tabulate_handle_dots(table, ..., .list = .list)
  measures <- codes$measures

  if (is.null(language))
    language <- table$language
  column <- paste0("label_", language)

  if (round)
    for (measure in measures) {
      meta_m <- table$meta$measures
      meta_m <- meta_m[meta_m$code == measure, ]
      measure <- ifelse(raw, meta_m$code, meta_m[[column]])
      data[[measure]] <- round(data[[measure]], meta_m$precision)
    }
  if (annotations) {
    stop("param 'annotations' needs to be re-implemented")
  }

  data
}
