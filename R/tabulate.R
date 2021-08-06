#' Turn sc_table objects into tidy data frames
#'
#' @description
#' `sc_tabulate()` extracts the data in the table and turns it into a tidy
#' data.frame. Additionaly, certain STATcube conventions are applied
#' * zeros are recoded to `NA`s
#' * rounding is done according to the precision of each measure
#' * time variables are converted into `POSIXct`
#'
#' the `...` argument decides which measures and/or fields should be included
#' in the output. If no measures are given, all measures are included. The same
#' is true for fields.
#'
#' @param table An object of class `sc_table`
#' @param ... Names of measures and/or fields
#' @param parse_time should field variables of type time be converted
#'   into a `POSIXct` format?
#' @param recode_zeros turn zero values into `NA`s prior to rounding
#' @param annotations Include separate annotation columns in the returned
#'   `data.frame`? Those extra columns are of type list and contain character
#'   vectors with all annotations for the corresponding measure
#' @param round use the precision of each measure for rounding?
#' @param .list allows to define the arguments for `...` as a character vector.
#' @param raw If FALSE (the default), apply labeling to the dataset.
#'   Otherwise, return codes.
#' @examples
#' table_tourism <- sc_example("accomodation.json") %>% sc_table("de")
#'
#' sc_tabulate(table_tourism)
#' sc_tabulate(table_tourism, "Saison/Tourismusmonat")
#' sc_tabulate(table_tourism, "Saison/Tourismusmonat", "Ankünfte")
#' sc_tabulate(table_tourism, "Ankünfte")
#'
#' table_trade <- sc_example("foreign_trade.json") %>% sc_table("de")
#' tt <- sc_tabulate(table_trade, "Berichtsjahr", "Import, Wert in Euro",
#'                   annotations = TRUE)
#' tt
#' tt[['Import, Wert in Euro_a']] %>% str()
#' @export
sc_tabulate <- function(table, ..., .list = NULL, parse_time = TRUE,
                        round = TRUE, recode_zeros = TRUE, annotations = FALSE,
                        raw = FALSE) {
  data <- od_tabulate(table, ..., .list = .list, parse_time = parse_time,
                      recode_zeros = recode_zeros, raw = raw)

  codes <- od_tabulate_handle_dots(table, ..., .list = .list)
  measures <- codes$measures

  if (round)
    for (measure in measures) {
      meta_m <- table$meta$measures
      meta_m <- meta_m[meta_m$code == measure, ]
      measure <- ifelse(raw, meta_m$code, meta_m$label)
      data[[measure]] <- round(data[[measure]], meta_m$precision)
    }
  if (annotations) {
    stop("param 'annotations' needs to be re-implemented")
  }

  data
}
