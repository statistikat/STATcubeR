#' Turn od_table objects into tidy data frames
#'
#' @description
#' `od_tabulate()` extracts the data in the table and turns it into a tidy
#' data.frame. It is basically a copy of [sc_tabulate()]. Additionally,
#' certain STATcube conventions are applied
#' * zeros are recoded to `NA`s
#' * rounding is done according to the precision of each measure
#'
#' the `...` argument decides which measures and/or fields should be included
#' in the output. If no measures are given, all measures are included. The same
#' is true for fields.
#'
#' @param table An object of class `od_table`
#' @param ... Names of measures and/or fields
#' @param parse_time should field variables of type time be converted
#'   into a `POSIXct` format? (currently not respected)
#' @param recode_zeros turn zero values into `NA`s prior to rounding
#' @param round use the precision of each measure for rounding (currently not
#' respected as this meta-information is not available)
#' @param .list allows to define the arguments for `...` as a character vector.
#' @examples
#' \dontrun{
#' table <- od_table(od_list()$id[1])
#'
#' od_tabulate(table)
#' od_tabulate(table, "Geschlecht", "Staatsangehörigkeit")
#' }
#' @export
od_tabulate <- function (table,
                         ...,
                         .list = NULL,
                         parse_time = TRUE,
                         round = TRUE,
                         recode_zeros = TRUE) {

  . <- NULL
  parse_time <- round <- FALSE

  data <- table$data
  if (!is.null(.list)) {
    col_names <- .list
  } else {
    col_names <- c(...)
  }
  fields <- intersect(col_names, table$meta$fields$label)
  if (length(fields) == 0) {
    fields <- table$meta$fields$label
  }
  measures <- intersect(col_names, table$meta$measures$label)
  if (length(measures) == 0) {
    measures <- table$meta$measures$label
  }
  ind <- rep(TRUE, nrow(data))
  for (field in table$meta$fields$label) {
    if (field %in% fields) {
      ind <- ind & !is.na(data[[field]])
    } else {
      code <- table$meta$fields$code[table$meta$fields == field]
      ind <- ind & data[[field]] == table$field()[[code]]$label[1]
    }
  }

  data <- data[ind, ] %>% .[, c(fields, measures)]
  if (recode_zeros) {
    data[data == 0] <- NA
  }
  if (parse_time) {
    meta_f <- table$meta$fields
    time_vars <- meta_f$label[meta_f$type != "Category"]
    for (field in intersect(time_vars, fields)) {
      i <- which(meta_f$label == field)
      parsed <- table$field(i)$parsed
      data[[field]] <- parsed[data[[field]]]
    }
  }
  if (round) {
    for (measure in measures) {
      meta_m <- table$meta$measures
      precision <- meta_m$precision[meta_m$label == measure]
      if (!is.na(precision)) {
        data[[measure]] <- round(data[[measure]], precision)
      }
    }
  }
  data
}
