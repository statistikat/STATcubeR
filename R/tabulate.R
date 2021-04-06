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
sc_tabulate <- function(table, ..., .list = NULL, parse_time = TRUE, round = TRUE,
                        recode_zeros = TRUE, annotations = FALSE) {
  data <- table$data
  if (!is.null(.list))
    col_names <- .list
  else
    col_names <- c(...)
  fields <- intersect(col_names, table$meta$fields$label)
  if (length(fields) == 0)
    fields <- table$meta$fields$label
  measures <- intersect(col_names, table$meta$measures$label)
  if (length(measures) == 0)
    measures <- table$meta$measures$label

  ## subset rows and columns
  ind <- rep(TRUE, nrow(data))
  for (field in table$meta$fields$label) {
    if (field %in% fields)
      ind <- ind & !is.na(data[[field]])
    else
      ind <- ind & is.na(data[[field]])
  }
  data <- data[ind, ] %>% .[, c(fields, measures)]

  if (recode_zeros)
    data[data == 0] <- NA
  if (parse_time) {
    meta_f <- table$meta$fields
    time_vars <- meta_f$label[meta_f$type != "Category"]
    for (field in intersect(time_vars, fields)) {
      i <- which(meta_f$label == field)
      parsed <- table$field(i)$parsed
      data[[field]] <- parsed[data[[field]]]
    }
  }
  if (round)
    for (measure in measures) {
      meta_m <- table$meta$measures
      precision <- meta_m$precision[meta_m$label == measure]
      data[[measure]] <- round(data[[measure]], precision)
    }
  if (annotations) {
    for (measure in measures) {
      ann <- attr(table$data[[measure]], "annotations")
      #data[[paste0(measure, "_a")]] <- I(ann[ind])
      data[[paste0(measure, "_a")]] <- sapply(ann[ind], paste, collapse = ", ")
    }
  }
  data
}
