#' Turn od_table objects into tidy data frames
#'
#' @description
#' `od_tabulate()` extracts the data in the table and turns it into a tidy
#' `data.frame`. It is basically a copy of [sc_tabulate()].
#'
#' the `...` argument decides which measures and/or fields should be included
#' in the output. If no measures are given, all measures are included. The same
#' is true for fields.
#'
#' @details
#' Aggregation is done as follows
#'
#' * First, all columns that priovide a total code via `table$total_codes()`
#'   will be used to filter for `column == total_code` or `column != toal_code`
#' * Then, the remaining data is aggregated using [rowSums()]
#'
#' The ellipsis (`...`) supports partial matching of codes, english labels and
#' german labels. See Examples
#'
#' @param table An object of class `od_table`
#' @param ... Names of measures and/or fields
#' @examples
#' table <- od_table("OGD_veste309_Veste309_1")
#'
#' # no arguments -> same output as `table$data`
#' od_tabulate(table)
#'
#' # provide some fields -> aggregate to keep only these fields
#' od_tabulate(table, "Sex", "Citizenship")
#'
#' # provide some measures -> drop all other measures from the output
#' od_tabulate(table, "Arithmetic mean")
#'
#' # mixture of measures and fields  -> keep exactly those columns
#' od_tabulate(table, "Sex", "Arithmetic mean")
#'
#' ## define total codes
#' table$total_codes(
#'   `C-A11-0` = "A11-1",
#'   `C-STAATS-0` = "STAATS-9",
#'   `C-VEBDL-0` = "VEBDL-10",
#'   `C-BESCHV-0` = "BESCHV-1"
#' )
#'
#' # filter for totals in `Region (NUTS2)` and `Form of employment`. Drop totals
#' # in `Sex` and `Citizenship`.
#' od_tabulate(table, "Sex", "Citizenship")
#'
#' ## switch language
#' table$language <- "de"
#' od_tabulate(table, "Sex", "Citizenship")
#'
#' ## `...` matches for codes, english labels and german labels
#' od_tabulate(table, "C-A11-0", "Citizenship", "2. Quartil (Median)")
#'
#' ## Keep totals in the output by removing total codes
#' od_tabulate(table, "C-A11-0")      # -> 2 rows: "male" "female"
#' table$total_codes(`C-A11-0` = NA)
#' od_tabulate(table, "C-A11-0")      # -> 3 rows: "total", "male", "female"
#'
#' ## table$tabulate(...) is an alias for sc_tabulate(table, ...)
#' table$tabulate("C-A11-0")
#' @export
od_tabulate <- function(table, ...) {
  stopifnot(inherits(table, "od_table"))
  codes <- od_match_codes(table, c(...))
  fields <- codes$fields
  measures <- codes$measures
  mf <- table$meta$fields
  if (length(measures) == 0)
    measures <- table$meta$measures$code
  if (length(fields) == 0)
    fields <- mf$code
  x <- table$data_raw
  x <- x[, setdiff(names(x), setdiff(table$meta$measures$code, measures))]
  fields_to_aggregate <- setdiff(mf$code, fields)
  has_total <- mf$code[!is.na(mf$total_code)]
  x <- x[, setdiff(names(x), setdiff(fields_to_aggregate, has_total))]

  aggregate_via_total <- intersect(fields_to_aggregate, has_total)
  for (field_code in aggregate_via_total) {
    i <- match(field_code, mf$code)
    x <- x[x[[field_code]] == mf$total_code[i], ]
  }
  x <- x[, setdiff(names(x), aggregate_via_total)]

  drop_total <- intersect(fields, has_total)
  for (field_code in drop_total) {
    i <- match(field_code, mf$code)
    x <- x[x[[field_code]] != mf$total_code[i], ]
  }

  aggregate_via_sum <- setdiff(fields_to_aggregate, aggregate_via_total)
  if (length(aggregate_via_sum) > 0) {
    grouping_var <- do.call(paste, x[fields])
    x <- cbind(
      subset(x, !duplicated(grouping_var), fields),
      rowsum(x[measures], group = grouping_var, reorder = FALSE)
    )
  }
  od_label_data(table, x)
}

od_match_codes <- function(table, patterns) {
  fields <- table$meta$fields
  measures <- table$meta$measures
  flds <- pmatch(patterns, fields$code)
  msrs <- pmatch(patterns, measures$code)
  flds[is.na(flds)] <- pmatch(patterns[is.na(flds)], fields$label)
  msrs[is.na(msrs)] <- pmatch(patterns[is.na(msrs)], measures$label)
  flds[is.na(flds)] <- pmatch(patterns[is.na(flds)], fields$label_en)
  msrs[is.na(msrs)] <- pmatch(patterns[is.na(msrs)], measures$label_en)

  no_match <- is.na(msrs) & is.na(flds)
  if (any(no_match)) {
    stop("could not match the following patterns: ",
         paste(shQuote(patterns[no_match]), collapse = ", "), call. = FALSE)
  }
  list(
    measures = table$meta$measures$code[stats::na.omit(msrs)],
    fields = table$meta$fields$code[stats::na.omit(flds)]
  )
}

