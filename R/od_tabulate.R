sc_data_tabulate <- function(table, ..., .list = NULL, raw = FALSE, parse_time = TRUE,
                        recode_zeros = FALSE, language = NULL, sort = FALSE) {
  # coerce arguments
  stopifnot(inherits(table, "sc_data"))
  codes <- od_tabulate_handle_dots(table, ..., .list = .list)
  # extract data
  fields <- codes$fields
  measures <- codes$measures
  mf <- table$meta$fields
  x <- table$data
  unused_measures <- setdiff(table$meta$measures$code, measures)
  x <- x[, setdiff(names(x), unused_measures)]
  if (recode_zeros)
    x[measures][0 == x[measures]] <- NA
  # define aggregation modes
  fields_to_aggregate <- setdiff(mf$code, fields)
  has_total <- mf$code[!is.na(mf$total_code)]
  is_time <- mf$code[mf$type != "Category" & is.na(mf$total_code)]
  aggregate_via_total <- intersect(fields_to_aggregate, has_total)
  aggregate_via_sum <- setdiff(fields_to_aggregate, aggregate_via_total)
  aggregate_via_max_time <- intersect(aggregate_via_sum, is_time)
  aggregate_via_sum <- setdiff(aggregate_via_sum, aggregate_via_max_time)
  # apply aggregation
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

  for (field_code in aggregate_via_max_time) {
    field <- table$field(field_code)
    max_time <- max(field$parsed[field$visible])
    max_time_code <- which(field$parsed == max_time)
    x <- x[as.numeric(x[[field_code]]) == max_time_code, ]
  }

  x <- x[, setdiff(names(x), setdiff(fields_to_aggregate, has_total))]
  if (length(aggregate_via_sum) > 0) {
    grouping_var <- do.call(paste, x[fields])
    x <- sc_tibble(cbind(
      subset(x, !duplicated(grouping_var), fields),
      rowsum(x[measures], group = grouping_var, reorder = FALSE)
    ))
  }
  # post process aggregated data
  for (field_code in fields) {
    field <- table$field(field_code)
    visible <- field$visible
    if (!all(visible))
      x <- x[as.integer(x[[field_code]]) %in% which(visible), ]
    order <- field$order
    if (!identical(order, seq_len(nrow(field)))) {
      x[[field_code]] <- order[x[[field_code]]]
      levels(x[[field_code]]) <- field$code[order(order)]
      class(x[[field_code]]) <- "factor"
    }
  }
  if (sort)
    x <- x[do.call(base::order, x[fields]), ]
  if (!raw)
    x <- od_label_data(table, x, parse_time, language)
  x
}

od_tabulate_handle_dots <- function(table, ..., .list) {
  if (is.null(.list))
    .list <- c(...)
  fields <- od_match_codes(table$meta$fields, .list, dots = TRUE)
  measures <- od_match_codes(table$meta$measures, .list, dots = TRUE)
  no_match <- is.na(fields) & is.na(measures)
  if (any(no_match))
    stop("Unable to match pattern ", shQuote(.list[no_match][1]), call. = FALSE)
  if (all(is.na(measures)))
    measures <- table$meta$measures$code
  if (all(is.na(fields)))
    fields <- table$meta$fields$code
  list(
    measures = measures[!is.na(measures)],
    fields = fields[!is.na(fields)]
  )
}

od_match_codes <- function(dict, patterns, dots = FALSE, codes = dots,
                           require_match = !dots, single = !dots) {
  stopifnot(is.character(patterns) || length(patterns) == 0)
  if (single && length(patterns) != 1)
    stop("Multiple patterns were provided", call. = TRUE)
  matches <- pmatch(patterns, dict$code)
  matches[is.na(matches)] <- pmatch(patterns[is.na(matches)], dict$label_de)
  matches[is.na(matches)] <- pmatch(patterns[is.na(matches)], dict$label_en)
  if (require_match && anyNA(matches))
    stop("Could not match pattern ", shQuote(patterns[is.na(matches)][1]), call. = FALSE)
  if (codes)
    matches <- dict$code[matches]
  matches
}
