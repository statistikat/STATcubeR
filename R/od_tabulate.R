sc_data_tabulate <- function(table, ..., .list = NULL, raw = FALSE, parse_time = TRUE,
                        recode_zeros = FALSE) {
  stopifnot(inherits(table, "sc_data"))
  codes <- od_tabulate_handle_dots(table, ..., .list = .list)
  fields <- codes$fields
  measures <- codes$measures
  mf <- table$meta$fields
  x <- table$data
  x <- x[, setdiff(names(x), setdiff(table$meta$measures$code, measures))]
  fields_to_aggregate <- setdiff(mf$code, fields)
  has_total <- mf$code[!is.na(mf$total_code)]
  x <- x[, setdiff(names(x), setdiff(fields_to_aggregate, has_total))]
  if (recode_zeros)
    x[measures][0 == x[measures]] <- NA
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
    ) %>% sc_tibble()
  }
  if (!raw)
    x <- od_label_data(table, x, parse_time)
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
  matches[is.na(matches)] <- pmatch(patterns[is.na(matches)], dict$label)
  if (require_match && anyNA(matches))
    stop("Could not match pattern ", shQuote(patterns[is.na(matches)][1]), call. = FALSE)
  if (codes)
    matches <- dict$code[matches]
  matches
}
