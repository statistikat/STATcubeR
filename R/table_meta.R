globalVariables(".")

get_var_code <- function(x, split_minus = FALSE) {
  if (is.null(x))
    return("SC_TOTAL")
  res <- utils::tail(strsplit(x, ":")[[1]], 1)
  if (split_minus)
    res <- utils::tail(strsplit(res, "-")[[1]], 1)
  res
}

summarize_annotations <- function(x, i) {
  annotations <- x$raw$cubes[[i]]$annotations
  if (is.null(annotations))
    return("")
  freq <- table(unlist(annotations))
  sapply(seq_along(freq), function(i) { paste0(names(freq)[i], "(", as.numeric(freq)[i],
    ")")}) %>% paste(collapse = ", ")
}

#' Get metadata for a STATcube table
#'
#' Functions to extract metadata from a `sc_table` object.
#'
#' @param response An object of class `sc_table`
#' @family functions for /table
#' @keywords internal
sc_meta <- function(response) {
  content <- response$raw
  measure_info <- lapply(seq_along(content$measures), function(i) {
    measure <- content$measures[[i]]
    data.frame(
      label = measure$label,
      code = get_var_code(measure$measure),
      fun = measure$`function`,
      precision = content$cubes[[i]]$precision,
      annotations = summarize_annotations(response, i),
      NAs = sum(unlist(content$cubes[[i]]$values) == 0),
      stringsAsFactors = FALSE
    )
  }) %>% do.call(rbind, .)

  field_info <- lapply(content$fields, function(field) {
    has_total <- field$items[[length(field$items)]]$type == "Total"
    data.frame(
      label = field$label,
      code = get_var_code(field$uri),
      nitems = length(field$items),
      type = sc_field_type(field),
      total_code = ifelse(has_total, "SC_TOTAL", NA_character_),
      stringsAsFactors = FALSE
    )
  }) %>% do.call(rbind, .)
  db_info <- data.frame(
    label = content$database$label,
    code = content$database$id,
    stringsAsFactors = FALSE
  )
  list(source = db_info, measures = measure_info, fields = field_info)
}

#' @rdname sc_meta
#' @param i index of the field, for which further metadata are desired
#' @keywords internal
sc_meta_field <- function(response, i = 1) {
  content <- response$raw
  field <- content$fields[[i]]
  res <- lapply(field$items, function(item) {
    data.frame(
      label = item$labels[[1]],
      code = get_var_code(item$uris[[1]])
    )
  }) %>% do.call(rbind, .)
  res$parsed <- sc_field_parse(field)
  res
}
