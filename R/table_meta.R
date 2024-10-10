globalVariables(".")

get_var_code <- function(x, split_minus = FALSE) {
  if (is.null(x))
    return("SC_TOTAL")
  res <- utils::tail(strsplit(x, ":")[[1]], 1)
  if (split_minus)
    res <- utils::tail(strsplit(res, "-")[[1]], 1)
  res
}

get_item_code <- function(item, split_minus = FALSE) {
  if (item$type == "Total")
    return("SC_TOTAL")
  stopifnot(item$type == "RecodeItem")
  uris <- item$uris
  codes <- as.character(uris) |>
    strsplit(":") |>
    lapply(utils::tail, 1)
  if (split_minus)
    codes <- as.character(codes) |>
    strsplit("-") |>
    lapply(utils::tail(1))
  paste(codes, collapse = ";")
}

summarize_annotations <- function(content, i) {
  annotations <- content$cubes[[i]]$annotations
  if (is.null(annotations))
    return("")
  freq <- table(unlist(annotations))
  sapply(seq_along(freq), function(i) { paste0(names(freq)[i], "(", as.numeric(freq)[i],
    ")")}) |> paste(collapse = ", ")
}

sc_meta <- function(content) {
  measure_info <- do.call(rbind,lapply(seq_along(content$measures), function(i) {
    measure <- content$measures[[i]]
    data_frame(
      label = measure$label,
      code = get_var_code(measure$measure),
      fun = measure$`function`,
      precision = content$cubes[[i]]$precision,
      annotations = summarize_annotations(content, i),
      NAs = sum(unlist(content$cubes[[i]]$values) == 0)
    )
  }))

  field_info <- do.call(rbind,lapply(content$fields, function(field) {
    has_total <- field$items[[length(field$items)]]$type == "Total"
    data_frame(
      label = field$label,
      code = get_var_code(field$uri),
      nitems = length(field$items),
      type = sc_field_type(field),
      total_code = ifelse(has_total, "SC_TOTAL", NA_character_)
    )
  }))
  db_info <- data_frame(
    label = content$database$label,
    code = content$database$id
  )
  list(source = db_info, measures = measure_info, fields = field_info)
}

sc_meta_field <- function(field) {
  res <- do.call(rbind,lapply(field$items, function(item) {
    data_frame(
      label = paste(item$labels, collapse = ";"),
      code = get_item_code(item)
    )
  }))
  res$parsed <- sc_field_parse(field)
  res
}
