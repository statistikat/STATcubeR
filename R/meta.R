globalVariables(".")

get_var_code <- function(x) {
  if (is.null(x))
    return("")
  utils::tail(strsplit(x, ":")[[1]], 1)
}

#' @export
sc_meta <- function(response) {
  content <- sc_content(response)
  measure_info <- lapply(content$measures, function(measure) {
    data.frame(
      label = measure$label,
      code = get_var_code(measure$measure),
      fun = measure$`function`
    )
  }) %>% do.call(rbind, .)

  measure_info$precision <- sapply(content$cubes, function(cube) { cube$precision })

  field_info <- lapply(content$fields, function(field) {
    data.frame(
      label = field$label,
      code = get_var_code(field$uri),
      nitems = length(field$items)
    )
  }) %>% do.call(rbind, .)
  db_info <- data.frame(
    label = content$database$label,
    code = content$database$id
  )
  list(database = db_info, measures = measure_info, fields = field_info)
}

#' @export
sc_meta_field <- function(response, i) {
  content <- sc_content(response)
  field <- content$fields[[i]]
  lapply(field$items, function(item) {
    data.frame(
      label = item$labels[[1]],
      code = get_var_code(item$uris[[1]]),
      type = item$type
    )
  }) %>% do.call(rbind, .)
}
