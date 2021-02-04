globalVariables(".")

get_var_code <- function(x, split_minus = FALSE) {
  if (is.null(x))
    return("")
  res <- utils::tail(strsplit(x, ":")[[1]], 1)
  if (split_minus)
    res <- utils::tail(strsplit(res, "-")[[1]], 1)
  res
}

#' Get metadata for a STATcube table
#'
#' Functions to extract metadata from a `sc_table` object.
#'
#' @param response An object of class `sc_table`
#' @export
sc_meta <- function(response) {
  content <- response$raw
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
      nitems = length(field$items),
      type = sc_field_type(field)
    )
  }) %>% do.call(rbind, .)
  db_info <- data.frame(
    label = content$database$label,
    code = content$database$id
  )
  list(database = db_info, measures = measure_info, fields = field_info)
}

#' @rdname sc_meta
#' @param i index of the field, for which further metadata are desired
#' @examples
#' \dontrun{
#'
#' my_response <- sc_example("bev_seit_1982") %>%
#'   sc_post_json()
#' sc_meta(my_response)
#' sc_meta_field(my_response, 1)
#' }
#' @export
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
