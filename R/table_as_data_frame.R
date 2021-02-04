unlist_n <- function(x, times) {
  x <- unlist(x, recursive = FALSE)
  if (times == 1)
    return(x)
  unlist_n(x, times - 1)
}

get_annotations <- function(x, i = 1) {
  content <- x$raw
  cube <- content$cubes[[i]]$annotations
  if (is.null(cube))
    return("")
  dims <- content$fields %>%
    lapply(function(x) x$items) %>%
    sapply(length)
  unlist_n(cube, length(dims) - 1) %>%
    sapply(function(x) paste(x, collapse = ", "))
}

sc_model_matrix <- function(dims) {
  times <- 1
  each <- prod(dims)
  out <- list()
  for (i in seq_along(dims)) {
    each <- each / dims[i]
    out[[paste0("FIELD_", i)]] <- seq_len(dims[i]) %>%
      rep(times = times, each = each)
    times <- times*dims[i]
  }
  as.data.frame(out)
}

#' @rdname sc_post_json
#' @param x an R object of class `sc_table`
#' @param ... unused
#' @param drop_aggregates remove rows containing aggregates from the table.
#'   The default (`TRUE`) makes the table tidy
#' @param recode_na Recode cells to `NA` if the corresponding annotations have
#'   the value `"X"` (cross tabulation not allowed).
#' @param label_vars Use labels for column names. Alternatively, codes are
#'   used
#' @param parse_fields Parse field columns as `character` or `POSIXct`
#'   depending on labels? Alternatively, codes are used
#' @export
as.data.frame.sc_table <- function(
  x, ..., drop_aggregates = TRUE, recode_na = TRUE, label_vars = TRUE,
  parse_fields = TRUE
) {
  content <- x$raw
  dims_fields <- content$fields %>%
    lapply(function(x) x$items) %>%
    sapply(length)
  df <- sc_model_matrix(dims_fields)
  # labeling of fields
  for (i in seq_along(content$fields)) {
    field <- content$fields[[i]]
    parsed <- switch(parse_fields + 1,
                     sc_field_codes(field, split_minus = FALSE),
                     sc_field_parse(field))
    df[[i]] <- parsed[df[[i]]]
    names(df)[i] <- ifelse(label_vars, field$label, get_var_code(field$uri))
  }
  for (i in seq_along(content$measures)) {
    measure <- content$measures[[i]]
    label <- ifelse(label_vars, measure$label, get_var_code(measure$measure))
    values <- unlist(content$cubes[[i]]$values)
    annotations <- get_annotations(x, i)
    if (recode_na)
      values[annotations == "X"] <- NA
    df[[label]] <- values
    df[[paste0(label, "_a")]] <- annotations
  }
  if (drop_aggregates) {
    for (i in seq_along(content$fields))
      df <- df[!is.na(df[[i]]), ]
    rownames(df) <- NULL
  }
  df
}
