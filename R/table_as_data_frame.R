unlist_n <- function(x, times) {
  if (times <= 0)
    return(x)
  x <- unlist(x, recursive = FALSE)
  if (times == 1)
    return(x)
  unlist_n(x, times - 1)
}

get_annotations <- function(content, i = 1) {
  cube <- content$cubes[[i]]$annotations
  if (is.null(cube)) {
    n_values <-  length(unlist(content$cubes[[i]]$values))
    return(rep(list(NULL), n_values))
  }
  dims <- sapply(lapply(content$fields,function(x) x$items), length)
  return(sapply(unlist_n(cube, length(dims) - 1), unlist))
}

sc_model_matrix <- function(dims) {
  times <- 1
  each <- prod(dims)
  out <- list()
  for (i in seq_along(dims)) {
    each <- each / dims[i]
    out[[paste0("FIELD_", i)]] <- rep(seq_len(dims[i]),
                                      times = times, each = each)
    times <- times * dims[i]
  }
  vctrs::new_data_frame(out)
}

sc_table_create_data <- function(content) {
  dims_fields <- lapply(content$fields, function(x) x$items) |>
    sapply(length)
  df <- sc_model_matrix(dims_fields)
  # labeling of fields
  for (i in seq_along(content$fields)) {
    field <- content$fields[[i]]
    codes <- sc_field_codes(field, split_minus = FALSE)
    df[[i]] <- factor(df[[i]], labels = codes)
    names(df)[i] <- get_var_code(field$uri)
  }
  # add measures
  for (i in seq_along(content$measures)) {
    measure <- content$measures[[i]]
    label <- get_var_code(measure$measure)
    values <- unlist(content$cubes[[i]]$values)
    annotations <- get_annotations(content, i)
    df[[label]] <- values
    attr(df[[label]], "annotations") <- annotations
  }
  df
}

#' @export
as.data.frame.sc_data <- function(x, ...) {
  od_label_data(x, ...)
}

sc_table_modify_totals <- function(data, meta, meta_fields) {
  ind <- which(meta$fields$type != "Category")
  for (i in ind) {
    mf <- meta_fields[[i]]
    ind_total <- which(mf$code == "SC_TOTAL")
    ind_latest <- which(mf$parsed == max(mf$parsed, na.rm = TRUE))
    numeric_columns <- seq(length(meta_fields) + 1, ncol(data))
    data[as.numeric(data[[i]]) == ind_total, numeric_columns] <-
      data[as.numeric(data[[i]]) == ind_latest, numeric_columns]
  }
  data
}
