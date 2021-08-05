unlist_n <- function(x, times) {
  x <- unlist(x, recursive = FALSE)
  if (times == 1)
    return(x)
  unlist_n(x, times - 1)
}

get_annotations <- function(x, i = 1) {
  content <- x$raw
  cube <- content$cubes[[i]]$annotations
  if (is.null(cube)) {
    n_values <- content$cubes[[i]]$values %>% unlist() %>% length()
    return(rep(list(NULL), n_values))
  }
  dims <- content$fields %>%
    lapply(function(x) x$items) %>%
    sapply(length)
  unlist_n(cube, length(dims) - 1) %>%
    sapply(unlist)
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

sc_table_create_data <- function(x) {
  content <- x$raw
  dims_fields <- content$fields %>%
    lapply(function(x) x$items) %>%
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
    annotations <- get_annotations(x, i)
    df[[label]] <- values
    attr(df[[label]], "annotations") <- annotations
  }
  df
}

#' @export
as.data.frame.sc_table <- function(x, ...) {
  sc_tabulate(x)
}
