unlist_n <- function(x, times) {
  x <- unlist(x, recursive = FALSE)
  if (times == 1)
    return(x)
  unlist_n(x, times - 1)
}

get_annotations <- function(content, i = 1) {
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

sc_table_create_data <- function(content) {
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
    class(df[[i]]) <- c("sc_field", class(df[[i]]))
  }
  # add measures
  for (i in seq_along(content$measures)) {
    measure <- content$measures[[i]]
    label <- get_var_code(measure$measure)
    values <- unlist(content$cubes[[i]]$values)
    annotations <- get_annotations(content, i)
    df[[label]] <- values
    attr(df[[label]], "annotations") <- annotations
    class(df[[label]]) <- c("sc_measure", class(df[[label]]))
  }
  df
}

#' @export
`[.sc_measure` <- function(x, i) {
  ann <- attr(x, "annotations")
  unclass(x)[i] %>% `attr<-`("annotations", ann[i]) %>% `class<-`(class(x))
}

#' @export
`[.sc_field` <- function(x, i) {
  `class<-`(x, "factor")[i] %>% `class<-`(class(x))
}

#' @export
as.data.frame.sc_data <- function(x, ..., recode_zeros = FALSE) {
  data <- x$data
  if (recode_zeros) {
    measures <- x$meta$measures$code
    data[measures][0 == data[measures]] <- NA
  }
  od_label_data(x, data, ...)
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
