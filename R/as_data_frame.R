unlist_n <- function(x, times) {
  x <- unlist(x, recursive = FALSE)
  if (times == 1)
    return(x)
  unlist_n(x, times - 1)
}

get_annotations <- function(x, i = 1) {
  content <- sc_content(x)
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

#' @export
as.data.frame.STATcube_response <- function(
  x, ..., drop_aggregates = TRUE, recode_na = TRUE, var_labels = TRUE)
{
  content <- sc_content(x)
  dims_fields <- content$fields %>%
    lapply(function(x) x$items) %>%
    sapply(length)
  df <- sc_model_matrix(dims_fields)
  # labeling of fields
  for (i in seq_along(content$fields)) {
    field <- content$fields[[i]]
    parsed <- sc_field_parse(field)
    df[[i]] <- parsed[df[[i]]]
    names(df)[i] <- ifelse(var_labels, field$label, get_var_code(field$uri))
  }
  for (i in seq_along(content$measures)) {
    measure <- content$measures[[i]]
    label <- ifelse(var_labels, measure$label, get_var_code(measure$measure))
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
