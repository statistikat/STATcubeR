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

get_dimnames <- function(fields) {
  dimnames <- list()
  for (i in seq_along(fields)) {
    field <- fields[[i]]
    dimnames[[i]] <- field$items %>% sapply(function(x) x$labels[[1]])
  }
  names(dimnames) <- fields %>% sapply(function(x) x$label)
  dimnames
}

get_fields <- function(x) {
  content <- sc_content(x)
  dims <- content$fields %>%
    lapply(function(x) x$items) %>%
    sapply(length)
  res <- array(0, rev(dims), dimnames = rev(get_dimnames(content$fields))) %>%
    stats::ftable() %>% as.data.frame() %>% (function(x) {
      x[, (ncol(x)-1):1]
    })
  names(res) <- names(get_dimnames(content$fields))
  res
}

#' @export
as.data.frame.STATcube_response <- function(x) {
  content <- sc_content(x)
  df <- get_fields(x)
  for (i in seq_along(content$measures)) {
    label <- content$measures[[i]]$label
    df[[label]] <- unlist(content$cubes[[i]]$values)
    df[[paste0(label, "_a")]] <- get_annotations(x, i)
  }
  df
}

#' @export
as.array.STATcube_response <- function(x, i = 1, ...) {
  content <- sc_content(x)
  first_cube <- content$cubes[[i]]$values
  dims <- content$fields %>% lapply(function(x) x$items) %>% sapply(length)
  labels <- content$fields %>% lapply(function(x) x$label)
  array(unlist(first_cube), rev(dims), dimnames = rev(get_dimnames(content$fields)))
}

#' @export
sc_annotation_legend <- function(x) {
  content <- sc_content(x)
  content$annotationMap
}
