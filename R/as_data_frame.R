tarray <- function(x) aperm(x, rev(seq_along(dim(x))))

#' @export
as.array.STATcube_response <- function(x, i = 1, ...) {
  content <- httr::content(x$response)
  first_cube <- content$cubes[[i]]$values
  dims <- content$fields %>% lapply(function(x) x$items) %>% sapply(length)
  labels <- content$fields %>% lapply(function(x) x$label)
  array(unlist(first_cube), rev(dims), dimnames = rev(get_dimnames(content$fields)))
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

#' @export
as.data.frame.STATcube_response <- function(x, ...) {
  content <- httr::content(x$response)
  for (i in seq_along(content$measures)) {
    df <- x %>% as.array(i) %>% tarray() %>% stats::ftable() %>% as.data.frame()
    names(df) <- c(
      names(get_dimnames(content$fields)),
      content$measures[[i]]$label
    )
    if (i == 1)
      ret <- df
    else
      ret <- merge(ret, df)
  }
  ret
}
