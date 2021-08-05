df_table <- function(x) {
  x <- as.data.frame(x)

  measures <- df_table_meta(x, "numeric", "integer")
  fields_m <- df_table_meta(x, "factor", "character") # Date
  fields_m$total_code <- NA_character_

  names(x) <- seq_along(x)

  fields <- lapply(seq_len(nrow(fields_m)), function(i) {
    col <- as.numeric(fields_m$code[[i]])
    data.frame(
      code = as.character(seq_along(unique(x[[col]]))),
      label = as.character(unique(x[[col]])),
      parsed = as.character(unique(x[[col]])),
      stringsAsFactors = FALSE
    )
  })

  for (i in which(sapply(x, is.character)))
    x[[i]] <- factor(x[[i]], unique(x[[i]]), seq_along(unique(x[[i]])))
  for (i in which(sapply(x, is.factor)))
    levels(x[[i]]) <- seq_along(unique(x[[i]]))

  x <- sc_data_class$new(
    data = x,
    meta = list(measures = measures, fields = fields_m),
    fields = fields
  )
  class(x) <- c("df_table", class(x))
  x
}

df_table_meta = function(x, ...) {
  classes <- c(...)
  col_classes <- lapply(x, class) %>% sapply(utils::head, 1)
  ind <- which(col_classes %in% classes)
  if (length(ind) == 0) {
    data.frame(code = character(0), label = character())
  } else {
    data.frame(
      code = as.character(ind),
      label = names(x)[ind],
      stringsAsFactors = FALSE
    )
  }
}

print.df_table <- function(x, ...) {
  cat("An object of class df_table\n\n")
  cat("Measures:  ", with_wrap(x$meta$measures$label),"\n")
  cat("Fields:    ", with_wrap(x$meta$fields$label), "\n\n")
  cat("STATcubeR: ", x$scr_version, "\n")
}
