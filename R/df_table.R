df_table <- function(x, label = "data") {
  x <- as.data.frame(x)

  measures <- df_table_meta(x, "numeric", "integer")
  fields_m <- df_table_meta(x, "factor", "character") # Date
  fields_m$total_code <- NA_character_
  measures$NAs <- sapply(seq_len(nrow(measures)), function(i) {
    label <- measures$label[i]
    sum(is.na(x[[label]]))
  })

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

  x <- sc_data$new(
    data = x,
    meta = list(
      source = data.frame(code = substr(sc_checksum(x), 1, 10),
                          label = label, stringsAsFactors = FALSE),
      measures = measures, fields = fields_m),
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
  cat("STATcubeR: ", x$meta$source$scr_version, "\n")
}
