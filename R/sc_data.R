sc_data_class <- R6::R6Class(
  "sc_data",
  cloneable = FALSE,
  public = list(
    initialize = function(data, meta, fields) {
      private$p_data <- data
      private$p_meta <- meta
      private$p_fields <- fields
      private$version <- sc_version()
    },
    field = function(i = 1) {
      if (!is.numeric(i))
        i <- od_match_codes(self$meta$fields, i)
      private$p_fields[[i]]
    },
    tabulate = function(...) {
      od_tabulate(self, ...)
    },
    total_codes = function(...) {
      args <- list(...)
      if (length(args) == 0)
        return(private$p_meta$fields[, c("code", "total_code")])
      keys <- od_match_codes(private$p_meta$fields, names(args), single = FALSE)
      values <- unlist(args)
      for (i in seq_along(keys)) {
        key <- keys[i]
        value <- values[i]
        if (!is.na(value))
          value <- od_match_codes(self$field(key), value, codes = TRUE)
        private$p_meta$fields$total_code[key] <- value
      }
    }
  ),
  active = list(
    data_raw = function() {
      private$p_data
    },
    meta = function() {
      private$p_meta
    },
    data = function() {
      od_label_data(self)
    },
    scr_version = function() {
      private$version
    }
  ),
  private = list(
    version = NULL,
    p_data = NULL,
    p_meta = NULL,
    p_fields = NULL
  )
)
