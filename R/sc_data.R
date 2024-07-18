#' @title Common interface for STATcubeR datasets
#'
#' @description
#' This class represents a common interface for datasets returned from the
#' STATcube REST API and OGD datasets. `sc_data` objects are usually created with
#' [od_table()] or [sc_table()].
#' @examples
#' ## create a new sc_data object via od_table()
#' x <- od_table("OGD_krebs_ext_KREBS_1")
#'
#' ## show data
#' x$data
#'
#' ## show metadata
#' x$meta
#' @keywords internal
sc_data <- R6::R6Class(
  "sc_data",
  public = list(
    #' @description
    #' This class is not exported. Use [od_table()] or [sc_table()]
    #' to initialize objects of class `sc_data`.
    #' @param data,meta,fields raw data, metadata and field information.
    #'   Do not use directly but initialize objects with [sc_table()] or
    #'   [od_table()]
    initialize = function(data, meta, fields) {
      meta$source$scr_version <- sc_version()
      meta$source <- sc_tibble_meta(meta$source, "lang")
      meta$measures <- sc_tibble_meta(meta$measures, "NAs")
      meta$fields <- sc_tibble_meta(
        meta$fields, c("total_code", "nitems", "type"))
      private$p_data <- sc_tibble(data)
      private$p_meta <- meta
      for (i in seq_along(fields)) {
        fields[[i]]$visible <- TRUE
        fields[[i]]$order <- seq_len(nrow(fields[[i]]))
      }
      private$p_fields <- fields
      private$version <- sc_version()
      private$recoder <- sc_recoder$new(private)
    },
    #' @description get information about a specific field. The format of
    #'   the return value is similar to `$meta`. A `data.frame` that includes
    #'   codes and labels for each level of the field.
    #' @param i specifier for the field. Integer or character. If an integer
    #'   is provided, it should match the row number in `$meta$fields`. If
    #'   a character is provided, the field is matched using [pmatch()] on
    #'   all available codes and labels.
    #' @examples
    #' x <- od_table("OGD_krebs_ext_KREBS_1")
    #' x$field(1)
    #' x$field("Sex")
    field = function(i = 1) {
      if (!is.numeric(i))
        i <- od_match_codes(self$meta$fields, i)
      field <- private$p_fields[[i]]
      field$label <- switch(self$language, en = field$label_en,
                            de = field$label_de)
      if (is.character(field$parsed))
        field$parsed <- field$label
      sc_tibble_meta(field, "parsed")
    },
    #' @description create a tidy dataset. See [sc_tabulate()] for details.
    #' @param ... arguments that are passed down to [sc_tabulate()]
    #' @examples
    #' x <- od_table("OGD_krebs_ext_KREBS_1")
    #' x$tabulate("Reporting year", "Sex")
    tabulate = function(...) {
      sc_data_tabulate(self, ...)
    },
    #' @param ... key value pairs to define the total codes. Key should be a
    #'   field code and value a code from `$field(i)`. If empty, it will
    #'   return a data.frame with all specified total codes. Keys and values
    #'   can also use labels instead of codes. See examples.
    #' @examples
    #' earnings <- od_table("OGD_veste309_Veste309_1")
    #' earnings$total_codes(Sex = "Sum total", Citizenship = "Total",
    #'                      Region = "Total", `Form of employment` = "Total")
    #' earnings$tabulate("Form of employment")
    #' earnings$tabulate("Sex", "Form of employment")
    total_codes = function(...) {
      args <- list(...)
      if (length(args) == 0)
        return(private$p_meta$fields[, c("code", "total_code")] |>
                 `class<-`(c("tbl", "data.frame")))
      keys <- od_match_codes(private$p_meta$fields, names(args), single = FALSE)
      values <- unlist(args)
      for (i in seq_along(keys)) {
        key <- keys[i]
        value <- values[i]
        if (!is.na(value))
          value <- od_match_codes(self$field(key), value, codes = TRUE)
        private$p_meta$fields$total_code[key] <- value
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field data
    #' the raw data. A data.frame that uses codes for all field variables and
    #' for all column names. To obtain labeled data, use `$tabulate()`.
    data = function() {
      private$p_data
    },
    #' @field language
    #' language to be used for labeling. `"en"` or `"de"`
    language = function(value) {
      if (missing(value)) {
        private$lang
      } else {
        value <- sc_language(value)
        private$lang <- value
      }
    },
    #' @field meta
    #' A list containing metadata about the dataset. It has at least the
    #' following entries
    #'
    #' * __`$source`__ is a dataframe with a single row that contains
    #'   information on the data source.
    #' * __`$measures`__ is a dataframe with one row for every measure in the
    #'   dataset. It contains codes and labels for each measure as well as
    #'   the number of `NAs`. Derived classes might add additional columns.
    #' * __`$fields`__ is a dataframe with one row for every field in the
    #'   dataset. It contains codes and labels for each measure as well as
    #'   the total codes. Derived classes might add additional columns
    meta = function() {
      meta <- private$p_meta
      meta$source$label <- switch(private$lang, en = meta$source$label_en,
                                  de = meta$source$label_de)
      meta$measures$label <- switch(private$lang, en = meta$measures$label_en,
                                    de = meta$measures$label_de)
      meta$fields$label <- switch(private$lang, en = meta$fields$label_en,
                                  de = meta$fields$label_de)
      meta
    },
    #' @field recode
    #' An object of class [sc_recoder] that can be used to change labels
    #'  and perform other recoding operations.
    recode = function() {
      private$recoder
    }
  ),
  private = list(
    version = NULL,
    p_data = NULL,
    p_meta = NULL,
    p_fields = NULL,
    recoder = NULL,
    lang = NULL
  )
)
