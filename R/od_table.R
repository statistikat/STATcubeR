od_table_class <- R6::R6Class(
  classname = "od_table",
  cloneable = FALSE,
  public = list(
    initialize = function(id, language) {
      stime <- Sys.time()
      stopifnot(rlang::is_scalar_character(id))
      private$lang <- language
      private$id <- id
      json <- od_json(id)
      private$json <- json
      res <- od_create_data(id, json, language)
      if (!is.null(res)) {
        attr(res, "time") <- as.numeric(difftime(Sys.time(), stime, unit = "secs"))
      }
      private$request_time <- stime
      private$cache <- res
      private$version <- sc_version()
      invisible(self)
    },
    field = function(i = 1) {
      if (!is.numeric(i))
        i <- od_match_codes(self$meta$fields, i)
      private$cache$fields[[i]]
    },
    tabulate = function(...) {
      od_tabulate(self, ...)
    },
    total_codes = function(...) {
      args <- list(...)
      if (length(args) == 0)
        return(self$meta$fields[, c("code", "total_code")])
      keys <- od_match_codes(self$meta$fields, names(args), single = FALSE)
      values <- unlist(args)
      for (i in seq_along(keys)) {
        key <- keys[i]
        value <- values[i]
        if (!is.na(value))
          value <- od_match_codes(self$field(key), value, codes = TRUE)
        private$cache$meta$fields$total_code[key] <- value
      }
    }
  ),
  active = list(
    raw = function() {
      private$json
    },
    meta = function() {
      private$cache$meta
    },
    data_raw = function() {
      private$cache$data
    },
    data = function() {
      od_label_data(self)
    },
    header = function() {
      private$cache$header
    },
    scr_version = function() private$version,
    times = function() { list(
      request = private$request_time
    )},
    language = function(value) {
      if (missing(value))
        private$lang
      else {
        value <- match.arg(value, c("en", "de"))
        private$lang <- value
        for (i in seq_along(private$cache$fields)) {
          field <- private$cache$fields[[i]]
          if (is.character(field$parsed)) {
            private$cache$fields[[i]]$parsed <- od_get_labels(field, value)
          }
        }
      }
    },
    resources = function() {
      private$cache$resources
    }
  ),
  private = list(
    id = NULL,
    create_time = NULL,
    request_time = NULL,
    json = NULL,
    cache = NULL,
    lang = NULL,
    time = function() {
      attributes(private$cache)$time
    },
    version = NULL
  )
)

#' Create a table-instance from an open-data dataset
#'
#' @description
#'
#' [od_table()] returns an `R6`-class object containing all relevant data
#' and metadata from data.statistik.gv.at.
#'
#' * `$data_raw` contains the contents of `{id}.csv`
#' * `$meta` includes information from `{id}_HEADER.csv`
#' * `$field(i)` contains information from `{id}_{field_code}.csv`
#'
#' @param id the name of the data-set that should be accessed
#' @param language the language to be used for labelling data: `"en"` or `"de"`
#'
#' @details
#' The returned abject also provides certain functionalities to label and
#' aggreate the dataset.
#' * `object$data` takes `object$data_raw` and applies labels based on
#'   the metadata.
#' * `object$tabulate()` also applies labeling and aggregates the
#'   dataset. See [od_tabulate()] for more information.
#' * `object$language` can be used to get or set the language of the dataset.
#'   This affects the labelling behavior
#' @export
#' @examples
#' x <- od_table("OGD_krebs_ext_KREBS_1")
#'
#' ## metadata
#' x
#' x$meta
#' x$field("Sex")
#' x$field(3)
#'
#' ## data
#' x$data_raw %>% head()
#' x$data     %>% head()
#'
#' ## switch language
#' x$language <- "de"
#' x
#' x$data     %>% head()
#'
#' ## tabulation: see `?od_tabulate` for more examples
#' x$tabulate("Reporting year", "Sex") %>% head()
#'
#' ## other interesting tables
#' od_table("OGD_veste309_Veste309_1")
#' od_table("OGD_konjunkturmonitor_KonMon_1")
#' od_table("OGD_krankenbewegungen_ex_LEISTUNGEN_1")
#' od_table("OGD_f1741_HH_Proj_1")
#' od_table("OGD_veste303_Veste203_1")
od_table <- function(id, language = c("en", "de")) {
  language <- match.arg(language)
  od_table_class$new(id = id, language = language)
}

with_wrap <- function(x, lang, label = TRUE) {
  if (label)
    x <- od_get_labels(x, lang)
  if (length(x) > 10)
    x <- c(x[1:10], "...")
  x <- paste(x, collapse = ", ")
  strwrap(x, width = getOption("width") - 12, exdent = 12) %>%
    paste(collapse = "\n")
}

#' @export
print.od_table <- function(x, ...) {
  col_lang <- ifelse(x$language == "de", "label", "label_en")
  lang <- x$language
  cat("An object of class od_table\n\n")
  cat("Database:  ", with_wrap(x$meta$database, lang), "\n")
  cat("Measures:  ", with_wrap(x$meta$measures, lang),"\n")
  cat("Fields:    ", with_wrap(x$meta$fields, lang), "\n\n")
  cat("Request:   ", format(x$times$request), "\n")
  cat("STATcubeR: ", x$scr_version, "\n")
}
