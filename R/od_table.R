od_table_class <- R6::R6Class(
  classname = "od_table",
  inherit = sc_data_class,
  cloneable = FALSE,
  public = list(
    initialize = function(id, language) {
      stime <- Sys.time()
      stopifnot(rlang::is_scalar_character(id))
      private$id <- id
      json <- od_json(id)
      private$json <- json
      res <- od_create_data(id, json, language)
      private$request_time <- stime
      private$cache <- res[c("header", "resources")]
      super$initialize(res$data, res$meta, res$fields)
      self$language <- language
      invisible(self)
    }
  ),
  active = list(
    raw = function() {
      private$json
    },
    header = function() {
      private$cache$header
    },
    times = function() { list(
      request = private$request_time
    )},
    language = function(value) {
      if (missing(value))
        private$lang
      else {
        value <- match.arg(value, c("en", "de"))
        private$lang <- value
        private$p_meta$database$label <- od_get_labels(private$p_meta$database)
        private$p_meta$measures$label <- od_get_labels(private$p_meta$measures)
        private$p_meta$fields$label <- od_get_labels(private$p_meta$fields)
        for (i in seq_along(private$p_fields)) {
          field <- private$p_fields[[i]]
          private$p_fields[[i]]$label <- od_get_labels(field, value)
          if (is.character(field$parsed)) {
            private$p_fields[[i]]$parsed <- od_get_labels(field, value)
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
    lang = NULL
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

with_wrap <- function(x) {
  if (length(x) > 10)
    x <- c(x[1:10], "...")
  x <- paste(x, collapse = ", ")
  strwrap(x, width = getOption("width") - 12, exdent = 12) %>%
    paste(collapse = "\n")
}

#' @export
print.od_table <- function(x, ...) {
  cat("An object of class od_table\n\n")
  cat("Database:  ", with_wrap(x$meta$database$label), "\n")
  cat("Measures:  ", with_wrap(x$meta$measures$label),"\n")
  cat("Fields:    ", with_wrap(x$meta$fields$label), "\n\n")
  cat("Request:   ", format(x$times$request), "\n")
  cat("STATcubeR: ", x$scr_version, "\n")
}
