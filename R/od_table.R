od_table_class <- R6::R6Class(
  classname = "od_table",
  cloneable = FALSE,
  public = list(
    initialize = function(id, language) {
      stime <- Sys.time()
      stopifnot(rlang::is_scalar_character(id))
      private$lang <- language
      private$id <- id
      json <- od_json_get(id)
      private$json <- json
      res <- od_create_data(json, id = id, lang = language)
      if (!is.null(res)) {
        attr(res, "time") <- as.numeric(difftime(Sys.time(), stime, unit = "secs"))
      }
      private$request_time <- stime
      private$cache <- res
      private$version <- sc_version()
      invisible(self)
    },
    field = function(i = 1) {
      private$cache$fields[[i]]
    },
    tabulate = function(...) {
      od_tabulate(self, ...)
    },
    total_codes = function(...) {
      args <- list(...)
      if (length(args) == 0)
        return(self$meta$fields[, c("code", "total_code")])
      stopifnot(all(names(args) %in% self$meta$fields$code))
      for (i in seq_along(args)) {
        arg <- args[[i]]
        j <- match(names(args)[i], self$meta$fields$code)
        stopifnot(is.na(arg) | arg %in% self$field(j)$code)
        private$cache$meta$fields$total_code[j] <- arg
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

#' Create a table-instance for a open-data Dataset (data.statistik.gv.at)
#'
#' [od_table()] returns an `R6`-class object containing all relevant data
#' and metadata information to be used as input for graphSTAT.
#'
#' @param id the name of the data-set that should be accessed
#'
#' @return An object of class `od_table` which contains the return
#'   value of the [httr::POST()] request in `obj$response`. The object also
#'   provides member functions to parse this response object.
#' @export
#' @examples
#' r1 <- od_table(id = "OGD_veste309_Veste309_1")
#'
#' r1$meta
#' r1$field()
#' r1$field(1)
#' r1$data
#'
#' r2 <- od_table(id = "OGD_krebs_ext_KREBS_1")
#' r2$meta
#' r2$field()
#' r2$field(1)
#' r2$data
#'
#' r3 <- od_table(id = "OGD_konjunkturmonitor_KonMon_1")
#' r3$meta
#' r3$field()
#' r3$field(1)
#' r3$data
#'
#' od_table("OGD_krankenbewegungen_ex_LEISTUNGEN_1")
#' od_table("OGD_f1741_HH_Proj_1")
#' od_table("OGD_veste303_Veste203_1")
od_table <- function(id, language = c("en", "de")) {
  language <- match.arg(language)
  od_table_class$new(id = id, language = language)
}

with_wrap <- function(x, lang) {
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
  cat("An object of class \033[1mod_table\033[22m\n\n")
  cat("Database:  ", with_wrap(x$meta$database, lang), "\n")
  cat("Measures:  ", with_wrap(x$meta$measures, lang),"\n")
  cat("Fields:    ", with_wrap(x$meta$fields, lang), "\n\n")
  cat("Request:   ", format(x$times$request), "\n")
  cat("STATcubeR: ", x$scr_version)
}
