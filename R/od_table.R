#' @title Create a table-instance from an open-data dataset
#'
#' @description
#'
#' `od_table(id)` returns an `R6`-class object containing all relevant data
#' and metadata from either https://data.statistik.gv.at/data/ or a locally
#' generated json-file (using `STATgraphUtils::od_generator()`. In the later
#' case it is assumed that all related files are located in the same folder.
#'
#' @section Components:
#'
#' **Component**   | **Corresponding File on Server** | **Type**
#' --------------  | ---------------------------------|----------
#' `$data         `| `https://data.statistik.gv.at/data/${id}.csv`         | `remote`
#' `$header       `| `https://data.statistik.gv.at/data/${id}_HEADER.csv`  | `remote`
#' `$field(code)  `| `https://data.statistik.gv.at/data/${id}_${code}.csv` | `remote`
#' `$json         `| `https://data.statistik.gv.at/ogd/json?dataset=${id}` | `remote`
#' `$data         `| `/path/to/{id}.csv`                                   | `local`
#' `$header       `| `/path/to/{id}_HEADER.csv`                            | `local`
#' `$field(code)  `| `/path/to/{id}_${code}.csv`                           | `local`
#' `$json         `| `/path/to/{id}.json`                                  | `local`
#'
#' @param id the id of the data-set that should be accessed
#' @param language language to be used for labeling. `"en"` or `"de"`
#'
#' @return
#' The returned objects is of class `sc_table` and inherits several parsing
#' methods from [sc_data]. See [od_table_class] for the full class
#' documentation.
#'
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
#' x$data
#' x$tabulate()
#'
#' ## tabulation: see `?sc_tabulate` for more examples
#' x$tabulate("Reporting year", "Sex")
#'
#' ## switch language
#' x$language <- "de"
#' x
#' x$tabulate()
#'
#' ## other interesting tables
#' od_table("OGD_veste309_Veste309_1")
#' od_table("OGD_konjunkturmonitor_KonMon_1")
#' od_table("OGD_krankenbewegungen_ex_LEISTUNGEN_1")
#' od_table("OGD_f1741_HH_Proj_1")
#' od_table("OGD_veste303_Veste203_1")
#' @export
od_table <- function(id, language = c("en", "de")) {
  od_table_class$new(id, language)
}

#' @title Create a table-instance from an open-data dataset
#'
#' @description R6 Class open data datasets.
#' @keywords internal
od_table_class <- R6::R6Class(
  classname = "od_table",
  inherit = sc_data,
  cloneable = FALSE,
  public = list(
    #' @description This class is not exported. Use [od_table()] to
    #' initialize objects of class `od_table`.
    #' @param id the id of the data-set that should be accessed
    #' @param language language to be used for labeling. `"en"` or `"de"`
    initialize = function(id, language = c("en", "de")) {
      language <- match.arg(language)
      stime <- Sys.time()
      stopifnot(rlang::is_scalar_character(id))
      is_local <- file.exists(id) # falls true, ist id ein json-file
      if (is_local) {
        stopifnot(tools::file_ext(id) == "json")
        private$id <- tools::file_path_sans_ext(basename(id))
      } else {
        private$id <- id
      }
      json <- od_json(id)
      private$p_json <- json
      res <- od_create_data(id, json, language)
      private$cache <- res[c("header", "resources")]
      res$meta$source$requested <- stime
      res$meta$source$lang <- language
      super$initialize(res$data, res$meta, res$fields)
      self$language <- language
      invisible(self)
    }
  ),
  active = list(
    #' @field json
    #' parsed version of `https://data.statistik.gv.at/ogd/json?dataset=${id}`
    json = function() {
      private$p_json
    },
    #' @field header
    #' parsed version of `https://data.statistik.gv.at/data/${id}_HEADER.csv`.
    #'
    #' Similar contents can be found in `$meta`.
    header = function() {
      private$cache$header %>% sc_tibble_meta(c("label_de", "label_en"))
    },
    #' @field resources
    #' lists all files downloaded from the server to contruct this table
    resources = function() {
      private$cache$resources %>% `class<-`(c("tbl", "data.frame"))
    }
  ),
  private = list(
    id = NULL,
    p_json = NULL,
    cache = NULL
  )
)

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
  cat("Dataset    ", with_wrap(x$meta$source$label), "\n")
  cat("Measures   ", with_wrap(x$meta$measures$label),"\n")
  cat("Fields     ", with_wrap(x$meta$fields$label), "\n\n")
  cat("Request    ", format(x$meta$source$requested), "\n")
  cat("STATcubeR  ", x$meta$source$scr_version, "\n")
}
