sc_version <- function() {
  toString(utils::packageVersion(utils::packageName()))
}

base_url <- "http://sdbext:8082/statistik.at/ext/statcube/rest/v1"

sc_env <- new.env(parent = emptyenv())

sc_get_last_error <- function() {
  sc_env$last_error
}

sc_set_last_error <- function(x) {
  sc_env$last_error <- x
}

sc_table_class <- R6::R6Class(
  "sc_table",
  cloneable = FALSE,
  public = list(
    initialize = function(response, json = NULL, file = NULL) {
      stopifnot(inherits(response, "response"))
      private$httr_response <- response
      private$version <- sc_version()
      if (is.null(json))
        json <- jsonlite::toJSON(
          self$raw$query, auto_unbox = TRUE, pretty = TRUE) %>% toString()
      private$json_content <- sc_json_class$new(json, file)
    },
    update = function() {
      response <- sc_table_json_post(self$json$content)
      if (response$status_code != 200)
        stop(httr::content(response)$message)
      private$httr_response <- response
      private$cache <- NULL
    },
    field = function(i = 1) {
      cache_id <- paste0("field_", i)
      if (!(cache_id %in% names(private$cache)))
        private$cache[[cache_id]] <- sc_meta_field(self, i)
      private$cache[[cache_id]]
    },
    tabulate = function(...) {
      sc_tabulate(self, ...)
    },
    browse = function() {
      browseURL(paste0(
        "https://statcube.at/statcube/openinfopage?id=",
        self$raw$database$id
      ))
    }
  ),
  active = list(
    response = function() private$httr_response,
    raw = function() httr::content(self$response),
    meta = function() {
      if (is.null(private$cache$meta))
        private$cache$meta <- sc_meta(self)
      private$cache$meta
    },
    data = function(val) {
      if (!missing(val))
        stop("data is read-only", call. = FALSE)
      if (is.null(private$cache$data))
        private$cache$data <- sc_table_create_data(self)
      private$cache$data
    },
    scr_version = function() private$version,
    annotation_legend = function() sc_annotation_legend(self),
    rate_limit = function() sc_table_rate_limit(self),
    json = function() private$json_content
  ),
  private = list(
    httr_response = NULL,
    version = NULL,
    json_content = NULL,
    cache = NULL
  )
)

#' Create a request against the /table endpoint
#' @description
#' Send requests against the **`/table`** endpoint of the STATcube REST API. The
#' requests can use three formats with corresponding functions
#' * `sc_table()` uses a json file downloaded via the STATcube GUI
#' * `sc_table_custom()` uses the ids of a database, measures and fields
#' * `sc_table_saved()` uses a table uri of a saved table.
#'
#' Those three functions all return an object of class `"sc_table"`.
#' @param json_file path to a json file, which was downloaded via the STATcube
#'   gui ("Open Data API Abfrage")
#' @param add_totals Should totals be added for each measure in the json
#'   request?
#' @return An object of class `sc_table` which contains the return
#'   value of the [httr::POST()] request in `obj$response`. The object also
#'   provides member functions to parse this response object.
#' @inheritParams sc_key
#' @param language The language to be used for labeling. `"en"` or `"de"`
#' @family functions for /table
#' @examples
#' my_table <- sc_table(json_file = sc_example("population_timeseries.json"))
#'
#' # print
#' my_table
#'
#' # get matadata for the table
#' my_table$meta
#'
#' # get a data.frame
#' as.data.frame(my_table) %>% head()
#'
#' # get metadata for field 2
#' my_table$field(2)
#' @export
sc_table <- function(json_file, language = c("en", "de"), add_totals = TRUE,
                     key = sc_key()) {
  sc_table_json_post(readLines(json_file), language, add_totals, key) %>%
    sc_table_class$new(file = json_file)
}

#' @export
#' @rdname sc_table
sc_examples_list <- function() {
  example_dir <- system.file(package = utils::packageName(), "json_examples")
  dir(example_dir, pattern = "json")
}

#' @export
#' @param filename The name of an example json file.
#' @rdname sc_table
sc_example <- function(filename) {
  filename <- match.arg(filename, sc_examples_list())
  system.file(package = utils::packageName(), "json_examples", filename)
}

#' @export
print.sc_table <- function(x, ...) {
  content <- x$raw
  cat("An object of class sc_table\n\n")
  cat("Database:     ", content$database$label, "\n")
  cat("Measures:     ", content$measures %>% sapply(function(x) x$label) %>%
        paste(collapse = ", "), "\n")
  cat("Fields:       ", content$fields %>% sapply(function(x) x$label) %>%
        paste(collapse = ", "), "\n\n")
  cat("Request:      ", format(x$response$date), "\n")
  cat("STATcubeR:    ", x$scr_version)
}
