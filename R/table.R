sc_version <- function() {
  toString(utils::packageVersion(utils::packageName()))
}

base_url <- "http://sdbext:8082/statistik.at/ext/statcube/rest/v1"

sc_table_class <- R6::R6Class(
  "sc_table",
  cloneable = FALSE,
  public = list(
    initialize = function(response, json = NULL) {
      stopifnot(inherits(response, "response"))
      if (response$status_code != 200)
        stop(httr::content(response)$message)
      private$httr_response <- response
      private$version <- sc_version()
      if (is.null(json))
        json <- jsonlite::toJSON(
          self$raw$query, auto_unbox = TRUE, pretty = TRUE) %>% toString()
      private$json_content <- sc_json_class$new(json)
    },
    field = function(i = 1) {
      sc_meta_field(self, i)
    }
  ),
  active = list(
    response = function() private$httr_response,
    raw = function() httr::content(self$response),
    meta = function() sc_meta(self),
    data = function(val) {
      if (!missing(val))
        stop("data is read-only", call. = FALSE)
      if (is.null(private$table))
        private$table <- as.data.frame(self)
      private$table
    },
    scr_version = function() private$version,
    annotation_legend = function() sc_annotation_legend(self),
    rate_limit = function() sc_table_rate_limit(self),
    json = function() private$json_content
  ),
  private = list(
    httr_response = NULL,
    table = NULL,
    version = NULL,
    json_content = NULL
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
sc_table <- function(json_file, language = c("en", "de"), key = sc_key()) {
  httr::POST(
    url = paste0(base_url, "/table"),
    body = httr::upload_file(json_file),
    config = sc_headers(language, key)
  ) %>% sc_table_class$new(json = readLines(json_file))
}

#' @export
#' @param filename The name of an example json file.
#' @rdname sc_table
sc_example <- function(filename) {
  example_dir <- system.file(package = utils::packageName(), "json_examples")
  available_examples <- dir(example_dir)
  if (!(filename %in% available_examples))
    stop("Example ", filename, " does not exist. Valid examples are \"",
         paste(available_examples, collapse = "\", \""), "\"")
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
