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

#' @title  Class for /table responses
#' @description R6 Class for all responses of the /table endpoint of the
#'   STATcube REST API.
#' @keywords internal
sc_table_class <- R6::R6Class(
  "sc_table",
  cloneable = FALSE,
  inherit = sc_data,
  public = list(
    #' @description Ususally, objects of class `sc_table` are generated with
    #'   one of the factory methods [sc_table()], [sc_table_saved()] or
    #'   [sc_table_custom()]. If this constructor is invoked directly,
    #'   either omit the parameters `json` and `file` or make sure that they
    #'   match with `response`.
    #' @param response a response from `httr::POST()` against the /table
    #'   endpoint.
    #' @param json the json file used in the request as a string.
    #' @param file the file path to the json file
    initialize = function(response, json = NULL, file = NULL) {
      stopifnot(inherits(response, "response"))
      private$httr_response <- response
      if (is.null(json) && is.null(file))
        json <- jsonlite::toJSON(
          self$raw$query, auto_unbox = TRUE, pretty = TRUE) %>% toString()
      private$json_content <- sc_json_class$new(json, file)

      meta <- sc_meta(self)
      super$initialize(
        data = sc_table_create_data(self),
        meta = meta,
        field = lapply(seq_len(nrow(meta$fields)), function(i) {
          sc_meta_field(self, i)
        })
      )
    },
    #' @description Update the data by re-sending the json to the API. This
    #'   is still experimental and could break the object in case new levels
    #'   were added to one of the fields. For example, if a new entry is
    #'   added to a timeseries
    update = function() {
      response <- sc_table_json_post(self$json$content)
      if (response$status_code != 200)
        stop(httr::content(response)$message)
      self$initialize(response, self$json$content, self$json$file)
    },
    #' @description An extension of [sc_tabulate()] with additional
    #'   parameters.
    #' @param ... Parameters which are passed down to [sc_tabulate()]
    #' @param round apply rounding to each measure accoring to the precision
    #'   provided by the API.
    #' @param annotations Include separate annotation columns in the returned
    #'   table. This parameter is currently broken and needs to be re-implemented
    tabulate = function(..., round = TRUE, annotations = FALSE) {
      sc_table_tabulate(self, ..., round = round, annotations = annotations)
    },
    #' @description open the dataset in a browser
    browse = function() {
      browseURL(paste0(
        "https://statcube.at/statcube/openinfopage?id=",
        self$raw$database$id
      ))
    }
  ),
  active = list(
    #' @field response
    #' the httr response
    response = function() private$httr_response,
    #' @field raw
    #' the raw response content
    raw = function() httr::content(self$response),
    #' @field annotation_legend
    #' list of all annotations occuring in the data
    annotation_legend = function() sc_annotation_legend(self),
    #' @field rate_limit
    #' how much requests were left after the POST request for this table was sent?
    rate_limit = function() sc_table_rate_limit(self),
    #' @field json
    #' an object of class `sc_json` based the json file used in the request
    json = function() private$json_content
  ),
  private = list(
    httr_response = NULL,
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
#' @param add_totals Should totals be added for each measure in the json
#'   request?
#' @return An object of class `sc_table` which contains the return
#'   value of the [httr::POST()] request in `obj$response`. The object also
#'   provides member functions to parse this response object. See
#'   [sc_table_class] for the class documentation.
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
  sc_table_json_post(readLines(json_file, warn = FALSE), language, add_totals, key) %>%
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
  cat("Database:     ", content$source$label, "\n")
  cat("Measures:     ", content$measures %>% sapply(function(x) x$label) %>%
        paste(collapse = ", "), "\n")
  cat("Fields:       ", content$fields %>% sapply(function(x) x$label) %>%
        paste(collapse = ", "), "\n\n")
  cat("Request:      ", format(x$response$date), "\n")
  cat("STATcubeR:    ", x$meta$source$scr_version)
}
