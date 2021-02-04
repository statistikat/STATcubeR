sc_version <- function() {
  toString(utils::packageVersion(utils::packageName()))
}

base_url <- "http://sdbext:8082/statistik.at/ext/statcube/rest/v1"

sc_table_class <- R6::R6Class(
  "sc_table",
  cloneable = FALSE,
  public = list(
    initialize = function(response) {
      stopifnot(inherits(response, "response"))
      if (response$status_code != 200)
        stop(httr::content(response)$message)
      private$httr_response <- response
      private$version <- sc_version()
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
    scr_version = function() private$version
  ),
  private = list(
    httr_response = NULL,
    table = NULL,
    version = NULL
  )
)

#' Create a request against the /table endpoint
#'
#' retrieve a response from STATcube based on a json file
#' @param json_file path to a json file, which was downloaded via the STATcube
#'   gui ("Open Data API Abfrage")
#' @return An object of class `sc_table` which contains the return
#'   value of [httr::POST()]
#' @inheritParams sc_key
#' @param language The language to be used for labeling. `"en"` or `"de"`
#' @examples
#' \dontrun{
#' my_table <- sc_table_json(sc_example("LGR01.json"))
#'
#' # print
#' my_table
#'
#' # get matadata for the table
#' my_table$meta
#'
#' # get a data.frame
#' as.data.frame(my_table)
#'
#' # get metadata for field 1
#' my_table$field(1)
#' }
#' @export
sc_table <- function(json_file, language = c("en", "de"), key = sc_key()) {
  httr::POST(
    url = paste0(base_url, "/table"),
    body = httr::upload_file(json_file),
    config = httr::add_headers(
      APIKey = key,
      `Accept-Language` = match.arg(language)
    )
  ) %>% sc_table_class$new()
}

#' @export
#' @param filename The name of an example json file.
#' @rdname sc_table
sc_example <- function(filename) {
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
