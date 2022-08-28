sc_version <- function() {
  pd <- utils::packageDescription("STATcubeR")
  version <- pd$Version
  if (!is.null(pd$RemoteSha))
    version <- paste0(version, " (@", substr(pd$RemoteSha, 1, 7), ")")
  version
}

base_url <- function(server = "ext") {
  sprintf("http://sdb%s:8082/statistik.at/%s/statcube/rest/v1", server, server)
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
    #' @param response a response from [httr::POST()] against the /table
    #'   endpoint.
    #' @param json the json file used in the request as a string.
    #' @param file the file path to the json file
    #' @param add_totals was the json request modified by adding totals via
    #'   the add_toals parameter in one of the factory functions (`sc_table()`,
    #'   `sc_table_custom()`). Necessary, in order to also request totals via
    #'   the `$add_language()` method.
    initialize = function(response, json = NULL, file = NULL, add_totals = FALSE) {
      stopifnot(inherits(response, "response"))
      private$httr_response <- response
      content <- httr::content(response)

      if (is.null(json) && is.null(file))
        json <- jsonlite::toJSON(
          content$query, auto_unbox = TRUE, pretty = TRUE) %>% toString()
      private$json_content <- sc_json_class$new(json, file, add_totals)

      meta <- sc_meta(content)
      meta$source$lang <- response$headers$`content-language`
      meta$source$label_de <- meta$source$label
      meta$source$label_en <- meta$source$label
      meta$fields$label_de <- meta$fields$label
      meta$fields$label_en <- meta$fields$label
      meta$measures$label_de <- meta$measures$label
      meta$measures$label_en <- meta$measures$label

      data <- sc_table_create_data(content)
      meta_fields <- lapply(seq_len(nrow(meta$fields)), function(i) {
        field <- sc_meta_field(content$fields[[i]])
        field$label_de <- field$label
        field$label_en <- field$label
        field
      })
      if (!all(meta$fields$type == "Category"))
        data <- sc_table_modify_totals(data, meta, meta_fields)

      super$initialize(
        data = data,
        meta = meta,
        field = meta_fields
      )
      private$lang <- response$headers$`content-language`
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
        self$meta$source$code
      ))
    },
    #' @description add a second language to the dataset
    #' @param language a language to add. `"en"` or `"de"`.
    #' @param key an API key
    add_language = function(language = c("en", "de"), key = NULL) {
      language <- match.arg(language)
      response <- sc_table_json_post(self$json$content, language = language,
                                     key = key, add_totals = self$json$totals)
      content <- httr::content(response)
      column <- paste0("label_", language)
      private$p_meta$source[[column]] <- content$database$label
      private$p_meta$measures[[column]] <- sapply(content$measures, function(x) x$label)
      private$p_meta$fields[[column]] <- sapply(content$fields, function(x) x$label)
      for (i in seq_along(private$p_fields)) {
        private$p_fields[[i]][[column]] <- sapply(
          content$fields[[i]]$items, function(item) { item$labels[[1]] })
      }
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
    #' list of all annotations occuring in the data as a `data.frame` with
    #' two columns for the annotation keys and annotation labels.
    annotation_legend = function() {
      am <- self$raw$annotationMap
      data.frame(annotation = names(am), label = unlist(am), row.names = NULL)
    },
    #' @field rate_limit
    #' how much requests were left after the POST request for this table was sent?
    #' Uses the same format as [sc_rate_limit_table()].
    rate_limit = function() {
      headers <- self$response$headers
      res <- data.frame(
        remaining = headers$`x-ratelimit-remaining-table`,
        limit     = headers$`x-ratelimit-table`,
        reset     = headers$`x-ratelimit-reset-table`,
        stringsAsFactors = FALSE
      )
      class(res) <- "sc_rate_limit_table"
      res
    },
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
#' * [sc_table()] uses a json file downloaded via the STATcube GUI
#' * [sc_table_custom()] uses the ids of a database, measures and fields
#' * [sc_table_saved()] uses a table uri of a saved table.
#'
#' Those three functions all return an object of class `"sc_table"`.
#' @param json_file path to a json file, which was downloaded via the STATcube
#'   GUI ("Open Data API Abfrage")
#' @param add_totals Should totals be added for each measure in the json
#'   request?
#' @return An object of class `sc_table` which contains the return
#'   value of the [httr::POST()] request in `obj$response`. The object also
#'   provides member functions to parse this response object. See
#'   [sc_table_class] for the class documentation.
#' @inheritParams sc_key
#' @param language The language to be used for labeling. `"en"` or `"de"`.
#'   The third option `"both"` will import both languages by sending two requests
#'   to the `/table` endpoint.
#' @family functions for /table
#' @examplesIf sc_key_exists()
#' my_table <- sc_table(json_file = sc_example("population_timeseries.json"))
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
#' # get metadata for field 2
#' my_table$field(2)
#'
#' sc_table_custom(
#'   db = "str:database:detouextregsai",
#'   measures = c(
#'     "str:statfn:detouextregsai:F-DATA1:F-ANK:SUM",
#'     "str:measure:detouextregsai:F-DATA1:F-UEB"
#'   ),
#'   dimensions = c(
#'     "str:field:detouextregsai:F-DATA1:C-SDB_TIT-0",
#'     "str:valueset:detouextregsai:F-DATA1:C-C93-2:C-C93SUM-0"
#'   )
#' )
#'
#' # get the ids and labels of all saved tables
#' (saved_tables <- sc_table_saved_list())
#' table_uri <- saved_tables$id[1]
#'
#' # get a table based on one of these ids
#' my_response <- sc_table_saved(table_uri)
#' as.data.frame(my_response)
#' @export
sc_table <- function(json_file, language = c("en", "de", "both"), add_totals = TRUE,
                     key = NULL) {
  language <- match.arg(language)
  both <- language == "both"
  if (both)
    language <- "de"
  res <- sc_table_json_post(readLines(json_file, warn = FALSE), language, add_totals, key) %>%
    sc_table_class$new(file = json_file, add_totals = add_totals)
  if (both)
    res$add_language("en", key)
  res
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
  cat("An object of class sc_table\n\n")
  cat("Database   ", x$meta$source$label, "\n")
  cat("Measures   ", with_wrap(x$meta$measures$label), "\n")
  cat("Fields     ", with_wrap(x$meta$fields$label), "\n\n")
  cat("Request    ", format(x$response$date), "\n")
  cat("STATcubeR  ", x$meta$source$scr_version)
}
