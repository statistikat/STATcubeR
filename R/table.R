sc_version <- function(sha = TRUE) {
  pd <- utils::packageDescription("STATcubeR")
  version <- pd$Version
  if (sha && !is.null(pd$RemoteSha))
    version <- paste0(version, " (@", substr(pd$RemoteSha, 1, 7), ")")
  version
}

base_url <- function(server = "ext") {
  stopifnot(is.character(server), length(server) == 1)
  if (server == "ext")
    return("https://statcubeapi.statistik.at/statistik.at/ext/statcube/rest/v1")
  if (!in_stat())
    warning("Trying to use an internal STATcube API Server")
  if (server == "test")
    return("https://statcubeapit.statistik.local/statistik.at/lxdev/statcube/rest/v1")
  sprintf("http://sdb%s:8082/statistik.at/%s/statcube/rest/v1", server, server)
}

#' @title  Class for /table responses
#' @description R6 Class for all responses of the /table endpoint of the
#'   'STATcube' REST API.
#' @keywords internal
sc_table_class <- R6::R6Class(
  "sc_table",
  cloneable = FALSE,
  inherit = sc_data,
  public = list(
    #' @description Usually, objects of class `sc_table` are generated with
    #'   one of the factory methods [sc_table()], [sc_table_saved()] or
    #'   [sc_table_custom()]. If this constructor is invoked directly,
    #'   either omit the parameters `json` and `file` or make sure that they
    #'   match with `response`.
    #' @param response a response from [httr::POST()] against the /table
    #'   endpoint.
    #' @param json the json file used in the request as a string.
    #' @param file the file path to the json file
    #' @param add_totals was the json request modified by adding totals via
    #'   the add_totals parameter in one of the factory functions (`sc_table()`,
    #'   `sc_table_custom()`). Necessary, in order to also request totals via
    #'   the `$add_language()` method.
    initialize = function(response, json = NULL, file = NULL, add_totals = FALSE) {
      stopifnot(inherits(response, "response"))
      private$httr_response <- response
      content <- httr::content(response)

      if (is.null(json) && is.null(file))
        json <- jsonlite::toJSON(
          content$query, auto_unbox = TRUE, pretty = TRUE) |> toString()
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
    #' @param round apply rounding to each measure according to the precision
    #'   provided by the API.
    #' @param annotations Include separate annotation columns in the returned
    #'   table. This parameter is currently broken and needs to be re-implemented
    #' @param recode_zeros interpret zero values as missings?
    tabulate = function(..., round = FALSE, annotations = FALSE, recode_zeros = FALSE) {
      sc_table_tabulate(self, ..., round = round, annotations = annotations,
                        recode_zeros = recode_zeros)
    },
    #' @description open the dataset in a browser
    browse = function() {
      sc_json_get_server(self$json$content) |> sc_url_gui() |>
        paste0("openinfopage?id=", self$meta$source$code) |> sc_url()
    },
    #' @description add a second language to the dataset
    #' @param language a language to add. `"en"` or `"de"`.
    #' @param key an API key
    add_language = function(language = NULL, key = NULL) {
      language <- sc_language(language)
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
      attr(private$httr_response, "sc_cache_file") <- c(
        attr(private$httr_response, "sc_cache_file"), attr(response, "sc_cache_file")
      )
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
    #' list of all annotations occurring in the data as a `data.frame` with
    #' two columns for the annotation keys and annotation labels.
    annotation_legend = function() {
      am <- self$raw$annotationMap
      data_frame(annotation = names(am), label = unlist(am))
    },
    #' @field rate_limit
    #' how much requests were left after the POST request for this table was sent?
    #' Uses the same format as [sc_rate_limit_table()].
    rate_limit = function() {
      headers <- self$response$headers
      res <- list(
        remaining = headers$`x-ratelimit-remaining-table`,
        limit     = headers$`x-ratelimit-table`,
        reset     = headers$`x-ratelimit-reset-table`
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
#' @param json Path to a json file, which was downloaded via the STATcube
#'   GUI ("Open Data API Request"). Alternatively, a json string which
#'   passes [jsonlite::validate()].
#' @param add_totals Should totals be added for each classification field in
#'   the json request?
#' @return An object of class `sc_table` which contains the return
#'   value of the [httr::POST()] request in `obj$response`. The object also
#'   provides member functions to parse this response object. See
#'   [sc_table_class] for the class documentation.
#' @inheritParams sc_key
#' @param language The language to be used for labeling. `"en"` (the default)
#'   will use english. `"de"` uses German.
#'   The third option `"both"` will import both languages by sending two requests
#'   to the `/table` endpoint.
#' @param json_file Deprecated. Use `json` instead
#' @family functions for /table
#' @examplesIf sc_key_exists()
#' my_table <- sc_table(json = sc_example("population_timeseries.json"))
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
#'
#' # get the ids and labels of all saved tables
#' (saved_tables <- sc_table_saved_list())
#' table_uri <- saved_tables$id[1]
#'
#' # get a table based on one of these ids
#' my_response <- sc_table_saved(table_uri)
#' as.data.frame(my_response)
#' @export
sc_table <- function(json, language = NULL, add_totals = TRUE, key = NULL,
                     json_file = NA) {
  json <- normalize_json(json, json_file)
  language <- sc_language(language, c("en", "de", "both"))
  both <- language == "both"
  if (both)
    language <- "de"
  res <- sc_table_json_post(json$string, language, add_totals, key) |>
    sc_table_class$new(json$string, json$file, add_totals)
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
  cat(format(x, ...), sep = "\n")
}

normalize_json <- function(json, json_file) {
  if (!is.na(json_file)) {
    json <- json_file
    warning("parameter `json_file` was renamed to `json`")
  }
  file <- NULL
  if (length(json) == 1 && !jsonlite::validate(json)) {
    file <- json
    json <- readLines(file, warn = FALSE)
  }
  list(file = file, string = json)
}

#' @export
format.sc_table <- function(x, ...) {
  c(
    cli::style_bold(strwrap(x$meta$source$label)),
    "",
    cli_dl2(list(
      Database = paste0(cli::style_hyperlink(x$meta$source$code, x$browse()),
                        " (", cli::style_italic("STATcube"), ")"),
      Measures = x$meta$measures$label,
      Fields = paste0(
        x$meta$fields$label, cli::style_italic(
          paste0(" <", x$meta$fields$nitems, ">"))))
    ),
    "",
    cli_dl2(list(
      Request = cli_class(x$response$date, "timestamp"),
      STATcubeR = cli_class(x$meta$source$scr_version, "version")
    ))
  )
}

cli_class <- function(x, class) {
  cli::cli_fmt({
      cli::cli_text(paste0("{.", class, " ", x, "}"))
  })
}
