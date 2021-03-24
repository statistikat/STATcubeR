sc_json_class <- R6::R6Class(
  "sc_json", cloneable = FALSE,
  list(
    initialize = function(json = NULL, file = NULL) {
      if (!is.null(file))
        json <- readLines(file)
      private$json_content <- json
      private$file_ <- file
    },
    print = function() {
      cat(self$content, sep = "\n")
    },
    write = function(file) {
      writeLines(text = self$content, con = file)
      private$file_ <- file
    },
    edit = function() {
      if (is.null(self$file)) {
        temp_file <- tempfile(fileext = ".json")
        message("creating ", shQuote(temp_file))
        self$write(temp_file)
      }
      rstudioapi::navigateToFile(self$file)
      message("after editing, use $update() to resend the request")
    }
  ),
  active = list(
    file = function() private$file_,
    content = function() {
      if (is.null(self$file))
        private$json_content
      else
        readLines(self$file)
    },
    database = function() private$parse()$database,
    dimensions = function() unlist(private$parse()$dimensions),
    measures = function() unlist(private$parse()$measures)
  ),
  private = list(
    json_content = NULL, file_ = NULL, parse = function() {
      jsonlite::parse_json(self$content)
    }
  )
)

sc_table_json_post <- function(json, language = c("en", "de"), key = sc_key()) {
  sc_with_cache(list(json, language), function() {
    response <- httr::POST(
      url = paste0(base_url, "/table"),
      body = json,
      config = sc_headers(language, key)
    )
    if (response$status_code != 200) {
      sc_set_last_error(response)
      stop(httr::content(response)$message)
    }
    response
  })
}
