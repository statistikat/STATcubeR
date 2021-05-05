sc_json_class <- R6::R6Class(
  "sc_json", cloneable = FALSE,
  list(
    initialize = function(json = NULL, file = NULL) {
      if (!is.null(file))
        json <- readLines(file, warn = FALSE)
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
        readLines(self$file, warn = FALSE)
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

sc_json_add_totals <- function(json_content) {
  measures <- unlist(json_content$dimensions)
  for (measure in measures)
    json_content$recodes[[measure]]$total <- TRUE
  json_content
}

sc_table_json_post <- function(json, language = c("en", "de"),
                               add_totals = TRUE, key = sc_key()) {
  if (add_totals)
    json <- json %>%
      jsonlite::fromJSON(simplifyVector = FALSE) %>%
      sc_json_add_totals() %>%
      jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
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
