sc_json_class <- R6::R6Class(
  "sc_json", cloneable = FALSE,
  list(
    initialize = function(json = NULL, file = NULL, add_totals = TRUE) {
      if (!is.null(file))
        json <- readLines(file, warn = FALSE)
      private$json_content <- json
      private$file_ <- file
      private$add_totals <- add_totals
    },
    print = function() {
      cat(self$content, sep = "\n")
    },
    write = function(file) {
      writeLines(text = self$content, con = file)
      private$file_ <- file
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
    measures = function() unlist(private$parse()$measures),
    totals = function() private$add_totals
  ),
  private = list(
    json_content = NULL, file_ = NULL, parse = function() {
      jsonlite::parse_json(self$content)
    },
    add_totals = TRUE
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
    httr::POST(
      url = paste0(base_url, "/table"),
      body = json,
      config = sc_headers(language, key)
    ) %>% sc_check_response()
  })
}
