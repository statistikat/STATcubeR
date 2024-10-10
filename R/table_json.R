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

#' @export
as.character.sc_json <- function(x, ..., collapse = "\n") {
  paste(x$content,..., collapse = collapse)
}

sc_json_add_totals <- function(json_content) {
  measures <- unlist(json_content$dimensions)
  for (measure in measures)
    json_content$recodes[[measure]]$total <- TRUE
  json_content
}

#' Get the server from a json request
#'
#' parses a json request and returns a short string representing
#' the corresponding STATcube server
#' @return `"ext"`, `"red"` or `"prod"` depending on the database uri in the
#'   json request
#' @param json path to a request json
#' @examples
#' sc_json_get_server(sc_example('accomodation'))
#' @export
sc_json_get_server <- function(json) {
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  sc_database_get_server(database_uri = parsed$database)
}

sc_database_get_server <- function(database_uri) {
  switch(
    substring(database_uri, 14, 15),
    de = "ext",
    di = "red",
    db = "prod",
    stop('database uri \033[1m"', database_uri, '"\033[22m could not be ',
         "assigned to a STATcube server")
  )
}

sc_table_json_post <- function(json, language = NULL,
                               add_totals = TRUE, key = NULL) {
  language <- sc_language(language)
  server <- sc_json_get_server(json)
  if (add_totals)
    json <- json |>
      jsonlite::fromJSON(simplifyVector = FALSE) |>
      sc_json_add_totals() |>
      jsonlite::toJSON(pretty = TRUE, auto_unbox = TRUE)
  sc_with_cache(c("sc_table_json_post", json, language, add_totals), function() {
    httr::POST(
      url = paste0(base_url(server), "/table"),
      body = json,
      config = sc_headers(language, key, server)
    ) |> sc_check_response()
  })
}
