sc_json_class <- R6::R6Class(
  "sc_json",
  list(
    initialize = function(json) {
      private$json_content <- json
    },
    print = function() {
      cat(private$json_content, sep = "\n")
    },
    write = function(file) {
      writeLines(text = private$json_content, con = file)
    }
  ),
  private = list(
    json_content = NULL
  )
)
