sc_version <- function() {
  toString(utils::packageVersion(utils::packageName()))
}

base_url <- "http://sdbext:8082/statistik.at/ext/statcube/rest/v1"

as_sc_response <- function(response) {
  if (response$status_code != 200)
    stop(httr::content(response)$message)
  x <- list(
    response = response,
    scr_version = sc_version()
  )
  class(x) <- "STATcube_response"
  x
}

#' Post an api request against STATcube
#'
#' retrieve a response from STATcube based on a json file
#' @param file path to a json file, which was downloaded via the STATcube
#'   gui ("Open Data API Abfrage")
#' @return An object of class `STATcube_response` which contains the return
#'   value of [httr::POST()]
#' @inheritParams sc_token
#' @param language The language to be used for labeling. `"en"` or `"de"`
#' @examples
#' \dontrun{
#' lgr_01 <- sc_get_response(sc_example("LGR01.json"))
#' }
#' @export
sc_get_response <- function(file, token = sc_token(), language = c("en", "de")) {
  httr::POST(
    url = paste0(base_url, "/table"),
    body = httr::upload_file(file),
    config = httr::add_headers(
      APIKey = token,
      `Accept-Language` = match.arg(language)
    )
  ) %>% as_sc_response()
}

#' @export
#' @param filename The name of an example json file.
#' @rdname sc_get_response
sc_example <- function(filename) {
  system.file(package = utils::packageName(), "json_examples", filename)
}

#' @export
print.STATcube_response <- function(x, ...) {
  content <- sc_content(x)
  cat("An object of class STATcube_response\n\n")
  cat("Database:     ", content$database$label, "\n")
  cat("Measures:     ", content$measures %>% sapply(function(x) x$label) %>% paste(collapse = ", "), "\n")
  cat("Fields:       ", content$fields %>% sapply(function(x) x$label) %>% paste(collapse = ", "), "\n\n")
  cat("Request:      ", format(x$response$date), "\n")
  cat("STATcubeR:    ", x$scr_version)
}
