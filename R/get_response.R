lgr01 <- function() {
  fs::path_package(utils::packageName(), "LGR01.json")
}

sc_version <- function() {
  toString(utils::packageVersion(utils::packageName()))
}

base_url <- "http://sdbext:8082/statistik.at/ext/statcube/rest/v1/"

#' Führe eine API Abfrage gegen STATcube durch
#'
#' Erlaube es anhand eines jsone Files eine Tabelle von STATcube abzurufen.
#' @param file Pfad zu einem JSON file, welches über die STATcube GUI
#'   heruntergeladen wurde (Open Data API Abfrage)
#' @return Ein Objekt der Klasse `STATcube_response` welche den Rückgabewert
#'   von [httr::POST()] beinhaltet
#' @inheritParams sc_token
#' @examples
#' lgr_01 <- sc_get_response(sc_example("LGR01.json"))
#' @export
sc_get_response <- function(file, token = sc_token()) {
  response <- httr::POST(
    url = fs::path(base_url, "table"),
    body = httr::upload_file(file),
    config = httr::add_headers(APIKey = token)
  )
  x <- list(
    response = response,
    json = readLines(file, warn = FALSE),
    scr_version = sc_version()
  )
  class(x) <- "STATcube_response"
  x
}

#' @export
#' @param filename Name eines Beispiel-Json files.
#' @rdname sc_get_response
sc_example <- function(filename) {
  fs::path_package(utils::packageName(), "json_examples", filename)
}

#' @export
print.STATcube_response <- function(x, ...) {
  content <- httr::content(x$response)
  cat("Objekt der Klasse STATcube_response\n\n")
  cat("Datenbank:    ", content$database$label, "\n")
  cat("Werte:        ", content$measures %>% sapply(function(x) x$label) %>% paste(collapse = ", "), "\n")
  cat("Dimensionen:  ", content$fields %>% sapply(function(x) x$label) %>% paste(collapse = ", "), "\n\n")
  cat("Abfrage:      ", format(x$response$date), "\n")
  cat("STATcubeR:    ", x$scr_version)
}
