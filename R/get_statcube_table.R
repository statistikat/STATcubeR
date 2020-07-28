lgr01 <- function() {
  fs::path_package(utils::packageName(), "LGR01.json")
}

base_url <- "http://sdbext:8082/statistik.at/ext/statcube/rest/v1/"

#' Führe eine API Abfrage gegen STATcube durch
#'
#' Erlaube es anhand eines jsone Files eine Tabelle von STATcube abzurufen.
#' @param file Pfad zu einem JSON file, welches über die STATcube GUI
#'   heruntergeladen wurde (Open Data API Abfrage)
#' @return Ein Objekt der Klasse `STATcube_response` welche den Rückgabewert
#'   von [httr::POST()] beinhaltet
#' @inheritParams statcube_token
#' @examples
#' lgr_01 <- get_statcube_table()
#' @export
get_statcube_table <- function(file = lgr01(), token = statcube_token()) {
  response <- httr::POST(
    url = fs::path(base_url, "table"),
    body = httr::upload_file(file),
    config = httr::add_headers(APIKey = token)
  )
  x <- list(
    response = response,
    json = readLines(file, warn = FALSE)
  )
  class(x) <- "STATcube_response"
  x
}

#' @export
print.STATcube_response <- function(x, ...) {
  content <- httr::content(x$response)
  cat("Objekt der Klasse STATcube_response\n\n")
  cat("Datenbank: \t", content$database$label, "\n")
  cat("Werte:\t\t", content$measures %>% sapply(function(x) x$label) %>% paste(collapse = ", "), "\n")
  cat("Dimensionen: \t", content$fields %>% sapply(function(x) x$label) %>% paste(collapse = ", "), "\n")
  #cat("Abfrage: \n")
  #cat(x$json, sep = "\n")
}
