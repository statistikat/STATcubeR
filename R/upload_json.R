#' Lade json APi Abfrage aus dem Windows system hoch
#'
#' Diese Funktion startet einen Upload-Dialog, welcher es erlaubt API Abfragen
#' im JSON-Format aus dem Windows-System zu Importieren. Die JSON-Datei wird
#' hohgeladen und über [get_statcube_response()] ausgeführt.
#' @return Ein Objekt der Klasse `STATcube_response`
#' @examples
#' \dontrun{
#'
#' my_table <- upload_json()
#' }
#' @export
upload_json <- function() {
  shiny::runGadget(
    shiny::fluidPage(
      shiny::tags$script('setTimeout(function(){ $(".btn").click() }, 50)'),
      shiny::fileInput("json", "json", accept = ".json")
    ),
    function(input, output, session) {
      shiny::observeEvent(input$json, {
        input$json$datapath %>%
          get_statcube_response() %>%
          shiny::stopApp()
      })
    }
  )
}
