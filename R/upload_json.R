#' Lade JSON API Abfrage aus dem Windows System hoch
#'
#' Diese Funktion startet einen Upload-Dialog, welcher es erlaubt API Abfragen
#' im JSON-Format aus dem Windows-System zu Importieren. Die JSON-Datei wird
#' hohgeladen und über [sc_get_response()] ausgeführt.
#' @return Ein Objekt der Klasse `STATcube_response`
#' @examples
#' \dontrun{
#'
#' my_table <- sc_upload_json()
#' }
#' @export
sc_upload_json <- function() {
  shiny::runGadget(
    shiny::fluidPage(
      shiny::tags$script('setTimeout(function(){ $(".btn").click() }, 50)'),
      shiny::fileInput("json", "json", accept = ".json")
    ),
    function(input, output, session) {
      shiny::observeEvent(input$json, {
        input$json$datapath %>%
          sc_get_response() %>%
          shiny::stopApp()
      })
    }
  )
}
