secret_name <- "STATcube_token"

#' API Token hinzufügen
#'
#' Diese Funktion erlaubt es, einen STATcube API Token in den authSTAT vault zu
#' speichern bzw den Token aus dem Vault zu laden.
#'
#' * `sc_token()` führt `sc_token_get()` oder `sc_token_prompt()` aus, je
#'   nachdem, ob der Token verfügbar ist.
#' @param token (`string`) Ein API Token. Ein persönlicher Token kann via
#'   [sc_browse_preferences()] angezeigt werden.
#' @return Alle vier Funktionen geben den Token (invisible) zurück
#' @export
sc_token <- function() {
  if (!sc_token_exists()) {
    if (rstudioapi::isAvailable())
      sc_token_prompt()
    else
      stop("Kein STATcube API Token vorhanden")
  }
  sc_token_get()
}

sc_token_exists <- function() {
  authSTAT::auth_exists_secret(secret_name)
}

sc_token_valid <- function(token = sc_token()) {
  response <- sc_get_info(token)
  response$status_code == "200"
}

#' @rdname sc_token
#' @details
#' * `statcube_token_set()` kann verwendet werden um den Token als Parameter
#'   (`string`) zu übergeben
#' @export
sc_token_set <- function(token) {
  if (!sc_token_valid(token))
    stop("Der angegebene Token konnte nicht verwendet werden")
  authSTAT::auth_add_secret(secret_name, token)
  message("STATcube Key wurde erfolgreich getestet und im Vault hinterlegt")
  invisible(token)
}

#' @rdname sc_token
#' @details
#' * `sc_token_get()` gibt den Token zurück, falls er existiert. Sonst
#'   wird ein Fehler geworfen.
#' @export
sc_token_get <- function() {
  authSTAT::auth_get_secret(secret_name)
}

#' @rdname sc_token
#' @details
#' * `sc_token_prompt()` fragt den Token via [readline()] ab
#' @export
sc_token_prompt <- function() {
  token <- readline("Geben Sie ihren API Token ein: \n")
  sc_token_set(token)
}
