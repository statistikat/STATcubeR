secret_name <- "STATcube_token"

#' API Token hinzufügen
#'
#' Diese Funktion erlaubt es, einen STATcube API Token in den authSTAT vault zu
#' speichern bzw den Token aus dem Vault zu laden.
#'
#' * `statcube_token()` führt `statcube_token_get()` oder
#'   `statcube_token_prompt()` aus, je nachdem, ob der Token verfügbar ist.
#' @param token (`string`) Ein API Token. Ein persönlicher Token kann via
#'   [statcube_browse_preferences()] angezeigt werden.
#' @return Alle vier Funktionen geben den Token (invisible) zurück
#' @export
statcube_token <- function() {
  if (!statcube_token_exists()) {
    if (rstudioapi::isAvailable())
      statcube_token_prompt()
    else
      stop("Kein STATcube API Token vorhanden")
  }
  statcube_token_get()
}

statcube_token_exists <- function() {
  authSTAT::auth_exists_secret(secret_name)
}

statcube_token_valid <- function(token = statcube_token()) {
  response <- get_statcube_info(token)
  response$status_code == "200"
}

#' @rdname statcube_token
#' @details
#' * `statcube_token_set()` kann verwendet werden um den Token als Parameter
#'   (`string`) zu übergeben
#' @export
statcube_token_set <- function(token) {
  if (!statcube_token_valid(token))
    stop("Der angegebene Token konnte nicht verwendet werden")
  authSTAT::auth_add_secret(secret_name, token)
  message("STATcube Key wurde erfolgreich getestet und im Vault hinterlegt")
  invisible(token)
}

#' @rdname statcube_token
#' @details
#' * `statcube_token_get()` gibt den Token zurück, falls er existiert. Sonst
#'   wird ein Fehler geworfen.
#' @export
statcube_token_get <- function() {
  authSTAT::auth_get_secret(secret_name)
}

#' @rdname statcube_token
#' @details
#' * `statcube_token_prompt()` fragt den Token via [readline()] ab
#' @export
statcube_token_prompt <- function() {
  token <- readline("Geben Sie ihren API Token ein: \n")
  statcube_token_set(token)
}
