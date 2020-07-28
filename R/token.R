secret_name <- "STATcube_token"

#' API Token hinzufügen
#'
#' Diese Funktion erlaubt es, einen STATcube API Token in den authSTAT vault zu
#' speichern bzw den Token aus dem Vault zu laden.
#'
#' `statcube_token()` verhält sich wie `statcube_token_get()`, führt aber
#' `statcube_token_prompt()` aus, falls der Token fehlt.
#'
#' @param token Ein API Token, welcher via
#'   [statcube_browse_preferences()] angezeigt werden kann.
#' @return Es wird der API Token (invisible) zurückgegeben
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
#' @export
statcube_token_set <- function(token) {
  if (!statcube_token_valid(token))
    stop("Der angegebene Token konnte nicht verwendet werden")
  authSTAT::auth_add_secret(secret_name, token)
  message("STATcube Key wurde erfolgreich getestet und im Vault hinterlegt")
  invisible(token)
}

#' @rdname statcube_token
#' @export
statcube_token_get <- function() {
  authSTAT::auth_get_secret(secret_name)
}

#' @rdname statcube_token
#' @export
statcube_token_prompt <- function() {
  token <- readline("Geben Sie ihren API Token ein: \n")
  statcube_token_set(token)
}
