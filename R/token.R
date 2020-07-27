secret_name <- "STATcube_token"

#' API Token hinzufügen
#'
#' Diese Funktion erlaubt es, einen STATcube API Token in den authSTAT vault zu
#' speichern bzw den Token aus dem Vault zu laden.
#'
#' @param token (Optional) Ein API Token, welcher via "Benutzerkonto" einsehbar
#'   ist.
#' @return Es wird der API Token (invisible) zurückgegeben
#' @export
statcube_token <- function(token = NULL) {
  if (!is.null(token))
    statcube_token_set(token)
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

statcube_token_set <- function(token) {
  ## TODO: verify API key
  authSTAT::auth_add_secret(secret_name, token)
}

statcube_token_get <- function() {
  authSTAT::auth_get_secret(secret_name)
}

statcube_token_prompt <- function() {
  token <- rstudioapi::askForPassword("Geben Sie ihren API Token ein")
  statcube_token_set(token)
}
