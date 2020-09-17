#' Manage your API Token
#'
#' Functions to get/set the STATcube api token and make them available for calls
#' against the STATcube api.
#'
#' * `sc_token()` forwards to `sc_token_get()` if the token is already present.
#'   Otherwise, `sc_token_prompt()` will be invoked.
#' @param token (`string`) An API token. To display your token, call
#'   [sc_browse_preferences()].
#' @return All functions return the token (invisibly)
#' @export
sc_token <- function() {
  if (!sc_token_exists()) {
    if (interactive())
      sc_token_prompt()
    else
      stop("No STATcube API Token available")
  }
  sc_token_get()
}

sc_token_exists <- function() {
  Sys.getenv("STATCUBE_TOKEN") != ""
}

sc_token_valid <- function(token = sc_token()) {
  response <- sc_get_info(token)
  response$status_code == "200"
}

#' @rdname sc_token
#' @param test Use a test-requst to verify the token?
#' @details
#' * `sc_token_set()` can be used to pass the token as a parameter (`string`)
#' @export
sc_token_set <- function(token, test = TRUE) {
  if (test && !sc_token_valid(token))
    stop("Der token could not be verified")
  Sys.setenv(STATCUBE_TOKEN = token)
  message("The provided token will be available for this R session. Add",
          "\n\n  STATCUBE_TOKEN=XXXX\n\nto your .Renviron to set ",
          "the token persistently")
  invisible(token)
}

#' @rdname sc_token
#' @details
#' * `sc_token_get()` returns the token, if it exists. Otherwise,
#'   an error is thrown.
#' @export
sc_token_get <- function() {
  if (!sc_token_exists())
    stop("No STATcube token available. Set token with sc_token_set()")
  invisible(Sys.getenv("STATCUBE_TOKEN"))
}

#' @rdname sc_token
#' @details
#' * `sc_token_prompt()` prompts for a token via [readline()]
#' @export
sc_token_prompt <- function(test = TRUE) {
  token <- readline("Provide your API token: \n")
  sc_token_set(token, test = test)
}
