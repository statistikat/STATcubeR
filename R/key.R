#' Manage your API Key
#'
#' Functions to get/set the STATcube api key and make them available for calls
#' against the STATcube api.
#'
#' * `sc_key()` forwards to `sc_key_get()` if the key is already present.
#'   Otherwise, `sc_key_prompt()` will be invoked.
#' @param key (`string`) An API key. To display your key, call
#'   [sc_browse_preferences()].
#' @return All functions return the key (invisibly)
#' @export
sc_key <- function() {
  if (!sc_key_exists()) {
    if (interactive())
      sc_key_prompt()
    else
      stop("No STATcube API key available")
  }
  sc_key_get()
}

sc_key_exists <- function() {
  Sys.getenv("STATCUBE_KEY") != ""
}

sc_key_valid <- function(key = sc_key()) {
  response <- httr::GET(
    url = paste0(base_url, "/info"),
    config = httr::add_headers(APIKey = key)
  )
  response$status_code == "200"
}

#' @rdname sc_key
#' @param test Use a test-requst to verify the key?
#' @details
#' * `sc_key_set()` can be used to pass the key as a parameter (`string`)
#' @export
sc_key_set <- function(key, test = TRUE) {
  if (test && !sc_key_valid(key))
    stop("The key could not be verified")
  Sys.setenv(STATCUBE_key = key)
  message("The provided key will be available for this R session. Add",
          "\n\n  STATCUBE_KEY=XXXX\n\nto your .Renviron to set ",
          "the key persistently")
  invisible(key)
}

#' @rdname sc_key
#' @details
#' * `sc_key_get()` returns the key, if it exists. Otherwise,
#'   an error is thrown.
#' @export
sc_key_get <- function() {
  if (!sc_key_exists())
    stop("No STATcube key available. Set key with sc_key_set()")
  invisible(Sys.getenv("STATCUBE_KEY"))
}

#' @rdname sc_key
#' @details
#' * `sc_key_prompt()` prompts for a key via [readline()]
#' @export
sc_key_prompt <- function(test = TRUE) {
  key <- readline("Provide your API key: \n")
  sc_key_set(key, test = test)
}
