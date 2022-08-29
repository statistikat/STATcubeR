#' Manage your API Keys
#'
#' Functions to get/set the STATcube API keys and make them available for calls
#' against the STATcube API.
#'
#' @describeIn sc_key forwards to [sc_key_get()] if the key is already present.
#'   Otherwise, [sc_key_prompt()] will be invoked.
#' @param key (`string`) An API key. To display your key, call
#'   [sc_browse_preferences()].
#' @param server A STATcube API server. Defaults to the external Server via
#'   `"ext"`. Other options are `"red"` for the editing server and `"prod"` for
#'   the production server. External users should always use the default option `"ext"`.
#' @return All functions return the key (invisibly) except for
#'   `sc_key_exists()` and `sc_key_valid()`, which return a [logical()] of
#'   length one.
#' @export
sc_key <- function(server = "ext", test = FALSE) {
  if (!sc_key_exists(server)) {
    if (interactive())
      sc_key_prompt(server, test)
    else
      stop("No STATcube API key available")
  }
  sc_key_get(server)
}

#' @param test Use `sc_key_valid()` to verify the key? If the key is
#'   invalid, an error is returned and the key will not be set or updated.
#' @describeIn sc_key can be used to pass the key as a parameter (`string`)
#' @export
sc_key_set <- function(key, server = "ext", test = TRUE) {
  if (test && !sc_key_valid(key, server))
    stop("The key could not be verified")
  do.call(Sys.setenv, as.list(stats::setNames(key, sc_key_env_var(server))))
  message("The provided key will be available for this R session. Add",
          "\n\n  ", sc_key_env_var(server), " = XXXX\n\nto your .Renviron to set ",
          "the key persistently")
  invisible(key)
}

#' @describeIn sc_key returns the key, if it exists. Otherwise,
#'   an error is thrown.
#' @export
sc_key_get <- function(server = "ext") {
  if (!sc_key_exists())
    stop("No STATcube key available. Set key with sc_key_set()")
  invisible(Sys.getenv(sc_key_env_var(server)))
}

#' @describeIn sc_key prompts for a key via [readline()]
#' @export
sc_key_prompt <- function(server = "ext", test = TRUE) {
  key <- readline("Provide your API key: \n")
  sc_key_set(key, test = test)
}

#' @describeIn sc_key returns `TRUE` if a key was set and `FALSE` otherwise.
#' @export
sc_key_exists <- function(server = "ext") {
  Sys.getenv(sc_key_env_var(server)) != ""
}

sc_key_env_var <- function(server = "ext") {
  paste0("STATCUBE_KEY_", toupper(server))
}

#' @describeIn sc_key performs a test request and returns `TRUE` if the
#'   key is valid and `FALSE` otherwise.
sc_key_valid <- function(key = NULL, server = "ext") {
  if (is.null(key))
    key <- sc_key(server)
  response <- httr::GET(
    url = paste0(base_url(server), "/info"),
    config = sc_headers(key = key)
  )
  response$status_code == "200"
}

sc_servers <- c("ext", "red", "prod")
