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
  if (!sc_key_exists(server = server)) {
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
    stop("The key could not be verified\n", message_sc_last_error(), call. = FALSE)
  if (test)
    cli::cli_alert_success("Key could be verified via a test request")
  do.call(Sys.setenv, as.list(stats::setNames(key, sc_key_env_var(server))))
  #cli::cli_alert_info("The provided key will be available for this R session"
  cli::cli_bullets(c(
    i = "The provided key will be available for this R session",
    i = paste0("Add {.code {sc_key_env_var(server)} = XXXX} to ",
               "{.file ~/.Renviron} to set the key persistently. Replace",
               " {.code XXXX} with your key")
  ))
  invisible(key)
}

#' @describeIn sc_key returns the key, if it exists. Otherwise,
#'   an error is thrown.
#' @export
sc_key_get <- function(server = "ext") {
  if (!sc_key_exists(server = server))
    stop("No STATcube key available. Set key with sc_key_set()")
  invisible(Sys.getenv(sc_key_env_var(server)))
}

#' @describeIn sc_key prompts for a key via [readline()]
#' @export
sc_key_prompt <- function(server = "ext", test = TRUE) {
  cli::cli_alert_info(c("You can view your API key under the ",
    "{.href [STATcube account preferences]({sc_browse_preferences()})}"))
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
  response <- httr::GET(
    url = paste0(base_url(server), "/info"),
    config = sc_headers("en", key, server)
  )
  valid <- response$status_code == "200"
  if (!valid)
    sc_env$last_error <- response
  valid
}

sc_servers <- c("ext", "red", "prod")
