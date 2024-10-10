#' Get OGD revisions
#'
#' Use the `/revision` endpoint of the OGD server to get a list
#' of all datasets that have changed since a certain timestamp.
#' @param since (optional) A timestamp. If supplied, only datasets updated
#'   later will be returned. Otherwise, all datasets are returned.
#'   Can be in either one of the following formats
#'   * a native R time type that is compatible with `strftime()`
#'     such as the return values of `Sys.Date()`, `Sys.time()` and `file.mtime()`.
#'   * a string of the form `YYYY-MM-DD` to specify a day.
#'   * a string of the form `YYYY-MM-DDThh:mm:ss` to specify a day and a time.
#' @param exclude_ext If `TRUE` (default) exclude all results that have
#'   `OGDEXT_` as a prefix
#' @return a character vector with dataset ids
#' @inheritParams od_list
#' @examples
#' # get all datasets (including OGDEXT_*)
#' ids <- od_revisions(exclude_ext = FALSE)
#' ids
#' sample(ids, 6)
#'
#' # get all the datasets since the fifteenth of august
#' od_revisions("2022-09-15")
#' @export
od_revisions <- function(since = NULL, exclude_ext = TRUE, server = "ext") {
  resp <- httr::GET(
    od_url(server, "ogd", "revision"),
    query = list(since_time = ogd_revison_normalize_time(since)))
  sc_check_response(resp)
  content <- httr::content(resp, simplify = TRUE)
  if (exclude_ext)
    content <- grep("^OGD_", content, value = TRUE)
  attr(content, "response") <- resp
  attr(content, "since") <- since
  class(content) <- c("od_revisions", class(content))
  content
}

#' @export
print.od_revisions <- function(x, ...) {
  since <- attr(x, "since")
  response <- attr(x, "response")
  if (!is.null(since))
    message(cli::format_inline("{.strong {length(x)}} changes between
                {.timestamp {attr(x, 'since')}} and
                {.timestamp {response$date}}"))
  else
    message(cli::format_inline("{.strong {length(x)}} datasets are available ",
                "({.timestamp {response$date}})\n"))
  if (length(x) > 0) {
    y <- cli::cli_vec(x, list("vec-trunc" = 3))
    message(cli::format_inline("{.strong ids}: {.emph {y}}"))
  }
  invisible(x)
}

# normalize (and possibly format) the timestamp
ogd_revison_normalize_time <- function(since) {
  if (is.null(since))
    return(NULL)
  if (!is.character(since))
    since <- strftime(since, "%Y-%m-%dT%H:%M:%S")
  stopifnot(length(since) == 1)
  since
}
