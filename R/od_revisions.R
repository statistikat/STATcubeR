#' Get OGD revisions
#'
#' Use the `/revision` endpoint of the OGD server to get a list
#' of all datasets that have changed since a certain timestamp.
#' @param since (optional) A timestamp. If supplied, only datasets updated
#'   later will be returned. Otherwise, all datasets are retured.
#'   Can be in either one of the following formats
#'   * a native R time type that is compatible with `strftime()`
#'     such as the return values of `Sys.Date()`, `Sys.time()` and `file.mtime()`.
#'   * a string of the form `YYYY-MM-DD` to specify a day.
#'   * a string of the form `YYYY-MM-DDThh:mm:ss` to specify a day and a time.
#' @param exclude_ext If `TRUE` (default) exclude all results that have
#'   `OGDEXT_` as a prefix
#' @return a character verctor with dataset ids
#' @inheritParams od_list
#' @examples
#' # get all datasets (including OGDEXT_*)
#' ids <- od_revisions(exclude_ext = FALSE)
#' length(ids)
#' head(ids)
#'
#' # get all the datasets since the last cache update
#' od_cache_dir() %>%
#'   file.mtime() %>%
#'   od_revisions()
#' @export
od_revisions <- function(since = NULL, exclude_ext = TRUE, server = "ext") {
  resp <- httr::GET(
    od_url(server, "ogd", "revision"),
    query = list(since_time = ogd_revison_normalize_time(since)))
  sc_check_response(resp)
  content <- httr::content(resp, simplify = TRUE)
  if (exclude_ext)
    content <- grep("^OGD_", content, value = TRUE)
  attr(content, "reponse") <- resp
  content
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
