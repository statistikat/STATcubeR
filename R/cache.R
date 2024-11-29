#' Cache responses from the STATcube REST API
#'
#' Functions to cache requested resources in the directory `~/.STATcubeR_cache`
#' and reuse them in calls to [sc_table()], [sc_table_custom()] [sc_schema()] and so forth.
#' These functions are designed for testing and documentation and should not be
#' regarded as part of the STATcubeR interface. The caching logic is likely to
#' change in the future in which case [sc_cache_clear()] is required to purge
#' old cache entries.
#'
#' Caching can be set up using environment variables. To set up a persistent cache
#' for both Open Data and the REST API, the following lines in `.Renviron` can
#' be used.
#' The paths in this example are only applicable for UNIX-based operating systems.
#'
#' ```sh
#' STATCUBE_KEY_EXT   = YOUR_API_KEY_GOES_HERE
#' STATCUBE_CACHE     = TRUE
#' OD_CACHE_DIR       = "~/.cache/STATcubeR/open_data/"
#' STATCUBE_CACHE_DIR = "~/.cache/STATcubeR/api/"
#' ```
#' If caching is enabled, there is no check to verify if the
#' resources are unchanged in the server.
#' Caching is not implemented for the
#' endpoints [sc_info()] and [sc_rate_limit_table()].
#' @rdname sc_cache
#' @param verbose print instructions on how to set up caching persistently
#'   via environment variables?
#' @name sc_cache
#' @return
#' - for [sc_cache_enable()], [sc_cache_dir()]: the path to the cache-directory
#' - for [sc_cache_disable()]: `TRUE`
#' - for [sc_cache_enabled()]: `TRUE` if caching is enabled, `FALSE` otherwise
#' - for [sc_cache_files()]: the content of the cache associated with a file
#' - for [sc_cache_clear()]: `NULL`
NULL

#' @describeIn sc_cache enables caching for the current R session
#' @export
sc_cache_enable <- function(verbose = TRUE) {
  Sys.setenv(STATCUBE_CACHE = TRUE)
  if (verbose)
    message(paste0(
      "Caching will be available for this session. Add\n\n",
      "  STATCUBE_CACHE     = TRUE\n  STATCUBE_CACHE_DIR = \"", sc_cache_dir(),
      "\"\n\nto your .Renviron to enable",
      " caching persistently."
    ))
  invisible(sc_cache_dir())
}

#' @describeIn sc_cache disables caching for the current R session
#' sc_cache_disable()
#' @export
sc_cache_disable <- function() {
  Sys.unsetenv("STATCUBE_CACHE")
}

#' @describeIn sc_cache informs whether the cache is currently enabled
#' @export
sc_cache_enabled <- function() {
  Sys.getenv("STATCUBE_CACHE") != ""
}

#' @export
#' @param dir a cache directory
#' @describeIn sc_cache get/set the directory used for caching
sc_cache_dir <- function(dir = NULL) {
  if (is.null(dir))
    return(sc_cache_dir_get())
  dir <- gsub("/$", "", dir)
  Sys.setenv(STATCUBE_CACHE_DIR = dir)
  invisible(dir)
}

sc_cache_dir_get <- function() {
  cache_dir <- Sys.getenv("STATCUBE_CACHE_DIR")
  if ("" == cache_dir)
    cache_dir <- rappdirs::user_cache_dir("STATcubeR")
  cache_dir
}

sc_checksum <- function(x) {
    gsub("/", "-", httr::sha1_hash(NULL, x))
}

#' @describeIn sc_cache get the cache file associated with an object
#' @param x an object of class `sc_table` or `sc_schema`
#' @export
sc_cache_files <- function(x) {
  if (inherits(x, "sc_table"))
    return(attr(x$response, "sc_cache_file"))
  if (inherits(x, "sc_schema"))
    return( attr(attr(x, "response"), "sc_cache_file"))
  stop("sc_cache_file() can only be used with sc_table and sc_schema objects")
}

sc_cache_file <- function(params, ext = ".rds") {
    paste0(sc_cache_dir(), "/", sc_checksum(params), ext)
}

#' @describeIn sc_cache removes all files from the cache
#' @export
sc_cache_clear <- function() {
  nfiles <- length(dir(sc_cache_dir()))
  if (dir.exists(sc_cache_dir()))
    unlink(sc_cache_dir(), recursive = TRUE)
  message("deleted ", nfiles, " files from '", sc_cache_dir(), "'")
}

sc_with_cache <- function(params, fun) {
  if (!sc_cache_enabled())
    return(fun())
  cache_file <- sc_cache_file(params)
  if (file.exists(cache_file))
    return(`attr<-`(readRDS(cache_file), "sc_cache_file", cache_file))
  if (!dir.exists(sc_cache_dir()))
    dir.create(sc_cache_dir(), recursive = TRUE)
  return_value <- fun()
  saveRDS(return_value, cache_file)
  return(`attr<-`(return_value, "sc_cache_file", cache_file))
}
