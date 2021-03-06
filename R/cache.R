#' Cache responses from the /table endpoint
#'
#' Functions to cache all requested tables in the directory `~/.STATcubeR_cache`
#' and reuse them in calls to `sc_table()` and `sc_table_custom()`. These
#' functions are designed for testing and documentation and should not be
#' regarded as part of the STATcubeR interface. The caching logic is likely to
#' change in the future in which case `sc_cache_clear()` is required to purge
#' old cache entries.
#' @usage
#' ## enable caching for the current R session
#' sc_cache_enable()
#' @rdname sc_cache
#' @name sc_cache
#' @keywords internal
sc_cache_enable <- function() {
  Sys.setenv(STATCUBE_CACHE = TRUE)
  message(paste0(
    "Caching will be available for this session. Add\n\n",
    "  STATCUBE_CACHE=TRUE\n\nto your .Renviron to enable",
    " caching persistently. \nCache directory: '", sc_cache_dir(), "'"
  ))
  invisible(sc_cache_dir())
}

#' @rdname sc_cache
#' @usage
#' ## disable caching for the current R session
#' sc_cache_disable()
sc_cache_disable <- function() {
  Sys.unsetenv("STATCUBE_CACHE")
}

sc_cache_enabled <- function() {
  Sys.getenv("STATCUBE_CACHE") != ""
}

sc_cache_dir <- function() {
  cache_dir <- Sys.getenv("STATCUBE_CACHE_DIR")
  if ("" == cache_dir)
    cache_dir <- "~/.STATcubeR_cache"
  cache_dir
}

sc_cache_file <- function(params, ext = ".rds") {
  openssl::md5(serialize(params, NULL)) %>%
    paste0(sc_cache_dir(), "/", ., ext)
}

#' @rdname sc_cache
#' @usage
#' ## remove all files from the cache
#' sc_cache_clear()
sc_cache_clear <- function() {
  if (dir.exists(sc_cache_dir()))
    unlink(sc_cache_dir(), recursive = TRUE)
}

sc_with_cache <- function(params, fun) {
  if (!sc_cache_enabled())
    return(fun())
  cache_file <- sc_cache_file(params)
  if (file.exists(cache_file))
    return(readRDS(cache_file))
  if (!dir.exists(sc_cache_dir()))
    dir.create(sc_cache_dir())
  return_value <- fun()
  saveRDS(return_value, cache_file)
  return(return_value)
}
