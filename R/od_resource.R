#' @name od_resource
#' @title Resource management for open.data
#'
#' @description
#' Helper functions for caching and parsing open.data resources.
#' @return For `od_cache_file()` and `od_resource()`, the returned objects
#'   contain a hidden attribute `attr(., "od")` about the time used for
#'   downloading and parsing the resource. `od_resource_all()` converts these
#'   hidden attribute into columns.
#' @importFrom magrittr %<>%
NULL

od_resource_blacklist <- c(
  # bad whitespace in attribute_description
  # only two columns in HEADER
  # html escapes in attribute_description
  "OGD_f1197_Bev_Jahresdurchschn_1", "OGD_f1585_Stud_Abschl_1",
  # only header in OGD_f1531neu_Aussenhandel_1.csv, file size: 52 bytes.
  "OGD_f1531neu_Aussenhandel_1"
)

od_resource_check_id <- function(id) {
  if (substr(id, 1, 4) != "OGD_")
    stop("Dataset ids must begin with \"OGD_\": ", shQuote(id), call. = FALSE)
  if (id %in% od_resource_blacklist)
    stop("Dataset ", shQuote(id), " was blacklisted in STATcubeR ",
         "because of inconsistent formats", call. = FALSE)
}

#' @name od_resource
#' @param dir If `NULL`, the cache directory is returned. Otherwise, the
#'   cache directory will be updated to `dir`.
#' @examples
#' # get the current cache directory
#' od_cache_dir()
#' @export
od_cache_dir <- function(dir = NULL) {
  if (is.null(dir))
    Sys.getenv("od_cache_dir", paste0(tempdir(), "/STATcubeR/open_data/"))
  else
    Sys.setenv(od_cache_dir = paste0(gsub("/$", "", dir), "/"))
}

#' @name od_resource
#' @details
#' `od_cache_clear(id)` removes all files belonging to the specified id.
#' @export
od_cache_clear <- function(id) {
  od_resource_check_id(id)
  files <- od_cache_dir() %>% dir(id, full.names = TRUE)
  file.remove(files)
  message("deleted ", length(files), " files from ", shQuote(od_cache_dir()))
}

od_cache_update <- function(url, filename) {
  cache_file <- paste0(od_cache_dir(), filename)
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  r <- httr::GET(url, httr::write_disk(cache_file, overwrite = TRUE))
  if (httr::http_error(r) || identical(r$headers$`content-length`, "0")) {
    file.remove(cache_file)
    stop("Resource not available: ", url, call. = FALSE)
  }
  t <- r$times[["total"]]*1000
  cat(format(Sys.time()), ",", filename, ",", t, "\n", append = TRUE,
      file = paste0(od_cache_dir(), "/downloads.log"), sep = "")
  t
}

#' @name od_resource
#' @param id A database id
#' @param suffix A suffix for the resource: `"HEADER"` or a field code.
#' @param timestamp A timestamp in `POSIXct` format. If provided, the
#'   cached resource will be updated if it is older than that value. Otherwise
#'   it will be downloaded only if it does not exist in the cache.
#' @param ... For internal use
#' @examples
#'
#' # Get paths to cached files
#' od_cache_file("OGD_veste309_Veste309_1")
#' od_cache_file("OGD_veste309_Veste309_1", "C-A11-0")
#' @export
od_cache_file <- function(id, suffix = NULL, timestamp = NULL, ...) {
  ext <- match.arg(list(...)$ext, c("csv", "json"))
  stopifnot(is.character(id) && length(id) > 0)
  od_resource_check_id(id)
  filename <- c(id, suffix) %>% paste(collapse = "_") %>% paste0(".", ext)
  cache_file <- paste0(od_cache_dir(), filename)
  download <- NA_real_
  if (!file.exists(cache_file) || !is.null(timestamp) &&
      timestamp > file.mtime(cache_file)) {
    url <- ifelse(ext == "csv", paste0("https://data.statistik.gv.at/data/", filename),
                  paste0("https://data.statistik.gv.at/ogd/json?dataset=", id))
    download <- od_cache_update(url, filename)
  }
  structure(cache_file, class = c("character", "od_cache_file"), od = list(
    download = download, size = file.size(cache_file), cached = file.mtime(cache_file),
    last_modified = timestamp))
}

#' @export
print.od_cache_file <- function(x, ...) {
  print(as.character(x), ...)
}

#' @name od_resource
#' @examples
#'
#' # get a parsed verison of the resource
#' od_resource("OGD_veste309_Veste309_1", "C-A11-0")
#' @export
od_resource <- function(id, suffix = NULL, timestamp = NULL) {
  cache_file <- od_cache_file(id, suffix, timestamp, ext = "csv")
  t <- Sys.time()
  x <- utils::read.csv2(cache_file, na.strings = c("", ":"),
                        check.names = FALSE, stringsAsFactors = FALSE) %>%
    od_normalize_columns(suffix)
  t <- Sys.time() - t
  t <- 1000*as.numeric(t)
  structure(x, od = c(attr(cache_file, "od"), list(parsed = t)),
            class = c("tbl", "data.frame"))
}

od_resource_parse_all <- function(resources) {
  parsed <- resources %>% lapply(function(x) {
    last_modified <- as.POSIXct(x$last_modified, format = "%Y-%m-%dT%H:%M:%OS")
    od_resource(x$name, timestamp = last_modified)
  })
  od <- lapply(parsed, attr, "od")

  data.frame(
    name = sapply(resources, function(x) x$name),
    last_modified = lapply(od, function(x) x$last_modified) %>% do.call(c, .),
    cached = lapply(od, function(x) x$cached) %>% do.call(c, .),
    size = sapply(od, function(x) x$size),
    download = vapply(od, function(x) x$download, 1.0),
    parsed = sapply(od, function(x) x$parsed),
    data = I(parsed %>% lapply(`attr<-`, "od", NULL)),
    stringsAsFactors = FALSE
  )
}

od_resources_check <- function(json) {
  resources <- json$resources
  att <- od_attr(json)
  id <- resources[[1]]$name
  stopifnot(resources[[2]]$name == paste0(id, "_HEADER"))
  stopifnot(all(sapply(resources, function(x) { x$format == "csv" })))
  fc_res <- resources %>% .[-c(1, 2)] %>% sapply(function(x) x$name)
  fc_att <- att$code[substr(att$code, 1, 2) == "C-"]
  stopifnot(setequal(fc_res, paste0(id, "_", fc_att)))
  function(header) {
    stopifnot(identical(header$code, att$code))
    if (!identical(header$name, att$label))
      message("HEADER and attribute description do not match")
  }
}

od_normalize_columns <- function(x, suffix) {
  if (!is.null(suffix)) {
    index_label_en <- ifelse(suffix == "HEADER", 3, 4)
    x <- x[, c(1, 2, 2, index_label_en)] %>%
      `names<-`(c("code", "label", "label_de", "label_en"))
    x$label <- NA_character_
    x$label_en <- as.character(x$label_en)
    x$label_de <- as.character(x$label_de)
    x$code <- as.character(x$code)
  }
  x
}

#' @name od_resource
#' @details
#' By default, downloaded json files will "expire" in one hour or 3600 seconds.
#' That is, if a json is requested, it will be reused from the cache unless the
#' `file.ctime()` is more than one hour behind `Sys.time()`.
#' @export
od_json <- function(id, timestamp = Sys.time() - 3600) {
  file <- od_cache_file(id, NULL, timestamp = timestamp, ext = "json")
  t <- Sys.time()
  json <- jsonlite::read_json(file)
  t <- Sys.time() - t
  attr(json, "od") <- c(attr(file, "od"), list(parsed = t))
  class(json) <- c("od_json", "list")
  json
}

#' @name od_resource
#' @param json The JSON file belonging to the dataset
#' @examples
#'
#' # Bundle all resources
#' od_resource_all("OGD_veste309_Veste309_1")
#' @export
od_resource_all <- function(id, json = od_json(id)) {
  check_header <- od_resources_check(json)
  out <- od_resource_parse_all(json$resources)
  check_header(out$data[[2]])
  out$data[[2]] %<>% od_normalize_columns("HEADER")
  out$data[seq(3, nrow(out))] %<>% lapply(od_normalize_columns, "FIELD")
  out %>% `class<-`(c("tbl", "data.frame"))
}
