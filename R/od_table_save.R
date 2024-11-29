#' Saves/load opendata datasets via tar archives
#'
#' `od_table_save()` creates a tar archive containing all relevant data from the
#' OGD portal. `od_table_local()` parses the tar archive and recreates the
#' `od_table` object.
#'
#' @param x an object of class `od_table`
#' @param file An archive file file for the dataset. For `od_table_save()`,
#'   the default is `{id}.tar.gz` where `id` denotes the OGD identifier.
#' @examples
#' x <- od_table("OGD_krebs_ext_KREBS_1")
#'
#' # save dataset as an archive
#' archive <- file.path(tempdir(), "table.tar.gz")
#' od_table_save(x, archive)
#'
#' # read the saved archive
#' x2 <- od_table_local(archive)
#'
#' # cleanup
#' file.remove(archive)
#' @return
#' - for [od_table_save()]: the path to the generated file
#' - for [od_table_local()]: the OGD identifier
#' @export
od_table_save <- function(x, file = NULL) {
  stopifnot(inherits(x, "od_table"))
  id <- x$meta$source$code
  export_dir <- tempfile(pattern = "dir")
  data_dir <- file.path(export_dir, id)
  dir.create(data_dir, recursive = TRUE)
  on.exit(unlink(export_dir, recursive = TRUE))
  oldwd <- setwd(data_dir)
  on.exit(setwd(oldwd), add = TRUE)

  if (is.null(file)) {
    file <- paste0(id, ".tar.gz")
    message("No file argument provided. Using \"", file, "\"")
  }

  file.copy(od_cache_file(id), "data.csv")
  file.copy(od_cache_file(id, ext = "json"), "meta.json")
  file.copy(od_cache_file(id, "HEADER"), "header.csv")
  dir.create("classifications")

  for (code in x$meta$fields$code) {
    file.copy(
      od_cache_file(id, code),
      paste0("classifications/", code, ".csv")
    )
  }

  setwd("..")
  outfile <- tempfile(fileext = ".tar.gz")
  utils::tar(outfile, files = id, compression = "gzip")
  setwd(oldwd)
  file.copy(outfile, file)
  file.remove(outfile)
  invisible(file)
}

#' @rdname od_table_save
#' @export
od_table_local <- function(file) {
  stopifnot(file.exists(file))
  import_dir <- tempfile(pattern = "dir")
  dir.create(import_dir)
  on.exit(unlink(import_dir, recursive = TRUE))
  utils::untar(file, exdir = import_dir)
  oldwd <- setwd(import_dir)
  on.exit(setwd(oldwd), add = TRUE)
  paths <- od_table_local_paths()
  cache <- "temporary_cache/"
  dir.create(cache)
  file.rename(paths$classifications, paste0(cache, paths$id, "_", basename(paths$classifications)))
  file.rename(paths$data, paste0(cache, paths$id, ".csv"))
  file.rename(paths$header, paste0(cache, paths$id, "_HEADER.csv"))
  Sys.setFileTime(paths$meta, Sys.time())
  file.rename(paths$meta, paste0(cache, paths$id, ".json"))
  old_cache_dir <- od_cache_dir()
  od_cache_dir(cache)
  on.exit(od_cache_dir(old_cache_dir), add = TRUE)
  od_table(paths$id)
}

od_table_local_paths <- function() {
  extracted <- dir()
  stopifnot(length(extracted) == 1)
  json_file <- file.path(extracted, "meta.json")
  json <- jsonlite::read_json(json_file)
  id <- json$resources[[1]]$name
  stopifnot(is.character(id), length(id) == 1)
  if (json$extras$metadata_modified == "$PublDateTime$") {
    writeLines(gsub("\\$PublDateTime\\$", json$extras$begin_datetime, readLines(json_file)), json_file)
    json <- jsonlite::read_json(json_file)
  }
  timestamps <- as.POSIXct(sapply(json$resources, function(x) x$last_modified),
                           format = "%Y-%m-%dT%H:%M:%OS")
  stopifnot(all(timestamps <= Sys.time()))
  paths <- list(
    classifications = dir(file.path(extracted, "classifications"), full.names = TRUE),
    data = file.path(extracted, "data.csv"),
    header = file.path(extracted, "header.csv"),
    meta = json_file,
    id = id
  )
  stopifnot(all(file.exists(c(paths$data, paths$header, paths$meta))))
  # check if json "attribute description" matches the contents of classifications/
  columns <- od_attr(json)$code
  classifications <- columns[substr(columns, 1, 1) == "C"]
  stopifnot(setequal(paste0(classifications, ".csv"), basename(paths$classifications)))
  paths
}
