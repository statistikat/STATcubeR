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
  file.rename(outfile, file)
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
  extracted <- dir()
  stopifnot(length(extracted) == 1)
  json <- jsonlite::read_json(file.path(extracted, "meta.json"))
  id <- json$resources[[1]]$name
  tmp_cache <- "cache"
  dir.create(tmp_cache)
  classification_files <- dir(file.path(extracted, 'classifications'), full.names = TRUE)
  file.rename(classification_files, paste0("cache/", id, "_", basename(classification_files)))
  file.rename(file.path(extracted, 'data.csv'), paste0('cache/', id, '.csv'))
  file.rename(file.path(extracted, 'header.csv'), paste0('cache/', id, '_HEADER.csv'))
  file.rename(file.path(extracted, 'meta.json'), paste0('cache/', id, '.json'))
  old_cache_dir <- od_cache_dir()
  od_cache_dir('cache')
  on.exit(od_cache_dir(old_cache_dir), add = TRUE)
  od_table(id)
}
