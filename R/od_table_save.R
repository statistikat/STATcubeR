#' Saves/load opendata datasets via zip archives
#'
#' `od_table_save()` creates a zip file containing all relevant data from the
#' OGD portal. `od_table_local()` creates an `od_table` object based on
#' a zipped version of a dataset.
#'
#' @param x an object of class `od_table`
#' @param file A zip file for the dataset. For `od_table_save()`, the default
#'   is `{id}.zip` where `id` denotes the OGD identifier.
#' @examples
#' x <- od_table("OGD_krebs_ext_KREBS_1")
#'
#' # save dataset to ./OGD_krebs_ext_KREBS_1.zip
#'
#' od_table_save(x)
#' od_table_local("OGD_krebs_ext_KREBS_1.zip")
#'
#' # save dataset to a custom path
#'
#' od_table_save(x, "~/dataset.zip")
#' od_table_local("~/dataset.zip")
#' @export
od_table_save <- function(x, file = NULL) {
  stopifnot(inherits(x, "od_table"))
  data_dir <- tempfile(pattern = "od_table_save")
  dir.create(data_dir)
  on.exit(unlink(data_dir))
  oldwd <- setwd(data_dir)
  on.exit(setwd(oldwd))

  id <- x$meta$source$code
  if (is.null(file))
    file <- paste0(id, ".zip")

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

  utils::zip("dataset.zip", files = list.files(".", recursive = TRUE), flags = "-r9Xq")
  setwd(oldwd)
  file.rename(file.path(data_dir, "dataset.zip"), file)
  invisible(file)
}

#' @rdname od_table_save
#' @export
od_table_local <- function(file) {
  message("TODO")
}
