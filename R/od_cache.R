#' Cache management for Open Data
#'
#' Functions to inspect the contents of the current cache.
#' @name od_cache
#' @rdname od_cache
#' @param server the OGD-Server to use. `"ext"` for the external server (the
#'   default) or `"red"` for the editing server
#' @examples
#' ## make sure the cache is not empty
#' od_table("OGD_krebs_ext_KREBS_1")
#' od_table("OGD_veste309_Veste309_1")
#'
#' ## inspect
#' od_cache_summary()
#' od_downloads()
#' @details
#' [od_cache_summary()] provides an overview of all contents of the cache through
#' a data.frame. It has one row for each dataset and the following columns.
#' All file sizes are given in bytes
#' - **`id`** the dataset id
#' - **`updated`** the last modified time for `${id}.json`
#' - **`json`** the file size of `${id}.json`
#' - **`data`** the file size of `${id}.csv`
#' - **`header`** the file size of `${id}_HEADER.csv`
#' - **`fields`** the total file size of all files belonging to fields (`{id}_C*.csv`).
#' - **`n_fields`** the number of field files
#'
#' [od_downloads()] shows a download history for the current cache
#'
#' - **`time`** a timestamp for the download
#' - **`file`** the filename
#' - **`downloaded`** the download time in milliseconds
#' @export
od_cache_summary <- function(server = "ext") {
  cache_dir <- od_cache_path(server)
  files <- dir(cache_dir, pattern = ".csv")
  if(length(files)==0)
    return(NULL)
  pos_underscore <- as.integer(gregexpr("_C-", files))
  is_field <- pos_underscore != -1
  field <- substr(files[is_field], 1 + pos_underscore[is_field], nchar(files[is_field]) - 4)
  id <- substr(files[is_field], 1, pos_underscore[is_field] - 1)
  sizes_fields <- file.size(file.path(od_cache_dir(), files[is_field])) %>% split(id) %>% sapply(sum)
  fields <- list(id = id, field = field)

  files <- files[!is_field]
  pos_underscore <- as.integer(gregexpr("_HEADER", files))
  is_header <- pos_underscore != -1
  id_header <- substr(files[is_header], 0, pos_underscore[is_header] - 1)
  files <- files[!is_header]
  id_data <- substr(files, 1, nchar(files) - 4)
  all_ids <- unique(c(id_data, id_header, fields$id))
  res <- data_frame(
    id = all_ids %>% `class<-`(c("ogd_id", "character")),
    updated = file.mtime(paste0(cache_dir, all_ids, ".json")),
    json = file.size(paste0(cache_dir, all_ids, ".json")),
    data = file.size(paste0(cache_dir, all_ids, ".csv")),
    header = file.size(paste0(cache_dir, all_ids, "_HEADER.csv")),
    fields = sizes_fields[match(unique(fields$id), all_ids)],
    n_fields = match(fields$id, all_ids) %>% factor(seq_along(all_ids)) %>%
      table() %>% as.integer()
  )
  class(res$updated) <- c("sc_dttm", class(res$updated))
  res
}


#' @rdname od_cache
#' @importFrom magrittr %T>%
#' @export
od_downloads <- function(server = "ext") {
  x <- od_cache_path(server, "downloads.log") %T>%
    (function(x) {if (!file.exists(x)) stop("No file 'downloads.log' in cache")}) %>%
    utils::read.csv(header = FALSE) %>% `names<-`(c("time", "file", "downloaded"))
  x$time <- as.POSIXct(x$time)
  x %>% .[rev(seq_len(nrow(.))), ] %>% `class<-`(c("tbl", "data.frame"))
}
