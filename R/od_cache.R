#' Cache management for Open Data
#'
#' Functions to inspect the contents of the current cache.
#' @name od_cache
#' @rdname od_cache
#' @examples
#' ## make sure the cache is not empty
#' od_table("OGD_krebs_ext_KREBS_1")
#' od_table("OGD_veste309_Veste309_1")
#'
#' ## inspect
#' od_cache_summary()
#' od_downloads()
#' @details
#' `od_cache_summary()` provides an overview of all contents of the cache through
#' a data.frame. It hasone row for each dataset and the following columns.
#' All file sizes are given in bytes
#' - **`id`** the dataset id
#' - **`updated`** the last modified time for `${id}.json`
#' - **`json`** the file size of `${id}.json`
#' - **`data`** the file size of `${id}.csv`
#' - **`header`** the file size of `${id}_HEADER.csv`
#' - **`fields`** the total file size of all files belonging to fields (`{id}_C*.csv`).
#' - **`n_fields`** the number of field files
#'
#' `od_downloads()` shows a download history for the current cache
#'
#' - **`time`** a timestamp for the download
#' - **`file`** the filename
#' - **`downloaded`** the download time in milliseconds
#' @export
od_cache_summary <- function() {
  files <- dir(od_cache_dir(), pattern = ".csv")
  pos_underscore <- as.integer(gregexpr("_C-", files))
  is_field <- pos_underscore != -1
  field <- substr(files[is_field], 1 + pos_underscore[is_field], nchar(files[is_field]) - 4)
  id <- substr(files[is_field], 1, pos_underscore[is_field] - 1)
  sizes_fields <- file.size(file.path(od_cache_dir(), files[is_field])) %>% split(id) %>% sapply(sum)
  fields <- data.frame(id, field, stringsAsFactors = FALSE)

  files <- files[!is_field]
  pos_underscore <- as.integer(gregexpr("_HEADER", files))
  is_header <- pos_underscore != -1
  id_header <- substr(files[is_header], 0, pos_underscore[is_header] - 1)
  files <- files[!is_header]
  id_data <- substr(files, 1, nchar(files) - 4)
  all_ids <- unique(c(id_data, id_header, fields$id))
  data.frame(
    id = all_ids,
    updated = file.mtime(paste0(od_cache_dir(), all_ids, ".json")),
    json = file.size(paste0(od_cache_dir(), all_ids, ".json")),
    data = file.size(paste0(od_cache_dir(), all_ids, ".csv")),
    header = file.size(paste0(od_cache_dir(), all_ids, "_HEADER.csv")),
    fields = sizes_fields[match(unique(fields$id), all_ids)],
    n_fields = match(fields$id, all_ids) %>% factor(seq_along(all_ids)) %>%
      table() %>% as.integer(),
    row.names = NULL, stringsAsFactors = FALSE
  ) %>% `class<-`(c("tbl", "data.frame"))
}


#' @rdname od_cache
#' @export
od_downloads <- function() {
  x <- od_cache_dir() %>% paste0("/downloads.log") %T>%
    (function(x) {if(!file.exists(x)) stop("No file 'dowloads.log' in cahce")}) %>%
    utils::read.csv(header = FALSE) %>% `names<-`(c("time", "file", "downloaded"))
  x$time <- as.POSIXct(x$time)
  x %>% .[rev(seq_len(nrow(.))), ] %>% `class<-`(c("tbl", "data.frame"))
}

