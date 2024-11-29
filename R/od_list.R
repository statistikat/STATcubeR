#' List available Opendata datasets
#'
#' [od_list()] returns a `data.frame ` containing all datasets published at
#' [data.statistik.gv.at](https://data.statistik.gv.at)
#'
#' @param unique some datasets are published under multiple groups.
#'   They will only be listed once with the first group they appear in unless
#'   this parameter is set to `FALSE`.
#' @param server the open data server to use. Either `ext` for the external
#'   server (the default) or `red` for the editing server. The editing server
#'   is only accessible for employees of Statistics Austria
#' @return a `data.frame` with two columns
#' - `"category"`: Grouping under which a dataset is listed
#' - `"id"`: Name of the dataset which can later be used in
#' [od_table()]
#' - `"label"`: Description of the dataset
#' @export
#' @examples
#' df <- od_list()
#' df
#' subset(df, category == "Bildung und Forschung")
#' # use an id to load a dataset
#' od_table("OGD_fhsstud_ext_FHS_S_1")
od_list <- function(unique = TRUE, server = c("ext", "red")) {
  stopifnot(requireNamespace("xml2"))
  server <- match.arg(server)
  url <- od_url(server, "web", "catalog.jsp")
  r <- httr::GET(url)
  if (httr::http_error(r)) {
    stop("Error while reading ", shQuote(url), call. = FALSE)
  }

  html <- httr::content(r, encoding = "UTF-8")

  # main-groups
  grp <- html |>
    xml2::xml_find_all('//*[@class="panel-heading"]') |>
    xml2::xml_find_all(".//a") |>
    xml2::xml_text()

  el <- html |>
    xml2::xml_find_all(".//h4") |>
    xml2::xml_find_all(".//a")

  # ids
  df <- data_frame(
    category = rep("NA", length(el)),
    id = el |> xml2::xml_attr("aria-label"),
    label = el |> xml2::xml_text()
  )

  ignored_labels <- c("[Alle \u00f6ffnen]", "[Alle schlie\u00dfen]",
                      "Neueste Datens\u00e4tze")

  df <- df[!(df$label %in% ignored_labels), ]
  grp <- grp[!(grp %in% ignored_labels)]

  tt <- diff(c(which(is.na(df$id)), nrow(df) + 1))
  df$category <- rep(grp, tt)
  df <- df[!is.na(df$id), ]
  if (unique)
    df <- df[!duplicated(df$id), ]
  else
    df <- df[df$label != "JSON", ]
  df <- df[substr(df$id, 1, 4) == "OGD_", ]
  df <- df[!(df$id %in% od_resource_blacklist), ]
  rownames(df) <- NULL
  attr(df, "od") <- r$times[["total"]]
  class(df$id) <- c("ogd_id", "character")
  class(df) <- c("tbl_df", class(df))
  df
}

#' Get a catalogue for OGD datasets
#'
#' **EXPERIMENTAL** This function parses several json metadata files at once
#' and combines them into a `data.frame` so the datasets can easily be
#' filtered based on categorizations, tags, number of classifications, etc.
#'
#' @details
#' The naming, ordering and choice of the columns is likely to change.
#' @return a `data.frame` with the following structure
#'
#' |**Column**|**Type**       | **Description**
#' | ---------| -------       | -------------
#' |title     |`chr`          | Title of the dataset
#' |measures  |`int`          | Number of measure variables
#' |fields    |`int`          | Number of classification fields
#' |modified  |`datetime`     | Timestamp when the dataset was last modified
#' |created   |`datetime`     | Timestamp when the dataset was created
#' |database  |`chr`          | ID of the corresponding STATcube database
#' |title_en  |`chr`          | English title
#' |notes     |`chr`          | Description for the dataset
#' |frequency |`chr`          | How often is the dataset updated?
#' |category  |`chr`          | Category of the dataset
#' |tags      |`list<chr>`    | tags assigned to the dataset
#' |json      |`list<od_json>`| Full json metadata
#'
#' The type `datetime` refers to the `POSIXct` format as returned by [Sys.time()].
#' The last column `"json"` contains the full json metadata as returned by
#' [od_json()].
#'
#' @inheritParams od_table
#' @param local If `TRUE` (the default), the catalogue is created based on
#'   cached json metadata. Otherwise, the cache is updated prior to
#'   creating the catalogue using a "bulk-download" for metadata files.
#' @examples
#' catalogue <- od_catalogue()
#' catalogue
#' table(catalogue$update_frequency)
#' table(catalogue$categorization)
#' catalogue[catalogue$categorization == "Gesundheit", 1:4]
#' catalogue[catalogue$measures >= 70, 1:3]
#' catalogue$json[[1]]
#' head(catalogue$database)
#' @export
od_catalogue <- function(server = "ext", local = TRUE) {
  if (local) {
    files <- dir(od_cache_path(server), '*.json')
    ids <- substr(files, 1, nchar(files) - 5)
  } else {
    ids <- od_revisions(server = server)
  }
  timestamp <- switch(as.character(local), "TRUE" = NULL, "FALSE" = Sys.time())
  jsons <- lapply(
    cli::cli_progress_along(
      ids, type = "tasks", "downloading json metadata files"),
    function(i) {
      od_json(ids[i], timestamp, server)
    }
  )
  if (!local)
    cli::cli_text("\rDownloaded {.field {length(ids)}} metadata files with {.fn od_json}")
  as_df_jsons(jsons)
}

as_df_jsons <- function(jsons) {
  parse_time <- function(x) {
    as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS")
  }

  descs <- paste0(";", sapply(jsons, function(x) x$extras$attribute_description))
  tmpfn <- function(x) {x[!grepl("statcube", x)] <- NA_character_; x}
  out <- data_frame(
    title = sapply(jsons, function(x) x$title),
    measures = sapply(gregexpr(";F-", descs),length),
    fields = sapply(gregexpr(";C-", descs),length),
    modified = sapply(jsons, function(x) x$extras$metadata_modified),
    created = sapply(jsons, function(x) x$resources[[1]]$created),
    id = sapply(jsons, function(x) x$resources[[1]]$name),
    database = sapply(jsons, function(x) x$extras$metadata_linkage[[1]]) |>
      tmpfn() |> strsplit("?id=") |>
      sapply(function(x) x[2]),
    title_en = sapply(jsons, function(x) x$extras$en_title_and_desc),
    notes = sapply(jsons, function(x) x$notes),
    update_frequency = sapply(jsons, function(x) x$extras$update_frequency),
    tags = I(lapply(jsons, function(x) unlist(x$tags))),
    categorization = sapply(jsons, function(x) unlist(x$extras$categorization[1])),
    json = I(jsons)
  )
  out$modified <- parse_time(out$modified)
  out$created <- parse_time(out$created)
  class(out$id) <- c("ogd_id", "character")
  out
}

