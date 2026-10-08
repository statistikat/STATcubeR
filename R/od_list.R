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
#' @param lang either `"de"` or `"en"`
#' @return a `data.frame` with the following columns
#' - `"category"`: Grouping under which a dataset is listed
#' - `"id"`: Name of the dataset which can later be used in
#' [od_table()]
#' - `"label"`: Description of the dataset
#' - `"date"`: the last update date of the dataset
#' - `"csv_link"`: the URL of the (bulk) csv file
#' - `"json_link"`: the URL of the json metadata file
#' @export
#' @examplesIf od_server_reachable()
#' df <- od_list()
#' df
#' subset(df, category == "Bildung und Forschung")
#' # use an id to load a dataset
#' od_table("OGD_fhsstud_ext_FHS_S_1")

od_list <- function(unique = TRUE, server = c("ext", "red"), lang = c("de", "en")) {
  stopifnot(requireNamespace("xml2", quietly = TRUE))

  server <- match.arg(server)
  if (!od_server_reachable(server))
    return(od_abort_unavailable(server))
  base_url <- od_url(server, "web")
  url <- paste0(base_url, "/catalog.jsp")

  lang <- match.arg(lang)

  # Header based on defined language
  if (lang == "de") {
    custom_headers <- httr::add_headers(
      `Accept-Language` = "de-AT,de;q=0.9,en;q=0.8"
    )
  } else {
    custom_headers <- httr::add_headers(
      `Accept-Language` = "en-US,en;q=0.9"
    )
  }

  # read data
  r <- httr::GET(url, custom_headers)
  if (httr::http_error(r)) {
    stop("Error while reading ", shQuote(url), call. = FALSE)
  }

  html <- httr::content(r, as = "parsed", encoding = "UTF-8")

  # extract categories
  panels <- xml2::xml_find_all(html, "//*[@class='panel panel-default']")

  # Liste fuer die Tabellen-Fragmente vorallokieren
  res_list <- vector("list", length(panels))
  base_url <- "https://data.statistik.gv.at/web/"

  # ignored labels based on language
  ignored_labels <- if (lang == "de") {
    c("[Alle \u00f6ffnen]", "[Alle schlie\u00dfen]", "Neueste Datens\u00e4tze", "Neueste Daten")
  } else {
    c("[Open all]", "[Close all]", "Latest data", "Latest Datasets")
  }

  for (i in seq_along(panels)) {
    panel <- panels[[i]]

    # read category from panel-heading
    cat_node <- xml2::xml_find_first(panel, ".//*[@class='panel-heading']//a")
    category <- xml2::xml_text(cat_node, trim = TRUE)

    # skip ignored
    if (is.na(category) || category %in% ignored_labels) {
      next
    }

    # read relevant rows
    rows <- xml2::xml_find_all(panel, ".//tr[td/h4/a[starts-with(@aria-label, 'OGD_')]]")
    if (length(rows) == 0) next

    # extract id and label
    main_links <- xml2::xml_find_first(rows, "./td[1]/h4/a")
    ids <- xml2::xml_attr(main_links, "aria-label")
    labels <- xml2::xml_text(main_links, trim = TRUE)

    # extract date
    date_nodes <- xml2::xml_find_first(rows, "./td[2]")
    dates <- xml2::xml_text(date_nodes, trim = TRUE)

    # csv/json links
    csv_nodes <- xml2::xml_find_first(rows, "./td[3]/a")
    csv_links <- xml2::xml_attr(csv_nodes, "href")

    json_nodes <- xml2::xml_find_first(rows, "./td[4]//a")
    json_links <- xml2::xml_attr(json_nodes, "href")

    # convert relative -> abs. hrefs
    if (!all(is.na(csv_links))) {
      csv_links <- ifelse(is.na(csv_links), NA_character_, xml2::url_absolute(csv_links, base_url))
    }
    if (!all(is.na(json_links))) {
      json_links <- ifelse(is.na(json_links), NA_character_, xml2::url_absolute(json_links, base_url))
    }

    # data.frame for given category
    res_list[[i]] <- data.frame(
      category = category,
      id = ids,
      label = labels,
      date = dates,
      csv_link = csv_links,
      json_link = json_links,
      stringsAsFactors = FALSE
    )
  }

  # combine to a single data.frame
  df <- do.call(rbind, res_list[!sapply(res_list, is.null)])

  if (is.null(df) || nrow(df) == 0) {
    return(
      data.frame(
        category = character(),
        id = character(),
        label = character(),
        date = as.Date(character()),
        csv_link = character(),
        json_link = character(),
        stringsAsFactors = FALSE
      )
    )
  }

  # cleanup/filter
  df <- df[!is.na(df$id) & substr(df$id, 1, 4) == "OGD_", ]

  if (exists("od_resource_blacklist")) {
    df <- df[!(df$id %in% od_resource_blacklist), ]
  }

  if (unique) {
    df <- df[!duplicated(df$id), ]
  }

  # language-based date-parsing
  if (lang == "de") {
    df$date <- as.Date(df$date, format = "%d.%m.%Y")
  } else {
    # Format: "Apr 15, 2019" -> system independent mapping of english month abbreviations
    date_clean <- df$date
    months_regex <- c(
      "^Jan" = "01", "^Feb" = "02", "^Mar" = "03", "^Apr" = "04",
      "^May" = "05", "^Jun" = "06", "^Jul" = "07", "^Aug" = "08",
      "^Sep" = "09", "^Oct" = "10", "^Nov" = "11", "^Dec" = "12"
    )

    days <- gsub("^[A-Za-z ]+ ([0-9]+), .*", "\\1", date_clean)
    years <- gsub(".*, ([0-9]{4})$", "\\1", date_clean)
    days <- sprintf("%02d", as.integer(days))

    extracted_months <- sub("^([A-Za-z]{3}).*", "\\1", date_clean)
    num_months <- rep(NA_character_, length(extracted_months))

    for (pat in names(months_regex)) {
      matches <- grepl(pat, extracted_months, ignore.case = TRUE)
      num_months[matches] <- months_regex[pat]
    }
    iso_dates <- paste(years, num_months, days, sep = "-")
    df$date <- as.Date(iso_dates)
  }

  rownames(df) <- NULL
  attr(df, "od") <- r$times[["total"]]
  class(df$id) <- c("ogd_id", "character")
  class(df) <- c("tbl_df", "tbl", "data.frame")
  return(df)
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
#' @examplesIf od_server_reachable()
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
    if (!od_server_reachable(server))
      return(od_abort_unavailable(server))
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

