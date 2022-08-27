#' List available Opendata datasets
#'
#' [od_list()] returns a `data.frame ` containing all datasets published at
#' [data.statistik.gv.at](https://data.statistik.gv.at)
#'
#' @param unique some datasets are pulbished under multiple groups.
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
od_list <- function(unique = TRUE, server = c('ext', 'red')) {
  stopifnot(requireNamespace("xml2"))
  server <- match.arg(server)
  url <- od_url(server, "web", "catalog.jsp")
  r <- httr::GET(url)
  if (httr::http_error(r)) {
    stop("Error while reading ", shQuote(url), call. = FALSE)
  }

  html <- httr::content(r, encoding = "UTF-8")

  # main-groups
  grp <- html %>%
    xml2::xml_find_all('//*[@class="panel-heading"]') %>%
    xml2::xml_find_all(".//a") %>%
    xml2::xml_text()

  el <- html %>%
    xml2::xml_find_all(".//h4") %>%
    xml2::xml_find_all(".//a")

  # ids
  df <- data.frame(
    category = "NA",
    id = el %>% xml2::xml_attr("aria-label"),
    label = el %>% xml2::xml_text(),
    stringsAsFactors = FALSE
  )

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
  df %>% `class<-`(c("tbl", "data.frame"))
}
