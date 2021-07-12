#' List available Opendata datasets
#'
#' [od_list()] returns a `data.frame ` containing all datasets published at
#' [data.statistik.gv.at](https://data.statistik.gv.at)
#'
#' @details some datasets are pulbished under multiple groups
#' @return a `data.frame` with two columns
#' - `"gruppe"`: Grouping under which a dataset is listed
#' - `"id"`: Name of the dataset which can later be used in
#' [od_table()]
#' - `"label"`: Description of the dataset
#' @export
#' @examples
#' df <- od_list()
#' subset(df, df$gruppe == "Arbeit")
od_list <- function() {
  url <- "https://data.statistik.gv.at/web/catalog.jsp"
  r <- httr::GET(url)
  if (r$status_code != 200) {
    stop("Error while reading ", shQuote(url), call. = FALSE)
  }

  html <- httr::content(r, encoding = "UTF-8")

  # main-groups
  grp <- html %>%
    rvest::html_elements(".panel-heading") %>%
    rvest::html_elements("a") %>%
    rvest::html_text()

  el <- html %>%
    rvest::html_elements("h4") %>%
    rvest::html_elements("a")

  # ids
  df <- data.frame(
    gruppe = "NA",
    id = el %>% rvest::html_attr("aria-label"),
    label = el %>% rvest::html_text()
  )

  tt <- diff(c(which(is.na(df$id)), nrow(df) + 1))
  df$gruppe <- rep(grp, tt)
  df <- df[!is.na(df$id), ]
  df <- df[!duplicated(df$id), ]
  rownames(df) <- NULL
  df
}

