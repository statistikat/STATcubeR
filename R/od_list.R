#' Liste alle Datensätze
#'
#' [od_list()] gibt alle auf [data.statistik.gv.at](https://data.statistik.gv.at) verfügbaren Datensätze
#' zurück.
#'
#' @details manche Datensätze sind unter mehreren Gruppen gelistet.
#'
#' @return ein `data.frame` mit zwei Spalten
#' - `"gruppe"`: Inhaltliche Gliederungsgruppe, unter der ein Datensatz gegliedert ist.
#' - `"id"`: Name des Datensatzes; Kann in [od_table()] verwendet werden
#' - `"label"` Bezeichnung der Datenbank
#' @export
#' @examples
#' df <- od_list()
#' subset(df, df$grupp == "Arbeit")
od_list <- function() {
  url <- "https://data.statistik.gv.at/web/catalog.jsp"
  r <- httr::GET(url)
  if (r$status_code != 200) {
    stop("Fehler beim Lesen von ", shQuote(url), call. = FALSE)
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

