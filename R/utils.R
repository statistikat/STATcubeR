sc_parse_time <- function(timestamp) {
   as.POSIXct((as.numeric(timestamp) / 1000), origin = "1970-01-01")
}

sc_user_agent <- function(){
  paste0("STATcubeR/", sc_version(FALSE),
         " (http://github.com/statistikat/STATcubeR)",
         " httr/", utils::packageVersion("httr"),
         " R/", R.version$major, ".", R.version$minor)
}

sc_headers <- function(language = NULL, key = NULL, server = "ext", ...) {
  if (is.null(key))
    key <- sc_key(server)
  httr::add_headers(
    APIKey = key, `Accept-Language` = sc_language(language), ...,
    `User-Agent` = sc_user_agent())
}

sc_language <- function(language = NULL, options = c("en", "de")) {
  if (is.null(language))
    language <- getOption("STATcubeR.language")
  match.arg(language, options)
}

data_frame <- function(...) {
  vctrs::new_data_frame(list(...), class = "tbl")
}
