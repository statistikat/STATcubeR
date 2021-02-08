#' Info and rate limit
#'
#' Utilize the simple endpoints `/info` and `/ratelimit`. Those provide
#' information about available locales and the amount of requests available
#' for calls against the `/table` endpoint.
#' @examples
#' \dontrun{
#'
#' sc_info()
#' sc_rate_limit()
#' }
#' @name info_and_rate_limit
#' @rdname info_and_rate_limit
#' @inheritParams sc_key
#' @export
sc_info <- function(key = sc_key()) {
  response <- httr::GET(
    url = paste0(base_url, "/info"),
    config = httr::add_headers(APIKey = key)
  )
  info_content <- httr::content(response)
  info_content$languages %>%
    lapply(function(x)
      data.frame(locale = x$locale, displayName = x$displayName)) %>%
    do.call(rbind, .)
}
