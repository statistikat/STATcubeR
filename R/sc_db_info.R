#' Get information about a database
#'
#' Invoke the /schema endpoint and recurse into all valuesets. The return
#' value can be displayed as a tree object
#' @param db_id a database id
#' @inheritParams sc_key
#' @examples
#' my_db_info <- sc_db_info("devgrrgr004")
#'
#' # printing
#' my_db_info
#' print(my_db_info, value = TRUE)
#'
#'# access parsed table
#'my_db_info$parsed[2:7, 1:3]
#'
#'# access the raw response from httr::GET()
#'my_response <- my_db_info$response
#'my_response$headers$date
#'my_content <- httr::content(my_response)
#'my_content$label
#' @export
sc_db_info <- function(db_id, key = sc_key()) {
  response <- sc_get_schema(key = sc_key(), "/str:database:", db_id, "?depth=valueset")
  content <- httr::content(response)
  parsed <- tabulate_db_info(content)
  x <- list(
    response = response,
    parsed = parsed,
    version = sc_version()
  )
  class(x) <- "sc_db_info"
  x
}

#' @param functions show resources of type `STAT_FUNCTION`?
#' @param value show resources of type `VALUE`?
#' @param x object to be printed
#' @param ... ignored
#' @rdname sc_db_info
#' @export
print.sc_db_info <- function(x, functions = FALSE, value = FALSE, ...) {
  parsed <- x$parsed
  if (!functions)
    parsed <- parsed[parsed$type != "STAT_FUNCTION", ]
  if (!value)
    parsed <- parsed[parsed$type != "VALUE", ]
  data.tree::as.Node(parsed) %>% as.data.frame(NULL, FALSE, "type") %>%
    `names<-`(c("", "resource_type")) %>% print()
}

tabulate_db_info <- function(db, pathString = "") {
  ret <- data.frame(id = db$id, label = db$label, type = db$type,
                    pathString = paste0(pathString, "/", db$label))
  if (!is.null(db$children)) {
    ret <- do.call(rbind, c(
      list(ret),
      lapply(db$children, tabulate_db_info, paste0(pathString, "/", db$label))
    ))
  }
  ret
}
