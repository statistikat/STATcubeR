#' Invoke the /schema endpoint and recurse into all valuesets. The return
#' value can be displayed as a tree object
#' @param db_id a database id
#' @inheritParams sc_key
#' @examples
#' \dontrun{
#'
#' db_schema <- sc_schema_db("deake005")
#'
#' # printing
#' db_schema
#'
#' # access child nodes
#' db_schema$`Demographic Characteristics`
#' db_schema$`Demographic Characteristics`$Gender$Gender
#' db_schema$`Demographic Characteristics`$Gender$Gender$male
#'
#' # access the raw response from httr::GET()
#' my_response <- attr(db_schema, "response")
#' my_response$headers$date
#' my_content <- httr::content(my_response)
#' my_content$label
#' }
#' @rdname sc_schema
#' @export
sc_schema_db <- function(db_id, key = sc_key()) {
  response <- sc_get_schema(key = sc_key(), "/str:database:", db_id, "?depth=valueset")
  content <- httr::content(response)
  x <- tabulate_db_info(content)
  attr(x, "response") <- response
  x
}

drop_values <- function(x) {
  if (!is.list(x))
    return(x)
  if (!is.null(x$type) && x$type == "VALUE")
    return(NULL)
  lapply(x, drop_values)
}

tabulate_db_info <- function(db, drop_value = FALSE) {
  ret <- lapply(db$children, function(x) {
    if (x$type == "VALUE")
      return(x)
    tabulate_db_info(x)
  })
  names(ret) <- sapply(db$children, function(x) x$label)
  ret <- c(ret, db[which(names(db) != "children")])
  class(ret) <- "sc_schema"
  ret
}

#' @param value show resources of type `VALUE`?
#' @param x object to be printed
#' @param limit maximum number of entries to be printed
#' @param ... ignored
#' @rdname sc_schema
#' @export
print.sc_schema <- function(x, limit = 30, value = FALSE, ...) {
  if (!any(sapply(x, is.list)))
    print(unclass(x))
  else {
    if (x$type == "VALUESET")
      value <- TRUE
    if (!value)
      x <- drop_values(x)
    x <- unclass(x) %>% data.tree::as.Node(nodeName = x$label, check = "no-check")
    data.tree::Prune(x, function(node) {
      !is.null(node$type) &&
        !(node$type == "FOLDER" && length(node$children) == 0) &&
        !(node$type == "TABLE")
    })
    data.tree::Do(data.tree::Traverse(x), function(node) {
      if (!is.null(node$type) && node$type == "STAT_FUNCTION")
        node$type <- node$location %>% strsplit(":") %>% .[[1]] %>%
          utils::tail(1)
    })
    print(x, limit = limit, ..., "type")
  }
}
