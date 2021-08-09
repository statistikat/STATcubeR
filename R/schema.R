#' Create a request against the /schema endpoint
#'
#' Invoke the /schema endpoint of the STATcube REST API. In case of
#' `sc_schema_catalogue()`, recurse into all datasets and tables and return a
#' nested list with ids and labels for all resources. For `sc_schema_db()`,
#' recurse into all valuesets and return a list of all resources available
#' tor the specific dataset. The return values can be displayed as a
#' tree object.
#' @inheritParams sc_key
#' @inheritParams sc_table
#' @param resource_id A resource identifier in uid format
#' @param depth If provided, the request will recurse into the given level.
#'   For datasets, available options are `NULL` (no recursion), `"folder"`,
#'   `"field"` and `"valueset"`. For the catalogue, only `NULL` and `"folder"`
#'   are applicable.
#' @family functions for /schema
#' @export
sc_schema <- function(resource_id = NULL, depth = NULL,
                      language = c("en", "de"), key = sc_key()) {
  response <- httr::GET(
    url = paste0(
      base_url, "/schema",
      ifelse(is.null(resource_id), "", paste0("/", resource_id)),
      ifelse(is.null(depth), "", paste0("?depth=", depth))
    ),
    config = sc_headers(language, key)
  )
  content <- httr::content(response)
  x <- sc_as_nested_list(content)
  attr(x, "response") <- response
  x
}

#' @param value show resources of type `VALUE`?
#' @param x object to be printed
#' @param limit maximum number of entries to be printed
#' @param recursive should the whole tree be shown? Alternatively, only display
#'   the direct children of the current resource.
#' @param ... ignored
#' @rdname sc_schema
#' @export
print.sc_schema <- function(x, limit = 30, value = FALSE, recursive = TRUE, ...) {
  if (!any(sapply(x, is.list)))
    print(unclass(x))
  else {
    if (x$type == "VALUESET")
      value <- TRUE
    if (!value)
      x <- drop_values(x)
    x <- unclass(x) %>% data.tree::as.Node(nodeName = x$label, check = "no-check")
    if (!recursive)
      data.tree::Prune(x, function(node) { length(node$path) <= 2 })
    data.tree::Prune(x, function(node) {
      !is.null(node$type) &&
        !(recursive && node$type == "FOLDER" && length(node$children) == 0) &&
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

drop_values <- function(x) {
  if (!is.list(x))
    return(x)
  if (!is.null(x$type) && x$type == "VALUE")
    return(NULL)
  lapply(x, drop_values)
}

sc_as_nested_list <- function(db, drop_value = FALSE) {
  ret <- lapply(db$children, function(x) {
    if (x$type == "VALUE")
      return(x)
    sc_as_nested_list(x)
  })
  names(ret) <- sapply(db$children, function(x) x$label)
  ret <- c(ret, db[which(names(db) != "children")])
  class(ret) <- "sc_schema"
  ret
}
