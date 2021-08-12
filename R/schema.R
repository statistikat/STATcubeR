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
  response <- sc_with_cache(
    list(resource_id, depth, language, key),
    function() { httr::GET(
      url = paste0(
        base_url, "/schema",
        ifelse(is.null(resource_id), "", paste0("/", resource_id)),
        ifelse(is.null(depth), "", paste0("?depth=", depth))
      ),
      config = sc_headers(language, key)
    )}
  )
  content <- httr::content(response)
  x <- sc_as_nested_list(content)
  attr(x, "response") <- response
  x
}

print_schema_with_tree <- function(x, ...) {
  stopifnot(requireNamespace("data.tree"))
  x <- unclass(x) %>% data.tree::as.Node(nodeName = x$label, check = "no-check")
  print(x, ..., "type")
  invisible(x)
}

#' @rdname sc_schema
#' @param x object to be printed
#' @param tree wether to use the `data.tree` package for printing.
#' @param limit,... passed to [data.tree::print.Node()]
#' @section Printing with data.tree:
#' `limit` and `...` will simply be ignored if `tree` is set to `FALSE`, which is
#' the default. The printing via `data.tree` can take longer than the default
#' implementation because `x` will need to be converted into a `data.tree` node.
#' To use `data.tree` printing permanently, use
#' ```r
#' options(STATcubeR.print_tree = TRUE)
#' ````
#' @export
print.sc_schema <- function(x, tree = NULL, ..., limit = 30) {
  if (is.null(tree))
    tree <- getOption("STATcubeR.print_tree", FALSE)
  classes <- sapply(x, class)
  if (tree && any(classes == "sc_schema"))
    return(print_schema_with_tree(x, limit = 30, ...))
  cat(x$type, ": ", x$label, "\n", sep = "")
  sc_schema_print_children(x, message_empty = switch(
    x$type,
    DATABASE = paste0("# Get more info with `sc_schema_db('", x$id, "')`"),
    TABLE = paste0("Get the data with `sc_table_saved('", x$id, "')`"),
    NULL
  ))
}

sc_schema_print_children <- function(x, message_empty = NULL) {
  classes <- sapply(x, class)
  child_schemas <- names(x)[classes == "sc_schema"]
  if (length(child_schemas) > 0) {
    data.frame(
      child = child_schemas,
      type = sapply(x[child_schemas], function(x) x$type),
      stringsAsFactors = FALSE
    ) %>% `class<-`(c("tbl", "data.frame")) %>% `row.names<-`(NULL) %>% print()
  } else if (!is.null(message_empty))
    cat(message_empty, "\n")
}

sc_as_nested_list <- function(db) {
  ret <- lapply(db$children, function(x) {
    sc_as_nested_list(x)
  })
  names(ret) <- sapply(db$children, function(x) x$label)
  ret <- c(ret, db[which(names(db) != "children")])
  class(ret) <- "sc_schema"
  ret
}
