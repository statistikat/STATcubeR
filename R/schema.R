#' Create a request against the /schema endpoint
#'
#' Invoke the **/schema** endpoint of the STATcube REST API. In case of
#' [sc_schema_catalogue()], recurse into all datasets and tables and return a
#' nested list with ids and labels for all resources. For [sc_schema_db()],
#' recurse into all valuesets and return a list of all resources available
#' tor the specific dataset. The return values can be displayed as a
#' tree object.
#' @inheritParams sc_key
#' @inheritParams sc_table
#' @param language The language to be used for labeling. `"en"` or `"de"`
#' @param resource_id A resource identifier in uid format
#' @param depth If provided, the request will recurse into the given level.
#'   For datasets, available options are `NULL` (no recursion), `"folder"`,
#'   `"field"` and `"valueset"`. For the catalogue, only `NULL` and `"folder"`
#'   are applicable.
#' @family functions for /schema
#' @export
sc_schema <- function(resource_id = NULL, depth = NULL,
                      language = c("en", "de"), key = NULL, server = "ext") {
  language <- match.arg(language)
  if (is.null(key))
    key <- sc_key(server)
  response <- sc_with_cache(
    c("sc_schema", resource_id, depth, language, key), function() {
      httr::GET(
        url = paste0(
          base_url(server), "/schema",
          ifelse(is.null(resource_id), "", paste0("/", resource_id)),
          ifelse(is.null(depth), "", paste0("?depth=", depth))
        ),
        config = sc_headers(language, key, server)
      ) %>% sc_check_response()
    })
  content <- httr::content(response)
  x <- sc_as_nested_list(content)
  attr(x, "response") <- response
  x
}

print_schema_with_tree <- function(x, ...) {
  stopifnot(requireNamespace("data.tree", quietly = TRUE))
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
    return(print_schema_with_tree(x, limit = limit, ...))
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
      n_childs = sapply(x[child_schemas], function(x) {
        sum(sapply(x, class) == "sc_schema")
      }),
      stringsAsFactors = FALSE
    ) %>% `class<-`(c("tbl", "data.frame")) %>% `row.names<-`(NULL) %>% print()
  } else if (!is.null(message_empty))
    cat(message_empty, "\n")
}

sc_as_nested_list <- function(x) {
  ret <- lapply(x$children, sc_as_nested_list)
  names(ret) <- sapply(x$children, function(x) x$label)
  ret <- c(ret, x[which(names(x) != "children")])
  class(ret) <- "sc_schema"
  ret
}

#' @describeIn sc_schema turns an sc_schema object into a `data.frame`
#' @param type a schema type such as "DATABASE", "VALUE" or "TABLE".
#'   See \href{https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/schema-endpoint#id-.SchemaOpenDataAPIv9.9.6-SchemaTypesandAssociatedIDSchemes}{the API reference} for a list of all schema types.
#' @export
sc_schema_flatten <- function(x, type) {
  stopifnot(inherits(x, "sc_schema"))
  response <- attr(x, "response")
  stopifnot(!is.null(response))
  response <- httr::content(response)
  flattened <- sc_schema_flatten_impl(response, type)
  flattened <- as.data.frame(flattened, stringsAsFactors = FALSE)
  class(flattened) <- c("tbl", "data.frame")
  flattened
}

sc_schema_flatten_impl <- function(resp, type) {
  if (resp$type == type)
    return(list(id = resp$id, label = resp$label))
  if (is.null(resp$children))
    return(NULL)
  ret <- lapply(resp$children, sc_schema_flatten_impl, type)
  list(
    id = lapply(ret, function(x) { x$id }) %>% unlist(),
    label = lapply(ret, function(x) { x$label }) %>% unlist()
  )
}
