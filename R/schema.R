#' Create a request against the /schema endpoint
#' @description
#' Invoke the [**/schema**](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/schema-endpoint) endpoint of the STATcube REST API.
#' This endpoint can be used to get all available databases and tables
#' as well as metadata about specific databases.
#'
#' The main function `sc_schema()` can be used with any resource id.
#' [sc_schema_catalogue()] and [sc_schema_db()] are very simple
#' wrapper functions around [`sc_schema()`] and are comparable to the
#' catalogue explorer or the
#' table view of the STATcube GUI.
#'
#' The responses of the API are tree-like data structures which
#' are wrapped into a class called `sc_schema` to simplify the usage in R.
#' @inheritParams sc_key
#' @inheritParams sc_table
#' @param language The language to be used for labeling. `"en"` or `"de"`
#' @param id A resource identifier in uid format. In case of `sc_schema_db()`,
#'   this should be a database id. For `sc_schema()` any resource-id
#'   (folder, measure, table, ...) is accepted.
#' @param depth If provided, the request will recurse into the given level.
#'   For datasets, available options are `NULL` (no recursion), `"folder"`,
#'   `"field"` and `"valueset"`. For the catalogue, only `NULL` and `"folder"`
#'   are applicable.
#' @family functions for /schema
#' @return
#' - for [sc_schema()] and [sc_schema_db()]: an object of class `sc_schema`
#' - for [sc_schema_flatten()]: a `data.frame`
#' - for [sc_schema_catalogue()]: a `list`
#' @export
sc_schema <- function(id = NULL, depth = NULL,
                      language = NULL, key = NULL, server = "ext") {
  language <- sc_language(language)
  if (is.null(key))
    key <- sc_key(server)
  response <- sc_with_cache(
    c("sc_schema", id, depth, language, key), function() {
      httr::GET(
        url = paste0(
          base_url(server), "/schema",
          ifelse(is.null(id), "", paste0("/", id)),
          ifelse(is.null(depth), "", paste0("?depth=", depth))
        ),
        config = sc_headers(language, key, server)
      ) |> sc_check_response()
    })
  content <- httr::content(response)
  x <- sc_as_nested_list(content)
  attr(x, "response") <- response
  x
}

print_schema_with_tree <- function(x, ...) {
  stopifnot(requireNamespace("data.tree", quietly = TRUE))
  x <- data.tree::as.Node(unclass(x), nodeName = x$label, check = "no-check")
  print(x, ..., "type")
  invisible(x)
}

#' @rdname sc_schema
#' @param x an object of class `sc_schema()` i.e. the return value of
#'   [sc_schema()], [sc_schema_db()] or [sc_schema_catalogue()].
#' @param tree whether to use the [`data.tree`](https://rdrr.io/cran/data.tree/man/data.tree.html) package for printing.
#' @param limit,... passed to [data.tree::print.Node()] if `tree` is set
#'   to `TRUE`. Ignored otherwise.
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
  style <- cli::make_ansi_style(sc_schema_colors()[[x$type]])
  cat(style(x$type), ": ", cli::style_bold(x$label), "\n", sep = "")
  short_id <- strsplit(x$id, ":")[[1]][3]
  message_empty <- switch(
    x$type,
    DATABASE = c("# Get more metdata with {.run [sc_schema_db('{short_id}')]",
                 "(STATcubeR::sc_schema_db('{x$id}'))}"),
    TABLE = c("# Get the data with {.run [sc_table_saved('{short_id}')]",
              "(STATcubeR::sc_table_saved('{x$id}'))}"),
    NULL
  )
  sc_schema_print_children(x, message_empty = message_empty, ...)
  invisible(x)
}

sc_schema_print_children <- function(x, message_empty = NULL, ...) {
  classes <- sapply(x, class)
  ind <- which(classes == "sc_schema")
  child_schemas <- names(x)[ind]
  if (length(child_schemas) > 0) {
    children <- vctrs::new_data_frame(list(
      child = new_schema_uri(
        label = child_schemas,
        uri = sapply(ind, function(i) x[[i]]$id)
      ),
      type = sc_schema_type(sapply(ind, function(i) x[[i]]$type)),
      n = sapply(ind, function(i) {
        sum(sapply(x[[i]], class) == "sc_schema")
      })
    ), class = c("tbl_df", "tbl"))
    if (all(children$n == 0))
      children$n <- NULL
    formatted <- format(children, ...)
    cat(formatted[seq(4, length(formatted))], sep = "\n")
  } else if (!is.null(message_empty)) {
    short_id <- strsplit(x$id, ":")[[1]][3]
    cat(cli::format_inline(message_empty), "\n")
  }
}

sc_as_nested_list <- function(x) {
  ret <- lapply(x$children, sc_as_nested_list)
  names(ret) <- sapply(x$children, function(x) x$label)
  ret <- c(ret, x[which(names(x) != "children")])
  class(ret) <- "sc_schema"
  ret
}

#' @describeIn sc_schema turns a `sc_schema` object into a `data.frame`
#' @param type a schema type such as "DATABASE", "VALUE" or "TABLE".
#'   See \href{https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/schema-endpoint#id-.SchemaOpenDataAPIv9.9.6-SchemaTypesandAssociatedIDSchemes}{the API reference} for a list of all schema types.
#' @export
sc_schema_flatten <- function(x, type) {
  stopifnot(inherits(x, "sc_schema"))
  type <- match.arg(toupper(type), names(sc_schema_colors()))
  response <- attr(x, "response")
  stopifnot(!is.null(response))
  response <- httr::content(response)
  flattened <- sc_schema_flatten_impl(response, type)
  flattened <- vctrs::new_data_frame(flattened,
    class = c("sc_schema_flatten", "tbl", "tbl_df"))
  flattened
}

#' @export
print.sc_schema_flatten <- function(x, ...) {
  y <- x
  y$id <- new_schema_uri(x$id, x$id)
  class(y) <- setdiff(class(x), "sc_schema_flatten")
  print(y, ...)
  invisible(x)
}

sc_schema_flatten_impl <- function(resp, type) {
  id <- character()
  label <- character()
  if (!is.null(resp$children)) {
    ret <- lapply(resp$children, sc_schema_flatten_impl, type)
    id <- unlist(lapply(ret, function(x) x$id))
    label <- unlist(lapply(ret, function(x) x$label))
  }
  if (resp$type == type) {
    id <- c(resp$id, id)
    label <- c(resp$label, label)
  }
  list(id = id, label = label)
}

#' @describeIn sc_schema is similar to the
#' [catalogue explorer](`r sc_browse_catalogue()`) of the STATcube GUI and returns
#' a tree-type object containing all databases and tables.
#' @export
sc_schema_catalogue <- function(depth = "folder", ...) {
  sc_schema(id = NULL, depth = depth, ...)
}

