#' @rdname sc_schema
#' @export
sc_schema_catalogue <- function(depth = "folder", language = c("en", "de"),
                                key = NULL, server = 'ext') {
  sc_schema(depth = depth, language = language, key = key, server = server)
}
