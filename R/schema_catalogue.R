#' @rdname sc_schema
#' @export
sc_schema_catalogue <- function(depth = "folder", language = c("en", "de"),
                                key = sc_key()) {
  sc_schema(depth = depth, language = language, key = key)
}
