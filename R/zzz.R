.onLoad <- function(...) {
  if (requireNamespace("pillar", quietly = TRUE)) {
    register_s3 <- function(method, class, fun, pkg = "pillar")
      registerS3method(method, class, fun, asNamespace(pkg))
    register_s3("tbl_format_footer", "sc_meta", tbl_format_footer.sc_meta)
    register_s3("tbl_sum", "sc_meta", tbl_sum.sc_meta)
    register_s3("tbl_sum", "sc_tibble", tbl_sum.sc_tibble)
  }
}
