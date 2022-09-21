.onLoad <- function(...) {
  if (requireNamespace("pillar", quietly = TRUE)) {
    register_s3 <- function(method, class, fun, pkg = "pillar")
      registerS3method(method, class, fun, asNamespace(pkg))
    register_s3("tbl_format_footer", "sc_meta", tbl_format_footer.sc_meta)
    register_s3("tbl_sum", "sc_meta", tbl_sum.sc_meta)
    register_s3("tbl_sum", "sc_tibble", tbl_sum.sc_tibble)
  }

  if (in_pkgdown()) {
    options(cli.theme = list(
      ".field" = list("color" = "#0d0d73"),
      ".code" = list("color" = "blue"),
      ".fun" = list("color" = "#350b8e"),
      "span.version" = list("color" = "#910808"),
      "span.timestamp" = list("color" = "#430674")
    ))
    options(cli.hyperlink = TRUE)
    options("cli.num_colors" = 16777216L)
    options(fansi.warn = FALSE)
    Sys.setenv(R_CLI_HYPERLINK_MODE = "posix")
  }
}
