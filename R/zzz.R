.onLoad <- function(...) {
  if (requireNamespace("pillar", quietly = TRUE)) {
    register_s3 <- function(method, class, fun, pkg = "pillar")
      registerS3method(method, class, fun, asNamespace(pkg))
    register_s3("tbl_format_footer", "sc_meta", tbl_format_footer.sc_meta)
    register_s3("tbl_sum", "sc_meta", tbl_sum.sc_meta)
    register_s3("tbl_sum", "sc_tibble", tbl_sum.sc_tibble)
  }

  if (in_pkgdown())
    cli_theme_pkgdown()
}

cli_theme_pkgdown <- function() {
  options(cli.theme = list(
    ".field" = list("color" = "#0d0d73"),
    ".code" = list("color" = "blue"),
    ".fun" = list("color" = "#350b8e"),
    "span.version" = list("color" = "#910808"),
    "span.timestamp" = list("color" = "#430674"),
    ".run" = list(color = "#910808"),
    ".file" = list(color = "#004700", transform = function(x) shQuote(x, "cmd"))
  ))
  options(fansi.warn = FALSE)
  Sys.setenv(R_CLI_HYPERLINK_MODE = "posix")
  cli::stop_app()
}

cli_theme_reset <- function() {
  Sys.unsetenv("R_CLI_HYPERLINK_MODE")
  options(cli.theme = NULL)
  options(fansi.warn = NULL)
  cli::stop_app()
}
