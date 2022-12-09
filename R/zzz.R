.onLoad <- function(...) {
  if (in_pkgdown())
    cli_theme_pkgdown()
}

cli_theme_pkgdown <- function() {
  options(cli.hyperlink_run = FALSE)
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
  options(cli.hyperlink_run = TRUE)
  Sys.unsetenv("R_CLI_HYPERLINK_MODE")
  options(cli.theme = NULL)
  options(fansi.warn = NULL)
  cli::stop_app()
}
