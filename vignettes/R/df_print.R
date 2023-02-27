library(magrittr)
library(pillar)

knit_print.data.frame <- function(x, ...) {
  withr::local_options(width = getOption("width") + 3)
  out <- capture.output(print(x, ...)) %>%
    htmltools::htmlEscape() %>%
    fansi::sgr_to_html() %>%
    paste(collapse = "\n") %>%
    paste0("<pre class='r-output'><code>", ., "</code></pre>")
  knitr::asis_output(out)
}

registerS3method(
  "knit_print", "data.frame", knit_print.data.frame,
  envir = asNamespace("knitr")
)

options(crayon.enabled = TRUE)
options(pillar.min_chars = 30)
options(pillar.bold = TRUE)
options(width = 77)
