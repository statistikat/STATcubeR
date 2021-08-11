tags <- htmltools::tags

tippy_dataset <- function(x, text = NULL) {
  id <- x$meta$source$code
  if (is.null(text))
    text <- id
  url <- glue::glue("https://data.statistik.gv.at/web/meta.jsp?dataset={x$meta$source$code}")
  url_json <- glue::glue("https://data.statistik.gv.at/ogd/json?dataset={x$meta$source$code}")
  url_sc <- x$json$extras$metadata_linkage[[1]]
  tooltip <- glue::glue(
    "<div style=' text-align: left;'>
  <b> {x$meta$source$label_en} </b><br/><br/>
  <b>Measures</b>: {paste(x$meta$measures$label, collapse = ', ')}<br/>
  <b>Fields</b>: {paste(x$meta$fields$label, collapse = ', ')}<br/><br/>
  <b>links</b>:
    <a href='{url}' target='_blank'>metadata</a>
    <a href='{url_json}' target='_blank'>json</a>
    <a href='{url_sc}' target='_blank'>STATcube</a>
</div>
")
  tags$u(
    text,
    class = "STATcubeR",
    `data-tippy-content` = tooltip,
    `data-tippy-interactive` = "true",
  )
}

ticle <- function(x) {
  fm <- rmarkdown::yaml_front_matter(paste0(x, ".Rmd"))
  if (is.null(fm$link_text))
    stop("no link text")
  tags$a(
    fm$link_text,
    href = paste0(x, ".html"),
    class = "STATcubeR",
    `data-tippy-content` = tags$div(
      style = "font-size: 14px; text-align: left;",
      tags$b(fm$title), tags$br(),
      fm$description
    )
  )
}

STATcubeR <- tags$a(
  "STATcubeR",
  href = "../index.html",
  `data-tippy-content` = "
  <b>R Package for all things STATcube</b><br/>
  Transfer data from the STATcube REST API or from the Open Data portal into your R sessions
  ",
  class = "STATcubeR"
)

ogd_portal <- tags$a(
  "data.statistik.gv.at",
  href = "https://data.statistik.gv.at",
  `data-tippy-allowHTML` = "true",
  `data-tippy-content` = "
  <b>Open Government Data from Statistics Austria</b><br/>
The open data portal provides datasets from Statistics Austria according to open data guidelines
  ",
  class = "STATcubeR"
)

htmltools::tagList(
  tags$script(src = "https://unpkg.com/@popperjs/core@2"),
  tags$script(src = "https://unpkg.com/tippy.js@6"),
  tags$link(rel = "stylesheet", href = "https://unpkg.com/tippy.js@6/themes/light.css"),
  tags$script(
    "$(document).ready(function () {
    window.setTimeout(\"tippy('.STATcubeR', {allowHTML: true, theme:'light rounded'});\", 50);
    });
    "
  )
)