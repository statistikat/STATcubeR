add_tooltip <- function(id, url, title) {
  tippy::tippy_this(
    id, paste0(
    "<div style='font-size: 15px; text-align: left;'>
       <b>", title, "</b><br/><br/>
       <a href='", url, "' target='_blank'>", url, "</a><br/>
     </div>"),
    allowHTML = TRUE, interactive = TRUE, theme = "light-border", arrow = TRUE,
    animation = "perspective",   placement = 'bottom'
  )
}
