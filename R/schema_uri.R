new_schema_uri <- function(label, uri) {
  vctrs::vec_assert(label, character())
  vctrs::vec_assert(uri, character())
  vctrs::new_rcrd(list(label = label, uri = uri), class = "sc_schema_uri")
}

#' @export
format.sc_schema_uri <- function(x, ...) {
  format(vctrs::field(x, "label"), ...)
}

sc_schema_run <- function(uri) {
  run <- paste0("STATcubeR::sc_schema(\"", uri, "\")")
  is_table <- grep("^str:table", uri)
  run[is_table] <- paste0("STATcubeR::sc_table_saved(\"", uri[is_table], "\")")
  run
}

sc_schema_url <- function(uri) {
  url <- rep(NA_character_, length(uri))
  is_database <- grep("^str:database", uri)
  if (length(is_database) > 0)
    url[is_database] <-  sub("^str:database:", "", uri[is_database]) |>
    sc_browse_database(server = "ext") |>
    as.character()
  is_table <- grepl("^str:table", uri) &
    !grepl("^([0-9a-f-])+$", sub("str:table:", "", uri))
  if (length(is_table) > 0)
    url[is_table] <- sub("^str:table:", "", uri[is_table]) |>
    sc_browse_table(server = "ext") |>
    as.character()
  url
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.sc_schema_uri <- function(x, ...) {
  label <- vctrs::field(x, "label")
  formatted <- label
  short_formatted <- substr(formatted, 1, 40)
  uri <- vctrs::field(x, "uri")
  if (cli::ansi_hyperlink_types()$run) {
    run <- sc_schema_run(uri)
    template <- cli::format_inline("{.run [%s](%s)}") |> cli::style_underline()
    formatted <- sprintf(template, run, formatted)
    short_formatted <- sprintf(template, run, short_formatted)
  } else if (cli::ansi_has_hyperlink_support()) {
    url <- sc_schema_url(uri)
    formatted[!is.na(url)] <- cli::style_hyperlink(formatted[!is.na(url)],
       url[!is.na(url)])
    short_formatted[!is.na(url)] <- cli::style_hyperlink(
      short_formatted[!is.na(url)], url[!is.na(url)])
  }
  pillar::new_pillar_shaft_simple(
    formatted,
    width = max(nchar(label)),
    min_width = 40,
    type_sum = "chr",
    short_formatted = short_formatted
  )
}

#' @export
as.character.sc_schema_uri <- function(x, ...) {
  format(x)
}

sc_schema_colors <- function() {
  if (!is.null(getOption("STATcubeR.schema_colors")))
    return(getOption("STATcubeR.schema_colors"))
  list(
    "FOLDER" = "#8470FF", "DATABASE" = "cadetblue", "TABLE" = "peru",
    "GROUP" = "#8470FF", "FIELD" = "cyan", "VALUESET" = "cadetblue",
    "VALUE" = "#8470FF", "MEASURE" = "yellow", "STAT_FUNCTION" = "cadetblue",
    "COUNT" = "cadetblue"
  )
}

sc_schema_type <- function(type) {
  stopifnot(is.character(type), all(type %in% names(sc_schema_colors())))
  vctrs::new_vctr(type, class = "sc_schema_type", inherit_base_type = TRUE)
}

#' @export
pillar_shaft.sc_schema_type <- function(x, ...) {
  type <- vctrs::vec_data(x)
  stl <- sc_schema_colors()
  formatted <- sapply(type, function(y) {
    style <- cli::make_ansi_style(stl[[y]])
    style(y)
  })
  pillar::new_pillar_shaft_simple(formatted, type_sum = "chr")
}
