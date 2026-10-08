od_url <- function(server = c("ext", "red"), ..., sep = "/") {
  base_url <- switch(
    match.arg(server),
    ext = "https://data.statistik.gv.at",
    red = "http://sdbred:8080/data.statistik.gv.at"
  )
  paste(base_url, ..., sep = sep)
}

# Check whether the open data server is reachable and returns valid content.
# The /ogd/revision endpoint is used as a health check because it returns
# clean JSON. This also detects cases where the server is technically
# reachable (HTTP 200) but serves an intermediate html page (e.g. during
# maintenance work). The result is cached per session, so the check costs
# at most one request per R session.
#' @title Check availability of the OGD server
#'
#' @description
#' Returns `TRUE` if the open data server of Statistics Austria is reachable
#' and returns valid (JSON) content. This also detects cases where the server
#' is technically reachable (HTTP status 200) but serves an intermediate
#' html page (e.g. during maintenance work). The result is cached within an
#' R session. It is mainly used internally to guard examples and network
#' functions against an unavailable server.
#'
#' @param server the OGD-Server to check. `"ext"` for the external server
#'   (the default) or `"red"` for the editing server
#' @param timeout timeout of the health check request in seconds
#' @return a [logical()] of length one
#' @keywords internal
#' @export
od_server_reachable <- function(server = c("ext", "red"), timeout = 5) {
  server <- match.arg(server)
  if (isTRUE(getOption("STATcubeR.skip_server_check", FALSE)))
    return(FALSE)
  key <- paste0("server_reachable_", server)
  if (is.null(sc_env[[key]])) {
    r <- tryCatch(
      httr::GET(od_url(server, "ogd", "revision"), httr::timeout(timeout)),
      error = function(e) NULL
    )
    sc_env[[key]] <- !is.null(r) && !httr::http_error(r) &&
      identical(httr::http_type(r), "application/json")
  }
  sc_env[[key]]
}

# Gracefully inform the user that a server is not available.
# Used as a guard in top-level exported functions, which return
# invisible(NULL) in this case.
abort_unavailable <- function(url) {
  cli::cli_alert_danger(c(
    "The server {.url {url}} is currently not available",
    " (possibly under maintenance). Please try again later.",
    "i" = "Returning {.code NULL}."
  ))
  invisible(NULL)
}

url_host <- function(url) {
  sub("^(https?://)?([^/]+).*$", "\\2", url)
}

od_abort_unavailable <- function(server = "ext") {
  abort_unavailable(url_host(od_url(server)))
}

od_get_total_code <- function(code, parent) {
  if (length(parent) > 1 && sum(is.na(parent)) == 1) {
    code[which(is.na(parent))]
  } else {
    NA_character_
  }
}

od_attr <- function(json) {
  desc <- paste0(";", json$extras$attribute_description)
  index_c <- gregexpr(";C-", desc)[[1]]
  index_f <- gregexpr(";F-", desc)[[1]]
  index_code <- sort(c(index_c, index_f))
  index_colon <- gregexpr(":", desc)[[1]]
  index_end <- c(index_code[-1], 1000000L) - 1
  code <- character(0)
  label <- character(0)
  for (i in seq_along(index_code)) {
    next_col <- min(index_colon[index_colon > index_code[i]])
    code <- c(code, substr(desc, index_code[i] + 1, next_col - 1))
    label <- c(label, substr(desc, next_col + 1, index_end[i]))
  }
  data_frame(label = label, code = code)
}

od_create_data <- function(id, json = od_json(id), lang = NULL,
                           server = "ext") {
  lang <- sc_language(lang)
  resources <- od_resource_all(id, json, server)
  dat <- resources$data[[1]]
  header <- resources$data[[2]]
  meta <- list(
    source = data_frame(code = id, label = NA, label_de = json$title,
                        label_en = json$extras$en_title_and_desc),
    measures = header[substr(header$code, 1, 1) == "F", ],
    fields   = header[substr(header$code, 1, 1) == "C", ]
  )

  fields <- lapply(seq_along(meta$fields$code), function(i) {
    code <- meta$fields$code[i]
    fld <- resources$data[[2 + i]]
    fld$label_en[is.na(fld$label_en)] <- fld$label_de[is.na(fld$label_en)]
    fld
  })

  meta$measures$NAs <- sapply(dat[meta$measures$code], function(x) sum(is.na(x)))
  meta$fields$nitems <- sapply(fields, nrow)
  meta$fields$type <- sapply(fields, function(x) sc_field_type(x$code))
  meta$fields$total_code <- NA_character_
  meta$fields$label_en[is.na(meta$fields$label_en)] <-
    meta$fields$label_de[is.na(meta$fields$label_en)]
  meta$measures$label_en[is.na(meta$measures$label_en)] <-
    meta$measures$label_de[is.na(meta$measures$label_en)]

  for (i in seq_along(fields)) {
    fields[[i]]$parsed <- switch(
      meta$fields$type[i],
      Category = NA_character_,
      sc_field_parse_time(fields[[i]]$code)
    )
    j <- match(meta$fields$code[i], names(dat))
    dat[[j]] <- factor(dat[[j]], fields[[i]]$code)
    meta$fields$total_code[i] <- od_get_total_code(fields[[i]]$code, fields[[i]]$parent)
  }

  resources$name <- paste0(resources$name, ".csv")
  od <- attr(json, "od")
  resources <- rbind(data_frame(
    name = paste0(id, ".json"), last_modified = json$extras$metadata_modified |>
      as.POSIXct(format = "%Y-%m-%dT%H:%M:%OS"), cached = od$cached,
    size = od$size, download = od$download, parsed = NA), resources[1:6]
  )

  list(data = dat, meta = meta, fields = fields, resources = resources,
       header = header)
}

od_label_data <- function(table, x = table$data, parse_time = TRUE, language = NULL) {
  if (is.null(language))
    language <- table$language
  column <- paste0("label_", language)
  field_codes <- table$meta$fields$code
  for (i in which(field_codes %in% names(x))) {
    field <- table$field(i)
    code <- field_codes[i]
    order <- field$order
    if (is.character(field$parsed))
      levels(x[[code]]) <- field[[column]][order(order)]
    else {
      if (parse_time)
        x[[code]] <- field$parsed[order(order)][x[[code]]]
      else
        levels(x[[code]]) <- field[[column]][order(order)]
    }
  }

  idx <- match(names(x), c(table$meta$measures$code, table$meta$fields$code))
  names(x) <- c(table$meta$measures[[column]], table$meta$fields[[column]])[idx]

  x
}

#' @export
format.od_json <- function(x, ...) {
  att <- od_attr(x)
  measures <- att$label[substr(att$code, 1, 1) == "F"]
  fields <- att$label[substr(att$code, 1, 1) == "C"]
  last_modified <- format(as.POSIXct(x$extras$metadata_modified, format = "%Y-%m-%dT%H:%M:%OS"))
  notes <- ""
  if (x$title != x$notes) {
    notes <- c("", cli::style_italic(strwrap(x$notes)), "")
  }
  c(
    cli::style_bold(strwrap(x$title)),
    notes,
    cli_dl2(list(
      Measures = measures, Fields = fields,
      Updated = last_modified, Tags = unlist(x$tags),
      Categories = x$extras$categorization
    ))
  )
}

#' @export
print.od_json <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

cli_dl2 <- function(items, labels = names(items)) {
  labels <- cli::cli_fmt(lapply(labels, function(x) {
    cli::cli_text("{.field {x}}") }))
  out <- character()
  for (i in seq_along(items)) {
    item <- unlist(items[[i]])
    if (length(item) > 10)
      item <- c(item[1:10], paste(cli::symbol$continue,
                                  cli::style_italic("(", length(item) - 10, " more)")))
    new <- cli::ansi_strwrap(paste0(labels[i], ": ", paste(unlist(item), collapse = ", ")), exdent = 2)
    out <- c(out, new)
  }
  out
}
