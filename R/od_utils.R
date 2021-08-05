od_url <- function(id) {
  baseurl <- "https://data.statistik.gv.at"
  file.path(baseurl, paste0("ogd/json?dataset=", id))
}

od_attr <- function(json) {
  desc <- json$extras$attribute_description %>% paste0(";", .)
  index_c <- gregexpr(";C-", desc) %>% .[[1]]
  index_f <- gregexpr(";F-", desc) %>% .[[1]]
  index_code <- sort(c(index_c, index_f))
  index_colon <- gregexpr(":", desc) %>% .[[1]]
  index_end <- c(index_code[-1], 1000000L) - 1
  code <- character(0)
  label = character(0)
  for (i in seq_along(index_code)) {
    next_col <- min(index_colon[index_colon > index_code[i]])
    code <- c(code, substr(desc, index_code[i] + 1, next_col - 1))
    label <- c(label, substr(desc, next_col + 1, index_end[i]))
  }
  data.frame(label = label, code = code, stringsAsFactors = FALSE)
}

od_get_labels <- function(x, lang = c("en", "de")) {
  lang <- match.arg(lang)
  if (lang == "de")
    return(x$label_de)
  out <- x$label_en
  out[is.na(out) | out == ""] <- x$label_de[is.na(out) | out == ""]
  out
}

od_create_data <- function(id, json = od_json(id), lang = c("en", "de")) {
  lang <- match.arg(lang)
  resources <- od_resource_all(json = json)
  dat <- resources$data[[1]]
  header <- resources$data[[2]]
  meta <- list(
    database = data.frame(code = id, label = NA, label_de = json$title,
                          label_en = json$extras$en_title_and_desc),
    measures = header[substr(header$code, 1, 1) == "F", ],
    fields   = header[substr(header$code, 1, 1) == "C", ]
  )

  fields <- lapply(seq_along(meta$fields$code), function(i) {
    code <- meta$fields$code[i]
    fld <- resources$data[[2 + i]]
    udc <- unique(dat[[code]])
    stopifnot(all(udc %in% fld$code))
    if (length(udc) != nrow(fld))
      message("dropping unused levels in ", shQuote(code), ": ",
              paste(shQuote(setdiff(fld$code, udc)), collapse = ", "))
    fld[fld$code %in% udc, ]
  })

  meta$measures$NAs <- sapply(dat[meta$measures$code], function(x) sum(is.na(x)))
  meta$fields$nitems <- sapply(fields, nrow)
  meta$fields$type <- sapply(fields, function(x) sc_field_type(x$code))
  meta$fields$total_code <- NA_character_

  for (i in seq_along(fields)) {
    fields[[i]]$parsed <- switch(
      meta$fields$type[i],
      Category = NA_character_,
      sc_field_parse_time(fields[[i]]$code)
    )
    j <- match(meta$fields$code[i], names(dat))
    dat[[j]] <- factor(dat[[j]], fields[[i]]$code)
  }

  resources$name <- paste0(resources$name, ".csv")
  od <- attr(json, "od")
  resources <- rbind(data.frame(
    name = paste0(id, ".json"), last_modified = json$extras$metadata_modified %>%
      as.POSIXct(format = "%Y-%m-%dT%H:%M:%OS"), cached = od$cached,
    size = od$size, download = od$download, parsed = NA), resources[1:6]
  )

  list(data = dat, meta = meta, fields = fields, resources = resources,
       header = header)
}

od_label_data <- function(table, x = table$data_raw, parse_time = TRUE) {
  for (i in which(table$meta$fields$code %in% names(x))) {
    field <- table$field(i)
    code <- table$meta$fields$code[i]
    if (is.character(field$parsed))
      levels(x[[code]]) <- field$parsed
    else {
      if (parse_time)
        x[[code]] <- field$parsed[x[[code]]]
      else
        levels(x[[code]]) <- field$label
    }
  }

  idx <- match(names(x), c(table$meta$measures$code, table$meta$fields$code))
  names(x) <- c(table$meta$measures$label, table$meta$fields$label)[idx]

  x
}
