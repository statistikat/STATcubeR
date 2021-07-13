od_url <- function(id) {
  baseurl <- "https://data.statistik.gv.at"
  file.path(baseurl, paste0("ogd/json?dataset=", id))
}

od_attr <- function(json) {
  desc <- json$extras$attribute_description %>% paste0(";", .) %>% gsub("; ", ";", .)
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
  data.frame(label = label, code = code)
}

od_json_get <- function(id) {
  r <- httr::GET(url =  od_url(id = id))
  if (length(r$content) == 0)
    stop(shQuote(id), ": invalid dataset id", call. = FALSE)
  httr::content(r)
}

od_json_get_id <- function(json) {
  strsplit(json$extras$metadata_original_portal, "=")[[1]][2]
}

od_cache_up_to_date <- function(json, id = od_json_get_id(json)) {
  cache_file <- paste0(sc_cache_dir(), "/open_data/", id, ".csv")
  if (!file.exists(cache_file))
    return(FALSE)
  last_modified_server <- json$resources[[1]]$last_modified %>%
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%OS")
  last_modified_cache <- file.info(cache_file)$mtime
  last_modified_cache > last_modified_server
}

od_get_csv <- function(id, suffix = NULL, cache = TRUE, rename_vars = TRUE) {
  filename <- c(id, suffix) %>% paste(collapse = "_") %>% paste0(".csv")
  cache_file <- paste0(sc_cache_dir(), "/open_data/", filename)
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  if (!cache || !file.exists(cache_file))
    utils::download.file(
      paste0("https://data.statistik.gv.at/data/", filename),
      cache_file, quiet = TRUE
    )
  x <- utils::read.csv2(cache_file, na.strings = c("", "NA", ":"), check.names = FALSE)
  if (rename_vars && !is.null(suffix))
    x[, 2:1] %>% `names<-`(c("label", "code"))
  else
    x
}

od_create_data <- function(json, id = od_json_get_id(json)) {
  cache <- od_cache_up_to_date(json, id)
  vars <- od_attr(json)
  meta <- list(
    database = data.frame(label = json$title, code = id),
    measures = vars[substr(vars$code, 1, 1) == "F", ],
    fields   = vars[substr(vars$code, 1, 1) == "C", ]
  )

  if (json$resources[[1]]$format != "csv")
    stop("Dataset ", shQuote(id), ": invalid format", call. = FALSE)
  if (length(json$resources) !=  2 + nrow(meta$fields))
    stop("Dataset ", shQuote(id), ": unexpected resources field in json", call. = FALSE)

  dat <- od_get_csv(id, cache = cache)
  if (!setequal(names(dat), vars$code))
    stop(shQuote(paste0(id, ".csv")), " and attribute description do not match", call. = FALSE)

  header <- od_get_csv(id, "HEADER", cache = cache)
  if (!identical(header$code, vars$code))
    stop("HEADER and attribute description do not match", call. = FALSE)
  if (!identical(header$label, vars$label)) {
    message("HEADER and attribute description do not match")
    meta$measures = header[substr(header$code, 1, 1) == "F", ]
    meta$fields   = header[substr(header$code, 1, 1) == "C", ]
  }

  fields <- lapply(meta$fields$code, function(code) {
    fld <- od_get_csv(id, code, cache = cache)
    udc <- unique(dat[[code]])
    fld$code <- as.character(fld$code)
    stopifnot(all(udc %in% fld$code))
    if (length(udc) != nrow(fld))
      message("dropping unused levels in ", shQuote(code))
    fld[fld$code %in% udc, ]
  })

  meta$measures$NAs <- sapply(dat[meta$measures$code], function(x) sum(is.na(x)))
  meta$fields$nitems <- sapply(fields, nrow)
  meta$fields$type <- sapply(fields, function(x) sc_field_type(x$code))

  for (i in seq_along(fields)) {
    fields[[i]]$parsed <- switch(
      meta$fields$type[i],
      Category = factor(fields[[i]]$label, fields[[i]]$label),
      sc_field_parse_time(fields[[i]]$code)
    )
  }

  for (i in seq_along(meta$fields$code)) {
    code <- meta$fields$code[i]
    fld <- fields[[i]]
    dat[[code]] <- fld$parsed[match(dat[[code]], fld$code)]
  }

  idx <- match(header$code, colnames(dat))
  colnames(dat)[idx] <- header$label

  list(data = dat, meta = meta, fields = fields)
}
