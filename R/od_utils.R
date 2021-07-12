od_url <- function(id) {
  baseurl <- "https://data.statistik.gv.at"
  file.path(baseurl, paste0("ogd/json?dataset=", id))
}

od_attr <- function(rq) {
  desc <- rq$extras$attribute_description %>% paste0(";", .) %>% gsub("; ", ";", .)
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
  data.frame(code = code, label = label)
}

od_create_data <- function(x, id) {
  rq <- httr::content(x, encoding = "UTF-8")
  df <- do.call("rbind", lapply(rq$resources, as.data.frame))
  if (any(df$format != "csv")) {
    stop(paste0("Datensatz ", shQuote(id), " kann nicht eingelesen werden.\n",
    "Grund: es sind nicht alle Metadaten als csv vorhanden"), call. = FALSE)
  }

  suppressWarnings(df_inps <- lapply(df$url, function(x) {
    tryCatch(readr::read_delim(x, delim = ";", col_types = readr::cols(), locale = readr::locale(decimal_mark = ",")), error = function(e) e)
  }))
  names(df_inps) <- df$url

  if (any(sapply(df_inps, function(x) inherits(x, "simpleError")))) {
    stop(paste0(
      "Datensatz ", shQuote(id), " kann nicht eingelesen werden.\n",
      "Grund: Fehler beim Einlesen mit `readr::read_delim()`"), call. = FALSE)
  }

  dat <- df_inps[[1]]

  idx_hdr <- which(grepl("_HEADER", df$name))
  if (length(idx_hdr) == 1) {
    df_hdr <- df_inps[[idx_hdr]][, c("code", "name")]
    df <- df[-idx_hdr, ]
  } else {
    df_hdr <- od_attr(rq)
  }

  cn <- colnames(dat)
  fields <- list()
  if (nrow(df) > 1) {
    cols_facts <- c()
    for (i in 2:nrow(df)) {
      idx <- which(sapply(cn, function(x) grepl(x, df$url[i])))
      ff <- df_inps[[df$url[i]]]
      if (length(idx) == 1) {
        cols_facts <- c(cols_facts, cn[idx])
        cc <- match(dat[[idx]], ff$code)
        dat[[idx]] <- ff$name[cc]

        new_field <- list(data.frame(label = ff$name, code = ff$code, parsed = NA_character_))
        fields <- append(fields, new_field)
        names(fields)[length(fields)] <- cn[idx]
      }
    }
  } else {
    message("datensatz ", id, ": fehlende metadaten")
    return(NULL)
  }

  cols_nums <- setdiff(cn, cols_facts)
  meta <- list()
  meta$database <- data.frame(
    label = rq$title, code = id
  )

  meta$measures <- subset(df_hdr, df_hdr$code %in% cols_nums)[, 2:1]
  colnames(meta$measures) <- c("label", "code")
  meta$measures$fun <- NA_character_
  meta$measures$precision <- NA_integer_
  meta$measures$annotations  <- NA_character_
  meta$measures$NAs <- sapply(dat[, meta$measures$code], function(x) sum(is.na(x)))

  metafields <-  subset(df_hdr, df_hdr$code %in% cols_facts)[, 2:1]
  colnames(metafields) <- c("label", "code")

  metafields$nitems <- sapply(metafields$code, function(x) length(unique(dat[[x]])))
  metafields$type <- sapply(cols_facts, function(x) {
    tryCatch(sc_field_type(df_inps[[1]][[x]]), error = function(e) "Category")
  })

  # any time-field?
  idx <- which(metafields$type != "Category")
  if (length(idx) > 0) {
    for (v in names(idx)) {
      dat[[v]] <- sc_field_parse_time(df_inps[[1]][[v]])
    }
  }
  metafields$total <- TRUE # # todo: check if total is always included
  meta$fields <- metafields

  # colnames
  idx <- match(metafields$code, colnames(dat))
  colnames(dat)[idx] <- metafields$label

  idx <- match(meta$measures$code, colnames(dat))
  colnames(dat)[idx] <- meta$measures$label

  # fields -> numeric
  for (v in meta$measures$label) {
    dat[[v]] <- suppressWarnings(as.numeric(dat[[v]]))
  }
  attr(dat, "spec") <- NULL
  list(data = dat, meta = meta, fields = fields)
}
