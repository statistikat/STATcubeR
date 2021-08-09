sc_field_parse <- function(field, type = sc_field_type(field)) {
  if (type == "Category")
    sc_field_parse_category(field)
  else
    sc_field_parse_time(field)
}

sc_field_codes <- function(field, split_minus = TRUE) {
  res <- field$items %>%
    sapply(function(x) x$uris[[1]] %>% get_var_code(split_minus))
  res[res == ""] <- NA
  res
}

sc_field_type <- function(field) {
  if (is.character(field))
    varcodes <- sapply(field, function(x) utils::tail(strsplit(x, "-")[[1]], 1))
  else
    varcodes <- sc_field_codes(field)
  varcodes <- varcodes[!is.na(varcodes)]
  varcodes <- varcodes[varcodes != "SC_TOTAL"]
  codes_numeric <- all(!is.na(suppressWarnings(as.numeric(varcodes))))
  if (!codes_numeric)
    return("Category")
  if (!all(diff(nchar(varcodes)) == 0) || !(nchar(varcodes)[1] %in% 4:6))
    return("Category")
  year <- as.numeric(substr(varcodes, 1, 4))
  if (!all(year %in% 1900:2150))
    return("Category")
  time_type <- switch(
    as.character(nchar(varcodes[1])),
    `4` = "year", `5` = "quarter", `6` = "month"
  )
  if ((time_type == "quarter") && all(substr(varcodes, 5, 5) %in% 5:6))
    time_type <- "half-year"
  paste0("Time (", time_type, ")")
}

sc_field_parse_category <- function(field) {
  field$items %>% sapply(function(x) x$labels[[1]])
}

sc_as_time <- function(year, month, ind) {
  as.Date(paste0(
    year[!ind], "/", month[!ind], "/1"
  ))
}

sc_field_parse_time_month <- function(remainder) {
  if (nchar(remainder[1]) == 0) {
    rep(1, length(remainder))
  } else if (nchar(remainder[1]) == 1) {
    if (remainder[1] %in% 1:4)
      as.numeric(remainder)*3 - 2
    else
      ifelse(remainder == "5", 1, 6)
  } else {
    as.numeric(remainder)
  }
}

sc_field_parse_time <- function(field) {
  if (is.character(field))
    varcodes <- sapply(field, function(x) utils::tail(strsplit(x, "-")[[1]], 1))
  else
    varcodes <- sc_field_codes(field)
  varcodes[varcodes == "SC_TOTAL"] <- NA
  year <- substr(varcodes, 1, 4)
  remainder <- substr(varcodes, 5, 8)
  month <- sc_field_parse_time_month(remainder)

  parsed <- as.Date(rep(NA, length(varcodes)))
  ind <- is.na(varcodes)
  parsed[!ind] <- sc_as_time(year, month, ind)
  parsed
}
