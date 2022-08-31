sc_tibble_meta <- function(x, names_keep = c()) {
  rownames(x) <- NULL
  names_keep <- c("code", "label", names_keep)
  stopifnot(all(names_keep %in% names(x)))
  class(x) <- c("sc_tibble_meta", "tbl", class(x))
  attr(x, "names_keep") <- names_keep
  attr(x, "names_skip") <- setdiff(names(x), names_keep)
  x
}

tbl_sum.sc_meta <- function(x, ...) {
  paste0("STATcubeR metadata: ", format(nrow(x), big.mark = ","), " x ",
         ncol(x) + length(attr(x, "names_skip")))
}

tbl_format_footer.sc_meta <- function(x, setup, ...) {
  names_skip <- attr(x, "names_skip")
  c(NextMethod(), if (length(names_skip)) paste0(
    "\033[38;5;246m#", " \u2026", " with ", length(names_skip),
    " more columns: ", paste(shQuote(names_skip), collapse = ", "), "\033[39m")
  )
}

#' @export
print.sc_tibble_meta <- function(x, ...) {
  names_keep <- attr(x, "names_keep")
  xx <- x
  class(xx) <- c("sc_meta", setdiff(class(x), "sc_tibble_meta"))
  xx <- xx[, names_keep]
  attr(xx, "names_skip") <- attr(x, "names_skip")
  print(xx, ...)
  invisible(x)
}

sc_tibble <- function(x) {
  class(x) <- c("sc_tibble", "tbl", class(x))
  x
}

tbl_sum.sc_tibble <- function(x, ...) {
  paste0("A STATcubeR tibble: ", format(nrow(x), big.mark = ","), " x ", ncol(x))
}

pillar_shaft.sc_measure <- function(x, ...) {
  x[x == 0] <- NA
  ret <- pillar::pillar_shaft(as.numeric(x), ...)
  class(ret) <- c("pillar_shaft_sc_measure", class(ret))
  ret$is_na <- is.na(as.numeric(x))
  ret$annotations <- attr(x, "annotations") %>% sapply(paste, collapse = ",")
  ret
}

#' @export
format.pillar_shaft_sc_measure <- function(x, width, ...) {
  ret <- NextMethod("format", x, width, ...)
  ind <- x$is_na & x$annotations != ""
  ret[ind] <- cli::col_red(x$annotations[ind])
  ind <- !x$is_na & x$annotations != ""
  ret[ind] <- cli::col_red(ret[ind])
  ret
}

pillar_shaft.sc_field <- function(x, ...) {
  ret <- NextMethod("pillar_shaft", x, ...)
  class(ret) <- c("pillar_shaft_sc_field", class(ret))
  ret$is_total <- x == "SC_TOTAL"
  ret
}

#' @export
format.pillar_shaft_sc_field <- function(x, width, ...) {
  ret <- NextMethod("format", x, width, ...)
  ind <- x$is_total
  ret[ind] <- cli::col_red(ret[ind])
  ret
}

vec_ptype_abbr.sc_measure <- function(x) {
  "dbl"
}

print.sc_measure <- function(x, ...) {
  print(as.numeric(x))
}
