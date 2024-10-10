sc_tibble_meta <- function(x, names_keep = c()) {
  rownames(x) <- NULL
  names_keep <- c("code", "label", names_keep)
  stopifnot(all(names_keep %in% names(x)))
  class(x) <- c("sc_tibble_meta", "tbl", class(x))
  attr(x, "names_keep") <- names_keep
  attr(x, "names_skip") <- setdiff(names(x), names_keep)
  x
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.sc_meta <- function(x, ...) {
  paste0("STATcubeR metadata: ", format(nrow(x), big.mark = ","), " x ",
         ncol(x) + length(attr(x, "names_skip")))
}

style_subtle <- cli::make_ansi_style('#999999')

#' @importFrom pillar tbl_format_footer
#' @export
tbl_format_footer.sc_meta <- function(x, setup, ...) {
  names_skip <- attr(x, "names_skip")
  c(NextMethod(), if (length(names_skip)) style_subtle(
    "# ", cli::symbol$continue, " with ", length(names_skip),
    " more columns: ", paste(shQuote(names_skip), collapse = ", "))
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
  class(x) <- unique(c("sc_tibble", "tbl", class(x)))
  x
}

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.sc_tibble <- function(x, ...) {
  paste0("A STATcubeR tibble: ", format(nrow(x), big.mark = ","), " x ", ncol(x))
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.sc_dttm <- function(x, ...) {
  ymd <- format(x, "%Y-%m-%d")
  hms <- cli::col_silver(format(x, "%H:%M:%S"))
  short <- ymd
  ind <- !is.na(x) & as.numeric(Sys.time()) - as.numeric(x) < 60*24
  short[ind] <- hms[ind]
  long <- paste(ymd, hms)
  long[is.na(x)] <- NA
  short[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(
    long,
    width = 19,
    min_width = 10,
    short_formatted = short,
    type_sum = "dttm"
  )
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.ogd_file <- function(x, ...) {
  pillar::new_pillar_shaft(
    list(x = x),
    width = pillar::get_max_extent(x),
    min_width = 20,
    class = "pillar_shaft_ogd_file",
    type_sum = "chr"
  )
}

#' @export
format.pillar_shaft_ogd_file <- function(x, width, ...) {
  files <- x$x
  if (in_pkgdown()) {
    id <- substr(files[1], 1, nchar(files[1]) - 5)
    files[1:2] <- c("meta.json", "data.csv")
    files <- gsub(paste0(id, "_"), "", files, fixed = TRUE)
  }
  too_long <- nchar(files) > width
  files[too_long] <- paste0(substring(files[too_long], 1, width - 2),
                            cli::symbol$ellipsis)
  if (in_pkgdown()) {
    files <- cli::style_hyperlink(
      files, paste0("https://data.statistik.gv.at/data/", x$x))
  } else {
    files <- as.character(cli::style_hyperlink(files, paste0("file://", path.expand(
      od_cache_dir()), x$x)))
  }
  pillar::new_ornament(files, align = "left")
}

#' @export
pillar_shaft.ogd_id <- function(x, ...) {
  pillar::new_pillar_shaft(list(x = x), width = pillar::get_max_extent(x),
                           min_width = 20, class = "pillar_shaft_ogd_id",
                           type_sum = "chr")
}

#' @export
format.pillar_shaft_ogd_id <- function(x, width, ...) {
  id <- x$x
  too_long <- nchar(id) > width
  id[too_long] <- paste0(substring(id[too_long], 1, width - 2),
                         cli::symbol$ellipsis)
  id <- cli::style_hyperlink(id, paste0(
    "https://data.statistik.gv.at/web/meta.jsp?dataset=", x$x))
  pillar::new_ornament(id, align = "left")
}

