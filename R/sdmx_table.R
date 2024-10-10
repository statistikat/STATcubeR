#' Import data from SDMX
#'
#' Function that reads STATcube data from an sdmx archive - a zip file
#' consisting of `structure.xml` with metadata and `dataset.xml` for the
#' values.
#'
#' @note [sdmx_table()] should be treated as experimental for now.
#'
#' @param file a "sdmx archive" file that was downloaded from STATcube.
#' @return An object of class `sc_data`
#' @keywords internal
#' @examples
#' x <- sdmx_table(system.file("sdmx/dedemo.zip", package = "STATcubeR"))
#' # print and tabulate
#' x
#' x$tabulate()
#' # explore hierarchies
#' nuts2 <- x$field("C-B00-0")
#' data.frame(label = nuts2$label,
#'   parent = nuts2$label[match(nuts2$parent, nuts2$code)])
#' # extract more data from the raw xml
#' xml2::xml_find_first(x$xml$meta, ".//Name")
#' @export
sdmx_table <- function(file) {
  sdmx_table_class$new(file)
}

sdmx_read <- function(folder = ".") {
  list(
    meta = xml2::xml_ns_strip(xml2::read_xml(sprintf('%s/structure.xml', folder))),
    data = xml2::xml_ns_strip(xml2::read_xml(sprintf('%s/dataset.xml', folder)))
  )
}

sdmx_read_zip <- function(zip_file) {
  exdir <- tempfile()
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE))
  utils::unzip(zipfile = zip_file, exdir = exdir)
  sdmx_read(exdir)
}

sdmx_as_raw_df <- function(x) {
  obs <- x$data |> xml2::xml_find_all(".//ObsValue") |>
    xml2::xml_attr("value") |> as.numeric()
  val <- x$data |> xml2::xml_find_all(".//SeriesKey//Value") |>
    xml2::xml_attr("value")
  # assume that entries of SeriesKey always use the same order
  val_lab <- x$data |> xml2::xml_find_first(".//SeriesKey") |>
    xml2::xml_find_all(".//Value") |> xml2::xml_attr("concept")
  val_split <- split(val, rep(
    seq_len(length(val)/length(obs)), length(obs)
  ))
  names(val_split) <- val_lab
  n <- which(val_lab == "MEASURES_DIMENSION")
  obs_split <- split(obs, val_split[[n]])
  ind <- val_split[[n]] == val_split[[n]][1]
  res <- c(
    lapply(val_split[-n], function(x) {
      u <- x[ind]
      factor(u, unique(u), sdmx_codes(unique(u)))
    }),
    obs_split
  ) |> vctrs::new_data_frame()
  names(res) <- gsub("F-DATA_", "", names(res)) |> sdmx_unescape_codes()
  res
}

sdmx_fields <- function(x) {
  fields <- x$meta |>
    xml2::xml_find_all(".//CodeList[(@id and starts-with(@id, 'C-'))]")
  lapply(fields, function(field) {
    names <- xml2::xml_find_all(field, ".//Name") |>
      xml2::xml_text()
    codes <- xml2::xml_find_all(field, "Code")
    values <- xml2::xml_attr(codes, "value")
    desc <- codes |> xml2::xml_find_all("Description") |>
      xml2::xml_text()
    ann_list <- codes |>
      xml2::xml_find_all(".//common:AnnotationText", flatten = FALSE) |>
      lapply(xml2::xml_text)
    has_ann <- vapply(ann_list, length, 0) > 0
    ann_de <- ann_en <- rep(NA_character_, length(ann_list))
    ann_de[has_ann] <- vapply(ann_list[has_ann], function(x) {x[1]}, "")
    ann_en[has_ann] <- vapply(ann_list[has_ann], function(x) {x[2]}, "")
    ind_de <- seq(1, length(desc), 2)
    ind_en <- seq(2, length(desc), 2)
    parent <- xml2::xml_attr(codes, "parentCode")
    list(
      id = field |> xml2::xml_attr("id"),
      label_en = names[2],
      label_de = names[1],
      elements = vctrs::new_data_frame(list(
        label = desc[ind_en],
        code = sdmx_codes(values),
        parsed = desc[ind_en],
        label_de = desc[ind_de],
        label_en = desc[ind_en],
        parent = factor(parent, levels = values, labels = sdmx_codes(values)),
        de_desc = ann_de,
        en_desc = ann_en
      ))
    )
  })
}

sdmx_meta <- function(x) {
  code_measure <- x$meta |> xml2::xml_find_all(
    ".//*[(@id = 'CL_MEASURES_DIMENSION')]/Code") |> xml2::xml_attr("value")
  label_measure <- x$meta |> xml2::xml_find_all(
    ".//*[(@id = 'CL_MEASURES_DIMENSION')]/Code/Description") |>
    xml2::xml_text()
  code_db <- xml2::xml_find_all(x$meta, ".//ConceptScheme") |>
    xml2::xml_attr("id") |> sdmx_unescape_codes()
  label_dataset <- x$meta |> xml2::xml_find_all(".//ConceptScheme/Name") |>
    xml2::xml_text()
  ind_de <- seq(1, length(label_measure), 2)
  ind_en <- seq(2, length(label_measure), 2)
  prepared <- x$meta |> xml2::xml_find_all(".//message:Prepared") |>
    xml2::xml_text() |> as.POSIXct(format = "%FT%T")
  list(
    source = data_frame(label = label_dataset[2], code = code_db,
                          lang = "en", label_de = label_dataset[1],
                          label_en = label_dataset[2], prepared = prepared),
    measures = data_frame(
      label = label_measure[ind_en],
      code = sdmx_unescape_codes(code_measure),
      label_de = label_measure[ind_de],
      label_en = label_measure[ind_en],
      NAs = rep(0, length(code_measure))
    )
  )
}

#' @export
format.sdmx_table <- function(x, ...) {
  c(
    cli::style_bold(strwrap(x$meta$source$label)),
    "",
    cli_dl2(list(
      Database = cli::style_hyperlink(
        x$meta$source$code, sprintf(
          "https://statcube.at/statistik.at/ext/statcube/openinfopage?id=%s",
          x$meta$source$code)
      ),
      Measures = x$meta$measures$label,
      Fields = x$meta$fields$label
    )),
    "",
    cli_dl2(list(
      Downloaded = cli_class(x$meta$source$prepared, "timestamp"),
      STATcubeR = cli_class(x$meta$source$scr_version, "version")
    ))
  )
}

sdmx_codes <- function(codes) {
  simplified <- gsub("^.*_", "", codes)
  if (anyDuplicated(simplified))
    codes
  else
    simplified
}

sdmx_esc <- function(codes, char) {
  int <- utf8ToInt(char)
  gsub(sprintf("@%x@", int), char, codes, fixed = TRUE)
}

sdmx_unescape_codes <- function(codes) {
  codes |> sdmx_esc("\u5f") |> sdmx_esc("\u7c") |> sdmx_esc("\u2b") |>
    sdmx_esc("\u2e") |> sdmx_esc("\u23") |> sdmx_esc("\u40")
}

sdmx_table_class <- R6::R6Class(
  classname = "sdmx_table", class = TRUE,
  inherit = sc_data,
  list(
    initialize = function(file) {
      x <- sdmx_read_zip(file)
      df <- sdmx_as_raw_df(x)
      fields <- sdmx_fields(x)
      meta <- sdmx_meta(x)
      meta$fields <- data_frame(
        code = vapply(fields, function(x) x$id, ""),
        label = vapply(fields, function(x) x$label_en, "") ,
        label_de = vapply(fields, function(x) x$label_de, ""),
        label_en = vapply(fields, function(x) x$label_en, ""),
        total_code = rep(NA, length(fields)),
        nitems = sapply(fields, function(x) {nrow(x$elements)}),
        type = rep("Category", length(fields))
      )
      fields2 <- lapply(fields, function(x) x$elements)
      names(df) <- c(meta$fields$code, meta$measures$code)
      super$initialize(df, meta, fields2)
      self$language <- "en"
      private$p_xml <- x
    }
  ),
  list(p_xml = NULL),
  list(
    xml = function() { private$p_xml },
    description = function() {
      self$xml$meta |> xml2::xml_find_first(
        sprintf(".//ConceptScheme/Description[(@xml:lang='%s')]", self$language)
      ) |> xml2::xml_text() |> `class<-`("sdmx_description")
    }
  )
)

#' @export
print.sdmx_description <- function(x, ...) {
  desc <- strsplit(x, "\r\n")[[1]]
  desc <- ifelse(grepl("^.)", desc),
                 cli::style_bold(substring(desc, 4)),
                 desc)
  cat(desc, sep = "\n")
}
