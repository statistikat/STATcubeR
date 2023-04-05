#' Import data from SDMX
#'
#' Function that reads STATcube data from an sdmx archive - a zip file
#' consisting of `structure.xml` with metadata and `dataset.xml` for the
#' values.
#'
#' @param file a "sdmx archive" file that was downloaded from STATcube.
#' @return An object of class `sc_data`
#' @keywords experimental
#' @export
sdmx_table <- function(file) {
  sdmx_read_zip(file) %>%
    sdmx_as_data_object()
}

sdmx_read <- function(folder = ".") {
  list(
    meta = folder %>% sprintf('%s/structure.xml', .) %>% xml2::read_xml() %>%
      xml2::xml_ns_strip(),
    data = folder %>% sprintf('%s/dataset.xml', .) %>% xml2::read_xml() %>%
      xml2::xml_ns_strip()
  )
}

sdmx_read_zip <- function(zip_file) {
  exdir <- tempfile()
  dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE))
  unzip(zipfile = zip_file, exdir = exdir)
  sdmx_read(exdir)
}

sdmx_as_raw_df <- function(x) {
  obs <- x$data %>% xml2::xml_find_all(".//ObsValue") %>%
    xml2::xml_attr("value") %>% as.numeric()
  val <- x$data %>% xml2::xml_find_all(".//SeriesKey//Value") %>%
    xml2::xml_attr("value")
  # assume that entries of SeriesKey always use the same order
  val_lab <- x$data %>% xml2::xml_find_first(".//SeriesKey") %>%
    xml2::xml_find_all(".//Value") %>% xml2::xml_attr("concept")
  val_split <- split(val, rep(
    seq_len(length(val)/length(obs)), length(obs)
  ))
  names(val_split) <- val_lab
  n <- which(val_lab == "MEASURES_DIMENSION")
  obs_split <- split(obs, val_split[[n]])
  res <- c(
    lapply(val_split[-n], function(x) {
      u <- head(x, length(obs_split[[1]]))
      factor(u, unique(u))
    }),
    obs_split
  ) %>% vctrs::new_data_frame()
  names(res) <- gsub("F-DATA_", "", names(res))
  res
}

sdmx_fields <- function(x) {
  fields <- x$meta %>%
    xml2::xml_find_all(".//CodeList[(@id and starts-with(@id, 'C-'))]")
  lapply(fields, function(field) {
    names <- xml2::xml_find_all(field, ".//Name") %>%
      xml2::xml_text()
    codes <- xml2::xml_find_all(field, "Code")
    values <- xml2::xml_attr(codes, "value")
    desc <- codes %>% xml2::xml_find_all("Description") %>%
      xml2::xml_text()
    ann_list <- codes %>%
      xml2::xml_find_all(".//common:AnnotationText", flatten = FALSE) %>%
      lapply(xml2::xml_text)
    has_ann <- vapply(ann_list, length, 0) > 0
    ann_de <- ann_en <- rep(NA_character_, length(ann_list))
    ann_de[has_ann] <- vapply(ann_list[has_ann], function(x) {x[1]}, "")
    ann_en[has_ann] <- vapply(ann_list[has_ann], function(x) {x[2]}, "")
    ind_de <- seq(1, length(desc), 2)
    ind_en <- seq(2, length(desc), 2)
    parent <- xml2::xml_attr(codes, "parentCode")
    list(
      id = field %>% xml2::xml_attr("id"),
      label_en = names[2],
      label_de = names[1],
      elements = vctrs::new_data_frame(list(
        label = desc[ind_en],
        code = values,
        parsed = desc[ind_en],
        label_de = desc[ind_de],
        label_en = desc[ind_en],
        parent = factor(parent, levels = values),
        de_desc = ann_de,
        en_desc = ann_en
      ))
    )
  })
}

sdmx_meta <- function(x) {
  code_measure <- x$meta %>% xml2::xml_find_all(
    ".//*[(@id = 'CL_MEASURES_DIMENSION')]/Code") %>% xml2::xml_attr("value")
  label_measure <- x$meta %>% xml2::xml_find_all(
    ".//*[(@id = 'CL_MEASURES_DIMENSION')]/Code/Description") %>%
    xml2::xml_text()
  code_db <- xml2::xml_find_all(x$meta, ".//ConceptScheme") %>%
    xml2::xml_attr("id")
  label_dataset <- x$meta %>% xml2::xml_find_all(".//ConceptScheme/Name") %>%
    xml2::xml_text()
  ind_de <- seq(1, length(label_measure), 2)
  ind_en <- seq(2, length(label_measure), 2)
  list(
    source = data_frame(label = label_dataset[2], code = code_db,
                          lang = "en", label_de = label_dataset[1],
                          label_en = label_dataset[2]),
    measures = data_frame(
      label = label_measure[ind_en],
      code = code_measure,
      label_de = label_measure[ind_de],
      label_en = label_measure[ind_en],
      NAs = rep(0, length(code_measure))
    )
  )
}

sdmx_as_data_object <- function(x) {
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
  y <- sc_data$new(df, meta, fields2)
  y$language <- "en"
  y
}
