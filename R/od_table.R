#' @title Create a table-instance from an open-data dataset
#'
#' @description
#'
#' `od_table(id)` returns an `R6`-class object containing all relevant data
#' and metadata from https://data.statistik.gv.at/data/
#'
#' @section Components:
#'
#' **Component**   | **Corresponding File on Server**
#' --------------  | ---------------------------------
#' `$data         `| `https://data.statistik.gv.at/data/${id}.csv`
#' `$header       `| `https://data.statistik.gv.at/data/${id}_HEADER.csv`
#' `$field(code)  `| `https://data.statistik.gv.at/data/${id}_${code}.csv`
#' `$json         `| `https://data.statistik.gv.at/ogd/json?dataset=${id}`
#'
#' @param id the id of the dataset that should be accessed
#' @param language language to be used for labeling. `"en"` or `"de"`
#' @param server the OGD-server to be used. `"ext"` (the default) for the
#'   external server or `prod` for the production server
#'
#' @return
#' The returned objects is of class `sc_table` and inherits several parsing
#' methods from [sc_data]. See [od_table_class] for the full class
#' documentation.
#'
#' @examples
#' x <- od_table("OGD_krebs_ext_KREBS_1")
#'
#' ## metadata
#' x
#' x$meta
#' x$field("Sex")
#' x$field(3)
#'
#' ## data
#' x$data
#' x$tabulate()
#'
#' ## tabulation: see `?sc_tabulate` for more examples
#' x$tabulate("Reporting year", "Sex")
#'
#' ## switch language
#' x$language <- "de"
#' x
#' x$tabulate()
#'
#' ## other interesting tables
#' od_table("OGD_veste309_Veste309_1")
#' od_table("OGD_konjunkturmonitor_KonMon_1")
#' od_table("OGD_krankenbewegungen_ex_LEISTUNGEN_1")
#' od_table("OGD_veste303_Veste203_1")
#' @export
od_table <- function(id, language = NULL, server = "ext") {
  od_table_class$new(id, language, server = server)
}

#' @title Create a table-instance from an open-data dataset
#'
#' @description R6 Class open data datasets.
#' @keywords internal
od_table_class <- R6::R6Class(
  classname = "od_table",
  inherit = sc_data,
  cloneable = FALSE,
  public = list(
    #' @description This class is not exported. Use [od_table()] to
    #' initialize objects of class `od_table`.
    #' @param id the id of the dataset that should be accessed
    #' @param language language to be used for labeling. `"en"` or `"de"`
    #' @param server the OGD-Server server to be used
    initialize = function(id, language = NULL, server = "ext") {
      language <- sc_language(language)
      stime <- Sys.time()
      stopifnot(rlang::is_scalar_character(id))
      private$id <- id
      private$p_server <- server
      json <- od_json(id, server = server)
      private$p_json <- json
      res <- od_create_data(id, json, language, server = server)
      private$cache <- res[c("header", "resources")]
      res$meta$source$requested <- stime
      res$meta$source$lang <- language
      super$initialize(res$data, res$meta, res$fields)
      self$language <- language
      invisible(self)
    },
    #' @description open the metadata for the dataset in a browser
    browse = function() {
      sc_url(od_url(
        'ext', '/web/meta.jsp?dataset=', self$meta$source$code, sep = ""))
    }
  ),
  active = list(
    #' @field json
    #' parsed version of `https://data.statistik.gv.at/ogd/json?dataset=${id}`
    json = function() {
      private$p_json
    },
    #' @field header
    #' parsed version of `https://data.statistik.gv.at/data/${id}_HEADER.csv`.
    #'
    #' Similar contents can be found in `$meta`.
    header = function() {
       sc_tibble_meta(private$cache$header, c("label_de", "label_en"))
    },
    #' @field resources
    #' lists all files downloaded from the server to construct this table
    resources = function() {
      resources <- private$cache$resources
      class(resources$name) <- c("ogd_file", "character")
      class(resources$last_modified) <- c("sc_dttm", class(resources$last_modified))
      class(resources$cached) <- c("sc_dttm", class(resources$cached))
      resources
    },
    #' @field od_server
    #' The server used for initialization (see to `?od_table`)
    od_server = function() {
      private$p_server
    }
  ),
  private = list(
    id = NULL,
    p_json = NULL,
    cache = NULL,
    p_server = NULL
  )
)

#' @export
print.od_table <- function(x, ...) {
  cat(format(x), sep = "\n")
}

#' @export
format.od_table <- function(x, ...) {
  c(
    cli::style_bold(strwrap(x$meta$source$label)),
    "",
    cli_dl2(list(
      Dataset = paste0(cli::style_hyperlink(
        x$meta$source$code, x$browse()), " (",
        cli::style_italic("data.statistik.gv.at"), ")"),
      Measures = x$meta$measures$label,
      Fields = paste0(x$meta$fields$label, cli::style_italic(paste0(
        " <", x$meta$fields$nitems, ">")))
    )),
    "",
    cli_dl2(c(Request = cli_class(x$meta$source$requested, "timestamp"),
              STATcubeR = cli_class(x$meta$source$scr_version, "version")))
  )
}
