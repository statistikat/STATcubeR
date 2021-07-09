od_table_class <- R6::R6Class(
  classname = "od_table",
  cloneable = FALSE,
  public = list(
    initialize = function(id) {
      stime <- Sys.time()
      stopifnot(rlang::is_scalar_character(id))
      private$id <- id
      r <- httr::GET(url =  od_url(id = id))
      if (is.null(httr::content(r))) {
        stop(shQuote(id), " ist keine vorhandene Datensatz-Id", call. = FALSE)
      }
      private$httr_response <- r
      if (r$status_code != 200) {
        stop("angegebene Resource wurde nicht gefunden (err = ", r$status_code, ")", call. = FALSE)
      }
      res <- od_create_data(r, id = id)
      if (!is.null(res)) {
        attr(res, "time") <- as.numeric(difftime(Sys.time(), stime, unit = "secs"))
      }
      private$cache <- res
      invisible(self)
    },
    field = function(i = 1) {
      if (missing(i)) {
        return(private$cache$fields)
      }
      private$cache$fields[[i]]
    },
    tabulate = function(...) {
      od_tabulate(self, ...)
    },
    render = function() {
      measures <- self$meta$measures
      measures2 <- data.frame(
        Name = measures$label,
        Fehlend = measures$NAs,
        Anmerkungen = measures$annotations,
        Aggregation = sapply(measures$fun, switch, SUM = "Summe", MEDIAN = "Median", MEAN = "Arith. Mittel"),
        Kommast. = measures$precision
      )
      fields <- self$meta$fields
      fields2 <- data.frame(
        Name = fields$label,
        "Auspr\u00e4gungen" = fields$nitems,
        Typ = fields$type,check.names = FALSE
      )
      rownames(fields2) <- NULL

      db_id <- self$meta$database$code
      db_link <- paste0(
        " (<a href='https://data.statistik.gv.at/web/meta.jsp?dataset=",
        self$meta$database$code, "' target = '_blank'><code>", db_id, "</code></a>)"
      )

      htmltools::tagList(
        htmltools::tags$h3("Datenbank"),
        htmltools::HTML(paste0(self$meta$database$label, db_link)),
        htmltools::tags$h3("Messwerte"),
        reactable::reactable(measures2, columns = list(
          Kommast. = reactable::colDef(width = 90),
          Anmerkungen = reactable::colDef(width = 120),
          Aggregation = reactable::colDef(width = 100),
          Fehlend = reactable::colDef(width = 70)
        ), compact = TRUE, bordered = TRUE, striped = FALSE, wrap = FALSE,
        rownames = FALSE, defaultPageSize = 5, language = reactable::reactableLang(
          pageInfo = "Messwerte {rowStart} bis {rowEnd} von {rows}",
          pagePrevious = "\u276e",
          pageNext = "\u276f"
        )),

        htmltools::tags$h3("Felder"),
        reactable::reactable(fields2, compact = TRUE, columns = list(
          "Auspr\u00e4gungen" = reactable::colDef(width = 120),
          Typ = reactable::colDef(width = 160)
        ), bordered = TRUE, striped = FALSE, defaultPageSize = 5, details = function(index) {
          field_info <- self$field(index)
          field_info$parsed <- format(field_info$parsed)
          names(field_info) <- c("Kategorie", "Code", "Darstellung")
          htmltools::div(style = "padding-left: 50px;", reactable::reactable(
            language = reactable::reactableLang(
              pageInfo = "Auspr\u00e4gung {rowStart} bis {rowEnd} von {rows}",
              pagePrevious = "\u276e",
              pageNext = "\u276f"
            ),
            field_info, defaultPageSize = 5, wrap = FALSE, columns = list(
              Code = reactable::colDef(width = 120, cell = function(v) {
                htmltools::tags$code(v)
              }),
              Darstellung = reactable::colDef(width = 120)
            )))
        }),
        htmltools::tags$h3("Abfrage"),
        htmltools::HTML(paste0(
          format(self$response$date),
          ", Dauer: ", private$time() %>% round(2), " Sekunden"
        ))

      ) %>% htmltools::browsable()
    }
  ),
  active = list(
    response = function() {
      private$httr_response
    },
    raw = function() {
      httr::content(self$response)
    },
    meta = function() {
      private$cache$meta
    },
    data = function() {
      private$cache$data
    }
  ),
  private = list(
    id = NULL,
    create_time = NULL,
    httr_response = NULL,
    cache = NULL,
    time = function() {
      attributes(private$cache)$time
    }
  )
)

#' Create a table-instance for a open-data Dataset (data.statistik.gv.at)
#'
#' [od_table()] returns an `R6`-class object containing all relevant data
#' and metadata information to be used as input for graphSTAT.
#'
#' @param id the name of the data-set that should be accessed
#'
#' @return An object of class `od_table` which contains the return
#'   value of the [httr::POST()] request in `obj$response`. The object also
#'   provides member functions to parse this response object.
#' @export
#' @examples
#' r1 <- od_table(id = "OGD_veste309_Veste309_1")
#'
#' r1$meta
#' r1$field()
#' r1$field(1)
#' r1$data
#'
#' r2 <- od_table(id = "OGD_krebs_ext_KREBS_1")
#' r2$meta
#' r2$field()
#' r2$field(1)
#' r2$data
#'
#' r3 <- od_table(id = "OGD_konjunkturmonitor_KonMon_1")
#' r3$meta
#' r3$field()
#' r3$field(1)
#' r3$data
od_table <- function(id) {
  od_table_class$new(id = id)
}

#' @export
print.od_table <- function(x, ...) {
  cat("An object of class od_table\n\n")
  cat("Database:     ", x$meta$database$label, "\n")
  cat("Measures:     ", paste(x$meta$measures$label, collapse = ", "),"\n")
  cat("Fields:       ", paste(x$meta$fields$label, collapse = ", "), "\n\n")
  cat("Request:      ", format(x$response$date), "\n")
  cat("OpenData:     ", od_version())
}