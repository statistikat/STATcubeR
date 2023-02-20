#' @title Recode sc_table objects
#' @description
#' A collection of methods that can be used to modify an object of class
#' sc_table by reference. Typical usage is to access the `recode` binding
#' of an `sc_table` object and then use method chaining to perform recode
#' operations.
#'
#' ````
#' x <- od_table("OGD_krebs_ext_KREBS_1")
#' x$recode$
#'   label_field("C-BERJ-0", "de", "JAHR")$
#'   label_measure("F-KRE", "de", "Anzahl")
#' ````
#'
#' See the example section for more details.
#' @examples
#' x <- od_table("OGD_krebs_ext_KREBS_1")
#'
#' x$recode$
#'   label_field("C-KRE_GESCHLECHT-0", "en", "SEX")$
#'   label_measure("F-KRE", "en", "NUMBER")$
#'   level("C-KRE_GESCHLECHT-0", "GESCHLECHT-1", "en", "MALE")
#'
#' x$tabulate("C-KRE_GESCHLECHT-0", "F-KRE")
#'
#' earnings <- od_table("OGD_veste309_Veste309_1")
#' earnings$recode$
#'   total_codes("C-A11-0", "A11-1")$
#'   total_codes("C-STAATS-0", "STAATS-9")$
#'   total_codes("C-VEBDL-0", "VEBDL-10")$
#'   total_codes("C-BESCHV-0", "BESCHV-1")
#'
#' earnings$total_codes()
#'
#' earnings$tabulate("C-STAATS-0")
#' earnings$recode$visible("C-STAATS-0", "STAATS-8", FALSE)
#' earnings$tabulate("C-STAATS-0")
#'
#' earnings$recode$
#'   order("C-A11-0", c("A11-3", "A11-1", "A11-2"))
sc_recoder <- R6::R6Class(
  cloneable = FALSE,
  "sc_recoder",
  list(
    #' @description
    #' Create a new recoder instance. This will automatically
    #'   be performed during the setup of `sc_data` objects
    #' @param x the private environment of an `sc_data` object
    initialize = function(x) {
      stopifnot(is.environment(x))
      private$x <- x
    },
    #' @description Change the label of a field variable
    #' @param field a field code
    #' @param language a language, "de" or "en"
    #' @param new the new label
    label_field = function(field, language, new) {
      i <- private$match_index(field, "fields")
      private$x$p_meta$fields[i, private$l(language)] <- new
      invisible(self)
    },
    #' @description Change the label of a measure variable
    #' @param measure a measure code
    #' @param language a language "de" or "en"
    #' @param new the new label
    label_measure = function(measure, language, new) {
      i <- private$match_index(measure, "measures")
      private$x$p_meta$measures[i, private$l(language)] <- new
      invisible(self)
    },
    #' @description Change the labels of a level
    #' @param field a field code
    #' @param level a level code for the field
    #' @param language a language "de" or "en"
    #' @param new the new label for the level
    level = function(field, level, language, new) {
      i <- private$match_index(field, "fields")
      j <- match(level, private$x$p_fields[[i]]$code)
      stopifnot(!is.na(j))
      private$x$p_fields[[i]][j, private$l(language)] <- new
      invisible(self)
    },
    #' @description Change the total code for a field
    #' @param field a field code
    #' @param new a level code for the field or `NA`. Will be used as the
    #'   new total code. In case of `NA`, the total code will be unset.
    total_codes = function(field, new) {
      i <- private$match_index(field, "fields")
      stopifnot(is.na(new) || new %in% private$x$p_fields[[i]]$code)
      private$x$p_meta$fields$total_code[i] <- new
      invisible(self)
    },
    #' @description set the visibility of a level. Invisible levels are
    #'   omitted in the output of `$tabulate()` but don't affect aggregation
    #' @param field a field code
    #' @param level a level code for the field
    #' @param new visibility. `TRUE` or `FALSE`
    visible = function(field, level, new) {
      stopifnot(is.logical(new), length(new) > 0)
      i <- private$match_index(field, "fields")
      j <- match(level, private$x$p_fields[[i]]$code)
      stopifnot(!is.na(j))
      private$x$p_fields[[i]]$visible[j] <- new
      invisible(self)
    },
    #' @description set the order of levels.
    #' @param field a field code
    #' @param new the new order. A permutation of all level codes for the field.
    #'   alternatively, an integer vector that defines the permutation.
    order = function(field, new) {
      i <- private$match_index(field, "fields")
      if (is.character(new))
        new <- match(private$x$p_fields[[i]]$code, new)
      stopifnot(is.integer(new), !is.na(new),
                sort(new) == seq_len(nrow(private$x$p_fields[[i]])))
      private$x$p_fields[[i]]$order <- new
      invisible(self)
    }
  ),
  list(
    x = NULL,
    # convert language param into a column name
    l = function(language) {
      switch(language, de = "label_de", en = "label_en", stop("invalid language"))
    },
    match_index = function(code, type = c("measures", "fields")) {
      type <- match.arg(type)
      i <- match(code, private$x$p_meta[[type]]$code)
      stopifnot(!is.na(i))
      i
    }
  )
)
