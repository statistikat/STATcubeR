library(STATcubeR)
sc_key_set(Sys.getenv("STATCUBE_KEY_EXT"))
source("R/df_print.R")
knitr::opts_chunk$set(comment = "#>")
options(
  STATcubeR.schema_colors = list(
    "FOLDER" = "#4400cc", "DATABASE" = "#186868", "TABLE" = "#624918",
    "GROUP" = "#4400cc", "FIELD" = "cyan", "VALUESET" = "cadetblue",
    "VALUE" = "#4400cc", "MEASURE" = "#624918", "STAT_FUNCTION" = "cadetblue",
    "COUNT" = "#624918"
  )
)
source("R/add_tooltip.R")$value
