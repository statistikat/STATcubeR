# generate icons for the reference index

library(fontawesome)

fa_png <- fontawesome::fa_png

if (!fs::dir_exists("icons"))
  fs::dir_create("icons")

color <- "hsl(356deg 27% 70%)"

use_icon <- function(icon, topic, ...) {
  fontawesome::fa_png(icon, sprintf("icons/%s.png", topic), fill = color, ...)
}

use_icon("triangle-exclamation", "sc_last_error")
use_icon("pen-to-square", "sc_recoder")
use_icon("table", "od_table")
use_icon("floppy-disk", "od_table_save")
use_icon("folder-tree", "sc_schema")
use_icon("circle-info", "other_endpoints")
use_icon("table", "sc_table")
use_icon("key", "sc_key")
use_icon("recycle", "sc_cache")
use_icon("recycle", "od_resource")
use_icon("cloud-arrow-down", "od_cache")
use_icon("server", "sc_json_get_server")
use_icon("internet-explorer", "sc_browse") # :)
use_icon("table-list", "od_list")
use_icon("table", "sc_data")
use_icon("table", "sc_tabulate")

pkgdown::build_reference_index()
