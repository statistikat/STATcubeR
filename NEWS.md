# STATcubeR 0.2.4

* add user-agent according to `vignette("api-packages", "httr")`
* error handling
   * check for content types and http status consistently
   * document error handling in `?sc_last_error`
   * new export: `sc_last_error()`

# STATcubeR 0.2.3

Almost all changes between 0.2.2 and 0.2.3 are included in #13

* cleanup of some function names
* faster parsing in `sc_table()`
* remove dependencies to `{rmarkdown}` and `{rstudioapi}`
* improve caching for REST API to support `sc_schema()` and `sc_table_saved()`

## Documentation updates

* refactor all `{pkgdown}` articles including old articles about the REST API
* modify readme to showcase OGD, API and the base class
* update reference documentation and reference index

# STATcubeR 0.2.2

This version finalizes #11

* Common base class for OGD data and data from the REST API
* Improved print methods with `{tibble}`
* Direct documentation of certain `{R6}` classes with `{roxygen2}`
* remove unnecessary exports

# STATcubeR 0.2.1

* remove dependency to `{openssl}`
* avoid EOL warnings when reading JSON requests
* start using `NEWS.md`
* reorganize `README.md` and put open data there front and center

## Open Data

`STATcubeR` now contains functions to access open government data from
https://data.statistik.gv.at/

* new class `od_table` to get OGD data
* methods to tabulate reponses
* caching
* four new pkgdown articles for `od_table()`, `od_list()`, `od_resource()` and `sc_data`

# STATcubeR 0.2.0

* Update contents for `sc_example()`
* Use `Date` instead of `POSIXct` for time variables
* Cache `$meta` and `$field` in memory for class `sc_table`
* Add caching

# STATcubeR 0.1.1

* Improve `sc_example()`
* Add `sc_examples_list()` to get all available examples
* add `$browse()` and `$edit()`
* add language parameter to `sc_schema()`
* pkgdown article for custom tables (#6)
* update and harmonize naming of functions and parameters
