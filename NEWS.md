# STATcubeR 1.0.0

* First Version for CRAN
* Update print methods with the `{tibble}` package (#32)
* New exported function `sdmx_table()` (#27)
* Simplifications and Code-refacturing

# STATcubeR 0.5.2

* Add filters and other recodes to `sc_table_custom()` (#33)
* Add global option `STATcubeR.language` to override the default language
* `od_table()`: Add descriptions to `x$header` and `x$field(i)`
* Depend on cli >= 3.4.1 (@matmo, #35)
* Allow json strings in `sc_table()` (@matmo, #36)
* add `sdmx_table()` to import sdmx archives (.zip)

# STATcubeR 0.5.0

* adapt `od_list()` to data.statistik.at update ([`2249b66`](https://github.com/statistikat/STATcubeR/commit/2249b6607cb822a4aac56c6258cbe967832171f1))
* Update print methods with the `{cli}` package [#31](https://github.com/statistikat/STATcubeR/pull/31)

# STATcubeR 0.4.3

* add `od_revisions()` to check for updates on the OGD server
* add `od_catalogue()` to combine multiple jsons metadata files
  into a single catalogue table

# STATcubeR 0.4.2

* add `sc_browse_catalogue()`, `sc_browse_database()` and `sc_browse_table()`
* add `sc_schema_flatten()`
   * update `vignette("sc_schema")`
* improve error handling when interacting with the REST API
   * add `vignette("sc_last_error")`

# STATcubeR 0.4.1

* adapt `od_list()` to data.statistik.at update ([`ea59c71`](https://github.com/statistikat/STATcubeR/commit/ea59c718edec373ba71074005099ef519033bf51))
* extend support for rate limits and update `vignette("sc_info")`

# STATcubeR 0.4.0

* Documentation updates
* Document and export caching of API responses (#23)
* Update URLs for the API release (#29)
* Add support for multiple API servers (#25)
* Automatically add totals to OGD datasets (#28)
* Use `rappdirs::user_cache_dir()` to determine the
  default value for caching.
* Set up continuous integration via github actions

# STATcubeR 0.3.0

* Allow recodes of `sc_data` objects (#17)
* Better parsing of time variables (#15, #16)
* Use bootstrap 5  and `{pkgdown}` 2.0.0 for the website
* Allow export and import of open data using tar archives (#20)

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
* methods to tabulate responses
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
