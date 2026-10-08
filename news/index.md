# Changelog

## STATcubeR 1.0.1

- now depends on `R >= 4.1.0`
- Examples are skipped when the OGD server or the ‘STATcube’ REST API is
  not reachable or serves an intermediate page (e.g. during maintenance
  work) instead of failing with an error (CRAN check).
- Network functions
  ([`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md),
  [`od_list()`](https://statistikat.github.io/STATcubeR/reference/od_list.md),
  [`od_revisions()`](https://statistikat.github.io/STATcubeR/reference/od_revisions.md),
  [`od_catalogue()`](https://statistikat.github.io/STATcubeR/reference/od_catalogue.md),
  [`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
  [`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md),
  [`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
  [`sc_table_saved_list()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
  [`sc_schema()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md),
  [`sc_info()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md),
  `sc_rate_limit_*()`) inform via a message and return `invisible(NULL)`
  when the server is not available, instead of failing with a cryptic
  parse error. The target server (`ext`/`red`/`prod`) is derived from
  the requested dataset and reported in the message.
- `od_cache_update()` and `sc_check_response()` detect html responses
  (e.g. maintenance pages or firewall rejections) and report an
  informative error.
- New (internal) availability checks
  [`od_server_reachable()`](https://statistikat.github.io/STATcubeR/reference/od_server_reachable.md)
  and
  [`sc_server_reachable()`](https://statistikat.github.io/STATcubeR/reference/sc_server_reachable.md),
  cached per R session.
  [`sc_server_reachable()`](https://statistikat.github.io/STATcubeR/reference/sc_server_reachable.md)
  validates the `server` argument via
  [`match.arg()`](https://rdrr.io/r/base/match.arg.html) and checks
  every server (`ext`/`red`/`prod`/`test`) instead of assuming internal
  servers are reachable.

## STATcubeR 1.0.0

CRAN release: 2024-11-29

- First Version for CRAN
- Update print methods with the [tibble](https://tibble.tidyverse.org/)
  package ([\#32](https://github.com/statistikat/STATcubeR/issues/32))
- New exported function
  [`sdmx_table()`](https://statistikat.github.io/STATcubeR/reference/sdmx_table.md)
  ([\#27](https://github.com/statistikat/STATcubeR/issues/27))
- Simplifications and Code-refacturing

## STATcubeR 0.5.2

- Add filters and other recodes to
  [`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
  ([\#33](https://github.com/statistikat/STATcubeR/issues/33))
- Add global option `STATcubeR.language` to override the default
  language
- [`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md):
  Add descriptions to `x$header` and `x$field(i)`
- Depend on cli \>= 3.4.1 ([@matmo](https://github.com/matmo),
  [\#35](https://github.com/statistikat/STATcubeR/issues/35))
- Allow json strings in
  [`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  ([@matmo](https://github.com/matmo),
  [\#36](https://github.com/statistikat/STATcubeR/issues/36))
- add
  [`sdmx_table()`](https://statistikat.github.io/STATcubeR/reference/sdmx_table.md)
  to import sdmx archives (.zip)

## STATcubeR 0.5.0

- adapt
  [`od_list()`](https://statistikat.github.io/STATcubeR/reference/od_list.md)
  to data.statistik.at update
  ([`2249b66`](https://github.com/statistikat/STATcubeR/commit/2249b6607cb822a4aac56c6258cbe967832171f1))
- Update print methods with the [cli](https://cli.r-lib.org) package
  [\#31](https://github.com/statistikat/STATcubeR/pull/31)

## STATcubeR 0.4.3

- add
  [`od_revisions()`](https://statistikat.github.io/STATcubeR/reference/od_revisions.md)
  to check for updates on the OGD server
- add
  [`od_catalogue()`](https://statistikat.github.io/STATcubeR/reference/od_catalogue.md)
  to combine multiple jsons metadata files into a single catalogue table

## STATcubeR 0.4.2

- add
  [`sc_browse_catalogue()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md),
  [`sc_browse_database()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md)
  and
  [`sc_browse_table()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md)
- add
  [`sc_schema_flatten()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  - update
    [`vignette("sc_schema")`](https://statistikat.github.io/STATcubeR/articles/sc_schema.md)
- improve error handling when interacting with the REST API
  - add
    [`vignette("sc_last_error")`](https://statistikat.github.io/STATcubeR/articles/sc_last_error.md)

## STATcubeR 0.4.1

- adapt
  [`od_list()`](https://statistikat.github.io/STATcubeR/reference/od_list.md)
  to data.statistik.at update
  ([`ea59c71`](https://github.com/statistikat/STATcubeR/commit/ea59c718edec373ba71074005099ef519033bf51))
- extend support for rate limits and update
  [`vignette("sc_info")`](https://statistikat.github.io/STATcubeR/articles/sc_info.md)

## STATcubeR 0.4.0

- Documentation updates
- Document and export caching of API responses
  ([\#23](https://github.com/statistikat/STATcubeR/issues/23))
- Update URLs for the API release
  ([\#29](https://github.com/statistikat/STATcubeR/issues/29))
- Add support for multiple API servers
  ([\#25](https://github.com/statistikat/STATcubeR/issues/25))
- Automatically add totals to OGD datasets
  ([\#28](https://github.com/statistikat/STATcubeR/issues/28))
- Use
  [`rappdirs::user_cache_dir()`](https://rappdirs.r-lib.org/reference/user_cache_dir.html)
  to determine the default value for caching.
- Set up continuous integration via github actions

## STATcubeR 0.3.0

- Allow recodes of `sc_data` objects
  ([\#17](https://github.com/statistikat/STATcubeR/issues/17))
- Better parsing of time variables
  ([\#15](https://github.com/statistikat/STATcubeR/issues/15),
  [\#16](https://github.com/statistikat/STATcubeR/issues/16))
- Use bootstrap 5 and [pkgdown](https://pkgdown.r-lib.org/) 2.0.0 for
  the website
- Allow export and import of open data using tar archives
  ([\#20](https://github.com/statistikat/STATcubeR/issues/20))

## STATcubeR 0.2.4

- add user-agent according to
  [`vignette("api-packages", "httr")`](https://httr.r-lib.org/articles/api-packages.html)
- error handling
  - check for content types and http status consistently
  - document error handling in
    [`?sc_last_error`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md)
  - new export:
    [`sc_last_error()`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md)

## STATcubeR 0.2.3

Almost all changes between 0.2.2 and 0.2.3 are included in
[\#13](https://github.com/statistikat/STATcubeR/issues/13)

- cleanup of some function names
- faster parsing in
  [`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
- remove dependencies to
  [rmarkdown](https://github.com/rstudio/rmarkdown) and
  [rstudioapi](https://rstudio.github.io/rstudioapi/)
- improve caching for REST API to support
  [`sc_schema()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  and
  [`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)

### Documentation updates

- refactor all [pkgdown](https://pkgdown.r-lib.org/) articles including
  old articles about the REST API
- modify readme to showcase OGD, API and the base class
- update reference documentation and reference index

## STATcubeR 0.2.2

This version finalizes
[\#11](https://github.com/statistikat/STATcubeR/issues/11)

- Common base class for OGD data and data from the REST API
- Improved print methods with [tibble](https://tibble.tidyverse.org/)
- Direct documentation of certain [R6](https://r6.r-lib.org) classes
  with [roxygen2](https://roxygen2.r-lib.org/)
- remove unnecessary exports

## STATcubeR 0.2.1

- remove dependency to [openssl](https://jeroen.r-universe.dev/openssl)
- avoid EOL warnings when reading JSON requests
- start using `NEWS.md`
- reorganize `README.md` and put open data there front and center

### Open Data

`STATcubeR` now contains functions to access open government data from
<https://data.statistik.gv.at/>

- new class `od_table` to get OGD data
- methods to tabulate responses
- caching
- four new pkgdown articles for
  [`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md),
  [`od_list()`](https://statistikat.github.io/STATcubeR/reference/od_list.md),
  [`od_resource()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
  and `sc_data`

## STATcubeR 0.2.0

- Update contents for
  [`sc_example()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
- Use `Date` instead of `POSIXct` for time variables
- Cache `$meta` and `$field` in memory for class `sc_table`
- Add caching

## STATcubeR 0.1.1

- Improve
  [`sc_example()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
- Add
  [`sc_examples_list()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  to get all available examples
- add `$browse()` and `$edit()`
- add language parameter to
  [`sc_schema()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
- pkgdown article for custom tables
  ([\#6](https://github.com/statistikat/STATcubeR/issues/6))
- update and harmonize naming of functions and parameters
