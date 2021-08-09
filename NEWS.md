# STATcubeR 0.2.2

* Common base class for OGD data and data from the REST API
* Improved print methods
* Direct documentation of certain R6 classes with roxygen2
* remove unnecessary exports

# STATcubeR 0.2.1

## Open Data

`STATcubeR` now contains functions to access open government data from
https://data.statistik.gv.at/

* new class `od_table` to get OGD data
* methods to tabulate reponses
* caching
* four new pkgdown articles

## Other

* remove dependency to `openssl`
* avoid EOL warnings when reading JSON requests
* start using `NEWS.md`
* reorganize `README.md` and put open data there front and center

# STATcubeR 0.2.0

* Update contents for `sc_examples()`
* Use `Date` instead of `POSIXct` for time variables
* Add caching for `$meta` and `$field` for class `sc_table`

# STATcubeR 0.1.3

* Improvements in caching

# STATcubeR 0.1.2

* Add caching

# STATcubeR 0.1.1

* Improve `sc_examples()`
* Add `sc_examples_list()` to get all available examples
* add `$browse()` and `$edit()`
* add language parameter to `sc_schema()`
* pkgdown article for custom tables
* update and harmonize naming of functions and parameters


