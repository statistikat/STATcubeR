
# STATcubeR <img src="man/figures/logo_readme.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/statistikat/STATcubeR.svg?branch=master)](https://travis-ci.org/statistikat/STATcubeR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![GitHub last
commit](https://img.shields.io/github/last-commit/statistikat/STATcubeR.svg?logo=github)](https://github.com/statistikat/STATcubeR/commits/master)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/statistikat/STATcubeR?logo=github)](https://github.com/statistikat/STATcubeR)
<!-- badges: end -->

R Interface for the
[STATcube](http://sdbext:8081/statistik.at/ext/statcube/jsf/dataCatalogueExplorer.xhtml)
REST API. This package exposes certain parts of the [SuperSTAR
API](https://docs.wingarc.com.au/superstar/latest/open-data-api) to
transfer your STATcube tables into R. The package provides utility
functions to create API requests and parser functions to transform the
API responses into common R objects.

## Setup

See the [setup
article](https://statistikat.github.io/STATcubeR/articles/articles/Setup.html)
for instructions on how to install the package and set your API key.
Note that the API is currently only available inside the firewall of
Statistics Austria. Support for external users will be added in the near
future.

## Main Functions

Currently, the package provides four main functions

  - `sc_schema_catalogue()` lists all available datasets and tables
  - `sc_schema_db()` provides metadata about a specific database
  - `sc_table()` requests a table from the API according to a json
    standard.
  - `sc_table_saved()` requests a table based on an id

More information about the first two functions can be found in the
[schema
article](https://statistikat.github.io/STATcubeR/articles/Schema.html).
`sc_table()` and `sc_table_saved()` have their own articles
[here](https://statistikat.github.io/STATcubeR/articles/JSON-requests.html)
and
[here](https://statistikat.github.io/STATcubeR/articles/Saved-Tables.html).

## API documentation

The STATcube API is based on the superSTAR REST API which is documented
on <https://docs.wingarc.com.au>.

  - [`/table`
    endpoint](https://docs.wingarc.com.au/superstar/latest/open-data-api/open-data-api-reference/table-endpoint)
  - [`/schema`
    endpoint](https://docs.wingarc.com.au/superstar/latest/open-data-api/open-data-api-reference/schema-endpoint)
