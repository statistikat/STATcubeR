
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

R client for all things [STATcube](http://sdbext:8081/statistik.at/ext/statcube/jsf/dataCatalogueExplorer.xhtml).
Get data from the STATcube REST API or via the open government data portal at
https://data.statistik.gv.at. STATcubeR makes it easy to include both those
datasources into your R projects.

## Installation

This package can be installed directly from github using the `{remotes}` package.

```r
remotes::install_github("statistikat/STATcubeR")
```

## Open Data

To import datasets from https://data.statistik.gv.at, simply pass the dataset
id to the `od_table()` function. For example, the OGD data from the [structure of earnings survey](https://data.statistik.gv.at/web/meta.jsp?dataset=OGD_veste309_Veste309_1)
can be accessed as follows.

```r
earnings <- od_table("OGD_veste309_Veste309_1")
earnings$tabulate()
```

```
# A STATcubeR tibble: 72 x 9
  Sex    Citizenship `Region (NUTS2)` `Form of employment`   `Arithmetic mea… `1st quartile`
* <fct>  <fct>       <fct>            <fct>                             <int>          <int>
1 Sum t… Total       Total            "Total"                              18             12
2 Sum t… Total       Total            "Standard employment "               19             13
3 Sum t… Total       Total            "Non-standard employm…               15             10
4 Sum t… Total       Total            "Non-standard employm…               16             11
# … with 68 more rows, and 3 more variables: 2nd quartile (median) <int>,
#   3rd quartile <int>, Number of employees <int>
```

The resulting object contains labeled data (see above), raw data, metadata and
more. See the [OGD Article](https://statistikat.github.io/STATcubeR/articles/od_table.html) for further details.

## STATcube API

In order to use the REST API, it is required to set up an API key. See the
[setup article](https://statistikat.github.io/STATcubeR/articles/articles/Setup.html)
for more details. The API is currently only available for employees of
Statistics Austria. Support for external users will be added in the near
future.

There are four main functions that interact with the API

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
