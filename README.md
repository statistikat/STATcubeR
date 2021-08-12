
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

To import datasets from https://data.statistik.gv.at, pass the dataset
id to the `od_table()` function. For example, OGD about the [austrian population in 2020](https://data.statistik.gv.at/web/meta.jsp?dataset=OGD_bevstandjbab2002_BevStand_2020)
can be accessed as follows.

```r
population <- od_table("OGD_bevstandjbab2002_BevStand_2020")
population$tabulate()
```

```
# A STATcubeR tibble: 392,508 x 5
  `Time section` Sex   `Commune (aggregation by political distri… `Age in single year… Number
* <date>         <fct> <fct>                                      <fct>                 <int>
1 2020-01-01     male  Eisenstadt <10101>                         under 1 year old         77
2 2020-01-01     male  Eisenstadt <10101>                         1 year old               75
3 2020-01-01     male  Eisenstadt <10101>                         2 years old              70
4 2020-01-01     male  Eisenstadt <10101>                         3 years old              83
# … with 392,504 more rows
```

The resulting object contains labeled data (see above), raw data, metadata and
more. See the [OGD Article](https://statistikat.github.io/STATcubeR/articles/od_table.html) for further details.

## STATcube API

In order to use the REST API, it is required to set up an API key. See the
[api key article](https://statistikat.github.io/STATcubeR/articles/sc_key.html)
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
article](https://statistikat.github.io/STATcubeR/articles/sc_schema.html).
`sc_table()` and `sc_table_saved()` have their own articles
[here](https://statistikat.github.io/STATcubeR/articles/sc_table.html)
and
[here](https://statistikat.github.io/STATcubeR/articles/sc_table_saved.html).

## Consistent data formats

Both OGD data and data form the `/table` endpoint are wrapped into an `{R6}`
class to provide easy acces to data and metadata. For example, the
`$tabulate()` method is also available for tables from the REST API.

```r
# https://statcube.at/statcube/openinfopage?id=debevstandjbab2002
population <- sc_table_saved("str:table:defaulttable_debevstandjbab2002")
population$tabulate()
```

```
# A STATcubeR tibble: 10 x 3
  `Time section` `Commune (aggregation by political district)`  Number
  <date>         <fct>                                           <dbl>
1 2021-01-01     Burgenland <AT11>                              296010
2 2021-01-01     Carinthia <AT21>                               562089
3 2021-01-01     Lower Austria <AT12>                          1690879
4 2021-01-01     Upper Austria <AT31>                          1495608
# … with 6 more rows
```

See the [base class article](https://statistikat.github.io/STATcubeR/articles/sc_data.html)
for more information about the features of this class.
