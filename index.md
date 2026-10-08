# STATcubeR

R client for all things [STATcube](https://statcube.at). Get data from
the STATcube REST API or via the open government data portal at
<https://data.statistik.gv.at>.
[STATcubeR](https://statistikat.github.io/STATcubeR/) makes it easy to
include both those datasources into your R projects.

## Installation

Current versions of STATcubeR can be directly downloaded from CRAN using

``` r

install.packages("STATcubeR")
```

or directly from [github](https://github.com/statistikat/STATcubeR)
using the [remotes](https://remotes.r-lib.org) package.

``` r

remotes::install_github("statistikat/STATcubeR")
```

Alternative: Install from `tar.gz` Archives

If you are not able to use `remotes::nstall_github()` to install
`STATcubeR`, you can also download the package as an archive from
<https://github.com/statistikat/STATcubeR/tags>. The package can then be
installed by providing a path to the downloaded archive file, for
example:

``` r

install.packages('STATcubeR-x.y.z.tar.gz', repos = NULL)
```

## Open Data

To import datasets from <https://data.statistik.gv.at>, pass the dataset
id to the
[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
function. For example, OGD data about the [Austrian population in
2020](https://data.statistik.gv.at/web/meta.jsp?dataset=OGD_bevstandjbab2002_BevStand_2020)
can be accessed as follows.

``` r

library(STATcubeR)
population <- od_table("OGD_bevstandjbab2002_BevStand_2020")
population$tabulate()
```

    # A STATcubeR tibble: 392,508 x 5
      `Time section` Sex   `Commune (aggregation by polit… `Age in single ye… Number
    * <date>         <fct> <fct>                           <fct>               <int>
    1 2020-01-01     male  Eisenstadt <10101>              under 1 year old       77
    2 2020-01-01     male  Eisenstadt <10101>              1 year old             75
    3 2020-01-01     male  Eisenstadt <10101>              2 years old            70
    4 2020-01-01     male  Eisenstadt <10101>              3 years old            83
    # … with 392,504 more rows

The resulting object contains labeled data (see above), raw data,
metadata and more. See the [OGD
article](https://statistikat.github.io/STATcubeR/articles/od_table.html)
for further details. The [available datasets
article](https://statistikat.github.io/STATcubeR/articles/od_list.html)
provides an overview of the 315 datasets that are compatible with
[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md).

## STATcube API

In order to use the REST API, it is required to set up an API key. As
mentioned in the [API key
article](https://statistikat.github.io/STATcubeR/articles/sc_key.html),
this requires a STATcube subscription.

There are four main functions that interact with the API

- [`sc_schema_catalogue()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  lists all available datasets and tables
- [`sc_schema_db()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  provides metadata about a specific database
- [`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  requests a table from the API according to a json standard
- [`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  requests a table based on an id

More information about the first two functions can be found in the
[schema
article](https://statistikat.github.io/STATcubeR/articles/sc_schema.html).
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
and
[`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
have their own articles
[here](https://statistikat.github.io/STATcubeR/articles/sc_table.html)
and
[here](https://statistikat.github.io/STATcubeR/articles/sc_table_saved.html).

## Consistent data formats

Both OGD data and tables form the REST API are wrapped into an
[R6](https://r6.r-lib.org) class to provide easy access to data and
metadata. For example, the `$tabulate()` method is also available for
tables from the REST API.

``` r

# https://statcube.at/statcube/openinfopage?id=debevstandjbab2002
population <- sc_table_saved("defaulttable_debevstandjbab2002")
population$tabulate()
```

    # A STATcubeR tibble: 10 x 3
      `Time section` `Commune (aggregation by political district)`  Number
      <date>         <fct>                                           <dbl>
    1 2021-01-01     Burgenland <AT11>                              296010
    2 2021-01-01     Carinthia <AT21>                               562089
    3 2021-01-01     Lower Austria <AT12>                          1690879
    4 2021-01-01     Upper Austria <AT31>                          1495608
    # … with 6 more rows

See the [base class
article](https://statistikat.github.io/STATcubeR/articles/sc_data.html)
for more information about the features of this class.
