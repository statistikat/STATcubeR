# Resource management for open.data

Helper functions for caching and parsing open.data resources.

## Usage

``` r
od_cache_dir(dir = NULL)

od_cache_clear(id, server = "ext")

od_cache_file(id, suffix = NULL, timestamp = NULL, ..., server = "ext")

od_resource(id, suffix = NULL, timestamp = NULL, server = "ext")

od_json(id, timestamp = Sys.time() - 3600, server = "ext")

od_resource_all(id, json = od_json(id), server = "ext")
```

## Arguments

- dir:

  If `NULL`, the cache directory is returned. Otherwise, the cache
  directory will be updated to `dir`.

- id:

  A database id

- server:

  the OGD-Server to use to load update the resources in case they are
  outdated. `"ext"` for the external server (the default) od `"red"` for
  the editing server.

- suffix:

  A suffix for the resource: `"HEADER"` or a field code.

- timestamp:

  A timestamp in `POSIXct` format. If provided, the cached resource will
  be updated if it is older than that value. Otherwise it will be
  downloaded only if it does not exist in the cache.

- ...:

  For internal use

- json:

  The JSON file belonging to the dataset

## Value

For `od_cache_file()` and `od_resource()`, the returned objects contain
a hidden attribute `attr(., "od")` about the time used for downloading
and parsing the resource. `od_resource_all()` converts these hidden
attribute into columns.

## Details

`od_cache_clear(id)` removes all files belonging to the specified id.

By default, downloaded json files will "expire" in one hour or 3600
seconds. That is, if a json is requested, it will be reused from the
cache unless the [`file.mtime()`](https://rdrr.io/r/base/file.info.html)
is more than one hour behind
[`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).

## Examples

``` r
# get the current cache directory
od_cache_dir()
#> [1] "/tmp/Rtmpze1zZ6/STATcubeR/open_data/"

# Get paths to cached files
od_cache_file("OGD_veste309_Veste309_1")
#> [1] "/tmp/Rtmpze1zZ6/STATcubeR/open_data/OGD_veste309_Veste309_1.csv"
od_cache_file("OGD_veste309_Veste309_1", "C-A11-0")
#> [1] "/tmp/Rtmpze1zZ6/STATcubeR/open_data/OGD_veste309_Veste309_1_C-A11-0.csv"

# get a parsed verison of the resource
od_resource("OGD_veste309_Veste309_1", "C-A11-0")
#> # A data frame: 3 × 7
#>   code  label label_de  label_en  parent de_desc en_desc
#> * <chr> <chr> <chr>     <chr>     <fct>  <lgl>   <lgl>  
#> 1 A11-1 NA    insgesamt Sum total NA     NA      NA     
#> 2 A11-2 NA    männlich  Male      NA     NA      NA     
#> 3 A11-3 NA    weiblich  Female    NA     NA      NA     

# get json metadata about a dataset
od_json('OGD_veste309_Veste309_1')
#> Verdienststrukturerhebung 2018 Bruttostundenverdienste in EUR
#> nach Staatsangehörigkeit, Bundesland und
#> Beschäftigungsverhältnis
#> 
#> Verdienststruktur nach Geschlecht, Staatsangehörigkeit,
#> Bundesland und Beschäftigungsverhältnis
#> 
#> Measures: Arithmetisches Mittel, 1. Quartil, 2. Quartil (Median), 3.
#>   Quartil, Zahl d unselbst Beschäftigten
#> Fields: Geschlecht, Staatsangehörigkeit, Bundesland (NUTS 2), Form
#>   des Beschäftigungsverhältnisses
#> Updated: 2022-03-24 11:29:48
#> Tags: Staatsangehörigkeit, Bundesland, Beschäftigungsverhältnis
#> Categories: Arbeit, Bevölkerung

# Bundle all resources
od_resource_all("OGD_veste309_Veste309_1")
#> # A data frame: 6 × 7
#>   name                 last_modified cached    size download parsed
#>   <chr>                <dttm>        <dttm>   <dbl>    <dbl>  <dbl>
#> 1 meta.json            2022-03-24    04:52:23  4931       NA  0.690
#> 2 data.csv             2022-03-24    04:52:23   516       NA  0.371
#> 3 OGD_veste309_Veste3… 2022-03-24    04:52:23   159       NA  0.372
#> 4 OGD_veste309_Veste3… 2022-03-24    04:52:23   697       NA  0.367
#> 5 OGD_veste309_Veste3… 2022-03-24    04:52:24   518       NA  0.351
#> 6 OGD_veste309_Veste3… 2022-03-24    04:52:24   641       NA  0.350
#> # ℹ 1 more variable: data <I<list>>
```
