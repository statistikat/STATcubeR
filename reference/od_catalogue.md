# Get a catalogue for OGD datasets

**EXPERIMENTAL** This function parses several json metadata files at
once and combines them into a `data.frame` so the datasets can easily be
filtered based on categorizations, tags, number of classifications, etc.

## Usage

``` r
od_catalogue(server = "ext", local = TRUE)
```

## Arguments

- server:

  the OGD-server to be used. `"ext"` (the default) for the external
  server or `prod` for the production server

- local:

  If `TRUE` (the default), the catalogue is created based on cached json
  metadata. Otherwise, the cache is updated prior to creating the
  catalogue using a "bulk-download" for metadata files.

## Value

a `data.frame` with the following structure

|            |                 |                                              |
|------------|-----------------|----------------------------------------------|
| **Column** | **Type**        | **Description**                              |
| title      | `chr`           | Title of the dataset                         |
| measures   | `int`           | Number of measure variables                  |
| fields     | `int`           | Number of classification fields              |
| modified   | `datetime`      | Timestamp when the dataset was last modified |
| created    | `datetime`      | Timestamp when the dataset was created       |
| database   | `chr`           | ID of the corresponding STATcube database    |
| title_en   | `chr`           | English title                                |
| notes      | `chr`           | Description for the dataset                  |
| frequency  | `chr`           | How often is the dataset updated?            |
| category   | `chr`           | Category of the dataset                      |
| tags       | `list<chr>`     | tags assigned to the dataset                 |
| json       | `list<od_json>` | Full json metadata                           |

The type `datetime` refers to the `POSIXct` format as returned by
[`Sys.time()`](https://rdrr.io/r/base/Sys.time.html). The last column
`"json"` contains the full json metadata as returned by
[`od_json()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md).

## Details

The naming, ordering and choice of the columns is likely to change.

## Examples

``` r
catalogue <- od_catalogue()
catalogue
#> # A data frame: 2 × 13
#>   title        measures fields modified            created            
#>   <chr>           <int>  <int> <dttm>              <dttm>             
#> 1 Krebsstatis…        1      4 2026-01-29 09:43:15 2019-08-08 11:09:49
#> 2 Verdienstst…        5      4 2022-03-24 11:29:48 2017-08-02 20:00:00
#> # ℹ 8 more variables: id <ogd_id>, database <chr>, title_en <chr>,
#> #   notes <chr>, update_frequency <chr>, tags <I<list>>,
#> #   categorization <chr>, json <I<list>>
table(catalogue$update_frequency)
#> 
#>      jährlich nicht geplant 
#>             1             1 
table(catalogue$categorization)
#> 
#>     Arbeit Gesundheit 
#>          1          1 
catalogue[catalogue$categorization == "Gesundheit", 1:4]
#> # A data frame: 1 × 4
#>   title          measures fields modified           
#> * <chr>             <int>  <int> <dttm>             
#> 1 Krebsstatistik        1      4 2026-01-29 09:43:15
catalogue[catalogue$measures >= 70, 1:3]
#> # A data frame: 0 × 3
#> # ℹ 3 variables: title <chr>, measures <int>, fields <int>
catalogue$json[[1]]
#> Krebsstatistik
#> 
#> Krebsstatistik nach Krebslokalisation (ICD10), Geschlecht und
#> Wohnbundesland
#> 
#> Measures: Anzahl der Datensätze F-KRE
#> Fields: Tumore ICD/10 3-Steller, Berichtsjahr, Bundesland, Geschlecht
#> Updated: 2026-01-29 09:43:15
#> Tags: Krebsstatistik, Krebslokalisation-ICD10, Geschlecht,
#>   Wohnbundesland
#> Categories: Gesundheit
head(catalogue$database)
#> [1] "dekrebs_ext" "deveste309" 
```
