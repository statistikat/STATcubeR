# Cache management for Open Data

Functions to inspect the contents of the current cache.

## Usage

``` r
od_cache_summary(server = "ext")

od_downloads(server = "ext")
```

## Arguments

- server:

  the OGD-Server to use. `"ext"` for the external server (the default)
  or `"red"` for the editing server

## Value

- `od_cache_summary()` provides an overview of all contents of the cache
  through a data.frame. It has one row for each dataset and returns a
  `data.frame` with# the following columns in which all file sizes are
  given in bytes.

  - **`id`** the dataset id

  - **`updated`** the last modified time for `${id}.json`

  - **`json`** the file size of `${id}.json`

  - **`data`** the file size of `${id}.csv`

  - **`header`** the file size of `${id}_HEADER.csv`

  - **`fields`** the total file size of all files belonging to fields
    (`{id}_C*.csv`).

  - **`n_fields`** the number of field files

- `od_downloads()` shows a download history for the current cache and
  returns a `data.frame` with the following columns:

  - **`time`** a timestamp for the download

  - **`file`** the filename

  - **`downloaded`** the download time in milliseconds

## Examples

``` r
## make sure the cache is not empty
od_table("OGD_krebs_ext_KREBS_1")
#> Cancer statistics by reporting year, province of residence and
#> localisation of cancer
#> 
#> Dataset: OGD_krebs_ext_KREBS_1 (data.statistik.gv.at)
#> Measures: Number of records F-KRE
#> Fields: Tumore ICD/10 3-Steller <98>, Reporting year <42>, Province
#>   of residence <9>, Sex <2>
#> 
#> Request: [2026-10-09 04:52:19.205147]
#> STATcubeR: 1.0.1
od_table("OGD_veste309_Veste309_1")
#> Structure of Earnings Survey (SES) 2018 Gross hourly earnings
#> in EUR by citizenship, region (NUTS 2) and form of employment
#> 
#> Dataset: OGD_veste309_Veste309_1 (data.statistik.gv.at)
#> Measures: Arithmetic mean, 1st quartile, 2nd quartile (median), 3rd
#>   quartile, Number of employees
#> Fields: Sex <3>, Citizenship <9>, Region (NUTS2) <10>, Form of
#>   employment <7>
#> 
#> Request: [2026-10-09 04:52:23.136973]
#> STATcubeR: 1.0.1

## inspect
od_cache_summary()
#> # A data frame: 2 × 7
#>   id                    updated   json    data header fields n_fields
#>   <chr>                 <dttm>   <dbl>   <dbl>  <dbl>  <dbl>    <int>
#> 1 OGD_krebs_ext_KREBS_1 04:52:19  3731 2978108    287  10003        4
#> 2 OGD_veste309_Veste3…  04:52:23  4028    4931    516   2015        4
od_downloads()
#> # A data frame: 14 × 3
#>    time                file                                 downloaded
#>    <dttm>              <chr>                                     <dbl>
#>  1 2026-10-09 04:52:19 OGD_krebs_ext_KREBS_1.json                 152.
#>  2 2026-10-09 04:52:22 OGD_krebs_ext_KREBS_1.csv                 2863.
#>  3 2026-10-09 04:52:22 OGD_krebs_ext_KREBS_1_HEADER.csv           152.
#>  4 2026-10-09 04:52:22 OGD_krebs_ext_KREBS_1_C-TUM_ICD10_3…       151.
#>  5 2026-10-09 04:52:22 OGD_krebs_ext_KREBS_1_C-BERJ-0.csv         150.
#>  6 2026-10-09 04:52:22 OGD_krebs_ext_KREBS_1_C-BUNDESLAND-…       151.
#>  7 2026-10-09 04:52:23 OGD_krebs_ext_KREBS_1_C-KRE_GESCHLE…       151.
#>  8 2026-10-09 04:52:23 OGD_veste309_Veste309_1.json               152.
#>  9 2026-10-09 04:52:23 OGD_veste309_Veste309_1.csv                151.
#> 10 2026-10-09 04:52:23 OGD_veste309_Veste309_1_HEADER.csv         151.
#> 11 2026-10-09 04:52:23 OGD_veste309_Veste309_1_C-A11-0.csv        150.
#> 12 2026-10-09 04:52:23 OGD_veste309_Veste309_1_C-STAATS-0.…       150.
#> 13 2026-10-09 04:52:24 OGD_veste309_Veste309_1_C-VEBDL-0.c…       151.
#> 14 2026-10-09 04:52:24 OGD_veste309_Veste309_1_C-BESCHV-0.…       151.
```
