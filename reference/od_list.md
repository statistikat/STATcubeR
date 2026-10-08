# List available Opendata datasets

`od_list()` returns a `data.frame ` containing all datasets published at
[data.statistik.gv.at](https://data.statistik.gv.at)

## Usage

``` r
od_list(unique = TRUE, server = c("ext", "red"), lang = c("de", "en"))
```

## Arguments

- unique:

  some datasets are published under multiple groups. They will only be
  listed once with the first group they appear in unless this parameter
  is set to `FALSE`.

- server:

  the open data server to use. Either `ext` for the external server (the
  default) or `red` for the editing server. The editing server is only
  accessible for employees of Statistics Austria

- lang:

  either `"de"` or `"en"`

## Value

a `data.frame` with the following columns

- `"category"`: Grouping under which a dataset is listed

- `"id"`: Name of the dataset which can later be used in
  [`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)

- `"label"`: Description of the dataset

- `"date"`: the last update date of the dataset

- `"csv_link"`: the URL of the (bulk) csv file

- `"json_link"`: the URL of the json metadata file

## Examples

``` r
df <- od_list()
df
#> # A tibble: 471 × 6
#>    category   id                  label  date       csv_link json_link
#>    <chr>      <chr>               <chr>  <date>     <chr>    <chr>    
#>  1 Hochwerti… OGD_skesvg2010indi… Nicht… 2026-10-05 https:/… https://…
#>  2 Hochwerti… OGD_oeff_fin_Oeff_… Öffen… 2026-09-30 https:/… https://…
#>  3 Hochwerti… OGD_kons_brv_q_HVD… Konso… 2026-09-30 https:/… https://…
#>  4 Hochwerti… OGD_kons_brv_HVD_K… Konso… 2026-09-30 https:/… https://…
#>  5 Hochwerti… OGD_vgr108_VGR_HA_… Haupt… 2026-09-30 https:/… https://…
#>  6 Hochwerti… OGD_vgr109_VGR_Erw… Haupt… 2026-09-30 https:/… https://…
#>  7 Hochwerti… OGD_vgr107_VGR_HA_… Haupt… 2026-09-30 https:/… https://…
#>  8 Hochwerti… OGD_vgr105_VGR_HA_… Haupt… 2026-09-30 https:/… https://…
#>  9 Hochwerti… OGD_vgr101_VGRJahr… VGR-J… 2026-09-30 https:/… https://…
#> 10 Hochwerti… OGD_hvpi25_HVD_HVP… Harmo… 2026-09-17 https:/… https://…
#> # ℹ 461 more rows
subset(df, category == "Bildung und Forschung")
#> # A tibble: 49 × 6
#>    category   id                  label  date       csv_link json_link
#>    <chr>      <chr>               <chr>  <date>     <chr>    <chr>    
#>  1 Bildung u… OGD_unistud5_ext_U… Studi… 2026-09-24 https:/… https://…
#>  2 Bildung u… OGD_ordstud_ext_OR… Beleg… 2026-08-19 https:/… https://…
#>  3 Bildung u… OGD_ordabs_ext_ORD… Orden… 2026-08-19 https:/… https://…
#>  4 Bildung u… OGD_phsabs_ext_PHS… Studi… 2026-07-27 https:/… https://…
#>  5 Bildung u… OGD_phsstud_ext_PH… Studi… 2026-07-27 https:/… https://…
#>  6 Bildung u… OGD_innov015_CIS_0… Unter… 2026-07-08 https:/… https://…
#>  7 Bildung u… OGD_innov014_CIS_0… Umsät… 2026-07-08 https:/… https://…
#>  8 Bildung u… OGD_innov013_CIS_0… Innov… 2026-07-08 https:/… https://…
#>  9 Bildung u… OGD_innov012_CIS_0… Unter… 2026-07-08 https:/… https://…
#> 10 Bildung u… OGD_innov011_CIS_0… Unter… 2026-07-08 https:/… https://…
#> # ℹ 39 more rows
# use an id to load a dataset
od_table("OGD_fhsstud_ext_FHS_S_1")
#> Studies at universities of applied sciences
#> 
#> Dataset: OGD_fhsstud_ext_FHS_S_1 (data.statistik.gv.at)
#> Measures: Ordinary Studies, Courses of studies (Lehrgang), Newly
#>   enrolled ordinary studies, Newly enrolled courses of studies
#>   (Lehrgang)
#> Fields: Semester <43>
#> 
#> Request: [2026-10-08 14:57:07.870193]
#> STATcubeR: 1.0.1
```
