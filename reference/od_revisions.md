# Get OGD revisions

Use the `/revision` endpoint of the OGD server to get a list of all
datasets that have changed since a certain timestamp.

## Usage

``` r
od_revisions(since = NULL, exclude_ext = TRUE, server = "ext")
```

## Arguments

- since:

  (optional) A timestamp. If supplied, only datasets updated later will
  be returned. Otherwise, all datasets are returned. Can be in either
  one of the following formats

  - a native R time type that is compatible with
    [`strftime()`](https://rdrr.io/r/base/strptime.html) such as the
    return values of
    [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html),
    [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) and
    [`file.mtime()`](https://rdrr.io/r/base/file.info.html).

  - a string of the form `YYYY-MM-DD` to specify a day.

  - a string of the form `YYYY-MM-DDThh:mm:ss` to specify a day and a
    time.

- exclude_ext:

  If `TRUE` (default) exclude all results that have `OGDEXT_` as a
  prefix

- server:

  the open data server to use. Either `ext` for the external server (the
  default) or `red` for the editing server. The editing server is only
  accessible for employees of Statistics Austria

## Value

a character vector with dataset ids

## Examples

``` r
# get all datasets (including OGDEXT_*)
ids <- od_revisions(exclude_ext = FALSE)
ids
#> 542 datasets are available ([2026-10-08 14:57:09])
#> ids: OGDEXT_AEST_GEMTAB_1, OGDEXT_AMB_1, OGDEXT_BINNENWAND_1, …, OGD_zlf_komm_ZLF_KOM_1, and OGD_zlf_komm_ZLF_KOM_2
sample(ids, 6)
#> [1] "OGD_bevstprogjdgebland_PR_BEVJDGB_7"
#> [2] "OGD_phsstud_ext_PHS_S_1"            
#> [3] "OGD_unistud2_ext_UNI_STUD2_1"       
#> [4] "OGDEXT_POLBEZ_1"                    
#> [5] "OGD_vgr105_VGR_HA_Bws_2"            
#> [6] "OGD_tli16nace20_TLI_110"            

# get all the datasets since the fifteenth of august
od_revisions("2022-09-15")
#> 396 changes between
#>                 [2022-09-15] and
#>                 [2026-10-08 14:57:09]
#> ids: OGD_1531kn2_Aussenhandel_4, OGD_1905fue_FUE_B1905FUE_1, OGD__steuer_est_ab_2008_altgesch_EST_2_2, …, OGD_zlf_komm_ZLF_KOM_1, and OGD_zlf_komm_ZLF_KOM_2
```
