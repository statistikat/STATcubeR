# File Management

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

This article explains how and where
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) caches
resources from [data.statistik.gv.at](https://data.statistik.gv.at) in
the local file system. Understanding this behavior will allow you to
enable persistent caches and directly use the cached resources.

## Overview

By default,
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) caches all
accessed resources from
[data.statistik.gv.at](https://data.statistik.gv.at) in the temporary
directory of the current R session.

``` r

od_cache_dir()
```

    #> [1] "/tmp/RtmprF7rPE/STATcubeR/open_data/"

Let’s examine for example what happens when the data from the structure
of earnings survey (SES) is requested.

``` r

earnings <- od_table("OGD_veste309_Veste309_1")
```

First `STATcubeR` will grab a json with metadata about this dataset from
<https://data.statistik.gv.at/ogd/json?dataset=OGD_veste309_Veste309_1>
and check which resources belong to it. For any resource, the attributes
`name` and `last_modified` are extracted from the json. They are also
included in the `od_table` object under `$resources`.

``` r

earnings$resources
```

``` r-output
# A data frame: 7 × 6
  name           last_modified       cached               size download parsed
  <chr>          <dttm>              <dttm>              <dbl>    <dbl>  <dbl>
1 meta.json      2022-03-24 11:29:48 2026-10-08 14:58:52  4028     105. NA    
2 data.csv       2022-03-24 11:29:48 2026-10-08 14:58:52  4931     104.  0.680
3 HEADER.csv     2022-03-24 11:29:48 2026-10-08 14:58:52   516     103.  0.372
4 C-A11-0.csv    2022-03-24 11:29:48 2026-10-08 14:58:52   159     103.  0.361
5 C-STAATS-0.csv 2022-03-24 11:29:48 2026-10-08 14:58:52   697     104.  0.379
6 C-VEBDL-0.csv  2022-03-24 11:29:48 2026-10-08 14:58:53   518     104.  0.367
7 C-BESCHV-0.csv 2022-03-24 11:29:48 2026-10-08 14:58:53   641     104.  0.385
```

`last_modified` tells us when the resource was changed on the
fileserver. If a resource does not exist in the cache or if the last
modified entry in the json is newer than the cached file, it will be
downloaded from the server. Otherwise, the cached version is reused.

## Access and Updates

Cached files can be accessed with
[`od_cache_file()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md).
If the specified file exists in the cache, a path to the file will be
returned. Otherwise, the file is downloaded to the cache and then the
path is returned. The files use the same naming conventions as the open
data fileserver.

``` r

od_cache_file("OGD_veste309_Veste309_1")
```

    #> [1] "/tmp/RtmprF7rPE/STATcubeR/open_data/OGD_veste309_Veste309_1.csv"

``` r

od_cache_file("OGD_veste309_Veste309_1", "C-A11-0")
```

    #> [1] "/tmp/RtmprF7rPE/STATcubeR/open_data/OGD_veste309_Veste309_1_C-A11-0.csv"

To read files from the cache as `data.frame`s, use
[`od_resource()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
with same parameters as in
[`od_cache_file()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md).
This will apply a special parser to the dataset which drops unneeded
columns and normalizes column names.

``` r

od_resource("OGD_veste309_Veste309_1", "C-A11-0")
```

``` r-output
# A data frame: 3 × 7
  code  label label_de  label_en  parent de_desc en_desc
* <chr> <chr> <chr>     <chr>     <fct>  <lgl>   <lgl>  
1 A11-1 NA    insgesamt Sum total NA     NA      NA     
2 A11-2 NA    männlich  Male      NA     NA      NA     
3 A11-3 NA    weiblich  Female    NA     NA      NA     
```

The parser behaves differently for header files, data files and fields.
Json files can be accessed with
[`od_json()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md).

``` r

json <- od_json("OGD_veste309_Veste309_1")
unlist(json$tags)
```

    #> [1] "Staatsangehörigkeit"      "Bundesland"              
    #> [3] "Beschäftigungsverhältnis"

## Clearing and Changing

`od_cache_clear(id)` can be used to clear the cache from all files
belonging to the passed dataset id. We saw that `earnings$resources`
contains 7 rows, therefore 7 files will be deleted during cleanup.

``` r

od_cache_clear("OGD_veste309_Veste309_1")
```

    #> deleted 7 files from '/tmp/RtmprF7rPE/STATcubeR/open_data/'

If you want to use a persistent directory like
`~/.cache/STATcubeR/open_data/` for caching, the directory can be
changed with `od_cache_dir(new)`.

``` r

od_cache_dir("~/.cache/STATcubeR/open_data/")
```

## The resources field

Let’s go back to the `$resources` field of `earnings`.

``` r

earnings$resources
```

``` r-output
# A data frame: 7 × 6
  name           last_modified       cached               size download parsed
  <chr>          <dttm>              <dttm>              <dbl>    <dbl>  <dbl>
1 meta.json      2022-03-24 11:29:48 2026-10-08 14:58:52  4028     105. NA    
2 data.csv       2022-03-24 11:29:48 2026-10-08 14:58:52  4931     104.  0.680
3 HEADER.csv     2022-03-24 11:29:48 2026-10-08 14:58:52   516     103.  0.372
4 C-A11-0.csv    2022-03-24 11:29:48 2026-10-08 14:58:52   159     103.  0.361
5 C-STAATS-0.csv 2022-03-24 11:29:48 2026-10-08 14:58:52   697     104.  0.379
6 C-VEBDL-0.csv  2022-03-24 11:29:48 2026-10-08 14:58:53   518     104.  0.367
7 C-BESCHV-0.csv 2022-03-24 11:29:48 2026-10-08 14:58:53   641     104.  0.385
```

We already looked at **`name`** and **`last_modified`**. The remaining
columns can be interpreted as follows

- **`cached`** tells us the last time the cache file for the resource
  was modified.
- **`size`** is the file size in bytes
- **`download`** contains the amount of milliseconds used to retrieve
  the resource when it was last updated.
- **`parsed`** reports the amount of milliseconds it took
  [`od_resource()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
  to convert the file contents into a
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) format. For
  the json file, the parsing time is always reported as `NA`.

## What’s in the cache?

[`od_cache_summary()`](https://statistikat.github.io/STATcubeR/reference/od_cache.md)
will give an overview about all files that are available in the cache
directory. The returned table contains one row for every dataset.

- The column **`updated`** contains the last modified date for the
  datasets json file.
- **`json`**, **`data`** and **`header`** give the file sizes in bytes
  for the corresponding files.
- **`fields`** is the total size of all fields and **`n_fields`** is the
  number of classification files available.

We can get a clear picture on how much disk space is used for each
dataset.

``` r

od_cache_summary()
```

    #> NULL

Note that
[`od_cache_summary()`](https://statistikat.github.io/STATcubeR/reference/od_cache.md)
only gathers information from the local file system based on filenames,
[`file.mtime()`](https://rdrr.io/r/base/file.info.html) and
[`file.size()`](https://rdrr.io/r/base/file.info.html).

## Download history

To get a history of all files that have been downloaded from the server,
use
[`od_downloads()`](https://statistikat.github.io/STATcubeR/reference/od_cache.md).
For each file, a timestamp for the download is recorded as well as the
download time in milliseconds.

``` r

od_downloads()
```

``` r-output
# A data frame: 7 × 3
  time                file                                   downloaded
  <dttm>              <chr>                                       <dbl>
1 2026-10-08 14:58:52 OGD_veste309_Veste309_1.json                 105.
2 2026-10-08 14:58:52 OGD_veste309_Veste309_1.csv                  104.
3 2026-10-08 14:58:52 OGD_veste309_Veste309_1_HEADER.csv           103.
4 2026-10-08 14:58:52 OGD_veste309_Veste309_1_C-A11-0.csv          103.
5 2026-10-08 14:58:52 OGD_veste309_Veste309_1_C-STAATS-0.csv       104.
6 2026-10-08 14:58:53 OGD_veste309_Veste309_1_C-VEBDL-0.csv        104.
7 2026-10-08 14:58:53 OGD_veste309_Veste309_1_C-BESCHV-0.csv       104.
```
