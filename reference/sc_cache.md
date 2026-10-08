# Cache responses from the STATcube REST API

Functions to cache requested resources in the directory
`~/.STATcubeR_cache` and reuse them in calls to
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
[`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
[`sc_schema()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
and so forth. These functions are designed for testing and documentation
and should not be regarded as part of the STATcubeR interface. The
caching logic is likely to change in the future in which case
`sc_cache_clear()` is required to purge old cache entries.

## Usage

``` r
sc_cache_enable(verbose = TRUE)

sc_cache_disable()

sc_cache_enabled()

sc_cache_dir(dir = NULL)

sc_cache_files(x)

sc_cache_clear()
```

## Arguments

- verbose:

  print instructions on how to set up caching persistently via
  environment variables?

- dir:

  a cache directory

- x:

  an object of class `sc_table` or `sc_schema`

## Value

- for `sc_cache_enable()`, `sc_cache_dir()`: the path to the
  cache-directory

- for `sc_cache_disable()`: `TRUE`

- for `sc_cache_enabled()`: `TRUE` if caching is enabled, `FALSE`
  otherwise

- for `sc_cache_files()`: the content of the cache associated with a
  file

- for `sc_cache_clear()`: `NULL`

## Details

Caching can be set up using environment variables. To set up a
persistent cache for both Open Data and the REST API, the following
lines in `.Renviron` can be used. The paths in this example are only
applicable for UNIX-based operating systems.

    STATCUBE_KEY_EXT   = YOUR_API_KEY_GOES_HERE
    STATCUBE_CACHE     = TRUE
    OD_CACHE_DIR       = "~/.cache/STATcubeR/open_data/"
    STATCUBE_CACHE_DIR = "~/.cache/STATcubeR/api/"

If caching is enabled, there is no check to verify if the resources are
unchanged in the server. Caching is not implemented for the endpoints
[`sc_info()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md)
and
[`sc_rate_limit_table()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md).

## Functions

- `sc_cache_enable()`: enables caching for the current R session

- `sc_cache_disable()`: disables caching for the current R session
  sc_cache_disable()

- `sc_cache_enabled()`: informs whether the cache is currently enabled

- `sc_cache_dir()`: get/set the directory used for caching

- `sc_cache_files()`: get the cache file associated with an object

- `sc_cache_clear()`: removes all files from the cache
