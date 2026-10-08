# Caching API Responses

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) allows you
to cache responses from the STATcube REST API. This means, for example,
that responses for the /table endpoint can be reused.

## Setup

Caching is disabled by default. In order to activate or deactivate
caching, use the functions
[`sc_cache_enable()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
and
[`sc_cache_disable()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)

``` r

sc_cache_enable()
```

    #> Caching will be available for this session. Add
    #> 
    #>   STATCUBE_CACHE     = TRUE
    #>   STATCUBE_CACHE_DIR = "~/.STATcubeR_cache"
    #> 
    #> to your .Renviron to enable caching persistently.

The caching directory can be displayed or changed using
[`sc_cache_dir()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)

``` r

sc_cache_dir()
```

    #> [1] "~/.STATcubeR_cache"

``` r

sc_cache_dir("~/.cache/STATcubeR/api")
sc_cache_dir()
```

    #> [1] "~/.cache/STATcubeR/api"

## Using the cache

Caching will affect all calls to
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
and
[`sc_schema()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
as well as their “derived” functions:
[`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
[`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md),
[`sc_schema_db()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md),
[`sc_schema_catalogue()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md).

If the same resource is requested several times, the last valid API
response is reused. Invalid responses (such as 404 responses) will not
be added to the cache.

Cache files always contain unparsed API responses as returned by
[`httr::GET()`](https://httr.r-lib.org/reference/GET.html) or
[`httr::POST()`](https://httr.r-lib.org/reference/POST.html). Responses
are stored in an `rds` format. If caching is enabled, the corresponding
cache files to an object of class `sc_schema` or `sc_table` can be
retrieved using
[`sc_cache_files()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md).

``` r

sc_example("accomodation") %>% sc_table(language = "both") %>% sc_cache_files()
```

    #> [1] "~/.cache/STATcubeR/api/vkuxP3Pp-fTs9vvI20k1JVTU+lE=.rds"
    #> [2] "~/.cache/STATcubeR/api/2UB+IkVqb-amUVJWFI6yRGcZyfA=.rds"

``` r

sc_schema_catalogue() %>% sc_cache_files()
```

    #> [1] "~/.cache/STATcubeR/api/a1nJzuFIzQxUJT5mZcPhhjiGs9I=.rds"

Note that the first call to
[`sc_cache_files()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
returned two paths. Since the table was requested in two languages, two
API responses are necessary to construct the table object.

The content of the cache files can be parsed using
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) and
[`httr::content()`](https://httr.r-lib.org/reference/content.html). This
gives direct access to the API response in a
[`list()`](https://rdrr.io/r/base/list.html) format. For example, the
following syntax can be used to extract the database info from the
response.

``` r

sc_example("accomodation") %>% sc_table() %>% sc_cache_files() %>%
  readRDS() %>% httr::content() %>% .[["database"]] %>% str()
```

    #> List of 4
    #>  $ id            : chr "detouextregiosai"
    #>  $ uri           : chr "str:database:detouextregiosai"
    #>  $ label         : chr "Accomodation statistics as of 2000 according to tourism regions and seasons"
    #>  $ annotationKeys:List of 1
    #>   ..$ : chr "Q"

## Cleaning the cache

Cache files can be deleted individually using the paths returned by
[`sc_cache_files()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md).
Alternatively, use
[`sc_cache_clear()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
to delete all files from the cache.

``` r

sc_cache_clear()
#> deleted 12 entries from the cache
```

## Should I use caching?

If you are using
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md)
interactively, the answer is probably no. However, when building
applications that rely on STATcube data caching can be a useful way to
decrease the traffic with the STATcube server. Another use case for
caching is if you are writing
[rmarkdown](https://github.com/rstudio/rmarkdown) documents that rely on
STATcube data. Caching makes those documents both reproducible and
quicker to render.

Please note that there is currently no reliable way to invalidate the
cache. Therefore, API responses will be reused even if resources get
updated on the server.
