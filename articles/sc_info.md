# Other REST API Endpoints

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

Apart form
[`/table`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/table-endpoint)
and
[`/schema`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/schema-endpoint),
there is also support for the simple endpoints
[`/info`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/info-endpoint)
and
[`/rate_limit`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/rate-limit).

## Server Information

The
[`/info`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/info-endpoint)
endpoint gives and overview about the available languages on the server.

``` r

sc_info()
```

``` r-output
# A data frame: 2 × 2
  locale displayName
  <chr>  <chr>      
1 de     Deutsch    
2 en     English    
```

## Rate Limits (Table)

The
[`/rate_limit`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/rate-limit)
endpoint shows the number of calls to the
[`/table`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/table-endpoint)
endpoint that are remaining.

``` r

sc_rate_limit_table()
```

    #> 95 / 100 (Resets at [15:57:10])

In this case, we see that 8 out of the 100 requests per hour have been
used up and 92 are still available. The rate limit will be reset once
per hour. In this case this will be at `2022-08-30 13:53:55`. The entry
under reset should always be less than one hour after the request to the
[`/rate_limit`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/rate-limit)
endpoint was sent.

## Rate Limits (Schema)

Schema requests are currently limited to 10000 requests per hour. The
number of remaining requests can be obtained via
[`sc_rate_limit_schema()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md).
Rate limits are be returned in the same format as in
[`sc_rate_limit_table()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md).

``` r

sc_rate_limit_schema()
```

    #> 9999 / 10000 (Resets at [15:57:10])

## Rate Limits from headers

All responses from the STATcube API contain rate limit information
(including remaining requests) in the response headers[^1]. So instead
of using the `/rate_limit*` endpoints shown above, it is also possible
to use responses from other endpoints and extract rate limit information
from their headers.

The function
[`sc_rate_limits()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md)
does just that. Any return value from
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
[`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
and
[`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
can be passed to
[`sc_rate_limits()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md)
and the rate limits will be extracted from the response headers.

``` r

sc_example("population_timeseries.json") %>%
  sc_table() %>%
  sc_rate_limits()
```

    #> $schema
    #> 9999 / 10000 (Resets at [15:57:10])
    #> 
    #> 
    #> $table
    #> 95 / 100 (Resets at [15:57:10])

Note that the function gives rate limits for `/schama` and `/table` even
tough only the `/table` endpoint was used.

The function also works with return values from
[`sc_schema()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
and friends.

``` r

sc_schema_catalogue() %>%
  sc_rate_limits()
```

    #> $schema
    #> 9999 / 10000 (Resets at [15:57:10])
    #> 
    #> 
    #> $table
    #> 95 / 100 (Resets at [15:57:10])

## Server-Side Caching

~~STATcube uses caching for the
[`/table`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/table-endpoint)
endpoint by default. If the same request to
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
is sent several times, this will not count towards the rate-limit (100
requests per hour).~~

Server-Side caching of
[`/table`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/table-endpoint)
responses is currently disabled due to security reasons. Therefore, all
requests against the
[`/table`](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/table-endpoint)
endpoint count towards the rate-limit.

[^1]: Responses for unauthorized requests (response status 401) are an
    exception
