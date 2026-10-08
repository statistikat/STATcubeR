# Handling API Errors

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

This article explains how
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) deals with
situations where the communication with the STATcube REST API leads to
exceptions such as error codes.

## Throw all the Errors

All http errors codes from the REST API will be turned into R errors.
More precisely, the following conditions are checked to determine
whether a request to the STATcube API was successful

- The response returns `FALSE` for
  [`httr::http_error()`](https://httr.r-lib.org/reference/http_error.html)
  which means the response status is less than 400. See the [MDN
  reference](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status?retiredLocale=de)
  for more information about http status codes.
- The content type of the response is “application/json”.

If at least one of those two conditions is not met, an error is thrown.
This is different from the approach taken by other API clients, which
will retry requests after certain timeouts or return an “error object”.

## Get details about errors

If you encounter an error, you can get more details via
[`sc_last_error()`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md)
or
[`sc_last_error_parsed()`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md).
This will return the (parsed) response object that triggered the error.

``` r

sc_info(key = "wrong key")
#> Error: Client error: (401) Unauthorized ...
sc_last_error_parsed() %>% str()
```

[`sc_last_error()`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md)
returns the same response but in the format returned by
[`httr::GET()`](https://httr.r-lib.org/reference/GET.html). It is more
flexible than
[`sc_last_error_parsed()`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md)
but requires you to parse the object yourself. See
[`?httr::content`](https://httr.r-lib.org/reference/content.html) to and
[`?httr::headers`](https://httr.r-lib.org/reference/headers.html) to get
started.

## Common Errors

This section showcases the most common types of errors that you might
encounter when interacting with the API. Please feel free to open a new
issue on the [STATcubeR issue
tracker](https://github.com/statistikat/STATcubeR/issues) if you get an
error which is not listed here.

### Invalid API Key

If an invalid API key is used for a request, a 401 status is returned.

``` r

sc_info(key = "wrong key")
```

This example passes the API key as a parameter for simplicity. In
practice, you should set up your key according to the [API key
article](https://statistikat.github.io/STATcubeR/articles/sc_key.md).

### API Not Accessible

This occurs if
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) tries to
send requests to a server which is not accessible for the current
environment. This will result in a timeout error.

    Error in curl::curl_fetch_memory(url, handle = handle) : 
      Timeout was reached: [${server}] Resolving timed out after 10000 milliseconds

Outside of the Statistics Austria firewall, the only working base URL is
the following.

``` r

STATcubeR:::base_url()
```

    #> [1] "https://statcubeapi.statistik.at/statistik.at/ext/statcube/rest/v1"

Reasons this error might occur

1.  Your package Version of
    [STATcubeR](https://statistikat.github.io/STATcubeR/index.md) is
    outdated (version \< 0.4.0)
2.  Your company uses a firewall that blocks API requests from leaving.
3.  You manually set the `server` parameter in one of the API functions
    ([`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
    [`sc_schema()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md),
    …) to something other than `"ext"`.
4.  (Statistics Austria Employees only) You downloaded a json request
    from one of our internal STATcube servers and try to use this
    request with
    [`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
    from outside.

Note to future-self: It might be a good idea to set up some environment
variables on Statistic Austria’s internal R servers to avoid (3) and
(4).

### Rate Limit Exceeded

If the rate limit for the amount of requests against the `/table`
endpoint is exceeded, the following error will be shown.

``` r

sc_table_saved("defaulttable_deake005")
```

If you encounter this error, please check if the rate limits are in fact
a plausible reason by using
[`sc_rate_limit_table()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md).
Unfortunately, the response for exceeded rate limits is very generic and
can not be differentiated from the response for invalid json-bodies (see
below). This is why the error message lists two possible reasons.

### Schema

Invalid URIs used with `sc_schema` will be displayed with a special
error type `SCHEMA_COMPONENT_NOT_FOUND`.

``` r

sc_schema("invalid_uri")
```

### Saved Tables

As mentioned in the [saved tables
article](https://statistikat.github.io/STATcubeR/articles/sc_table_saved.md),
the function
[`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
can only access default tables and tables that are saved under the
current user. If an invalid table URI is passed, the server will respond
with a status code of 400.

``` r

sc_table_saved("invalid_uri")
```

### Invalid JSON

If a request against the `/table` endpoint uses an invalid URI in the
json body, this will trigger a content-type error. To showcase this,
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
is used. However, invalid URIs that are used with
[`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
will throw the same error.

``` r

readLines("request.json") %>% cat(sep = "\n")
```

``` json
{
  "database": "str:database:detouextregsai",
  "measures": [ "str:measure:detouextregsai:F-DATA1:INVALID" ],
  "dimensions": [[ "str:field:detouextregsai:F-DATA1:C-SDB_TIT-0" ]]
}
```

``` r

sc_table("request.json")
```

Unfortunately, the response for invalid json bodies is very generic and
can not be differentiated from the response for exceeded rate limits
(see above). This is why the error message lists two possible reasons.

If you encounter this error during the workflow described in the [JSON
requests
article](https://statistikat.github.io/STATcubeR/articles/sc_table.md),
it is very likely caused by exceeded rate limits. This is because json
request that are downloaded by the STATcube GUI should always contain
valid URIs. However, if you either modify the downloaded json requests
or use
[`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md),
the reason “invalid json body” is plausible.

### Cell Limit Exceeded

This error occurs if more than 1 million cells are requested via a
single call to
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
or
[`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md).
If you encounter this error, consider splitting up the request into
multiple smaller requests or defining a filter in the gui or via a
[custom table
filter](https://statistikat.github.io/STATcubeR/articles/sc_table_custom.html#filter).

``` r

sc_table_custom(
  "str:database:debevstand",
  "str:measure:debevstand:F-BEVSTAND:F-ISIS-1",
  c("str:field:debevstand:F-BEVSTAND:C-A10-0",
    "str:valueset:debevstand:F-BEVSTAND:C-GNU-2:C-GNU-2",
    "str:valueset:debevstand:F-BEVSTAND:C-BESC51-0:C-BESC51-0",
    "str:valueset:debevstand:F-BEVSTAND:C-BESC11-0:C-BESC11-0")
)
```

    #> Warning in gzfile(file, "rb"): cannot open compressed file
    #> 'sc_last_error/cell_limit.rds', probable reason 'No such file or directory'

    #> Error in `gzfile()`:
    #> ! cannot open the connection

## Custom Error Handling

If you want to use your own error-handling instead of the default
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) error
handlers, you can get started with the following code sample from one of
our [shiny](https://shiny.posit.co/) applications.

``` r

shiny::observeEvent(input$button_load_data, {
  table <- try(STATcubeR::sc_table_saved(input$id))
  if (inherits(table, "try-error"))
    STATcubeR::sc_last_error_parsed() %>%
    myApp::show_error_prompt()
  else
    as.data.frame(table) %>%
    myApp::process_data()
})
```

[`try()`](https://rdrr.io/r/base/try.html) will turn errors into
“error-objects” of class `"try-error"`. A conditional is then used to
perform different actions for successful and unsuccessful requests. If
an error occurs, the error details are fetched via
[`sc_last_error_parsed()`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md)
and then sent to an error handler. Otherwise, the return value from
[`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
is processed by the success handler.
