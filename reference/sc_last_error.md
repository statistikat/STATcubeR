# Error handling for the STATcube REST API

In case API requests are unsuccessful, `STATcubeR` will throw errors to
summarize the httr error type and its meaning. Requests are considered
unsuccessful if one of the following applies

- The response returns `TRUE` for
  [`httr::http_error()`](https://httr.r-lib.org/reference/http_error.html).

- The response is not of type `"application/json"`

In some cases it is useful to get direct access to a faulty response
object. For that purpose, it is possible to use `sc_last_error()` which
will provide the httr response object for the last unsuccessful request.

## Usage

``` r
sc_last_error()

sc_last_error_parsed()
```

## Value

The return value from
[`httr::GET()`](https://httr.r-lib.org/reference/GET.html) or
[`httr::POST()`](https://httr.r-lib.org/reference/POST.html).

## Functions

- `sc_last_error_parsed()`: returns the last error as a list containing
  the response content and the response status

## Examples

``` r
try(sc_table_saved("invalid_id"))
#> Error : Client error: (400) Bad Request
#> {
#>   "message": "The requested TXD 'str:table:invalid_id' could not be found for the current user.",
#>   "errorType": "TXD_NOT_FOUND",
#>   "component": "str:table:invalid_id"
#> }
#> Run `sc_last_error()` or read the online documentation for more details
last_error <- sc_last_error()
httr::content(last_error)
#> $message
#> [1] "The requested TXD 'str:table:invalid_id' could not be found for the current user."
#> 
#> $errorType
#> [1] "TXD_NOT_FOUND"
#> 
#> $component
#> [1] "str:table:invalid_id"
#> 
str(sc_last_error_parsed())
#> List of 3
#>  $ request:List of 2
#>   ..$ method: chr "GET"
#>   ..$ url   : chr "https://statcubeapi.statistik.at/statistik.at/ext/statcube/rest/v1/table/saved/str:table:invalid_id"
#>  $ content:List of 3
#>   ..$ message  : chr "The requested TXD 'str:table:invalid_id' could not be found for the current user."
#>   ..$ errorType: chr "TXD_NOT_FOUND"
#>   ..$ component: chr "str:table:invalid_id"
#>  $ status :List of 3
#>   ..$ category: chr "Client error"
#>   ..$ reason  : chr "Bad Request"
#>   ..$ message : chr "Client error: (400) Bad Request"
```
