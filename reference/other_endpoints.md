# Other endpoints of the STATcube REST API

Utilize the simple endpoints `/info` and `/table_rate_limit`. Those
provide information about available locales and the amount of requests
available for calls against the `/table` endpoint.

## Usage

``` r
sc_info(language = c("en", "de"), key = NULL, server = "ext")

sc_rate_limit_table(language = c("en", "de"), key = NULL, server = "ext")

sc_rate_limit_schema(language = c("en", "de"), key = NULL, server = "ext")

sc_rate_limits(x)
```

## Arguments

- language:

  The language to be used for labeling. `"en"` or `"de"`

- key:

  (`string`) An API key. To display your key, call
  [`sc_browse_preferences()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md).

- server:

  A STATcube API server. Defaults to the external Server via `"ext"`.
  Other options are `"red"` for the editing server and `"prod"` for the
  production server. External users should always use the default option
  `"ext"`.

- x:

  either a response-object (package `httr`), an object of class
  `sc_table` or an object of class `sc_schema`

## Value

- `sc_info()`: a `data.frame` with two columns identifying possible
  languages

- `sc_rate_limit_table()`, `sc_rate_limit_schema()`, `sc_rate_limits()`:
  a `list` with elements

  - `remaining`: how much requests can be sent until the rate limit is
    reached

  - `limit`: the number of requests allowed per hour

  - `reset`: a timestamp when the rate limit will be reset

- `sc_rate_limits()`:

## Functions

- `sc_info()`: returns information about all available database
  languages

- `sc_rate_limit_table()`: returns a `list` with information about
  current requests-limits with respect to the `/table` endpoint. It also
  shows when the limits reset which should be less than one hour after
  the current time.

- `sc_rate_limit_schema()`: returns a `list` with information about
  current requests-limits with respect to the `/schema` endpoint. It
  also shows when the limits reset which should be less than one hour
  after the current time.

- `sc_rate_limits()`: gets rate limits from response headers

## Examples

``` r
sc_info()
#> # A data frame: 2 × 2
#>   locale displayName
#>   <chr>  <chr>      
#> 1 de     Deutsch    
#> 2 en     English    
sc_rate_limit_table()
#> 100 / 100 (Resets at [05:52:26])
#> 
sc_rate_limit_schema()
#> 9997 / 10000 (Resets at [05:52:26])
#> 
sc_rate_limits(sc_schema("str:group:deake005:X_B1"))
#> $schema
#> 9997 / 10000 (Resets at [05:52:26])
#> 
#> 
#> $table
#> 100 / 100 (Resets at [05:52:26])
#> 
#> 
```
