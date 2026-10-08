# Check availability of the 'STATcube' REST API

Returns `TRUE` if the 'STATcube' REST API of Statistics Austria is
reachable and returns valid (JSON) content. This also detects cases
where the server is technically reachable (HTTP status 200) but serves
an intermediate html page (e.g. during maintenance work). Note that an
HTTP status 401 (invalid/missing API key) still counts as reachable, as
the API itself responds with valid JSON. The result is cached within an
R session. It is mainly used internally to guard examples and network
functions against an unavailable server.

## Usage

``` r
sc_server_reachable(server = c("ext", "red", "prod", "test"), timeout = 5)
```

## Arguments

- server:

  A STATcube API server. Defaults to the external Server via `"ext"`.
  Other options are `"red"` for the editing server and `"prod"` for the
  production server. External users should always use the default option
  `"ext"`.

- timeout:

  timeout of the health check request in seconds

## Value

a [`logical()`](https://rdrr.io/r/base/logical.html) of length one
