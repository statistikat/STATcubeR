# Check availability of the OGD server

Returns `TRUE` if the open data server of Statistics Austria is
reachable and returns valid (JSON) content. This also detects cases
where the server is technically reachable (HTTP status 200) but serves
an intermediate html page (e.g. during maintenance work). The result is
cached within an R session. It is mainly used internally to guard
examples and network functions against an unavailable server.

## Usage

``` r
od_server_reachable(server = c("ext", "red"), timeout = 5)
```

## Arguments

- server:

  the OGD-Server to check. `"ext"` for the external server (the default)
  or `"red"` for the editing server

- timeout:

  timeout of the health check request in seconds

## Value

a [`logical()`](https://rdrr.io/r/base/logical.html) of length one
