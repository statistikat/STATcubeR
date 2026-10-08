# Get the server from a json request

parses a json request and returns a short string representing the
corresponding STATcube server

## Usage

``` r
sc_json_get_server(json)
```

## Arguments

- json:

  path to a request json

## Value

`"ext"`, `"red"` or `"prod"` depending on the database uri in the json
request

## Examples

``` r
sc_json_get_server(sc_example('accomodation'))
#> [1] "ext"
```
