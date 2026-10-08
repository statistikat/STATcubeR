# Import data from SDMX

Function that reads STATcube data from an sdmx archive - a zip file
consisting of `structure.xml` with metadata and `dataset.xml` for the
values.

## Usage

``` r
sdmx_table(file)
```

## Arguments

- file:

  a "sdmx archive" file that was downloaded from STATcube.

## Value

An object of class `sc_data`

## Note

`sdmx_table()` should be treated as experimental for now.

## Examples

``` r
x <- sdmx_table(system.file("sdmx/dedemo.zip", package = "STATcubeR"))
# print and tabulate
x
#> Communes (Demo)
#> 
#> Database: dedemo
#> Measures: Ordinary expenditures, Ordinary revenues
#> Fields: Time, Einwohnerstufen, Bundesland (NUTS 2-Einheit)
#> 
#> Downloaded: [2023-04-12 11:04:09]
#> STATcubeR: 1.0.1
x$tabulate()
#> # A STATcubeR tibble: 16 x 5
#>    Time  Einwohnerstufen Bundesland (NUTS 2-E…¹ Ordinary expenditure…²
#>    <fct> <fct>           <fct>                                   <dbl>
#>  1 2016  bis 2 500 Einw… Burgenland <AT11>                      324021
#>  2 2016  bis 2 500 Einw… Carinthia <AT21>                       241691
#>  3 2016  bis 2 500 Einw… Eisenstadt urban area…                      0
#>  4 2016  bis 2 500 Einw… Rust urban area <102>                    5007
#>  5 2016  2 501 bis 5 00… Burgenland <AT11>                      128217
#>  6 2016  2 501 bis 5 00… Carinthia <AT21>                       185109
#>  7 2016  2 501 bis 5 00… Eisenstadt urban area…                      0
#>  8 2016  2 501 bis 5 00… Rust urban area <102>                       0
#>  9 2017  bis 2 500 Einw… Burgenland <AT11>                      301689
#> 10 2017  bis 2 500 Einw… Carinthia <AT21>                       246970
#> 11 2017  bis 2 500 Einw… Eisenstadt urban area…                      0
#> 12 2017  bis 2 500 Einw… Rust urban area <102>                    5321
#> 13 2017  2 501 bis 5 00… Burgenland <AT11>                      117864
#> 14 2017  2 501 bis 5 00… Carinthia <AT21>                       192845
#> 15 2017  2 501 bis 5 00… Eisenstadt urban area…                      0
#> 16 2017  2 501 bis 5 00… Rust urban area <102>                       0
#> # ℹ abbreviated names: ¹​`Bundesland (NUTS 2-Einheit)`,
#> #   ²​`Ordinary expenditures`
#> # ℹ 1 more variable: `Ordinary revenues` <dbl>
# explore hierarchies
nuts2 <- x$field("C-B00-0")
data.frame(label = nuts2$label,
  parent = nuts2$label[match(nuts2$parent, nuts2$code)])
#>                         label            parent
#> 1           Burgenland <AT11>              <NA>
#> 2            Carinthia <AT21>              <NA>
#> 3 Eisenstadt urban area <101> Burgenland <AT11>
#> 4       Rust urban area <102> Burgenland <AT11>
# extract more data from the raw xml
xml2::xml_find_first(x$xml$meta, ".//Name")
#> {xml_node}
#> <Name lang="de">
```
