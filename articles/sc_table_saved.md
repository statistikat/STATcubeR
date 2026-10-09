# Load Saved Tables

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

If [saved
tables](https://docs.wingarc.com.au/superstar/9.12/superweb2/user-guide/save-and-reload-tables)
are present in STATcube, those can be imported without downloading a
json file. All saved tables can be shown with
[`sc_table_saved_list()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md).

``` r

saved_tables <- sc_table_saved_list()
saved_tables
```

``` r-output
# A data frame: 3 × 2
  label                    id                                            
  <chr>                    <chr>                                         
1 meineErsteTabelle        str:table:16f39429-8a1b-4593-a129-d5c646368f0f
2 meineZweiteTabelle       str:table:e4e1b473-32c4-42b4-a67d-18169af557cc
3 gesundheitsausgaben_alex str:table:a5d29906-bfab-4823-9027-62a5f72e35e0
```

Subsequently the `id` of a saved table can be used to import the table
into R.

``` r

tab <- sc_table_saved(saved_tables$id[1])
```

## Keys and accounts

Tables are always saved to the logged in STATcube account. The API key
is bound to an account and can only list the saved tables from that
account. Saved tables from other accounts can not be listed or
requested.

## Converting saved tables to JSON requests

To make the table available for later use or for other users of
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md), the
response can be exported into a json.

``` r

tab$json$write("tab.json")
```

The generated json file contains an API request that can be used in
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md).

``` r

my_response <- sc_table("tab.json")
```

## Default Tables

Most STATcube databases have an associated default table. Those default
tables can also be loaded with
[`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md).

``` r

sc_table_saved('str:table:defaulttable_deake005')
```

    #> Working hours (Labour Force Survey)
    #> 
    #> Database: deake005 (STATcube)
    #> Measures: Average hours actually worked per week, Average hours usually
    #>   worked per week
    #> Fields: Time section <1>
    #> 
    #> Request: [2026-10-09 04:57:58]
    #> STATcubeR: 1.0.1

All available default tables as well as other saved tables can be
discovered using
[`sc_schema_catalogue()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md).
See the [schema
article](https://statistikat.github.io/STATcubeR/articles/sc_schema.md)
for more details.

``` r

sc_schema_catalogue() %>% 
  sc_schema_flatten("TABLE")
```

``` r-output
# A data frame: 809 × 2
   id                                         label                             
   <chr>                                      <chr>                             
 1 str:table:defaulttable_dekonjunkturmonitor Standardtabelle / Default table (…
 2 str:table:defaulttable_dewatlas1           Standardtabelle / Default table (…
 3 str:table:defaulttable_dewatlas11          Standardtabelle / Default table (…
 4 str:table:defaulttable_dewatlas3           Standardtabelle / Default table (…
 5 str:table:defaulttable_dewatlas4           Standardtabelle / Default table (…
 6 str:table:defaulttable_dewatlas5           Standardtabelle / Default table (…
 7 str:table:defaulttable_dewatlas6           Standardtabelle / Default table (…
 8 str:table:defaulttable_dewatlas7           Standardtabelle / Default table (…
 9 str:table:defaulttable_dewatlas8           Standardtabelle / Default table (…
10 str:table:defaulttable_dewatlas9           Standardtabelle / Default table (…
# ℹ 799 more rows
```
