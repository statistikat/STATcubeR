# Create a request against the /table endpoint

Send requests against the **`/table`** endpoint of the STATcube REST
API. The requests can use three formats with corresponding functions

- `sc_table()` uses a json file downloaded via the STATcube GUI

- [`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
  uses the ids of a database, measures and fields

- `sc_table_saved()` uses a table uri of a saved table.

Those three functions all return an object of class `"sc_table"`.

## Usage

``` r
sc_table(json, language = NULL, add_totals = TRUE, key = NULL, json_file = NA)

sc_examples_list()

sc_example(filename)

sc_table_saved_list(key = NULL, server = "ext")

sc_table_saved(table_uri, language = NULL, key = NULL, server = "ext")
```

## Arguments

- json:

  Path to a json file, which was downloaded via the STATcube GUI ("Open
  Data API Request"). Alternatively, a json string which passes
  [`jsonlite::validate()`](https://jeroen.r-universe.dev/jsonlite/reference/validate.html).

- language:

  The language to be used for labeling. `"en"` (the default) will use
  english. `"de"` uses German. The third option `"both"` will import
  both languages by sending two requests to the `/table` endpoint.

- add_totals:

  Should totals be added for each classification field in the json
  request?

- key:

  (`string`) An API key. To display your key, call
  [`sc_browse_preferences()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md).

- json_file:

  Deprecated. Use `json` instead

- filename:

  The name of an example json file.

- server:

  A STATcube API server. Defaults to the external Server via `"ext"`.
  Other options are `"red"` for the editing server and `"prod"` for the
  production server. External users should always use the default option
  `"ext"`.

- table_uri:

  Identifier of a saved table as returned by `sc_table_saved_list()`

## Value

An object of class `sc_table` which contains the return value of the
[`httr::POST()`](https://httr.r-lib.org/reference/POST.html) request in
`obj$response`. The object also provides member functions to parse this
response object. See
[sc_table_class](https://statistikat.github.io/STATcubeR/reference/sc_table_class.md)
for the class documentation.

## Examples

``` r
my_table <- sc_table(json = sc_example("population_timeseries.json"))

# print
my_table
#> Population at the beginning of the quarter since 2002
#> 
#> Database: debevstand (STATcube)
#> Measures: Number of persons
#> Fields: Quarter <98>, Age in single years <96> <7>, Sex <2> <3>,
#>   Commune <2383> (Province-District) <10>
#> 
#> Request: [2026-10-09 04:52:55]
#> STATcubeR: 1.0.1

# get matadata for the table
my_table$meta
#> $source
#> # STATcubeR metadata: 1 x 6
#>   code       label                                               lang 
#>   <chr>      <chr>                                               <chr>
#> 1 debevstand Population at the beginning of the quarter since 2… en   
#> # … with 3 more columns: 'label_de', 'label_en', 'scr_version'
#> 
#> $measures
#> # STATcubeR metadata: 1 x 8
#>   code     label               NAs
#>   <chr>    <chr>             <int>
#> 1 F-ISIS-1 Number of persons     0
#> # … with 5 more columns: 'fun', 'precision', 'annotations', 'label_de', 'label_en'
#> 
#> $fields
#> # STATcubeR metadata: 4 x 7
#>   code       label                             total_code nitems type 
#>   <chr>      <chr>                             <chr>       <int> <chr>
#> 1 C-A10-0    Quarter                           SC_TOTAL       98 Time…
#> 2 C-BESC51-0 Age in single years <96>          SC_TOTAL        7 Cate…
#> 3 C-BESC11-0 Sex <2>                           SC_TOTAL        3 Cate…
#> 4 C-C41-2    Commune <2383> (Province-Distric… SC_TOTAL       10 Cate…
#> # … with 2 more columns: 'label_de', 'label_en'
#> 

# get a data.frame
as.data.frame(my_table)
#> # A STATcubeR tibble: 20,580 x 5
#>    Quarter    Age in single years <…¹ `Sex <2>` Commune <2383> (Prov…²
#>    <date>     <fct>                   <fct>     <fct>                 
#>  1 2002-01-01 Up to 14 years old      male      Burgenland <AT11>     
#>  2 2002-01-01 Up to 14 years old      male      Carinthia <AT21>      
#>  3 2002-01-01 Up to 14 years old      male      Vienna <AT13>         
#>  4 2002-01-01 Up to 14 years old      male      Vorarlberg <AT34>     
#>  5 2002-01-01 Up to 14 years old      male      Tyrol <AT33>          
#>  6 2002-01-01 Up to 14 years old      male      Styria <AT22>         
#>  7 2002-01-01 Up to 14 years old      male      Salzburg <AT32>       
#>  8 2002-01-01 Up to 14 years old      male      Upper Austria <AT31>  
#>  9 2002-01-01 Up to 14 years old      male      Lower Austria <AT12>  
#> 10 2002-01-01 Up to 14 years old      male      Total                 
#> # ℹ 20,570 more rows
#> # ℹ abbreviated names: ¹​`Age in single years <96>`,
#> #   ²​`Commune <2383> (Province-District)`
#> # ℹ 1 more variable: `Number of persons` <int>

# get metadata for field 2
my_table$field(2)
#> # STATcubeR metadata: 7 x 7
#>   code     label              parsed            
#>   <chr>    <chr>              <chr>             
#> 1 BESN07-1 Up to 14 years old Up to 14 years old
#> 2 BESN07-2 15 to 29 years old 15 to 29 years old
#> 3 BESN07-3 30 to 44 years old 30 to 44 years old
#> 4 BESN07-4 45 to 59 years old 45 to 59 years old
#> 5 BESN07-5 60 to 74 years old 60 to 74 years old
#> 6 BESN07-6 75 plus years old  75 plus years old 
#> 7 SC_TOTAL Total              Total             
#> # … with 4 more columns: 'label_de', 'label_en', 'visible', 'order'


# get the ids and labels of all saved tables
(saved_tables <- sc_table_saved_list())
#> # A data frame: 3 × 2
#>   label                    id                                      
#>   <chr>                    <chr>                                   
#> 1 meineErsteTabelle        str:table:16f39429-8a1b-4593-a129-d5c646
#> 2 meineZweiteTabelle       str:table:e4e1b473-32c4-42b4-a67d-18169a
#> 3 gesundheitsausgaben_alex str:table:a5d29906-bfab-4823-9027-62a5f7
table_uri <- saved_tables$id[1]

# get a table based on one of these ids
my_response <- sc_table_saved(table_uri)
as.data.frame(my_response)
#> # A STATcubeR tibble: 9 x 4
#>   `Fruit variety` `Commune [partly ABO]` `Number of holdings`
#>   <fct>           <fct>                                 <int>
#> 1 Apples          Burgenland <AT11>                       111
#> 2 Apples          Carinthia <AT21>                         31
#> 3 Apples          Lower Austria <AT12>                    439
#> 4 Apples          Upper Austria <AT31>                    108
#> 5 Apples          Salzburg <AT32>                           2
#> 6 Apples          Styria <AT22>                          1116
#> 7 Apples          Tyrol <AT33>                             88
#> 8 Apples          Vorarlberg <AT34>                        31
#> 9 Apples          Vienna <AT13>                             6
#> # ℹ 1 more variable: `Area in ha` <dbl>
```
