# Create a request against the /schema endpoint

Invoke the
[**/schema**](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/schema-endpoint)
endpoint of the STATcube REST API. This endpoint can be used to get all
available databases and tables as well as metadata about specific
databases.

The main function `sc_schema()` can be used with any resource id.
`sc_schema_catalogue()` and `sc_schema_db()` are very simple wrapper
functions around `sc_schema()` and are comparable to the catalogue
explorer or the table view of the STATcube GUI.

The responses of the API are tree-like data structures which are wrapped
into a class called `sc_schema` to simplify the usage in R.

## Usage

``` r
sc_schema(id = NULL, depth = NULL, language = NULL, key = NULL, server = "ext")

# S3 method for class 'sc_schema'
print(x, tree = NULL, ..., limit = 30)

sc_schema_flatten(x, type)

sc_schema_catalogue(depth = "folder", ...)

sc_schema_db(id, depth = "valueset", language = c("en", "de"), key = NULL)
```

## Arguments

- id:

  A resource identifier in uid format. In case of `sc_schema_db()`, this
  should be a database id. For `sc_schema()` any resource-id (folder,
  measure, table, ...) is accepted.

- depth:

  If provided, the request will recurse into the given level. For
  datasets, available options are `NULL` (no recursion), `"folder"`,
  `"field"` and `"valueset"`. For the catalogue, only `NULL` and
  `"folder"` are applicable.

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

  an object of class `sc_schema()` i.e. the return value of
  `sc_schema()`, `sc_schema_db()` or `sc_schema_catalogue()`.

- tree:

  whether to use the
  [`data.tree`](https://rdrr.io/cran/data.tree/man/data.tree.html)
  package for printing.

- limit, ...:

  passed to
  [`data.tree::print.Node()`](https://rdrr.io/pkg/data.tree/man/print.Node.html)
  if `tree` is set to `TRUE`. Ignored otherwise.

- type:

  a schema type such as "DATABASE", "VALUE" or "TABLE". See [the API
  reference](https://docs.wingarc.com.au/superstar/9.12/open-data-api/open-data-api-reference/schema-endpoint#id-.SchemaOpenDataAPIv9.9.6-SchemaTypesandAssociatedIDSchemes)
  for a list of all schema types.

## Value

- for `sc_schema()` and `sc_schema_db()`: an object of class `sc_schema`

- for `sc_schema_flatten()`: a `data.frame`

- for `sc_schema_catalogue()`: a `list`

## Functions

- `sc_schema_flatten()`: turns a `sc_schema` object into a `data.frame`

- `sc_schema_catalogue()`: is similar to the Catalogue explorer (see
  [`sc_browse_catalogue()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md))
  of the STATcube GUI and returns a tree-type object containing all
  databases and tables.

- `sc_schema_db()`: is similar to the [table
  view](https://portal.statistik.at/statistik.at/ext/statcube/opendatabase?id=deake005)
  of the STATcube GUI and gives information about all measures and
  classification fields for a specific database

## Printing with data.tree

`limit` and `...` will simply be ignored if `tree` is set to `FALSE`,
which is the default. The printing via `data.tree` can take longer than
the default implementation because `x` will need to be converted into a
`data.tree` node. To use `data.tree` printing permanently, use

    options(STATcubeR.print_tree = TRUE)

## Examples

``` r
my_catalogue <- sc_schema_catalogue()

## print
my_catalogue
#> FOLDER: Extern V9
#>  1 ABS FH NÖ Fachhochschule St. Pölten                 FOLDER     1
#>  2 ABS FH NÖ Fachhochschule Wiener Neustadt            FOLDER     1
#>  3 ABS FH NÖ Fachhochschulen Niederösterreich Gesamt   FOLDER     1
#>  4 ABS FH NÖ Ferdinand Porsche FernFH                  FOLDER     1
#>  5 ABS FH NÖ IMC Fachhochschule Krems                  FOLDER     1
#>  6 Temporary Employment                                FOLDER    12
#>  7 ATRACK Akademie der bildenden Künste Wien           FOLDER     1
#>  8 ATRACK Alpen-Adria-Universität Klagenfurt           FOLDER     1
#>  9 ATRACK CAMPUS 02 Fachhochschule der Wirtschaft GmbH FOLDER     1
#> 10 ATRACK Fachhochschule Burgenland GmbH               FOLDER     1
#> # ℹ 43 more rows

## access the parsed catalogue
my_catalogue$Statistics$`Labour Market`
#> FOLDER: Labour Market
#>  1 Job Seeker                               FOLDER       0
#>  2 Labour Market Policy                     FOLDER       0
#>  3 Working Time                             FOLDER       0
#>  4 Selected Labour Market Indicators        FOLDER       6
#>  5 Activity Status                          FOLDER       0
#>  6 Employment                               FOLDER       0
#>  7 Job Vacancies                            FOLDER       4
#>  8 Working hours (Labour Force Survey)      DATABASE     0
#>  9 Austrian Micro census - Labour Force Sur DATABASE     0
#> 10 Austrian Micro census - Labour Force Sur DATABASE     0
#> 11 Mikrozensus-Zeitreihe ab 1974            DATABASE     0
#> 12 Standardtabelle / Default table (default TABLE        0
#> 13 Standardtabelle / Default table (default TABLE        0
#> 14 Standardtabelle / Default table (default TABLE        0
#> 15 Standardtabelle / Default table (default TABLE        0
my_catalogue$Statistics$`Labour Market`$`Working hours (Labour Force Survey)`
#> DATABASE: Working hours (Labour Force Survey)
#> # Get more metdata with `sc_schema_db('deake005')` 

db_schema <- sc_schema_db("deake005")

# printing
db_schema
#> DATABASE: Working hours (Labour Force Survey)
#> 1 Factors                              GROUP     9
#> 2 Datensätze/Records                   GROUP     1
#> 3 Time (mandatory field)               GROUP     1
#> 4 Demographic Characteristics          GROUP     8
#> 5 Employment Characteristics           GROUP     6
#> 6 Working Time                         GROUP     5
#> 7 Characteristics of second employment GROUP     6
#> 8 Geographic Characteristics           GROUP     1

# access child nodes
db_schema$`Demographic Characteristics`
#> GROUP: Demographic Characteristics
#> 1 Gender                                   FIELD     1
#> 2 Age in years-groups                      FIELD     3
#> 3 Educational attainment – highest complet FIELD     2
#> 4 Educational attainment – highest complet FIELD     2
#> 5 Educational attainment – highest complet FIELD     2
#> 6 Nationality                              FIELD     2
#> 7 Country of birth                         FIELD     2
#> 8 Migration background                     FIELD     2
db_schema$`Demographic Characteristics`$Gender$Gender
#> VALUESET: Gender
#> 1 male                 VALUE
#> 2 female               VALUE
#> 3 Not classifiable <0> VALUE
db_schema$`Demographic Characteristics`$Gender$Gender$male
#> VALUE: male

# access the raw response from httr::GET()
my_response <- attr(db_schema, "response")
my_response$headers$date
#> [1] "Thu, 08 Oct 2026 14:57:22 GMT"
my_content <- httr::content(my_response)
my_content$label
#> [1] "Working hours (Labour Force Survey)"

# print with data.tree

 treeX_B1 <- sc_schema("str:group:deake005:X_B1", depth = "valueset")
 print(treeX_B1, tree = TRUE)
#>                           levelName     type
#> 1  Demographic Characteristics         GROUP
#> 2   ¦--Gender                          FIELD
#> 3   ¦   °--Gender                   VALUESET
#> 4   ¦       ¦--male                    VALUE
#> 5   ¦       ¦--female                  VALUE
#> 6   ¦       °--Not classifiable <0>    VALUE
#> 7   ¦--Age in years-groups             FIELD
#> 8   ¦   ¦--Age in years-groups      VALUESET
#> 9   ¦   ¦   ¦--Under 15 years          VALUE
#> 10  ¦   ¦   ¦--15 to 19 years          VALUE
#> 11  ¦   ¦   ¦--20 to 24 years          VALUE
#> 12  ¦   ¦   ¦--25 to 29 years          VALUE
#> 13  ¦   ¦   ¦--30 to 34 years          VALUE
#> 14  ¦   ¦   ¦--35 to 39 years          VALUE
#> 15  ¦   ¦   ¦--40 to 44 years          VALUE
#> 16  ¦   ¦   ¦--45 to 49 years          VALUE
#> 17  ¦   ¦   ¦--50 to 54 years          VALUE
#> 18  ¦   ¦   ¦--55 to 59 years          VALUE
#> 19  ¦   ¦   ¦--60 to 64 years          VALUE
#> 20  ¦   ¦   ¦--65 to 69 years          VALUE
#> 21  ¦   ¦   ¦--70 to 74 years          VALUE
#> 22  ¦   ¦   ¦--75 years and older      VALUE
#> 23  ¦   ¦   °--Not classifiable <0>    VALUE
#> 24  ¦   ¦--Alter in Jahresgruppen   VALUESET
#> 25  ¦   ¦   ¦--Under 15 years          VALUE
#> 26  ¦   ¦   ¦--15 to 24 years          VALUE
#> 27  ¦   ¦   ¦--25 to 34 years          VALUE
#> 28  ¦   ¦   ¦--35 to 44 years          VALUE
#> 29  ¦   ¦   ¦--45 to 54 years          VALUE
#> 30  ¦   ¦   °--... 3 nodes w/ 0 sub         
#> 31  ¦   °--... 1 nodes w/ 6 sub             
#> 32  °--... 6 nodes w/ 80 sub                
```
