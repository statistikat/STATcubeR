# Turn sc_data objects into tidy data frames

`sc_tabulate()` extracts the data in the table and turns it into a tidy
data.frame. It applies labeling of the data and transforms time
variables into a `Date` format if they satisfy certain 'STATcube'
standards.

`sc_tabulate(table, ...)` is just an alias for `table$tabulate(...)` and
was added so this rather complicated method can have a separate
documentation page. It is recommended to use the `table$tabulate()`
syntax

the `...` argument decides which measures and/or fields should be
included in the output. If no measures are given, all measures are
included. The same is true for fields.

## Usage

``` r
sc_tabulate(
  table,
  ...,
  .list = NULL,
  raw = FALSE,
  parse_time = TRUE,
  recode_zeros = inherits(table, "sc_table"),
  language = NULL,
  sort = FALSE
)
```

## Arguments

- table:

  An object of class `sc_data`

- ...:

  Names of measures and/or fields

- .list:

  allows to define the arguments for `...` as a character vector.

- raw:

  If FALSE (the default), apply labeling to the dataset. Otherwise,
  return codes.

- parse_time:

  Should time variables be converted into a `Date` format? Ignored if
  `raw` is set to `TRUE`.

- recode_zeros:

  turn zero values into `NA`s

- language:

  The language to be used for labeling. By default, the dataset language
  (`table$language`) is used.

- sort:

  If `TRUE`, the resulting data will be sorted by all provided field
  values

## Value

a `data.frame`

## Details

Aggregation is done as follows

- First, all columns that provide a total code via `table$total_codes()`
  will be used to filter for `column == total_code` or
  `column != total_code`

- Then, the remaining data is aggregated using
  [`rowsum()`](https://rdrr.io/r/base/rowsum.html)

The ellipsis (`...`) supports partial matching of codes and labels. See
Examples

For objects of class `sc_table` two additional operations are performed.

- zeros are recoded to `NA`s

- rounding is done according to the precision of each measure. Rounding
  happens after the recoding to `NA` values

## See also

sc_table_class

## Examples

``` r
############################ OGD Data #######################################

table <- od_table("OGD_veste309_Veste309_1")

# no arguments -> same output as `table$data`
table$tabulate()
#> # A STATcubeR tibble: 72 x 9
#>    Sex       Citizenship `Region (NUTS2)`   `Form of employment`      
#>  * <fct>     <fct>       <fct>              <fct>                     
#>  1 Sum total Total       Total              "Total"                   
#>  2 Sum total Total       Total              "Standard employment "    
#>  3 Sum total Total       Total              "Non-standard employment …
#>  4 Sum total Total       Total              "Non-standard employment:…
#>  5 Sum total Total       Total              "Non-standard employment:…
#>  6 Sum total Total       Total              "Non-standard employment:…
#>  7 Sum total Total       Total              "Non-standard employment:…
#>  8 Sum total Total       AT11 Burgenland    "Total"                   
#>  9 Sum total Total       AT12 Lower Austria "Total"                   
#> 10 Sum total Total       AT13 Vienna        "Total"                   
#> # ℹ 62 more rows
#> # ℹ 5 more variables: `Arithmetic mean` <dbl>, `1st quartile` <dbl>,
#> #   `2nd quartile (median)` <dbl>, `3rd quartile` <dbl>,
#> #   `Number of employees` <dbl>

# provide some fields -> aggregate to keep only these fields
table$tabulate("Sex", "Citizenship")
#> # A STATcubeR tibble: 27 x 7
#>    Sex       Citizenship              `Arithmetic mean` `1st quartile`
#>  * <fct>     <fct>                                <dbl>          <dbl>
#>  1 Sum total "Total"                              266.          181.  
#>  2 Sum total "Austrian"                            18.5          12.4 
#>  3 Sum total "Non-Austrian "                       14.4           9.72
#>  4 Sum total "EU-28/EWR/Switzerland …              15.2          10.0 
#>  5 Sum total "EU-15/EWR before 2004/…              19.6          11.9 
#>  6 Sum total "EU-13 EU Member States…              13.0           9.59
#>  7 Sum total "Former Yugoslavia (wit…              12.8           9.58
#>  8 Sum total "Turkey"                              12.2           9.35
#>  9 Sum total "Other"                               13.8           9.07
#> 10 Male      "Total"                              290.          195.  
#> # ℹ 17 more rows
#> # ℹ 3 more variables: `2nd quartile (median)` <dbl>,
#> #   `3rd quartile` <dbl>, `Number of employees` <dbl>

# provide some measures -> drop all other measures from the output
table$tabulate("Arithmetic mean")
#> # A STATcubeR tibble: 72 x 5
#>    Sex       Citizenship `Region (NUTS2)`   `Form of employment`      
#>  * <fct>     <fct>       <fct>              <fct>                     
#>  1 Sum total Total       Total              "Total"                   
#>  2 Sum total Total       Total              "Standard employment "    
#>  3 Sum total Total       Total              "Non-standard employment …
#>  4 Sum total Total       Total              "Non-standard employment:…
#>  5 Sum total Total       Total              "Non-standard employment:…
#>  6 Sum total Total       Total              "Non-standard employment:…
#>  7 Sum total Total       Total              "Non-standard employment:…
#>  8 Sum total Total       AT11 Burgenland    "Total"                   
#>  9 Sum total Total       AT12 Lower Austria "Total"                   
#> 10 Sum total Total       AT13 Vienna        "Total"                   
#> # ℹ 62 more rows
#> # ℹ 1 more variable: `Arithmetic mean` <dbl>

# mixture of measures and fields  -> keep exactly those columns
table$tabulate("Sex", "Arithmetic mean")
#> # A STATcubeR tibble: 3 x 2
#>   Sex       `Arithmetic mean`
#> * <fct>                 <dbl>
#> 1 Sum total              385.
#> 2 Male                   418.
#> 3 Female                 342.

## define total codes
table$total_codes(
  `C-A11-0` = "A11-1",
  `C-STAATS-0` = "STAATS-9",
  `C-VEBDL-0` = "VEBDL-10",
  `C-BESCHV-0` = "BESCHV-1"
)

## alternatively, use partial matching to define totals
table$total_codes(
  Sex = "Sum total",
  Citizenship = "Total",
  Region = "Total",
  `Form of employment` = "Total"
)

# filter for totals in `Region (NUTS2)` and `Form of employment`. Drop totals
# in `Sex` and `Citizenship`.
table$tabulate("Sex", "Citizenship")
#> # A STATcubeR tibble: 16 x 7
#>    Sex    Citizenship                 `Arithmetic mean` `1st quartile`
#>  * <fct>  <fct>                                   <dbl>          <dbl>
#>  1 Male   "Austrian"                               20.4          13.8 
#>  2 Male   "Non-Austrian "                          15.3          10.6 
#>  3 Male   "EU-28/EWR/Switzerland (wi…              16.0          10.9 
#>  4 Male   "EU-15/EWR before 2004/Swi…              21.2          12.6 
#>  5 Male   "EU-13 EU Member States af…              13.6          10.3 
#>  6 Male   "Former Yugoslavia (withou…              14.1          11.2 
#>  7 Male   "Turkey"                                 13.3          10.4 
#>  8 Male   "Other"                                  14.0           9.07
#>  9 Female "Austrian"                               15.9          11.1 
#> 10 Female "Non-Austrian "                          13.0           9.19
#> 11 Female "EU-28/EWR/Switzerland (wi…              13.7           9.38
#> 12 Female "EU-15/EWR before 2004/Swi…              17.1          11.0 
#> 13 Female "EU-13 EU Member States af…              12.1           9.06
#> 14 Female "Former Yugoslavia (withou…              10.9           9   
#> 15 Female "Turkey"                                 10.3           8.68
#> 16 Female "Other"                                  13.3           9.07
#> # ℹ 3 more variables: `2nd quartile (median)` <dbl>,
#> #   `3rd quartile` <dbl>, `Number of employees` <dbl>

## switch language
table$language <- "de"

## `...` matches for codes and labels
table$tabulate("C-A11-0", "Staats", "2. Quartil (Median)")
#> # A STATcubeR tibble: 16 x 3
#>    Geschlecht Staatsangehörigkeit                `2. Quartil (Median)`
#>  * <fct>      <fct>                                              <dbl>
#>  1 männlich   "Österreich"                                       17.4 
#>  2 männlich   "Nicht-Österreich"                                 13.2 
#>  3 männlich   "EU-28/EWR/Schweiz (ohne Österrei…                 13.5 
#>  4 männlich   "EU-15/EWR vor 2004/Schweiz "                      16.7 
#>  5 männlich   "EU-13 Beitrittsstaaten ab 2004"                   12.7 
#>  6 männlich   "ehemaliges Jugoslawien (außerhal…                 13.6 
#>  7 männlich   "Türkei"                                           12.8 
#>  8 männlich   "Andere"                                           11.2 
#>  9 weiblich   "Österreich"                                       14.0 
#> 10 weiblich   "Nicht-Österreich"                                 10.6 
#> 11 weiblich   "EU-28/EWR/Schweiz (ohne Österrei…                 11.3 
#> 12 weiblich   "EU-15/EWR vor 2004/Schweiz "                      14.7 
#> 13 weiblich   "EU-13 Beitrittsstaaten ab 2004"                   10.3 
#> 14 weiblich   "ehemaliges Jugoslawien (außerhal…                  9.78
#> 15 weiblich   "Türkei"                                            9.51
#> 16 weiblich   "Andere"                                           10.6 

## Keep totals in the output by removing total codes
table$tabulate("C-A11-0")      # -> 2 rows: "male" "female"
#> # A STATcubeR tibble: 2 x 6
#>   Geschlecht Arithmetisches Mitte…¹ `1. Quartil` `2. Quartil (Median)`
#> * <fct>                       <dbl>        <dbl>                 <dbl>
#> 1 männlich                     19.2         12.8                  16.3
#> 2 weiblich                     15.3         10.4                  13.4
#> # ℹ abbreviated name: ¹​`Arithmetisches Mittel`
#> # ℹ 2 more variables: `3. Quartil` <dbl>,
#> #   `Zahl d unselbst Beschäftigten` <dbl>
table$total_codes(`C-A11-0` = NA)
table$tabulate("C-A11-0")      # -> 3 rows: "total", "male", "female"
#> # A STATcubeR tibble: 3 x 6
#>   Geschlecht Arithmetisches Mitte…¹ `1. Quartil` `2. Quartil (Median)`
#> * <fct>                       <dbl>        <dbl>                 <dbl>
#> 1 insgesamt                    17.6         11.6                  15.1
#> 2 männlich                     19.2         12.8                  16.3
#> 3 weiblich                     15.3         10.4                  13.4
#> # ℹ abbreviated name: ¹​`Arithmetisches Mittel`
#> # ℹ 2 more variables: `3. Quartil` <dbl>,
#> #   `Zahl d unselbst Beschäftigten` <dbl>

## table$tabulate(...) is an alias for sc_tabulate(table, ...)
sc_tabulate(table, "C-A11-0")
#> # A STATcubeR tibble: 3 x 6
#>   Geschlecht Arithmetisches Mitte…¹ `1. Quartil` `2. Quartil (Median)`
#> * <fct>                       <dbl>        <dbl>                 <dbl>
#> 1 insgesamt                    17.6         11.6                  15.1
#> 2 männlich                     19.2         12.8                  16.3
#> 3 weiblich                     15.3         10.4                  13.4
#> # ℹ abbreviated name: ¹​`Arithmetisches Mittel`
#> # ℹ 2 more variables: `3. Quartil` <dbl>,
#> #   `Zahl d unselbst Beschäftigten` <dbl>

######################## 'STATcube' REST API ################################
table_tourism <- sc_table(sc_example("accomodation.json"), "de")

table_tourism$tabulate()
#> # A STATcubeR tibble: 2,898 x 5
#>    `Saison / Tourismusmonat` Herkunftsland Beherbergungsbetrieb     
#>  * <date>                    <fct>         <fct>                    
#>  1 1999-11-01                Österreich    Hotels gesamt            
#>  2 1999-11-01                Österreich    Ferienwohnungen insgesamt
#>  3 1999-11-01                Österreich    Sonstige                 
#>  4 1999-11-01                Deutschland   Hotels gesamt            
#>  5 1999-11-01                Deutschland   Ferienwohnungen insgesamt
#>  6 1999-11-01                Deutschland   Sonstige                 
#>  7 1999-11-01                andere Länder Hotels gesamt            
#>  8 1999-11-01                andere Länder Ferienwohnungen insgesamt
#>  9 1999-11-01                andere Länder Sonstige                 
#> 10 1999-12-01                Österreich    Hotels gesamt            
#> # ℹ 2,888 more rows
#> # ℹ 2 more variables: Übernachtungen <dbl>, Ankünfte <int>
table_tourism$tabulate("Saison / Tourismusmonat")
#> # A STATcubeR tibble: 322 x 3
#>    `Saison / Tourismusmonat` Übernachtungen Ankünfte
#>  * <date>                             <dbl>    <int>
#>  1 1999-11-01                       2646717   961334
#>  2 1999-12-01                       7373034  1771530
#>  3 2000-01-01                      12437812  2462258
#>  4 2000-02-01                      13480000  2532404
#>  5 2000-03-01                      11340174  2273436
#>  6 2000-04-01                       6138146  1666777
#>  7 2000-05-01                       5150347  1673232
#>  8 2000-06-01                       8908931  2438518
#>  9 2000-07-01                      14618147  3118576
#> 10 2000-08-01                      16498662  3248084
#> # ℹ 312 more rows
table_tourism$tabulate("Saison / Tourismusmonat", "Ankünfte")
#> # A STATcubeR tibble: 322 x 2
#>    `Saison / Tourismusmonat` Ankünfte
#>  * <date>                       <int>
#>  1 1999-11-01                  961334
#>  2 1999-12-01                 1771530
#>  3 2000-01-01                 2462258
#>  4 2000-02-01                 2532404
#>  5 2000-03-01                 2273436
#>  6 2000-04-01                 1666777
#>  7 2000-05-01                 1673232
#>  8 2000-06-01                 2438518
#>  9 2000-07-01                 3118576
#> 10 2000-08-01                 3248084
#> # ℹ 312 more rows
table_tourism$tabulate("Ankünfte")
#> # A STATcubeR tibble: 2,898 x 4
#>    Saison / Tourismusmon…¹ Herkunftsland Beherbergungsbetrieb Ankünfte
#>  * <date>                  <fct>         <fct>                   <int>
#>  1 1999-11-01              Österreich    Hotels gesamt          371215
#>  2 1999-11-01              Österreich    Ferienwohnungen ins…     5365
#>  3 1999-11-01              Österreich    Sonstige                67272
#>  4 1999-11-01              Deutschland   Hotels gesamt          223856
#>  5 1999-11-01              Deutschland   Ferienwohnungen ins…    13095
#>  6 1999-11-01              Deutschland   Sonstige                24262
#>  7 1999-11-01              andere Länder Hotels gesamt          235343
#>  8 1999-11-01              andere Länder Ferienwohnungen ins…     6493
#>  9 1999-11-01              andere Länder Sonstige                14433
#> 10 1999-12-01              Österreich    Hotels gesamt          346040
#> # ℹ 2,888 more rows
#> # ℹ abbreviated name: ¹​`Saison / Tourismusmonat`
```
