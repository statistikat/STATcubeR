# Common interface for STATcubeR datasets

This class represents a common interface for datasets returned from the
STATcube REST API and OGD datasets. `sc_data` objects are usually
created with
[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
or
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md).

## Active bindings

- `data`:

  the raw data. A data.frame that uses codes for all field variables and
  for all column names. To obtain labeled data, use `$tabulate()`.

- `language`:

  language to be used for labeling. `"en"` or `"de"`

- `meta`:

  A list containing metadata about the dataset. It has at least the
  following entries

  - **`$source`** is a dataframe with a single row that contains
    information on the data source.

  - **`$measures`** is a dataframe with one row for every measure in the
    dataset. It contains codes and labels for each measure as well as
    the number of `NAs`. Derived classes might add additional columns.

  - **`$fields`** is a dataframe with one row for every field in the
    dataset. It contains codes and labels for each measure as well as
    the total codes. Derived classes might add additional columns

- `recode`:

  An object of class
  [sc_recoder](https://statistikat.github.io/STATcubeR/reference/sc_recoder.md)
  that can be used to change labels and perform other recoding
  operations.

## Methods

### Public methods

- [`sc_data$new()`](#method-sc_data-initialize)

- [`sc_data$field()`](#method-sc_data-field)

- [`sc_data$tabulate()`](#method-sc_data-tabulate)

- [`sc_data$total_codes()`](#method-sc_data-total_codes)

- [`sc_data$clone()`](#method-sc_data-clone)

------------------------------------------------------------------------

### `sc_data$new()`

This class is not exported. Use
[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
or
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
to initialize objects of class `sc_data`.

#### Usage

    sc_data$new(data, meta, fields)

#### Arguments

- `data, meta, fields`:

  raw data, metadata and field information. Do not use directly but
  initialize objects with
  [`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  or
  [`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)

------------------------------------------------------------------------

### `sc_data$field()`

get information about a specific field. The format of the return value
is similar to `$meta`. A `data.frame` that includes codes and labels for
each level of the field.

#### Usage

    sc_data$field(i = 1)

#### Arguments

- `i`:

  specifier for the field. Integer or character. If an integer is
  provided, it should match the row number in `$meta$fields`. If a
  character is provided, the field is matched using
  [`pmatch()`](https://rdrr.io/r/base/pmatch.html) on all available
  codes and labels.

------------------------------------------------------------------------

### `sc_data$tabulate()`

create a tidy dataset. See
[`sc_tabulate()`](https://statistikat.github.io/STATcubeR/reference/sc_tabulate.md)
for details.

#### Usage

    sc_data$tabulate(...)

#### Arguments

- `...`:

  arguments that are passed down to
  [`sc_tabulate()`](https://statistikat.github.io/STATcubeR/reference/sc_tabulate.md)

------------------------------------------------------------------------

### `sc_data$total_codes()`

#### Usage

    sc_data$total_codes(...)

#### Arguments

- `...`:

  key value pairs to define the total codes. Key should be a field code
  and value a code from `$field(i)`. If empty, it will return a
  data.frame with all specified total codes. Keys and values can also
  use labels instead of codes. See examples.

------------------------------------------------------------------------

### `sc_data$clone()`

The objects of this class are cloneable with this method.

#### Usage

    sc_data$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## create a new sc_data object via od_table()
x <- od_table("OGD_krebs_ext_KREBS_1")

## show data
x$data
#> # A STATcubeR tibble: 51,796 x 5
#>    `C-TUM_ICD10_3ST-0` `C-BERJ-0` `C-BUNDESLAND-0`
#>  * <fct>               <fct>      <fct>           
#>  1 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-1    
#>  2 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-2    
#>  3 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-2    
#>  4 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-3    
#>  5 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-3    
#>  6 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-4    
#>  7 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-4    
#>  8 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-5    
#>  9 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-6    
#> 10 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-6    
#> # ℹ 51,786 more rows
#> # ℹ 2 more variables: `C-KRE_GESCHLECHT-0` <fct>, `F-KRE` <int>

## show metadata
x$meta
#> $source
#> # STATcubeR metadata: 1 x 7
#>   code                  label                                    lang 
#>   <chr>                 <chr>                                    <chr>
#> 1 OGD_krebs_ext_KREBS_1 Cancer statistics by reporting year, pr… en   
#> # … with 4 more columns: 'label_de', 'label_en', 'requested', 'scr_version'
#> 
#> $measures
#> # STATcubeR metadata: 1 x 7
#>   code  label                     NAs
#>   <chr> <chr>                   <int>
#> 1 F-KRE Number of records F-KRE     0
#> # … with 4 more columns: 'label_de', 'label_en', 'de_desc', 'en_desc'
#> 
#> $fields
#> # STATcubeR metadata: 4 x 9
#>   code               label                   total_code nitems type   
#>   <chr>              <chr>                   <chr>       <int> <chr>  
#> 1 C-TUM_ICD10_3ST-0  Tumore ICD/10 3-Steller NA             98 Catego…
#> 2 C-BERJ-0           Reporting year          NA             42 Time (…
#> 3 C-BUNDESLAND-0     Province of residence   NA              9 Catego…
#> 4 C-KRE_GESCHLECHT-0 Sex                     NA              2 Catego…
#> # … with 4 more columns: 'label_de', 'label_en', 'de_desc', 'en_desc'
#> 
x <- od_table("OGD_krebs_ext_KREBS_1")
x$field(1)
#> # STATcubeR metadata: 98 x 10
#>    code              label                                      parsed
#>    <chr>             <chr>                                      <chr> 
#>  1 TUM_ICD10_3ST-C00 <C00> Bösartige Neubildung der Lippe       <C00>…
#>  2 TUM_ICD10_3ST-C01 <C01> Bösartige Neubildung des Zungengrun… <C01>…
#>  3 TUM_ICD10_3ST-C02 <C02> Bösartige Neubildung sonstiger und … <C02>…
#>  4 TUM_ICD10_3ST-C03 <C03> Bösartige Neubildung des Zahnfleisc… <C03>…
#>  5 TUM_ICD10_3ST-C04 <C04> Bösartige Neubildung des Mundbodens  <C04>…
#>  6 TUM_ICD10_3ST-C05 <C05> Bösartige Neubildung des Gaumens     <C05>…
#>  7 TUM_ICD10_3ST-C06 <C06> Bösartige Neubildung sonstiger und … <C06>…
#>  8 TUM_ICD10_3ST-C07 <C07> Bösartige Neubildung der Parotis     <C07>…
#>  9 TUM_ICD10_3ST-C08 <C08> Bösartige Neubildung sonstiger und … <C08>…
#> 10 TUM_ICD10_3ST-C09 <C09> Bösartige Neubildung der Tonsille    <C09>…
#> # ℹ 88 more rows
#> # … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
x$field("Sex")
#> # STATcubeR metadata: 2 x 10
#>   code         label  parsed
#>   <chr>        <chr>  <chr> 
#> 1 GESCHLECHT-1 male   male  
#> 2 GESCHLECHT-2 female female
#> # … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
x <- od_table("OGD_krebs_ext_KREBS_1")
x$tabulate("Reporting year", "Sex")
#> # A STATcubeR tibble: 84 x 3
#>    `Reporting year` Sex    `Number of records F-KRE`
#>  * <date>           <fct>                      <int>
#>  1 1983-01-01       male                       13625
#>  2 1983-01-01       female                     16702
#>  3 1984-01-01       male                       13996
#>  4 1984-01-01       female                     16622
#>  5 1985-01-01       male                       13656
#>  6 1985-01-01       female                     16488
#>  7 1986-01-01       male                       13592
#>  8 1986-01-01       female                     16172
#>  9 1987-01-01       male                       13957
#> 10 1987-01-01       female                     16725
#> # ℹ 74 more rows
earnings <- od_table("OGD_veste309_Veste309_1")
earnings$total_codes(Sex = "Sum total", Citizenship = "Total",
                     Region = "Total", `Form of employment` = "Total")
earnings$tabulate("Form of employment")
#> # A STATcubeR tibble: 6 x 6
#>   `Form of employment`                `Arithmetic mean` `1st quartile`
#> * <fct>                                           <dbl>          <dbl>
#> 1 "Standard employment "                           19.0           12.8
#> 2 "Non-standard employment (total)"                15.2           10.2
#> 3 "Non-standard employment: part-tim…              15.9           10.7
#> 4 "Non-standard employment: fixed-te…              16.8           10.2
#> 5 "Non-standard employment: marginal…              12              9  
#> 6 "Non-standard employment: temporar…              14.0           11.5
#> # ℹ 3 more variables: `2nd quartile (median)` <dbl>,
#> #   `3rd quartile` <dbl>, `Number of employees` <dbl>
earnings$tabulate("Sex", "Form of employment")
#> # A STATcubeR tibble: 12 x 7
#>    Sex    `Form of employment`        `Arithmetic mean` `1st quartile`
#>  * <fct>  <fct>                                   <dbl>          <dbl>
#>  1 Male   "Standard employment "                   20.0          13.5 
#>  2 Male   "Non-standard employment (…              16.5          10.5 
#>  3 Male   "Non-standard employment: …              18.8          11.1 
#>  4 Male   "Non-standard employment: …              18.3          10.9 
#>  5 Male   "Non-standard employment: …              12.6           9   
#>  6 Male   "Non-standard employment: …              14.4          12   
#>  7 Female "Standard employment "                   16.3          11.1 
#>  8 Female "Non-standard employment (…              14.6          10.0 
#>  9 Female "Non-standard employment: …              15.2          10.6 
#> 10 Female "Non-standard employment: …              15.2           9.68
#> 11 Female "Non-standard employment: …              11.6           9.02
#> 12 Female "Non-standard employment: …              13.0          10.4 
#> # ℹ 3 more variables: `2nd quartile (median)` <dbl>,
#> #   `3rd quartile` <dbl>, `Number of employees` <dbl>
```
