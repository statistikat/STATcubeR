# Recode sc_table objects

A collection of methods that can be used to modify an object of class
sc_table by reference. Typical usage is to access the `recode` binding
of an `sc_table` object and then use method chaining to perform recode
operations.

    x <- od_table("OGD_krebs_ext_KREBS_1")
    x$recode$
      label_field("C-BERJ-0", "de", "JAHR")$
      label_measure("F-KRE", "de", "Anzahl")

See the example section for more details.

## Methods

### Public methods

- [`sc_recoder$new()`](#method-sc_recoder-initialize)

- [`sc_recoder$label_field()`](#method-sc_recoder-label_field)

- [`sc_recoder$label_measure()`](#method-sc_recoder-label_measure)

- [`sc_recoder$level()`](#method-sc_recoder-level)

- [`sc_recoder$total_codes()`](#method-sc_recoder-total_codes)

- [`sc_recoder$visible()`](#method-sc_recoder-visible)

- [`sc_recoder$order()`](#method-sc_recoder-order)

------------------------------------------------------------------------

### `sc_recoder$new()`

Create a new recoder instance. This will automatically be performed
during the setup of `sc_data` objects

#### Usage

    sc_recoder$new(x)

#### Arguments

- `x`:

  the private environment of an `sc_data` object

------------------------------------------------------------------------

### `sc_recoder$label_field()`

Change the label of a field variable

#### Usage

    sc_recoder$label_field(field, language, new)

#### Arguments

- `field`:

  a field code

- `language`:

  a language, "de" or "en"

- `new`:

  the new label

------------------------------------------------------------------------

### `sc_recoder$label_measure()`

Change the label of a measure variable

#### Usage

    sc_recoder$label_measure(measure, language, new)

#### Arguments

- `measure`:

  a measure code

- `language`:

  a language "de" or "en"

- `new`:

  the new label

------------------------------------------------------------------------

### `sc_recoder$level()`

Change the labels of a level

#### Usage

    sc_recoder$level(field, level, language, new)

#### Arguments

- `field`:

  a field code

- `level`:

  a level code for the field

- `language`:

  a language "de" or "en"

- `new`:

  the new label for the level

------------------------------------------------------------------------

### `sc_recoder$total_codes()`

Change the total code for a field

#### Usage

    sc_recoder$total_codes(field, new)

#### Arguments

- `field`:

  a field code

- `new`:

  a level code for the field or `NA`. Will be used as the new total
  code. In case of `NA`, the total code will be unset.

------------------------------------------------------------------------

### `sc_recoder$visible()`

set the visibility of a level. Invisible levels are omitted in the
output of `$tabulate()` but don't affect aggregation

#### Usage

    sc_recoder$visible(field, level, new)

#### Arguments

- `field`:

  a field code

- `level`:

  a level code for the field

- `new`:

  visibility. `TRUE` or `FALSE`

------------------------------------------------------------------------

### `sc_recoder$order()`

set the order of levels.

#### Usage

    sc_recoder$order(field, new)

#### Arguments

- `field`:

  a field code

- `new`:

  the new order. A permutation of all level codes for the field.
  alternatively, an integer vector that defines the permutation.

## Examples

``` r
x <- od_table("OGD_krebs_ext_KREBS_1")

x$recode$
  label_field("C-KRE_GESCHLECHT-0", "en", "SEX")$
  label_measure("F-KRE", "en", "NUMBER")$
  level("C-KRE_GESCHLECHT-0", "GESCHLECHT-1", "en", "MALE")

x$tabulate("C-KRE_GESCHLECHT-0", "F-KRE")
#> # A STATcubeR tibble: 2 x 2
#>   SEX    NUMBER
#> * <fct>   <int>
#> 1 MALE    28337
#> 2 female  27828

earnings <- od_table("OGD_veste309_Veste309_1")
earnings$recode$
  total_codes("C-A11-0", "A11-1")$
  total_codes("C-STAATS-0", "STAATS-9")$
  total_codes("C-VEBDL-0", "VEBDL-10")$
  total_codes("C-BESCHV-0", "BESCHV-1")

earnings$total_codes()
#> # A data frame: 4 × 2
#>   code       total_code
#>   <chr>      <chr>     
#> 1 C-A11-0    A11-1     
#> 2 C-STAATS-0 STAATS-9  
#> 3 C-VEBDL-0  VEBDL-10  
#> 4 C-BESCHV-0 BESCHV-1  

earnings$tabulate("C-STAATS-0")
#> # A STATcubeR tibble: 8 x 6
#>   Citizenship  `Arithmetic mean` `1st quartile` 2nd quartile (median…¹
#> * <fct>                    <dbl>          <dbl>                  <dbl>
#> 1 "Austrian"                18.5          12.4                    15.9
#> 2 "Non-Austri…              14.4           9.72                   12.3
#> 3 "EU-28/EWR/…              15.2          10.0                    12.8
#> 4 "EU-15/EWR …              19.6          11.9                    15.9
#> 5 "EU-13 EU M…              13.0           9.59                   11.9
#> 6 "Former Yug…              12.8           9.58                   12.0
#> 7 "Turkey"                  12.2           9.35                   11.3
#> 8 "Other"                   13.8           9.07                   10.9
#> # ℹ abbreviated name: ¹​`2nd quartile (median)`
#> # ℹ 2 more variables: `3rd quartile` <dbl>,
#> #   `Number of employees` <dbl>
earnings$recode$visible("C-STAATS-0", "STAATS-8", FALSE)
earnings$tabulate("C-STAATS-0")
#> # A STATcubeR tibble: 7 x 6
#>   Citizenship  `Arithmetic mean` `1st quartile` 2nd quartile (median…¹
#> * <fct>                    <dbl>          <dbl>                  <dbl>
#> 1 "Austrian"                18.5          12.4                    15.9
#> 2 "Non-Austri…              14.4           9.72                   12.3
#> 3 "EU-28/EWR/…              15.2          10.0                    12.8
#> 4 "EU-15/EWR …              19.6          11.9                    15.9
#> 5 "EU-13 EU M…              13.0           9.59                   11.9
#> 6 "Former Yug…              12.8           9.58                   12.0
#> 7 "Turkey"                  12.2           9.35                   11.3
#> # ℹ abbreviated name: ¹​`2nd quartile (median)`
#> # ℹ 2 more variables: `3rd quartile` <dbl>,
#> #   `Number of employees` <dbl>

earnings$recode$
  order("C-A11-0", c("A11-3", "A11-1", "A11-2"))
```
