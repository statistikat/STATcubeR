# Tabulating STATcubeR Datasets

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

This article contains the most important aspects of the method
`$tabulate()`. This method aggregates `sc_data` objects. The first part
will use the *cancer dataset* from the [OGD
article](https://statistikat.github.io/STATcubeR/articles/od_table.md).
After that, other features of `$tabulate()` will be demonstrated with
the data from the *structure of earnings survey (SES)*.

``` r

cancer   <- od_table("OGD_krebs_ext_KREBS_1")
earnings <- od_table("OGD_veste309_Veste309_1")
```

Notice that these tabulation methods can also be used with the STATcube
REST API. This means that objects created by
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
also have a `$tabulate()` method.

## Tabulating Data

Calling the `$tabulate()` method with no arguments produces a table with
the same dimensions as `$data`.

``` r

cancer$tabulate()
```

``` r-output
# A STATcubeR tibble: 51,796 x 5
  `Tumore ICD/10 3-Steller`       `Reporting year` Province of residenc…¹ Sex   
* <fct>                           <date>           <fct>                  <fct> 
1 <C00> Bösartige Neubildung der… 1983-01-01       "Burgenland "          male  
2 <C00> Bösartige Neubildung der… 1983-01-01       "Carinthia"            male  
3 <C00> Bösartige Neubildung der… 1983-01-01       "Carinthia"            female
4 <C00> Bösartige Neubildung der… 1983-01-01       "Lower Austria"        male  
5 <C00> Bösartige Neubildung der… 1983-01-01       "Lower Austria"        female
# ℹ 51,791 more rows
# ℹ abbreviated name: ¹​`Province of residence`
# ℹ 1 more variable: `Number of records F-KRE` <int>
```

``` r

identical(dim(cancer$tabulate()), dim(cancer$data))
```

    #> [1] TRUE

Instead of `cancer$tabulate(...)` it is also possible to use
`sc_tabulate(cancer, ...)`. All available parameters for the
`$tabulate()` method are documented in
[`?sc_tabulate`](https://statistikat.github.io/STATcubeR/reference/sc_tabulate.md).

## Aggregation

### Aggregating with sums

To get the number of cases by reporting year and sex, use the labels of
those variables as arguments.

``` r

cancer$tabulate("Reporting year", "Sex")
```

``` r-output
# A STATcubeR tibble: 84 x 3
  `Reporting year` Sex    `Number of records F-KRE`
* <date>           <fct>                      <int>
1 1983-01-01       male                       13625
2 1983-01-01       female                     16702
3 1984-01-01       male                       13996
4 1984-01-01       female                     16622
5 1985-01-01       male                       13656
# ℹ 79 more rows
```

If more than one measure is included in the dataset, all measures will
be aggregated.
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) uses
[`rowsum()`](https://rdrr.io/r/base/rowsum.html) to ensure a good
performance with big datasets. It is also possible to use partial
matching or use codes.

``` r

cancer$tabulate("Reporting", "C-KRE")
```

``` r-output
# A STATcubeR tibble: 84 x 3
  `Reporting year` Sex    `Number of records F-KRE`
* <date>           <fct>                      <int>
1 1983-01-01       male                       13625
2 1983-01-01       female                     16702
3 1984-01-01       male                       13996
4 1984-01-01       female                     16622
5 1985-01-01       male                       13656
# ℹ 79 more rows
```

[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) will use
[`pmatch()`](https://rdrr.io/r/base/pmatch.html) to match the supplied
strings with the metadata to identify the variables that should be used
for aggregation.

### Limitations of sums

In some cases, datasets cannot be aggregated using the
[`rowsum()`](https://rdrr.io/r/base/rowsum.html) approach. As an
example, take the structure of earnings survey.

``` r

earnings <- od_table("OGD_veste309_Veste309_1")
earnings
```

    #> Structure of Earnings Survey (SES) 2018 Gross hourly earnings in EUR
    #> by citizenship, region (NUTS 2) and form of employment
    #> 
    #> Dataset: OGD_veste309_Veste309_1 (data.statistik.gv.at)
    #> Measures: Arithmetic mean, 1st quartile, 2nd quartile (median), 3rd
    #>   quartile, Number of employees
    #> Fields: Sex <3>, Citizenship <9>, Region (NUTS2) <10>, Form of employment
    #>   <7>
    #> 
    #> Request: [2026-10-08 15:00:48.959752]
    #> STATcubeR: 1.0.1

As we can see from the [`print()`](https://rdrr.io/r/base/print.html)
output, the measures contain means and quartiles. Therefore, aggregating
the data via [`rowsum()`](https://rdrr.io/r/base/rowsum.html) is not
meaningful. However, this dataset contains a “total code” for every
field.

``` r

earnings$tabulate()
```

``` r-output
# A STATcubeR tibble: 72 x 9
   Sex       Citizenship `Region (NUTS2)`   `Form of employment`                
 * <fct>     <fct>       <fct>              <fct>                               
 1 Sum total Total       Total              "Total"                             
 2 Sum total Total       Total              "Standard employment "              
 3 Sum total Total       Total              "Non-standard employment (total)"   
 4 Sum total Total       Total              "Non-standard employment: part-time…
 5 Sum total Total       Total              "Non-standard employment: fixed-ter…
 6 Sum total Total       Total              "Non-standard employment: marginal …
 7 Sum total Total       Total              "Non-standard employment: temporary…
 8 Sum total Total       AT11 Burgenland    "Total"                             
 9 Sum total Total       AT12 Lower Austria "Total"                             
10 Sum total Total       AT13 Vienna        "Total"                             
# ℹ 62 more rows
# ℹ 5 more variables: `Arithmetic mean` <dbl>, `1st quartile` <dbl>,
#   `2nd quartile (median)` <dbl>, `3rd quartile` <dbl>,
#   `Number of employees` <dbl>
```

### Aggregating via total codes

These total codes can be used to aggregate the data with `$tabulate()`.
In order to do that, the total codes need to be specified using
`$total_codes()`.

``` r

earnings$total_codes(Sex = "Sum total", Citizenship = "Total",
                     Region = "Total", `Form of employment` = "Total")
```

Now `$tabulate()` will use these total codes to form aggregates of the
data.

``` r

earnings$tabulate("Form of employment")
```

``` r-output
# A STATcubeR tibble: 6 x 6
  `Form of employment`                          `Arithmetic mean` `1st quartile`
* <fct>                                                     <dbl>          <dbl>
1 "Standard employment "                                     19.0           12.8
2 "Non-standard employment (total)"                          15.2           10.2
3 "Non-standard employment: part-time only"                  15.9           10.7
4 "Non-standard employment: fixed-term employm…              16.8           10.2
5 "Non-standard employment: marginal part-time…              12              9  
6 "Non-standard employment: temporary agency w…              14.0           11.5
# ℹ 3 more variables: `2nd quartile (median)` <dbl>, `3rd quartile` <dbl>,
#   `Number of employees` <dbl>
```

As we can see, the method extracted rows 2 to 7 from the data. The logic
for selecting those rows is equivalent to the following
[dplyr](https://dplyr.tidyverse.org) expression.

``` r

earnings$data %>% dplyr::filter(Sex == "Sum total" & Citizenship == "Total" &
  `Region (NUTS2)` == "Total" & `Form of employment` != "Total") %>%
  dplyr::select(-Sex, -Citizenship, -`Region (NUTS2)`)
```

The `$tabulate()` method also works with more than one variable.

#### 

- Sex & Form of employment
- Sex & Citizenship
- Sex & Region
- Citizenship & Region

``` r

earnings$tabulate("Sex", "Form of employment")
```

``` r-output
# A STATcubeR tibble: 12 x 7
   Sex    `Form of employment`                  `Arithmetic mean` `1st quartile`
 * <fct>  <fct>                                             <dbl>          <dbl>
 1 Male   "Standard employment "                             20.0          13.5 
 2 Male   "Non-standard employment (total)"                  16.5          10.5 
 3 Male   "Non-standard employment: part-time …              18.8          11.1 
 4 Male   "Non-standard employment: fixed-term…              18.3          10.9 
 5 Male   "Non-standard employment: marginal p…              12.6           9   
 6 Male   "Non-standard employment: temporary …              14.4          12   
 7 Female "Standard employment "                             16.3          11.1 
 8 Female "Non-standard employment (total)"                  14.6          10.0 
 9 Female "Non-standard employment: part-time …              15.2          10.6 
10 Female "Non-standard employment: fixed-term…              15.2           9.68
11 Female "Non-standard employment: marginal p…              11.6           9.02
12 Female "Non-standard employment: temporary …              13.0          10.4 
# ℹ 3 more variables: `2nd quartile (median)` <dbl>, `3rd quartile` <dbl>,
#   `Number of employees` <dbl>
```

``` r

earnings$tabulate("Sex", "Citizenship")
```

``` r-output
# A STATcubeR tibble: 16 x 7
   Sex    Citizenship                           `Arithmetic mean` `1st quartile`
 * <fct>  <fct>                                             <dbl>          <dbl>
 1 Male   "Austrian"                                         20.4          13.8 
 2 Male   "Non-Austrian "                                    15.3          10.6 
 3 Male   "EU-28/EWR/Switzerland (without Aust…              16.0          10.9 
 4 Male   "EU-15/EWR before 2004/Switzerland"                21.2          12.6 
 5 Male   "EU-13 EU Member States after 2004"                13.6          10.3 
 6 Male   "Former Yugoslavia (without Slovenia…              14.1          11.2 
 7 Male   "Turkey"                                           13.3          10.4 
 8 Male   "Other"                                            14.0           9.07
 9 Female "Austrian"                                         15.9          11.1 
10 Female "Non-Austrian "                                    13.0           9.19
11 Female "EU-28/EWR/Switzerland (without Aust…              13.7           9.38
12 Female "EU-15/EWR before 2004/Switzerland"                17.1          11.0 
# ℹ 4 more rows
# ℹ 3 more variables: `2nd quartile (median)` <dbl>, `3rd quartile` <dbl>,
#   `Number of employees` <dbl>
```

``` r

earnings$tabulate("Sex", "Region")
```

``` r-output
# A STATcubeR tibble: 18 x 7
   Sex    `Region (NUTS2)`   `Arithmetic mean` `1st quartile`
 * <fct>  <fct>                          <dbl>          <dbl>
 1 Male   AT11 Burgenland                 16.6           11.6
 2 Male   AT12 Lower Austria              17.8           12.3
 3 Male   AT13 Vienna                     20.9           12.2
 4 Male   AT21 Carinthia                  18.3           12.6
 5 Male   AT22 Styria                     18.4           12.8
 6 Male   AT31 Upper Austria              19.7           13.7
 7 Male   AT32 Salzburg                   19.2           12.7
 8 Male   AT33 Tyrol                      18.6           12.8
 9 Male   AT34 Vorarlberg                 20.4           14.5
10 Female AT11 Burgenland                 14.5           10.2
11 Female AT12 Lower Austria              14.2           10  
12 Female AT13 Vienna                     17.1           10.6
# ℹ 6 more rows
# ℹ 3 more variables: `2nd quartile (median)` <dbl>, `3rd quartile` <dbl>,
#   `Number of employees` <dbl>
```

``` r

earnings$tabulate("Citizenship", "Region")
```

``` r-output
# A STATcubeR tibble: 0 x 7
# ℹ 7 variables: Citizenship <fct>, Region (NUTS2) <fct>,
#   Arithmetic mean <dbl>, 1st quartile <dbl>, 2nd quartile (median) <dbl>,
#   3rd quartile <dbl>, Number of employees <dbl>
```

We get an empty table because this cross tabulation is not included in
the OGD dataset. The same will happen for Citizenship & Form of
employment as well as Region & Form of employment.

``` r

earnings$tabulate("Citizenship", "Form of employment") %>% dim()
```

    #> [1] 0 7

``` r

earnings$tabulate("Region", "Form of employment") %>% dim()
```

    #> [1] 0 7

### Totals and the REST API

By default,
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md) will
always add totals for datasets from the REST API and use those totals to
aggregate the datasets.

``` r

x <- sc_table(sc_example("accomodation"))
x$meta$fields
```

``` r-output
# STATcubeR metadata: 3 x 7
  code          label                      total_code nitems type        
  <chr>         <chr>                      <chr>       <int> <chr>       
1 C-SDB_TIT-0   Season/Tourism Month       SC_TOTAL      323 Time (month)
2 C-C93SUM-0    Country of origin          SC_TOTAL        4 Category    
3 C-BBTR_REG2-0 Accomodation establishment SC_TOTAL        4 Category    
# … with 2 more columns: 'label_de', 'label_en'
```

### Including totals in the output

It is not necessary that all fields have totals. For example, suppose we
want to include the totals for `Sex` in the output table. We can just
remove the total code before running
[`sc_tabulate()`](https://statistikat.github.io/STATcubeR/reference/sc_tabulate.md).
The special symbol `NA` can be used to unset a total code.

``` r

earnings$total_codes(Sex = NA)
earnings$tabulate("Sex")
```

``` r-output
# A STATcubeR tibble: 3 x 6
  Sex       `Arithmetic mean` `1st quartile` `2nd quartile (median)`
* <fct>                 <dbl>          <dbl>                   <dbl>
1 Sum total              17.6           11.6                    15.1
2 Male                   19.2           12.8                    16.3
3 Female                 15.3           10.4                    13.4
# ℹ 2 more variables: `3rd quartile` <dbl>, `Number of employees` <dbl>
```

## German Labels and Codes

It is possible to switch the language used for labeling the data. This
can be done by setting `$language` to `"de"` or `"en"`.

``` r

earnings$language <- "de"
earnings$tabulate("Geschlecht")
```

``` r-output
# A STATcubeR tibble: 3 x 6
  Geschlecht `Arithmetisches Mittel` `1. Quartil` `2. Quartil (Median)`
* <fct>                        <dbl>        <dbl>                 <dbl>
1 insgesamt                     17.6         11.6                  15.1
2 männlich                      19.2         12.8                  16.3
3 weiblich                      15.3         10.4                  13.4
# ℹ 2 more variables: `3. Quartil` <dbl>, `Zahl d unselbst Beschäftigten` <dbl>
```

To skip labeling altogether and use variable codes in the output, use
`raw=TRUE`.

``` r

earnings$tabulate("Geschlecht", raw = TRUE)
```

``` r-output
# A STATcubeR tibble: 3 x 6
  `C-A11-0` `F-VESTE_AM` `F-VESTE_Q25` `F-VESTE_Q50` `F-VESTE_Q75` `F-VESTE_UB`
* <fct>            <dbl>         <dbl>         <dbl>         <dbl>        <dbl>
1 A11-1             17.6          11.6          15.1          20.1      2650938
2 A11-2             19.2          12.8          16.3          22.0      1544588
3 A11-3             15.3          10.4          13.4          17.6      1106350
```

Switching languages is always available for
[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
objects. For
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
it depends on which languages were requested.

``` r

# default: get labels in German and English
x <- sc_table(sc_example("accomodation"))
# only get English labels
x <- sc_table(sc_example("accomodation"), lang = "en")
# only get German labels
x <- sc_table(sc_example("accomodation"), lang = "de")
```

## Subsetting columns

In the previous examples, we only supplied names and/or codes of fields
to
[`sc_tabulate()`](https://statistikat.github.io/STATcubeR/reference/sc_tabulate.md).
It is also possible to include measures in which case the unlisted
measures will be omitted.

``` r

earnings$tabulate("Geschlecht", "Arithmetisches Mittel", "2. Quartil")
```

``` r-output
# A STATcubeR tibble: 3 x 3
  Geschlecht `Arithmetisches Mittel` `2. Quartil (Median)`
* <fct>                        <dbl>                 <dbl>
1 insgesamt                     17.6                  15.1
2 männlich                      19.2                  16.3
3 weiblich                      15.3                  13.4
```

Just like for fields, measures also support partial matching and codes.
In the above example, `"2. Quartil"` was matched to
`"2. Quartil (Median)"`.

## Programmatic usage

Notice that we used the German label for the column `"Sex"` in the last
calls to [`tabulate()`](https://rdrr.io/r/base/tabulate.html). This is
necessary because only the “active” labels are available to define the
tabulation. If you want to use
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md)
programmatically, always use codes to define the tabulation and also use
the `.list` parameter if you want to pass several codes.

``` r

earnings$field("C-A11-0")
```

``` r-output
# STATcubeR metadata: 3 x 10
  code  label     parsed   
  <chr> <chr>     <chr>    
1 A11-1 insgesamt insgesamt
2 A11-2 männlich  männlich 
3 A11-3 weiblich  weiblich 
# … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
```

``` r

earnings$total_codes(`C-A11-0` = "A11-1")
vars_to_tabulate <- c("C-A11-0", "C-BESCHV-0")
earnings$tabulate(.list = vars_to_tabulate)
```

``` r-output
# A STATcubeR tibble: 12 x 7
  Geschlecht Form des Beschäftigungsverhäl…¹ Arithmetisches Mitte…² `1. Quartil`
* <fct>      <fct>                                            <dbl>        <dbl>
1 männlich   "Normalarbeitsverhältnis"                         20.0         13.5
2 männlich   "Atypische Beschäftigung (insg…                   16.5         10.5
3 männlich   "Atypisch: ausschließlich Teil…                   18.8         11.1
4 männlich   "Atypisch: befristete Beschäft…                   18.3         10.9
5 männlich   "Atypisch: geringfügige Beschä…                   12.6          9  
6 männlich   "Atypisch: Leih- und Zeitarbei…                   14.4         12  
7 weiblich   "Normalarbeitsverhältnis"                         16.3         11.1
# ℹ 5 more rows
# ℹ abbreviated names: ¹​`Form des Beschäftigungsverhältnisses`,
#   ²​`Arithmetisches Mittel`
# ℹ 3 more variables: `2. Quartil (Median)` <dbl>, `3. Quartil` <dbl>,
#   `Zahl d unselbst Beschäftigten` <dbl>
```

`$total_codes()` currently uses an ellipsis (`...`) parameter to define
total codes. In the future, programmatic updates of `sc_data` objects
should be defined in `$recodes`. See
[\#17](https://github.com/statistikat/STATcubeR/issues/17).
