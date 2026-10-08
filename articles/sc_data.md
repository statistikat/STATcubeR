# The STATcubeR Data Class

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

The class
[sc_data](https://statistikat.github.io/STATcubeR/reference/sc_data.md)
defines a common interface for open data datasets and responses from the
`/table` endpoint of the STATcube REST API. It defines methods that are
applicable to both datasources like acquiring metadata, labeling the
data and aggregating results.

## Constructing sc_data objects

The
[sc_data](https://statistikat.github.io/STATcubeR/reference/sc_data.md)
class itself is not exported in
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md).
Therefore, objects of the class should be created with one of the
following functions

- [`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
  obtains data from the OGD portal. See the [OGD
  article](https://statistikat.github.io/STATcubeR/articles/od_table.md)
- [`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  creates a request against the table endpoint of the STATcube REST API.
  See the [JSON requests
  article](https://statistikat.github.io/STATcubeR/articles/sc_table.md)
  - [`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
    and
    [`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
    also use the `/table` endpoint. However, the request is specified
    via ids rather than a json file.

To illustrate, we will use one of the OGD datasets to showcase the
functionality of this class. Notice however, that objects created with
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
can be used interchangeably.

``` r

x <- od_table("OGD_krebs_ext_KREBS_1")
```

## Data

The data from the table can be extracted using the active binding
`$data`. Notice how *OGD_krebs_ext_KREBS_1* only includes codes and
possibly some totals. The data is always provided in a long format with
one column for each field and one column for each measure.

``` r

x$data
```

``` r-output
# A STATcubeR tibble: 51,796 x 5
  `C-TUM_ICD10_3ST-0` `C-BERJ-0` `C-BUNDESLAND-0` `C-KRE_GESCHLECHT-0` `F-KRE`
* <fct>               <fct>      <fct>            <fct>                  <int>
1 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-1     GESCHLECHT-1               2
2 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-2     GESCHLECHT-1               8
3 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-2     GESCHLECHT-2               2
4 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-3     GESCHLECHT-1               6
5 TUM_ICD10_3ST-C00   BERJ-1983  BUNDESLAND-3     GESCHLECHT-2               2
# ℹ 51,791 more rows
```

It will be explained how labeled data can be obtained in the
**Tabulation** section.

## Metadata

Metadata for a
[sc_data](https://statistikat.github.io/STATcubeR/reference/sc_data.md)
object includes labels and other information that is relevant to
correctly parse the raw data. The active binding `$meta` contains at
least the entries `$source`, `$measures` and `$fields`.

### Source

The source field contains information about the data source. The most
important entries are `code` and `label`.

``` r

x$meta$source
```

``` r-output
# STATcubeR metadata: 1 x 7
  code                  label                                              lang 
  <chr>                 <chr>                                              <chr>
1 OGD_krebs_ext_KREBS_1 Cancer statistics by reporting year, province of … en   
# … with 4 more columns: 'label_de', 'label_en', 'requested', 'scr_version'
```

On the bottom, we see that additional information about the source is
available, namely `label_en`, `label_de`, etc. These additional metadata
entries might not be available for `sc_table` objects.

### Measures

This part of the metadata is a `data.frame` with one row for each
measure. It contains codes and labels as well as the number of NAs found
in `$data` for that particular column.

``` r

x$meta$measures
```

``` r-output
# STATcubeR metadata: 1 x 7
  code  label                     NAs
  <chr> <chr>                   <int>
1 F-KRE Number of records F-KRE     0
# … with 4 more columns: 'label_de', 'label_en', 'de_desc', 'en_desc'
```

### Fields

The fields entry summarizes all classification fields i.e. categorical
variables. It includes the codes and labels as well as the total code
registered for the particular field.

``` r

x$meta$fields
```

``` r-output
# STATcubeR metadata: 4 x 9
  code               label                   total_code nitems type       
  <chr>              <chr>                   <chr>       <int> <chr>      
1 C-TUM_ICD10_3ST-0  Tumore ICD/10 3-Steller NA             98 Category   
2 C-BERJ-0           Reporting year          NA             42 Time (year)
3 C-BUNDESLAND-0     Province of residence   NA              9 Category   
4 C-KRE_GESCHLECHT-0 Sex                     NA              2 Category   
# … with 4 more columns: 'label_de', 'label_en', 'de_desc', 'en_desc'
```

## Field information

To get more info about specific fields, use the `$field()` method. This
will return all classification elements as a `data.frame`.

### 

- Tumor types
- Year
- Province
- Sex

``` r

x$field("Tumore")
```

``` r-output
# STATcubeR metadata: 98 x 10
  code              label                                                       
  <chr>             <chr>                                                       
1 TUM_ICD10_3ST-C00 <C00> Bösartige Neubildung der Lippe                        
2 TUM_ICD10_3ST-C01 <C01> Bösartige Neubildung des Zungengrundes                
3 TUM_ICD10_3ST-C02 <C02> Bösartige Neubildung sonstiger und nicht näher bezeic…
4 TUM_ICD10_3ST-C03 <C03> Bösartige Neubildung des Zahnfleisches                
5 TUM_ICD10_3ST-C04 <C04> Bösartige Neubildung des Mundbodens                   
# ℹ 93 more rows
# ℹ 1 more variable: parsed <chr>
# … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
```

``` r

x$field("Reporting year")
```

``` r-output
# STATcubeR metadata: 42 x 10
  code      label parsed    
  <chr>     <chr> <date>    
1 BERJ-1983 1983  1983-01-01
2 BERJ-1984 1984  1984-01-01
3 BERJ-1985 1985  1985-01-01
4 BERJ-1986 1986  1986-01-01
5 BERJ-1987 1987  1987-01-01
# ℹ 37 more rows
# … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
```

``` r

x$field("Province")
```

``` r-output
# STATcubeR metadata: 9 x 10
  code         label           parsed         
  <chr>        <chr>           <chr>          
1 BUNDESLAND-1 "Burgenland "   "Burgenland "  
2 BUNDESLAND-2 "Carinthia"     "Carinthia"    
3 BUNDESLAND-3 "Lower Austria" "Lower Austria"
4 BUNDESLAND-4 "Upper Austria" "Upper Austria"
5 BUNDESLAND-5 "Salzburg"      "Salzburg"     
6 BUNDESLAND-6 "Styria"        "Styria"       
7 BUNDESLAND-7 "Tyrol"         "Tyrol"        
8 BUNDESLAND-8 "Vorarlberg"    "Vorarlberg"   
9 BUNDESLAND-9 "Vienna"        "Vienna"       
# … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
```

``` r

x$field("Sex")
```

``` r-output
# STATcubeR metadata: 2 x 10
  code         label  parsed
  <chr>        <chr>  <chr> 
1 GESCHLECHT-1 male   male  
2 GESCHLECHT-2 female female
# … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
```

## Tabulation

The method `$tabulate()` can be used to turn `sc_table` objects into
tidy data.frames. See the [tabulation
article](https://statistikat.github.io/STATcubeR/articles/sc_tabulate.md)
for more defaults.

``` r

x$tabulate()
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
