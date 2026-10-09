# Open Government Data

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
makes it easy to import datasets from
[data.statistik.gv.at](https://data.statistik.gv.at) into your R
sessions. This function downloads csv sources from the fileserver. This
means that no API key is required to use STATcubeR with datasets from
the OGD portal.

In this example, we will use a *data set about cancer statistics*. The
dataset id `"OGD_krebs_ext_KREBS_1"` can be extracted from the url and
will be used in the data import.

## Import and overview

To import a dataset, provide the dataset id as an argument.

``` r

table <- od_table("OGD_krebs_ext_KREBS_1")
```

This returns an object of class
[`od_table`](https://statistikat.github.io/STATcubeR/reference/od_table_class.md),
which bundles all the data from the OGD portal that corresponds to this
dataset. Printing the object will show a summary of the contents.

``` r

table
```

    #> Cancer statistics by reporting year, province of residence and
    #> localisation of cancer
    #> 
    #> Dataset: OGD_krebs_ext_KREBS_1 (data.statistik.gv.at)
    #> Measures: Number of records F-KRE
    #> Fields: Tumore ICD/10 3-Steller <98>, Reporting year <42>, Province of
    #>   residence <9>, Sex <2>
    #> 
    #> Request: [2026-10-09 04:55:40.145163]
    #> STATcubeR: 1.0.1

The dataset contains the number of cancer patients by several
classification fields

- **tumor type** differentiates **\<95\>** types of cancers
- The **reporting period** spans **\<37\>** years (1983 to 2019).
- The **regional** variable contains the **\<9\>** NUTS-2 regions of
  Austria.
- The **demographic** variable “Sex” is reported with **\<2\>** levels

## Convert to a data frame

The method `$tabulate()` can be used to turn the object into a
`data.frame` in long format, which contains labeled data.

``` r

table$tabulate()
```

``` r-output
# A STATcubeR tibble: 51,796 x 5
   `Tumore ICD/10 3-Steller`      `Reporting year` Province of residenc…¹ Sex   
 * <fct>                          <date>           <fct>                  <fct> 
 1 <C00> Bösartige Neubildung de… 1983-01-01       "Burgenland "          male  
 2 <C00> Bösartige Neubildung de… 1983-01-01       "Carinthia"            male  
 3 <C00> Bösartige Neubildung de… 1983-01-01       "Carinthia"            female
 4 <C00> Bösartige Neubildung de… 1983-01-01       "Lower Austria"        male  
 5 <C00> Bösartige Neubildung de… 1983-01-01       "Lower Austria"        female
 6 <C00> Bösartige Neubildung de… 1983-01-01       "Upper Austria"        male  
 7 <C00> Bösartige Neubildung de… 1983-01-01       "Upper Austria"        female
 8 <C00> Bösartige Neubildung de… 1983-01-01       "Salzburg"             male  
 9 <C00> Bösartige Neubildung de… 1983-01-01       "Styria"               male  
10 <C00> Bösartige Neubildung de… 1983-01-01       "Styria"               female
# ℹ 51,786 more rows
# ℹ abbreviated name: ¹​`Province of residence`
# ℹ 1 more variable: `Number of records F-KRE` <int>
```

The dataset contains 51796 rows. If every combination of tumor type,
year, region and sex would contain a separate row the number of rows
would be the following.

``` math
95\times37\times9\times2 = 63270
```

This means that the table is fairly dense. But this might not be the
case for other OGD datasets.

## Metadata

This section will show the different metadata components contained in
the table object and how they relate to the resources on the OGD server.

``` r

table$resources$name
#> [1] "OGD_krebs_ext_KREBS_1.json"                  
#> [2] "OGD_krebs_ext_KREBS_1.csv"                   
#> [3] "OGD_krebs_ext_KREBS_1_HEADER.csv"            
#> [4] "OGD_krebs_ext_KREBS_1_C-TUM_ICD10_3ST-0.csv" 
#> [5] "OGD_krebs_ext_KREBS_1_C-BERJ-0.csv"          
#> [6] "OGD_krebs_ext_KREBS_1_C-BUNDESLAND-0.csv"    
#> [7] "OGD_krebs_ext_KREBS_1_C-KRE_GESCHLECHT-0.csv"
#> attr(,"class")
#> [1] "ogd_file"  "character"
```

### Header

The labels for the columns of the data.frame representation are
generated from
[`OGD_krebs_ext_KREBS_1_HEADER.csv`](https://data.statistik.gv.at/data/OGD_krebs_ext_KREBS_1_HEADER.csv)
and can be extracted from the table object via `$header`.

``` r

table$header
```

``` r-output
# STATcubeR metadata: 5 x 6
  code               label label_de                    label_en               
  <chr>              <chr> <chr>                       <chr>                  
1 F-KRE              NA    Anzahl der Datensätze F-KRE Number of records F-KRE
2 C-TUM_ICD10_3ST-0  NA    Tumore ICD/10 3-Steller     NA                     
3 C-BERJ-0           NA    Berichtsjahr                Reporting year         
4 C-BUNDESLAND-0     NA    Bundesland                  Province of residence  
5 C-KRE_GESCHLECHT-0 NA    Geschlecht                  Sex                    
# … with 2 more columns: 'de_desc', 'en_desc'
```

Additional metadata for the columns can be obtained via `$meta`. See the
[STATcubeR data
article](https://statistikat.github.io/STATcubeR/articles/sc_data.md)
for more details.

### Field infos

The method `table$field()` can be used to get information about specific
classification fields. These contain data from
`{dataset_id}_{field_code}.csv`. Unlike the metadata in `sc_table`, the
`od_table` class always contains German and English labels. Both can be
used to label the dataset.

- Tumor type
- Year
- Province
- Sex

The following call gives access to the German and English labels for the
95 different tumor types in the `"cancer type"` classification. Click
`"Year"` above to see information about the years.

``` r

table$field("C-TUM_ICD10_3ST-0")
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

[`OGD_krebs_ext_KREBS_1_C-TUM_ICD10_3ST-0.csv`](https://data.statistik.gv.at/data/OGD_krebs_ext_KREBS_1_C-TUM_ICD10_3ST-0.csv)

The reporting period spans 37 years (1983 to 2019). The classification
elements are parsed into a `<date>` format for the `<data.frame>`
representation.

``` r

table$field("C-BERJ-0")
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

[`OGD_krebs_ext_KREBS_1_C-BERJ-0.csv`](https://data.statistik.gv.at/data/OGD_krebs_ext_KREBS_1_C-BERJ-0.csv)

The regional classification contains 9 elements which correspond to the
NUTS2 regions (“Bundesländer”) of Austria.

``` r

table$field("C-BUNDESLAND-0")
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
# ℹ 4 more rows
# … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
```

[`OGD_krebs_ext_KREBS_1_C-BUNDESLAND-0.csv`](https://data.statistik.gv.at/data/OGD_krebs_ext_KREBS_1_C-BUNDESLAND-0.csv)

Sex is coded as a dichotomous variable with the classification elements
`"male"` and `"female"`.

``` r

table$field("C-KRE_GESCHLECHT-0")
```

``` r-output
# STATcubeR metadata: 2 x 10
  code         label  parsed
  <chr>        <chr>  <chr> 
1 GESCHLECHT-1 male   male  
2 GESCHLECHT-2 female female
# … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
```

[`OGD_krebs_ext_KREBS_1_C-KRE_GESCHLECHT-0.csv`](https://data.statistik.gv.at/data/OGD_krebs_ext_KREBS_1_C-KRE_GESCHLECHT-0.csv)

### json Metadata

The json metadata file
[`OGD_krebs_ext_KREBS_1.json`](https://data.statistik.gv.at/ogd/json?dataset=OGD_krebs_ext_KREBS_1)
is available via the `$json` binding.

- Cancer
- Earnings
- Economic Trend Monitor

``` r

table$json
```

    #> Krebsstatistik
    #> 
    #> Krebsstatistik nach Krebslokalisation (ICD10), Geschlecht und
    #> Wohnbundesland
    #> 
    #> Measures: Anzahl der Datensätze F-KRE
    #> Fields: Tumore ICD/10 3-Steller, Berichtsjahr, Bundesland, Geschlecht
    #> Updated: 2026-01-29 09:43:15
    #> Tags: Krebsstatistik, Krebslokalisation-ICD10, Geschlecht, Wohnbundesland
    #> Categories: Gesundheit

``` r

od_json("OGD_veste309_Veste309_1")
```

    #> Verdienststrukturerhebung 2018 Bruttostundenverdienste in EUR nach
    #> Staatsangehörigkeit, Bundesland und Beschäftigungsverhältnis
    #> 
    #> Verdienststruktur nach Geschlecht, Staatsangehörigkeit, Bundesland
    #> und Beschäftigungsverhältnis
    #> 
    #> Measures: Arithmetisches Mittel, 1. Quartil, 2. Quartil (Median), 3.
    #>   Quartil, Zahl d unselbst Beschäftigten
    #> Fields: Geschlecht, Staatsangehörigkeit, Bundesland (NUTS 2), Form des
    #>   Beschäftigungsverhältnisses
    #> Updated: 2022-03-24 11:29:48
    #> Tags: Staatsangehörigkeit, Bundesland, Beschäftigungsverhältnis
    #> Categories: Arbeit, Bevölkerung

``` r

od_json("OGD_konjunkturmonitor_KonMon_1")
```

    #> Konjunkturmonitor
    #> 
    #> Measures: Produktionsindex Industrie (at; 2021=100), Technische
    #>   Gesamtproduktion Industrie in Tsd. € (KJE), Umsatzindex Industrie
    #>   (2021=100), Umsatz Industrie inTsd.€ (KJE), Auftragseingangsindex
    #>   Industrie (2021=100), Beschäftigtenindex Industrie (2021=100),
    #>   Beschäftigte Industrie gesamt (KJE), Produktionsindex Industrie je
    #>   unselbständig Beschäftigtem (2021=100), Produktionsindex Industrie je
    #>   geleisteter Arbeitsstunde (2021=100), Erzeugerpreisindex für den
    #>   Produzierenden Bereich (2021=100; NACE B-E), … (78 more)
    #> Fields: Berichtszeitraum, Wertangabe
    #> Updated: 2026-10-06 10:52:45
    #> Tags: Konjunkturdaten
    #> Categories: Wirtschaft und Tourismus, Arbeit, Bevölkerung, Finanzen und
    #>   Rechnungswesen

### 

This print method only shows part of the metadata. More information can
be extracted by using the keys of the json object.

``` r

table$json$extras$publisher
#> [1] "Statistik Austria"
table$json$extras$update_frequency
#> [1] "jährlich"
table$json$resources[[1]]$url
#> [1] "https://data.statistik.gv.at/data/OGD_krebs_ext_KREBS_1.csv"
```

## Table Contents

To get the raw microdata from
[`OGD_krebs_ext_KREBS_1.csv`](https://data.statistik.gv.at/data/OGD_krebs_ext_KREBS_1.csv)
, use `table$data`. The output is similar to what is returned from
`read.csv2("OGD_krebs_ext_KREBS_1.csv")`.

``` r

table$data
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

[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
makes sure that the levels of all factor columns are in the same order
as in the metadata.

``` r

levels(table$data$`C-BUNDESLAND-0`) == table$field("C-BUNDESLAND-0")$code
```

    #> [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

As mentioned above, a labeled version of the data can be obtained via
`table$tabulate()`. The labeling is done by taking the raw dataset and
then joining the labels from `$header` and `$field()`.

``` r

table$tabulate()
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

Time variables are converted into a `<date>` format if they satisfy
certain STATcube standards. You can read more about `$tabulate()` in the
[tabulation
article](https://statistikat.github.io/STATcubeR/articles/sc_tabulate.md).

## A Trip to Germany

It is possible to switch the language used for labeling the dataset
using the `$language` field. This field can be used to get and set the
language. Allowed options are `"en"` for English and `"de"` for German.

``` r

table$language
#> [1] "en"
table$language <- "de"
table$language
#> [1] "de"
```

This option affects the [`print()`](https://rdrr.io/r/base/print.html)
method as well as the output of `$tabulate()`. If no English labels are
available, the German labels are used as a fallback mechanism.

``` r

table
```

    #> Krebsstatistik
    #> 
    #> Dataset: OGD_krebs_ext_KREBS_1 (data.statistik.gv.at)
    #> Measures: Anzahl der Datensätze F-KRE
    #> Fields: Tumore ICD/10 3-Steller <98>, Berichtsjahr <42>, Bundesland <9>,
    #>   Geschlecht <2>
    #> 
    #> Request: [2026-10-09 04:55:40.145163]
    #> STATcubeR: 1.0.1

``` r

table$tabulate()
```

``` r-output
# A STATcubeR tibble: 51,796 x 5
  `Tumore ICD/10 3-Steller`            Berichtsjahr Bundesland       Geschlecht
* <fct>                                <date>       <fct>            <fct>     
1 <C00> Bösartige Neubildung der Lippe 1983-01-01   Burgenland       männlich  
2 <C00> Bösartige Neubildung der Lippe 1983-01-01   Kärnten          männlich  
3 <C00> Bösartige Neubildung der Lippe 1983-01-01   Kärnten          weiblich  
4 <C00> Bösartige Neubildung der Lippe 1983-01-01   Niederösterreich männlich  
5 <C00> Bösartige Neubildung der Lippe 1983-01-01   Niederösterreich weiblich  
# ℹ 51,791 more rows
# ℹ 1 more variable: `Anzahl der Datensätze F-KRE` <int>
```

## Further reading

- See the [available datasets
  article](https://statistikat.github.io/STATcubeR/articles/od_list.md)
  to list all datasets that are compatible with
  [`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md).
- Open data datasets often contain a large amount of rows. Check out the
  [tabulation
  article](https://statistikat.github.io/STATcubeR/articles/sc_tabulate.md)
  to see how they can be summarized into a more compact form.
- [STATcubeR](https://statistikat.github.io/STATcubeR/index.md) caches
  all files requested from the server under the hood. The [caching
  article](https://statistikat.github.io/STATcubeR/articles/od_resources.md)
  explains where and how those caches are stored.
