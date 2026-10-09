# Create a table-instance from an open-data dataset

`od_table(id)` returns an `R6`-class object containing all relevant data
and metadata from https://data.statistik.gv.at/data/

## Usage

``` r
od_table(id, language = NULL, server = "ext")
```

## Arguments

- id:

  the id of the dataset that should be accessed

- language:

  language to be used for labeling. `"en"` or `"de"`

- server:

  the OGD-server to be used. `"ext"` (the default) for the external
  server or `prod` for the production server

## Value

The returned objects is of class `sc_table` and inherits several parsing
methods from
[sc_data](https://statistikat.github.io/STATcubeR/reference/sc_data.md).
See
[od_table_class](https://statistikat.github.io/STATcubeR/reference/od_table_class.md)
for the full class documentation.

## Components

|                 |                                                       |
|-----------------|-------------------------------------------------------|
| **Component**   | **Corresponding File on Server**                      |
| `$data `        | `https://data.statistik.gv.at/data/${id}.csv`         |
| `$header `      | `https://data.statistik.gv.at/data/${id}_HEADER.csv`  |
| `$field(code) ` | `https://data.statistik.gv.at/data/${id}_${code}.csv` |
| `$json `        | `https://data.statistik.gv.at/ogd/json?dataset=${id}` |

## Examples

``` r
x <- od_table("OGD_krebs_ext_KREBS_1")

## metadata
x
#> Cancer statistics by reporting year, province of residence and
#> localisation of cancer
#> 
#> Dataset: OGD_krebs_ext_KREBS_1 (data.statistik.gv.at)
#> Measures: Number of records F-KRE
#> Fields: Tumore ICD/10 3-Steller <98>, Reporting year <42>, Province
#>   of residence <9>, Sex <2>
#> 
#> Request: [2026-10-09 04:52:28.211327]
#> STATcubeR: 1.0.1
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
x$field("Sex")
#> # STATcubeR metadata: 2 x 10
#>   code         label  parsed
#>   <chr>        <chr>  <chr> 
#> 1 GESCHLECHT-1 male   male  
#> 2 GESCHLECHT-2 female female
#> # … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'
x$field(3)
#> # STATcubeR metadata: 9 x 10
#>   code         label           parsed         
#>   <chr>        <chr>           <chr>          
#> 1 BUNDESLAND-1 "Burgenland "   "Burgenland "  
#> 2 BUNDESLAND-2 "Carinthia"     "Carinthia"    
#> 3 BUNDESLAND-3 "Lower Austria" "Lower Austria"
#> 4 BUNDESLAND-4 "Upper Austria" "Upper Austria"
#> 5 BUNDESLAND-5 "Salzburg"      "Salzburg"     
#> 6 BUNDESLAND-6 "Styria"        "Styria"       
#> 7 BUNDESLAND-7 "Tyrol"         "Tyrol"        
#> 8 BUNDESLAND-8 "Vorarlberg"    "Vorarlberg"   
#> 9 BUNDESLAND-9 "Vienna"        "Vienna"       
#> # … with 7 more columns: 'label_de', 'label_en', 'parent', 'de_desc', 'en_desc', 'visible', 'order'

## data
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
x$tabulate()
#> # A STATcubeR tibble: 51,796 x 5
#>    `Tumore ICD/10 3-Steller`   `Reporting year` Province of residenc…¹
#>  * <fct>                       <date>           <fct>                 
#>  1 <C00> Bösartige Neubildung… 1983-01-01       "Burgenland "         
#>  2 <C00> Bösartige Neubildung… 1983-01-01       "Carinthia"           
#>  3 <C00> Bösartige Neubildung… 1983-01-01       "Carinthia"           
#>  4 <C00> Bösartige Neubildung… 1983-01-01       "Lower Austria"       
#>  5 <C00> Bösartige Neubildung… 1983-01-01       "Lower Austria"       
#>  6 <C00> Bösartige Neubildung… 1983-01-01       "Upper Austria"       
#>  7 <C00> Bösartige Neubildung… 1983-01-01       "Upper Austria"       
#>  8 <C00> Bösartige Neubildung… 1983-01-01       "Salzburg"            
#>  9 <C00> Bösartige Neubildung… 1983-01-01       "Styria"              
#> 10 <C00> Bösartige Neubildung… 1983-01-01       "Styria"              
#> # ℹ 51,786 more rows
#> # ℹ abbreviated name: ¹​`Province of residence`
#> # ℹ 2 more variables: Sex <fct>, `Number of records F-KRE` <int>

## tabulation: see `?sc_tabulate` for more examples
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

## switch language
x$language <- "de"
x
#> Krebsstatistik
#> 
#> Dataset: OGD_krebs_ext_KREBS_1 (data.statistik.gv.at)
#> Measures: Anzahl der Datensätze F-KRE
#> Fields: Tumore ICD/10 3-Steller <98>, Berichtsjahr <42>, Bundesland
#>   <9>, Geschlecht <2>
#> 
#> Request: [2026-10-09 04:52:28.211327]
#> STATcubeR: 1.0.1
x$tabulate()
#> # A STATcubeR tibble: 51,796 x 5
#>    `Tumore ICD/10 3-Steller`        Berichtsjahr Bundesland Geschlecht
#>  * <fct>                            <date>       <fct>      <fct>     
#>  1 <C00> Bösartige Neubildung der … 1983-01-01   Burgenland männlich  
#>  2 <C00> Bösartige Neubildung der … 1983-01-01   Kärnten    männlich  
#>  3 <C00> Bösartige Neubildung der … 1983-01-01   Kärnten    weiblich  
#>  4 <C00> Bösartige Neubildung der … 1983-01-01   Niederöst… männlich  
#>  5 <C00> Bösartige Neubildung der … 1983-01-01   Niederöst… weiblich  
#>  6 <C00> Bösartige Neubildung der … 1983-01-01   Oberöster… männlich  
#>  7 <C00> Bösartige Neubildung der … 1983-01-01   Oberöster… weiblich  
#>  8 <C00> Bösartige Neubildung der … 1983-01-01   Salzburg   männlich  
#>  9 <C00> Bösartige Neubildung der … 1983-01-01   Steiermark männlich  
#> 10 <C00> Bösartige Neubildung der … 1983-01-01   Steiermark weiblich  
#> # ℹ 51,786 more rows
#> # ℹ 1 more variable: `Anzahl der Datensätze F-KRE` <int>

## other interesting tables
od_table("OGD_veste309_Veste309_1")
#> Structure of Earnings Survey (SES) 2018 Gross hourly earnings
#> in EUR by citizenship, region (NUTS 2) and form of employment
#> 
#> Dataset: OGD_veste309_Veste309_1 (data.statistik.gv.at)
#> Measures: Arithmetic mean, 1st quartile, 2nd quartile (median), 3rd
#>   quartile, Number of employees
#> Fields: Sex <3>, Citizenship <9>, Region (NUTS2) <10>, Form of
#>   employment <7>
#> 
#> Request: [2026-10-09 04:52:28.476719]
#> STATcubeR: 1.0.1
od_table("OGD_konjunkturmonitor_KonMon_1")
#> Economic Trend Monitor
#> 
#> Dataset: OGD_konjunkturmonitor_KonMon_1 (data.statistik.gv.at)
#> Measures: Production index industry (wd; 2021=100), Technical total
#>   production industry (in 1.000 €), Turnover index industry
#>   (2021=100), Turnover industry (in 1.000 €), Index of new orders
#>   industry (2021=100), Index of persons employed industry (2021=100),
#>   Persons employed industry, Production index industry per employee
#>   (2021=100), Production index industry per hours worked (2021=100),
#>   Industrial output price index (2021=100), … (78 more)
#> Fields: reporting period <302>, value indication <3>
#> 
#> Request: [2026-10-09 04:52:28.51414]
#> STATcubeR: 1.0.1
od_table("OGD_krankenbewegungen_ex_LEISTUNGEN_1")
#> Medical procedures during inpatient stays since 1989 by
#> patient characteristics (number of medical procedures)
#> 
#> Dataset: OGD_krankenbewegungen_ex_LEISTUNGEN_1 (data.statistik.gv.at)
#> Measures: Medical procedures
#> Fields: Year of discharge <32>, Sex <2>, Age (four classes) <4>,
#>   NUTS-2 region (place of residence) <12>, Medical procedures -
#>   subchapters <115>
#> 
#> Request: [2026-10-09 04:52:29.535348]
#> STATcubeR: 1.0.1
od_table("OGD_veste303_Veste203_1")
#> Structure of Earnings Survey (SES) 2018 Gross hourly earnings
#> in EUR by characteristics of the enterprise
#> 
#> Dataset: OGD_veste303_Veste203_1 (data.statistik.gv.at)
#> Measures: Arithmetic mean, 1st quartile, 2nd quartile (median), 3rd
#>   quartile, Number of employees
#> Fields: ÖNACE 2008 (NACE Rev.2) <97>, Sex <3>, Regions (Nuts1) <4>,
#>   Size of the enterprise <6>
#> 
#> Request: [2026-10-09 04:52:34.541486]
#> STATcubeR: 1.0.1
```
