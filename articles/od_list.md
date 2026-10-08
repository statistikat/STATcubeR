# Available Datasets

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

At the time of writing this article, there are 315 datasets that are
assumed to be compatible with
[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md).
This list is not updated regularly, so to get the most recent list, a
call to od_list() will return the current list.

## Interactive overview

Since some of the metadata contained in the OGD JSON files is only
available in German, the following overview uses German labels. Click on
the individual table cells to get more information.

## CLI usage

To get a simplified version of this summary, use the
[`od_list()`](https://statistikat.github.io/STATcubeR/reference/od_list.md)
function. It uses webscraping techniques to get dataset ids and German
labels based on the contents of
<https://data.statistik.gv.at/web/catalog.jsp>.

``` r

all_datasets <- od_list()
all_datasets
```

``` r-output
# A tibble: 471 × 6
   category                                  id                                
   <chr>                                     <chr>                             
 1 Hochwertige Datensätze / HighValueDataset OGD_skesvg2010indikat_HVD_NFSK_IN…
 2 Hochwertige Datensätze / HighValueDataset OGD_oeff_fin_Oeff_Fin_1           
 3 Hochwertige Datensätze / HighValueDataset OGD_kons_brv_q_HVD_KONS_BRV_Q_1   
 4 Hochwertige Datensätze / HighValueDataset OGD_kons_brv_HVD_KONS_BRV_1       
 5 Hochwertige Datensätze / HighValueDataset OGD_vgr108_VGR_HA_vj_1            
 6 Hochwertige Datensätze / HighValueDataset OGD_vgr109_VGR_Erwerb_1           
 7 Hochwertige Datensätze / HighValueDataset OGD_vgr107_VGR_HA_BAI_2           
 8 Hochwertige Datensätze / HighValueDataset OGD_vgr105_VGR_HA_Bws_2           
 9 Hochwertige Datensätze / HighValueDataset OGD_vgr101_VGRJahresR_3           
10 Hochwertige Datensätze / HighValueDataset OGD_hvpi25_HVD_HVPI_2025_1        
# ℹ 461 more rows
# ℹ 4 more variables: label <chr>, date <date>, csv_link <chr>, json_link <chr>
```

## Overview via json

If you identify an interesting dataset, consider downloading the
metadata json to get more details. The json contains links to further
metadata including a link to
[data.statistik.gv.at](https://data.statistik.gv.at).

``` r

(id <- all_datasets$id[2])
```

    #> [1] "OGD_oeff_fin_Oeff_Fin_1"

``` r

json <- od_json(id)
json
```

    #> Öffentliche Finanzen ab 1995, ESVG 2010
    #> 
    #> Erstellung der Nichtfinanziellen Konten des Sektors Staat
    #> (Jahresrechnung)
    #> 
    #> Measures: P.2 Vorleistungen <101> in Mio.EUR, D.11 Bruttolöhne und Gehälter
    #>   <102> in Mio.EUR, D.121 Tatsächliche Sozialbeiträge der Arbeitgeber <103>
    #>   in Mio.EUR, D.122 Unterstellte Sozialbeiträge der Arbeitgeber <104> in
    #>   Mio.EUR, D.21 Gütersteuern <105> in Mio.EUR, D.29 Sonstige
    #>   Produktionsabgaben <106> in Mio.EUR, D.31 Gütersubventionen <107> in
    #>   Mio.EUR, D.39 Sonstige Subventionen <108> in Mio.EUR, D.41 Zinsen <109> in
    #>   Mio.EUR, D.421 Ausschüttungen <110> in Mio.EUR, … (78 more)
    #> Fields: Zeit, Sektor Staat
    #> Updated: 2026-09-30 09:22:39
    #> Tags: HighValueDataset, Sektor, Staat
    #> Categories: Finanzen und Rechnungswesen

This output is generated from
[`OGD_oeff_fin_Oeff_Fin_1.json`](https://data.statistik.gv.at/ogd/json?dataset=OGD_oeff_fin_Oeff_Fin_1)
and shows a summary of the available metadata. Other parts of the
metadata can be extracted with `$` using the keys from the json
specification.

``` r

json$extras$update_frequency
```

    #> [1] "jährlich"

## Showcase

- Population
- Hospitalizations
- Earnings
- Household forecast
- Gross regional product

The *population dataset* measures the Austrian population for 2116
different regions.

``` r

od_table("OGD_bevstandjbab2002_BevStand_2020")$tabulate()
```

``` r-output
# A STATcubeR tibble: 391,956 x 5
   `Time section` Sex   Commune (aggregation by p…¹ `Age in single years` Number
 * <date>         <fct> <fct>                       <fct>                  <int>
 1 2020-01-01     male  Eisenstadt <10101>          under 1 year old          77
 2 2020-01-01     male  Eisenstadt <10101>          1 year old                75
 3 2020-01-01     male  Eisenstadt <10101>          2 years old               70
 4 2020-01-01     male  Eisenstadt <10101>          3 years old               83
 5 2020-01-01     male  Eisenstadt <10101>          4 years old               67
 6 2020-01-01     male  Eisenstadt <10101>          5 years old               56
 7 2020-01-01     male  Eisenstadt <10101>          6 years old               75
 8 2020-01-01     male  Eisenstadt <10101>          7 years old               73
 9 2020-01-01     male  Eisenstadt <10101>          8 years old               74
10 2020-01-01     male  Eisenstadt <10101>          9 years old               86
# ℹ 391,946 more rows
# ℹ abbreviated name: ¹​`Commune (aggregation by political district)`
```

The *hospitalizations dataset* is a timeseries from 2009 to 2019 for 115
different medical procedures.

``` r

od_table("OGD_krankenbewegungen_ex_LEISTUNGEN_1")$tabulate()
```

``` r-output
# A STATcubeR tibble: 91,898 x 6
   `Year of discharge` Sex   `Age (four classes)` NUTS-2 region (place of resi…¹
 * <date>              <fct> <fct>                <fct>                         
 1 2009-01-01          male  Up to 14 years old   Non-Austria                   
 2 2009-01-01          male  Up to 14 years old   Non-Austria                   
 3 2009-01-01          male  Up to 14 years old   Non-Austria                   
 4 2009-01-01          male  Up to 14 years old   Non-Austria                   
 5 2009-01-01          male  Up to 14 years old   Non-Austria                   
 6 2009-01-01          male  Up to 14 years old   Non-Austria                   
 7 2009-01-01          male  Up to 14 years old   Non-Austria                   
 8 2009-01-01          male  Up to 14 years old   Non-Austria                   
 9 2009-01-01          male  Up to 14 years old   Non-Austria                   
10 2009-01-01          male  Up to 14 years old   Non-Austria                   
# ℹ 91,888 more rows
# ℹ abbreviated name: ¹​`NUTS-2 region (place of residence)`
# ℹ 2 more variables: `Medical procedures - subchapters` <fct>,
#   `Medical procedures` <int>
```

The *structure of earnings dataset* showcases average earnings by four
different classifications. See the [tabulation
article](https://statistikat.github.io/STATcubeR/articles/sc_tabulate.md)
for some usage examples with this dataset.

``` r

od_table("OGD_veste309_Veste309_1")$tabulate()
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

The *household forecast* contains predictions about the number of
private households by 4 household characteristics from 2011 to 2080.

``` r

od_table(dat_name)$tabulate()
```

``` r-output
# A STATcubeR tibble: 630 x 4
   Time       `Province (NUTS 2-digit) <9>` Private households at the end of t…¹
 * <date>     <fct>                                                        <int>
 1 2011-01-01 Burgenland <AT11>                                           117588
 2 2011-01-01 Carinthia <AT21>                                            241461
 3 2011-01-01 Lower Austria <AT12>                                        682380
 4 2011-01-01 Upper Austria <AT31>                                        593029
 5 2011-01-01 Salzburg <AT32>                                             224629
 6 2011-01-01 Styria <AT22>                                               515258
 7 2011-01-01 Tyrol <AT33>                                                299024
 8 2011-01-01 Vorarlberg <AT34>                                           152948
 9 2011-01-01 Vienna <AT13>                                               843181
10 2012-01-01 Burgenland <AT11>                                           118776
# ℹ 620 more rows
# ℹ abbreviated name: ¹​`Private households at the end of the year`
# ℹ 1 more variable: `Annual average of private households` <int>
```

The *GRP dataset* contains GRP for all NUTS-3 regions between 2000 and
2019.

``` r

od_table("OGD_vgrrgr104_RGR104_1")$tabulate()
```

``` r-output
# A STATcubeR tibble: 1,214 x 7
   Time       `NUTS-3`                        Gross regional product; current …¹
 * <date>     <fct>                                                        <dbl>
 1 2000-01-01 Mittelburgenland <AT111>                                       572
 2 2000-01-01 Nordburgenland <AT112>                                        2600
 3 2000-01-01 Südburgenland <AT113>                                         1728
 4 2000-01-01 Mostviertel-Eisenwurzen <AT121>                               4558
 5 2000-01-01 Niederösterreich-Süd <AT122>                                  4827
 6 2000-01-01 Sankt Pölten <AT123>                                          3783
 7 2000-01-01 Waldviertel <AT124>                                           4101
 8 2000-01-01 Weinviertel <AT125>                                           1722
 9 2000-01-01 Wiener Umland-Nordteil <AT126>                                5097
10 2000-01-01 Wiener Umland-Südteil <AT127>                                 9553
# ℹ 1,204 more rows
# ℹ abbreviated name: ¹​`Gross regional product; current prices in million Euro`
# ℹ 4 more variables: `Gross regional product per inhabitant` <dbl>,
#   `Gross regional product per person employed` <dbl>,
#   `Change in % to previous year prices` <dbl>,
#   `Veränderung des BRP pro Kopf auf Basis von Vorjahrespreisen (in %)` <dbl>
```
