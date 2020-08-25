
# STATcubeR <img src="man/figures/STATcube_logo.png" align="right" alt="" width="120" />

R Interface für die STATcube REST API

## Setup

Zum erstmaligen Verwenden des Pakets ist es notwendig, das Paket zu
installieren und einen API Schlüssel hinzuzufügen. Eine Anleitung hierzu
kann in dem [Setup
Artikel](http://xlwt0012/rpkgs/dev/STATcubeR/articles/Setup.html)
gefunden werden.

## Anwendungsbeispiel: JSON-Abfrage

Im folgenden Beispiel wird eine Tabelle von STATcube in R exportiert.
Hierzu sind folgende Schritte notwendig.

  - Erstellen einer Tabelle auf STATcube (Tabellenansicht)
  - Herunterladen einer API Abfrage für diese Tabelle. (JSON-Format)
  - Absenden der Abfrage über den R Server
  - Konvertieren der API-Response in ein typisches R Datenformat

### Erstellen einer Tabelle auf STATcube

Verwenden Sie das grafische User-Interface von STATcube um eine Tabelle
zu erstellen. Besuchen Sie hierzu
<http://sdbext:8081/statistik.at/ext/statcube/home>. Womöglich ist eine
Anmeldung mit Windows-Username und Windows-Passwort notwendig.

### Herunterladen der API Abfrage

Laden sie eine “Open Data API Abfrage” über die STATcube GUI herunter.
Wählen Sie hierzu “API Abfrage” als Download-Option. Nun wird eine
json-Datei auf dem Windows-System abgelegt.

![](man/figures/download_json.png)

### Absenden der Abfrage

Geben Sie den Pfad zu dem heruntergeladenen JSON-File in der Funktion
`sc_get_response()` ein, um die API Abfrage auszuführen.

``` r
my_response <- sc_get_response("pfad/zu/api_abfrage.json")
```

Das Objekt `my_response` beinhaltet die “rohe” API-Response. Die
Print-Methode gibt einen Einblick in die Abfrage.

``` r
(json_pfad <- sc_example("bev_seit_1982.json"))
#> /data/home/decill/projects/STATcubeR/inst/json_examples/bev_seit_1982.json
my_response <- sc_get_response(json_pfad)
my_response
#> Objekt der Klasse STATcube_response
#> 
#> Datenbank:     Bevölkerung zu Jahresbeginn ab 1982 
#> Werte:         Fallzahl 
#> Dimensionen:   Jahr, Bundesland, Geburtsland 
#> 
#> Abfrage:       2020-08-17 12:53:43 
#> STATcubeR:     0.1.0
```

### Umwandeln in typische R-Datenformate

Um die importierten Daten in R zu nutzen kann `as.data.frame()` oder
`as.array()` verwendet werden.

``` r
as.array(my_response)
as.data.frame(my_response)
```

Im Falle von `as.data.frame()` wird ein tidy Datensatz zurückgegeben,
der jede Dimension des Cubes als Spalte enthält. Außerdem gibt es zwei
Spalten pro Wert. Beispiel:

``` r
set.seed(1234)
as.data.frame(my_response) %>% dplyr::sample_n(10)
#>        Jahr                Bundesland Geburtsland Fallzahl Fallzahl_a
#> 1  Zusammen              Tirol <AT33>    Zusammen       NA          X
#> 2      2011 Nicht klassifizierbar <0>  Österreich        0           
#> 3      2015         Burgenland <AT11>    Zusammen   288356           
#> 4      2011           Salzburg <AT32>     Ausland    82391           
#> 5      1999 Nicht klassifizierbar <0>     Ausland       NA          X
#> 6      2008           Salzburg <AT32>     Ausland    80421           
#> 7      2000         Steiermark <AT22>    Zusammen       NA          X
#> 8      2009     Oberösterreich <AT31>  Österreich  1238869           
#> 9      1993            Kärnten <AT21>  Österreich       NA          X
#> 10     2008   Niederösterreich <AT12>    Zusammen  1595503
```

Die Spalte `Fallzahl_a` enthält die Anmerkungen (Annotations) zur Spalte
`Fallzahl`. Um die Bedeutungen der Annotations zu sehen kann
`sc_annotation_legend()` verwendet werden.

``` r
sc_annotation_legend(my_response)
#> $Q
#> [1] "STATcube – Statistische Datenbank von STATISTIK AUSTRIA"
#> 
#> $X
#> [1] "Verkreuzung nicht erlaubt"
```

In diesem Fall ist der Nuller in Zeile 2 ein “echter nuller” und der
Wert in Zeile 1 ist `NA` (**N**ot **A**vailable) aufgrund einer
Sperrung.

## Anwendungsbeispiel: Gespeicherte Tabelle

Wenn auf STATcube gespeicherte Tabellen vorhanden sind, können diese
auch ohne eine JSON-Abfrage importiert werden. Alle gespeicherten
Tabellen können mit `sc_saved_tables_list()` aufgelistet werden.

``` r
sc_saved_tables_list()
#>               label                                             id
#> 1 krankenbewegungen str:table:c7902e8d-5165-44e9-b17e-34ae20e2d1d4
#> 2        tourism_ts str:table:eec7dd70-25c4-4e5a-a6ae-1a9cd15d3c4c
#> 3      entlassungen str:table:f63f0713-155f-4d1d-8d41-4a50f0815fc7
```

Anschließend kann die `id` einer gespeicherten Tabelle verwendet werden,
um diese in R zu importieren.

``` r
tourism_ts <- sc_saved_table("str:table:eec7dd70-25c4-4e5a-a6ae-1a9cd15d3c4c")
tourism_ts
#> Objekt der Klasse STATcube_response
#> 
#> Datenbank:     Nächtigungsstatistik ab 1974 nach Saison 
#> Werte:         Übernachtungen 
#> Dimensionen:   Regionale Gliederung [teilw. SPE], Saison/Tourismusmonat, Herkunftsland 
#> 
#> Abfrage:       2020-08-17 13:33:44 
#> STATcubeR:     0.1.0
```

Um die Tabellen für andere Nutzer von `STATcubeR` verfügbar zu machen,
kann die Response wieder als json exportiert werden

``` r
sc_write_json(tourism_ts, "tourism_ts.json")
```

Die dabei generierte JSON-Datei beinhaltet eine API Abfrage, welche mit
`sc_get_response()` verwendet werden kann

``` r
sc_get_response("tourism_ts.json")
```

## Sonstiges

Um den Inhalt einer Response zu erhalten, kann `sc_content()` verwendet
werden. Hierbei wird eine verschachtelte Liste zurückgegeben, welche dem
JSON-Inhalt der API-Response entspricht.

``` r
my_content <- sc_content(my_response)
names(my_content)
#> [1] "query"         "database"      "measures"      "fields"       
#> [5] "cubes"         "annotationMap"
my_content$measures
#> [[1]]
#> [[1]]$uri
#> [1] "str:statfn:debevstandjb:F-BEVSTANDJB:F-ISIS-1:SUM"
#> 
#> [[1]]$label
#> [1] "Fallzahl"
#> 
#> [[1]]$measure
#> [1] "str:measure:debevstandjb:F-BEVSTANDJB:F-ISIS-1"
#> 
#> [[1]]$`function`
#> [1] "SUM"
```

Die Endpoints `/info`, `/schema` und `/rate_limit` sind testweise
angebunden aber noch nicht exportiert.

``` r
STATcubeR:::sc_get_info() %>% httr::content()
STATcubeR:::sc_get_schema() %>% httr::content()
STATcubeR:::sc_get_rate_limit() %>% httr::content()
```

STATcube verfügt über einen Cache. Wenn die selbe Abfrage mehrmals
abgeschickt wird, so wird das Rate-Limit (100 Anfragen pro Stunde) nicht
belastet. Sämtliche Anfragen werden an die externe STATcube Instanz
gesendet. Eine Überlastung der API kann somit die UX von externen
Nutzern negativ beeinflussen.

## TODO

Vorschläge zur Weiterentwicklung des Pakets können in dem [TODO
Artikel](http://xlwt0012/rpkgs/dev/STATcubeR/articles/TODO.html)
nachgelesen werden.

## API Dokumentation

  - `/table` Endpoint:
    <https://docs.wingarc.com.au/superstar/latest/open-data-api/open-data-api-reference/table-endpoint>
  - `/schema` Endpoint:
    <https://docs.wingarc.com.au/superstar/latest/open-data-api/open-data-api-reference/schema-endpoint>
