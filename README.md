# STATcubeR

R Interface für die STATcube API

## Installation

Falls die Portal-Credentials am R Server hinterlegt sind, kann über 
`install_statbucket()` installiert werden.

``` r
rinstSTAT::install_statbucket("METH/STATcubeR")
library(STATcubeR)
```

Sonst ist auch eine Installation mit `git clone`/`R CMD INSTALL` möglich

```
git clone ssh://git@gitrepo:7999/METH/STATcubeR.git
R CMD INSTALL STATcubeR
```

## Verwendung

Zur Verwendung diese Paketes sind folgende Schritte notwendig

* (einmalig) Hinzufügen des API Keys zum authSTAT-Vault
* Herunterladen einer API Abfrage von STATcube. (JSON-Format)
* Absenden der Abfrage über den R Server
* Konvertieren der API-Response in ein typisches R Datenformat

### Hinzufügen des API Keys

Zum verwenden dieses Pakets muss einmalig ein API-Schlüssel für STATcube
auf den R Server geladen werden. Rufen Sie die Funktion
`statcube_browse_preferences()` auf. Nun öffnet sich ein Browserfenster in
dem der Schlüssel sichtbar ist. Eventuell muss davor Windows-User und
Windows-Passwort eingegeben werden. Kopieren Sie den Schlüssel in die
Zwischenablage.

```r
statcube_browse_preferences()
```

Rufen Sie nun die Funktion `statcube_token_set()` auf. Ersetzen Sie dabei
`"XXXX"` durch den vorhin kopierten Token.

```r
statcube_token_set("XXXX")
#> STATcube Key wurde erfolgreich getestet und im Vault hinterlegt
```

### Herunterladen der API Abfrage

Laden sie eine "Open Data API Abfrage" über die STATcube GUI herunter. Erstellen
Sie hierzu eine STATcube Tabelle und wählen Sie "API Abfrage" als
Download-Option.

![](man/figures/download_json.png)

Nun wird eine json-Datei auf dem Windows-System abgelegt

### Importieren in R

Geben Sie den Pfad zu dem heruntergeladenen JSON-File in
der Funktion `get_statcube_response()` ein, um die API Abfrage auszuführen.

``` r
my_response <- get_statcube_response("pfad/zu/api_abfrage.json")
```

Alternativ kann mit der Funktion `upload_json()` die json-Datei über einen
Upload-Dialog angegeben werden.

```r
my_response <- upload_json()
```

Das Objekt `my_response` beinhaltet die "rohe" API-Response. Die Print-Methode
gibt einen Einblick in die Abfrage.

```r
my_response
#> Objekt der Klasse STATcube_response
#> 
#> Datenbank:    Bevölkerungsbewegung 1961 bis 2100 
#> Werte:        Fälle
#> Dimensionen:  Bewegungsarten, Bundesland, Jahr (1961-2100) 
```

### Umwandeln in "R-freundliche" Datenformate

Um die importierten Daten in R zu nutzen kann `as.data.frame()` oder
`as.array()` verwendet werden.

```r
as.array(my_response)
as.data.frame(my_response)
```

Im Falle von `as.data.frame` wird ein tidy Datensatz zurückgegeben, der jede
Dimension des Cubes als Spalte enthält. Außerdem gibt es eine Spalte pro
Wert. Beispiel:

```r
as.data.frame(my_response)[235:245, ]
#>     Bewegungsarten              Bundesland Jahr (1961-2100)      Fälle
#> 235    Sterbefälle Niederösterreich <AT12>             2020      17721
#> 236    Sterbefälle Niederösterreich <AT12>             2030      19058
#> 237    Sterbefälle   Oberösterreich <AT31>             2000      11850
#> 238    Sterbefälle   Oberösterreich <AT31>             2010      12427
#> 239    Sterbefälle   Oberösterreich <AT31>             2020      13757
#> 240    Sterbefälle   Oberösterreich <AT31>             2030      14657
#> 241    Sterbefälle         Salzburg <AT32>             2000       4176
#> 242    Sterbefälle         Salzburg <AT32>             2010       4162
#> 243    Sterbefälle         Salzburg <AT32>             2020       4739
#> 244    Sterbefälle         Salzburg <AT32>             2030       5248
#> 245    Sterbefälle       Steiermark <AT22>             2000      11599
```

### Sonstiges

Um den Inhalt der Response zu erhalten, kann `httr::content` verwendet werden

```r
httr::content(my_response$response)
```

## TODO

* Echtes Beispiel mit einem JSON, das schöne Ergebnisse liefert
   * Dieses JSON als "vorzeigebeispiel" mit dem Paket installieren
* Erklären der Begriffe Datenbank, Werte und Dimensionen
* Verwenden von `content$cubes${value_key}$annotations`
* Verwenden der URIs, um Variablen auch als Codes (nicht Labels) anzubieten.
    * `content$fields[[i]]$items[[j]]$uris[[1]]`
    * `content$fields[[i]]$uri`
    * `content$database$uri`
    * `content$measures[[i]]$uri`
* Fehlermeldung, falls `response$status_code != 200`
