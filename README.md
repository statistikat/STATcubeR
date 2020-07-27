# STATcubeR

R Interface für die STATcube API

## Installation

``` r
rinstSTAT::install_statbucket("meth/STATcubeR")
```

## Verwendung

Zur Verwendung diese Paketes sind zwei Schritte notwendig

* Hinzufügen des API Keys zum authSTAT-Vault
* Absenden einer Abfrage über ein JSON-File

### Hinzufügen des API Keys

Rufen Sie die Funktion `api_token()` auf. Wenn noch kein Token hinzugefügt
wurde wird dieser über ein RStudio-Prompt abgefragt.

```r
library(STATcubeR)
statcube_token()
```

Alternativ kann der Token auch als Parameter übergeben werden

```r
statcube_token(new_token)
```

Der Token kann über die STATcube GUI unter "Benutzerkonto" abgefragt werden

![](man/figures/get_key.png)

### API Abfrage

Laden sie eine "Open Data API Abfrage" über die STATcube GUI herunter.
Anschließend können Sie den Pfad zu dem heruntergeladenen JSON-File in
der Funktion `get_statcube_table()` verwenden.

``` r
library(STATcubeR)
my_table <- get_statcube_table("pfad/zu/api_abfrage.json")
```

![](man/figures/download_json.png)

## TODO

* Parser für die API Response schreiben

