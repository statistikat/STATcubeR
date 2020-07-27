# STATcubeR

R Interface für die STATcube API

## Installation

``` r
rinstSTAT::install_statbucket("meth/STATcubeR")
```

## Verwendung

Zur Verwendung diese Paketes sind zwei Schritte notwendig

* (einmalig) Hinzufügen des API Keys zum authSTAT-Vault
* Herunterladen einer API Abfrage von STATcube. (JSON-Format)
* Absenden der Abfrage über das JSON-File

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

### Herunterladen der API Abfrage

Laden sie eine "Open Data API Abfrage" über die STATcube GUI herunter.

![](man/figures/download_json.png)

Dabei wird eine json-Datei auf dem Windows-System abgelegt

### Importieren in R

Geben Sie den Pfad zu dem heruntergeladenen JSON-File in
der Funktion `get_statcube_table()` ein, um die API Abfrage auszuführen.

``` r
library(STATcubeR)
my_table <- get_statcube_table("pfad/zu/api_abfrage.json")
```

Alternativ kann mit der Funktion `upload_json()` die json-Datei über einen
Upload-Dialog angegeben werden.

```r
my_table <- upload_json()
```

## TODO

* Parser für die API Response schreiben

