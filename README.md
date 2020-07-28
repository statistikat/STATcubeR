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
* Absenden der Abfrage über den R Server

### Hinzufügen des API Keys

Zum verwenden dieses Pakets muss einmalig ein API-Schlüssel für STATcube
auf den R Server geladen werden. Rufen Sie die Funktion
`statcube_browse_preferences()` auf. Nun öffnet sich ein Browserfenster in
dem der Schlüssel sichtbar ist. Kopieren Sie den Schlüssel in die
Zwischenablage.

```r
statcube_browse_preferences()
```

Rufen Sie nun die Funktion `statcube_token_prompt()` auf. Der Token wird damit
über ein RStudio-Prompt abgefragt.

```r
statcube_token_prompt()
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

