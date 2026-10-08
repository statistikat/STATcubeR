# Links to important 'STATcube' and 'OGD' pages

A collection of links, to browse important 'STATcube' pages.

## Usage

``` r
sc_browse(server = "ext")

sc_browse_preferences(server = "ext")

sc_browse_table(table, server = "ext")

sc_browse_database(database, server = NULL, open = FALSE)

sc_browse_catalogue(server = "ext")

sc_browse_ogd()
```

## Arguments

- server:

  A STATcube API server. Defaults to the external Server via `"ext"`.
  Other options are `"red"` for the editing server and `"prod"` for the
  production server. External users should always use the default option
  `"ext"`.

- table:

  a table id

- database:

  a database id

- open:

  If `FALSE` (the default), open the infopage for the database.
  Otherwise, open the table view.

## Value

the URL of a specific webpage which is opened by default in a web
browser.

## Functions

- `sc_browse()`: opens the home menu of 'STATcube'

- `sc_browse_preferences()`: opens the preference menu with the API key

- `sc_browse_table()`: shows the info page for a table

- `sc_browse_database()`: shows the info page for a database

- `sc_browse_catalogue()`: shows the data catalogue explorer

- `sc_browse_ogd()`: shows the landing page for OGD datasets

## Examples

``` r
sc_browse()
#> https://portal.statistik.at/statistik.at/ext/statcube/home
sc_browse_preferences()
#> https://portal.statistik.at/statistik.at/ext/statcube/jsf/preferences/editPreferences.xhtml
sc_browse_table('defaulttable_deake005')
#> https://portal.statistik.at/statistik.at/ext/statcube/openinfopage?tableId=defaulttable_deake005
sc_browse_database('deake005')
#> https://portal.statistik.at/statistik.at/ext/statcube/openinfopage?id=deake005
sc_browse_catalogue()
#> https://portal.statistik.at/statistik.at/ext/statcube/jsf/dataCatalogueExplorer.xhtml
sc_browse_ogd()
#> https://data.statistik.gv.at/
```
