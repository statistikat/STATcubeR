# Example requests

A collection of json files that can be sent to the `/table` endpoint of the
STATcube REST API.

```r
library(STATcubeR)

## accomodation statistics (arrivals, nights spent) by season, establishment
## type and country of origin, 2000-2021
table_accomodation <- sc_table(sc_example("accomodation.json"))

## economic accounts for agriculture: 97 items, 1990-2020
table_agriculture <- sc_table(sc_example("agriculture_prices.json"))

## monthly data of 63 economic trend measures for industry, construction,
## trade, services, imports, exports, and more: http://monitor.statistik.at
table_trends <- sc_table(sc_example("economic_trend_monitor.json"))

## Gross regional product by NUTS-2 region, 2000-2011
table_grp <- sc_table(sc_example("gross_regional_product.json"))

## Working hours by time, gender, education and region
table_lfs <- sc_table(sc_example("labor_force_survey.json"))

## Population by region and country of birth, 1982-2015
table_population <- sc_table(sc_example("population_timeseries.json"))

## Imports and exports by commodity (CPA) and and activity sector (NACE)
## 2008-2018
table_trade <- sc_table(sc_example("foreign_trade.json"))
```

See the [JSON Article](https://statistikat.github.io/STATcubeR/articles/JSON-requests.html) for more details.
