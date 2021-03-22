# Example requests

A collection of json files that can be sent to the `/table` endpoint of the
**STATcube** REST API. The examples are installed together with the package and
they can be referenced with `sc_example()`.

```r
library(STATcubeR)
table_accomodation <- sc_table(sc_example("accomodation.json"))
```

The tables cover different topics from social statistics, business statistics
and more. The most common categorical variables are years and (NUTS-2) regions.

File                     | Description
------------------------ | ----------------------------------------------------------------------------------
`accomodation`           | accomodation statistics (arrivals, nights spent) by month, establishment type and country of origin, 2000-2021
`agriculture_prices`     | economic accounts for agriculture: 97 items, 1990-2020
`economic_atlas`         | Key data for federal provinces from the economic atlas. Private households, employment rate, accomodation. 1995-2019. https://www.statistik.at/atlas/
`economic_trend_monitor` | monthly data of 63 economic trend measures for industry, construction, trade, services, imports, exports, and more: http://monitor.statistik.at
`gross_regional_product` | Gross regional product by NUTS-2 region, 2000-2011
`labor_force_survey`     | Working hours by gender, education and NUTS-2 region
`population_timeseries`  | Population by NUTS-2 region and country of birth, 1982-2015
`foreign_trade`          | Imports and exports by commodity (CPA) and and activity sector (NACE) 2008-2018

See the [JSON Article](https://statistikat.github.io/STATcubeR/articles/JSON-requests.html) for more details.
