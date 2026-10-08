# Articles

### STATcube REST API

Import tables from STATcube and use them as `data.frame`s in your R
projects. You can also use the API to discover available databases as
well as their metadata.

- [Set Up the API
  Key](https://statistikat.github.io/STATcubeR/articles/sc_key.md):

  Learn how get access to an API key and set up authentication to the
  STATcube REST API by making the key available for STATcubeR.

- [Get Tables via JSON
  Requests](https://statistikat.github.io/STATcubeR/articles/sc_table.md):

  Hands-on guide about transfering data from the STATcube GUI (table
  view) to R sessions via a the download option “open data api request”.

- [Get Metadata with the Schema
  Endpoint](https://statistikat.github.io/STATcubeR/articles/sc_schema.md):

  The `/schema` endpoint of the REST API provides metadata about
  databases and can also be used to retrieve a catalogue with all
  available databases.

- [Load Saved
  Tables](https://statistikat.github.io/STATcubeR/articles/sc_table_saved.md):

  STATcube allows you to save tables in the GUI. You can import those
  saved tables using
  [`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md).
  This function can also be used to import default tables for databases.

- [Other REST API
  Endpoints](https://statistikat.github.io/STATcubeR/articles/sc_info.md):

  How to use the (rather simple) endpoints `/info`, `/rate_limit_table`
  and `/rate_limit_schema`.

### Open Government Data

Articles about STATcubeR functions that interact with the open
government portal of Statistics Austria
(<https://data.statistik.gv.at>).

- [Open Government
  Data](https://statistikat.github.io/STATcubeR/articles/od_table.md):

  Hands-on guide on how to get and use datasets from the OGD portal.
  This article also contains a short introduction on tabulation and
  translation of datasets.

- [Available
  Datasets](https://statistikat.github.io/STATcubeR/articles/od_list.md):

  A concise overview about the 315 datasets from the OGD portal which
  are compatible with STATcubeR. Learn how to discover useful datasets
  and read OGD metadata.

- [File
  Management](https://statistikat.github.io/STATcubeR/articles/od_resources.md):

  Learn how STATcubeR caches resources from the OGD portal in the local
  file system. Understanding this allows you to enable persistent caches
  and directly use cached resources.

### Data Class

- [The STATcubeR Data
  Class](https://statistikat.github.io/STATcubeR/articles/sc_data.md):

  STATcubeR uses a common base class for OGD datasets and datasets from
  the STATcube REST API. This article explains the core functionalities
  and allows you to use both datasources more flexibly.

- [Tabulating STATcubeR
  Datasets](https://statistikat.github.io/STATcubeR/articles/sc_tabulate.md):

  Learn how to turn datasets from the REST API or from the OGD portal
  into tidy `data.frame`s by using the tabulate() method.

### Advanced Usage

Articles that are only relevant for users who are already familiar with
the core functionalities of STATcubeR.

- [Caching API
  Responses](https://statistikat.github.io/STATcubeR/articles/sc_cache.md):

  STATcubeR can be configured to cache API responses and reuse them
  later. This article describes the necessary setup as well as potential
  usecases.

- [Define Custom
  Tables](https://statistikat.github.io/STATcubeR/articles/sc_table_custom.md):

  Define custom requests against the `/table` endpoint programmatically
  by providing character vectors with ids of databases, measures and
  classification fields.

- [Handling API
  Errors](https://statistikat.github.io/STATcubeR/articles/sc_last_error.md):

  STATcubeR converts all http errors into R errors. This atricle
  explains how this conversion happens and showcases some common error
  messages.
