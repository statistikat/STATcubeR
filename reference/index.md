# Package index

## Open Government Data

Get resources from the open govenment data portal from Statistics
Austria. See the [OGD
Article](https://statistikat.github.io/STATcubeR/articles/od_table.md)
for a hands-on documentation.

- [`od_cache_summary()`](https://statistikat.github.io/STATcubeR/reference/od_cache.md)
  [`od_downloads()`](https://statistikat.github.io/STATcubeR/reference/od_cache.md)
  : Cache management for Open Data
- [`od_catalogue()`](https://statistikat.github.io/STATcubeR/reference/od_catalogue.md)
  : Get a catalogue for OGD datasets
- [`od_list()`](https://statistikat.github.io/STATcubeR/reference/od_list.md)
  : List available Opendata datasets
- [`od_cache_dir()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
  [`od_cache_clear()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
  [`od_cache_file()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
  [`od_resource()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
  [`od_json()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
  [`od_resource_all()`](https://statistikat.github.io/STATcubeR/reference/od_resource.md)
  : Resource management for open.data
- [`od_revisions()`](https://statistikat.github.io/STATcubeR/reference/od_revisions.md)
  : Get OGD revisions
- [`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
  : Create a table-instance from an open-data dataset
- [`od_table_save()`](https://statistikat.github.io/STATcubeR/reference/od_table_save.md)
  [`od_table_local()`](https://statistikat.github.io/STATcubeR/reference/od_table_save.md)
  : Saves/load opendata datasets via tar archives
- [`sc_recoder`](https://statistikat.github.io/STATcubeR/reference/sc_recoder.md)
  : Recode sc_table objects
- [`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
  [`sc_recode()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
  : Create custom tables

## STATcube REST API

Get resources from the REAST API of STATcube. See the [API key
article](https://statistikat.github.io/STATcubeR/articles/sc_key.md) for
instructions about the API Key and the [json requests
article](https://statistikat.github.io/STATcubeR/articles/sc_table.md)
for a hands-on documentation.

- [`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  [`sc_examples_list()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  [`sc_example()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  [`sc_table_saved_list()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  [`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
  : Create a request against the /table endpoint
- [`sc_key()`](https://statistikat.github.io/STATcubeR/reference/sc_key.md)
  [`sc_key_set()`](https://statistikat.github.io/STATcubeR/reference/sc_key.md)
  [`sc_key_get()`](https://statistikat.github.io/STATcubeR/reference/sc_key.md)
  [`sc_key_prompt()`](https://statistikat.github.io/STATcubeR/reference/sc_key.md)
  [`sc_key_exists()`](https://statistikat.github.io/STATcubeR/reference/sc_key.md)
  [`sc_key_valid()`](https://statistikat.github.io/STATcubeR/reference/sc_key.md)
  : Manage your API Keys
- [`sc_schema()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  [`print(`*`<sc_schema>`*`)`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  [`sc_schema_flatten()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  [`sc_schema_catalogue()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  [`sc_schema_db()`](https://statistikat.github.io/STATcubeR/reference/sc_schema.md)
  : Create a request against the /schema endpoint
- [`sc_info()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md)
  [`sc_rate_limit_table()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md)
  [`sc_rate_limit_schema()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md)
  [`sc_rate_limits()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md)
  : Other endpoints of the STATcube REST API
- [`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
  [`sc_recode()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)
  : Create custom tables

## General

Other functionalities including the [STATcubeR data
class](https://statistikat.github.io/STATcubeR/articles/sc_data.md).

- [`sc_data`](https://statistikat.github.io/STATcubeR/reference/sc_data.md)
  : Common interface for STATcubeR datasets
- [`sc_browse()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md)
  [`sc_browse_preferences()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md)
  [`sc_browse_table()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md)
  [`sc_browse_database()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md)
  [`sc_browse_catalogue()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md)
  [`sc_browse_ogd()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md)
  : Links to important 'STATcube' and 'OGD' pages
- [`sc_tabulate()`](https://statistikat.github.io/STATcubeR/reference/sc_tabulate.md)
  : Turn sc_data objects into tidy data frames
- [`sc_json_get_server()`](https://statistikat.github.io/STATcubeR/reference/sc_json_get_server.md)
  : Get the server from a json request
- [`sc_last_error()`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md)
  [`sc_last_error_parsed()`](https://statistikat.github.io/STATcubeR/reference/sc_last_error.md)
  : Error handling for the STATcube REST API
- [`sc_cache_enable()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
  [`sc_cache_disable()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
  [`sc_cache_enabled()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
  [`sc_cache_dir()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
  [`sc_cache_files()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
  [`sc_cache_clear()`](https://statistikat.github.io/STATcubeR/reference/sc_cache.md)
  : Cache responses from the STATcube REST API
- [`sdmx_table()`](https://statistikat.github.io/STATcubeR/reference/sdmx_table.md)
  : Import data from SDMX
