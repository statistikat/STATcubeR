# Class for /table responses

R6 Class for all responses of the /table endpoint of the 'STATcube' REST
API.

## Super class

[`sc_data`](https://statistikat.github.io/STATcubeR/reference/sc_data.md)
-\> `sc_table`

## Active bindings

- `response`:

  the httr response

- `raw`:

  the raw response content

- `annotation_legend`:

  list of all annotations occurring in the data as a `data.frame` with
  two columns for the annotation keys and annotation labels.

- `rate_limit`:

  how much requests were left after the POST request for this table was
  sent? Uses the same format as
  [`sc_rate_limit_table()`](https://statistikat.github.io/STATcubeR/reference/other_endpoints.md).

- `json`:

  an object of class `sc_json` based the json file used in the request

## Methods

### Public methods

- [`sc_table$new()`](#method-sc_table-initialize)

- [`sc_table$update()`](#method-sc_table-update)

- [`sc_table$tabulate()`](#method-sc_table-tabulate)

- [`sc_table$browse()`](#method-sc_table-browse)

- [`sc_table$add_language()`](#method-sc_table-add_language)

Inherited methods

- [`sc_data$field()`](https://statistikat.github.io/STATcubeR/html/sc_data.html#method-sc_data-field)
- [`sc_data$total_codes()`](https://statistikat.github.io/STATcubeR/html/sc_data.html#method-sc_data-total_codes)

------------------------------------------------------------------------

### `sc_table$new()`

Usually, objects of class `sc_table` are generated with one of the
factory methods
[`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
[`sc_table_saved()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md)
or
[`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md).
If this constructor is invoked directly, either omit the parameters
`json` and `file` or make sure that they match with `response`.

#### Usage

    sc_table$new(response, json = NULL, file = NULL, add_totals = FALSE)

#### Arguments

- `response`:

  a response from
  [`httr::POST()`](https://httr.r-lib.org/reference/POST.html) against
  the /table endpoint.

- `json`:

  the json file used in the request as a string.

- `file`:

  the file path to the json file

- `add_totals`:

  was the json request modified by adding totals via the add_totals
  parameter in one of the factory functions
  ([`sc_table()`](https://statistikat.github.io/STATcubeR/reference/sc_table.md),
  [`sc_table_custom()`](https://statistikat.github.io/STATcubeR/reference/sc_table_custom.md)).
  Necessary, in order to also request totals via the `$add_language()`
  method.

------------------------------------------------------------------------

### `sc_table$update()`

Update the data by re-sending the json to the API. This is still
experimental and could break the object in case new levels were added to
one of the fields. For example, if a new entry is added to a timeseries

#### Usage

    sc_table$update()

------------------------------------------------------------------------

### `sc_table$tabulate()`

An extension of
[`sc_tabulate()`](https://statistikat.github.io/STATcubeR/reference/sc_tabulate.md)
with additional parameters.

#### Usage

    sc_table$tabulate(
      ...,
      round = FALSE,
      annotations = FALSE,
      recode_zeros = FALSE
    )

#### Arguments

- `...`:

  Parameters which are passed down to
  [`sc_tabulate()`](https://statistikat.github.io/STATcubeR/reference/sc_tabulate.md)

- `round`:

  apply rounding to each measure according to the precision provided by
  the API.

- `annotations`:

  Include separate annotation columns in the returned table. This
  parameter is currently broken and needs to be re-implemented

- `recode_zeros`:

  interpret zero values as missings?

------------------------------------------------------------------------

### `sc_table$browse()`

open the dataset in a browser

#### Usage

    sc_table$browse()

------------------------------------------------------------------------

### `sc_table$add_language()`

add a second language to the dataset

#### Usage

    sc_table$add_language(language = NULL, key = NULL)

#### Arguments

- `language`:

  a language to add. `"en"` or `"de"`.

- `key`:

  an API key
