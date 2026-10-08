# Create a table-instance from an open-data dataset

R6 Class open data datasets.

## Super class

[`sc_data`](https://statistikat.github.io/STATcubeR/reference/sc_data.md)
-\> `od_table`

## Active bindings

- `json`:

  parsed version of
  `https://data.statistik.gv.at/ogd/json?dataset=${id}`

- `header`:

  parsed version of
  `https://data.statistik.gv.at/data/${id}_HEADER.csv`.

  Similar contents can be found in `$meta`.

- `resources`:

  lists all files downloaded from the server to construct this table

- `od_server`:

  The server used for initialization (see to
  [`?od_table`](https://statistikat.github.io/STATcubeR/reference/od_table.md))

## Methods

### Public methods

- [`od_table$new()`](#method-od_table-initialize)

- [`od_table$browse()`](#method-od_table-browse)

Inherited methods

- [`sc_data$field()`](https://statistikat.github.io/STATcubeR/html/sc_data.html#method-sc_data-field)
- [`sc_data$tabulate()`](https://statistikat.github.io/STATcubeR/html/sc_data.html#method-sc_data-tabulate)
- [`sc_data$total_codes()`](https://statistikat.github.io/STATcubeR/html/sc_data.html#method-sc_data-total_codes)

------------------------------------------------------------------------

### `od_table$new()`

This class is not exported. Use
[`od_table()`](https://statistikat.github.io/STATcubeR/reference/od_table.md)
to initialize objects of class `od_table`.

#### Usage

    od_table$new(id, language = NULL, server = "ext")

#### Arguments

- `id`:

  the id of the dataset that should be accessed

- `language`:

  language to be used for labeling. `"en"` or `"de"`

- `server`:

  the OGD-Server server to be used

------------------------------------------------------------------------

### `od_table$browse()`

open the metadata for the dataset in a browser

#### Usage

    od_table$browse()
