# Saves/load opendata datasets via tar archives

`od_table_save()` creates a tar archive containing all relevant data
from the OGD portal. `od_table_local()` parses the tar archive and
recreates the `od_table` object.

## Usage

``` r
od_table_save(x, file = NULL)

od_table_local(file)
```

## Arguments

- x:

  an object of class `od_table`

- file:

  An archive file file for the dataset. For `od_table_save()`, the
  default is `{id}.tar.gz` where `id` denotes the OGD identifier.

## Value

- for `od_table_save()`: the path to the generated file

- for `od_table_local()`: the OGD identifier

## Examples

``` r
x <- od_table("OGD_krebs_ext_KREBS_1")

# save dataset as an archive
archive <- file.path(tempdir(), "table.tar.gz")
od_table_save(x, archive)

# read the saved archive
x2 <- od_table_local(archive)

# cleanup
file.remove(archive)
#> [1] TRUE
```
