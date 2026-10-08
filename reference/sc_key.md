# Manage your API Keys

Functions to get/set the STATcube API keys and make them available for
calls against the STATcube API.

## Usage

``` r
sc_key(server = "ext", test = FALSE)

sc_key_set(key, server = "ext", test = TRUE)

sc_key_get(server = "ext")

sc_key_prompt(server = "ext", test = TRUE)

sc_key_exists(server = "ext")

sc_key_valid(key = NULL, server = "ext")
```

## Arguments

- server:

  A STATcube API server. Defaults to the external Server via `"ext"`.
  Other options are `"red"` for the editing server and `"prod"` for the
  production server. External users should always use the default option
  `"ext"`.

- test:

  Use `sc_key_valid()` to verify the key? If the key is invalid, an
  error is returned and the key will not be set or updated.

- key:

  (`string`) An API key. To display your key, call
  [`sc_browse_preferences()`](https://statistikat.github.io/STATcubeR/reference/sc_browse.md).

## Value

All functions return the key (invisibly) except for `sc_key_exists()`
and `sc_key_valid()`, which return a
[`logical()`](https://rdrr.io/r/base/logical.html) of length one.

## Functions

- `sc_key()`: forwards to `sc_key_get()` if the key is already present.
  Otherwise, `sc_key_prompt()` will be invoked.

- `sc_key_set()`: can be used to pass the key as a parameter (`string`)

- `sc_key_get()`: returns the key, if it exists. Otherwise, an error is
  thrown.

- `sc_key_prompt()`: prompts for a key via
  [`readline()`](https://rdrr.io/r/base/readline.html)

- `sc_key_exists()`: returns `TRUE` if a key was set and `FALSE`
  otherwise.

- `sc_key_valid()`: performs a test request and returns `TRUE` if the
  key is valid and `FALSE` otherwise.
