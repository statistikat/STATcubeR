# Set Up the API Key

    ## ✔ Key could be verified via a test request

    ## ℹ The provided key will be available for this R session

    ## ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    ##   persistently. Replace `XXXX` with your key

Instructions for the first-time use of the STATcube REST API. See also
the [wingarc
docs](https://docs.wingarc.com.au/superstar/9.12/open-data-api/api-keys)
for the vendor documentation on API keys.

## Adding an API key

In order to use the REST API of STATcube it is necessary to provide an
API key. This means that you will need a subscription for STATcube.
Subscribed users will automatically get access to the STATcube API. The
API key can be displayed under the [account
preferences](https://portal.statistik.at/statistik.at/ext/statcube/jsf/preferences/editPreferences.xhtml)
[^1] of STATcube.

To make the key available for
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md), use the
function
[`sc_key_set()`](https://statistikat.github.io/STATcubeR/reference/sc_key.md).
Replace `"XXXX"` with your key.

``` r

sc_key_set("XXXX")
```

    #> ℹ The provided key will be available for this R session
    #> ℹ Add `STATCUBE_KEY_EXT = XXXX` to "~/.Renviron" to set the key
    #>   persistently. Replace `XXXX` with your key

As suggested by the code output, the key will be used for any API
requests that happen in the same R session. You can set the key
persistently by modifying `.Renviron`. This can be done easily with
`usethis::edit_r_environ()`.

By default,
[`sc_key_set()`](https://statistikat.github.io/STATcubeR/reference/sc_key.md)
will perform a test request to verify that the key is valid and throw an
error if the test request is unsuccessful.

``` r

sc_key_set('wrong key')
#> Error in sc_key_set("wrong key") : The key could not be verified
#> Run sc_last_error() or read the online documentation for more details
```

## Next Steps

See the [JSON requests
article](https://statistikat.github.io/STATcubeR/articles/sc_table.md)
to see how tables can be defined in the STATcube GUI and imported into
R. Alternatively, import a saved table by following the [saved tables
article](https://statistikat.github.io/STATcubeR/articles/sc_table_saved.md).
For more advanced usage of
[STATcubeR](https://statistikat.github.io/STATcubeR/index.md), see the
[schema
article](https://statistikat.github.io/STATcubeR/articles/sc_schema.md)
and [custom tables
article](https://statistikat.github.io/STATcubeR/articles/sc_table_custom.md)

## For employees of Statistics Austria

Employees should use one of our internal STATcube servers for API
requests. Please refer to the following internal confluence
documentation for more information on how to get started:

<https://statportal.statistik.local/statistik.at/confluence/x/uDiTAQ>

[^1]: The link to the preferences will only work if you are already
    logged in into STATcube
