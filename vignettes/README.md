## Source files for pkgdown articles

These Rmd files define the contents of the pkgdown articles at https://statistikat.github.io/STATcubeR/articles/

### Availability

The articles are not installed as vignettes because of [`.Rbuildignore`](../.Rbuildignore).
Making these articles available as vignettes would require some modifications.
These modifications might be made prior to a CRAN release of `STATcubeR`.
For now, they will only be available in the online documentation.

### Development guide

* Use exactly one line per sentence
* Always include the setup chunk to load [R/](R)
* Use function names as filenames
* Always provide `title`, `desc` and `link_text` in the yaml metadata
* Include new articles in the articles index via [`_pkgdown.yml`](../pkgdown/_pkgdown.yml)
