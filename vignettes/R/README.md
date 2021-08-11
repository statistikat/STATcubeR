## Helper functions for pkgdown documentation

These R scripts provide functions for the pkgdown articles.
They are loaded with `setup.R` as an entry point at the beginning of each document.

````
```{r, echo=FALSE}
source("setup.R")$value
```
````

Use exactly like this, otherwise the tooltips might not work.

### df_print.R 

provides better printing of `data.frames` and tibbles using the `fansi` package.

### add_tooltip.R 

provides some functionalities for adding tooltips

````
# tooltip to other articles
See the `r ticle("od_tabulate")`

# tooltip for od_table objects
The `r tooltip_data(od_table_object)` has three measures

# STATcubeR tooltip
Another awesome package: `r STATcubeR`

# OGD tooltip
We will take use data from `ogd_portal` for this example
````
