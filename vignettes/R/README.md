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
See the `r ticle("sc_tabulate")` for more details

# tooltip for od_table objects
We can see that `r tippy_dataset(od_table("OGD_krebs_ext_KREBS_1"))` has four fields

# STATcubeR tooltip
Initially, I was using `r STATcubeR` but later I switched to MS-paint

# OGD tooltip
We will take use data from `ogd_portal` for this example
````
