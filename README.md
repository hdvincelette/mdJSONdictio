
# mdJSONdictio

<!-- badges: start -->
<!-- badges: end -->

Tools to write mdJSON dictionaries that can be imported to mdEditor to create Dictionary records.

## Installation

You can install the development version of mdJSONdictio from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hdvincelette/mdJSONdictio")
```

## Example

``` r
library(mdJSONdictio)
f<- system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
write.mdJSON(file = f, title = "Example Dictionary")
```

