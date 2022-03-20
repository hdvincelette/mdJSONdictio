
# mdJSONdictio

<!-- badges: start -->
[![Travis build status](https://travis-ci.com/hdvincelette/mdJSONdictio.svg?branch=master)](https://travis-ci.com/hdvincelette/mdJSONdictio)
[![R-CMD-check](https://github.com/hdvincelette/mdJSONdictio/workflows/R-CMD-check/badge.svg)](https://github.com/hdvincelette/mdJSONdictio/actions)
<!-- badges: end -->

Tools to write mdJSON dictionaries that can be imported to mdEditor to create Dictionary records.

## Installation

You can install the development version of mdJSONdictio from [GitHub](https://github.com/) with:

```
# Install devtools from CRAN
install.packages("devtools")

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio")
```

## Usage

```build.mdJSON()``` builds an mdJSON dictionary that can be imported to mdEditor to create a Dictionary record. The input file must be formatted to a [tabular dictionary template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/build.mdJSON_Dictionary_Template.xlsx?raw=true).

