
# mdJSONdictio

Tools to Build and Transform mdEditor mdJSON Data Dictionaries

## Installation

You can install the development version of mdJSONdictio from [GitHub](https://github.com/) with:

```
# Install devtools from CRAN
install.packages("devtools")
library(devtools)

# Install mdJSONdictio from GitHub
# optional: include `build_vingettes = TRUE`
devtools::install_github("hdvincelette/mdJSONdictio")
library(mdJSONdictio)

```

## Usage

```build.mdJSON()``` builds an mdJSON data dictionary that can be imported to mdEditor to create a Dictionary record. The input file must be formatted to a [tabular data dictionary template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/build.mdJSON_Dictionary_Template.xlsx?raw=true). ```build.table()``` builds a tabular data dictionary from an mdEditor mdJSON data dictionary.

