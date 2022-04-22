
# mdJSONdictio

Tools to Build and Translate mdEditor mdJSON Data Dictionaries

## Installation
You can install the released version of mdJSONdictio from [GitHub](https://github.com/) with:
```
# Install devtools from CRAN
install.packages("devtools")
library(devtools)

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio", ref = 'v0.0.1')
library(mdJSONdictio)
```

You can install the development version of mdJSONdictio from [GitHub](https://github.com/) with:
```
# Install devtools from CRAN
install.packages("devtools")
library(devtools)

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio", ref = 'master')
library(mdJSONdictio)
```

For more detailed instructions and troubleshooting tips, see [Install mdJSONdictio R package](https://hdvincelette.github.io/mdJSONdictio/articles/03_Setup_mdJSONdictio.html#install-mdjsondictio-r-package).

## Usage

```build.mdJSON()``` builds an mdJSON data dictionary that can be imported to mdEditor to create a Dictionary Record. The input file must be formatted to a [tabular data dictionary template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v1.xlsx?raw=true). ```build.table()``` builds a tabular data dictionary from an mdEditor mdJSON data dictionary.

