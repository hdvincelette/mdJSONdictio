
# mdJSONdictio

Tools to Build and Transform mdEditor mdJSON Data Dictionaries

## Installation

You can install the development version of mdJSONdictio from [GitHub](https://github.com/) with:

```
# Install devtools from CRAN
install.packages("devtools")
library(devtools)

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio")
library(mdJSONdictio)



# If mdJSONdictio installation fails due to dependent packages:

# Install dependent packages
packages<-c(
  "purrr",
  "readxl",
  "rjson",
  "stats",
  "tibble",
  "stringr",
  "uuid",
  "dplyr",
  "plyr"
)

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio")
library(mdJSONdictio)



# If mdJSONdictio installation fails due to command line settings:

# Install mdJSONdictio with compiled code only for sub-architecture used by R CMD INSTALL
devtools::install_github("hdvincelette/mdJSONdictio",
                         INSTALL_opts = c("--no-multiarch"))



```

## Usage

```build.mdJSON()``` builds an mdJSON data dictionary that can be imported to mdEditor to create a Dictionary record. The input file must be formatted to a [tabular data dictionary template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v1.xlsx?raw=true). ```build.table()``` builds a tabular data dictionary from an mdEditor mdJSON data dictionary.

