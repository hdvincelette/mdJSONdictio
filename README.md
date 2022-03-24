
# mdJSONdictio

Tools to Build and Transform mdEditor mdJSON Data Dictionaries

## Installation

You can install the development version of mdJSONdictio from [GitHub](https://github.com/) with:

```
# Install devtools from CRAN
install.packages("devtools")
library(devtools)

# Install qdap from CRAN
install.packages("qdap")
library(qdap)

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio")
library(mdJSONdictio)



# If mdJSONdictio installation fails:

# Install mdJSONdictio with compiled code only for sub-architecture used by R CMD INSTALL
devtools::install_github("hdvincelette/mdJSONdictio", INSTALL_opts=c("--no-multiarch"))

# OR

# Download mdJSONdictio zip from GitHub repository
browseURL("https://github.com/hdvincelette/mdJSONdictio/archive/refs/heads/master.zip")

# Set working directory to zip location - Session->Set Working Directory->Choose Directory
# Install mdJSONdictio from zip source
install.packages("mdJSONdictio-master")

```

## Usage

```build.mdJSON()``` builds an mdJSON data dictionary that can be imported to mdEditor to create a Dictionary record. The input file must be formatted to a [tabular data dictionary template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/build.mdJSON_Dictionary_Template.xlsx?raw=true). ```build.table()``` builds a tabular data dictionary from an mdEditor mdJSON data dictionary.

