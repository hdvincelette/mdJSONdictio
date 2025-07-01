
# mdJSONdictio
Tools to Extract, Build, Modify, and Validate mdEditor Dictionary records in R.

## Installation
You can install the released version of mdJSONdictio from [GitHub](https://github.com/) with:
```
# Install devtools from CRAN
install.packages("devtools")

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio", ref = 'v0.2.3')
```

And the development version of mdJSONdictio from [GitHub](https://github.com/) with:
```
# Install devtools from CRAN
install.packages("devtools")

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio", ref = 'master')
```

For more detailed instructions and troubleshooting tips, see [Install mdJSONdictio R package](https://hdvincelette.github.io/mdJSONdictio/articles/03_Setup_mdJSONdictio.html#install-mdjsondictio-r-package).

## Usage

```build.mdJSON()``` builds an mdJSON data dictionary that can be imported to mdEditor to create a Dictionary record. The input file must be formatted to a [tabular data dictionary template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v2.xlsx?raw=true). ```build.table()``` builds a tabular data dictionary from an mdEditor mdJSON data dictionary. Additional functions include ```validate.mdJSON()```/```validate.table()```, which validates mdJSON and tabular data dictionaries against a dataset, and ```modify.mdJSON()```/```modify.table()```, which modifies mdJSON and tabular data dictionaries based on inputs.```extract.mdJSON()``` extracts one or more mdJSON data dictionary from an mdJSON file comprised of other types of records.


## Getting help

Submit a [GitHub
Issue](https://github.com/hdvincelette/mdJSONdictio/issues) to report a
bug and request a feature or enhancement. Contact the [project
maintainer](https://github.com/vincelette-usfws) for information about
contributing to this project.
