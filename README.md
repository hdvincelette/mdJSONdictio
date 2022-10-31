
# mdJSONdictio
Tools to Build, Translate, and Validate mdEditor mdJSON Data Dictionaries in R.

## Installation
You can install the released version of mdJSONdictio from [GitHub](https://github.com/) with:
```
# Install devtools from CRAN
install.packages("devtools")

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio", ref = 'v0.1.0')
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

```build.mdJSON()``` builds an mdJSON data dictionary that can be imported to mdEditor to create a Dictionary Record. The input file must be formatted to a [tabular data dictionary template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v1.xlsx?raw=true). ```build.table()``` builds a tabular data dictionary from an mdEditor mdJSON data dictionary. mdJSON and tabular data dictionaries can be validated against a dataset with ```validate.mdJSON()``` and ```validate.table()```, respectively.

