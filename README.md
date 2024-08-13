
# mdJSONdictio
Tools to Extract, Build, Modify, and Validate mdEditor Dictionary records in R.

## Installation
You can install the released version of mdJSONdictio from [GitHub](https://github.com/) with:
```
# Install devtools from CRAN
install.packages("devtools")

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio", ref = 'v0.2.1')
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

## USWFS Disclaimer

The United States Fish and Wildlife Service (FWS) GitHub project code is
provided on an “as is” basis and the user assumes responsibility for its
use. FWS has relinquished control of the information and no longer has
responsibility to protect the integrity, confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by FWS. The FWS seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by FWS or the United States Government.

## Getting help

Submit a [GitHub
Issue](https://github.com/hdvincelette/FWSAkRDRtools/issues) to report a
bug and request a feature or enhancement. Contact the [project
maintainer](https://github.com/vincelette-usfws) for information about
contributing to this project.
