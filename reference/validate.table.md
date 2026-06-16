# Validate Tabular Data Dictionaries

Compares a tabular data dictionary to a tabular dataset and summarizes
discrepancies in a data frame. The input data dictionary must be
formatted to a
[template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v2.xlsx?raw=true).

## Usage

``` r
validate.table(x, y)
```

## Arguments

- x:

  Data frame of a tabular data dictionary.

- y:

  Data frame of a dataset.

## Value

Returns a data frame comprised of warning messages about the tabular
data dictionary.

## See also

`validate.table()`

## Examples

``` r
# Import tabular data dictionary as data frame
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
input.dxnry<-readxl::read_excel(path = path)

# Import tabular dataset as data frame
path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
input.data<-read.csv(file = path, na.strings = "", stringsAsFactors = FALSE)

# Validate data frame against data frame
all.warnings<- validate.table(x = input.dxnry, y = input.data)

# Export table to disk
write.csv(x = all.warnings, file = "e.g.warnings2.csv")
```
