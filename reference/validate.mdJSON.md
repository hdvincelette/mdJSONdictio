# Validate mdJSON Data Dictionaries

Compares an mdJSON data dictionary to a tabular dataset and summarizes
discrepancies in a data frame.

## Usage

``` r
validate.mdJSON(x, y, entity_num = 1)
```

## Arguments

- x:

  List object converted from an mdJSON file.

- y:

  Data frame of a dataset.

- entity_num:

  Default=1. Integer indicating the entity if there is more than one in
  the mdJSON file.

## Value

Returns a data frame comprised of warning messages about the mdJSON data
dictionary.

## See also

`validate.mdJSON()`

## Examples

``` r
# Import mdJSON data dictionary as list
path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
input.dxnry <- rjson::fromJSON(file = path)

# Import tabular dataset as data frame
path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
input.data<-read.csv(file = path, na.strings = "", stringsAsFactors = FALSE)

# Validate list against data frame
all.warnings<- validate.mdJSON(x = input.dxnry, y = input.data, entity_num = 1)

# Export table to disk
write.csv(x = all.warnings, file = "e.g.warnings.csv")
```
