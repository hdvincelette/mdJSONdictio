# Build Tabular Data Dictionaries

Translates a list object converted from an mdEditor mdJSON data
dictionary file into a data frame.

## Usage

``` r
build.table(x, entity_num)
```

## Arguments

- x:

  List object converted from an mdJSON file.

- entity_num:

  Default=1. Integer indicating the entity if there is more than one in
  the mdJSON file.

- y:

  Data frame of a dataset.

## Value

Returns a data frame corresponding to the mdJSON data dictionary.

## See also

[`build.mdJSON()`](https://hdvincelette.github.io/mdJSONdictio/reference/build.mdJSON.md)

## Examples

``` r
# Import mdJSON data dictionary as list
path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
input.list <- rjson::fromJSON(file = path)

# Translate list to data frame
new.table<- mdJSONdictio::build.table(x = input.list, entity_num = 1)

# Export table to disk
write.csv(x = new.table, file = "e.g.dictionary2.csv", na="", row.names = FALSE)
```
