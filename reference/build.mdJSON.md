# Build mdJSON Data Dictionaries

Translates a data frame of a tabular data dictionary into an list object
that can be subsequently converted to mdJSON and imported to mdEditor as
a Dictionary Record. The input tabular data dictionary must be formatted
to a
[template](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v2.xlsx?raw=true).

## Usage

``` r
build.mdJSON(x, title)
```

## Arguments

- x:

  Data frame of the tabular data dictionary.

- title:

  String designating the title of the Dictionary Record in
  mdEditor.Default=deparse(match.call()\$x).

## Value

Returns a list object corresponding to the tabular data dictionary.

## See also

[`build.table()`](https://hdvincelette.github.io/mdJSONdictio/reference/build.table.md)

## Examples

``` r
# Import tabular data dictionary as data frame
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
input.table<-readxl::read_excel(path = path)

# Translate data frame to list
new.list<- mdJSONdictio::build.mdJSON(x = input.table, title = "Example Dictionary")

# Convert list to JSON
new.json = rjson::toJSON(x = new.list)

# Export JSON to disk
write(x = new.json, file = "e.g.dictionary.json")
```
