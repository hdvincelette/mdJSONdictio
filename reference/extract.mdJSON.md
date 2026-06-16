# Extract mdJSON Data Dictionaries from a Metadata file

Plucks one or more mdJSON data dictionaries from an mdJSON file
comprised of other types of records.

## Usage

``` r
extract.mdJSON(x, record.type = "dictionaries", multiple = TRUE, all = FALSE)
```

## Arguments

- x:

  List object converted from an mdJSON file.

- record.type:

  Default="dictionaries". String or vector representing the type(s) of
  records to extract. Additional options include "records" and
  "contacts".

- multiple:

  Default=TRUE. Whether to allow a selection of multiple records.

- all:

  Default=FALSE. Whether to automatically select all records of the
  specified record type. all=TRUE overrides the multiple argument and
  avoids the user record selection menu.

## Value

Returns a list object corresponding to the mdJSON data dictionary file.

## See also

`extract.mdJSON()`

## Examples

``` r
# Import mdJSON data dictionary as list
path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
input.metadata <- rjson::fromJSON(file = path)

# Extract a data dictionary record from a Metadata file
dxnry.record<- extract.mdJSON(x = input.metadata, record.type = "dictionaries")

# Convert list to JSON
output.dxnry = rjson::toJSON(x = dxnry.record)

# Export JSON to disk
write(x = output.dxnry, file = "e.g.dictionary.json")
```
