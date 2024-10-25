---
editor_options: 
  markdown: 
    wrap: 72
---

# mdJSONdictio 0.2.2

## Fixed

-   Detection/reporting of unlisted entry values in `validate.table`

## Added

-   Optional argument to allow only one record selection in
    `extract.mdJSON`

# mdJSONdictio 0.2.1

## Fixed

-   Minor bugs related to max character length tests in
    `validate.mdJSON` and `validate.table`

# mdJSONdictio 0.2.0

## Added

-   "Data Validation with mdJSONdictio" vignette for `validate.table`
    and `validate.mdJSON`

-   `extract.mdJSON`: extracts one or more mdJSON data dictionaries from
    an mdJSON file comprised of other types of records

## Changed

-   mdJSONdictio_Dictionary_Template_v2 now utilizes logical values for
    allowNull and isCaseSensitive (TRUE/FALSE) Critical bug fixes for
    `build.mdJSON` and `validate.table`
