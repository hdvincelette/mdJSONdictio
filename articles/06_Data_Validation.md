# Data Validation with mdJSONdictio

------------------------------------------------------------------------

[`validate.mdJSON()`](https://hdvincelette.github.io/mdJSONdictio/reference/validate.mdJSON.md)/[`validate.table()`](https://hdvincelette.github.io/mdJSONdictio/reference/validate.table.md)
compares mdJSON and tabular data dictionaries against a corresponding
dataset and outputs a warnings table, as shown below. The tabular data
dictionary must be formatted to the mdJSONdictio
[template](https://hdvincelette.github.io/mdJSONdictio/articles/02_Dictionary_Template.html).These
functions are intended to be used as a Quality Assurance step in the
data management process.

###### Warnings table in Excel

![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/Warnings_Table.png)  
  

## Validation procedures

[`validate.mdJSON()`](https://hdvincelette.github.io/mdJSONdictio/reference/validate.mdJSON.md)
and `validate.tableN()` can detect numerous discrepencies between a
dataset and dictionary, as shown below. “category” corresponds to
field(s) in the tabular data dictionary or mdEditor Dictionary record
which failed one or more logical tests. “discrepancy” describes the
cause of that failure. The output is called a “warnings” table because
some discrepancies may not warrant an action (e.g., the data type for
the attribute “TagID” appears to be “integer” in the dataset, but the
data type is described as “character varying” in the dictionary because
entry values can contains letters). It is ultimately up to the data
steward to decide what needs to be corrected to most accurately describe
the associated dataset.

Refer to the [mdEditor Reference
Manual](https://guide.mdeditor.org/reference/reference-manual.html) for
complete definitions and constraints of all mdEditor Attribute, Domain,
and Domain Item fields.

###### Potential dataset-dictionary discrepancies

| category | discrepancy |
|:---|:---|
| codeName | attributes not listed in the dictionary |
| domainItem_value | entry values not listed in the dictionary |
| allowNull | NAs when allowNull is FALSE in the dictionary |
| dataType_Rdatatype | different datatype than described in the dictionary\* |
| dataType_datetime | entry values with datetime, date, or time datatypes not in standard ISO 1806 datetime format (e.g., 2022-09-27 18:00:00.000) |
| dataType_maxLength | entry values with more characters than allowed for the datatype\* |
| dataType_maxPrecision | entry values with more precision than allowed for the datatype\* |
| dataType_minValue | smaller entry values than allowed for the datatype\* |
| dataType_maxValue | larger entry values than allowed for the datatype\* |
| dataType_distinctValue | greater number of distinct values than allowed for the datatype\* |
| dataType_distinctLength | variable length entry values when the datatype is fixed length\* |
| unitsResolution | entry values with greater or lower resolution than indicated in the dictionary |
| fieldWidth | entry values with more characters than indicated in the dictionary |
| missingValue | NAs when missingValue is otherwise defined in the dictionary |
| minValue | smaller entry values than indicated in the dictionary |
| maxValue | larger entry values than indicated in the dictionary |

\*see data type rules  
  

## Data type constraints

“dataType” values undergo a series of tests based on [Structured Query
Language
(SQL)](https://www.iso.org/obp/ui/en/#iso:std:iso-iec:9075:-1:ed-6:v1:en)
data constraints to ensure the associated dataset attribute is described
accurately. Data types are described as followed. Definitions are from
the [mdCodes Viewer](https://adiwg.github.io/mdTools/#codes-page) in the
mdTools interface, and contraints from the [International Organization
for Standardization
(ISO)](https://www.iso.org/obp/ui/en/#iso:std:iso-iec:9075:-1:ed-6:v1:en).

###### Data type rules

| value | definition | RdataType | maxLength | maxPrecision | minValue_unsigned | maxValue_unsigned | distinctValue | distinctLength |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| character | Fixed length character strings | character | 255 | NA | NA | NA | NA | 1 |
| character varying | Variable length character strings | character | 65535 | NA | NA | NA | NA | NA |
| character large object | Character large object; large objects are designed to hold extremely large column values | character | 2147483647 | NA | NA | NA | NA | NA |
| national character | Fixed length national character strings (2 byte unicode); UTF-8 or UTF-16 encoded characters as defined by the Unicode Standard | character | 255 | NA | NA | NA | NA | 1 |
| national character varying | Variable length national character strings (2 byte unicode); UTF-8 or UTF-16 encoded characters as defined by the Unicode Standard | character | 65535 | NA | NA | NA | NA | NA |
| national character large object | National character (2 byte unicode) large object; UTF-8 or UTF-16 encoded characters as defined by the Unicode Standard | character | 2147483647 | NA | NA | NA | NA | NA |
| text | Variable string to maximum of 65k characters | character | 65535 | NA | NA | NA | NA | NA |
| tinytext | Variable string to maximum of 255 characters | character | 255 | NA | NA | NA | NA | NA |
| mediumtext | Variable string to maximum of 16m characters | character | 16777215 | NA | NA | NA | NA | NA |
| longtext | Variable string to maximum of 4g characters | character | 4294967295 | NA | NA | NA | NA | NA |
| binary | Fixed length binary | integer | 255 | NA | NA | NA | NA | 1 |
| binary varying | Variable length binary | integer | 32704 | NA | NA | NA | NA | NA |
| binary large object | Binary large object to maximum of 65k bytes; large objects are designed to hold extremely large column values | integer | 2147483647 | NA | NA | NA | NA | NA |
| mediumblob | Binary large object to maximum of 16m bytes; large objects are designed to hold extremely large column values | integer | 16777215 | NA | NA | NA | NA | NA |
| longblob | Binary large object to maximum of 4g bytes; large objects are designed to hold extremely large column values | integer | 4294967295 | NA | NA | NA | NA | NA |
| integer | Integers number (+-2b) | integer | NA | NA | -2.147484e+09 | 2.147484e+09 | NA | NA |
| tinyint | Integer numbers (+-128) | integer | NA | NA | -1.280000e+02 | 1.270000e+02 | NA | NA |
| mediumint | Integer numbers (+-16k) | integer | NA | NA | -8.388608e+06 | 8.388608e+06 | NA | NA |
| smallint | Integer numbers (+-32k) | integer | NA | NA | -3.276700e+04 | 3.276700e+04 | NA | NA |
| bigint | Integer numbers (+-1e27) | integer | NA | NA | -9.220000e+18 | -9.220000e+18 | NA | NA |
| float | Floating point numbers | numeric | NA | 24 | -1.790000e+308 | 1.790000e+308 | NA | NA |
| real | Low precision floating point numbers | numeric | NA | NA | -3.400000e+38 | 3.400000e+38 | NA | NA |
| double precision | High precision floating point numbers | double | NA | 53 | -1.790000e+308 | 1.790000e+308 | NA | NA |
| numeric | Fixed precision and scale decimal numbers | numeric | NA | 38 | -1.000000e+38 | 1.000000e+38 | NA | NA |
| decimal | Fixed precision and scale decimal numbers (numeric alternate) | decimal | NA | 38 | -1.000000e+38 | 1.000000e+38 | NA | NA |
| bit | Fixed length bit strings | integer | NA | NA | -9.220000e+18 | -9.220000e+18 | NA | 1 |
| bit varying | Variable length bit strings | integer | NA | NA | -9.220000e+18 | -9.220000e+18 | NA | NA |
| date | Calendar date |  | NA | NA | NA | NA | NA | NA |
| time | Clock time |  | NA | NA | NA | NA | NA | NA |
| datetime | Date and time |  | NA | NA | NA | NA | NA | NA |
| timestamp | Number of seconds since the unix epoch (1970-01-01t00:00:00 utc) | numeric | NA | NA | NA | NA | NA | NA |
| year | Year | integer | NA | NA | 1.000000e+00 | 9.999000e+03 | NA | NA |
| interval | Time intervals (i.e., length of time in year, month, day, hour, minute, or second) | numeric | NA | NA | NA | NA | NA | NA |
| interval day | Day intervals | numeric | NA | NA | NA | NA | NA | NA |
| interval year | Year intervals | numeric | NA | NA | NA | NA | NA | NA |
| currency | Monetary value | numeric | 19 | 4 | -3.960000e+28 | 3.960000e+28 | NA | NA |
| money | Monetary value | numeric | NA | NA | -9.220000e+14 | 9.220000e+14 | NA | NA |
| boolean | Boolean value (yes/no) |  | NA | NA | NA | NA | 2 | NA |
| xml | Extensible Markup Language (XML) formatted data |  | NA | NA | NA | NA | NA | NA |
| enum | List of possible values: enum(‘a’,‘b’,‘c’) |  | NA | NA | NA | NA | 65535 | NA |

  
  
