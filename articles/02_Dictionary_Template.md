# Tabular Data Dictionary Template

------------------------------------------------------------------------

[`build.mdJSON()`](https://hdvincelette.github.io/mdJSONdictio/reference/build.mdJSON.md)
only reads data frames formatted to a data dictionary template, which
can be downloaded
[here](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v2.xlsx?raw=true).
Guidelines for using this template are outlined below.

*While csv is a more flexible file format, this template is in xlsx
format because it contains validation rules that prevent incorrect
values from being entered, resulting in errors when using
[`build.mdJSON()`](https://hdvincelette.github.io/mdJSONdictio/reference/build.mdJSON.md)
or creating the Dictionary Record in mdEditor.*  

###### Data Dictionary Template in Excel

![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/Dictionary_Template.png)  
  

###### Example dataset and data dictionary in Excel

![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/Dataset_and_Dictionary.png)  
  

## Attributes and domain items

The two main components of the tabular data dictionary template are
attributes and domain items, each of which are represented as a row.
Attributes are units of information (metadata) describing data fields
(e.g. columns in a table). Domain items are accepted entry values for a
data field, which collectively make up the domain of an attribute. It is
important to remember that not all attribute domains are defined. For
example, the attribute “Sex” may have a domain comprised of three domain
items (“F”,“M”, and “U”), where as the attribute “Year” does not have a
defined domain since an unlimited number of years can be entered.  
  
The table below describes the header in the tabular data dictionary
template, which represent field(s) in mdEditor Dictionary Records.
“definition” describes the template column. These definitions are
derived from the [mdEditor User
Manual](https://guide.mdeditor.org/reference/edit-window/dictionary/dictionary-record.html).
“type” indicates the type of value allowed in the template column.
“required” indicates whether the template column is required by
attributes and/or domain items (“No” indicates it is not required by
either attributes or domain items).  

*Note: The template columns “codeName,” “domainItem_name,”
“domainItem_value,” and “definition” are all required by attributes and
domain items. The template columns “dataType” and “allowNull” are
required by attributes*  
  

##### Data dictionary template column descriptions

| column | type | definition | required |
|:---|:---|:---|:---|
| codeName | character varying | Attribute name; the code used to identify this attribute; most often this will be the table or spreadsheet column name | Attributes; Domain items |
| domainItem_name | character varying | A descriptive name associated with the domain item value; Default is to use the domain item value; rows regarding attributes are ‘dataField’ | Attributes; Domain items |
| domainItem_value | character varying | The domain item value; the entry value in the table; rows regarding attributes are ‘dataField’ | Attributes; Domain items |
| definition | character varying | A succinct but comprehensive definition for the attribute or domain item | Attributes; Domain items |
| dataType | character varying | The datatype or format of the entry values for an attribute; choose from drop down | Attributes |
| allowNull | boolean | Indicates whether null values are permitted as the attribute value (yes/no) | Attributes |
| units | character varying | A unit-of-measure for the attribute; e.g. ‘meters’ | No |
| unitsResolution | decimal | The smallest unit increment to which an attribute value is measured | No |
| isCaseSensitive | boolean | Indicates if the content of the data set is encoded in case-sensitive ASCII (yes/no) | No |
| fieldWidth | integer | The number of characters in the data field | No |
| missingValue | character varying | The code which represents missing data | No |
| minValue | character varying | The minimum range value permissible for this attribute; this may be a numeric or character | No |
| maxValue | character varying | The maximum range value permissible for this attribute; this may be a numeric or character | No |
| notes | character varying | Anything entered here will be excluded from the mdJSON data dictionary | No |

  
  

## Attribute data types

Attributes require a defined data type. The “dataType” template column
accepts numerous values as shown in the table below. These are derived
from the mdEditor web application, and the definitions were acquired
from the [mdCodes Viewer](https://adiwg.github.io/mdTools/#codes-page)
in the mdTools interface.  
  

##### Data type definitions

| value | definition |
|:---|:---|
| character | fixed length character strings |
| character varying | Variable length character strings |
| character large object | Character large object |
| national character | Fixed length national character strings (2 byte unicode) |
| national character varying | Variable length national character strings (2 byte unicode) |
| national character large object | National character (2 byte unicode) large object |
| text | Variable string to maximum of 65k characters |
| tinytext | Variable string to maximum of 255 characters |
| mediumtext | Variable string to maximum of 16m characters |
| longtext | Variable string to maximum of 4g characters |
| binary | Fixed length binary |
| binary varying | Variable length binary |
| binary large object | Binary large object to maximum of 65k bytes |
| mediumblob | Binary large object to maximum of 16m bytes |
| longblob | Binary large object to maximum of 4g bytes |
| integer | Integers number (+-2b) |
| tinyint | Integer numbers (+-128) |
| mediumint | Integer numbers (+-16k) |
| smallint | Integer numbers (+-32k) |
| bigint | Integer numbers (+-1e27) |
| float | Floating point numbers |
| real | Low precision floating point numbers |
| double precision | High precision floating point numbers |
| numeric | Fixed precision and scale decimal numbers |
| decimal | Fixed precision and scale decimal numbers (numeric alternate) |
| bit | Fixed length bit strings |
| bit varying | Variable length bit strings |
| date | Calendar date |
| time | Clock time |
| datetime | Date and time |
| timestamp | Number of seconds since the unix epoch (1970-01-01t00:00:00 utc) |
| year | Year |
| interval | Time intervals |
| interval day | Day intervals |
| interval year | Year intervals |
| currency | Monetary value |
| money | Monetary value |
| boolean | Boolean value (yes/no) |
| xml | Xml formatted data |
| enum | List of possible values: enum(‘a’,‘b’,‘c’) |

  
  
  
