---
title: "Tabular Data Dictionary Template"
output: 
  # html_document:
  #   theme: readable
  #   highlight:
  #   fig_caption: true
  knitr:::html_vignette:
    toc: true
  pdf_document:
    highlight: null
    number_sections: yes
vignette: >
  %\VignetteIndexEntry{mdJSONdictio}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
<style type="text/css">
  body{
  font-size: 12pt;
}
</style>
---






```build.mdJSON()``` only reads data frames formatted to a data dictionary template (Figure 1.1), which can be downloaded [here](https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v1.xlsx?raw=true). While csv is a more flexible file format, this template is in xlsx format because it contains validation rules that prevent incorrect values from being entered, resulting in errors when using ```build.mdJSON()``` or creating the Dictionary record in mdEditor.
<br />
<br />

#### Figure 1.1: Data dictionary template in Excel.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/Dictionary_Template.png)

<br />
<br />
The two main components of the tabular data dictionary template are attributes and entry values, each of which are represented as a row. Entry values are known as "domain items" which collectively make up the "domain" of an attribute. It is important to remember that while entry values are associated with a certain attribute, not all attributes have defined entry values. For example, the attribute "Sex" may have three defined entry values ("F","M", and "U"), where as the attribute "Year" does not have defined entry values since an unlimited number of years can be entered.
<br />
<br />
Table 1.1 describes the header in the tabular data dictionary template, which represent field(s) in mdEditor Dictionary records. "Definition" describes the template column. Many of these definitions are derived from the [mdEditor User Manual](https://guide.mdeditor.org/reference/edit-window/dictionary/dictionary-record.html). "DataType" indicates the type of value allowed in the template column. "Required" indicates whether the template column is required by attributes and/or entry values ("No" indicates it is not required by either attributes or entry values). 
<br />
<br />
 
Note:

- The template columns "codeName," "domainItem_name," "domainItem_value," and "definition" are all required by attributes and entry values.
- The template columns "dataType" and "allowNull" are required by attributes
<br />
<br />

<em>Tip: View attribute rows by filtering the dictionary column "domainItem_name" to "colname" and verify all rows have values for dictionary columns "dataType" and "allowNull".</em>
<br />
<br />

#### Table 1.1: Column descriptions for the data dictionary template.

<font size="2.5"> 
<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Column </th>
   <th style="text-align:left;"> DataType </th>
   <th style="text-align:left;"> Definition </th>
   <th style="text-align:left;"> Required </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> codeName </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> Attribute name; the code used to identify this attribute; most often this will be the table or spreadsheet column name. </td>
   <td style="text-align:left;"> Attributes; Entry values </td>
  </tr>
  <tr>
   <td style="text-align:left;"> domainItem_name </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> A descriptive name associated with the Domain Item value; Default is to use the Domain Item Value; Colum name rows are 'colname' </td>
   <td style="text-align:left;"> Attributes; Entry values </td>
  </tr>
  <tr>
   <td style="text-align:left;"> domainItem_value </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> The Domain Item value; the value in the table; colum name rows are 'colname' </td>
   <td style="text-align:left;"> Attributes; Entry values </td>
  </tr>
  <tr>
   <td style="text-align:left;"> definition </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> A succinct but comprehensive definition for the attribute or Domain Item </td>
   <td style="text-align:left;"> Attributes; Entry values </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dataType </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> The datatype or format of the entry values for an attribute; choose from drop down </td>
   <td style="text-align:left;"> Attributes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> allowNull </td>
   <td style="text-align:left;"> binary </td>
   <td style="text-align:left;"> Indicates whether null values are permitted as the attribute value (yes/no) </td>
   <td style="text-align:left;"> Attributes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> units </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> A unit-of-measure for the attribute. E.g. 'meters', 'atmospheres', 'liters'; Unrestricted, though SI units are recommended; lowercase only </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> unitsResolution </td>
   <td style="text-align:left;"> decimal </td>
   <td style="text-align:left;"> The smallest unit increment to which an attribute value is measured. </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isCaseSensitive </td>
   <td style="text-align:left;"> binary </td>
   <td style="text-align:left;"> Indicates if the content of the data set is encoded in case-sensitive ASCII (yes/no) </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fieldWidth </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> The number of characters in the data field. </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> missingValue </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> The code which represents missing data. </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> minValue </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> The minimum range value permissible for this attribute. The minimum value may be either numeric or character. </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> maxValue </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> The maximum range value permissible for this attribute. The maximum value may be either numeric or character. </td>
   <td style="text-align:left;"> No </td>
  </tr>
  <tr>
   <td style="text-align:left;"> notes </td>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> Anything entered here will be excluded from the JSON dictionary </td>
   <td style="text-align:left;"> No </td>
  </tr>
</tbody>
</table>
</font>
<br />
<br />
The "dataType" dictionary column has numerous defined entry values as shown in Table 1.2. These are derived from the mdEditor web application. The definitions in Table 1.2 were acquired from the [mdCodes Viewer](https://adiwg.github.io/mdTools/#codes-page) in the mdTools interface.
<br />
<br />

#### Table 1.2: dataType entry value descriptions for the data dictionary template.

<font size="2.5"> 
<table class="table" style="width: auto !important; ">
 <thead>
  <tr>
   <th style="text-align:left;"> Value </th>
   <th style="text-align:left;"> Definition </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> fixed length character strings </td>
  </tr>
  <tr>
   <td style="text-align:left;"> character varying </td>
   <td style="text-align:left;"> Variable length character strings </td>
  </tr>
  <tr>
   <td style="text-align:left;"> character large object </td>
   <td style="text-align:left;"> Character large object </td>
  </tr>
  <tr>
   <td style="text-align:left;"> national character </td>
   <td style="text-align:left;"> Fixed length national character strings (2 byte unicode) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> national character varying </td>
   <td style="text-align:left;"> Variable length national character strings (2 byte unicode) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> national character large object </td>
   <td style="text-align:left;"> National character (2 byte unicode) large object </td>
  </tr>
  <tr>
   <td style="text-align:left;"> text </td>
   <td style="text-align:left;"> Variable string to maximum of 65k characters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tinytext </td>
   <td style="text-align:left;"> Variable string to maximum of 255 characters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mediumtext </td>
   <td style="text-align:left;"> Variable string to maximum of 16m characters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> longtext </td>
   <td style="text-align:left;"> Variable string to maximum of 4g characters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> binary </td>
   <td style="text-align:left;"> Fixed length binary </td>
  </tr>
  <tr>
   <td style="text-align:left;"> binary varying </td>
   <td style="text-align:left;"> Variable length binary </td>
  </tr>
  <tr>
   <td style="text-align:left;"> binary large object </td>
   <td style="text-align:left;"> Binary large object to maximum of 65k bytes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mediumblob </td>
   <td style="text-align:left;"> Binary large object to maximum of 16m bytes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> longblob </td>
   <td style="text-align:left;"> Binary large object to maximum of 4g bytes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:left;"> Integers number (+-2b) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tinyint </td>
   <td style="text-align:left;"> Integer numbers (+-128) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mediumint </td>
   <td style="text-align:left;"> Integer numbers (+-16k) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> smallint </td>
   <td style="text-align:left;"> Integer numbers (+-32k) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bigint </td>
   <td style="text-align:left;"> Integer numbers (+-1e27) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> float </td>
   <td style="text-align:left;"> Floating point numbers </td>
  </tr>
  <tr>
   <td style="text-align:left;"> real </td>
   <td style="text-align:left;"> Low precision floating point numbers </td>
  </tr>
  <tr>
   <td style="text-align:left;"> double precision </td>
   <td style="text-align:left;"> High precision floating point numbers </td>
  </tr>
  <tr>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> Fixed precision  and scale decimal numbers </td>
  </tr>
  <tr>
   <td style="text-align:left;"> decimal </td>
   <td style="text-align:left;"> Fixed precision  and scale decimal numbers (numeric alternate) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bit </td>
   <td style="text-align:left;"> Fixed length bit strings </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bit varying </td>
   <td style="text-align:left;"> Variable length bit strings </td>
  </tr>
  <tr>
   <td style="text-align:left;"> date </td>
   <td style="text-align:left;"> Calendar date </td>
  </tr>
  <tr>
   <td style="text-align:left;"> time </td>
   <td style="text-align:left;"> Clock time </td>
  </tr>
  <tr>
   <td style="text-align:left;"> datetime </td>
   <td style="text-align:left;"> Date and time </td>
  </tr>
  <tr>
   <td style="text-align:left;"> timestamp </td>
   <td style="text-align:left;"> Number of seconds since the unix epoch (1970-01-01t00:00:00 utc) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> year </td>
   <td style="text-align:left;"> Year </td>
  </tr>
  <tr>
   <td style="text-align:left;"> interval </td>
   <td style="text-align:left;"> Time intervals </td>
  </tr>
  <tr>
   <td style="text-align:left;"> interval day </td>
   <td style="text-align:left;"> Day intervals </td>
  </tr>
  <tr>
   <td style="text-align:left;"> interval year </td>
   <td style="text-align:left;"> Year intervals </td>
  </tr>
  <tr>
   <td style="text-align:left;"> currency </td>
   <td style="text-align:left;"> Monetary value </td>
  </tr>
  <tr>
   <td style="text-align:left;"> money </td>
   <td style="text-align:left;"> Monetary value </td>
  </tr>
  <tr>
   <td style="text-align:left;"> boolean </td>
   <td style="text-align:left;"> Boolean value (yes/no) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> xml </td>
   <td style="text-align:left;"> Xml formatted data </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enum </td>
   <td style="text-align:left;"> List of possible values: enum('a','b','c') </td>
  </tr>
</tbody>
</table>
</font>
<br />
<br />
<br />


