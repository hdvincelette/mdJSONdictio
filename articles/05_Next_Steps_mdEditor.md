# Next Steps in mdEditor

------------------------------------------------------------------------

mdJSONdictio functions are built to work in concert with the
[mdEditor](https://go.mdeditor.org/#/dashboard) web application. The
following describes how to finish an mdJSON data dictionary after using
[`build.mdJSON()`](https://hdvincelette.github.io/mdJSONdictio/reference/build.mdJSON.md),
as well how to prepare an mdJSON data dictionary before using
[`build.table()`](https://hdvincelette.github.io/mdJSONdictio/reference/build.table.md).  
  

## After using `build.mdJSON()`

##### Import the mdJSON data dictionary to mdEditor

**Open mdEditor in your browser and navigate to the Import tab at the
top of the page. Drag and drop the newly written mdJSON file and click
“Import Data.”** This will generate a Dictionary Record with the
dictionary title you specified in the function. You may need to refresh
your browser if the dictionary does not immediately appear.

###### Import page in mdEditor

![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/mdEditor_Import.png)  
  

##### Fix errors in the Dictionary Record

When viewing your Dictionary Record, you will notice an orange
exclamation mark next to the title that indicates the dictionary is
incomplete and has missing or erroneous elements. **Click the orange
exclamation mark to view error details.** These errors can be remedied
by completing the Responsible Party field in the Main tab and the Entity
Code Name and Definition fields in the Entities tab. For more
information about these fields, refer to the [mdEditor User
Manual](https://guide.mdeditor.org/reference/reference-manual.html). You
will know your dictionary is free of errors when the orange triangle
disappears and the edit icon next to your Dictionary Record in the
left-hand menu is green. If you see more than these three errors, make
sure the tabular data dictionary is filled out as described in the
[Tabular Data Dictionary Template
article](https://hdvincelette.github.io/mdJSONdictio/articles/02_Dictionary_Template.html).

###### Dictionary Record errors in mdEditor

![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/mdEditor_Errors.png)  

##### Associate and maintain the Dictionary Record

Once the Dictionary Record is free of errors, it can be associated with
the Metadata record(s) it describes (e.g. tabular dataset, shapefiles)
to help create archival quality project metadata. For details of this
process and other information, refer to the [mdEditor User
Manual](https://guide.mdeditor.org/). Any future updates to your
Dictionary Record can be done manually in the mdEditor web
application.  
  
  

## Before using `build.table()`

##### Complete required mdEditor fields

**Open mdEditor in your browser and import your mdJSON data dictionary
file as described in [Import the mdJSON data dictionary to
mdEditor](https://hdvincelette.github.io/mdJSONdictio/articles/05_Next_Steps_mdEditor.html#import-the-mdjson-data-dictionary-to-mdeditor).Navigate
to your Dictionary Record and verify the contents and check for
errors.** You will know your dictionary is free of errors when the
orange triangle disappears and the edit icon next to your Dictionary
Record in the left-hand menu is green. While
[`build.table()`](https://hdvincelette.github.io/mdJSONdictio/reference/build.table.md)
only requires an entity with at least one defined attribute, an
error-free Dictionary Record is more likely to be exported with a
structure that can be processed by the function. Please note, if your
Dictionary Record contains two entities, they will be processed into a
single tabular data dictionary.  
  

##### Export the mdJSON data dictionary

**Navigate to the Export tab at the top of the page.** Make sure no
Metadata records, Contacts or undesired Dictionaries are selected
(select and deselect the checkboxes under the search bars). **Select the
desired dictionary and click “Export Selected” to the right of the
screen. Save the file to your working space.** If you are not prompted
to select a save location, the file may have automatically saved to your
local Downloads folder. If you intend to translate more than one
Dictionary Record into a table, you can select and export them together
into a single mdJSON file and specify which dictionary is processed by
[`build.table()`](https://hdvincelette.github.io/mdJSONdictio/reference/build.table.md)
with the parameter `dictionary_num`.

###### Export page in mdEditor

![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/mdEditor_Export.png)  
