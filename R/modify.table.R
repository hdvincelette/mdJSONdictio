#' Modify Tabular Data Dictionaries
#'
#' Amends a tabular data dictionary, including adding, updating, and removing attributes and domains.
#' @param x Data frame. Tabular data dictionary.
#' @param how Character string. How the dictionary will be modified. Available options include 'add_attribute', 'add_domain', 'add_domainItem', 'update_attribute', 'update_domain', 'update_domainItem', 'remove_attribute', 'remove_domain', 'remove_domainItem'; see 'Details'.
#' @param attribute_codeName Conditionally optional character string. Name of the attribute to add or modify.
#' @param attribute_allowNull Optional logical (TRUE/FALSE). Whether null values are permissible for the attribute specified with 'attribute_codeName'.
#' @param attribute_dataType Optional character string. Datatype of the attribute specified with 'attribute_codeName'.
#' @param attribute_definition Optional character string. Definition of the attribute specified with 'attribute_codeName'.
#' @param attribute_units Optional character string. Unit-of-measure for the attribute specified with 'attribute_codeName'.
#' @param attribute_unitsResolution Optional numeric. Smallest increment of measurement for the attribute specified with 'attribute_codeName'.
#' @param attribute_isCaseSensitive Optional logical (TRUE/FALSE). Whether case-sensitive ASCII encoding is used for the attribute specified with 'attribute_codeName'.
#' @param attribute_missingValue Optional character string. Code representing missing values of the attribute specified with 'attribute_codeName'.
#' @param attribute_minValue Optional character string. Minimum value permissible for the attribute specified with 'attribute_codeName'.
#' @param attribute_maxValue Optional character string. Maximum value permissible for the attribute specified with 'attribute_codeName'.
#' @param attribute_fieldWidth Optional integer. Field width of the attribute indicated with 'attribute_codeName'.
#' @param domain_domainId Conditionally optional character string. Universally Unique Identifier (UUID) of the domain specified with 'domain_codeName', or the domain linked to the attribute specified with 'attribute_codeName'.
#' @param domain_codeName Conditionally optional character string. Name of the domain to add or modify.
#' @param domain_description Optional character string. Description of the domain specified with 'domain_codeName'.
#' @param domainItem Optional list or vector. Permissible entry value(s), name(s), and definition(s) of the domain specified with 'domain_codeName'. Unnamed list elements must be ordered correctly (i.e., name, value, definition). 'domainItem' will override 'domainItem_value', 'domainItem_name', and 'domainItem_definition' when 'how' == 'add_attribute', 'add_domain' or 'add_domainItem.
#' @param domainItem_name Optional character string or vector. Name(s) of permissible entry value(s) specified with 'domainItem_value'.
#' @param domainItem_value Optional character string or vector. Permissible entry value(s) of the domain specified with 'domain_codeName'.
#' @param domainItem_definition Optional character string or vector. Definition(s) of permissible entry value(s) specified with 'domainItem_value'.
#' @param new.attribute_codeName Optional character string. New name for the attribute specified with 'attribute_codeName'.
#' @param new.domain_codeName Optional character string. New name for the domain specified with 'domain_codeName'.
#' @param new.domainItem_value Optional character string. New entry value for the domain item specified with 'domainItem_value'.
#' @param quiet Optional logical. Default=FALSE. If TRUE, function runs with minimum input required and does not print revisions for approval.
#' @return Returns a modified data frame of the data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```modify.table()```
#' @export
#' @examples
#' # TBD
#'

modify.table <-
  function(x,
           how = c(
             "add_attribute",
             "add_domain",
             "add_domainItem",
             "update_attribute",
             "update_domain",
             "update_domainItem",
             "remove_attribute",
             "remove_domain",
             "remove_domainItem"
           ),
           attribute_codeName = "",
           attribute_allowNull = "",
           attribute_dataType = "",
           attribute_definition = "",
           attribute_units = "",
           attribute_unitsResolution = "",
           attribute_isCaseSensitive = "",
           attribute_missingValue = "",
           attribute_minValue = "",
           attribute_maxValue = "",
           attribute_fieldWidth = "",
           domain_domainId = "",
           domain_codeName = "",
           domain_description = "",
           domainItem = list(),
           domainItem_name = "",
           domainItem_value = "",
           domainItem_definition = "",
           new.attribute_codeName = "",
           new.domain_codeName = "",
           new.domainItem_value = "",
           quiet = FALSE) {
    `%>%` <- magrittr::`%>%`

    x <- mdJSONdictio::build.mdJSON(x = x)


    if(missing(domainItem)) {
      domainItem <- list()
    }

    x <- modify.mdJSON(
      x,
      how,
      attribute_codeName,
      attribute_allowNull,
      attribute_dataType,
      attribute_definition,
      attribute_units,
      attribute_unitsResolution,
      attribute_isCaseSensitive,
      attribute_missingValue,
      attribute_minValue,
      attribute_maxValue,
      attribute_fieldWidth,
      domain_domainId,
      domain_codeName,
      domain_description,
      domainItem,
      domainItem_name,
      domainItem_value,
      domainItem_definition,
      new.attribute_codeName,
      new.domain_codeName,
      new.domainItem_value,
      quiet
    )

    x <- mdJSONdictio::build.table(x = x)

    return(x)


  }
