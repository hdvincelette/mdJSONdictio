Modify Tabular Data Dictionaries
#'
#' Amends a tabular data dictionary, including adding/updating attributes and domains.
#' @param x  Data frame of a tabular data dictionary.
#' @param how Character string matching one of the modification options: see ‘Details’.
#' @param codeName Conditionally optional character string. Name of the attribute to add or modify.
#' @param allowNull Optional logical (TRUE/FALSE). Whether null values are permissible for the attribute indicated with 'codeName'.
#' @param dataType Optional character string. Datatype of the attribute indicated with 'codeName'.
#' @param definition Optional character string. Definition of the attribute indicated with 'codeName'.
#' @param units Optional character string. Unit-of-measure for the attribute indicated with 'codeName'.
#' @param unitsResolution Optional numeric. Smallest increment of measurement for the attribute indicated with 'codeName'.
#' @param isCaseSensitive Optional logical (TRUE/FALSE). Whether case-sensitive ASCII encoding is used for the attribute indicated with 'codeName'.
#' @param missingValue Optional character string. Code representing missing values of the attribute indicated with 'codeName'.
#' @param minValue Optional character string. Minimum value permissible for the attribute indicated with 'codeName'.
#' @param maxValue Optional character string. Maximum value permissible for the attribute indicated with 'codeName'.
#' @param fieldWidth Optional integer. Field width of the attribute indicated with 'codeName'.
#' @param domainId Conditionally optional character string. Universally Unique Identifier (UUID) of the attribute or domain indicated with 'codeName' or domainName', respectively.
#' @param domainName Conditionally optional character string. Name of the domain to add or modify.
#' @param domainDescription Optional character string. Description of the domain indicated with 'domainName'.
#' @param domainItem_value Optional character string. Value of the domain item (entry value) indicated with 'domainName'.
#' @param domainItem_name Optional character string. Name of the domain item (entry value) indicated with 'domainItem_value'.
#' @param domainItem_definition Optional character string. Definition of the domain item (entry value) indicated with 'domainItem_value'.
#' @param quiet Optional logical. Default=FALSE. If TRUE, function runs with minimum input required and does not print revisions for approval.
#' @return Returns a modified data frame of the data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```modify.table()```
#' @export
#' @examples
#'



modify.table <-
  function(x,
           how = c(
             "add_attribute",
             "add_domain",
             "add_domainItem",
             "update_attribute",
             "update_domain",
             "update_domainItem"
           ),
           codeName = "",
           allowNull = "",
           dataType = "",
           definition = "",
           units = "",
           unitsResolution = "",
           isCaseSensitive = "",
           missingValue = "",
           minValue = "",
           maxValue = "",
           fieldWidth = "",
           domainId = "",
           domainName = "",
           domainDescription = "",
           domainItem_value = "",
           domainItem_name = "",
           domainItem_definition = "",
           quiet = FALSE) {
    `%>%` <- magrittr::`%>%`



    x.2 <- mdJSONdictio::build.mdJSON(x = x)



    input.dxnry <- modify.mdJSON(
      x.2 = ,
      how = how,
      codeName = codeName,
      allowNull = allowNull,
      dataType = dataType,
      definition = definition,
      units = units,
      unitsResolution = unitsResolution,
      isCaseSensitive = isCaseSensitive,
      missingValue = missingValue,
      minValue = minValue,
      maxValue = maxValue,
      fieldWidth = fieldWidth,
      domainId = domainId,
      domainName = domainName,
      domainDescription = domainDescription,
      domainItem_value = domainItem_value,
      domainItem_name = domainItem_name,
      domainItem_definition = domainItem_definition,
      quiet = FALSE
    )


  }
