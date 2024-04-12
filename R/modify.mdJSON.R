#' Modify mdJSON Data Dictionaries
#'
#' Amends an mdJSON data dictionary, including adding/updating attributes and domains.
#' @param x List object converted from an mdJSON file.
#' @param how Character string matching one of the modification options: see ‘Details’.
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
#' @param domain_domainId Conditionally optional character string. Universally Unique Identifier (UUID) of the attribute or domain specified with 'attribute_codeName' or domain_codeName', respectively.
#' @param domain_codeName Conditionally optional character string. Name of the domain to add or modify.
#' @param domain_description Optional character string. Description of the domain specified with 'domain_codeName'.
#' @param domainItem Optional list. Permissible entry value(s), name(s), and definition(s) of the domain specified with 'domain_codeName'.
#' @param domainItem_value Optional character string or vector. Permissible entry value(s) of the domain specified with 'domain_codeName'.
#' @param domainItem_name Optional character string or vector. Name(s) of permissible entry value(s) specified with 'domainItem_value'.
#' @param domainItem_definition Optional character string or vector. Definition(s) of permissible entry value(s) specified with 'domainItem_value'.
#' @param new.attribute_codeName Optional character string. New name for the attribute specified with 'attribute_codeName'.
#' @param new.domain_codeName Optional character string. New name for the domain specified with 'domain_codeName'.
#' @param quiet Optional logical. Default=FALSE. If TRUE, function runs with minimum input required and does not print revisions for approval.
#' @return Returns a modified list object corresponding to the mdJSON file.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```modify.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' input.dxnry <- rjson::fromJSON(file = path)
#'
#' # Add an attribute
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_attribute", attribute_codeName = "WingArea")
#'
#' # Add a domain
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domain")
#'
#' # Add a domain item
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domainItem", attribute_codeName = "WingArea", domainItem_value = "U")
#'
#' # Update attribute datatype and allowNull values
#' modified.dxnry<- modify.mdJSON(x = ref.dictionary, how = "update_attribute", attribute_codeName = "BandSize", attribute_dataType = "Integer", attribute_allowNull = TRUE)
#'
#' Update domain codeName
#' modified.dxnry<- modify.mdJSON(x = ref.dictionary, how = "update_domain", domain_codeName = "Wing_mm", new.domain_codeName = "WingFlattened_mm")
#'
#' # Convert list to JSON
#' new.json = rjson::toJSON(x = modified.dxnry)
#'
#' # Export JSON to disk
#' write(x = new.json, file = "e.g.dictionary.json")


# remove domain item, attribute, domain
# add_attribute, add_domain: allow domainItem inputs (vectors?)
## check if domainItem == "", then check if domainItem_value, etc == "", then ask for input

modify.mdJSON <-
  function(x,
           how = c(
             "add_attribute",
             "add_domain",
             "add_domainItem",
             "update_attribute",
             "update_domain",
             "update_domainItem"
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
           domainItem = list(list(name = "", value = "", definition = "")),
           domainItem_value = "",
           domainItem_name = "",
           domainItem_definition = "",
           new.attribute_codeName = "",
           new.domain_codeName = "",
           quiet = FALSE) {
    `%>%` <- magrittr::`%>%`

    blankattribute<- get0("blankattribute", envir = asNamespace("mdJSONdictio"))
    blankdictionarylist<- get0("blankdictionarylist", envir = asNamespace("mdJSONdictio"))
    blankdomain<- get0("blankdomain", envir = asNamespace("mdJSONdictio"))
    blankdomainItem<- get0("blankdomainItem", envir = asNamespace("mdJSONdictio"))
    dataType.vector<- get0("dataType.vector", envir = asNamespace("mdJSONdictio"))



    #### Extract dictionary list ####
    if (length(x[["data"]]) > 1) {
      input.dxnry <-
        extract.mdJSON(x = x,
                       record.type = "dictionaries",
                       multiple = FALSE)

      for (a in 1:length(x[["data"]])) {
        if (x[["data"]][[a]][["id"]] == input.dxnry[["data"]][[1]][["id"]]) {
          record.num <- a
        }
      }

    } else {
      input.dxnry <- x
    }


    dictionarylist <-
      rjson::fromJSON(input.dxnry[["data"]][[1]][["attributes"]][["json"]])


    #### Check parameters ####
    definedparam <- c()

    if (missing(attribute_codeName)) {
      attribute_codeName.input <- ""
    } else {
      attribute_codeName.input <- attribute_codeName
      if (is.null(attribute_codeName) == FALSE) {
        definedparam <- c(definedparam, "attribute_codeName")
      }
    }
    if (missing(attribute_allowNull)) {
      attribute_allowNull.input <- ""
    } else if (is.null(attribute_allowNull) == FALSE) {
      if (!attribute_allowNull %in% c(TRUE, FALSE)) {
        message("'attribute_allowNull' is invalid and will be ignored.")
        attribute_allowNull.input <- ""
      } else {
        attribute_allowNull.input <- attribute_allowNull
        definedparam <- c(definedparam, "attribute_allowNull")
      }
    } else {
      attribute_allowNull.input <- attribute_allowNull
    }
    if (missing(attribute_dataType)) {
      attribute_dataType.input <- ""
    } else if (is.null(attribute_dataType) == FALSE) {
      if (!attribute_dataType %in% dataType.vector) {
        message("'attribute_dataType' is invalid and will be ignored.")
        attribute_dataType.input <- ""
      } else {
        attribute_dataType.input <- attribute_dataType
        definedparam <- c(definedparam, "attribute_dataType")
      }
    } else {
      attribute_dataType.input <- attribute_dataType
    }
    if (missing(attribute_definition)) {
      attribute_definition.input <- ""
    } else {
      attribute_definition.input <- attribute_definition
      if (is.null(attribute_definition) == FALSE) {
        definedparam <- c(definedparam, "attribute_definition")
      }
    }
    if (missing(attribute_units)) {
      attribute_units.input <- ""
    } else {
      attribute_units.input <- attribute_units
      if (is.null(attribute_units) == FALSE) {
        definedparam <- c(definedparam, "attribute_units")
      }
    }
    if (missing(attribute_unitsResolution)) {
      attribute_unitsResolution.input <- ""
    } else if (is.null(attribute_unitsResolution) == FALSE) {
      if (suppressWarnings(is.na(as.numeric(attribute_unitsResolution))) == TRUE) {
        message("'attribute_unitsResolution' is invalid and will be ignored.")
        attribute_unitsResolution.input <- ""
      } else {
        attribute_unitsResolution.input <- attribute_unitsResolution
        definedparam <- c(definedparam, "attribute_unitsResolution")
      }
    } else {
      attribute_unitsResolution.input <- attribute_unitsResolution
    }
    if (missing(attribute_isCaseSensitive)) {
      attribute_isCaseSensitive.input <- ""
    } else if (is.null(attribute_isCaseSensitive) == FALSE) {
      if (!attribute_isCaseSensitive %in% c(TRUE, FALSE)) {
        message("'attribute_isCaseSensitive' is invalid and will be ignored.")
        attribute_isCaseSensitive.input <- ""
      } else {
        attribute_isCaseSensitive.input <- attribute_isCaseSensitive
        definedparam <- c(definedparam, "attribute_isCaseSensitive")
      }
    } else {
      attribute_isCaseSensitive.input <- attribute_isCaseSensitive
    }
    if (missing(attribute_missingValue)) {
      attribute_missingValue.input <- ""
    } else {
      attribute_missingValue.input <- attribute_missingValue
      if (is.null(attribute_missingValue) == FALSE) {
        definedparam <- c(definedparam, "attribute_missingValue")
      }
    }
    if (missing(attribute_minValue)) {
      attribute_minValue.input <- ""
    } else {
      attribute_minValue.input <- attribute_minValue
      if (is.null(attribute_minValue) == FALSE) {
        definedparam <- c(definedparam, "attribute_minValue")
      }
    }
    if (missing(attribute_maxValue)) {
      attribute_maxValue.input <- ""
    } else {
      attribute_maxValue.input <- attribute_maxValue
      if (is.null(attribute_maxValue) == FALSE) {
        definedparam <- c(definedparam, "attribute_maxValue")
      }
    }
    if (missing(attribute_fieldWidth)) {
      attribute_fieldWidth.input <- ""
    } else if (is.null(attribute_fieldWidth) == FALSE) {
      if (!grepl("[^[:digit:]]",
                 format(attribute_fieldWidth,
                        digits = 20,
                        scientific = FALSE)) == FALSE) {
        message("'attribute_fieldWidth' is invalid and will be ignored.")
        attribute_fieldWidth.input <- ""
      } else {
        attribute_fieldWidth.input <- attribute_fieldWidth
        definedparam <- c(definedparam, "attribute_fieldWidth")
      }
    } else {
      attribute_fieldWidth.input <- attribute_fieldWidth
    }
    if (missing(domain_domainId)) {
      domain_domainId.input <- ""
    } else if (is.null(domain_domainId) == FALSE) {
      if (!domain_domainId %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId")) {
        message(cat(paste0(
          "'domain_domainId' is invalid and will be ignored."
        )))
        domain_domainId.input <- ""
      } else {
        domain_domainId.input <- domain_domainId
        definedparam <- c(definedparam, "domain_domainId")
      }
    } else {
      domain_domainId.input <- domain_domainId
    }
    if (missing(domain_codeName)) {
      domain_codeName.input <- ""
    } else if (is.null(domain_codeName) == FALSE) {
      if (!domain_codeName %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
        message(cat(paste0(
          "'domain_codeName' is invalid and will be ignored."
        )))
        domain_codeName.input <- ""
      } else {
        domain_codeName.input <- domain_codeName
        definedparam <- c(definedparam, "domain_codeName")
      }
    } else {
      domain_codeName.input <- domain_codeName
    }
    if (missing(domain_description)) {
      domain_description.input <- ""
    } else {
      domain_description.input <- domain_description
      if (is.null(domain_description) == FALSE) {
        definedparam <- c(definedparam, "domain_description")
      }
    }
    if (missing(domainItem)) {
      domainItem.input <- ""
    } else {
      domainItem.input <- domainItem
      if (is.null(domainItem) == FALSE) {
        definedparam <- c(definedparam, "domainItem")
      }
    }
    if (missing(domainItem_value)) {
      domainItem_value.input <- ""
    } else {
      domainItem_value.input <- domainItem_value
      if (is.null(domainItem_value) == FALSE) {
        definedparam <- c(definedparam, "domainItem_value")
      }
    }
    if (missing(domainItem_name)) {
      domainItem_name.input <- ""
    } else {
      domainItem_name.input <- domainItem_name
      if (is.null(domainItem_name) == FALSE) {
        definedparam <- c(definedparam, "domainItem_name")
      }
    }
    if (missing(domainItem_definition)) {
      domainItem_definition.input <- ""
    } else {
      domainItem_definition.input <- domainItem_definition
      if (is.null(domainItem_definition) == FALSE) {
        definedparam <- c(definedparam, "domainItem_definition")
      }
    }
    if (missing(new.attribute_codeName)) {
      new.attribute_codeName <- ""
    } else {
      new.attribute_codeName.input <- new.attribute_codeName
      if (is.null(new.attribute_codeName) == FALSE) {
        definedparam <- c(definedparam, "new.attribute_codeName")
      }
    }
    if (missing(new.domain_codeName)) {
      new.domain_codeName <- ""
    } else {
      new.domain_codeName.input <- new.domain_codeName
      if (is.null(new.domain_codeName) == FALSE) {
        definedparam <- c(definedparam, "new.domain_codeName")
      }
    }
    if (missing(quiet)) {
      quiet <- FALSE
    }

    # definedparam <- definedparam[!mget(definedparam) %in% c("")]
    # definedparam <-
    #   names(Filter(Negate(is.null), mget(definedparam)))

    ## If attribute codeName required..
    if (how %in% c("add_attribute",
                   "add_domain",
                   "update_attribute")) {
      if (is.null(attribute_codeName.input)) {
        attribute_codeName.input <- ""
      }
      while (attribute_codeName.input == "") {
        message(cat(paste0(
          "\nREQUIRED: Provide the attribute name."
        )))
        attribute_codeName.input <-
          noquote(as.character(readline(prompt = )))
      }
      definedparam <- c(definedparam, "attribute_codeName")
    }

    ## If domain input required..
    if (how %in% c("add_domainItem",
                   "update_domain")) {
      if (is.null(domain_domainId.input)) {
        domain_domainId.input <- ""
      }
      if (is.null(domain_codeName.input)) {
        domain_codeName.input <- ""
      }
      if (is.null(domain_description.input)) {
        domain_description.input <- ""
      }
      if (domain_domainId.input == "" & domain_codeName.input == "") {
        domain.check <- 0

        while (domain.check == 0) {
          message(cat(
            paste0("\nREQUIRED: Provide the ID (UUID) or name of the domain.")
          ))
          domain.input <-
            noquote(as.character(readline(prompt =)))


          if (domain.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
            domain_codeName.input <- domain.input
            definedparam <- c(definedparam, "domain_codeName")
            domain.check <- 1
          } else if (domain.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId")) {
            domain_domainId.input <- domain.input
            definedparam <- c(definedparam, "domain_domainId")
            domain.check <- 1
          } else {
            message(cat(paste0("domain not found.\n")))

          }
        }
      }
    }

    ## Auto-fill domain input
    if (is.null(domain_codeName.input) == FALSE) {
      if (domain_codeName.input != "") {
        if (is.null(domain_domainId.input)) {
          domain_domainId.input <- ""
        }
        if (domain_domainId.input == "") {
          domain.num <-
            which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                    domain_codeName.input)

          ## Check if a single domain with the codeName exists
          if (length(domain.num) > 1) {
            message(cat(
              paste0(
                "Operation canceled. More than one domain with the name '",
                domain_codeName.input,
                "' was found. Correct one or both domains before proceeding."
              )
            ))
            how <-  "quit"
          } else if (length(domain.num) == 0) {
            message(cat(
              paste0(
                "Operation canceled. No domain with the name '",
                domain_codeName.input,
                "' was found."
              )
            ))
            how <-  "quit"
          } else {
            domain_domainId.input <-
              dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]
            definedparam <- c(definedparam, "domain_domainId")
          }
        }
      }
    } else if (is.null(domain_domainId.input) == FALSE) {
      if (domain_domainId.input != "") {
        if (is.null(domain_codeName.input)) {
          domain_codeName.input <- ""
        }
        if (domain_codeName.input == "") {
          domain.num <-
            which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId") ==
                    domain_domainId.input)

          if (length(domain.num) > 1) {
            message(cat(
              paste0(
                "Operation canceled. More than one domain with the name '",
                domain_codeName.input,
                "' was found. Correct one or both domains before proceeding."
              )
            ))
            how <-  "quit"
          } else if (length(domain.num) == 0) {
            message(cat(
              paste0(
                "Operation canceled. No domain with the name '",
                domain_codeName.input,
                "' was found."
              )
            ))
            how <-  "quit"
          } else {
            domain_codeName.input <-
              dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["codeName"]]
            definedparam <- c(definedparam, "domain_codeName")
          }
        }
      }
      ## Check domain Id and name match
    } else if (is.null(domain_domainId.input) == FALSE &
               is.null(domain_codeName.input) == FALSE) {
      if (domain_domainId.input != "" &
          domain_codeName.input != "") {
        domain.name.num <-
          which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                  domain_codeName.input)
        domain.Id.num <-
          which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId") ==
                  domain_domainId.input)
        if (domain.name.num != domain.Id.num) {
          domain_codeName.other <-
            dictionarylist[["dataDictionary"]][["domain"]][[domain.Id.num]][["codeName"]]

          domain_domainId.other <-
            dictionarylist[["dataDictionary"]][["domain"]][[domain.name.num]][["domainId"]]

          if (how %in% c("add_domainItem",
                         "update_domain")) {
            domain.choice <- 0
            while (domain.choice == 0) {
              domain.choice <-
                utils::menu(c(domain_codeName.input, domain_codeName.other),
                            title =
                              cat(
                                paste0(
                                  "\nREQUIRED: Domain ID and name do not correspond.\nWhich is correct?\n"
                                )
                              ))

            }
          } else {
            domain.choice <-
              utils::menu(c(domain_codeName.input, domain_codeName.other),
                          title =
                            cat(
                              paste0(
                                "\nREQUIRED: Domain ID and name do not correspond.\nWhich is correct?\nEnter '0' to omit this information.\n"
                              )
                            ))
          }
          if (domain.choice == 0) {
            domain_codeName.input <- ""
            domain_domainId.input <- ""
          }
          else if (domain.choice == 1) {
            domain_domainId.input <- domain_domainId.other
          } else if (domain.choice == 2) {
            domain_codeName.input <- domain_codeName.other
          }
        }
      }
    }

    ## Check domainItem

    # If domainItem is a vector of three variables
    if (is.null(domainItem.input) == FALSE) {
      if (length(domainItem) == 3 &
          !TRUE %in% sapply(domainItem.input, function(x)
            is.list(x))) {
        domainItem.input <-
          list(
            list(
              "name" = domainItem.input[1],
              "value" = domainItem.input[2],
              "definition" = domainItem.input[3]
            )
          )

        # If domainItem is a nested list with lengths of three
      } else if (!FALSE %in% sapply(domainItem.input, function(x)
        is.list(x)) &
        !FALSE %in% sapply(domainItem.input, function(x)
          length(x) == 3)) {
        #...determine if each list has the names "name", "value", and "definition"
        if (FALSE %in% sapply(domainItem.input, function(x)
          rlang::has_name(x, c("name", "value", "definition")))) {
          domainItem.input <-
            lapply(domainItem.input,
                   setNames,
                   c("name", "value", "definition"))

        }
      } else {
        message(cat(
          paste0("'domainItem' is incorrectly formatted and will be ignored.")
        ))
        domainItem.input <- ""
        definedparam <- definedparam[!definedparam == "domainItem"]
      }
    }

    ## Check domainItem_value, domainItem_name, and domainItem_definition
    if (is.null(domainItem_value.input) == FALSE &
        is.null(domainItem_name.input) == FALSE &
        is.null(domainItem_definition.input) == FALSE) {
      if (domainItem_value.input != "" &
          domainItem_name.input != "" &
          domainItem_definition.input != "") {
        if (domainItem.input == "") {
          # If the parameters are the same length
          if (length(domainItem_value.input) == length(domainItem_name.input) &
              length(domainItem_name.input) == length(domainItem_definition.input)) {
            # If the parameters describe more than one domainItem, transform them into a list
            if (length(domainItem_value.input) > 1) {
              domainItem.input <-
                lapply(1:length(domainItem_name.input), function(x)
                  list(
                    name = domainItem_name.input[[x]],
                    value = domainItem_value.input[[x]],
                    definition = domainItem_definition.input[[x]]
                  ))
            }
          } else {
            message(cat(
              paste0(
                "'domainItem_value', 'domainItem_name', and 'domainItem_definition' have different lengths and will be ignored."
              )
            ))
            domainItem_value.input <- ""
            domainItem_name.input <- ""
            domainItem_definition.input <- ""
            definedparam <-
              definedparam[!definedparam %in% c("domainItem_value",
                                                "domainItem_name",
                                                "domainItem_definition")]
          }

        } else {
          message(cat(
            paste0(
              "'domainItem' is defined, so 'domainItem_name', and 'domainItem_definition' will be ignored."
            )
          ))
          domainItem_value.input <- ""
          domainItem_name.input <- ""
          domainItem_definition.input <- ""
          definedparam <-
            definedparam[!definedparam %in% c("domainItem_value",
                                              "domainItem_name",
                                              "domainItem_definition")]

        }
      }
    }


    #### Add an attribute (and domain - optional) ####
    if (how == "add_attribute") {
      ## Check if attribute already exists
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                attribute_codeName.input)

      if (length(attribute.num) != 0) {
        message(cat(
          paste0(
            "Operation canceled. An attribute with the name '",
            attribute_codeName.input,
            "' already exists."
          )
        ))
      } else {
        newattribute <- blankattribute

        newattribute[[1]][["codeName"]] <- attribute_codeName.input

        ## Correct required parameters
        if (is.null(attribute_allowNull.input)) {
          attribute_allowNull.input <- ""
        }
        if (is.null(attribute_dataType.input)) {
          attribute_dataType.input <- ""
        }
        if (is.null(attribute_definition.input)) {
          attribute_definition.input <- ""
        }

        ## attribute allowNull
        if (attribute_allowNull.input == "") {
          allowNull.choice <- 0

          while (allowNull.choice == 0) {
            allowNull.choice <-
              utils::menu(c(TRUE, FALSE),
                          title =
                            cat(
                              paste0(
                                "\nREQUIRED: Are null values permitted for the attribute '",
                                attribute_codeName.input,
                                "'?\n"
                              )
                            ))
          }
          attribute_allowNull.input <- c(TRUE, FALSE)[allowNull.choice]
        }
        newattribute[[1]][["allowNull"]] <-
          attribute_allowNull.input


        ## attribute dataType
        if (attribute_dataType.input == "") {
          dataType.choice <- 0

          while (dataType.choice == 0) {
            dataType.choice <-
              utils::menu(c(dataType.vector),
                          title = cat(
                            paste0(
                              "\nREQUIRED: Select the datatype/format for entry values of the attribute '",
                              attribute_codeName.input,
                              "'.\n"
                            )
                          ))
          }
          attribute_dataType.input <- dataType.vector[dataType.choice]
        }
        newattribute[[1]][["dataType"]] <-
          attribute_dataType.input


        ## attribute definition
        if (attribute_definition.input == "") {
          while (attribute_definition.input == "") {
            message(cat(
              paste0(
                "\nREQUIRED: Provide a definition for the attribute '",
                attribute_codeName.input,
                "'."
              )
            ))
            attribute_definition.input <-
              noquote(as.character(readline(prompt =)))
          }
        }
        newattribute[[1]][["definition"]] <- attribute_definition.input


        ## attribute units
        if (is.null(attribute_units.input) == FALSE) {
          if (attribute_units.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the unit-of-measure for the attribute '",
                attribute_codeName.input,
                "' (e.g., meters, atmospheres, liters).\nPress Enter to omit this information."
              )
            ))
            attribute_units.input <- as.character(readline(prompt = ))
          }
          newattribute[[1]][["units"]] <- attribute_units.input
        }

        ## attribute unitsResolution
        if (is.null(attribute_unitsResolution.input) == FALSE) {
          if (attribute_unitsResolution.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the smallest unit increment (numeric) to which the attribute '",
                attribute_codeName.input,
                "' is measured (e.g., 1, .1, .01).\nPress Enter to omit this information."
              )
            ))
            attribute_unitsResolution.input <-
              noquote(as.character(readline(prompt = )))
          }

          if (attribute_unitsResolution.input != "") {
            while (suppressWarnings(is.na(as.numeric(attribute_unitsResolution.input))) == TRUE) {
              message(cat(
                paste0(
                  "\nInvalid entry: unit resolution must be numeric.\nPress Enter to omit this information."
                )
              ))
              attribute_unitsResolution.input <-
                as.character(readline(prompt =))

              if (attribute_unitsResolution.input == "")
              {
                break
              }

            }
          }

          newattribute[[1]][["unitsResolution"]] <-
            attribute_unitsResolution.input
        }

        ## attribute isCaseSensitive
        if (is.null(attribute_isCaseSensitive.input) == FALSE) {
          if (attribute_isCaseSensitive.input == "") {
            isCaseSensitive.choice <-
              utils::menu(c(TRUE, FALSE),
                          title =
                            cat(
                              paste0(
                                "\nOPTIONAL: Are the entry values of the attribute '",
                                attribute_codeName.input,
                                "' encoded in case-sensitive ASCII?\nEnter '0' to omit this information.\n"
                              )
                            ))
            if (isCaseSensitive.choice == 0) {
              attribute_isCaseSensitive.input <- ""
            } else{
              attribute_isCaseSensitive.input <- c(TRUE, FALSE)[isCaseSensitive.choice]
            }
          }
          newattribute[[1]][["isCaseSensitive"]] <-
            attribute_isCaseSensitive.input

        }

        ## attribute missingValue
        if (is.null(attribute_missingValue.input) == FALSE) {
          if (c(TRUE, FALSE)[allowNull.choice] == TRUE) {
            if (attribute_missingValue.input == "") {
              message(cat(
                paste0(
                  "\nOPTIONAL: Provide the code which represents missing entry values for the attribute '",
                  attribute_codeName.input,
                  "' (e.g., NA, na, -).\nPress Enter to omit this information."
                )
              ))
              attribute_missingValue.input <-
                noquote(as.character(readline(prompt = )))
              newattribute[[1]][["missingValue"]] <-
                attribute_missingValue.input
            }
          }
        }


        ## attribute minValue
        if (is.null(attribute_minValue.input) == FALSE) {
          if (attribute_minValue.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the minimum value permissible for the attribute '",
                attribute_codeName.input,
                "'.\nPress Enter to omit this information."
              )
            ))
            attribute_minValue.input <-
              noquote(as.character(readline(prompt = )))
          }
          newattribute[[1]][["minValue"]] <- attribute_minValue.input
        }

        ## attribute maxValue
        if (is.null(attribute_maxValue.input) == FALSE) {
          if (attribute_maxValue.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the maximum value permissible for the attribute '",
                attribute_codeName.input,
                "'.\nPress Enter to omit this information."
              )
            ))
            attribute_maxValue.input <- as.character(readline(prompt =))
          }
          newattribute[[1]][["maxValue"]] <- attribute_maxValue.input
        }

        ## attribute fieldWidth
        if (is.null(attribute_fieldWidth.input) == FALSE) {
          if (attribute_fieldWidth.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the field width (integer) of entry values for the attribute '",
                attribute_codeName.input,
                "'.\nPress Enter to omit this information."
              )
            ))
            attribute_fieldWidth.input <-
              noquote(as.character(readline(prompt = )))
          }

          while (!grepl("[^[:digit:]]",
                        format(
                          attribute_fieldWidth.input,
                          digits = 20,
                          scientific = FALSE
                        )) == FALSE) {
            message(cat(
              paste0(
                "\nThe field width must be an integer.\nPress Enter to omit this information."
              )
            ))
            attribute_fieldWidth.input <-
              noquote(as.character(readline(prompt = )))

          }

          newattribute[[1]][["fieldWidth"]] <- attribute_fieldWidth.input

        }

        ## Add an associated domain

        if (is.null(domain_domainId.input) == TRUE &
            is.null(domain_codeName.input) == TRUE) {
          # Add attribute to dictionary list with domainId input or no domain
          if (quiet == FALSE) {
            update.choice <-
              utils::menu(c("disregard changes",
                            "looks good"),
                          title = cat(
                            paste0(
                              "\nThe attribute '",
                              attribute_codeName.input,
                              "' is described as followed.\n"
                            ),
                            paste0(
                              jsonlite::toJSON(newattribute, pretty = TRUE, force = TRUE),
                              sep = "\n"
                            ),
                            paste0("\nDoes this look correct?")
                          ))
          } else {
            update.choice <- 2
          }

          if (update.choice == 2) {
            dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]]) +
                                                                                  1]] <-
              newattribute[[1]]
          }


        } else {

          domain.choice <- 0

          if ("domain_domainId" %in% definedparam) {
            newattribute[[1]][["domainId"]] <-
              domain_domainId.input
            domain.choice <- 3
          } else if ("domain_codeName" %in% definedparam) {
            domain.num <-
              which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                      domain_codeName.input)
            newattribute[[1]][["domainId"]] <-
              dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]
            domain.choice <- 3
          }

          while (domain.choice == 0) {
            domain.choice <-
              utils::menu(c("yes - create a new domain",
                            "yes - link to an existing domain",
                            "no"),
                          title = cat(
                            paste0(
                              "\nREQUIRED: Does the attribute '",
                              attribute_codeName.input,
                              "' have a domain (defined entry values)?\n"
                            )
                          ))

            if (domain.choice == 3) {
              ## Add attribute to dictionary list with domainId input or no domain
              if (quiet == FALSE) {
                update.choice <-
                  utils::menu(c("disregard changes",
                                "looks good"),
                              title = cat(
                                paste0(
                                  "\nThe attribute '",
                                  attribute_codeName.input,
                                  "' is described as followed.\n"
                                ),
                                paste0(
                                  jsonlite::toJSON(newattribute, pretty = TRUE, force = TRUE),
                                  sep = "\n"
                                ),
                                paste0("\nDoes this look correct?")
                              ))
              } else {
                update.choice <- 2
              }
              if (update.choice == 2) {
                dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]]) +
                                                                                      1]] <-
                  newattribute[[1]]
              }
            }

            if (domain.choice == 2) {
              message(cat(
                paste0(
                  "\nProvide the ID (UUID) or name of the domain to associate with the attribute '",
                  attribute_codeName.input,
                  "'.\nPress Enter to omit this information."
                )
              ))
              domain.input <-
                noquote(as.character(readline(prompt =)))

              if (domain.input == "") {
                domain.choice <- 3
              } else {
                if (domain.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId")) {
                  newattribute[[1]][["domainId"]] <-
                    domain.input

                  domain.choice <- 3
                } else if (domain.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
                  domain.num <-
                    which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                            domain.input)
                  newattribute[[1]][["domainId"]] <-
                    dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]

                  domain.choice <- 3

                } else {
                  message(cat(paste0("domain not found.\n")))

                  domain.choice <- 0
                }
              }
            }


            if (domain.choice == 1) {
              ## attribute domainId
              newattribute[[1]][["domainId"]] <-
                uuid::UUIDgenerate(use.time = FALSE, n = 1)

              ## Add attribute to dictionary list with domainId
              if (quiet == FALSE) {
                update.choice <-
                  utils::menu(c("disregard changes",
                                "looks good"),
                              title = cat(
                                paste0(
                                  "\nThe attribute '",
                                  attribute_codeName.input,
                                  "' is described as followed.\n"
                                ),
                                paste0(
                                  jsonlite::toJSON(newattribute, pretty = TRUE, force = TRUE),
                                  sep = "\n"
                                ),
                                paste0("\nDoes this look correct?")
                              ))
              } else {
                update.choice <- 2
              }
              if (update.choice == 2) {
                dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]]) +
                                                                                      1]] <-
                  newattribute[[1]]
              }

              ## Create a domain
              newdomain <- blankdomain

              newdomain[[1]][["domainId"]] <-
                newattribute[[1]][["domainId"]]

              ## Auto-fill domain based on attribute
              domainfill.choice <- 0

              while (domainfill.choice == 0) {
                domainfill.choice <-
                  utils::menu(c("yes (recommended)", "no"),
                              title = cat(
                                paste0(
                                  "\nREQUIRED: Auto-fill domain name and description based on the associated attribute '",
                                  attribute_codeName.input,
                                  "'?\n"
                                )
                              ))
              }

              if (domainfill.choice == 1) {
                newdomain[[1]][["codeName"]] <- attribute_codeName.input
                newdomain[[1]][["description"]] <-
                  newattribute[[1]][["definition"]]

                ## Provide new domain codeName and description
              } else {
                ## domain codeName
                if (domain_codeName.input == "") {
                  while (domain_codeName.input == "") {
                    message(cat(paste0(
                      "\nREQUIRED: Provide a name for the domain."
                    )))
                    domain_codeName.input <-
                      as.character(readline(prompt =))
                  }
                }
                newdomain[[1]][["codeName"]] <-
                  domain_codeName.input

                ## domain description
                if (domain_description.input == "") {
                  while (domain_description.input == "") {
                    message(cat(
                      paste0(
                        "\nREQUIRED: Provide a description for the domain '",
                        domain_codeName.input,
                        "'."
                      )
                    ))
                    domain_description.input <-
                      as.character(readline(prompt =))
                  }
                }
                newdomain[[1]][["description"]] <-
                  domain_description.input

              }

              ## Add a domainItem
              if (is.null(domainItem_value.input) == TRUE &
                  is.null(domainItem_name.input) == TRUE &
                  is.null(domainItem_definition.input) == TRUE) {
                domainItems.choice <- 2
              } else {
                domainItems.choice <- 0
              }

              while (domainItems.choice == 0) {
                domainItems.choice <-
                  utils::menu(c("yes", "no"),
                              title = cat(
                                paste0(
                                  "\nREQUIRED: Would you like to define entry values for the domain associated with '",
                                  attribute_codeName.input,
                                  "'?\n"
                                )
                              ))
              }

              ## Describe initial domainItem, append subsequent
              if (domainItems.choice == 1) {
                moredomainItems.choice <- 1

                while (moredomainItems.choice == 1) {
                  if (newdomain[[1]][["domainItem"]][[1]][["name"]] != "NA") {
                    newdomain[[1]][["domainItem"]] <-
                      append(newdomain[[1]][["domainItem"]], blankdomain[[1]][["domainItem"]])
                    domainItem.num <-
                      length(newdomain[[1]][["domainItem"]])
                  } else {
                    domainItem.num <- 1
                  }

                  ## domainItem name
                  domainItem_name.input <- ""

                  while (domainItem_name.input == "") {
                    message(cat(
                      paste0("\nREQUIRED: Provide a name for the new domain item.")
                    ))
                    domainItem_name.input <-
                      noquote(as.character(readline(prompt = )))
                  }
                  newdomain[[1]][["domainItem"]][[domainItem.num]][["name"]] <-
                    domainItem_name.input


                  ## domainItem value
                  domainItem_value.input <- ""

                  while (domainItem_value.input == "") {
                    message(cat(
                      paste0(
                        "\nREQUIRED: Provide an entry value for the domain item '",
                        domainItem_name.input,
                        "'."
                      )
                    ))
                    domainItem_value.input <-
                      as.character(readline(prompt =))
                  }
                  newdomain[[1]][["domainItem"]][[domainItem.num]][["value"]] <-
                    domainItem_value.input


                  ## domainItem definition
                  domainItem_defintion.input <- ""

                  while (domainItem_defintion.input == "") {
                    message(cat(
                      paste0(
                        "\nREQUIRED: Provide a definition for the domain item '",
                        domainItem_name.input,
                        "'."
                      )
                    ))
                    domainItem_defintion.input <-
                      noquote(as.character(readline(prompt = )))
                  }
                  newdomain[[1]][["domainItem"]][[domainItem.num]][["definition"]] <-
                    domainItem_defintion.input


                  ## Check for more domainItems
                  moredomainItems.choice <-
                    utils::menu(c("yes", "no"),
                                title = cat(
                                  paste0(
                                    "\nREQUIRED: Would you like to add an additional item to the domain?"
                                  )
                                ))


                }

              }
              if (quiet == FALSE) {
                update.choice <-
                  utils::menu(c("disregard changes",
                                "looks good"),
                              title = cat(
                                paste0(
                                  "\nThe domain '",
                                  domain_codeName.input,
                                  "' is described as followed.\n"
                                ),
                                paste0(
                                  jsonlite::toJSON(newdomain, pretty = TRUE, force = TRUE),
                                  sep = "\n"
                                ),
                                paste0("\nDoes this look correct?")
                              ))
              } else {
                update.choice <- 2
              }

              if (update.choice == 2) {
                ## Add domain to dictionary list
                dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) +
                                                                  1]] <-
                  newdomain[[1]]
              }

            }
          }

        }
      }
    }


    #### Add domain to existing attribute ####
    if (how == "add_domain") {
      ## Find attribute number
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                attribute_codeName.input)

      ## Check if a single attribute exists
      if (length(attribute.num) > 1) {
        message(cat(
          paste0(
            "Operation canceled. More than one attribute with the name '",
            attribute_codeName.input,
            "' was found. Correct one or both attributes before proceeding."
          )
        ))
      } else if (length(attribute.num) == 0) {
        message(cat(
          paste0(
            "Operation canceled. No attribute with the name '",
            attribute_codeName.input,
            "' was found."
          )
        ))

      } else {
        ## Check if the attribute already has a domain
        domainId.attribute <-
          dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]]

        ## Check if the domain exists
        domain.num <-
          which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId") ==
                  domainId.attribute)


        if (length(domain.num) == 1) {
          original.value <-
            dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["attribute_codeName"]]

          message(cat(
            paste0(
              "The domain '",
              original.value,
              "' is already associated with the attribute '",
              attribute_codeName.input,
              "'."
            )
          ))

          overwrite.choice <-
            utils::menu(c("yes", "no"),
                        title =
                          cat(
                            paste0("\nWould you like to associate a different domain?\n")
                          ))

          if (overwrite.choice == 1) {
            domain.num <- c()
          }

        }

        ## ..if not, check if a domain with the same codeName exists
        if (length(domain.num) == 0) {
          domain.num <-
            which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["codeName"]])

          if (length(domain.num) == 1) {
            domainlink.choice <- 0

            while (domainlink.choice == 0) {
              domainlink.choice <-
                utils::menu(c(
                  "associate the existing domain",
                  "create a new domain"
                ),
                title = cat(
                  paste0(
                    "\nREQUIRED: A domain with the same name as the attribute '",
                    attribute_codeName.input,
                    "' was found.\nHow would you like to proceed?\n"
                  )
                ))
            }
            if (domainlink.choice == 1) {
              dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]] <-
                dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]
            } else if (domainlink.choice == 2) {
              domain.num <- c()
            }
          }
        }

        if (length(domain.num) == 0) {
          ## attribute domainId
          newdomainId <-
            uuid::UUIDgenerate(use.time = FALSE, n = 1)

          newdomain <- blankdomain

          newdomain[[1]][["domainId"]] <-
            newdomainId

          ## Auto-fill domain based on attribute
          domainfill.choice <- 0

          while (domainfill.choice == 0) {
            domainfill.choice <-
              utils::menu(c("yes", "no"),
                          title = cat(
                            paste0(
                              "\nREQUIRED: Auto-fill domain name and description based on the associated attribute '",
                              attribute_codeName.input,
                              "' (recommended)?\n"
                            )
                          ))
          }

          if (domainfill.choice == 1) {
            newdomain[[1]][["codeName"]] <- attribute_codeName.input
            newdomain[[1]][["description"]] <-
              dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["definition"]]

            ## Provide new domain name and description
          } else {
            ## Correct required parameters
            if (is.null(domain_codeName.input)) {
              domain_codeName.input <- ""
            }
            if (is.null(domain_domainId.input)) {
              domain_domainId.input <- ""
            }
            if (is.null(domain_description.input)) {
              domain_domainId.input <- ""
            }

            ## domain codeName
            if (domain_codeName.input == "") {
              while (domain_codeName.input == "") {
                message(cat(
                  paste0("\nREQUIRED: Provide a name for the domain.")
                ))
                domain_codeName.input <-
                  noquote(as.character(readline(prompt = )))
              }
            }
            newdomain[[1]][["codeName"]] <-
              domain_codeName.input

            ## domain description
            if (domain_description.input == "") {
              while (domain_description.input == "") {
                message(cat(
                  paste0(
                    "\nREQUIRED: Provide a description for the domain '",
                    domain_codeName.input,
                    "'."
                  )
                ))
                domain_description.input <-
                  noquote(as.character(readline(prompt = )))
              }
            }
            newdomain[[1]][["description"]] <-
              domain_description.input

          }

          ## Add domain items
          if (is.null(domainItem_value.input) == TRUE &
              is.null(domainItem_name.input) == TRUE &
              is.null(domainItem_definition.input) == TRUE) {
            domainItems.choice <- 2
          } else {
            domainItems.choice <- 0
          }

          while (domainItems.choice == 0) {
            domainItems.choice <-
              utils::menu(c("yes", "no"),
                          title = cat(
                            paste0(
                              "\nREQUIRED: Would you like to add items to the domain associated with '",
                              attribute_codeName.input,
                              "'?\n"
                            )
                          ))
          }

          if (domainItems.choice == 1) {
            ## Describe initial domainItem, append subsequent
            moredomainItems.choice <- 1

            while (moredomainItems.choice == 1) {
              if (newdomain[[1]][["domainItem"]][[1]][["name"]] != "NA") {
                newdomain[[1]][["domainItem"]] <-
                  append(newdomain[[1]][["domainItem"]], blankdomain[[1]][["domainItem"]])
                domainItem.num <-
                  length(newdomain[[1]][["domainItem"]])
              } else {
                domainItem.num <- 1
              }

              ## domainItem name
              domainItem_name.input <- ""

              while (domainItem_name.input == "") {
                message(cat(
                  paste0(
                    "\nREQUIRED: Provide a name for the new domain item."
                  )
                ))
                domainItem_name.input <-
                  as.character(readline(prompt = ))
              }
              newdomain[[1]][["domainItem"]][[domainItem.num]][["name"]] <-
                domainItem_name.input


              ## domainItem value
              domainItem_value.input <- ""

              while (domainItem_value.input == "") {
                message(cat(
                  paste0(
                    "\nREQUIRED: Provide an entry value for the domain item '",
                    domainItem_name.input,
                    "'."
                  )
                ))
                domainItem_value.input <-
                  noquote(as.character(readline(prompt =)))
              }
              newdomain[[1]][["domainItem"]][[domainItem.num]][["value"]] <-
                domainItem_value.input


              ## domainItem definition
              domainItem_defintion.input <- ""

              while (domainItem_defintion.input == "") {
                message(cat(
                  paste0(
                    "\nREQUIRED: Provide a definition for the domain item '",
                    domainItem_name.input,
                    "'."
                  )
                ))
                domainItem_defintion.input <-
                  as.character(readline(prompt = ))
              }
              newdomain[[1]][["domainItem"]][[domainItem.num]][["definition"]] <-
                domainItem_defintion.input


              ## Check for more domainItems
              moredomainItems.choice <-
                utils::menu(c("yes", "no"),
                            title = cat(
                              paste0(
                                "\nREQUIRED: Would you like to add an additional item to the domain?"
                              )
                            ))


            }
          }

          ## Add domain to dictionary list
          if (quiet == FALSE) {
            update.choice <-
              utils::menu(c("disregard changes",
                            "looks good"),
                          title = cat(
                            paste0("\nThe domain '",
                                   domain_codeName.input,
                                   "' is described as followed.\n"),
                            paste0(jsonlite::toJSON(
                              newdomain, pretty = TRUE, force = TRUE
                            ), sep = "\n"),
                            paste0("\nDoes this look correct?")
                          ))
          } else {
            update.choice <- 2
          }
          if (update.choice == 2) {
            dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]] <-
              newdomainId

            dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) +
                                                              1]] <-
              newdomain[[1]]
          }

        }
      }
    }

    #### Add domainItem ####
    if (how == "add_domainItem") {
      domain.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId") ==
                domain_domainId.input)

      newdomainItems <- blankdomainItem
      domainItem.num <- 1

      ## Correct required parameters
      if (is.null(domain_codeName.input)) {
        domain_codeName.input <- ""
      }
      if (is.null(domain_domainId.input)) {
        domain_domainId.input <- ""
      }
      if (is.null(domainItem_value.input)) {
        domainItem_value.input <- ""
      }
      if (is.null(domainItem_name.input)) {
        domainItem_name.input <- ""
      }
      if (is.null(domainItem_definition.input)) {
        domainItem_definition.input <- ""
      }
      ## domainItem value
      while (domainItem_value.input == "") {
        message(cat(
          paste0(
            "\nREQUIRED: Provide the new entry value for the domain '",
            domain_codeName.input,
            "'."
          )
        ))
        domainItem_value.input <-
          noquote(as.character(readline(prompt = )))
      }
      newdomainItems[[domainItem.num]][["value"]] <-
        domainItem_value.input


      ## domainItem name
      while (domainItem_name.input == "") {
        message(cat(
          paste0(
            "\nREQUIRED: Provide a name for the new entry value '",
            domainItem_value.input,
            "'."
          )
        ))
        domainItem_name.input <-
          noquote(as.character(readline(prompt = )))
      }
      newdomainItems[[domainItem.num]][["name"]] <-
        domainItem_name.input

      ## domainItem definition
      while (domainItem_definition.input == "") {
        message(cat(
          paste0(
            "\nREQUIRED: Provide a definition for the domain item '",
            domainItem_value.input,
            "'."
          )
        ))
        domainItem_definition.input <-
          as.character(readline(prompt =))
      }
      newdomainItems[[domainItem.num]][["definition"]] <-
        domainItem_definition.input


      ## Add domainItem to the domain in the dictionary list

      if (quiet == FALSE) {
        update.choice <-
          utils::menu(c("disregard changes",
                        "looks good"),
                      title = cat(
                        paste0("\nThe domain item value '",
                               domainItem_value.input,
                               "' is described as followed.\n"),
                        paste0(jsonlite::toJSON(
                          newdomainItems, pretty = TRUE, force = TRUE
                        ), sep = "\n"),
                        paste0("\nDoes this look correct?")
                      ))
      } else {
        update.choice <- 2
      }
      if (update.choice == 2) {
        for (a in 1:length(newdomainItems)) {
          dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[length(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]]) +
                                                                                          1]] <-
            newdomainItems[[a]]
        }
      }
    }

    #### Update attribute ####
    if (how == "update_attribute") {
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                attribute_codeName.input)

      ## Check if a single attribute exists
      if (length(attribute.num) > 1) {
        message(cat(
          paste0(
            "Operation canceled. More than one attribute with the name '",
            attribute_codeName.input,
            "' was found. Correct one or both attributes before proceeding."
          )
        ))
      } else if (length(attribute.num) == 0) {
        message(cat(
          paste0(
            "Operation canceled. No attribute with the name '",
            attribute_codeName.input,
            "' was found."
          )
        ))

      }

      originalattribute <-
        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]]

      newattribute <- originalattribute


      ## Make changes based on inputs
      attributeparam <- c(
        "new.attribute_codeName",
        "attribute_allowNull",
        "attribute_dataType",
        "attribute_definition",
        "attribute_units",
        "attribute_unitsResolution",
        "attribute_isCaseSensitive",
        "attribute_missingValue",
        "attribute_minValue",
        "attribute_maxValue",
        "attribute_fieldWidth",
        "domain_domainId"
      )

      if (length(definedparam %in% attributeparam) != 0) {

        ## attribute codeName
        if (is.null(new.attribute_codeName) == FALSE) {
          if (new.attribute_codeName.input != "") {
            if (new.attribute_codeName.input %in% sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName")) {
              message(cat(
                paste0(
                  "\nAn attribute with the name '",
                  new.attribute_codeName.input,
                  "' already exists. \nThe current name will be retained."
                )
              ))
            } else {
              newattribute[["codeName"]] <- new.attribute_codeName.input

              if (quiet == FALSE) {
                domain.num <-
                  which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                          attribute_codeName.input)
                if (length(domain.num) == 1) {
                  domain.choice <-
                    utils::menu(c("please do",
                                  "most certaintly not"),
                                title = cat(
                                  paste0(
                                    "\nA domain with the name '",
                                    attribute_codeName.input,
                                    "' also exists.\nWould you like to change the name to '",
                                    new.attribute_codeName.input,
                                    "'?\n"
                                  )
                                ))

                  if (domain.choice == 1) {
                    dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["codeName"]] <-
                      new.attribute_codeName.input
                  }

                }
              }

              attribute_codeName.input <-
                new.attribute_codeName.input
            }
          }
        }

        ## attribute allowNull
        if (is.null(attribute_allowNull.input) == FALSE) {
          if (attribute_allowNull.input != "") {
            newattribute[["allowNull"]] <- attribute_allowNull.input
          }
        }

        ## attribute dataType
        if (is.null(attribute_dataType.input) == FALSE) {
          if (attribute_dataType.input != "") {
            newattribute[["dataType"]] <- attribute_dataType.input
          }
        }

        ## attribute unitsResolution
        if (is.null(attribute_unitsResolution.input) == FALSE) {
          if (attribute_unitsResolution.input != "") {
            newattribute[["unitsResolution"]] <- attribute_unitsResolution.input
          }
        }

        ## attribute isCaseSensitive
        if (is.null(attribute_isCaseSensitive.input) == FALSE) {
          if (attribute_isCaseSensitive.input != "") {
            newattribute[["isCaseSensitive"]] <- attribute_isCaseSensitive.input
          }
        }

        ## attribute missingValue
        if (is.null(attribute_missingValue.input) == FALSE) {
          if (attribute_missingValue.input != "") {
            newattribute[["missingValue"]] <- attribute_missingValue.input
          }
        }

        ## attribute minValue
        if (is.null(attribute_minValue.input) == FALSE) {
          if (attribute_minValue.input != "") {
            newattribute[["minValue"]] <- attribute_minValue.input
          }
        }

        ## attribute maxValue
        if (is.null(attribute_maxValue.input) == FALSE) {
          if (attribute_maxValue.input != "") {
            newattribute[["maxValue"]] <- attribute_maxValue.input
          }
        }

        ## attribute fieldWidth
        if (is.null(attribute_fieldWidth.input) == FALSE) {
          if (attribute_fieldWidth.input != "") {
            newattribute[["fieldWidth"]] <- attribute_fieldWidth.input
          }
        }

        ## attribute domainId
        if (is.null(domain_domainId.input) == FALSE) {
          if (domain_domainId.input != "") {
            newattribute[["domainId"]] <- domain_domainId.input
          }
        }

        if (quiet == FALSE) {
          update.choice <-
            utils::menu(
              c(
                "disregard changes",
                "make additional changes",
                "looks good"
              ),
              title = cat(
                paste0(
                  "\nThe attribute '",
                  attribute_codeName.input,
                  "' has been updated as followed.\n"
                ),
                paste0(jsonlite::toJSON(newattribute, pretty = TRUE, force = TRUE), sep = "\n"),
                paste0("\nDoes this look correct?")
              )
            )
        } else {
          update.choice <- 3
        }
      } else {
        if (quiet == FALSE) {
          update.choice <-
            utils::menu(c("keep as is", "make changes"),
                        title = cat(
                          paste0("\nThe attribute '", attribute_codeName.input, "' is described as followed.\n"),
                          paste0(
                            jsonlite::toJSON(originalattribute, pretty = TRUE, force = TRUE),
                            sep = "\n"
                          ),
                          paste0("\nWould you like to do?")
                        ))

        } else {
          update.choice <- 1
        }
      }

      ## Make additional changes
      attributefield <- c(
        "codeName",
        "allowNull",
        "dataType",
        "definition",
        "units",
        "unitsResolution",
        "isCaseSensitive",
        "missingValue",
        "minValue",
        "maxValue",
        "fieldWidth",
        "domainId"
      )

      while (update.choice == 2) {
        field.choice <-
          utils::menu(attributefield,
                      title =
                        cat(paste0(
                          "\nWhich attribute field would you like to add/modify?\n"
                        )))


        # Create field if it doesn't exist
        if (attributefield[field.choice] %in% names(newattribute) == FALSE) {
          newfield <- list()
          newfield[[paste0(attributefield[field.choice])]] <- ""
          newattribute <- append(newattribute, newfield)
        }

        ## attribute codeName
        if (attributefield[field.choice] == "codeName") {
          originalvalue <- newattribute[["codeName"]]

          message(cat(
            paste0(
              "\nProvide an attribute name. \nThe current name is '",
              originalvalue,
              "'.\nPress Enter to abort changes."
            )
          ))

          new.attribute_codeName.input <-
            noquote(as.character(readline(prompt = )))


          if (new.attribute_codeName.input != "") {
            if (new.attribute_codeName.input %in% sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName")) {
              message(cat(
                paste0(
                  "\nAn attribute with the name '",
                  new.attribute_codeName.input,
                  "' already exists. \nThe current name will be retained."
                )
              ))

            } else {
              newattribute[["codeName"]] <-
                new.attribute_codeName.input

              if (quiet == FALSE) {
                domain.num <-
                  which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                          attribute_codeName.input)
                if (length(domain.num) == 1) {
                  domain.choice <-
                    utils::menu(c("please do",
                                  "most certaintly not"),
                                title = cat(
                                  paste0(
                                    "\nA domain with the name '",
                                    attribute_codeName.input,
                                    "' also exists.\nWould you like to change the name to '",
                                    new.attribute_codeName.input,
                                    "'?\n"
                                  )
                                ))

                  if (domain.choice == 1) {
                    dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["codeName"]] <-
                      new.attribute_codeName.input
                  }
                }
              }


              attribute_codeName.input <-
                new.attribute_codeName.input
            }
          }

          ## attribute allowNull
        } else if (attributefield[field.choice] == "allowNull") {
          allowNull.choice <- 0
          originalvalue <- newattribute[["allowNull"]]

          allowNull.choice <-
            utils::menu(c(TRUE, FALSE),
                        title =
                          cat(
                            paste0(
                              "\nAre null values permitted for the attribute '",
                              attribute_codeName.input,
                              "'? \nThe current value is '",
                              originalvalue,
                              "'.\n"
                            )
                          ))

          if (allowNull.choice %in% c(1, 2)) {
            attribute_allowNull.input <- c(TRUE, FALSE)[allowNull.choice]
            newattribute[["allowNull"]] <-
              attribute_allowNull.input
          }

          ## attribute dataType
        } else if (attributefield[field.choice] == "dataType") {
          dataType.choice <- 0
          originalvalue <- newattribute[["dataType"]]

          dataType.choice <-
            utils::menu(c(dataType.vector),
                        title = cat(
                          paste0(
                            "\nSelect the datatype/format for entry values of the attribute '",
                            attribute_codeName.input,
                            "'. \nThe current datatype is '",
                            originalvalue,
                            "'.\n"
                          )
                        ))

          if (dataType.choice != 0) {
            attribute_dataType.input <- dataType.vector[dataType.choice]

            newattribute[["dataType"]] <-
              attribute_dataType.input
          }

          ## attribute definition
        } else if (attributefield[field.choice] == "definition") {
          originalvalue <- newattribute[["definition"]]

          message(cat(
            paste0(
              "\nProvide a new definition for the attribute '",
              attribute_codeName.input,
              "'. \nThe current definition is '",
              originalvalue,
              "'\nPress Enter to abort changes."
            )
          ))
          attribute_definition.input <- as.character(readline(prompt =))


          if (attribute_definition.input != "") {
            newattribute[["definition"]] <- attribute_definition.input
          }

          ## attribute units
        } else if (attributefield[field.choice] == "units") {
          originalvalue <- newattribute[["units"]]

          message(cat(
            paste0(
              "\nProvide the unit-of-measure for the attribute '",
              attribute_codeName.input,
              "' (e.g., meters, atmospheres, liters). \nThe current unit is '",
              originalvalue,
              "'. \nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          attribute_units.input <-
            noquote(as.character(readline(prompt = )))

          if (attribute_units.input == "rm()") {
            newattribute[["units"]] <- ""
          } else if (attribute_units.input != "") {
            newattribute[["units"]] <- attribute_units.input
          }

          ## attribute unitsResolution
        } else if (attributefield[field.choice] == "unitsResolution") {
          originalvalue <- newattribute[["unitsResolution"]]

          message(cat(
            paste0(
              "\nProvide the smallest unit increment (numeric) to which the attribute '",
              attribute_codeName.input,
              "' is measured (e.g., 1, .1, .01). \nThe current unit resolution is '",
              originalvalue,
              "'\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          attribute_unitsResolution.input <-
            noquote(as.character(readline(prompt = )))

          if (attribute_unitsResolution.input == "rm()") {
            newattribute[["unitsResolution"]] <-
              ""
          } else if (suppressWarnings(is.na(as.numeric(attribute_unitsResolution.input))) == TRUE) {
            message(
              "\nInvalid entry: unit resolution must be numeric.\nThe current resolution will be retained."
            )
          } else {
            newattribute[["unitsResolution"]] <-
              attribute_unitsResolution.input
          }


          ## attribute isCaseSensitive
        } else if (attributefield[field.choice] == "isCaseSensitive") {
          isCaseSensitive.choice <- 0
          originalvalue <- newattribute[["isCaseSensitive"]]

          isCaseSensitive.choice <-
            utils::menu(c(TRUE, FALSE),
                        title =
                          cat(
                            paste0(
                              "\nAre the entry values of the attribute '",
                              attribute_codeName.input,
                              "' encoded in case-sensitive ASCII?\nThe current value is '",
                              originalvalue,
                              "'.\n"
                            )
                          ))

          if (isCaseSensitive.choice %in% c(1, 2)) {
            attribute_isCaseSensitive.input <- c(TRUE, FALSE)[isCaseSensitive.choice]

            newattribute[["isCaseSensitive"]] <-
              attribute_isCaseSensitive.input
          }

          ## attribute missingValue
        } else if (attributefield[field.choice] == "missingValue") {
          originalvalue <- newattribute[["missingValue"]]

          if ("allowNull" %in% names(newattribute) == TRUE &
              newattribute[["allowNull"]] == FALSE) {
            allowNull.check <-
              utils::menu(c("yes", "no"),
                          title =
                            cat(
                              paste0(
                                "\nThe dictionary indicates the attribute '",
                                attribute_codeName.input,
                                "' does not allow null values.\nWould you still like to proceed?\n"
                              )
                            ))


          } else {
            allowNull.check <- 1
          }

          if (allowNull.check == 1) {
            message(cat(
              paste0(
                "\nProvide the code which represents missing entry values for the attribute '",
                attribute_codeName.input,
                "' (e.g., NA, na, -).\nThe current code is '",
                originalvalue,
                "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
              )
            ))
            attribute_missingValue.input <-
              noquote(as.character(readline(prompt = )))

            if (attribute_missingValue.input == "rm()") {
              newattribute[["missingValue"]] <- ""
            } else if (attribute_missingValue.input != "") {
              newattribute[["missingValue"]] <-
                attribute_missingValue.input
            }

          } else {
            message(cat(paste0("\nChanges aborted.")))
          }

          ## attribute minValue
        } else if (attributefield[field.choice] == "minValue") {
          originalvalue <- newattribute[["minValue"]]

          message(cat(
            paste0(
              "\nProvide the minimum value permissible for the attribute '",
              attribute_codeName.input,
              "'.\nThe current minimum value is '",
              originalvalue,
              "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          attribute_minValue.input <-
            noquote(as.character(readline(prompt = )))

          if (attribute_minValue.input == "rm()") {
            newattribute[["minValue"]] <- ""
          } else if (attribute_minValue.input != "") {
            newattribute[["minValue"]] <- attribute_minValue.input
          }

          ## attribute maxValue
        } else if (attributefield[field.choice] == "maxValue") {
          originalvalue <- newattribute[["maxValue"]]

          message(cat(
            paste0(
              "\nProvide the maximum value permissible for the attribute '",
              attribute_codeName.input,
              "'.\nThe current maximum value is '",
              originalvalue,
              "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          attribute_maxValue.input <-
            noquote(as.character(readline(prompt = )))


          if (attribute_maxValue.input == "rm()") {
            newattribute[["maxValue"]] <- ""
          } else if (attribute_minValue.input != "") {
            newattribute[["maxValue"]] <- attribute_maxValue.input
          }

          ## attribute fieldWidth
        } else if (attributefield[field.choice] == "fieldWidth") {
          originalvalue <- newattribute[["fieldWidth"]]

          message(cat(
            paste0(
              "\nProvide the field width (integer) of entry values for the attribute '",
              attribute_codeName.input,
              "'.\nThe current field width is '",
              originalvalue,
              "'\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          attribute_fieldWidth.input <-
            noquote(as.character(readline(prompt = )))

          if (attribute_fieldWidth.input == "rm()") {
            newattribute[["fieldWidth"]] <- ""
          } else if (attribute_fieldWidth.input != "") {
            if (!grepl("[^[:digit:]]",
                       format(
                         attribute_fieldWidth.input,
                         digits = 20,
                         scientific = FALSE
                       )) == FALSE) {
              message(
                "\nInvalid entry: field width must be an integer.\nThe current field width will be retained."
              )
            } else {
              newattribute[["fieldWidth"]] <-
                attribute_fieldWidth.input
            }
          }
          ## attribute domainId
        } else if (attributefield[field.choice] == "domainId") {
          originalvalue.id <- newattribute[["domainId"]]

          domain.num <-
            which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId") ==
                    originalvalue.id)
          originalvalue.name <-
            dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["codeName"]]

          message(cat(
            paste0(
              "\nProvide the ID (UUID) or name of the domain to associate with the attribute '",
              attribute_codeName.input,
              "'.\nThe name of the current domain is '",
              originalvalue.name,
              "'\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          domain.input <-
            noquote(as.character(readline(prompt =)))

          if (domain.input == "rm()") {
            newattribute[["domainId"]] <- ""
          } else if (domain.input != "") {
            if (domain.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId")) {
              newattribute[["domainId"]] <-
                domain.input
            } else if (domain.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
              domain.num <-
                which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                        domain.input)
              newattribute[["domainId"]] <-
                dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]
            } else {
              message(cat(
                paste0(
                  "\nInvalid entry: no domain found.\nThe current domain Id will be retained."
                )
              ))
            }
          }
        }


        if (quiet == FALSE) {
          update.choice <-
            utils::menu(
              c(
                "disregard changes",
                "make additional changes",
                "looks good"
              ),
              title = cat(
                paste0(
                  "\nThe attribute '",
                  attribute_codeName.input,
                  "' has been updated as followed.\n"
                ),
                paste0(
                  jsonlite::toJSON(newattribute, pretty = TRUE, force = TRUE),
                  sep = "\n"
                ),
                paste0("\nDoes this look correct?")
              )
            )
        } else {
          update.choice <- 3
        }
      }

      if (update.choice == 1) {
        message("Operation canceled.")

      } else if (update.choice == 3) {
        # update attribute in dictionary
        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]] <-
          newattribute


      }
    }

    #### Update domain ####
    if (how == "update_domain") {
      originaldomain <-
        dictionarylist[["dataDictionary"]][["domain"]][[domain.num]]

      newdomain <- originaldomain


      ## Make changes based on inputs
      domainparam <- c("new.domain_codeName", "domain_description")

      if (length(definedparam %in% domainparam) != 0) {

        ## domain codeName
        if (is.null(new.domain_codeName.input) == FALSE) {
          if (new.domain_codeName.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
            message(cat(
              paste0(
                "\nAn attribute with the name '",
                new.domain_codeName.input,
                "' already exists. \nThe current name will be retained."
              )
            ))
          } else {
            newdomain[["codeName"]] <- new.domain_codeName.input

            if (quiet == FALSE) {
              attribute.num <-
                which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                        domain_codeName.input)

              if (length(attribute.num) == 1) {
                attribute.choice <-
                  utils::menu(c("please do",
                                "most certaintly not"),
                              title = cat(
                                paste0(
                                  "\nAn attribute with the name '",
                                  domain_codeName.input,
                                  "' also exists.\nWould you like to change the name to '",
                                  new.domain_codeName.input,
                                  "'?\n"
                                )
                              ))

                if (attribute.choice == 1) {
                  dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["codeName"]] <-
                    new.domain_codeName.input
                }

              }
            }
            domain_codeName.input <- new.domain_codeName.input

          }
        }


        ## domain description
        if (is.null(domain_description.input) == FALSE) {
          if (domain_description.input != "") {
            newdomain[["description"]] <- domain_description.input
          }
        }

        if (quiet == FALSE) {
          update.choice <-
            utils::menu(
              c(
                "disregard changes",
                "make additional changes",
                "looks good"
              ),
              title = cat(
                paste0(
                  "\nThe domain '",
                  domain_codeName.input,
                  "' has been updated as followed.\n"
                ),
                paste0(
                  jsonlite::toJSON(newdomain, pretty = TRUE, force = TRUE),
                  sep = "\n"
                ),
                paste0("\nDoes this look correct?")
              )
            )
        } else {
          update.choice <- 3
        }
      } else {
        if (quiet == FALSE) {
          update.choice <-
            utils::menu(c("keep as is", "make changes"),
                        title = cat(
                          paste0(
                            "\nThe domain '",
                            domain_codeName.input,
                            "' is described as followed.\n"
                          ),
                          paste0(
                            jsonlite::toJSON(originaldomain, pretty = TRUE, force = TRUE),
                            sep = "\n"
                          ),
                          paste0("\nWould you like to do?")
                        ))

        } else {
          update.choice <- 1
        }
      }

      ## Make additional changes
      domainfield <- c("codeName", "description")

      while (update.choice == 2) {
        field.choice <-
          utils::menu(domainfield,
                      title =
                        cat(paste0(
                          "\nWhich domain field would you like to add/modify?\n"
                        )))


        # Create field if it doesn't exist
        if (domainfield[field.choice] %in% names(newdomain) == FALSE) {
          newfield <- list()
          newfield[[paste0(domainfield[field.choice])]] <- ""
          newdomain <- append(newdomain, newfield)
        }

        # domain codeName
        if (domainfield[field.choice] == "codeName") {

          originalvalue <- newdomain[["codeName"]]

          message(cat(
            paste0(
              "\nProvide a domain name. \nThe current name is '",
              originalvalue,
              "'.\nPress Enter to abort changes."
            )
          ))

          new.domain_codeName.input <-
            noquote(as.character(readline(prompt = )))


          if (new.domain_codeName.input != "") {
            if (new.domain_codeName.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
              message(cat(
                paste0(
                  "\nA domain with the name '",
                  new.domain_codeName.input,
                  "' already exists. \nThe current name will be retained."
                )
              ))

            } else {
              newdomain[["codeName"]] <-
                new.domain_codeName.input

              if (quiet == FALSE) {
                attribute.num <-
                  which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                          domain_codeName.input)

                if (length(attribute.num) == 1) {
                  attribute.choice <-
                    utils::menu(c("please do",
                                  "most certaintly not"),
                                title = cat(
                                  paste0(
                                    "\nAn attribute with the name '",
                                    domain_codeName.input,
                                    "' also exists.\nWould you like to change the name to '",
                                    new.domain_codeName.input,
                                    "'?\n"
                                  )
                                ))

                  if (attribute.choice == 1) {
                    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["codeName"]] <-
                      new.domain_codeName.input
                  }
                }
              }

              domain_codeName.input <- new.domain_codeName.input

            }
          }

          # domain description
        } else if (domainfield[field.choice] == "description") {
          originalvalue <- newdomain[["description"]]

          message(cat(
            paste0(
              "\nProvide the description for the domain '",
              domain_codeName.input,
              "'\nThe current description is '",
              originalvalue,
              "'.\nPress Enter to abort changes."
            )
          ))
          domain_description.input <-
            noquote(as.character(readline(prompt =)))

          if (domain_description.input != "") {
            newdomain[["description"]] <- domain_description.input
          }

        }

        if (quiet == FALSE) {
          update.choice <-
            utils::menu(
              c(
                "disregard changes",
                "make additional changes",
                "looks good"
              ),
              title = cat(
                paste0(
                  "\nThe domain '",
                  domain_codeName.input,
                  "' has been updated as followed.\n"
                ),
                paste0(
                  jsonlite::toJSON(newdomain, pretty = TRUE, force = TRUE),
                  sep = "\n"
                ),
                paste0("\nDoes this look correct?")
              )
            )
        } else {
          update.choice <- 3
        }
      }

      if (update.choice == 1) {
        message("Operation canceled.")

      } else if (update.choice == 3) {
        # update attribute in dictionary
        dictionarylist[["dataDictionary"]][["domain"]][[domain.num]] <-
          newdomain


      }

}


    #### Reattach dictionary list ####
    lastUpdate <- strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M")
    lastUpdate <- paste0(lastUpdate, ":00.000Z", collapse = "")

    lastUpdate.num<- c()

    for (aa in 1:length(dictionarylist[["dataDictionary"]][["citation"]][["date"]])) {
      if (dictionarylist[["dataDictionary"]][["citation"]][["date"]][[aa]][["dateType"]] ==
          "lastUpdate") {
        lastUpdate.num <- c(lastUpdate.num, aa)
      }
    }

    if (is.null(lastUpdate.num) == FALSE) {
      if (length(lastUpdate.num) > 1) {
        dictionarylist[["dataDictionary"]][["citation"]][["date"]] <-
          dictionarylist[["dataDictionary"]][["citation"]][["date"]][-lastUpdate.num[-1]]
      }
      dictionarylist[["dataDictionary"]][["citation"]][["date"]][[lastUpdate.num[1]]][["date"]] <-
        lastUpdate
    } else {
      dictionarylist[["dataDictionary"]][["citation"]][["date"]][[length(dictionarylist[["dataDictionary"]][["citation"]][["date"]]) +
                                                                    1]] <-
        list(date = lastUpdate, dateType = "lastUpdate")
    }


    input.dxnry[["data"]][[1]][["attributes"]][["json"]] <-
      rjson::toJSON(dictionarylist)

    if (length(x[["data"]]) > 1) {
      x[["data"]][[record.num]] <- input.dxnry[["data"]][[1]]

      return(x)
    } else {
      return(input.dxnry)
    }
  }
