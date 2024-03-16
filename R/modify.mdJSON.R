#' Modify mdJSON Data Dictionaries
#'
#' Amends an mdJSON data dictionary, including adding/updating attributes and domains.
#' @param x List object converted from an mdJSON file.
#' @param how character string matching one of the modification options: see ‘Details’.
#' @param codeName a string representing the attribute to add or modify.
#' @param domainItem_value a string representing the domain item (entry value) to add to an attribute's domain.
#' @return Returns a modified list object corresponding to the mdJSON data dictionary file.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```modify.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' input.dxnry <- rjson::fromJSON(file = path)
#'
#' # Add an attribute
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_attribute", codeName = "WingArea")
#'
#' # Add a domain
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domain")
#'
#' # Add a domain item
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domainItem", codeName = "WingArea", domainItem_value = "U")
#'
#' # Convert list to JSON
#' new.json = rjson::toJSON(x = modified.dxnry)
#'
#' # Export JSON to disk
#' write(x = new.json, file = "e.g.dictionary.json")

# parameter to turn off update prompts

modify.mdJSON <-
  function(x,
           how = c("add_attribute",
                   "add_domain",
                   "add_domainItem",
                   "update_attribute"),
           codeName,
           domainItem,
           allowNull,
           dataType,
           definition,
           units,
           unitsResolution,
           isCaseSensitive,
           missingValue,
           minValue,
           maxValue,
           fieldWidth) {
    `%>%` <- magrittr::`%>%`

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


    if (missing(codeName)) {
      codeName <- ""
      while (codeName == "") {
        message(cat(
          paste0("\nREQUIRED: Provide the attribute or domain codeName.")
        ))
        codeName <- as.character(readline(prompt = ))
      }
    }

    if (missing(domainItem)) {
      domainItem_value.input <- ""
    } else {
      domainItem_value.input <- domainItem
    }
    if (missing(allowNull)) {
      allowNull.input <- ""
    } else if (!allowNull %in% c(TRUE, FALSE)) {
      message("allowNull is invalid and will be ignored.")
      allowNull.input <- ""
    } else {
      allowNull.input <- allowNull
    }
    if (missing(dataType)) {
      dataType.input <- ""
    } else if (!dataType %in% dataType.vector) {
      message("dataType is invalid and will be ignored.")
      dataType.input <- ""
    } else {
      dataType.input <- dataType
    }
    if (missing(definition)) {
      definition.input <- ""
    } else {
      definition.input <- definition
    }
    if (missing(units)) {
      units.input <- ""
    } else {
      units.input <- units
    }
    if (missing(unitsResolution)) {
      unitsResolution.input <- ""
    } else if (suppressWarnings(is.na(as.numeric(unitsResolution.input))) == TRUE) {
      message("unitsResolution is invalid and will be ignored.")
      unitsResolution.input <- ""
    } else {
      unitsResolution.input <- unitsResolution
    }
    if (missing(isCaseSensitive)) {
      isCaseSensitive.input <- ""
    } else if (!isCaseSensitive %in% c(TRUE, FALSE)) {
      message("isCaseSensitive is invalid and will be ignored.")
      isCaseSensitive.input <- ""
    } else {
      isCaseSensitive.input <- isCaseSensitive
    }
    if (missing(missingValue)) {
      missingValue.input <- ""
    } else {
      missingValue.input <- missingValue
    }
    if (missing(minValue)) {
      minValue.input <- ""
    } else {
      minValue.input <- minValue
    }
    if (missing(maxValue)) {
      maxValue.input <- ""
    } else {
      maxValue.input <- maxValue
    }
    if (missing(fieldWidth)) {
      fieldWidth.input <- ""
    } else if (!grepl("[^[:digit:]]",
                      format(fieldWidth.input,
                             digits = 20,
                             scientific = FALSE)) == FALSE) {
      message("fieldWidth is invalid and will be ignored.")
      fieldWidth.input <- ""
    } else {
      fieldWidth.input <- fieldWidth
    }



    dictionarylist <-
      rjson::fromJSON(input.dxnry[["data"]][[1]][["attributes"]][["json"]])


    #### Add an attribute (and domain - optional) ####
    if (how == "add_attribute") {
      ## Check if attribute already exists
      attribute.num <- c()
      for (a in 1:length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]])) {
        if (dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[a]][["codeName"]] ==
            codeName) {
          attribute.num <- c(attribute.num, a)
        }
      }

      if (length(attribute.num) != 0) {
        message(cat(
          paste0(
            "Operation canceled. An attribute with the codeName '",
            codeName,
            "' already exists."
          )
        ))
      } else {
        newattribute <- blankattribute

        newattribute[[1]][["codeName"]] <- codeName

        ## attribute allowNull
        if (allowNull.input == "") {
          allowNull.choice <- 0

          while (allowNull.choice == 0) {
            allowNull.choice <-
              utils::menu(c(TRUE, FALSE),
                          title =
                            cat(
                              paste0(
                                "\nREQUIRED: Are null values permitted for the attribute '",
                                codeName,
                                "'?\n"
                              )
                            ))
          }
          allowNull.input <- c(TRUE, FALSE)[allowNull.choice]
        }
        newattribute[[1]][["allowNull"]] <-
          allowNull.input


        ## attribute dataType
        if (dataType.input == "") {
          dataType.choice <- 0

          while (dataType.choice == 0) {
            dataType.choice <-
              utils::menu(c(dataType.vector),
                          title = cat(
                            paste0(
                              "\nREQUIRED: Select the datatype/format for entry values of the attribute '",
                              codeName,
                              "'.\n"
                            )
                          ))
          }
          dataType.input <- dataType.vector[dataType.choice]
        }
        newattribute[[1]][["dataType"]] <-
          dataType.input


        ## attribute definition
        if (definition.input == "") {
          while (definition.input == "") {
            message(cat(
              paste0(
                "\nREQUIRED: Provide a definition for the attribute '",
                codeName,
                "'."
              )
            ))
            definition.input <- as.character(readline(prompt = ))
          }
        }
        newattribute[[1]][["definition"]] <- definition.input


        ## attribute units
        if (units.input == "") {
          message(cat(
            paste0(
              "\nOPTIONAL: Provide the unit-of-measure for the attribute '",
              codeName,
              "' (e.g., meters, atmospheres, liters).\nPress Enter to omit this information."
            )
          ))
          units.input <- as.character(readline(prompt =))
        }
        newattribute[[1]][["units"]] <- units.input


        ## attribute unitsResolution
        if (unitsResolution.input == "") {
          message(cat(
            paste0(
              "\nOPTIONAL: Provide the smallest unit increment (numeric) to which the attribute '",
              codeName,
              "' is measured (e.g., 1, .1, .01).\nPress Enter to omit this information."
            )
          ))
          unitsResolution.input <- as.character(readline(prompt = ))
        }

        while (suppressWarnings(is.na(as.numeric(unitsResolution.input))) == TRUE) {
          message(cat(
            paste0(
              "\nInvalid entry: unit resolution must be numeric.\nPress Enter to omit this information."
            )
          ))
          unitsResolution.input <- as.character(readline(prompt = ))

        }

        newattribute[[1]][["unitsResolution"]] <-
          unitsResolution.input


        ## attribute isCaseSensitive
        if (isCaseSensitive.input == "") {
          isCaseSensitive.choice <-
            utils::menu(c(TRUE, FALSE),
                        title =
                          cat(
                            paste0(
                              "\nOPTIONAL: Are the entry values of the attribute '",
                              codeName,
                              "' encoded in case-sensitive ASCII?\nEnter '0' to omit this information.\n"
                            )
                          ))
          if (isCaseSensitive.choice == 0) {
            isCaseSensitive.input <- ""
          } else{
            isCaseSensitive.input <- c(TRUE, FALSE)[isCaseSensitive.choice]
          }
        }
        newattribute[[1]][["isCaseSensitive"]] <-
          isCaseSensitive.input



        ## attribute missingValue
        if (c(TRUE, FALSE)[allowNull.choice] == TRUE) {
          if (missingValue.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the code which represents missing entry values for the attribute '",
                codeName,
                "' (e.g., NA, na, -).\nPress Enter to omit this information."
              )
            ))
            missingValue.input <- as.character(readline(prompt = ))
            newattribute[[1]][["missingValue"]] <-
              missingValue.input
          }
        }


        ## attribute minValue
        if (minValue.input == "") {
          message(cat(
            paste0(
              "\nOPTIONAL: Provide the minimum value permissible for the attribute '",
              codeName,
              "'.\nPress Enter to omit this information."
            )
          ))
          minValue.input <- as.character(readline(prompt = ))
        }
        newattribute[[1]][["minValue"]] <- minValue.input


        ## attribute maxValue
        if (maxValue.input == "") {
          message(cat(
            paste0(
              "\nOPTIONAL: Provide the maximum value permissible for the attribute '",
              codeName,
              "'.\nPress Enter to omit this information."
            )
          ))
          maxValue.input <- as.character(readline(prompt =))
        }
        newattribute[[1]][["maxValue"]] <- maxValue.input


        ## attribute fieldWidth
        if (fieldWidth.input == "") {
          message(cat(
            paste0(
              "\nOPTIONAL: Provide the field width (integer) of entry values for the attribute '",
              codeName,
              "'.\nPress Enter to omit this information."
            )
          ))
          fieldWidth.input <- as.character(readline(prompt = ))
        }

        while (!grepl("[^[:digit:]]",
                      format(
                        fieldWidth.input,
                        digits = 20,
                        scientific = FALSE
                      )) == FALSE) {
          message(cat(
            paste0(
              "\nThe field width must be an integer.\nPress Enter to omit this information."
            )
          ))
          fieldWidth.input <- as.character(readline(prompt = ))

        }

        newattribute[[1]][["fieldWidth"]] <- fieldWidth.input


        ## Add attribute to dictionary list
        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]]) +
                                                                              1]] <-
          newattribute[[1]]


        ## Add an associated domain
        domainId.choice <- 0

        while (domainId.choice == 0) {
          domainId.choice <-
            utils::menu(c("yes", "no"),
                        title = cat(
                          paste0(
                            "\nREQUIRED: Does the attribute '",
                            codeName,
                            "' have a domain (defined entry values)?\n"
                          )
                        ))
        }
        ## attribute domainId
        newattribute[[1]][["domainId"]] <-
          uuid::UUIDgenerate(use.time = FALSE, n = 1)

        ## domain domainId, codeName, & definition
        if (domainId.choice == 1) {
          newdomain <- blankdomain

          newdomain[[1]][["domainId"]] <-
            newattribute[[1]][["domainId"]]
          newdomain[[1]][["codeName"]] <- codeName
          newdomain[[1]][["description"]] <-
            newattribute[[1]][["definition"]]


          ## Add a domainItem
          domainItems.choice <- 0

          while (domainItems.choice == 0) {
            domainItems.choice <-
              utils::menu(c("yes", "no"),
                          title = cat(
                            paste0(
                              "\nREQUIRED: Would you like to add items to the domain associated with '",
                              codeName,
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
          dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) +
                                                            1]] <-
            newdomain[[1]]

        }

      }
    }


    #### Add domain to existing attribute ####

    if (how == "add_domain") {
      ## Find attribute number
      attribute.num <- c()
      for (a in 1:length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]])) {
        if (dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[a]][["codeName"]] ==
            codeName) {
          attribute.num <- c(attribute.num, a)
        }
      }

      ## Check if a single attribute exists
      if (length(attribute.num) > 1) {
        message(cat(
          paste0(
            "Operation canceled. More than one attribute with the codeName '",
            codeName,
            "' was found. Correct one or both attributes before proceeding."
          )
        ))
      } else if (length(attribute.num) == 0) {
        message(cat(
          paste0(
            "Operation canceled. No attribute with the codeName '",
            codeName,
            "' was found."
          )
        ))

      } else {
        ## Check if a domain already exists for the attribute
        domain.num <- c()
        for (a in 1:length(dictionarylist[["dataDictionary"]][["domain"]])) {
          if (dictionarylist[["dataDictionary"]][["domain"]][[a]][["codeName"]] ==
              codeName) {
            domain.num <- c(domain.num, a)
          }
        }

        if (length(domain.num) == 0) {
          newdomain <- blankdomain

          ## domain domainId, codeName, & definition
          newdomain[[1]][["domainId"]] <-
            uuid::UUIDgenerate(use.time = FALSE, n = 1)
          newdomain[[1]][["codeName"]] <- codeName
          newdomain[[1]][["description"]] <-
            dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["definition"]]


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
                paste0("\nREQUIRED: Provide a name for the new domain item.")
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


          ## Add domain to dictionary list
          dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) +
                                                            1]] <-
            newdomain[[1]]


          ## Update attribute domainId in dictionary list
          dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]] <-
            newdomain[[1]][["domainId"]]


          ## When a domain exists...
        } else if (length(domain.num) == 1) {
          ## Check if attribute and domain domainId match
          if (dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]] == dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]) {
            message(cat(
              paste0(
                "Operation canceled. A domain already exists for the attribute '",
                codeName,
                "'."
              )
            ))
          } else if (dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]] != dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]) {
            message(cat(
              paste0(
                "\nA domain already exists for the attribute '",
                codeName,
                "'.",
                "\nAttribute domainId: ",
                dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]],
                "\nDomain domainId: ",
                dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]
              )
            ))
            ## Update attribute domainId if it doesn't match (assuming domain domainID is valid)
            attribute.domainId.choice <-
              utils::menu(c("yes", "no"),
                          title = cat(
                            paste0(
                              "\nWould you like to update the attribute domainId to match the existing domain?"
                            )
                          ))
            if (attribute.domainId.choice == 1) {
              dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]] <-
                dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]
            }
          }
        }

      }
    }

    #### Add domainItem ####
    if (how == "add_domainItem") {
      ## Find domain number
      domain.num <- c()
      for (a in 1:length(dictionarylist[["dataDictionary"]][["domain"]])) {
        if (dictionarylist[["dataDictionary"]][["domain"]][[a]][["codeName"]] ==
            codeName) {
          domain.num <- c(domain.num, a)
        }
      }

      ## Check if a single domain exists
      if (length(domain.num) > 1) {
        message(cat(
          paste0(
            "Operation canceled. More than one domain with the codeName '",
            codeName,
            "' was found. Correct one or both domains before proceeding."
          )
        ))
      } else if (length(domain.num) == 0) {
        message(cat(
          paste0(
            "Operation canceled. No domain with the codeName '",
            codeName,
            "' was found."
          )
        ))
      } else{
        newdomainItems <- blankdomainItem

        ## Describe initial domainItem, append subsequent
        moredomainItems.choice <- 1

        while (moredomainItems.choice == 1) {
          if (newdomainItems[[1]][["name"]] != "NA") {
            newdomainItems <-
              append(newdomainItems, blankdomainItem)
            domainItem.num <-
              length(newdomain[[1]][["domainItem"]])
          } else {
            domainItem.num <- 1
          }

          ## domainItem value
          if (domainItem_value.input == "") {
            domainItem_value.input <- ""

            while (domainItem_value.input == "") {
              message(cat(
                paste0(
                  "\nREQUIRED: Provide the new entry value for the attribute '",
                  codeName,
                  "'."
                )
              ))
              domainItem_value.input <-
                as.character(readline(prompt = ))
            }
          }
          newdomainItems[[domainItem.num]][["value"]] <-
            domainItem_value.input


          ## domainItem name
          domainItem_name.input <- ""

          while (domainItem_name.input == "") {
            message(cat(
              paste0(
                "\nREQUIRED: Provide a name for the new entry value '",
                domainItem_value.input,
                "'."
              )
            ))
            domainItem_name.input <-
              as.character(readline(prompt = ))
          }
          newdomainItems[[domainItem.num]][["name"]] <-
            domainItem_name.input


          ## domainItem definition
          domainItem_defintion.input <- ""

          while (domainItem_defintion.input == "") {
            message(cat(
              paste0(
                "\nREQUIRED: Provide a definition for the domain item '",
                domainItem_value.input,
                "'."
              )
            ))
            domainItem_defintion.input <-
              as.character(readline(prompt =))
          }
          newdomainItems[[domainItem.num]][["definition"]] <-
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

        ## Add each domainItem to the domain in the dictionary list
        for (a in 1:length(newdomainItems)) {
          dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[length(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]]) +
                                                                                          1]] <-
            newdomainItems[[a]]
        }
      }
    }

    #### Update attribute info ####
    if (how == "update_attribute") {
      ## Find attribute number
      attribute.num <- c()
      for (a in 1:length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]])) {
        if (dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[a]][["codeName"]] ==
            codeName) {
          attribute.num <- c(attribute.num, a)
        }
      }

      ## Check if a single attribute exists
      if (length(attribute.num) > 1) {
        message(cat(
          paste0(
            "Operation canceled. More than one attribute with the codeName '",
            codeName,
            "' was found. Correct one or both attributes before proceeding."
          )
        ))
      } else if (length(attribute.num) == 0) {
        message(cat(
          paste0(
            "Operation canceled. No attribute with the codeName '",
            codeName,
            "' was found."
          )
        ))

      }

      originalattribute <-
        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]]

      newattribute <- originalattribute


      ## Make changes based on inputs
      field.vector <-
        c(
          "allowNull",
          "dataType",
          "definition",
          "units",
          "unitsResolution",
          "isCaseSensitive",
          "missingValue",
          "minValue",
          "maxValue",
          "fieldWidth"
        )

      ## No inputs provided
      if (any(field.vector %in% ls()) == FALSE) {
        update.choice <-
          utils::menu(c("keep as is", "make changes"),
                      title = cat(
                        paste0("\n'", codeName, "' is described as followed.\n"),
                        paste0(jsonlite::toJSON(originalattribute, pretty = TRUE), sep = "\n"),
                        paste0("\nWould you like to do?")
                      ))

      } else if (any(field.vector %in% ls()) == TRUE) {
        ## Inputs provided

        ## attribute allowNull
        if (allowNull.input != "") {
          newattribute[["allowNull"]] <- allowNull.input
        }

        ## attribute dataType
        if (dataType.input != "") {
          newattribute[["dataType"]] <- dataType.input
        }

        ## attribute unitsResolution
        if (unitsResolution.input != "") {
          newattribute[["unitsResolution"]] <- unitsResolution.input
        }

        ## attribute isCaseSensitive
        if (fieldWidth.input != "") {
          newattribute[["isCaseSensitive"]] <- isCaseSensitive.input
        }

        ## attribute missingValue
        if (missingValue.input != "") {
          newattribute[["missingValue"]] <- missingValue.input
        }

        ## attribute minValue
        if (minValue.input != "") {
          newattribute[["minValue"]] <- minValue.input
        }

        ## attribute maxValue
        if (maxValue.input != "") {
          newattribute[["maxValue"]] <- maxValue.input
        }


        ## attribute fieldWidth
        if (fieldWidth.input != "") {
          newattribute[["fieldWidth"]] <- fieldWidth.input
        }

        update.choice <-
          utils::menu(
            c(
              "disregard all revisions",
              "make additional changes",
              "looks good"
            ),
            title = cat(
              paste0("\n'", codeName, "' has been updated as followed.\n"),
              paste0(jsonlite::toJSON(newattribute, pretty = TRUE), sep = "\n"),
              paste0("\nDoes this look correct?")
            )
          )

      }

      ## Make additional changes
      while (update.choice == 2) {
        field.choice <-
          utils::menu(c(field.vector),
                      title =
                        cat(paste0(
                          "\nWhich field would you like to add/modify?\n"
                        )))


        # Create field if it doesn't exist

        if (field.vector[field.choice] %in% names(newattribute) == FALSE) {
          newfield <- list()
          newfield[[paste0(field.vector[field.choice])]] <- ""
          newattribute <- append(newattribute, newfield)
        }

        ## attribute allowNull
        if (field.vector[field.choice] == "allowNull") {
          allowNull.choice <- 0
          originalvalue <- newattribute[["allowNull"]]

          allowNull.choice <-
            utils::menu(c(TRUE, FALSE),
                        title =
                          cat(
                            paste0(
                              "\nAre null values permitted for the attribute '",
                              codeName,
                              "'? \nThe current value is '",
                              originalvalue,
                              "'.\n"
                            )
                          ))

          if (allowNull.choice %in% c(1, 2)) {
            allowNull.input <- c(TRUE, FALSE)[allowNull.choice]
            newattribute[["allowNull"]] <-
              allowNull.input
          }

          ## attribute dataType
        } else if (field.vector[field.choice] == "dataType") {
          dataType.choice <- 0
          originalvalue <- newattribute[["dataType"]]

          dataType.choice <-
            utils::menu(c(dataType.vector),
                        title = cat(
                          paste0(
                            "\nSelect the datatype/format for entry values of the attribute '",
                            codeName,
                            "'. \nThe current datatype is '",
                            originalvalue,
                            "'.\n"
                          )
                        ))

          if (dataType.choice != 0) {
            dataType.input <- dataType.vector[dataType.choice]

            newattribute[["dataType"]] <-
              dataType.input
          }

          ## attribute definition
        } else if (field.vector[field.choice] == "definition") {
          originalvalue <- newattribute[["definition"]]

          message(cat(
            paste0(
              "\nProvide a new definition for the attribute '",
              codeName,
              "'. \nThe current definition is '",
              originalvalue,
              "'\nPress Enter to abort changes."
            )
          ))
          definition.input <- as.character(readline(prompt = ))


          if (definition.input != "") {
            newattribute[["definition"]] <- definition.input
          }

          ## attribute units
        } else if (field.vector[field.choice] == "units") {
          originalvalue <- newattribute[["units"]]

          message(cat(
            paste0(
              "\nProvide the unit-of-measure for the attribute '",
              codeName,
              "' (e.g., meters, atmospheres, liters). \nThe current unit is '",
              originalvalue,
              "'. \nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          units.input <- as.character(readline(prompt =))

          if (units.input == "rm()") {
            newattribute[["units"]] <- ""
          } else if (units.input != "") {
            newattribute[["units"]] <- units.input
          }

          ## attribute unitsResolution
        } else if (field.vector[field.choice] == "unitsResolution") {
          originalvalue <- newattribute[["unitsResolution"]]

          message(cat(
            paste0(
              "\nProvide the smallest unit increment (numeric) to which the attribute '",
              codeName,
              "' is measured (e.g., 1, .1, .01). \nThe current unit resolution is '",
              originalvalue,
              "'\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          unitsResolution.input <- as.character(readline(prompt =))

          if (unitsResolution.input == "rm()") {
            newattribute[["unitsResolution"]] <-
              ""
          } else if (suppressWarnings(is.na(as.numeric(unitsResolution.input))) == TRUE) {
            message(
              "\nInvalid entry: unit resolution must be numeric.\nThe current resolution will be retained."
            )
          } else {
            newattribute[["unitsResolution"]] <-
              unitsResolution.input
          }


          ## attribute isCaseSensitive
        } else if (field.vector[field.choice] == "isCaseSensitive") {
          isCaseSensitive.choice <- 0
          originalvalue <- newattribute[["isCaseSensitive"]]

          isCaseSensitive.choice <-
            utils::menu(c(TRUE, FALSE),
                        title =
                          cat(
                            paste0(
                              "\nAre the entry values of the attribute '",
                              codeName,
                              "' encoded in case-sensitive ASCII?\nThe current value is '",
                              originalvalue,
                              "'.\n"
                            )
                          ))

          if (isCaseSensitive.choice %in% c(1, 2)) {
            isCaseSensitive.input <- c(TRUE, FALSE)[isCaseSensitive.choice]

            newattribute[["isCaseSensitive"]] <-
              isCaseSensitive.input
          }

          ## attribute missingValue
        } else if (field.vector[field.choice] == "missingValue") {
          originalvalue <- newattribute[["missingValue"]]

          if ("allowNull" %in% names(newattribute) == TRUE &
              newattribute[["allowNull"]] == FALSE) {
            allowNull.check <-
              utils::menu(c("yes", "no"),
                          title =
                            cat(
                              paste0(
                                "\nThe dictionary indicates the attribute '",
                                codeName,
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
                codeName,
                "' (e.g., NA, na, -).\nThe current code is '",
                originalvalue,
                "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
              )
            ))
            missingValue.input <- as.character(readline(prompt =))

            if (missingValue.input == "rm()") {
              newattribute[["missingValue"]] <- ""
            } else if (missingValue.input != "") {
              newattribute[["missingValue"]] <-
                missingValue.input
            }

          } else {
            message(cat(paste0("\nChanges aborted.")))
          }

          ## attribute minValue
        } else if (field.vector[field.choice] == "minValue") {
          originalvalue <- newattribute[["minValue"]]

          message(cat(
            paste0(
              "\nProvide the minimum value permissible for the attribute '",
              codeName,
              "'.\nThe current minimum value is '",
              originalvalue,
              "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          minValue.input <- as.character(readline(prompt =))

          if (minValue.input == "rm()") {
            newattribute[["minValue"]] <- ""
          } else if (minValue.input != "") {
            newattribute[["minValue"]] <- minValue.input
          }

          ## attribute maxValue
        } else if (field.vector[field.choice] == "maxValue") {
          originalvalue <- newattribute[["maxValue"]]

          message(cat(
            paste0(
              "\nProvide the maximum value permissible for the attribute '",
              codeName,
              "'.\nThe current maximum value is '",
              originalvalue,
              "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          maxValue.input <- as.character(readline(prompt =))


          if (maxValue.input == "rm()") {
            newattribute[["maxValue"]] <- ""
          } else if (minValue.input != "") {
            newattribute[["maxValue"]] <- maxValue.input
          }

          ## attribute fieldWidth
        } else if (field.vector[field.choice] == "fieldWidth") {
          originalvalue <- newattribute[["fieldWidth"]]

          message(cat(
            paste0(
              "\nProvide the field width (integer) of entry values for the attribute '",
              codeName,
              "'.\nThe current field width is '",
              originalvalue,
              "'\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          fieldWidth.input <- as.character(readline(prompt =))

          if (fieldWidth.input == "rm()") {
            newattribute[["fieldWidth"]] <- ""
          } else if (fieldWidth.input != "") {
            if (!grepl("[^[:digit:]]",
                       format(
                         fieldWidth.input,
                         digits = 20,
                         scientific = FALSE
                       )) == FALSE) {
              message(
                "\nInvalid entry: field width must be an integer.\nThe current field width will be retained."
              )
            } else {
              newattribute[["fieldWidth"]] <-
                fieldWidth.input
            }
          }
        }

        update.choice <-
          utils::menu(
            c(
              "disregard all revisions",
              "make additional changes",
              "looks good"
            ),
            title = cat(
              paste0("\n'", codeName, "' has been updated as followed.\n"),
              paste0(jsonlite::toJSON(newattribute, pretty = TRUE), sep = "\n"),
              paste0("\nDoes this look correct?")
            )
          )
      }


      if (update.choice == 1) {
        message("Operation canceled.")

      } else if (update.choice == 3) {
        # update attribute in dictionary
        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]] <-
          newattribute


      }
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
