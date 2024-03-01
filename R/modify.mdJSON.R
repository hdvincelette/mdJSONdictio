#' Modify mdJSON Data Dictionaries
#'
#' Amends an mdJSON data dictionary, including adding/updating attributes and domains.
#' @param x List object converted from an mdJSON file.
#' @param how character string matching one of the modification options: see ‘Details’.
#' @param codeName a string representing the attribute to add or modify.
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
#' #' # Add a domain
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domain")
#'
#' # Convert list to JSON
#' new.json = rjson::toJSON(x = modified.dxnry)
#'
#' # Export JSON to disk
#' write(x = new.json, file = "e.g.dictionary.json")


modify.mdJSON <-
  function(x,
           how = c("add_attribute",
                   "add_domain",
                   "add_domainItem",
                   "update_attribute"),
           codeName) {
    blankjson <- blankjson
    blankdictionarylist <-
      rjson::fromJSON(blankjson[["data"]][[1]][["attributes"]][["json"]])
    blankattribute <-
      blankdictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]]
    blankdomain <-
      blankdictionarylist[["dataDictionary"]][["domain"]]
    blankdomainItem <- blankdomain[[1]][["domainItem"]]

    # dataType.vector <- dplyr::pull(
    #   readxl::read_excel(
    #     "inst/templates/mdJSONdictio_Dictionary_Template_v1.xlsx",
    #     sheet = 3,
    #     col_names = FALSE
    #   ) ,
    #   1
    # )

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


    if (missing("codeName")) {
      codeName <- ""
      while (codeName == "") {
        message(cat(
          paste0("\nREQUIRED: Provide the attribute or domain codeName.")
        ))
        codeName <- as.character(readline(prompt =))
      }
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
        allowNull.choice <- 0

        while (allowNull.choice == 0) {
          allowNull.choice <-
            utils::menu(c("yes", "no"),
                        title =
                          cat(
                            paste0(
                              "\nREQUIRED: Are null values permitted for the attribute '",
                              codeName,
                              "'?\n"
                            )
                          ))
        }
        newattribute[[1]][["allowNull"]] <-
          c("yes", "no")[allowNull.choice]


        ## attribute dataType
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
        newattribute[[1]][["dataType"]] <-
          dataType.vector[dataType.choice]


        ## attribute definition
        definition.input <- ""

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
        newattribute[[1]][["definition"]] <- definition.input


        ## attribute units
        message(cat(
          paste0(
            "\nOPTIONAL: Provide the unit-of-measure for the attribute '",
            codeName,
            "' (e.g., meters, atmospheres, liters)."
          )
        ))
        units.input <- as.character(readline(prompt = ))
        newattribute[[1]][["units"]] <- units.input


        ## attribute unitsResolution
        message(cat(
          paste0(
            "\nOPTIONAL: Provide the smallest unit increment (decimal) to which the attribute '",
            codeName,
            "' is measured (e.g., 1, .1, .01)."
          )
        ))
        unitsResolution.input <- as.character(readline(prompt =))
        newattribute[[1]][["unitsResolution"]] <-
          unitsResolution.input


        ## attribute isCaseSensitive
        isCaseSensitive.choice <-
          utils::menu(c("yes", "no"),
                      title =
                        cat(
                          paste0(
                            "\nOPTIONAL: Are the entry values of the attribute '",
                            codeName,
                            "' encoded in case-sensitive ASCII?\n"
                          )
                        ))
        if (isCaseSensitive.choice == 0) {
          isCaseSensitive.choice <- ""
        } else{
          isCaseSensitive.choice <- c("yes", "no")[isCaseSensitive.choice]
        }
        newattribute[[1]][["isCaseSensitive"]] <-
          isCaseSensitive.choice



        ## attribute missingValue
        if (c("yes", "no")[allowNull.choice] == "yes") {
          message(cat(
            paste0(
              "\nOPTIONAL: Provide the code which represents missing entry values for the attribute '",
              codeName,
              "' (e.g., NA, na, -)."
            )
          ))
          missingValue.input <- as.character(readline(prompt = ))
          newattribute[[1]][["missingValue"]] <-
            missingValue.input
        }


        ## attribute minValue
        message(cat(
          paste0(
            "\nOPTIONAL: Provide the minimum range value permissible for the attribute '",
            codeName,
            "'."
          )
        ))
        minValue.input <- as.character(readline(prompt =))
        newattribute[[1]][["minValue"]] <- minValue.input


        ## attribute maxValue
        message(cat(
          paste0(
            "\nOPTIONAL: Provide the maximum range value permissible for the attribute '",
            codeName,
            "'."
          )
        ))
        maxValue.input <- as.character(readline(prompt = ))
        newattribute[[1]][["maxValue"]] <- maxValue.input


        ## attribute fieldWidth
        message(cat(
          paste0(
            "\nOPTIONAL: Provide the field width (integer) of entry values for the attribute '",
            codeName,
            "'."
          )
        ))
        fieldWidth.input <- as.character(readline(prompt = ))

        while (!grepl("[^[:digit:]]",
                      format(
                        fieldWidth.input,
                        digits = 20,
                        scientific = FALSE
                      )) == FALSE) {
          message(cat(
            paste0(
              "\nfieldWidth must be an integer value. Enter 'skip' to omit this information."
            )
          ))
          fieldWidth.input <- as.character(readline(prompt =))

          if (fieldWidth.input == "skip") {
            fieldWidth.input <- ""
          }
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
        if (c("yes", "no")[domainId.choice] == "yes") {
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
          if (c("yes", "no")[domainItems.choice] == "yes") {
            moredomainItems.choice <- "yes"

            while (moredomainItems.choice == "yes") {
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
                  as.character(readline(prompt =))
              }
              newdomain[[1]][["domainItem"]][[domainItem.num]][["name"]] <-
                domainItem_name.input


              ## domainItem value
              domainItem_value.input <- ""

              while (domainItem_value.input == "") {
                message(cat(
                  paste0(
                    "\nREQUIRED: Provide a value (entry value) for the domain item '",
                    domainItem_name.input,
                    "'."
                  )
                ))
                domainItem_value.input <-
                  as.character(readline(prompt = ))
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
                  as.character(readline(prompt =))
              }
              newdomain[[1]][["domainItem"]][[domainItem.num]][["definition"]] <-
                domainItem_defintion.input


              ## Check for more domainItems
              moredomainItems.choice <-
                c("yes", "no")[utils::menu(c("yes", "no"),
                                           title = cat(
                                             paste0(
                                               "\nREQUIRED: Would you like to add an additional item to the domain?"
                                             )
                                           ))]


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
          moredomainItems.choice <- "yes"

          while (moredomainItems.choice == "yes") {
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
                as.character(readline(prompt =))
            }
            newdomain[[1]][["domainItem"]][[domainItem.num]][["name"]] <-
              domainItem_name.input


            ## domainItem value
            domainItem_value.input <- ""

            while (domainItem_value.input == "") {
              message(cat(
                paste0(
                  "\nREQUIRED: Provide a value (entry value) for the domain item '",
                  domainItem_name.input,
                  "'."
                )
              ))
              domainItem_value.input <-
                as.character(readline(prompt = ))
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
                as.character(readline(prompt =))
            }
            newdomain[[1]][["domainItem"]][[domainItem.num]][["definition"]] <-
              domainItem_defintion.input


            ## Check for more domainItems
            moredomainItems.choice <-
              c("yes", "no")[utils::menu(c("yes", "no"),
                                         title = cat(
                                           paste0(
                                             "\nREQUIRED: Would you like to add an additional item to the domain?"
                                           )
                                         ))]


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
              c("yes", "no")[utils::menu(c("yes", "no"),
                                         title = cat(
                                           paste0(
                                             "\nWould you like to update the attribute domainId to match the existing domain?"
                                           )
                                         ))]
            if (attribute.domainId.choice == "yes") {
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
        moredomainItems.choice <- "yes"

        while (moredomainItems.choice == "yes") {
          if (newdomainItems[[1]][["name"]] != "NA") {
            newdomainItems <-
              append(newdomainItems, blankdomainItem)
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
          newdomainItems[[domainItem.num]][["name"]] <-
            domainItem_name.input


          ## domainItem value
          domainItem_value.input <- ""

          while (domainItem_value.input == "") {
            message(cat(
              paste0(
                "\nREQUIRED: Provide a value (entry value) for the domain item '",
                domainItem_name.input,
                "'."
              )
            ))
            domainItem_value.input <-
              as.character(readline(prompt =))
          }
          newdomainItems[[domainItem.num]][["value"]] <-
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
          newdomainItems[[domainItem.num]][["definition"]] <-
            domainItem_defintion.input


          ## Check for more domainItems
          moredomainItems.choice <-
            c("yes", "no")[utils::menu(c("yes", "no"),
                                       title = cat(
                                         paste0(
                                           "\nREQUIRED: Would you like to add an additional item to the domain?"
                                         )
                                       ))]


        }

        ## Add each domainItem to the domain in the dictionary list
        for (a in 1:length(newdomainItems)) {
          dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[length(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]]) +
                                                                                          1]] <-
            newdomainItems[[a]]
        }
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
