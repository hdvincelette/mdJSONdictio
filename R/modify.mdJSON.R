#' Modify mdJSON Data Dictionaries
#'
#' Amends an mdJSON data dictionary, including adding/updating attributes and domains.
#' @param x List object converted from an mdJSON file.
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
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_attribute", codeName = "WingArea")
#'
#' # Add a domain
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domain")
#'
#' # Add a domain item
#' modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domainItem", codeName = "WingArea", domainItem_value = "U")
#'
#' # Update attribute datatype and allowNull values
#' modified.dxnry<- modify.mdJSON(x = ref.dictionary, how = "update_attribute", codeName = "BandSize", dataType = "Integer", allowNull = TRUE)
#'
#' # Convert list to JSON
#' new.json = rjson::toJSON(x = modified.dxnry)
#'
#' # Export JSON to disk
#' write(x = new.json, file = "e.g.dictionary.json")


# update_domain
# remove domain item, attribute, domain
# add_attribute, add_domain: allow domainItem inputs (vectors?)



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

    if (missing(codeName)) {
      codeName.input <- ""
    } else {
      codeName.input <- codeName
      if (is.null(codeName) == FALSE) {
        definedparam <- c(definedparam, "codeName")
      }
    }
    if (missing(allowNull)) {
      allowNull.input <- ""
    } else if (is.null(allowNull) == FALSE) {
      if (!allowNull %in% c(TRUE, FALSE)) {
        message("allowNull is invalid and will be ignored.")
        allowNull.input <- ""
      } else {
        allowNull.input <- allowNull
        definedparam <- c(definedparam, "allowNull")
      }
    } else {
      allowNull.input <- allowNull
    }
    if (missing(dataType)) {
      dataType.input <- ""
    } else if (is.null(dataType) == FALSE) {
      if (!dataType %in% dataType.vector) {
        message("dataType is invalid and will be ignored.")
        dataType.input <- ""
      } else {
        dataType.input <- dataType
        definedparam <- c(definedparam, "dataType")
      }
    } else {
      dataType.input <- dataType
    }
    if (missing(definition)) {
      definition.input <- ""
    } else {
      definition.input <- definition
      if (is.null(definition) == FALSE) {
        definedparam <- c(definedparam, "definition")
      }
    }
    if (missing(units)) {
      units.input <- ""
    } else {
      units.input <- units
      if (is.null(units) == FALSE) {
        definedparam <- c(definedparam, "units")
      }
    }
    if (missing(unitsResolution)) {
      unitsResolution.input <- ""
    } else if (is.null(unitsResolution) == FALSE) {
      if (suppressWarnings(is.na(as.numeric(unitsResolution))) == TRUE) {
        message("unitsResolution is invalid and will be ignored.")
        unitsResolution.input <- ""
      } else {
        unitsResolution.input <- unitsResolution
        definedparam <- c(definedparam, "unitsResolution")
      }
    } else {
      unitsResolution.input <- unitsResolution
    }
    if (missing(isCaseSensitive)) {
      isCaseSensitive.input <- ""
    } else if (is.null(isCaseSensitive) == FALSE) {
      if (!isCaseSensitive %in% c(TRUE, FALSE)) {
        message("isCaseSensitive is invalid and will be ignored.")
        isCaseSensitive.input <- ""
      } else {
        isCaseSensitive.input <- isCaseSensitive
        definedparam <- c(definedparam, "isCaseSensitive")
      }
    } else {
      isCaseSensitive.input <- isCaseSensitive
    }
    if (missing(missingValue)) {
      missingValue.input <- ""
    } else {
      missingValue.input <- missingValue
      if (is.null(missingValue) == FALSE) {
        definedparam <- c(definedparam, "missingValue")
      }
    }
    if (missing(minValue)) {
      minValue.input <- ""
    } else {
      minValue.input <- minValue
      if (is.null(minValue) == FALSE) {
        definedparam <- c(definedparam, "minValue")
      }
    }
    if (missing(maxValue)) {
      maxValue.input <- ""
    } else {
      maxValue.input <- maxValue
      if (is.null(maxValue) == FALSE) {
        definedparam <- c(definedparam, "maxValue")
      }
    }
    if (missing(fieldWidth)) {
      fieldWidth.input <- ""
    } else if (is.null(fieldWidth) == FALSE) {
      if (!grepl("[^[:digit:]]",
                 format(fieldWidth,
                        digits = 20,
                        scientific = FALSE)) == FALSE) {
        message("fieldWidth is invalid and will be ignored.")
        fieldWidth.input <- ""
      } else {
        fieldWidth.input <- fieldWidth
        definedparam <- c(definedparam, "fieldWidth")
      }
    } else {
      fieldWidth.input <- fieldWidth
    }
    if (missing(domainId)) {
      domainId.input <- ""
    } else if (is.null(domainId) == FALSE) {
      if (!domainId %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId")) {
        message(cat(paste0(
          "domainId is invalid and will be ignored."
        )))
        domainId.input <- ""
      } else {
        domainId.input <- domainId
        definedparam <- c(definedparam, "domainId")
      }
    } else {
      domainId.input <- domainId
    }
    if (missing(domainName)) {
      domainName.input <- ""
    } else if (is.null(domainName) == FALSE) {
      if (!domainName %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
        message(cat(paste0(
          "domainName is invalid and will be ignored."
        )))
        domainName.input <- ""
      } else {
        domainName.input <- domainName
        definedparam <- c(definedparam, "domainName")
      }
    } else {
      domainName.input <- domainName
    }
    if (missing(domainDescription)) {
      domainDescription.input <- ""
    } else {
      domainDescription.input <- domainDescription
      if (is.null(domainDescription) == FALSE) {
        definedparam <- c(definedparam, "domainDescription")
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
    if (missing(domainItem_value)) {
      domainItem_value.input <- ""
    } else {
      domainItem_value.input <- domainItem_value
      if (is.null(domainItem_value) == FALSE) {
        definedparam <- c(definedparam, "domainItem_value")
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
      if (is.null(codeName.input)) {
        codeName.input <- ""
      }
      while (codeName.input == "") {
        message(cat(paste0(
          "\nREQUIRED: Provide the attribute codeName."
        )))
        codeName.input <-
          noquote(as.character(readline(prompt = )))
      }
      definedparam <- c(definedparam, "codeName")
    }

    ## If domain input required..
    if (how %in% c("add_domainItem",
                   "update_domain")) {
      if (is.null(domainId.input)) {
        domainId.input <- ""
      }
      if (is.null(domainName.input)) {
        domainName.input <- ""
      }
      if (is.null(definition.input)) {
        definition.input <- ""
      }
      if (domainId.input == "" & domainName.input == "") {
        domain.check <- 0

        while (domain.check == 0) {
          message(cat(
            paste0("\nREQUIRED: Provide the ID (UUID) or name of the domain.")
          ))
          domain.input <-
            noquote(as.character(readline(prompt =)))


          if (domain.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
            domainName.input <- domain.input
            definedparam <- c(definedparam, "domainName")
            domain.check <- 1
          } else if (domain.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId")) {
            domainId.input <- domain.input
            definedparam <- c(definedparam, "domainId")
            domain.check <- 1
          } else {
            message(cat(paste0("domain not found.\n")))

          }
        }
      }
    }

    ## Auto-fill domain input
    if (is.null(domainName.input) == FALSE) {
      if (domainName.input != "") {
        if (is.null(domainId.input)) {
          domainId.input <- ""
        }
        if (domainId.input == "") {
          domain.num <-
            which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                    domainName.input)

          ## Check if a single domain with domainName exists
          if (length(domain.num) > 1) {
            message(cat(
              paste0(
                "Operation canceled. More than one domain named '",
                domainName.input,
                "' was found. Correct one or both domains before proceeding."
              )
            ))
          } else {
            domainId.input <-
              dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainId"]]
            definedparam <- c(definedparam, "domainId")
          }
        }
      }
    } else if (is.null(domainId.input) == FALSE) {
      if (domainId.input != "") {
        if (is.null(domainName.input)) {
          domainName.input <- ""
        }
        if (domainName.input == "") {
          domain.num <-
            which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId") ==
                    domainId.input)
          domainName.input <-
            dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["codeName"]]
          definedparam <- c(definedparam, "domainName")

          ## Check domain Id and name match
        }
      }
    } else if (is.null(domainId.input) == FALSE &
               is.null(domainName.input) == FALSE) {
      if (domainId.input != "" &
          domainName.input != "") {
        domain.name.num <-
          which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                  domainName.input)
        domain.Id.num <-
          which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId") ==
                  domainId.input)
        if (domain.name.num != domain.Id.num) {
          domainName.other <-
            dictionarylist[["dataDictionary"]][["domain"]][[domain.Id.num]][["codeName"]]

          domainId.other <-
            dictionarylist[["dataDictionary"]][["domain"]][[domain.name.num]][["domainId"]]

          if (how %in% c("add_domainItem",
                         "update_domain")) {
            domain.choice <- 0
            while (domain.choice == 0) {
              domain.choice <-
                utils::menu(c(domainName.input, domainName.other),
                            title =
                              cat(
                                paste0(
                                  "\nREQUIRED: Domain Id and name do not correspond.\nWhich is correct?\n"
                                )
                              ))

            }
          } else {
            domain.choice <-
              utils::menu(c(domainName.input, domainName.other),
                          title =
                            cat(
                              paste0(
                                "\nREQUIRED: Domain Id and name do not correspond.\nWhich is correct?\nEnter '0' to omit this information.\n"
                              )
                            ))
          }
          if (domain.choice == 0) {
            domainName.input <- ""
            domainId.input <- ""
          }
          else if (domain.choice == 1) {
            domainId.input <- domainId.other
          } else if (domain.choice == 2) {
            domainName.input <- domainName.other
          }
        }
      }
    }


    #### Add an attribute (and domain - optional) ####
    if (how == "add_attribute") {
      ## Check if attribute already exists
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                codeName.input)

      if (length(attribute.num) != 0) {
        message(cat(
          paste0(
            "Operation canceled. An attribute with the codeName '",
            codeName.input,
            "' already exists."
          )
        ))
      } else {
        newattribute <- blankattribute

        newattribute[[1]][["codeName"]] <- codeName.input

        ## Correct required parameters
        if (is.null(allowNull.input)) {
          allowNull.input <- ""
        }
        if (is.null(dataType.input)) {
          dataType.input <- ""
        }
        if (is.null(definition.input)) {
          definition.input <- ""
        }

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
                                codeName.input,
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
                              codeName.input,
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
                codeName.input,
                "'."
              )
            ))
            definition.input <-
              noquote(as.character(readline(prompt =)))
          }
        }
        newattribute[[1]][["definition"]] <- definition.input


        ## attribute units
        if (is.null(units.input) == FALSE) {
          if (units.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the unit-of-measure for the attribute '",
                codeName.input,
                "' (e.g., meters, atmospheres, liters).\nPress Enter to omit this information."
              )
            ))
            units.input <- as.character(readline(prompt = ))
          }
          newattribute[[1]][["units"]] <- units.input
        }

        ## attribute unitsResolution
        if (is.null(unitsResolution.input) == FALSE) {
          if (unitsResolution.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the smallest unit increment (numeric) to which the attribute '",
                codeName.input,
                "' is measured (e.g., 1, .1, .01).\nPress Enter to omit this information."
              )
            ))
            unitsResolution.input <-
              noquote(as.character(readline(prompt = )))
          }

          if (unitsResolution.input != "") {
            while (suppressWarnings(is.na(as.numeric(unitsResolution.input))) == TRUE) {
              message(cat(
                paste0(
                  "\nInvalid entry: unit resolution must be numeric.\nPress Enter to omit this information."
                )
              ))
              unitsResolution.input <-
                as.character(readline(prompt =))

              if (unitsResolution.input == "")
              {
                break
              }

            }
          }

          newattribute[[1]][["unitsResolution"]] <-
            unitsResolution.input
        }

        ## attribute isCaseSensitive
        if (is.null(isCaseSensitive.input) == FALSE) {
          if (isCaseSensitive.input == "") {
            isCaseSensitive.choice <-
              utils::menu(c(TRUE, FALSE),
                          title =
                            cat(
                              paste0(
                                "\nOPTIONAL: Are the entry values of the attribute '",
                                codeName.input,
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

        }

        ## attribute missingValue
        if (is.null(missingValue.input) == FALSE) {
          if (c(TRUE, FALSE)[allowNull.choice] == TRUE) {
            if (missingValue.input == "") {
              message(cat(
                paste0(
                  "\nOPTIONAL: Provide the code which represents missing entry values for the attribute '",
                  codeName.input,
                  "' (e.g., NA, na, -).\nPress Enter to omit this information."
                )
              ))
              missingValue.input <-
                noquote(as.character(readline(prompt = )))
              newattribute[[1]][["missingValue"]] <-
                missingValue.input
            }
          }
        }


        ## attribute minValue
        if (is.null(minValue.input) == FALSE) {
          if (minValue.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the minimum value permissible for the attribute '",
                codeName.input,
                "'.\nPress Enter to omit this information."
              )
            ))
            minValue.input <-
              noquote(as.character(readline(prompt = )))
          }
          newattribute[[1]][["minValue"]] <- minValue.input
        }

        ## attribute maxValue
        if (is.null(maxValue.input) == FALSE) {
          if (maxValue.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the maximum value permissible for the attribute '",
                codeName.input,
                "'.\nPress Enter to omit this information."
              )
            ))
            maxValue.input <- as.character(readline(prompt =))
          }
          newattribute[[1]][["maxValue"]] <- maxValue.input
        }

        ## attribute fieldWidth
        if (is.null(fieldWidth.input) == FALSE) {
          if (fieldWidth.input == "") {
            message(cat(
              paste0(
                "\nOPTIONAL: Provide the field width (integer) of entry values for the attribute '",
                codeName.input,
                "'.\nPress Enter to omit this information."
              )
            ))
            fieldWidth.input <-
              noquote(as.character(readline(prompt = )))
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
            fieldWidth.input <-
              noquote(as.character(readline(prompt = )))

          }

          newattribute[[1]][["fieldWidth"]] <- fieldWidth.input

        }

        ## Add an associated domain

        if (is.null(domainId.input) == TRUE &
            is.null(domainName.input) == TRUE) {
          # Add attribute to dictionary list with domainId input or no domain
          if (quiet == FALSE) {
            update.choice <-
              utils::menu(c("disregard changes",
                            "looks good"),
                          title = cat(
                            paste0(
                              "\nThe attribute '",
                              codeName.input,
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

          if ("domainId" %in% definedparam) {
            newattribute[[1]][["domainId"]] <-
              domainId.input
            domain.choice <- 3
          } else if ("domainName" %in% definedparam) {
            domain.num <-
              which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                      domainName.input)
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
                              codeName.input,
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
                                  codeName.input,
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
                  codeName.input,
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
                                  codeName.input,
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
                                  codeName.input,
                                  "'?\n"
                                )
                              ))
              }

              if (domainfill.choice == 1) {
                newdomain[[1]][["codeName"]] <- codeName.input
                newdomain[[1]][["description"]] <-
                  newattribute[[1]][["definition"]]

                ## Provide new domain name and description
              } else {
                ## domain codeName
                if (domainName.input == "") {
                  while (domainName.input == "") {
                    message(cat(paste0(
                      "\nREQUIRED: Provide a name for the domain."
                    )))
                    domainName.input <-
                      as.character(readline(prompt =))
                  }
                }
                newdomain[[1]][["codeName"]] <-
                  domainName.input

                ## domain description
                if (domainDescription.input == "") {
                  while (domainDescription.input == "") {
                    message(cat(
                      paste0(
                        "\nREQUIRED: Provide a description for the domain '",
                        domainName.input,
                        "'."
                      )
                    ))
                    domainDescription.input <-
                      as.character(readline(prompt =))
                  }
                }
                newdomain[[1]][["description"]] <-
                  domainDescription.input

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
                                  codeName.input,
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
                                  domainName.input,
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
                codeName.input)

      ## Check if a single attribute exists
      if (length(attribute.num) > 1) {
        message(cat(
          paste0(
            "Operation canceled. More than one attribute with the codeName '",
            codeName.input,
            "' was found. Correct one or both attributes before proceeding."
          )
        ))
      } else if (length(attribute.num) == 0) {
        message(cat(
          paste0(
            "Operation canceled. No attribute with the codeName '",
            codeName.input,
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
            dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["codeName"]]

          message(cat(
            paste0(
              "The domain '",
              original.value,
              "' is already associated with the attribute '",
              codeName.input,
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
                    codeName.input,
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
                              codeName.input,
                              "' (recommended)?\n"
                            )
                          ))
          }

          if (domainfill.choice == 1) {
            newdomain[[1]][["codeName"]] <- codeName.input
            newdomain[[1]][["description"]] <-
              dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["definition"]]

            ## Provide new domain name and description
          } else {
            ## Correct required parameters
            if (is.null(domainName.input)) {
              domainName.input <- ""
            }
            if (is.null(domainId.input)) {
              domainId.input <- ""
            }
            if (is.null(domainDescription.input)) {
              domainId.input <- ""
            }

            ## domain codeName
            if (domainName.input == "") {
              while (domainName.input == "") {
                message(cat(
                  paste0("\nREQUIRED: Provide a name for the domain.")
                ))
                domainName.input <-
                  noquote(as.character(readline(prompt = )))
              }
            }
            newdomain[[1]][["codeName"]] <-
              domainName.input

            ## domain description
            if (domainDescription.input == "") {
              while (domainDescription.input == "") {
                message(cat(
                  paste0(
                    "\nREQUIRED: Provide a description for the domain '",
                    domainName.input,
                    "'."
                  )
                ))
                domainDescription.input <-
                  noquote(as.character(readline(prompt = )))
              }
            }
            newdomain[[1]][["description"]] <-
              domainDescription.input

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
                              codeName.input,
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
                                   domainName.input,
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
                domainId.input)

      newdomainItems <- blankdomainItem
      domainItem.num <- 1

      ## Correct required parameters
      if (is.null(domainName.input)) {
        domainName.input <- ""
      }
      if (is.null(domainId.input)) {
        domainId.input <- ""
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
            domainName.input,
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

    #### Update attribute info ####
    if (how == "update_attribute") {
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                codeName.input)

      ## Check if a single attribute exists
      if (length(attribute.num) > 1) {
        message(cat(
          paste0(
            "Operation canceled. More than one attribute with the codeName '",
            codeName.input,
            "' was found. Correct one or both attributes before proceeding."
          )
        ))
      } else if (length(attribute.num) == 0) {
        message(cat(
          paste0(
            "Operation canceled. No attribute with the codeName '",
            codeName.input,
            "' was found."
          )
        ))

      }

      originalattribute <-
        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]]

      newattribute <- originalattribute


      ## Make changes based on inputs
      attributeparam <- c(
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

      if (length(definedparam %in% attributeparam) != 0) {
        ## attribute allowNull
        if (is.null(allowNull.input) == FALSE) {
          if (allowNull.input != "") {
            newattribute[["allowNull"]] <- allowNull.input
          }
        }

        ## attribute dataType
        if (is.null(dataType.input) == FALSE) {
          if (dataType.input != "") {
            newattribute[["dataType"]] <- dataType.input
          }
        }

        ## attribute unitsResolution
        if (is.null(unitsResolution.input) == FALSE) {
          if (unitsResolution.input != "") {
            newattribute[["unitsResolution"]] <- unitsResolution.input
          }
        }

        ## attribute isCaseSensitive
        if (is.null(isCaseSensitive.input) == FALSE) {
          if (isCaseSensitive.input != "") {
            newattribute[["isCaseSensitive"]] <- isCaseSensitive.input
          }
        }

        ## attribute missingValue
        if (is.null(missingValue.input) == FALSE) {
          if (missingValue.input != "") {
            newattribute[["missingValue"]] <- missingValue.input
          }
        }

        ## attribute minValue
        if (is.null(minValue.input) == FALSE) {
          if (minValue.input != "") {
            newattribute[["minValue"]] <- minValue.input
          }
        }

        ## attribute maxValue
        if (is.null(maxValue.input) == FALSE) {
          if (maxValue.input != "") {
            newattribute[["maxValue"]] <- maxValue.input
          }
        }

        ## attribute fieldWidth
        if (is.null(fieldWidth.input) == FALSE) {
          if (fieldWidth.input != "") {
            newattribute[["fieldWidth"]] <- fieldWidth.input
          }
        }

        ## attribute domainId
        if (is.null(domainId.input) == FALSE) {
          if (domainId.input != "") {
            newattribute[["domainId"]] <- domainId.input
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
                  codeName.input,
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
                          paste0("\nThe attribute '", codeName.input, "' is described as followed.\n"),
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
                          "\nWhich field would you like to add/modify?\n"
                        )))


        # Create field if it doesn't exist

        if (attributefield[field.choice] %in% names(newattribute) == FALSE) {
          newfield <- list()
          newfield[[paste0(attributefield[field.choice])]] <- ""
          newattribute <- append(newattribute, newfield)
        }

        ## attribute allowNull
        if (attributefield[field.choice] == "allowNull") {
          allowNull.choice <- 0
          originalvalue <- newattribute[["allowNull"]]

          allowNull.choice <-
            utils::menu(c(TRUE, FALSE),
                        title =
                          cat(
                            paste0(
                              "\nAre null values permitted for the attribute '",
                              codeName.input,
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
        } else if (attributefield[field.choice] == "dataType") {
          dataType.choice <- 0
          originalvalue <- newattribute[["dataType"]]

          dataType.choice <-
            utils::menu(c(dataType.vector),
                        title = cat(
                          paste0(
                            "\nSelect the datatype/format for entry values of the attribute '",
                            codeName.input,
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
        } else if (attributefield[field.choice] == "definition") {
          originalvalue <- newattribute[["definition"]]

          message(cat(
            paste0(
              "\nProvide a new definition for the attribute '",
              codeName.input,
              "'. \nThe current definition is '",
              originalvalue,
              "'\nPress Enter to abort changes."
            )
          ))
          definition.input <- as.character(readline(prompt =))


          if (definition.input != "") {
            newattribute[["definition"]] <- definition.input
          }

          ## attribute units
        } else if (attributefield[field.choice] == "units") {
          originalvalue <- newattribute[["units"]]

          message(cat(
            paste0(
              "\nProvide the unit-of-measure for the attribute '",
              codeName.input,
              "' (e.g., meters, atmospheres, liters). \nThe current unit is '",
              originalvalue,
              "'. \nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          units.input <-
            noquote(as.character(readline(prompt = )))

          if (units.input == "rm()") {
            newattribute[["units"]] <- ""
          } else if (units.input != "") {
            newattribute[["units"]] <- units.input
          }

          ## attribute unitsResolution
        } else if (attributefield[field.choice] == "unitsResolution") {
          originalvalue <- newattribute[["unitsResolution"]]

          message(cat(
            paste0(
              "\nProvide the smallest unit increment (numeric) to which the attribute '",
              codeName.input,
              "' is measured (e.g., 1, .1, .01). \nThe current unit resolution is '",
              originalvalue,
              "'\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          unitsResolution.input <-
            noquote(as.character(readline(prompt = )))

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
        } else if (attributefield[field.choice] == "isCaseSensitive") {
          isCaseSensitive.choice <- 0
          originalvalue <- newattribute[["isCaseSensitive"]]

          isCaseSensitive.choice <-
            utils::menu(c(TRUE, FALSE),
                        title =
                          cat(
                            paste0(
                              "\nAre the entry values of the attribute '",
                              codeName.input,
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
                                codeName.input,
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
                codeName.input,
                "' (e.g., NA, na, -).\nThe current code is '",
                originalvalue,
                "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
              )
            ))
            missingValue.input <-
              noquote(as.character(readline(prompt = )))

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
        } else if (attributefield[field.choice] == "minValue") {
          originalvalue <- newattribute[["minValue"]]

          message(cat(
            paste0(
              "\nProvide the minimum value permissible for the attribute '",
              codeName.input,
              "'.\nThe current minimum value is '",
              originalvalue,
              "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          minValue.input <-
            noquote(as.character(readline(prompt = )))

          if (minValue.input == "rm()") {
            newattribute[["minValue"]] <- ""
          } else if (minValue.input != "") {
            newattribute[["minValue"]] <- minValue.input
          }

          ## attribute maxValue
        } else if (attributefield[field.choice] == "maxValue") {
          originalvalue <- newattribute[["maxValue"]]

          message(cat(
            paste0(
              "\nProvide the maximum value permissible for the attribute '",
              codeName.input,
              "'.\nThe current maximum value is '",
              originalvalue,
              "'.\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          maxValue.input <-
            noquote(as.character(readline(prompt = )))


          if (maxValue.input == "rm()") {
            newattribute[["maxValue"]] <- ""
          } else if (minValue.input != "") {
            newattribute[["maxValue"]] <- maxValue.input
          }

          ## attribute fieldWidth
        } else if (attributefield[field.choice] == "fieldWidth") {
          originalvalue <- newattribute[["fieldWidth"]]

          message(cat(
            paste0(
              "\nProvide the field width (integer) of entry values for the attribute '",
              codeName.input,
              "'.\nThe current field width is '",
              originalvalue,
              "'\nEnter 'rm()' to remove the current value. Press Enter to abort changes."
            )
          ))
          fieldWidth.input <-
            noquote(as.character(readline(prompt = )))

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
              codeName.input,
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
                  codeName.input,
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

    #### Update domain info ####

    #### Reattach dictionary list ####
    input.dxnry[["data"]][[1]][["attributes"]][["json"]] <-
      rjson::toJSON(dictionarylist)

    if (length(x[["data"]]) > 1) {
      x[["data"]][[record.num]] <- input.dxnry[["data"]][[1]]

      return(x)
    } else {
      return(input.dxnry)
    }
  }
