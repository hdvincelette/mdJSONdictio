#' Modify mdJSON Data Dictionaries
#'
#' Amends an mdJSON data dictionary, including adding/updating attributes and domains.
#' @param x List object converted from an mdJSON data dictionary file.
#' @param how character string matching one of the modification options: see ‘Details’.
#' @param options a named list with additional options that only apply to certain choices of how: see ‘Details’.
#' @return Returns a modified list object corresponding to the mdJSON data dictionary file.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```modify.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' input.json <- rjson::fromJSON(file = path)
#'
#' # Add an attribute
#' modified.json<- modify.mdJSON(x = input.json, how = "add_attribute", options = list(codeName = "WingArea"))
#'
#' #' # Add a domain
#' modified.json<- modify.mdJSON(x = input.json, how = "add_domain")
#'
#' # Export table to disk
#' write.csv(x = new.table, file = "e.g.dictionary2.csv", na="", row.names = FALSE)







modify.mdJSON <-
  function(x,
           how = c("add_attribute", "add_domain", "edit_attribute"),
           options) {
    blankjson <- blankjson
    blankdictionarylist <-
      rjson::fromJSON(blankjson[["data"]][[1]][["attributes"]][["json"]])
    blankattribute <-
      blankdictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]]
    blankdomain <-
      blankdictionarylist[["dataDictionary"]][["domain"]]

    dataType.vector <- dplyr::pull(
      readxl::read_excel(
        "inst/templates/mdJSONdictio_Dictionary_Template_v1.xlsx",
        sheet = 3,
        col_names = FALSE
      ) ,
      1
    )

    `%>%` <- magrittr::`%>%`

    input.dict <- x

    if (missing(options[["codeName"]])) {
      codeName <- ""
      while (codeName == "") {
        message(cat(paste0("\nREQUIRED: Provide a codeName.")))
        codeName <- as.character(readline(prompt = ))
      }
    }

    dictionarylist <-
      rjson::fromJSON(input.dict[["data"]][[1]][["attributes"]][["json"]])


    if (how = "add_attribute") {
      #### Add an attribute (and domain - optional) ####

      newattribute <- blankattribute

      newattribute[[1]][["codeName"]] <- codeName

      allowNull.choice <- 0

      while (allowNull.choice == 0) {
        allowNull.choice <-
          utils::menu(c("yes", "no"),
                      title =
                        cat(
                          paste0(
                            "\nREQUIRED: Indicate whether null values are permitted for the attribute '",
                            codeName,
                            "'.\n"
                          )
                        ))
      }
      newattribute[[1]][["allowNull"]] <-
        c("yes", "no")[allowNull.choice]


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



      definition.input <- ""

      while (definition.input == "") {
        message(cat(
          paste0(
            "\nREQUIRED: Provide a brief definition for the attribute '",
            codeName,
            "'."
          )
        ))
        definition.input <- as.character(readline(prompt = ))
      }
      newattribute[[1]][["definition"]] <- definition.input


      message(cat(
        paste0(
          "\nOPTIONAL: Indicate the unit-of-measure for the attribute '",
          codeName,
          "' (e.g., meters, atmospheres, liters)."
        )
      ))
      units.input <- as.character(readline(prompt = ))
      newattribute[[1]][["units"]] <- units.input


      message(cat(
        paste0(
          "\nOPTIONAL: Indicate the smallest unit increment (decimal) to which the attribute '",
          codeName,
          "' is measured (e.g., 1, .1, .01)."
        )
      ))
      unitsResolution.input <- as.character(readline(prompt =))
      newattribute[[1]][["unitsResolution"]] <-
        unitsResolution.input


      isCaseSensitive.choice <-
        utils::menu(c("yes", "no"),
                    title =
                      cat(
                        paste0(
                          "\nOPTIONAL: Indicate whether the entry values of the attribute '",
                          codeName,
                          "' are encoded in case-sensitive ASCII.\n"
                        )
                      ))
      if (isCaseSensitive.choice == 0) {
        isCaseSensitive.choice <- ""
      } else{
        isCaseSensitive.choice <- c("yes", "no")[isCaseSensitive.choice]
      }
      newattribute[[1]][["isCaseSensitive"]] <-
        isCaseSensitive.choice



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


      message(cat(
        paste0(
          "\nOPTIONAL: Provide the minimum range value permissible for the attribute '",
          codeName,
          "'."
        )
      ))
      minValue.input <- as.character(readline(prompt =))
      newattribute[[1]][["minValue"]] <- minValue.input


      message(cat(
        paste0(
          "\nOPTIONAL: Provide the maximum range value permissible for the attribute '",
          codeName,
          "'."
        )
      ))
      maxValue.input <- as.character(readline(prompt = ))
      newattribute[[1]][["maxValue"]] <- maxValue.input


      message(cat(
        paste0(
          "\nOPTIONAL: Indicate the field width (integer) of entry values for the attribute '",
          codeName,
          "'."
        )
      ))
      fieldWidth.input <- as.character(readline(prompt =))
      newattribute[[1]][["fieldWidth"]] <- fieldWidth.input


      dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]]) +
                                                                            1]] <-
        newattribute[[1]]



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
      newattribute[[1]][["domainId"]] <-
        uuid::UUIDgenerate(use.time = FALSE, n = 1)

      if (c("yes", "no")[domainId.choice] == "yes") {
        newdomain <- blankdomain

        newdomain[[1]][["domainId"]] <-
          newattribute[[1]][["domainId"]]
        newdomain[[1]][["codeName"]] <- codeName
        newdomain[[1]][["description"]] <-
          newattribute[[1]][["definition"]]



        domainItems.choice <- 0

        while (domainItems.choice == 0) {
          domainItems.choice <-
            utils::menu(c("yes", "no"),
                        title = cat(
                          paste0(
                            "\nREQUIRED: Indicate whether you would you like to add domain items for the attribute '",
                            codeName,
                            "'.\n"
                          )
                        ))

        }


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

            domainItem_name.input <- ""

            while (domainItem_name.input == "") {
              message(cat(
                paste0(
                  "\nREQUIRED: Provide a domain item name for the attribute '",
                  codeName,
                  "'."
                )
              ))
              domainItem_name.input <-
                as.character(readline(prompt =))
            }
            newdomain[[1]][["domainItem"]][[domainItem.num]][["name"]] <-
              domainItem_name.input


            domainItem_value.input <- ""

            while (domainItem_value.input == "") {
              message(cat(
                paste0(
                  "\nREQUIRED: Provide a domain value (entry value) for the attribute '",
                  codeName,
                  "'."
                )
              ))
              domainItem_value.input <-
                as.character(readline(prompt = ))
            }
            newdomain[[1]][["domainItem"]][[domainItem.num]][["value"]] <-
              domainItem_value.input

          }

          domainItem_defintion.input <- ""

          while (domainItem_defintion.input == "") {
            message(cat(
              paste0(
                "\nREQUIRED: Provide a domain value (entry value) for the attribute '",
                codeName,
                "'."
              )
            ))
            domainItem_defintion.input <-
              as.character(readline(prompt =))
          }
          newdomain[[1]][["domainItem"]][[domainItem.num]][["definition"]] <-
            domainItem_defintion.input


          moredomainItems.choice <- utils::menu(c("yes", "no"),
                                                title = cat(
                                                  paste0(
                                                    "\nREQUIRED: Indicate whether you would like to add an additional domain item for the attribute '",
                                                    codeName,
                                                    "'."
                                                  )
                                                ))

        }

        dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) +
                                                          1]] <-
          newdomain[[1]]
      }

    }


    #### Add domain to existing attribute ####


    if (how == "add_domain") {
      newdomain <- blankdomain

      newdomain[[1]][["domainId"]] <-
        newdomain[[1]][["codeName"]] <- codeName
      newdomain[[1]][["description"]] <-


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

        domainItem_name.input <- ""

        while (domainItem_name.input == "") {
          message(cat(
            paste0(
              "\nREQUIRED: Provide a domain item name for the attribute '",
              codeName,
              "'."
            )
          ))
          domainItem_name.input <-
            as.character(readline(prompt =))
        }
        newdomain[[1]][["domainItem"]][[domainItem.num]][["name"]] <-
          domainItem_name.input


        domainItem_value.input <- ""

        while (domainItem_value.input == "") {
          message(cat(
            paste0(
              "\nREQUIRED: Provide a domain value (entry value) for the attribute '",
              codeName,
              "'."
            )
          ))
          domainItem_value.input <-
            as.character(readline(prompt = ))
        }
        newdomain[[1]][["domainItem"]][[domainItem.num]][["value"]] <-
          domainItem_value.input

      }

      domainItem_defintion.input <- ""

      while (domainItem_defintion.input == "") {
        message(cat(
          paste0(
            "\nREQUIRED: Provide a domain value (entry value) for the attribute '",
            codeName,
            "'."
          )
        ))
        domainItem_defintion.input <-
          as.character(readline(prompt =))
      }
      newdomain[[1]][["domainItem"]][[domainItem.num]][["definition"]] <-
        domainItem_defintion.input


      moredomainItems.choice <- utils::menu(c("yes", "no"),
                                            title = cat(
                                              paste0(
                                                "\nREQUIRED: Indicate whether you would like to add an additional domain item for the attribute '",
                                                codeName,
                                                "'."
                                              )
                                            ))


      dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) +
                                                        1]] <-
        newdomain[[1]]

    }


  }

