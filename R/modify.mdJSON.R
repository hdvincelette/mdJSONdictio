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
#' modified.json<- modify.mdJSON(x = input.json, how = "add_attribute", options = list(codeName = "WingArea")))
#'
#' #' # Add a domain
#' modified.json<- modify.mdJSON(x = input.json, how = "add_attribute")
#'
#' # Export table to disk
#' write.csv(x = new.table, file = "e.g.dictionary2.csv", na="", row.names = FALSE)



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



#### Add an attribute ####


newattribute <- blankattribute


for (b in 1:length(codeName.vector)) {
  codeName <- codeName.vector[b]

  if (codeName.num > 1) {
    newattribute <-
      append(newattribute, blankattribute)
  }

  newattribute[[codeName.num]][["codeName"]] <- codeName

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
  newattribute[[codeName.num]][["allowNull"]] <-
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
  newattribute[[codeName.num]][["dataType"]] <-
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
    definition.input <- as.character(readline(prompt =))
  }
  newattribute[[codeName.num]][["definition"]] <- definition.input


  message(cat(
    paste0(
      "\nOPTIONAL: Indicate the unit-of-measure for the attribute '",
      codeName,
      "' (e.g., meters, atmospheres, liters)."
    )
  ))
  units.input <- as.character(readline(prompt =))
  newattribute[[codeName.num]][["units"]] <- units.input


  message(cat(
    paste0(
      "\nOPTIONAL: Indicate the smallest unit increment (decimal) to which the attribute '",
      codeName,
      "' is measured (e.g., 1, .1, .01)."
    )
  ))
  unitsResolution.input <- as.character(readline(prompt = ))
  newattribute[[codeName.num]][["unitsResolution"]] <-
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
  newattribute[[codeName.num]][["isCaseSensitive"]] <-
    isCaseSensitive.choice



  if (c("yes", "no")[allowNull.choice] == "yes") {
    message(cat(
      paste0(
        "\nOPTIONAL: Provide the code which represents missing entry values for the attribute '",
        codeName,
        "' (e.g., NA, na, -)."
      )
    ))
    missingValue.input <- as.character(readline(prompt =))
    newattribute[[codeName.num]][["missingValue"]] <-
      missingValue.input
  }


  message(cat(
    paste0(
      "\nOPTIONAL: Provide the minimum range value permissible for the attribute '",
      codeName,
      "'."
    )
  ))
  minValue.input <- as.character(readline(prompt = ))
  newattribute[[codeName.num]][["minValue"]] <- minValue.input


  message(cat(
    paste0(
      "\nOPTIONAL: Provide the maximum range value permissible for the attribute '",
      codeName,
      "'."
    )
  ))
  maxValue.input <- as.character(readline(prompt =))
  newattribute[[codeName.num]][["maxValue"]] <- maxValue.input


  message(cat(
    paste0(
      "\nOPTIONAL: Indicate the field width (integer) of entry values for the attribute '",
      codeName,
      "'."
    )
  ))
  fieldWidth.input <- as.character(readline(prompt = ))
  newattribute[[codeName.num]][["fieldWidth"]] <- fieldWidth.input




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
  newattribute[[codeName.num]][["domainId"]] <-
    uuid::UUIDgenerate(use.time = FALSE, n = 1)

  if (c("yes", "no")[domainId.choice] == "yes") {
    if (exists("newdomain") == TRUE) {
      newdomain <-
        append(newdomain, blankdomain)
      domain.num <- length(newdomain)
    } else {
      newdomain <- blankdomain
      domain.num <- 1
    }

    newdomain[[domain.num]][["domainId"]] <-
      newattribute[[codeName.num]][["domainId"]]
    newdomain[[domain.num]][["codeName"]] <- codeName
    newdomain[[domain.num]][["description"]] <-
      newattribute[[codeName.num]][["definition"]]



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
        if (newdomain[[domain.num]][["domainItem"]][[1]][["name"]] != "NA") {
          newdomain[[domain.num]][["domainItem"]] <-
            append(newdomain[[domain.num]][["domainItem"]], blankdomain[[1]][["domainItem"]])
          domainItem.num <-
            length(newdomain[[domain.num]][["domainItem"]])
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
            as.character(readline(prompt = ))
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
            as.character(readline(prompt =))
        }
        newdomain[[domain.num]][["domainItem"]][[domainItem.num]][["value"]] <-
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
          as.character(readline(prompt = ))
      }
      newdomain[[domain.num]][["domainItem"]][[domainItem.num]][["definition"]] <-
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
  }
}


