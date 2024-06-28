#' Modify mdJSON Data Dictionaries
#'
#' Amends an mdJSON data dictionary, including adding, updating, and removing attributes and domains.
#' @param x List. Metadata file containing a Dictionary Record.
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
#' @param domainItem Optional list. Permissible entry value(s), name(s), and definition(s) of the domain specified with 'domain_codeName', i.e., domainItem = list(list(name = "", value = "", definition = "")). Unnamed list elements must be ordered correctly (name, value, definition). 'domainItem' will override 'domainItem_value', 'domainItem_name', and 'domainItem_definition' when 'how' == 'add_attribute', 'add_domain' or 'add_domainItem.
#' @param domainItem_value Optional character string or vector. Permissible entry value(s) of the domain specified with 'domain_codeName'.
#' @param domainItem_name Optional character string or vector. Name(s) of permissible entry value(s) specified with 'domainItem_value'.
#' @param domainItem_definition Optional character string or vector. Definition(s) of permissible entry value(s) specified with 'domainItem_value'.
#' @param new.attribute_codeName Optional character string. New name for the attribute specified with 'attribute_codeName'.
#' @param new.domain_codeName Optional character string. New name for the domain specified with 'domain_codeName'.
#' @param new.domainItem_value Optional character string. New entry value for the domain item specified with 'domainItem_value'.
#' @param quiet Optional logical. Default=FALSE. If TRUE, function runs with minimum input required and does not print revisions for approval.
#' @return Returns a modified list object corresponding to the mdJSON file.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```modify.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' # path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' # input.dxnry <- rjson::fromJSON(file = path)
#'
#' # Add an attribute
#' # modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_attribute", attribute_codeName = "WingArea")
#'
#' # Add a domain
#' # modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domain")
#'
#' # Add a domain item
#' # modified.dxnry<- modify.mdJSON(x = input.dxnry, how = "add_domainItem", attribute_codeName = "WingArea", domainItem_value = "U")
#'
#' # Update attribute datatype and allowNull values
#' # modified.dxnry<- modify.mdJSON(x = ref.dictionary, how = "update_attribute", attribute_codeName = "BandSize", attribute_dataType = "Integer", attribute_allowNull = TRUE)
#'
#' # Update domain codeName
#' # modified.dxnry<- modify.mdJSON(x = ref.dictionary, how = "update_domain", domain_codeName = "Wing_mm", new.domain_codeName = "WingFlattened_mm")
#'
#' # Convert list to JSON
#' # new.json = rjson::toJSON(x = modified.dxnry)
#'
#' # Export JSON to disk
#' # write(x = new.json, file = "e.g.dictionary.json")


# update_domainItem
# remove domain item, attribute, domain

modify.mdJSON <-
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
           domainItem_value = "",
           domainItem_name = "",
           domainItem_definition = "",
           new.attribute_codeName = "",
           new.domain_codeName = "",
           new.domainItem_value = "",
           quiet = FALSE) {
    `%>%` <- magrittr::`%>%`

    blankattribute <- get0("blankattribute", envir = asNamespace("mdJSONdictio"))
    blankdictionarylist <- get0("blankdictionarylist", envir = asNamespace("mdJSONdictio"))
    blankdomain <- get0("blankdomain", envir = asNamespace("mdJSONdictio"))
    blankdomainItem <- get0("blankdomainItem", envir = asNamespace("mdJSONdictio"))
    dataType.vector <- get0("dataType.vector", envir = asNamespace("mdJSONdictio"))



    #### Extract dictionary list ####
    if (length(x[["data"]]) > 1) {
      input.dxnry <-
        mdJSONdictio::extract.mdJSON(x = x,
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


    #### Check: all parameters ####
    definedparam <- c()

    if (missing(attribute_codeName) | attribute_allowNull == "") {
      attribute_codeName <- ""
    } else {
      if (is.null(attribute_codeName) == FALSE) {
        definedparam <- c(definedparam, "attribute_codeName")
      }
    }
    attribute_codeName.input <- attribute_codeName

    if (missing(attribute_allowNull) | attribute_allowNull == "") {
      attribute_allowNull <- ""
    } else if (is.null(attribute_allowNull) == FALSE) {
      if (!attribute_allowNull %in% c(TRUE, FALSE)) {
        message(cat(
          paste0("'attribute_allowNull' is invalid and will be ignored.")
        ))
        attribute_allowNull <- ""
      } else {
        definedparam <- c(definedparam, "attribute_allowNull")
      }
    }
    attribute_allowNull.input <- attribute_allowNull

    if (missing(attribute_dataType) | attribute_dataType == "") {
      attribute_dataType <- ""
    } else if (is.null(attribute_dataType) == FALSE) {
      if (!attribute_dataType %in% dataType.vector) {
        message(cat(
          paste0("'attribute_dataType' is invalid and will be ignored.")
        ))
        attribute_dataType <- ""
      } else {
        definedparam <- c(definedparam, "attribute_dataType")
      }
    }
    attribute_dataType.input <- attribute_dataType

    if (missing(attribute_definition) | attribute_definition == "") {
      attribute_definition <- ""
    } else if (is.null(attribute_definition) == FALSE) {
      definedparam <- c(definedparam, "attribute_definition")
    }
    attribute_definition.input <- attribute_definition

    if (missing(attribute_units) | attribute_units == "") {
      attribute_units <- ""
    } else if (is.null(attribute_units) == FALSE) {
      definedparam <- c(definedparam, "attribute_units")
    }
    attribute_units.input <- attribute_units

    if (missing(attribute_unitsResolution) | attribute_unitsResolution == "") {
      attribute_unitsResolution <- ""
    } else if (is.null(attribute_unitsResolution) == FALSE) {
      if (suppressWarnings(is.na(as.numeric(attribute_unitsResolution))) == TRUE) {
        message(cat(
          paste0(
            "'attribute_unitsResolution' is invalid and will be ignored."
          )
        ))
        attribute_unitsResolution <- ""
      } else {
        definedparam <- c(definedparam, "attribute_unitsResolution")
      }
    }
    attribute_unitsResolution.input <- attribute_unitsResolution

    if (missing(attribute_isCaseSensitive) | attribute_isCaseSensitive == "") {
      attribute_isCaseSensitive <- ""
    } else if (is.null(attribute_isCaseSensitive) == FALSE) {
      if (!attribute_isCaseSensitive %in% c(TRUE, FALSE)) {
        message(cat(
          paste0(
            "'attribute_isCaseSensitive' is invalid and will be ignored."
          )
        ))
        attribute_isCaseSensitive <- ""
      } else {
        definedparam <- c(definedparam, "attribute_isCaseSensitive")
      }
    }
    attribute_isCaseSensitive.input <- attribute_isCaseSensitive

    if (missing(attribute_missingValue) | attribute_missingValue == "") {
      attribute_missingValue <- ""
    } else if (is.null(attribute_missingValue) == FALSE) {
        definedparam <- c(definedparam, "attribute_missingValue")
      }
    attribute_missingValue.input <- attribute_missingValue

    if (missing(attribute_minValue) | attribute_minValue == "") {
      attribute_minValue <- ""
    } else if (is.null(attribute_minValue) == FALSE) {
      definedparam <- c(definedparam, "attribute_minValue")
    }
    attribute_minValue.input <- attribute_minValue

    if (missing(attribute_maxValue) | attribute_maxValue == "") {
      attribute_maxValue <- ""
    } else if (is.null(attribute_maxValue) == FALSE) {
      definedparam <- c(definedparam, "attribute_maxValue")
    }
    attribute_maxValue.input <- attribute_maxValue

    if (missing(attribute_fieldWidth) | attribute_fieldWidth == "") {
      attribute_fieldWidth <- ""
    } else if (is.null(attribute_fieldWidth) == FALSE) {
      if (!grepl("[^[:digit:]]",
                 format(
                   attribute_fieldWidth,
                   digits = 20,
                   scientific = FALSE
                 )) == FALSE) {
        message(cat(
          paste0("'attribute_fieldWidth' is invalid and will be ignored.")
        ))
        attribute_fieldWidth <- ""
      } else {
        definedparam <- c(definedparam, "attribute_fieldWidth")
      }
    }
    attribute_fieldWidth.input <- attribute_fieldWidth

    if (missing(domain_domainId) | domain_domainId == "") {
      domain_domainId <- ""
    } else if (is.null(domain_domainId) == FALSE) {
      if (!domain_domainId %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "domainId")) {
        message(cat(
          paste0("'domain_domainId' is invalid and will be ignored.")
        ))
        domain_domainId <- ""
      } else {
        definedparam <- c(definedparam, "domain_domainId")
      }
    }
    domain_domainId.input <- domain_domainId

    if (missing(domain_codeName) | domain_codeName == "") {
      domain_codeName <- ""
    } else if (is.null(domain_codeName) == FALSE) {
      if (!domain_codeName %in% sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")) {
        message(cat(
          paste0("'domain_codeName' is invalid and will be ignored.")
        ))
        domain_codeName <- ""
      } else {
        definedparam <- c(definedparam, "domain_codeName")
      }
    }
    domain_codeName.input <- domain_codeName

    if (missing(domain_description) | domain_description == "") {
      domain_description <- ""
    } else if (is.null(domain_description) == FALSE) {
      definedparam <- c(definedparam, "domain_description")
    }
    domain_description.input <- domain_description

    if (missing(domainItem) | length(domainItem)==0) {
      domainItem <-  list()
    } else if (is.null(domainItem) == FALSE) {
        definedparam <- c(definedparam, "domainItem")
    }
    domainItem.input <- domainItem

    if (missing(domainItem_value) | domainItem_value == "") {
      domainItem_value <- ""
    } else if (is.null(domainItem_value) == FALSE) {
        definedparam <- c(definedparam, "domainItem_value")
    }
    domainItem_value.input <- domainItem_value

    if (missing(domainItem_name) | domainItem_name == "") {
      domainItem_name <- ""
    } else if (is.null(domainItem_name) == FALSE) {
        definedparam <- c(definedparam, "domainItem_name")
    }
    domainItem_name.input <- domainItem_name

    if (missing(domainItem_definition) | domainItem_definition == "") {
      domainItem_definition <- ""
    } else if (is.null(domainItem_definition) == FALSE) {
        definedparam <- c(definedparam, "domainItem_definition")
    }
    domainItem_definition.input <- domainItem_definition

    if (missing(new.attribute_codeName) | new.attribute_codeName == "") {
      new.attribute_codeName <- ""
    } else if (is.null(new.attribute_codeName) == FALSE) {
        definedparam <- c(definedparam, "new.attribute_codeName")
    }
    new.attribute_codeName.input <- new.attribute_codeName

    if (missing(new.domain_codeName) | new.domain_codeName == "") {
      new.domain_codeName <- ""
    } else if (is.null(new.domain_codeName) == FALSE) {
      definedparam <- c(definedparam, "new.domain_codeName")
    }
    new.domain_codeName.input <- new.domain_codeName

    if (missing(new.domainItem_value) | new.domainItem_value == "") {
      new.domainItem_value <- ""
    } else if (is.null(new.domainItem_value) == FALSE) {
      definedparam <- c(definedparam, "new.domainItem_value")
    }
    new.domainItem_value.input <- new.domainItem_value

    if (missing(quiet)) {
      quiet <- FALSE
    }

    ## Correct required parameters
    if (how == "add_attribute") {
      if (is.null(attribute_codeName.input)) {
        attribute_codeName.input <- ""
      }
      if (is.null(attribute_allowNull.input)) {
        attribute_allowNull.input <- ""
      }
      if (is.null(attribute_dataType.input)) {
        attribute_dataType.input <- ""
      }
      if (is.null(attribute_definition.input)) {
        attribute_definition.input <- ""
      }

    } else if (how == "add_domain") {
      if (is.null(attribute_codeName.input)) {
        attribute_codeName.input <- ""
      }
      if (is.null(domain_domainId.input)) {
        domain_domainId.input <- ""
      }
      if (is.null(domain_codeName.input)) {
        domain_codeName.input <- ""
      }
      if (is.null(domain_description.input)) {
        domain_description.input <- ""
      }

    } else if (how ==  "add_domainItem") {
      if (is.null(domain_domainId.input)) {
        domain_domainId.input <- ""
      }
      if (is.null(domain_codeName.input)) {
        domain_codeName.input <- ""
      }
      if (is.null(domainItem.input)) {
        domainItem.input <- list()
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

    } else if (how %in% c("update_attribute", "remove_attribute")) {
      if (is.null(attribute_codeName.input)) {
        attribute_codeName.input <- ""
      }

    } else if (how %in% c("update_domain", "remove_domain")) {
      if (is.null(domain_domainId.input)) {
        domain_domainId.input <- ""
      }
      if (is.null(domain_codeName.input)) {
        domain_codeName.input <- ""
      }

    } else if (how %in% c("update_domainItem", "remove_domainItem")) {
      if (is.null(domain_domainId.input)) {
        domain_domainId.input <- ""
      }
      if (is.null(domain_codeName.input)) {
        domain_codeName.input <- ""
      }
      if (is.null(domainItem_value.input)) {
        domainItem_value.input <- ""
      }
    }

    # definedparam <- definedparam[!mget(definedparam) %in% c("")]
    # definedparam <-
    #   names(Filter(Negate(is.null), mget(definedparam)))

    #### Check: attribute codeName ####
    ## If new attribute codeName required
    if (how %in% c("add_attribute")) {
      while (attribute_codeName.input == "") {
        message(cat(paste0(
          "\nREQUIRED: Provide the attribute name."
        )))
        attribute_codeName.input <-
          noquote(as.character(readline(prompt = )))
      }
      definedparam <- c(definedparam, "attribute_codeName")

      # Check if attribute already exists
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
        how <- "quit"
      } else {
        definedparam <- c(definedparam, "attribute_codeName")
      }
    }

    ## If existing attribute codeName required
    if (how %in% c("add_domain", "update_attribute", "remove_attribute")) {
      if (attribute_codeName.input == "") {
        attribute.opt <-
          sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName")
        attribute.choice <- 0

        while (attribute.choice == 0) {
          attribute.choice <-
            utils::menu(c(attribute.opt), title =
                          cat(paste0(
                            "\nREQUIRED: Select an attribute to modify.\n"
                          )))

        }
        attribute_codeName.input <- unlist(attribute.opt[attribute.choice])
        definedparam <- c(definedparam, "attribute_codeName")
      } else {
        ## Check input
        attribute.num <-
          which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                  attribute_codeName.input)

        if (length(attribute.num) > 1) {
          message(cat(
            paste0(
              "Operation canceled. More than one attribute with the name '",
              attribute_codeName.input,
              "' was found. Correct one or both attributes before proceeding."
            )
          ))
          how <- "quit"
        } else if (length(attribute.num) == 0) {
          message(cat(
            paste0(
              "Operation canceled. No attribute with the name '",
              attribute_codeName.input,
              "' was found."
            )
          ))
          how <- "quit"

        }
      }
    }

    #### Check: domain id/codeName ####
    ## If existing domain codeName required
    if (how %in% c(
      "add_domainItem",
      "update_domain",
      "update_domainItem",
      "remove_domain",
      "remove_domainItem"
    )) {
      if (domain_domainId.input == "" &
          domain_codeName.input == "") {
        domain.opt <-
          sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")
        domain.choice <- 0

        while (domain.choice == 0) {
          domain.choice <-
            utils::menu(c(domain.opt), title =
                          cat(paste0(
                            "\nREQUIRED: Select a domain to modify.\n"
                          )))

        }
        domain_codeName.input <- unlist(domain.opt[domain.choice])
        definedparam <- c(definedparam, "domain_codeName")

      }
    }

    ## Check/auto-fill input
    if (is.null(domain_codeName.input) == FALSE) {
      if (domain_codeName.input != "") {
        if (is.null(domain_domainId.input)) {
          domain_domainId.input <- ""
        }
        if (domain_domainId.input == "") {
          domain.num <-
            which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                    domain_codeName.input)

          # Does a single domain exist
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
    }

    # Do the domain Id and codeName correspond
    if (is.null(domain_domainId.input) == FALSE &
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
                         "update_domain",
                         "update_domainItem")) {
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

    #### Check: domainItem ####
    # domainItem.input = list(
    #   list(
    #     value = "Traveling",
    #     names = NULL,
    #     definition = "aaa"
    #   ),
    #   list(
    #     name = "b",
    #     value = "Traveling",
    #     definition = "bbb"
    #   ),
    #   list(
    #     name = "Incidental",
    #     value = "cc",
    #     definition = "ccc"
    #   ),
    #   list(
    #     name = "Incidental",
    #     value = "dd",
    #     definition = NA
    #   )
    # )
    # domainItem.input = list(list("a", value = "aa", definition = "aaa"),
    #                         list("b",
    #                              "bb",
    #                              "bbb"))
    # domainItem.input = list(list("a", "aa", "aaa"), list("b", "bb", "bbb"))
    #
    # domainItem.input <- list()
    #
    # domainItem_value.input = "a"
    # domainItem_name.input = "aa"
    # domainItem_definition.input = "aaa"
    #
    # domainItem_value.input = c("a", "b", "c")
    # domainItem_name.input = c("aa", "bb", "cc")
    # domainItem_definition.input = c("aaa", "bbb", "ccc")
    #
    # domainItem_value.input = c("a", "", "c")
    # domainItem_name.input = c("aa", "bb", "cc")
    # domainItem_definition.input = c("aaa", "bbb", "")
    #
    # domainItem_value.input = list(value = "a", "b", "c")
    # domainItem_name.input = list(name = "aa", "bb", "cc")
    # domainItem_definition.input = list(definition = "aaa", "bbb", "ccc")

    ## If existing domainItem value required
    if (how %in% c("update_domainItem", "remove_domainItem")) {
      domain.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                domain_codeName.input)

      domainItem_value.input <- unlist(domainItem_value.input)
      if (length(domainItem_value.input) != 1) {
        message(cat(
          paste0("\n'domainItem_value' must be a string and will be ignored.")
        ))
        domainItem_value.input <- ""

      } else {
        if (!domainItem_value.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "value")) {
          message(cat(paste0("\nDomain item not found.")))
          domainItem_value.input <- ""
        }
      }

      if (domainItem_value.input == "") {
        domainItem.opt <-
          sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "value")
        domainItem.choice <- 0

        while (domainItem.choice == 0) {
          domainItem.choice <-
            utils::menu(c(domainItem.opt), title =
                          cat(
                            paste0("\nREQUIRED: Select a domain item value to modify.\n")
                          ))

        }
        domainItem_value.input <- domainItem.opt[domainItem.choice]
        definedparam <- c(definedparam, "domainItem_value")
      }


      if ("domainItem" %in% definedparam) {
        message(cat(
          paste0(
            "'domainItem' is not a valid input for this modification and will be ignored.\nUse 'domainItem_value' instead."
          )
        ))
      }

    }


    ## Check/auto-fill inputs
    # Is domainItem a vector of three variables
    if (how %in% c("add_attribute", "add_domain", "add_domainItem")) {
      if (is.null(domainItem.input) == FALSE) {
        if (length(domainItem.input) != 0) {
          if (length(domainItem.input) == 3 &
              !TRUE %in% sapply(domainItem.input, function(x)
                is.list(x))) {
            # domainItem.input = c("", "", "")
            if (length(names(domainItem.input)) == 0) {
              domainItem.input <- list(list(
                domainItem.input[1],
                domainItem.input[2],
                domainItem.input[3]
              ))

              # domainItem.input = c(value = "", names = "", definition = "")
            } else {
              domainItem.input <-
                list(lapply(split(
                  domainItem.input, names(domainItem.input)
                ), unname))
            }

            # Is domainItem is a nested list with lengths of three; domainItem.input = list(list("", "", ""), list("", "", ""))
          } else if (FALSE %in% sapply(domainItem.input, function(x)
            is.list(x)) |
            FALSE %in% sapply(domainItem.input, function(x)
              length(x) == 3)) {
            message(cat(
              paste0(
                "'domainItem' is incorrectly formatted and will be ignored."
              )
            ))

            domainItem.input <- list()
            definedparam <-
              definedparam[!definedparam == "domainItem"]
          }
        }
      }

      # Are domainItem_value, domainItem_name, and domainItem_definition same length vectors or lists
      # domainItem_value.input = "a" OR domainItem_value.input = c("a", "b", "c") OR domainItem_value.input = c(value = "a", "b", "c") OR domainItem_value.input = list(value = "a", "b", "c")
      if (is.null(domainItem_value.input) == FALSE &
          is.null(domainItem_name.input) == FALSE &
          is.null(domainItem_definition.input) == FALSE &
          is.null(domainItem.input) == FALSE) {
        if (domainItem_value.input != "" |
            domainItem_name.input != "" |
            domainItem_definition.input != "") {
          if (length(domainItem.input) != 0) {
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

          } else {
            # Translate parameters to domainItem
            if (length(domainItem_value.input) == length(domainItem_name.input) &
                length(domainItem_name.input) == length(domainItem_definition.input)) {
              domainItem.input <-
                lapply(1:length(domainItem_name.input), function(x)
                  list(
                    name = domainItem_name.input[[x]],
                    value = domainItem_value.input[[x]],
                    definition = domainItem_definition.input[[x]]
                  ))
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
          }
        }
      }

      # Are domainItem elements named
      if (is.null(domainItem.input) == FALSE) {
        if (length(domainItem.input) != 0) {
          names.ref <- c("name", "value", "definition")

          # Does a domain item contain a misnamed element
          if (FALSE %in% sapply(domainItem.input, function(x)
            rlang::has_name(x, names.ref))) {
            # Which domainItems have a misnamed element
            domainItem.num <-
              which(sapply(
                lapply(domainItem.input, function(x)
                  rlang::has_name(x, names.ref)),
                FUN = function(X)
                  FALSE %in% X
              ))


            for (a in 1:length(domainItem.num)) {
              domainItem.names <- names(domainItem.input[[a]])

              # Are elements named
              if (length(domainItem.names) == 0) {
                domainItem.input[[a]] <-
                  setNames(domainItem.input[[a]], names.ref)
              } else {
                # What is the missing name
                missing.name <-
                  names.ref[!names.ref %in% domainItem.names]

                # Where is the incorrect name
                incorrect.num <-
                  which(!domainItem.names %in% names.ref)

                # Is just one element is misnamed
                if (length(missing.name) == 1 &
                    length(incorrect.num) == 1 &
                    missing.name != "") {
                  domainItem.names[incorrect.num] <- missing.name

                  domainItem.input[[a]] <-
                    setNames(domainItem.input[[a]], domainItem.names)
                } else {
                  message(cat(
                    paste0(
                      "'domainItem' is incorrectly formatted and will be ignored."
                    )
                  ))

                  domainItem.input <- list()
                  definedparam <-
                    definedparam[!definedparam == "domainItem"]

                }
              }
            }
          }
        }

        # Do domainItems conflict with each other
        if (length(domainItem.input) != 0) {

          domainItem.name.dup <-
            which(duplicated(sapply(domainItem.input, "[", "name")))

          if (length(domainItem.name.dup != 0)) {
            for (a in 1:length(domainItem.name.dup)) {
              domainItem.dup <-
                which(
                  sapply(domainItem.input, "[", "name") %in% sapply(domainItem.input, "[", "name")[domainItem.name.dup[a]]
                )

              domainItem.opt <-
                c(domainItem.input[domainItem.dup], "all", "none")

              dup.choice <-
                utils::menu(c(domainItem.opt), title =
                              cat(
                                paste0(
                                  "\nMultiple domain items are named '",
                                  sapply(domainItem.input, "[", "name")[domainItem.name.dup[a]],
                                  "'.\n\nWhich should be retained?"
                                )
                              ))

              if (domainItem.opt[dup.choice] == "none") {
                domainItem.input <- domainItem.input[-c(domainItem.dup)]
              } else if (domainItem.opt[dup.choice] != "all") {
                domainItem.input <- domainItem.input[-domainItem.dup[-dup.choice]]

              }
            }

            domainItem.value.dup <-
              which(duplicated(sapply(domainItem.input, "[", "value")))

            for (a in 1:length(domainItem.value.dup)) {
              domainItem.dup <-
                which(
                  sapply(domainItem.input, "[", "value") %in% sapply(domainItem.input, "[", "value")[domainItem.value.dup[a]]
                )

              domainItem.opt <-
                c(domainItem.input[domainItem.dup], "all", "none")

              dup.choice <-
                utils::menu(c(domainItem.opt), title =
                              cat(
                                paste0(
                                  "\nMultiple domain items have the value '",
                                  sapply(domainItem.input, "[", "value")[domainItem.value.dup[a]],
                                  "'.\n\nWhich should be retained?"
                                )
                              ))

              if (domainItem.opt[dup.choice] == "none") {
                domainItem.input <- domainItem.input[-c(domainItem.dup)]
              } else if (domainItem.opt[dup.choice] != "all") {
                domainItem.input <- domainItem.input[-domainItem.dup[-dup.choice]]

              }
            }
          }
        }

        # Are domainItem elements complete
        if (quiet == FALSE) {
          if (length(domainItem.input) != 0) {
            domainItem.omit <- c()

            for (a in 1:length(domainItem.input)) {
              if (TRUE %in% sapply(domainItem.input[[a]], function(x)
                is.null(x)) |
                TRUE %in% sapply(domainItem.input[[a]], function(x)
                  is.na(x)) |
                TRUE %in% sapply(domainItem.input[[a]], function(x)
                  x == "")) {
                domainItem.choice <-
                  utils::menu(
                    c("omit the domain item", "fill out missing fields", "keep as is"),
                    title =
                      cat(
                        paste0(
                          "\nThe following domain item appears incomplete.\n"
                        ),
                        paste0(
                          jsonlite::toJSON(
                            domainItem.input[[a]],
                            pretty = TRUE,
                            force = TRUE
                          ),
                          sep = "\n"
                        ),
                        paste0("\nWhat would you like to do?")
                      )
                  )
              }
              if (domainItem.choice == 1) {
                domainItem.omit <- c(domainItem.omit, a)
              } else if (domainItem.choice == 2) {
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
                    noquote(as.character(readline(prompt =)))
                }
                domainItem.input[[a]][["name"]] <-
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
                    as.character(readline(prompt = ))
                }
                domainItem.input[[a]][["definition"]] <-
                  domainItem_definition.input
              }
            }
            if (length(domainItem.omit) != 0) {
              domainItem.input <- domainItem.input[-domainItem.omit]
            }

          }
        }
      }
    }


    #### Add an attribute (and domain - optional) ####
    if (how == "add_attribute") {
      newattribute <- blankattribute

      newattribute[[1]][["codeName"]] <- attribute_codeName.input

      ## attribute allowNull
      if (attribute_allowNull.input == "") {
        allowNull.choice <- 0

        while (allowNull.choice == 0) {
          allowNull.choice <-
            utils::menu(c(TRUE, FALSE), title =
                          cat(
                            paste0(
                              "\nREQUIRED: Are null values permitted for the attribute '",
                              attribute_codeName.input,
                              "'?\n"
                            )
                          ))
        }
        attribute_allowNull.input <-
          c(TRUE, FALSE)[allowNull.choice]
      }
      newattribute[[1]][["allowNull"]] <-
        attribute_allowNull.input


      ## attribute dataType
      if (attribute_dataType.input == "") {
        dataType.choice <- 0

        while (dataType.choice == 0) {
          dataType.choice <-
            utils::menu(c(dataType.vector), title = cat(
              paste0(
                "\nREQUIRED: Select the datatype/format for entry values of the attribute '",
                attribute_codeName.input,
                "'.\n"
              )
            ))
        }
        attribute_dataType.input <-
          dataType.vector[dataType.choice]
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
            noquote(as.character(readline(prompt = )))
        }
      }
      newattribute[[1]][["definition"]] <-
        attribute_definition.input


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
          attribute_units.input <-
            as.character(readline(prompt =))
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
            noquote(as.character(readline(prompt =)))
        }

        if (attribute_unitsResolution.input != "") {
          while (suppressWarnings(is.na(as.numeric(
            attribute_unitsResolution.input
          ))) == TRUE) {
            message(cat(
              paste0(
                "\nInvalid entry: unit resolution must be numeric.\nPress Enter to omit this information."
              )
            ))
            attribute_unitsResolution.input <-
              as.character(readline(prompt = ))

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
            utils::menu(c(TRUE, FALSE), title =
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
            attribute_isCaseSensitive.input <-
              c(TRUE, FALSE)[isCaseSensitive.choice]
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
              noquote(as.character(readline(prompt =)))
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
            noquote(as.character(readline(prompt =)))
        }
        newattribute[[1]][["minValue"]] <-
          attribute_minValue.input
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
          attribute_maxValue.input <-
            as.character(readline(prompt = ))
        }
        newattribute[[1]][["maxValue"]] <-
          attribute_maxValue.input
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
            noquote(as.character(readline(prompt =)))
        }

        while (!grepl(
          "[^[:digit:]]",
          format(
            attribute_fieldWidth.input,
            digits = 20,
            scientific = FALSE
          )
        ) == FALSE) {
          message(cat(
            paste0(
              "\nThe field width must be an integer.\nPress Enter to omit this information."
            )
          ))
          attribute_fieldWidth.input <-
            noquote(as.character(readline(prompt =)))

        }

        newattribute[[1]][["fieldWidth"]] <-
          attribute_fieldWidth.input

      }


      # Add attribute to dictionary list without domain
      if (is.null(domain_domainId.input) == TRUE |
          is.null(domain_codeName.input) == TRUE) {
        if (quiet == FALSE) {
          update.choice <-
            utils::menu(c("disregard changes", "looks good"),
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
        ## Add an associated domain
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
            utils::menu(
              c(
                "yes - create a new domain",
                "yes - link to an existing domain",
                "no"
              ),
              title = cat(
                paste0(
                  "\nREQUIRED: Does the attribute '",
                  attribute_codeName.input,
                  "' have a domain (defined entry values)?\n"
                )
              )
            )

        }

        if (domain.choice == 2) {
          domain.opt <-
            sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName")

          domain.selection <-
            utils::menu(c(domain.opt), title =
                          cat(
                            paste0("\nSelect a domain.\nEnter '0' to omit this information.")
                          ))

          if (domain.selection == 0) {
            domain.choice <- 3
          } else {
            newattribute[[1]][["domainId"]] <-
              dictionarylist[["dataDictionary"]][["domain"]][[domain.choice]][["domainId"]]

          }
        } else if (domain.choice == 1) {
          ## attribute domainId
          newattribute[[1]][["domainId"]] <-
            uuid::UUIDgenerate(use.time = FALSE, n = 1)

          ## Create a domain
          newdomain <- blankdomain

          newdomain[[1]][["domainId"]] <-
            newattribute[[1]][["domainId"]]

          ## Auto-fill domain based on attribute
          domainfill.choice <- 0

          while (domainfill.choice == 0) {
            domainfill.choice <-
              utils::menu(c("yes (recommended)", "no"), title = cat(
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
                message(cat(
                  paste0("\nREQUIRED: Provide a name for the domain.")
                ))
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
          if (length(domainItem.input) != 0) {
            newdomain[[1]][["domainItem"]] <- domainItem.input
          } else if (is.null(domainItem.input) == TRUE |
                     is.null(domainItem_value.input) == TRUE |
                     is.null(domainItem_name.input) == TRUE |
                     is.null(domainItem_definition.input) == TRUE) {
            domainItems.choice <- 2
          } else {
            domainItems.choice <- 0
          }

          while (domainItems.choice == 0) {
            domainItems.choice <-
              utils::menu(c("yes", "no"), title = cat(
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
                  paste0(
                    "\nREQUIRED: Provide a name for the new domain item."
                  )
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
                utils::menu(c("yes", "no"), title = cat(
                  paste0(
                    "\nREQUIRED: Would you like to add an additional item to the domain?"
                  )
                ))


            }

          }
          if (quiet == FALSE) {
            update.choice <-
              utils::menu(c("disregard changes", "looks good"),
                          title = cat(
                            paste0(
                              "\nThe domain '",
                              newdomain[[1]][["codeName"]],
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

        if (quiet == FALSE) {
          update.choice <-
            utils::menu(c("disregard changes", "looks good"),
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
    }

    #### Add domain to existing attribute ####
    if (how == "add_domain") {
      ## Find attribute number
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                attribute_codeName.input)

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
          utils::menu(c("yes", "no"), title =
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
              utils::menu(c("associate the existing domain", "create a new domain"),
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
            utils::menu(c("yes", "no"), title = cat(
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
          ## domain codeName
          if (domain_codeName.input == "") {
            while (domain_codeName.input == "") {
              message(cat(paste0(
                "\nREQUIRED: Provide a name for the domain."
              )))
              domain_codeName.input <-
                noquote(as.character(readline(prompt =)))
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
                noquote(as.character(readline(prompt =)))
            }
          }
          newdomain[[1]][["description"]] <-
            domain_description.input

        }

        ## Add domainItems
        if (length(domainItem.input) != 0) {
          newdomain[[1]][["domainItem"]] <- domainItem.input
        } else if (is.null(domainItem.input) == TRUE |
                   is.null(domainItem_value.input) == TRUE |
                   is.null(domainItem_name.input) == TRUE |
                   is.null(domainItem_definition.input) == TRUE) {
          domainItems.choice <- 2
        } else {
          domainItems.choice <- 0
        }

        while (domainItems.choice == 0) {
          domainItems.choice <-
            utils::menu(c("yes", "no"), title = cat(
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
                  "\nREQUIRED: Provide an entry value for the domain item '",
                  domainItem_name.input,
                  "'."
                )
              ))
              domainItem_value.input <-
                noquote(as.character(readline(prompt = )))
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
              utils::menu(c("yes", "no"), title = cat(
                paste0(
                  "\nREQUIRED: Would you like to add an additional item to the domain?"
                )
              ))


          }
        }

        ## Add domain to dictionary list
        if (quiet == FALSE) {
          update.choice <-
            utils::menu(c("disregard changes", "looks good"),
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
          dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][["domainId"]] <-
            newdomainId

          dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) +
                                                            1]] <-
            newdomain[[1]]
        }

      }
    }

    #### Add domainItem ####
    if (how == "add_domainItem") {
      ## Find domain number
      domain.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                domain_codeName.input)

      if (length(domainItem.input) == 0) {
        newdomainItems <- blankdomainItem
        domainItem.num <- 1

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


        domainItem.input <- newdomainItems

      }

      ## Check input
      domainItem.value.num <-
        which(
          sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "value") %in% sapply(domainItem.input, "[", "value")
        )

      if (length(domainItem.value.num != 0)) {
        for (a in 1:length(domainItem.value.num)) {
          domainItem.value.dup <-
            which(sapply(domainItem.input, "[", "value") == dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[domainItem.value.num]][["value"]])

          message(cat(
            paste0(
              "The domain '",
              domain_codeName.input,
              "' already contains an item with the value '",
              dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[domainItem.value.num]][["value"]],
              "'.\n\nThe following will be ignored.\n"
            ),
            paste0(
              jsonlite::toJSON(
                domainItem.input[domainItem.value.dup],
                pretty = TRUE,
                force = TRUE
              ),
              sep = "\n"
            )
          ))
          domainItem.input <-
            domainItem.input[-domainItem.value.dup]
        }
      }

      domainItem.name.num <-
        which(
          sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "name") %in% sapply(domainItem.input, "[", "name")
        )

      if (length(domainItem.name.num != 0)) {
        for (a in 1:length(domainItem.name.num)) {
          domainItem.name.dup <-
            which(sapply(domainItem.input, "[", "name") == dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[domainItem.name.num]][["name"]])

          message(cat(
            paste0(
              "The domain '",
              domain_codeName.input,
              "' already contains an item named '",
              dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[domainItem.name.num]][["name"]],
              "'.\n\nThe following will be ignored.\n"
            ),
            paste0(
              jsonlite::toJSON(
                domainItem.input[domainItem.name.dup],
                pretty = TRUE,
                force = TRUE
              ),
              sep = "\n"
            )
          ))

          domainItem.input <-
            domainItem.input[-domainItem.name.dup]

        }
      }

      if (length(domainItem.input) == 0) {
        message(cat(
          paste0(
            "Operation canceled. The domain '",
            domain_codeName.input,
            "' already contains the domain item(s) provided.\nUpdate or remove existing domain items before proceeding."
          )
        ))

      } else {
        ## Add domainItem to the domain in the dictionary list
        if (quiet == FALSE) {
          update.choice <-
            utils::menu(c("disregard changes", "looks good"),
                        title = cat(
                          paste0(
                            "\nThe domain item value '",
                            domainItem_value.input,
                            "' is described as followed.\n"
                          ),
                          paste0(
                            jsonlite::toJSON(domainItem.input, pretty = TRUE, force = TRUE),
                            sep = "\n"
                          ),
                          paste0("\nDoes this look correct?")
                        ))
        } else {
          update.choice <- 2
        }
        if (update.choice == 2) {
          for (a in 1:length(domainItem.input)) {
            dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[length(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]]) +
                                                                                            1]] <-
              domainItem.input[[a]]
          }
        }
      }
    }

    #### Update attribute ####
    if (how == "update_attribute") {
      ## Find attribute number
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                attribute_codeName.input)

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

      if (which(attributeparam %in% definedparam) != 0) {
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
                  which(
                    sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                      attribute_codeName.input
                  )
                if (length(domain.num) == 1) {
                  domain.choice <-
                    utils::menu(c("please do", "most certaintly not"),
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
      } else {
        if (quiet == FALSE) {
          update.choice <-
            utils::menu(c("keep as is", "make changes"),
                        title = cat(
                          paste0(
                            "\nThe attribute '",
                            attribute_codeName.input,
                            "' is described as followed.\n"
                          ),
                          paste0(
                            jsonlite::toJSON(
                              originalattribute,
                              pretty = TRUE,
                              force = TRUE
                            ),
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
          utils::menu(attributefield, title =
                        cat(
                          paste0("\nWhich attribute field would you like to add/modify?\n")
                        ))


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
            noquote(as.character(readline(prompt =)))


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
                  which(
                    sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                      attribute_codeName.input
                  )
                if (length(domain.num) == 1) {
                  domain.choice <-
                    utils::menu(c("please do", "most certaintly not"),
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
            utils::menu(c(TRUE, FALSE), title =
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
            utils::menu(c(dataType.vector), title = cat(
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
          attribute_definition.input <- as.character(readline(prompt =
          ))


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
            noquote(as.character(readline(prompt =)))

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
            noquote(as.character(readline(prompt =)))

          if (attribute_unitsResolution.input == "rm()") {
            newattribute[["unitsResolution"]] <-
              ""
          } else if (suppressWarnings(is.na(as.numeric(
            attribute_unitsResolution.input
          ))) == TRUE) {
            message(cat(
              paste0(
                "\nInvalid entry: unit resolution must be numeric.\nThe current resolution will be retained."
              )
            ))
          } else {
            newattribute[["unitsResolution"]] <-
              attribute_unitsResolution.input
          }


          ## attribute isCaseSensitive
        } else if (attributefield[field.choice] == "isCaseSensitive") {
          isCaseSensitive.choice <- 0
          originalvalue <- newattribute[["isCaseSensitive"]]

          isCaseSensitive.choice <-
            utils::menu(c(TRUE, FALSE), title =
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
              utils::menu(c("yes", "no"), title =
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
              noquote(as.character(readline(prompt =)))

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
            noquote(as.character(readline(prompt =)))

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
            noquote(as.character(readline(prompt =)))


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
            noquote(as.character(readline(prompt =)))

          if (attribute_fieldWidth.input == "rm()") {
            newattribute[["fieldWidth"]] <- ""
          } else if (attribute_fieldWidth.input != "") {
            if (!grepl(
              "[^[:digit:]]",
              format(
                attribute_fieldWidth.input,
                digits = 20,
                scientific = FALSE
              )
            ) == FALSE) {
              message(cat(
                paste0(
                  "\nInvalid entry: field width must be an integer.\nThe current field width will be retained."
                )
              ))
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
            noquote(as.character(readline(prompt = )))

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
      ## Find domain number
      domain.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                domain_codeName.input)

      originaldomain <-
        dictionarylist[["dataDictionary"]][["domain"]][[domain.num]]

      newdomain <- originaldomain


      ## Make changes based on inputs
      domainparam <- c("new.domain_codeName", "domain_description")

      if (which(domainparam %in% definedparam) != 0) {
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
                  utils::menu(c("please do", "most certaintly not"),
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
          utils::menu(domainfield, title =
                        cat(
                          paste0("\nWhich domain field would you like to add/modify?\n")
                        ))


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
            noquote(as.character(readline(prompt =)))


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
                    utils::menu(c("please do", "most certaintly not"),
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
            noquote(as.character(readline(prompt = )))

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


    #### Update domainItem ####
    if (how == "update_domainItem") {
      ## Find domain number
      domain.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                domain_codeName.input)

      ## Find domainItem number
      domainItem.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "value") ==
                domainItem.input[[1]][["value"]])

      originaldomainItem <-
        dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[domainItem.num]]

      newdomainItem <- originaldomainItem


      ## Make changes based on inputs
      domainItemparam <-
        c("new.domainItem_value",
          "domainItem_name",
          "domainItem_definition")

      if (which(domainItemparam %in% definedparam) != 0) {
        domainItem.inputs <- which(domainItemparam %in% definedparam)

        if (1 %in% domainItem.inputs) {
          new.domainItem_value.input <- unlist(new.domainItem_value.input)
          if (length(new.domainItem_value.input) != 1) {
            message(cat(
              paste0(
                "\n'new.domainItem_value' must be a string and will be ignored."
              )
            ))
            new.domainItem_value.input <- ""
            definedparam <-
              definedparam[!definedparam == "new.domainItem_value"]
          } else {
            if (new.domainItem_value.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "value")) {
              message(cat(
                paste0(
                  "\nA domain item with the value '",
                  new.domainItem_value.input,
                  "' already exists. \nThe current value will be retained."
                )
              ))
            } else {
              newdomainItem[["value"]] <- new.domainItem_value.input
            }
          }
        }
        if (2 %in% domainItem.inputs) {
          domainItem_name.input <- unlist(domainItem_name.input)
          if (length(domainItem_name.input) != 1) {
            message(cat(
              paste0(
                "\n'domainItem_name' must be a string and will be ignored."
              )
            ))
            domainItem_name.input <- ""
            definedparam <-
              definedparam[!definedparam == "domainItem_name"]
          } else {
            if (domainItem_name.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "name")) {
              message(cat(
                paste0(
                  "\nA domain item with the name '",
                  domainItem_name.input,
                  "' already exists. \nThe current name will be retained."
                )
              ))
            } else {
              newdomainItem[["name"]] <- domainItem_name.input
            }
          }
        }
        if (3 %in% domainItem.inputs) {
          domainItem_definition.input <- unlist(domainItem_definition.input)
          if (length(domainItem_definition.input) != 1) {
            message(cat(
              paste0(
                "\n'domainItem_definition' must be a string and will be ignored."
              )
            ))
            domainItem_definition.input <- ""
            definedparam <-
              definedparam[!definedparam == "domainItem_definition"]
          } else {
            newdomainItem[["definition"]] <- domainItem_definition.input
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
                  "\nThe domain item '",
                  domain_codeName.input,
                  "' has been updated as followed.\n"
                ),
                paste0(
                  jsonlite::toJSON(newdomainItem, pretty = TRUE, force = TRUE),
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
      domainItemfield <- c("value", "name", "definition")

      while (update.choice == 2) {
        field.choice <-
          utils::menu(domainItemfield, title =
                        cat(
                          paste0("\nWhich domain item field would you like to add/modify?\n")
                        ))


        # Create field if it doesn't exist
        if (domainItemfield[field.choice] %in% names(newdomainItem) == FALSE) {
          newfield <- list()
          newfield[[paste0(domainItemfield[field.choice])]] <- ""
          newdomainItem <- append(newdomainItem, newfield)
        }

        # domainItem value
        if (domainItemfield[field.choice] == "value") {
          originalvalue <- newdomainItem[["value"]]

          message(cat(
            paste0(
              "\nProvide a domain item value. \nThe current value is '",
              originalvalue,
              "'.\nPress Enter to abort changes."
            )
          ))

          new.domainItem_value.input <-
            noquote(as.character(readline(prompt =)))

          if (new.domainItem_value.input != "") {
            if (new.domainItem_value.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "value")) {
              message(cat(
                paste0(
                  "\nA domain item with the value '",
                  new.domainItem_value.input,
                  "' already exists. \nThe current value will be retained."
                )
              ))
            } else {
              newdomainItem[["value"]] <- new.domainItem_value.input
            }
          }

          # domainItem name
        } else if (domainItemfield[field.choice] == "name") {
          originalvalue <- newdomainItem[["name"]]

          message(cat(
            paste0(
              "\nProvide a name for the domain item value '",
              newdomainItem[["value"]],
              "'\nThe current name is '",
              originalvalue,
              "'.\nPress Enter to abort changes."
            )
          ))
          domainItem_name.input <-
            noquote(as.character(readline(prompt =)))

          if (domainItem_name.input != "") {
            if (domainItem_name.input %in% sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "name")) {
              message(cat(
                paste0(
                  "\nA domain item with the name '",
                  domainItem_name.input,
                  "' already exists. \nThe current value will be retained."
                )
              ))
            } else {
              newdomainItem[["name"]] <- domainItem_name.input
            }
          }
          # domainItem definition
        } else if (domainItemfield[field.choice] == "definition") {
          originalvalue <- newdomainItem[["definition"]]

          message(cat(
            paste0(
              "\nProvide a definition for the domain item value '",
              newdomainItem[["value"]],
              "'\nThe current definition is '",
              originalvalue,
              "'.\nPress Enter to abort changes."
            )
          ))
          domainItem_definition.input <-
            noquote(as.character(readline(prompt =)))

          if (domainItem_definition.input != "") {
            newdomainItem[["definition"]] <- domainItem_definition.input
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
        dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[domainItem.num]] <-
          newdomainItem

      }
    }


    #### Remove attribute ####
    if (how == "remove_attribute") {
      ## Find attribute number
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "codeName") ==
                attribute_codeName.input)

      originalattribute <-
        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]]

      if (quiet == FALSE) {
        attribute.choice <-
          utils::menu(c("definitely", "absolutely not"),
                      title =
                        cat(
                          paste0(
                            "\nAre you sure you want to delete the following attribute?\n"
                          ),
                          paste0(
                            jsonlite::toJSON(originalattribute, pretty = TRUE, force = TRUE),
                            sep = "\n"
                          )
                        ))
      } else {
        attribute.choice <- 1
      }
      if (attribute.choice == 1) {
        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]] <-
          dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][-attribute.num]

      }
    }

    #### Remove domain ####
    if (how == "remove_domain") {
      ## Find domain number
      domain.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                domain_codeName.input)

      ## Find attribute number
      attribute.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]], "[", "domainId") ==
                domain_domainId.input)

      originaldomain <-
        dictionarylist[["dataDictionary"]][["domain"]][[domain.num]]

      if (quiet == FALSE) {
        domain.choice <-
          utils::menu(c("definitely", "absolutely not"),
                      title =
                        cat(
                          paste0("\nAre you sure you want to delete the following domain?\n"),
                          paste0(
                            jsonlite::toJSON(originaldomain, pretty = TRUE, force = TRUE),
                            sep = "\n"
                          )
                        ))
      } else {
        domain.choice <- 1
      }
      if (domain.choice == 1) {
        dictionarylist[["dataDictionary"]][["domain"]] <-
          dictionarylist[["dataDictionary"]][["domain"]][-domain.num]

        for (a in 1:length(attribute.num)) {
          dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]] <-
            dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]][names(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[attribute.num]]) %in% "domainId" == FALSE]
        }
      }
    }

    #### Remove domainItem ####
    if (how == "remove_domainItem") {
      ## Find domain number
      domain.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]], "[", "codeName") ==
                domain_codeName.input)

      ## Find domainItem number
      domainItem.num <-
        which(sapply(dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]], "[", "value") ==
                domainItem_value.input)

      originaldomainItem <-
        dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][[domainItem.num]]

      if (quiet == FALSE) {
        domainItem.choice <-
          utils::menu(c("definitely", "absolutely not"),
                      title =
                        cat(
                          paste0(
                            "\nAre you sure you want to delete the following item from the domain '",
                            domain_codeName.input,
                            "'?\n"
                          ),
                          paste0(
                            jsonlite::toJSON(
                              originaldomainItem,
                              pretty = TRUE,
                              force = TRUE
                            ),
                            sep = "\n"
                          )
                        ))
      } else {
        domainItem.choice <- 1
      }
      if (domainItem.choice == 1) {
        dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]] <-
          dictionarylist[["dataDictionary"]][["domain"]][[domain.num]][["domainItem"]][-domainItem.num]
      }

    }

    #### Reattach dictionary list ####
    lastUpdate <- strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M")
    lastUpdate <- paste0(lastUpdate, ":00.000Z", collapse = "")

    lastUpdate.num <- c()

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
