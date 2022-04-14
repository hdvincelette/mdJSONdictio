#' Build Tabular Data Dictionaries
#'
#' Translates a list object converted from an mdEditor mdJSON data dictionary file into a data frame.
#' @param x A list object converted from an mdJSON data dictionary file.
#' @param dictionary_num Default=1. An integer indicating the dictionary if there is more than one in the mdJSON file (i.e. if multiple dictionaries are exported together in mdEditor).
#' @param entity_num Default=1. An integer indicating the entity if there is more than one in the mdJSON file.
#' @return Returns a data frame corresponding to the mdJSON data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```build.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' input.json <- rjson::fromJSON(file = path)
#'
#' # Translate list to data frame
#' new.table<- mdJSONdictio::build.table(x = input.json, dictionary_num = 1, entity_num = 1)
#'
#' # Export table to disk
#' write.csv(x = new.table, file = "e.g.dictionary2.csv", na="", row.names = FALSE)


build.table <- function(x, entity_num, dictionary_num) {

  `%>%` <- magrittr::`%>%`


  JSONdictionary <- x

  n <- 1
  a <- 1


  ## Parameter arguments
  if (missing(dictionary_num))
    n <- 1
  else
    n <- dictionary_num


  if (missing(entity_num))
    a <- 1
  else
    a <- entity_num


  # Check validity of the R list
  dictionarystring <-
    JSONdictionary[["data"]][[n]][["attributes"]][["json"]]

  if (grepl("entity", dictionarystring) == FALSE)
    stop('No Entity detected.\n  Print `help(package = "mdJSONdictio")` for Help Pages.')

  if (grepl("attribute", dictionarystring) == FALSE)
    stop(
      'Entity requires atleast one attribute.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
    )

  if (grepl("dataType", dictionarystring) == FALSE)
    stop(
      'Entity requires atleast one attribute.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
    )


  # Extract domain and entity string lists
  newlist = rjson::fromJSON(dictionarystring)

  entitylist <- newlist[["dataDictionary"]][["entity"]]
  domainlist <- newlist[["dataDictionary"]][["domain"]]


  # Create blank output table

  blanktable <- stats::setNames(
    data.frame(matrix(ncol = 16, nrow = 0)),
    c(
      "entityNum",
      "domainNum",
      "codeName",
      "name",
      "value",
      "definition",
      "dataType",
      "allowNull",
      "units",
      "unitsResolution",
      "isCaseSensitive",
      "fieldWidth",
      "missingValue",
      "minValue",
      "maxValue",
      "domainId"
    )
  )

  # Loop through domain and entity lists to fill out table

  # for (a in 1:length(entitylist)) {

    domaincount <- 0

    blanktable <- stats::setNames(
      data.frame(matrix(ncol = 16, nrow = 0)),
      c(
        "entityNum",
        "domainNum",
        "codeName",
        "name",
        "value",
        "definition",
        "dataType",
        "allowNull",
        "units",
        "unitsResolution",
        "isCaseSensitive",
        "fieldWidth",
        "missingValue",
        "minValue",
        "maxValue",
        "domainId"
      )
    )

    for (b in 1:length(entitylist[[a]][["attribute"]])) {
      blanktable[nrow(blanktable) + 1,] <- NA

      for (c in 1:length(entitylist[[a]][["attribute"]][[b]])) {
        column <- names(entitylist[[a]][["attribute"]][[b]])[c]
        entry <- entitylist[[a]][["attribute"]][[b]][[c]]


        blanktable[[paste0(column)]][b] <- entry

        if (is.na(blanktable$domainId[b]) == FALSE) {
          domaincount = domaincount + 1
          blanktable$domainNum[b] <-
            domaincount
        } else{
          blanktable$domainNum[b] = 0
        }

        blanktable$entityNum[b] <- b
      }
    }
    blanktable$name <- "colname"
    blanktable$value <- "colname"

    if (is.null(domainlist) == FALSE) {
      for (d in 1:length(domainlist)) {
        for (e in 1:length(domainlist[[d]])) {
          blanktable[nrow(blanktable) + 1, ] <- NA
          blanktable$domainId[nrow(blanktable)] <-
            domainlist[[d]][["domainId"]]
          blanktable$codeName[nrow(blanktable)] <-
            domainlist[[d]][["codeName"]]

          blanktable$domainNum[nrow(blanktable)] <- e
          blanktable$entityNum[nrow(blanktable)] <- b

          if (is.null(domainlist[[d]][["domainItem"]]) == TRUE) {
            next
          }

          for (f in 1:length(domainlist[[d]][["domainItem"]])) {
            for (g in 1:length(domainlist[[d]][["domainItem"]][[f]])) {
              column <- names(domainlist[[d]][["domainItem"]][[f]])[[g]]
              entry <- domainlist[[d]][["domainItem"]][[f]][[g]]

              blanktable[[paste0(column)]][nrow(blanktable)] <- entry

            }
          }
        }
      }
    }

    ## Fix table structure
    blanktable <- blanktable %>%
      dplyr::arrange(codeName, dataType, domainNum) %>%
      dplyr::select(-dplyr::one_of(c("domainNum", "entityNum"))) %>%
      dplyr::rename("domainItem_name" = "name",
             "domainItem_value" = "value") %>%
      dplyr::mutate_at(dplyr::vars(allowNull, isCaseSensitive),
                ~ replace(., which(. == "TRUE"), "yes")) %>%
      dplyr::mutate_at(dplyr::vars(allowNull, isCaseSensitive),
                ~ replace(., which(. == "FALSE"), "no"))

    assign(paste0("newtable", a), blanktable)



  }

# }



