#' Build Tabular Data Dictionaries
#'
#' Transforms an mdEditor mdJSON data dictionary (imported as an R list) into a data frame.
#' @param x An R list converted from an mdJSON data dictionary file.
#' @param dictionary_num Default=1. An integer indicating the dictionary if there is more than one in the R list (i.e. if multiple dictionaries are exported together in mdEditor).
#' @param entity_num Default=1. An integer indicating the entity if there is more than one in the R list.
#' @return Returns a data frame corresponding to the mdJSON data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```build.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as an R list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' e.g.dictionary2 <- fromJSON(file = path)
#'
#' # Transform R list to a data frame
#' newtable<- build.table(x = e.g.dictionary2, dictionary_num = 1, entity_num = 1)
#'
#' # Export JSON to disk
#' write.csv(newtable, "e.g.dictionary2.csv",na="",row.names = FALSE)



build.table <- function(x, dictionary_num, entity_num) {
  JSONdictionary <- x

  n <- 1
  a <- 1


  if (is.character(dictionary_num) == TRUE)
    stop (
      'dictionary_num only accepts an integer.\n  Print `help(package = "mdJSONdictio") ` for Help Pages.'
    )
  if (is.character(entity_num) == TRUE)
    stop (
      'entity_num only accepts an integer.\n  Print `help(package = "mdJSONdictio") ` for Help Pages.'
    )


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
    stop('No Entity detected.\n  Print `help(package = "mdJSONdictio") ` for Help Pages.')

  if (grepl("attribute", dictionarystring) == FALSE)
    stop(
      'Entity requires atleast one attribute.\n  Print `help(package = "mdJSONdictio") ` for Help Pages.'
    )

  if (grepl("dataType", dictionarystring) == FALSE)
    stop(
      'Entity requires atleast one attribute.\n  Print `help(package = "mdJSONdictio") ` for Help Pages.'
    )


  # Extract domain and entity string lists
  newlist = fromJSON(dictionarystring)

  entitylist <- newlist[["dataDictionary"]][["entity"]]
  domainlist <- newlist[["dataDictionary"]][["domain"]]


  # Create blank output table

  blanktable <- setNames(
    data.frame(matrix(ncol = 10, nrow = 0)),
    c(
      "entityNum",
      "domainNum",
      "codeName",
      "name",
      "value",
      "definition",
      "dataType",
      "allowNull",
      "isCaseSensitive",
      "domainId"
    )
  )

  # Loop through domain and entity lists to fill out table

  # for (a in 1:length(entitylist)) {

    domaincount <- 0

    blanktable <- setNames(
      data.frame(matrix(ncol = 10, nrow = 0)),
      c(
        "entityNum",
        "domainNum",
        "codeName",
        "name",
        "value",
        "definition",
        "dataType",
        "allowNull",
        "isCaseSensitive",
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


    for (d in 1:length(domainlist)) {
      for (e in 1:length(domainlist[[d]])) {
        blanktable[nrow(blanktable) + 1,] <- NA
        blanktable$domainId[nrow(blanktable)] <-
          domainlist[[d]][["domainId"]]
        blanktable$codeName[nrow(blanktable)] <-
          domainlist[[d]][["codeName"]]

        blanktable$domainNum[nrow(blanktable)] <- e
        blanktable$entityNum[nrow(blanktable)] <- b

        if(is.null(domainlist[[d]][["domainItem"]])==TRUE){next}

        for (f in 1:length(domainlist[[d]][["domainItem"]])) {
          for (g in 1:length(domainlist[[d]][["domainItem"]][[f]])) {
            column <- names(domainlist[[d]][["domainItem"]][[f]])[[g]]
            entry <- domainlist[[d]][["domainItem"]][[f]][[g]]

            blanktable[[paste0(column)]][nrow(blanktable)] <- entry

          }
        }
      }
    }

    ## Fix table structure
    blanktable <- blanktable %>%
      arrange(codeName, domainNum) %>%
      select(-one_of(c("domainNum", "entityNum"))) %>%
      rename("domainItem_name" = "name",
             "domainItem_value" = "value") %>%
      mutate_at(vars(allowNull, isCaseSensitive),
                ~ replace(., which(. == "TRUE"), "yes")) %>%
      mutate_at(vars(allowNull, isCaseSensitive),
                ~ replace(., which(. == "FALSE"), "no"))

    assign(paste0("table", a), blanktable)
  }

# }

