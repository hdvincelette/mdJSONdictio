#' Build Tabular Data Dictionaries
#'
#' Transforms an mdEditor mdJSON data dictionary (imported as an R list) into a data frame.
#' @param x An R list converted from an mdJSON data dictionary file.
#' @param dictionary_num Default=1. An integer indicating the dictionary if there is more than one in the R list (i.e. if multiple dictionaries are exported together in mdEditor).
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
#' newtable<- build.table(x = e.g.dictionary2, dictionary_num = 1)
#'
#' # Export JSON to disk
#' write.csv(newtable, "e.g.dictionary2.csv",na="",row.names = FALSE)



build.table <- function(x, dictionary_num, entity_num) {
  JSONdictionary <- x

  n <- 1
  a <- 1

  ## Parameter arguments
  if (missing(dictionary_num))
    n <- 1
  else
    n <- dictionary_num


  if (missing(dictionary_num))
    a <- 1
  else
    n <- entity_num


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
#
#   indices <-
#     c(0, which(cumsum(sapply(unlist(strsplit(dictionarystring, split = '')),
#                              function(x)
#                                ifelse(x == '{', 1, ifelse(x == '}',-1, 0)))) == 0))
#   sapply(1:(length(indices) - 1), function(i)
#     substring(dictionarystring, indices[i] + 1, indices[i + 1]))

  newlist = fromJSON(dictionarystring)

  entitylist <- newlist[["dataDictionary"]][["entity"]]
  domainlist <- newlist[["dataDictionary"]][["domain"]]


  # Create blank output table

  blanktable <- setNames(
    data.frame(matrix(ncol = 9, nrow = 0)),
    c(
      "entityNum",
      "domainNum",
      "codeName",
      "name",
      "value",
      "definition",
      "dataType",
      "allowNull",
      "domainId"
    )
  )

  # Loop through domain and entity lists to fill out table

  # for (a in 1:length(entitylist)) {

    domaincount <- 0

    blanktable <- setNames(
      data.frame(matrix(ncol = 9, nrow = 0)),
      c(
        "entityNum",
        "domainNum",
        "codeName",
        "name",
        "value",
        "definition",
        "dataType",
        "allowNull",
        "domainId"
      )
    )

    for (b in 1:length(entitystring[[a]][["attribute"]])) {
      blanktable[nrow(blanktable) + 1,] <- NA

      for (c in 1:length(entitystring[[a]][["attribute"]][[b]])) {
        column <- names(entitystring[[a]][["attribute"]][[b]])[c]
        entry <- entitystring[[a]][["attribute"]][[b]][[c]]


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

        for (f in 1:length(domainlist[[d]][["domainItem"]])) {
          for (g in 1:length(domainlist[[d]][["domainItem"]][[f]])) {
            column <- names(domainlist[[d]][["domainItem"]][[f]])[[g]]
            entry <- domainlist[[d]][["domainItem"]][[f]][[g]]

            blanktable[[paste0(column)]][nrow(blanktable)] <- entry
            blanktable$domainNum[nrow(blanktable)] <- f
            blanktable$entityNum[nrow(blanktable)] <- 0
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

