#' Build Tabular Data Dictionaries
#'
#' Translates a list object converted from an mdEditor mdJSON data dictionary file into a data frame.
#' @param x List object converted from an mdJSON file.
#' @param y Data frame of a dataset.
#' @param entity_num Default=1. Integer indicating the entity if there is more than one in the mdJSON file.
#' @return Returns a data frame corresponding to the mdJSON data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```build.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' input.list <- rjson::fromJSON(file = path)
#'
#' # Translate list to data frame
#' new.table<- mdJSONdictio::build.table(x = input.list, dictionary_num = 1, entity_num = 1)
#'
#' # Export table to disk
#' write.csv(x = new.table, file = "e.g.dictionary2.csv", na="", row.names = FALSE)


build.table <- function(x, entity_num) {

  `%>%` <- magrittr::`%>%`

  if (length(x[["data"]]) > 1) {
    JSONdictionary <-
      extract.mdJSON(x = x,
                     record.type = "dictionaries",
                     multiple = FALSE)
  } else {
    JSONdictionary <- x
  }


  # Check validity of the R list

  if (JSONdictionary[["data"]][[1]][["type"]] != "dictionaries")
    stop('Dictionary not detected.')

  dictionarystring <-
    JSONdictionary[["data"]][[1]][["attributes"]][["json"]]

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


  # Make entity selection, if necessary
  if (length(newlist[["dataDictionary"]][["entity"]]) > 1) {
    entity.names <- c()
    for (a in 1:length(newlist[["dataDictionary"]][["entity"]])) {
      entity.names <-
        c(entity.names, newlist[["dataDictionary"]][["entity"]][[1]][["codeName"]])
    }

    entity.choice <- utils::select.list(
      c(entity.names),
      title = cat(
        paste0("The mdJSON data dictionary contains more than one entity")
      ),
      multiple = FALSE,
      graphics = TRUE
    )

    entity_num <- which(entity.choice == entity.names)

  } else {

    if (missing(entity_num))
      entity_num <- 1
  }



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

  for (b in 1:length(entitylist[[entity_num]][["attribute"]])) {
    blanktable[nrow(blanktable) + 1, ] <- NA

    for (c in 1:length(entitylist[[entity_num]][["attribute"]][[b]])) {
      column <- names(entitylist[[entity_num]][["attribute"]][[b]])[c]
      entry <- entitylist[[entity_num]][["attribute"]][[b]][[c]]

      if (length(entry) != 0) {
        blanktable[[paste0(column)]][b] <- entry
      }

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
  blanktable$name <- "dataField"
  blanktable$value <- "dataField"

  if (is.null(domainlist) == FALSE) {
    for (d in 1:length(domainlist)) {
      for (f in 1:length(domainlist[[d]][["domainItem"]])) {
        blanktable[nrow(blanktable) + 1,] <- NA
        blanktable$domainId[nrow(blanktable)] <-
          domainlist[[d]][["domainId"]]
        blanktable$codeName[nrow(blanktable)] <-
          domainlist[[d]][["codeName"]]

        blanktable$domainNum[nrow(blanktable)] <- d
        blanktable$entityNum[nrow(blanktable)] <- b

        if (is.null(domainlist[[d]][["domainItem"]]) == TRUE) {
          next
        }

        for (g in 1:length(domainlist[[d]][["domainItem"]][[f]])) {
          column <- names(domainlist[[d]][["domainItem"]][[f]])[[g]]
          entry <- domainlist[[d]][["domainItem"]][[f]][[g]]

          blanktable[[paste0(column)]][nrow(blanktable)] <-
            entry


        }
      }
    }
  }

  ## Fix table structure
  blanktable <- blanktable %>%
    dplyr::arrange(codeName, dataType, domainNum) %>%
    dplyr::select(-dplyr::one_of(c("domainNum", "entityNum"))) %>%
    dplyr::rename("domainItem_name" = "name",
                  "domainItem_value" = "value")

  assign(paste0("newtable", a), blanktable)



}


