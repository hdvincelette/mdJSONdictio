#' Build Tabular Dictionaries
#'
#'
#' @param x An R list
#' @return data frame
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @export
#' @examples
#'

library(mdJSONdictio)
library(rjson)
library(qdap)


# fix order
# replace true/false with yes/no

setwd("~/GitHub/mdJSONdictio/inst/extdata")
e.g.json <- fromJSON(readLines("e.g.dictionary_complete.json"))

newtable <- build.table(e.g.json)


build.table <- function(x) {
  JSONdictionary <- e.g.json

  dictionarystring <-
    JSONdictionary[["data"]][[1]][["attributes"]][["json"]]


  #### Entity ####

  # \"entity\":[{....}],\"domain\":[{....}],\"responsibleParty\":{

  term1 <- paste0("\"", "entity", "\":[")
  term2 <- paste0("],\"", "domain", "\":")
  term3 <- paste0("attribute", "\":[")
  term4 <- paste0("],\"", "definition", "\":")

  # str_extract_all(dictionarystring, paste0('(?<=',term1,').+(?=',term2,')'))
  # str_extract(dictionarystring, paste0('(?<=',term1,')(.+)(?=',term2,')'))

  # Extract entity string
  entitystring <- genXtract(dictionarystring, term1, term2)

  entitystring2 <- genXtract(entitystring, term3, term4)

  # Create a vector of entity elements
  entityelements <-
    unlist(strsplit(entitystring2 , "(?<=\\},)(?=\\{)"  , perl = TRUE),
           use.names = FALSE)


  # Remove brackets and from vector
  for (a in 1:length(entityelements)) {
    if (grepl("},", entityelements[a]) == TRUE) {
      entityelements[a] <- sub("},", "}", entityelements[a])
    }

    entityelements[a] <- gsub("\\{|}", "", entityelements[a])
  }



  # Create blank table

  blanktable <- setNames(
    data.frame(matrix(ncol = 14, nrow = 0)),
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
      "minValue",
      "maxValue",
      "isCaseSensitive",
      "notes"
    )
  )

  # Extract separate elements from entity string and add each sub element to blank table

  for (b in 1:length(entityelements)) {
    element <- entityelements[b]
    subelements <-
      unlist(strsplit(element, ",\"", perl = TRUE), use.names = FALSE)

    blanktable[nrow(blanktable) + 1,] <- NA


    for (c in 1:length(subelements)) {
      if (grepl("\"", subelements[c]) == TRUE) {
        subelements[c] <- gsub("\"", "", subelements[c])
      }

      column <- beg2char(subelements[c], ":")
      entry <- char2end(subelements[c], ":")

      blanktable[[paste0(column)]][b] <- entry
      blanktable$entityNum[b] <- b
      blanktable$domainNum[b] <- 0


    }
  }

  blanktable$name <- "colname"
  blanktable$value <- "colname"


  #### Domains ####

  # \"entity\":[{....}],\"domain\":[{....}],\"responsibleParty\":{

  term1 <- paste0("\"", "domain", "\":[")
  term2 <- paste0("],\"", "responsibleParty", "\":")

  # Extract domain string
  domainstring <- genXtract(dictionarystring, term1, term2)


  # Create a vector of domain elements
  term3 <- paste0("{\"", "domainId", "\":")
  term4 <- paste0(",\"", "domainItem", "\"")
  term5 <- paste0("\"", "domainItem", "\":[")
  term6 <- paste0("]")

  # Extract Item and Ref strings

  domainRefs <-
    genXtract(dictionarystring, term3, term4, with = TRUE)
  domainRefs <- as.vector(domainRefs)
  domainItems <- genXtract(dictionarystring, term5, term6)
  domainItems <- as.vector(domainItems)

  # Remove extra characters
  for (d in 1:length(domainRefs)) {
    domainRefs[d] <- sub(term4, "}", domainRefs[d])
  }



  # Create blank ref table

  reftable <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                       c("domainId",
                         "codeName",
                         "domainNum"))


  # Associate codeName with a domain domainNum
  for (h in 1:length(domainRefs)) {
    Ref <- domainRefs[h]

    for (k in 1:length(Ref)) {
      if (grepl("},", Ref[k]) == TRUE) {
        Ref[k] <- sub("},", "}", Ref[k])
      }

      Ref[k] <- gsub("\\{|}", "", Ref[k])
    }


    Ref2 <-
      unlist(strsplit(Ref[k], ",\"", perl = TRUE), use.names = FALSE)

    reftable[nrow(reftable) + 1,] <- NA

    for (m in 1:length(Ref2)) {
      if (grepl("\"", Ref2[m]) == TRUE) {
        Ref2[m] <- gsub("\"", "", Ref2[m])
      }

      column <- beg2char(Ref2[m], ":")
      entry <- char2end(Ref2[m], ":")

      reftable[[paste0(column)]][h] <- entry
      reftable$domainNum[h] <- h
    }
  }




  # Extract separate elements from domain string and add each sub element to blank table

  #
  # blanktable<-setNames(data.frame(matrix(ncol = 12, nrow = 0)),
  #                      c("codeName",
  #                        "name",
  #                        "value",
  #                        "definition",
  #                        "dataType",
  #                        "allowNull",
  #                        "units",
  #                        "unitsResolution",
  #                        "minValue",
  #                        "maxValue",
  #                        "isCaseSensitive",
  #                        "notes"))


  ncolrows <- nrow(blanktable)

  for (e in 1:length(domainItems)) {
    Item <- domainItems[e]

    Item2 <-
      unlist(strsplit(Item , "(?<=\\},)(?=\\{)"  , perl = TRUE), use.names = FALSE)

    for (f in 1:length(Item2)) {
      if (grepl("},", Item2[f]) == TRUE) {
        Item2[f] <- sub("},", "}", Item2[f])
      }

      Item2[f] <- gsub("\\{|}", "", Item2[f])

      SubItem <-
        unlist(strsplit(Item2[f], ",\"", perl = TRUE), use.names = FALSE)

      blanktable[nrow(blanktable) + 1,] <- NA

      for (g in 1:length(SubItem)) {
        SubItem[g] <- gsub("\"", "", SubItem[g])

        column <- beg2char(SubItem[g], ":")
        entry <- char2end(SubItem[g], ":")

        codeName <- reftable$codeName[reftable$domainNum == e]

        blanktable[[paste0(column)]][nrow(blanktable)] <- entry
        blanktable$codeName[nrow(blanktable)] <- codeName
        blanktable$domainNum[nrow(blanktable)] <- e

      }
    }
  }

  for (o in 1:nrow(blanktable)) {
    for (p in 1:nrow(reftable)) {
      if (blanktable$codeName[o] == reftable$codeName[p]) {
        blanktable$domainNum[o] = reftable$domainNum[p]
      }
    }
  }

  blanktable <- blanktable %>%
    arrange(domainNum, entityNum) %>%
    select(-one_of(c("domainNum", "entityNum"))) %>%
    rename("domainItem_name" = "name",
           "domainItem_value" = "value")

  assign("newtable", blanktable)

}
