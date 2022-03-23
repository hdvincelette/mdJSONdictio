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
library(tidyverse)
library(stats)


# test jsons

setwd("C:/Users/hvincelette/OneDrive - DOI/Data_management")
e.g.json <- fromJSON(readLines("e.g.dictionary.json"))

newtable <- build.table(e.g.json)

#SEOW_PTT_Dictionary.json
#e.g.dictionary_complete.json
#JBER_lowland.json

build.table <- function(x) {
  JSONdictionary <- e.g.json

  dictionarystring <-
    JSONdictionary[["data"]][[1]][["attributes"]][["json"]]

  #\"subject\":[\"dataDictionary\"],\"entity\":
  testterm1 <- paste0("dataDictionary", "\"],\"", "entity", "\"")

  # #\"dictionaryIncludedWithResource\":true,\"domain\"
  # testterm2 <-
  #   paste0("dictionaryIncludedWithResource",
  #          "\":",
  #          "true",
  #          ",\"",
  #          "domain\"")


  if (grepl(testterm1, dictionarystring) == TRUE) {
    # \"entity\":[{....}],\"domain\":[{....}],\"responsibleParty\":{
    # Extract entity string
    term1 <- paste0("\"", "entity", "\":[")
    term2 <- paste0("],\"", "domain", "\":")
    entitystring <- genXtract(dictionarystring, term1, term2)

    term3 <- paste0("attribute", "\":[")
    term4 <- paste0("],\"", "definition", "\":")

    entitystring2 <- genXtract(entitystring, term3, term4)

    # Extract domain string
    term5 <- paste0("\"", "domain", "\":[")
    term6 <- paste0("],\"", "responsibleParty", "\":")
    domainstring <- genXtract(dictionarystring, term5, term6)

  } else {
    # \"domain\":[{....}],\"entity\":[{....}],\"description\":...}}
    # Extract entity string
    term1 <- paste0("\"", "entity", "\":[")
    # term2 <- paste0("],\"", "description", "\":")
    entitystring <- genXtract(dictionarystring, term1, "}}")

    term3 <- paste0("attribute", "\":[")
    term4 <- paste0("],\"", "codeName", "\":")

    entitystring2 <- genXtract(entitystring, term3, term4)

    # Extract domain string
    # \"domain\":[{....}],\"entity\":[{....}],\"description\":...}}
    term5 <- paste0("\"", "domain", "\":[")
    term6 <- paste0(",\"", "entity", "\":")
    domainstring <- genXtract(dictionarystring, term5, term6)
  }



  # (grepl(testterm2, dictionarystring) == TRUE)
  # else stop('R list in unsupported format.')



  #### Entity ####

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


  # Create a vector of domain elements
  term1 <- paste0("{\"", "domainId", "\":")
  term2 <- paste0(",\"", "description", "\"")

  #\"description\":\
  term3 <- paste0("\"", "domainItem", "\":[")
  term4 <- paste0("]")

  # Extract Item and Ref strings

  domainelements <-
    genXtract(domainstring, term1, term2, with = TRUE)

  domainRefs<-domainelements
  for(q in 1:length(domainRefs)){
    domainRefs[q]<-rm_between(domainRefs[q],",\"domainItem\":[","}]", replacement = "")
  }

  domainRefs <- as.vector(domainRefs)
  domainItems <- genXtract(domainstring, term3, term4)
  domainItems <- as.vector(domainItems)



  # Create blank ref table

  reftable <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                       c("domainId",
                         "codeName",
                         "domainNum"))


  # Associate codeName with a domain domainNum
  for (h in 1:length(domainRefs)) {

    if(grepl("domainReference",domainRefs[h])==TRUE){
      domainRefs[h]<-rm_between(domainRefs[h],",\"domainReference\":{","}", replacement = "")
    }
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


      if (grepl(":", Ref2[m]) == TRUE){
      column <- beg2char(Ref2[m], ":")
      entry <- char2end(Ref2[m], ":")
      }
      else {next}

      reftable[[paste0(column)]][h] <- entry
      reftable$domainNum[h] <- h
    }
  }




  # Extract separate elements from domain string and add each sub element to blank table


  for (e in 1:length(domainItems)) {
    Item <- domainItems[e]

    Item2 <-
      unlist(strsplit(Item , "(?<=\\},)(?=\\{)"  , perl = TRUE),
             use.names = FALSE)

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
    arrange(codeName,domainNum) %>%
    select(-one_of(c("domainNum", "entityNum"))) %>%
    rename("domainItem_name" = "name",
           "domainItem_value" = "value") %>%
    mutate(
      allowNull = replace(allowNull, allowNull == "true", "yes"),
      allowNull = replace(allowNull, allowNull == "false", "no")
    ) %>%
    mutate(
      isCaseSensitive = replace(isCaseSensitive, isCaseSensitive == "true", "yes"),
      isCaseSensitive = replace(isCaseSensitive, isCaseSensitive == "false", "no")
    )

  assign("newtable", blanktable)

}


  # rfile <- file.choose()
  # NCmisc::list.functions.in.file(rfile)
