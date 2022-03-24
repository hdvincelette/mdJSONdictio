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


build.table <- function(x, dictionary_num) {
  JSONdictionary <- x

  n<-1

  # Check for optional parameters
  if (missing(dictionary_num))
    n <- 1
  else
    n <- dictionary_num

  if (is.integer(n)==FALSE)
    stop ('dictionary_num must be an integer.\n  Print `??mdJSONdictio` for Help Pages.'
    )



  # Check validity of the input
  dictionarystring <-
    JSONdictionary[["data"]][[n]][["attributes"]][["json"]]

  if (grepl("entity", dictionarystring) == FALSE)
    stop(
      'No Entity detected.\n  Print `??mdJSONdictio` for Help Pages.'
    )

  if (grepl("attribute", dictionarystring) == FALSE)
    stop(
      'Entity requires atleast one attribute.\n  Print `??mdJSONdictio` for Help Pages.'
    )

  if (grepl("dataType", dictionarystring) == FALSE)
    stop(
      'Entity requires atleast one attribute.\n  Print `??mdJSONdictio` for Help Pages.'
    )


  # Detect format of the input and process accordingly

  #\"subject\":[\"dataDictionary\"],\"entity\":
  testterm1 <- paste0("dataDictionary", "\"],\"", "entity", "\"")

  if (grepl(testterm1, dictionarystring) == TRUE) {
    # \"entity\":[{....}],\"domain\":[{....}],\"responsibleParty\":{
    # Extract entity string
    term1 <- paste0("\"", "entity", "\":[")
    term2 <- paste0("],\"", "domain", "\":")
    entitystring <- genXtract(dictionarystring, term1, term2)

    term3 <- paste0("attribute", "\":[")
    term4 <- paste0("],\"", "definition", "\":")

    if (grepl(term4, entitystring) == TRUE) {
      entitystring2 <- genXtract(entitystring, term3, term4)
    } else {
      entitystring2 <- genXtract(entitystring, term3, "}]}")

    }

    # Extract domain string
    term5 <- paste0("\"", "domain", "\":[")
    term6 <- paste0("],\"", "responsibleParty", "\":")

    if (grepl(term6, entitystring) == TRUE) {
      domainstring <- genXtract(dictionarystring, term5, term6)
    } else {
      term6 <- paste0(",", "date-updated")
      term7 <- paste0("],\"", "description", "\":")

      domainstring <- genXtract(dictionarystring, term5, "}}")
    }

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

    blanktable[nrow(blanktable) + 1, ] <- NA


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


  if (length(domainstring) != 0) {
    # Create a vector of domain elements
    term1 <- paste0("{\"", "domainId", "\":")
    term2 <- paste0(",\"", "description", "\"")

    # Extract Item and Ref strings

    domainelements <-
      genXtract(domainstring, term1, term2, with = TRUE)

    domainItems <- c()
    domainRefs <- c()

    for (r in 1:length(domainelements)) {
      if (grepl("domainItem", domainelements[r]) == TRUE) {
        domainItems <- c(domainItems, domainelements[r])

        temp <-
          rm_between(domainelements[r],
                     ",\"domainItem\":[{",
                     "}]",
                     replacement = "")
        domainRefs <- c(domainRefs, temp)
      }

    }

    domainRefs <- as.vector(domainRefs)


    term3 <- paste0("\"", "domainItem", "\":[")
    term4 <- paste0("]")

    domainItems2 <- genXtract(domainItems, term3, term4)
    domainItems2 <- as.vector(domainItems2)



    # Create blank ref table

    reftable <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                         c("domainId",
                           "codeName",
                           "domainNum"))


    # Associate codeName with a domain domainNum
    for (h in 1:length(domainRefs)) {
      if (grepl("domainReference", domainRefs[h]) == TRUE) {
        domainRefs[h] <-
          rm_between(domainRefs[h],
                     ",\"domainReference\":{",
                     "}",
                     replacement = "")
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

      reftable[nrow(reftable) + 1, ] <- NA

      for (m in 1:length(Ref2)) {
        if (grepl("\"", Ref2[m]) == TRUE) {
          Ref2[m] <- gsub("\"", "", Ref2[m])
        }


        if (grepl(":", Ref2[m]) == TRUE) {
          column <- beg2char(Ref2[m], ":")
          entry <- char2end(Ref2[m], ":")
        }
        else {
          next
        }

        reftable[[paste0(column)]][h] <- entry
        reftable$domainNum[h] <- h
      }
    }




    # Extract separate elements from domain string and add each sub element to blank table


    for (e in 1:length(domainItems2)) {
      Item <- domainItems2[e]
      Item2 <-
        unlist(strsplit(as.character(Item), "(?<=\\},)(?=\\{)", perl = TRUE),
               use.names = FALSE)

      for (f in 1:length(Item2)) {
        if (grepl("},", Item2[f]) == TRUE) {
          Item2[f] <- sub("},", "}", Item2[f])
        }

        Item2[f] <- gsub("\\{|}", "", Item2[f])

        SubItem <-
          unlist(strsplit(Item2[f], ",\"", perl = TRUE), use.names = FALSE)

        blanktable[nrow(blanktable) + 1, ] <- NA

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

  }

  blanktable <- blanktable %>%
    arrange(codeName, domainNum) %>%
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

