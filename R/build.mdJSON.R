#' Build mdJSON Data Dictionaries
#'
#' Transforms a tabular data dictionary (read as a data frame) into an R list that can be subsequently converted to mdJSON and imported to mdEditor as a Dictionary record. The input data frame must be formatted to a \href{https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template.xlsx?raw=true}{template}.
#' @param x  A data frame of the tabular data dictionary.
#' @param title A string designating the title of the Dictionary record in mdEditor.
#' @return Returns an R list corresponding to the tabular data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```build.table()```
#' @export
#' @examples
#' # Import tabular data dictionary as data frame
#' path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
#' e.g.dictionary<-readxl::read_excel(path)
#'
#' # Transform data frame to R list
#' newjson<- build.mdJSON(x = e.g.dictionary, title = "Example Dictionary")
#'
#' # Convert R list to JSON
#' e.g.dictionary = rjson::toJSON(newjson)
#'
#' # Export JSON to disk
#' write(e.g.dictionary, "e.g.dictionary.json")


build.mdJSON <- function(x, title) {
  # Prepare the dictionary

  Data.Dictionary <- x



  ## Check for errors
  Required.cols <- c(
    "codeName",
    "domainItem_name",
    "domainItem_value",
    "definition",
    "dataType",
    "allowNull"
  )

  if (length(setdiff(Required.cols, names(Data.Dictionary))) != 0)
    stop(
      'Data frame missing required columns: ',
      toString(setdiff(Required.cols, names(Data.Dictionary))),
      '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
    )

  for (a in 1:ncol(Data.Dictionary)) {
    if (!colnames(Data.Dictionary[a]) %in% c(
      "codeName",
      "domainItem_name",
      "domainItem_value",
      "definition",
      "dataType",
      "allowNull",
      "units",
      "unitsResolution",
      "minValue",
      "maxValue",
      "missingValue",
      "fieldWidth",
      "isCaseSensitive",
      "notes"
    ))
    stop(
      'Data frame contains an invalid column: ',
      colnames(Data.Dictionary[a]),
      '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
    )
    for (aa in 1:nrow(Data.Dictionary)) {
      if (colnames(Data.Dictionary[a]) %in% c("codeName",
                                              "domainItem_name",
                                              "domainItem_value",
                                              "definition") &
          is.na(Data.Dictionary[aa, a]) == TRUE)
        stop(
          'Required field incomplete. \n  ',
          colnames(Data.Dictionary[a]),
          '==NA in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )
      if (Data.Dictionary$domainItem_name[aa] == "colname" &
          Data.Dictionary$domainItem_value[aa] != "colname")
        stop(
          'Data frame contains conflicting entries.',
          '\n  Row ',
          aa,
          ' has "',
          Data.Dictionary$domainItem_name[aa],
          '" for domainItem_name and "',
          Data.Dictionary$domainItem_value[aa],
          '" for domainItem_value.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )
      if (Data.Dictionary$domainItem_name[aa] != "colname" &
          Data.Dictionary$domainItem_value[aa] == "colname")
        stop(
          'Data frame contains conflicting entries.\n  domainItem_name=="',
          Data.Dictionary$domainItem_name[aa],
          '" and domainItem_value=="',
          Data.Dictionary$domainItem_value[aa],
          '" in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )

      if (Data.Dictionary$domainItem_name[aa] == "colname" &
          is.na(Data.Dictionary$dataType[aa]))
        stop(
          'Required field incomplete. \n  dataType==NA in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )

      if (Data.Dictionary$domainItem_name[aa] == "colname" &
          is.na(Data.Dictionary$allowNull[aa]))
        stop(
          'Required field incomplete. \n  allowNull==NA in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )

      if (is.character(Data.Dictionary$fieldWidth[aa]) == TRUE)
        stop(
          'fieldWidth must be an integer. \n  allowNull==NA in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )

      if (is.character(Data.Dictionary$unitsResolution[aa]) == TRUE)
        stop(
          'unitsResolution must be numeric. \n  allowNull==NA in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )


    }
  }


  ## Replace values and add domain column
  Data.Dictionary <- Data.Dictionary %>%
    mutate_if(is.character, str_replace_all, "\"", "'") %>%
    mutate_at(vars(allowNull, isCaseSensitive),
              ~ replace(., which(. == "yes"), "true")) %>%
    mutate_at(vars(allowNull, isCaseSensitive),
              ~ replace(., which(. == "no"), "false")) %>%
    select(-notes) %>%
    add_column(domainId = NA)

  for (b in 1:nrow(Data.Dictionary)) {
    if (Data.Dictionary$domainItem_name[b] != "colname") {
      next
    }
    else if (sum(Data.Dictionary$codeName == Data.Dictionary$codeName[b]) >
             1)
    {
      Data.Dictionary$domainId[b] = "true"
    }
  }


  ## Generate uuids
  id <- UUIDgenerate(use.time = FALSE, n = 1)
  id <- sub("\\-.*", "", id)

  dictionaryId <- UUIDgenerate(use.time = FALSE, n = 1)
  entityId <- UUIDgenerate(use.time = FALSE, n = 1)

  ## Create date in IOS format
  ## "2019-10-16T20:13:48.641Z"
  date <- strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M")
  date <- paste0(date, ":00.000Z", collapse = "")


  ## Add domain ids to original file
  ## Isolate colname rows with domain notations
  domaincolumns <- Data.Dictionary %>%
    filter(domainItem_value == "colname") %>%
    filter(domainId == "true") %>%
    select(codeName, domainId)

  ## Fill domain column with uuids
  domaincolumns$domainId <-
    UUIDgenerate(use.time = FALSE, n = nrow(domaincolumns))

  ## Join domainIds with origianl data frame
  for (e in 1:nrow(domaincolumns)) {
    for (d in 1:nrow(Data.Dictionary)) {
      if (domaincolumns$codeName[e] == Data.Dictionary$codeName[d] &
          Data.Dictionary$domainItem_value[d] == "colname") {
        Data.Dictionary$domainId[d] = domaincolumns$domainId[e]
      }
    }
  }



  # Add domain and entity reference numbers

  Data.Dictionary$entityNum <- NA
  Data.Dictionary$domainNum <- NA
  entitycount = 0
  domaincount = 0



  for (h in 1:nrow(Data.Dictionary)) {
    if (Data.Dictionary$domainItem_name[h] == "colname") {
      entitycount = entitycount + 1
      Data.Dictionary$entityNum[h] <-
        entitycount
    } else{
      Data.Dictionary$entityNum[h] = 0
    }
    Data.Dictionary$domainNum[h] = 0
  }


  # Create entity and domain references


  domainref.A <- Data.Dictionary %>%
    filter(domainItem_name == "colname", is.na(domainId) == FALSE) %>%
    select(-domainItem_name, domainItem_value)


  for (i in 1:nrow(Data.Dictionary)) {
    for (k in 1:nrow(domainref.A)) {
      if (Data.Dictionary$domainItem_name[i] != "colname" &
          Data.Dictionary$codeName[i] == domainref.A$codeName[k]) {
        domaincount = domaincount + 1
        Data.Dictionary$entityNum[i] <- domainref.A$entityNum[k]
        Data.Dictionary$domainNum[i] <- domaincount
      }

    }
  }

  domainref.I <- Data.Dictionary %>%
    filter(domainNum != 0)

  entityref <- Data.Dictionary %>%
    filter(domainItem_name == "colname") %>%
    select(-domainItem_name, domainItem_value)





  for (r in 1:nrow(entityref)) {
    for (s in 1:nrow(domainref.I)) {
      if (domainref.I$codeName[s] == entityref$codeName[r]) {
        domainref.I$domainId[s] = entityref$domainId[r]
      }
    }
  }


  # Add attributes

  dictionarylist <-
    fromJSON(blankjson[["data"]][[1]][["attributes"]][["json"]])

  dictionarylist[["dataDictionary"]][["entity"]][[1]][["entityId"]] <-
    entityId

  names <-
    names(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[1]])


  ## Duplicate first (empty) attribute and update the addition



  for (m in 2:nrow(entityref)) {
    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]]) +
                                                                          1]] <-
      dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[1]]

    for (n in 1:ncol(entityref)) {
      for (o in 1:length(names)) {
        if (colnames(entityref[m, n]) == names[o]) {
          value <- as.character(entityref[[paste0(names[o])]][m])

          dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[m]][[paste0(names[o])]] <-
            value
        }
      }
    }

    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[m]] <-
      dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[m]] %>%
      map(discard, is.na) %>%
      compact()

  }



  ## Update the first attribute

  for (n in 1:ncol(entityref)) {
    for (o in 1:length(names)) {
      if (colnames(entityref[1, n]) == names[o]) {
        value <- entityref[[paste0(names[o])]][1]

        if (!names[o] %in% c("fieldWidth", "unitsResolution")) {
          value <- as.character(value)
        }

        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[1]][[paste0(names[o])]] <-
          value

      }
    }
    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[1]] <-
      dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[1]] %>%
      map(discard, is.na) %>%
      compact()
  }




  # Add domains

  e.count <- c(1:length(domainref.A$entityNum))
  d.count <- c(1:length(domainref.I$domainNum))


  names <-
    names(dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]])


  ## Duplicate first (empty) domain and update the addition

  for (p in 2:length(e.count)) {
    dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) +
                                                      1]] <-
      dictionarylist[["dataDictionary"]][["domain"]][[1]]

    for (q in 2:nrow(domainref.I)) {
      dictionarylist[["dataDictionary"]][["domain"]][[p]][["codeName"]] <-
        as.character(domainref.A$codeName[p])
      dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainId"]] <-
        as.character(domainref.A$domainId[p])
      dictionarylist[["dataDictionary"]][["domain"]][[p]][["description"]] <-
        as.character(domainref.A$definition[p])
    }

    e.reference <- domainref.A$entityNum[p]

    items <- domainref.I %>%
      filter(entityNum == e.reference)

    ### Duplicate first (empty) domain item and update the addition

    for (t in 1:nrow(items)) {
      dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]][[length(dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]]) +
                                                                             1]] <-
        dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]][[1]]

      dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]][[t]][["name"]] <-
        as.character(items$domainItem_name[t])
      dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]][[t]][["value"]] <-
        as.character(items$domainItem_value[t])
      dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]][[t]][["definition"]] <-
        as.character(items$definition[t])

    }

  }


  ## Update the first domain and items

  e.reference <- domainref.A$entityNum[1]


  items <- domainref.I %>%
    filter(entityNum == e.reference)

  dictionarylist[["dataDictionary"]][["domain"]][[1]][["codeName"]] <-
    as.character(domainref.A$codeName[1])
  dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainId"]] <-
    as.character(domainref.A$domainId[1])
  dictionarylist[["dataDictionary"]][["domain"]][[1]][["description"]] <-
    as.character(domainref.A$definition[1])

  dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]][["name"]] <-
    as.character(items$domainItem_name[1])
  dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]][["value"]] <-
    as.character(items$domainItem_value[1])
  dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]][["definition"]] <-
    as.character(items$definition[1])


  # Update fields

  dictionarylist[["dictionaryId"]] <- as.character(dictionaryId)

  dictionarylist[["dataDictionary"]][["citation"]][["title"]] <-
    as.character(title)

  dictionarylist[["dataDictionary"]][["citation"]][["date"]][[1]][["date"]] <-
    as.character(date)


  blankjson[["data"]][[1]][["id"]] <- as.character(id)
  blankjson[["data"]][[1]][["attributes"]][["date-updated"]] <-
    as.character(date)

  newstring <- toJSON(dictionarylist)

  oldsubject <- paste0('\"', 'subject', '\":\"', 'dataDictionary', '\"')
  newsubject <- paste0('\"', 'subject', '\":[\"', 'dataDictionary', '\"]')
  newstring <- mgsub(oldsubject, newsubject, newstring)

  oldcase <- paste0('\"', 'isCaseSensitive', '\":\"', 'true', '\"')
  newcase <- paste0('\"', 'isCaseSensitive', '\":', 'true')
  newstring <- mgsub(oldcase, newcase, newstring)

  oldcase <- paste0('\"', 'isCaseSensitive', '\":\"', 'false', '\"')
  newcase <- paste0('\"', 'isCaseSensitive', '\":', 'false')
  newstring <- mgsub(oldcase, newcase, newstring)

  oldnull <- paste0('\"', 'allowNull', '\":\"', 'true', '\"')
  newnull <- paste0('\"', 'allowNull', '\":', 'true')
  newstring <- mgsub(oldnull, newnull, newstring)

  oldnull <- paste0('\"', 'allowNull', '\":\"', 'false', '\"')
  newnull <- paste0('\"', 'allowNull', '\":', 'false')
  newstring <- mgsub(oldnull, newnull, newstring)

  blankjson[["data"]][[1]][["attributes"]][["json"]] <- newstring

  assign("newjson", blankjson)


}
