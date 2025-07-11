#' Build mdJSON Data Dictionaries
#'
#' Translates a data frame of a tabular data dictionary into an list object that can be subsequently converted to mdJSON and imported to mdEditor as a Dictionary Record. The input tabular data dictionary must be formatted to a \href{https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/mdJSONdictio_Dictionary_Template_v2.xlsx?raw=true}{template}.
#' @param x  Data frame of the tabular data dictionary.
#' @param title String designating the title of the Dictionary Record in mdEditor.Default=deparse(match.call()$x).
#' @return Returns a list object corresponding to the tabular data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```build.table()```
#' @export
#' @examples
#' # Import tabular data dictionary as data frame
#' path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
#' input.table<-readxl::read_excel(path = path)
#'
#' # Translate data frame to list
#' new.list<- mdJSONdictio::build.mdJSON(x = input.table, title = "Example Dictionary")
#'
#' # Convert list to JSON
#' new.json = rjson::toJSON(x = new.list)
#'
#' # Export JSON to disk
#' write(x = new.json, file = "e.g.dictionary.json")



build.mdJSON <- function(x, title) {
  `%>%` <- magrittr::`%>%`

  blankjson<- get0("blankjson", envir = asNamespace("mdJSONdictio"))

  if (missing(title)) {
    title <- deparse(match.call()$x)
  }


  # Prepare the dictionary
  Data.Dictionary <- x

  Data.Dictionary <- Data.Dictionary %>% tidyr::replace_na(
    list(
      codeName = 'NA',
      domainItem_name = 'NA',
      domainItem_value = 'NA',
      definition = 'NA'
    )
  )

  # Check for tabular data dictionary template v1
  if ("yes" %in% unique(Data.Dictionary$allowNull) |
      "no" %in% unique(Data.Dictionary$allowNull)) {
    stop(
      "'allowNull' and 'isCaseSensitive' only accept logical values (TRUE/FALSE).\nCorrect these fields in the data frame input before continuing.\nUse the latest version (v2) of the tabular data dictionary template to ensure the functon runs smoothly.\n  Print `help(package = 'mdJSONdictio')` for Help Pages.'"
    )
  }

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
      'Data frame missing required column(s): ',
      toString(setdiff(Required.cols, names(Data.Dictionary))),
      '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
    )

  for (a in 1:ncol(Data.Dictionary)) {
    # if (!colnames(Data.Dictionary[a]) %in% c(
    #   "codeName",
    #   "domainItem_name",
    #   "domainItem_value",
    #   "definition",
    #   "dataType",
    #   "allowNull",
    #   "units",
    #   "unitsResolution",
    #   "minValue",
    #   "maxValue",
    #   "missingValue",
    #   "fieldWidth",
    #   "isCaseSensitive",
    #   "notes"
    # ))
    # stop(
    #   'Data frame contains an invalid column: ',
    #   colnames(Data.Dictionary[a]),
    #   '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
    # )
    for (aa in 1:nrow(Data.Dictionary)) {
      ## inoperative; NA replaced with 'NA'
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
      if (Data.Dictionary$domainItem_name[aa] == "dataField" &
          Data.Dictionary$domainItem_value[aa] != "dataField")
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
      if (Data.Dictionary$domainItem_name[aa] != "dataField" &
          Data.Dictionary$domainItem_value[aa] == "dataField")
        stop(
          'Data frame contains conflicting entries.\n  domainItem_name=="',
          Data.Dictionary$domainItem_name[aa],
          '" and domainItem_value=="',
          Data.Dictionary$domainItem_value[aa],
          '" in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )

      if (Data.Dictionary$domainItem_name[aa] == "dataField" &
          is.na(Data.Dictionary$dataType[aa]))
        stop(
          'Required field incomplete. \n  dataType==NA in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )

      if (Data.Dictionary$domainItem_name[aa] == "dataField" &
          is.na(Data.Dictionary$allowNull[aa]))
        stop(
          'Required field incomplete. \n  allowNull==NA in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )

      if (is.character(Data.Dictionary$fieldWidth[aa]) == TRUE)
        stop(
          'fieldWidth has an incompatible data type in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )

      if (is.character(Data.Dictionary$unitsResolution[aa]) == TRUE)
        stop(
          'unitsResolution has an incompatible data type in row ',
          aa,
          '.\n  Print `help(package = "mdJSONdictio")` for Help Pages.'
        )


    }
  }

  ## Replace values and add domain column
  Data.Dictionary <- Data.Dictionary %>%
    dplyr::mutate_if(is.character, stringr::str_replace_all, "\"", "'") %>%
    dplyr::select(-tidyselect::any_of(c("notes"))) %>%
    tibble::add_column(domainId = NA)

  for (b in 1:nrow(Data.Dictionary)) {
    if (Data.Dictionary$domainItem_name[b] != "dataField") {
      next
    }
    else if (sum(Data.Dictionary$codeName == Data.Dictionary$codeName[b]) >
             1)
    {
      Data.Dictionary$domainId[b] = "true"
    }
  }

  ## Generate uuids
  id <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
  id <- sub("\\-.*", "", id)

  dictionaryId <- uuid::UUIDgenerate(use.time = FALSE, n = 1)
  entityId <- uuid::UUIDgenerate(use.time = FALSE, n = 1)

  ## Create date in IOS format
  ## "2019-10-16T20:13:48.641Z"
  date <- strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M")
  date <- paste0(date, ":00.000Z", collapse = "")

  ## Add domain ids to original file
  ## Isolate dataField rows with domain notations
  domaincolumns <- Data.Dictionary %>%
    dplyr::filter(domainItem_value == "dataField") %>%
    dplyr::filter(domainId == "true") %>%
    dplyr::select(codeName, domainId)

  ## Fill domain column with uuids
  domaincolumns$domainId <-
    uuid::UUIDgenerate(use.time = FALSE, n = nrow(domaincolumns))

  ## Join domainIds with origianl data frame
  if(nrow(domaincolumns) != 0) {
    for (e in 1:nrow(domaincolumns)) {
      for (d in 1:nrow(Data.Dictionary)) {
        if (domaincolumns$codeName[e] == Data.Dictionary$codeName[d] &
            Data.Dictionary$domainItem_value[d] == "dataField") {
          Data.Dictionary$domainId[d] = domaincolumns$domainId[e]
        }
      }
    }
  }

  # Add domain and entity reference numbers
  Data.Dictionary$entityNum <- NA
  Data.Dictionary$domainNum <- NA
  entitycount = 0
  domaincount = 0

  for (h in 1:nrow(Data.Dictionary)) {
    if (Data.Dictionary$domainItem_name[h] == "dataField") {
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
    dplyr::filter(domainItem_name == "dataField", is.na(domainId) == FALSE) %>%
    dplyr::select(-domainItem_name, domainItem_value)

  if (nrow(domainref.A) != 0) {
    for (i in 1:nrow(Data.Dictionary)) {
      for (k in 1:nrow(domainref.A)) {
        if (Data.Dictionary$domainItem_name[i] != "dataField" &
            Data.Dictionary$codeName[i] == domainref.A$codeName[k]) {
          domaincount = domaincount + 1
          Data.Dictionary$entityNum[i] <- domainref.A$entityNum[k]
          Data.Dictionary$domainNum[i] <- domaincount
        }

      }
    }
  }

  domainref.I <- Data.Dictionary %>%
    dplyr::filter(domainNum != 0)

  entityref <- Data.Dictionary %>%
    dplyr::filter(domainItem_name == "dataField") %>%
    dplyr::select(-domainItem_name, domainItem_value)

  if (nrow(domainref.I) != 0) {
    for (r in 1:nrow(entityref)) {
      for (s in 1:nrow(domainref.I)) {
        if (domainref.I$codeName[s] == entityref$codeName[r]) {
          domainref.I$domainId[s] = entityref$domainId[r]
        }
      }
    }
  }

  # Add attributes
  dictionarylist <-
    rjson::fromJSON(blankjson[["data"]][[1]][["attributes"]][["json"]])

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
        if (colnames(entityref[n]) == names[o]) {
          value <- as.character(entityref[[paste0(names[o])]][m])

          dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[m]][[paste0(names[o])]] <-
            value
        }
      }
    }

    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[m]] <-
      dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[m]] %>%
      purrr::map(purrr::discard, is.na) %>%
      plyr::compact()

  }

  ## Update the first attribute
  for (n in 1:ncol(entityref)) {
    for (o in 1:length(names)) {
      if (colnames(entityref[n]) == names[o]) {
        value <- as.character(entityref[[paste0(names[o])]][1])

        if (!names[o] %in% c("fieldWidth", "unitsResolution")) {
          value <- as.character(value)
        }

        dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[1]][[paste0(names[o])]] <-
          value

      }
    }
    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[1]] <-
      dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[1]] %>%
      purrr::map(purrr::discard, is.na) %>%
      plyr::compact()
  }

  ## Correct data types for unitsResolution and fieldWidth
  suppressWarnings(for (nn in 1:length(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]])) {
    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[nn]][["unitsResolution"]] <-
      as.numeric(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[nn]][["unitsResolution"]])
    dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[nn]][["fieldWidth"]] <-
      as.numeric(dictionarylist[["dataDictionary"]][["entity"]][[1]][["attribute"]][[nn]][["fieldWidth"]])
  })

  # Add domains
  e.count <- c(1:length(domainref.A$entityNum))
  d.count <- c(1:length(domainref.I$domainNum))

  names <-
    names(dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]])


  if(nrow(domainref.A)!=0){
    if (length(e.count) > 1) {
      ## Duplicate first (empty) domain and update the addition
      for (p in 2:length(e.count)) {
        dictionarylist[["dataDictionary"]][["domain"]][[length(dictionarylist[["dataDictionary"]][["domain"]]) + 1]] <-
          dictionarylist[["dataDictionary"]][["domain"]][[1]]

        dictionarylist[["dataDictionary"]][["domain"]][[p]][["codeName"]] <-
          as.character(domainref.A$codeName[p])
        dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainId"]] <-
          as.character(domainref.A$domainId[p])
        dictionarylist[["dataDictionary"]][["domain"]][[p]][["description"]] <-
          as.character(domainref.A$definition[p])

        e.reference <- domainref.A$entityNum[p]

        items <- domainref.I %>%
          dplyr::filter(entityNum == e.reference)

        ### Duplicate first (empty) domain item and update the addition
        for (t in 1:nrow(items)) {
          dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]][[1]][["name"]] <-
            as.character(items$domainItem_name[1])
          dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]][[1]][["value"]] <-
            as.character(items$domainItem_value[1])
          dictionarylist[["dataDictionary"]][["domain"]][[p]][["domainItem"]][[1]][["definition"]] <-
            as.character(items$definition[1])

          if (nrow(items) > 1 & t > 1) {
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

      }
    }

  ## Update the first domain and items
  e.reference <- domainref.A$entityNum[1]

  items <- domainref.I %>%
    dplyr::filter(entityNum == e.reference)

  dictionarylist[["dataDictionary"]][["domain"]][[1]][["codeName"]] <-
    as.character(domainref.A$codeName[1])
  dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainId"]] <-
    as.character(domainref.A$domainId[1])
  dictionarylist[["dataDictionary"]][["domain"]][[1]][["description"]] <-
    as.character(domainref.A$definition[1])

  for (t in 1:nrow(items)) {
    dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]][["name"]] <-
      as.character(items$domainItem_name[1])
    dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]][["value"]] <-
      as.character(items$domainItem_value[1])
    dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]][["definition"]] <-
      as.character(items$definition[1])

    if (nrow(items) > 1 & t > 1) {
      dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[length(dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]]) +
                                                                             1]] <-
        dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[1]]

      dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[t]][["name"]] <-
        as.character(items$domainItem_name[t])
      dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[t]][["value"]] <-
        as.character(items$domainItem_value[t])
      dictionarylist[["dataDictionary"]][["domain"]][[1]][["domainItem"]][[t]][["definition"]] <-
        as.character(items$definition[t])
    }

  }
  } else {
    dictionarylist[["dataDictionary"]][["domain"]][[1]] <- NULL
  }


  # Update fields
  dictionarylist[["dictionaryId"]] <- as.character(dictionaryId)

  dictionarylist[["dataDictionary"]][["citation"]][["title"]] <-
    as.character(title)

  dictionarylist[["dataDictionary"]][["citation"]][["date"]][[1]][["date"]] <-
    as.character(date)


  blankjson[["data"]][[1]][["id"]] <- as.character(id)
  blankjson[["data"]][[1]][["attributes"]][["date-updated"]] <-
    as.character(date)

  newstring <- rjson::toJSON(dictionarylist)

  # ## Correct subject
  # oldsubject <-
  #   paste0('\"',
  #          'subject',
  #          '\":\"',
  #          gsub(".*\"subject\":\"(\\w+)\".*", "\\1", newstring),
  #          '\"')
  # newsubject <-
  #   paste0(
  #     '\"',
  #     'subject',
  #     '\":[\"',
  #     gsub(".*\"subject\":\"(\\w+)\".*", "\\1", newstring),
  #     '\"]'
  #   )
  # newstring <- gsub(oldsubject, newsubject, newstring)

  oldcase <- paste0('\"', 'isCaseSensitive', '\":\"', 'true', '\"')
  newcase <- paste0('\"', 'isCaseSensitive', '\":', 'true')
  newstring <- gsub(oldcase, newcase, newstring, ignore.case = TRUE)

  oldcase <- paste0('\"', 'isCaseSensitive', '\":\"', 'false', '\"')
  newcase <- paste0('\"', 'isCaseSensitive', '\":', 'false')
  newstring <- gsub(oldcase, newcase, newstring, ignore.case = TRUE)

  oldnull <- paste0('\"', 'allowNull', '\":\"', 'true', '\"')
  newnull <- paste0('\"', 'allowNull', '\":', 'true')
  newstring <- gsub(oldnull, newnull, newstring, ignore.case = TRUE)

  oldnull <- paste0('\"', 'allowNull', '\":\"', 'false', '\"')
  newnull <- paste0('\"', 'allowNull', '\":', 'false')
  newstring <- gsub(oldnull, newnull, newstring, ignore.case = TRUE)


  blankjson[["data"]][[1]][["attributes"]][["json"]] <- newstring

  assign("newjson", blankjson)


}

