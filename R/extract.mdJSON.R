#' Extract mdJSON Data Dictionaries from a Metadata file
#'
#' Plucks one or more mdJSON data dictionaries from an mdJSON file comprised of other types of records.
#' @param x List object converted from an mdJSON file.
#' @param record.type Default="dictionaries". String or vector representing the type(s) of records to extract. Additional options include "records" and "contacts".
#' @return Returns a list object corresponding to the mdJSON data dictionary file.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```extract.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' input.metadata <- rjson::fromJSON(file = path)
#'
#' # Extract a data dictionary record from a Metadata file
#' dxnry.record<- extract.mdJSON(x = input.metadata, record.type = "dictionaries")
#'
#' # Convert list to JSON
#' output.dxnry = rjson::toJSON(x = dxnry.record)
#'
#' # Export JSON to disk
#' write(x = output.dxnry, file = "e.g.dictionary.json")


extract.mdJSON <-
  function(x,
           record.type = "dictionaries",
           multiple = TRUE) {
    `%>%` <- magrittr::`%>%`

    if (missing(record.type)) {
      record.type <- "dictionaries"
    }
    if (missing(multiple)) {
      multiple <- TRUE
    }

    input.metadata <- x

    filtered.records <- list()

    for (d in 1:length(record.type)) {
      for (a in 1:length(input.metadata[["data"]])) {
        if (input.metadata[["data"]][[a]][["type"]] == record.type[d]) {
          filtered.records[["data"]][[length(filtered.records[["data"]]) + 1]] <-
            input.metadata[["data"]][[a]]
        }

      }
    }

    selected.records <- list()

    if (length(filtered.records[["data"]]) == 0) {
      message(cat(
        paste0(
          "Operation canceled. No ",
          knitr::combine_words(record.type, and = " or "),
          paste0(" were found")
        )
      ))
    } else if (length(filtered.records[["data"]]) > 1) {
      filtered.names <- c()

      for (b in 1:length(filtered.records[["data"]])) {
        recordlist <-
          rjson::fromJSON(filtered.records[["data"]][[b]][["attributes"]][["json"]])
        if (filtered.records[["data"]][[b]][["type"]] == "dictionaries") {
          filtered.names <-
            c(filtered.names, recordlist[["dataDictionary"]][["citation"]][["title"]])
        } else if (filtered.records[["data"]][[b]][["type"]] == "records") {
          filtered.names <-
            c(filtered.names, recordlist[["metadata"]][["resourceInfo"]][["citation"]][["title"]])
        } else if (filtered.records[["data"]][[b]][["type"]] == "contacts") {
          filtered.names <- c(filtered.names, recordlist[["name"]])
        }
      }

      record.choice <- utils::select.list(
        c(filtered.names),
        title = cat(
          paste0("The mdJSON list object contains more than one record.")
        ),
        multiple = multiple,
        graphics = TRUE
      )

      record.index <- c()
      for (c in 1:length(record.choice)) {
        record.index <-
          c(record.index,
            which(record.choice[c] == filtered.names))
        selected.records[["data"]] <-
          filtered.records[["data"]][record.index]

      }

    } else if (length(filtered.records[["data"]]) == 1) {
      selected.records[["data"]] <-
        filtered.records[["data"]]
    }

    return(selected.records)

  }
