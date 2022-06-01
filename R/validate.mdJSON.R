#' Validate mdJSON Data Dictionaries
#'
#' Compares an mdJSON data dictionary against a dataset and summarizes discrepancies in a data frame.
#' @param x A list object converted from an mdJSON data dictionary file.
#' @param y A data frame of the dataset.
#' @param dictionary_num Default=1. An integer indicating the dictionary if there is more than one in the mdJSON file (i.e. if multiple dictionaries are exported together in mdEditor).
#' @param entity_num Default=1. An integer indicating the entity if there is more than one in the mdJSON file.
#' @return Returns a data frame comprised of warning messages about and the mdJSON data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```validate.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' input.dict <- rjson::fromJSON(file = path)
#'
#' # Import dataset as data frame
#' path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
#' input.data<-read.csv(path, na.strings = "", stringsAsFactors = FALSE)
#'
#' # Validate dataset against dictionary
#' all.warnings<- validate.mdJSON(input.dict, input.data, dictionary_num = 1, entity_num = 1)
#'
#' # Export table to disk
#' write.csv(x = all.warnings, file = "e.g.warnings2.json")



validate.mdJSON <- function(x, y, dictionary_num = 1, entity_num = 1) {

  `%>%` <- magrittr::`%>%`


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



  input.dict2<- mdJSONdictio::build.table(x = x, dictionary_num = n, entity_num = a)
  input.data2 <- y

  all.warnings<- validate.table(input.dict2, input.data2)


}
