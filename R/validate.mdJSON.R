#' Validate mdJSON Data Dictionaries
#'
#' Compares an mdJSON data dictionary to a tabular dataset and summarizes discrepancies in a data frame.
#' @param x List object converted from an mdJSON file.
#' @param y Data frame of a dataset.
#' @param entity_num Default=1. Integer indicating the entity if there is more than one in the mdJSON file.
#' @return Returns a data frame comprised of warning messages about the mdJSON data dictionary.
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @seealso ```validate.mdJSON()```
#' @export
#' @examples
#' # Import mdJSON data dictionary as list
#' path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
#' input.dxnry <- rjson::fromJSON(file = path)
#'
#' # Import tabular dataset as data frame
#' path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
#' input.data<-read.csv(file = path, na.strings = "", stringsAsFactors = FALSE)
#'
#' # Validate list against data frame
#' all.warnings<- validate.mdJSON(x = input.dxnry, y = input.data, entity_num = 1)
#'
#' # Export table to disk
#' write.csv(x = all.warnings, file = "e.g.warnings.csv")



validate.mdJSON <- function(x, y, entity_num = 1) {

  `%>%` <- magrittr::`%>%`

  # if (length(x[["data"]]) > 1) {
  #   input.dxnry <-
  #     extract.mdJSON(x = x,
  #                    record.type = "dictionaries",
  #                    multiple = FALSE)
  # } else {
  #   input.dxnry <- x
  # }

  a <- 1


  ## Parameter arguments
  if (missing(entity_num))
    a <- 1
  else
    a <- entity_num



  input.dxnry2<- mdJSONdictio::build.table(x = input.dxnry, entity_num = a)
  input.data2 <- y

  all.warnings<- validate.table(input.dxnry2, input.data2)


}
