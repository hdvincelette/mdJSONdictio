#' Build mdJSON Dictionaries
#'
#' Transforms a tabular data dictionary into an R list that can be subsequently converted to mdJSON and imported to mdEditor as a Dictionary record. The input data frame must be formatted to a \href{https://github.com/hdvincelette/mdJSONdictio/blob/master/inst/templates/build.mdJSON_Dictionary_Template.xlsx?raw=true}{template}.
#' @param data  Data frame
#' @param title String
#' @return R list
#' @keywords mdEditor, mdJSON, json, dictionary, metadata
#' @export
#' @examples
#' #Import tabular data dictionary as data frame
#' path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
#' e.g.dictionary<-readxl::read_excel(path)
#'
#' #Transform data frame to R list
#' newjson<- build.mdJSON(data = e.g.dictionary, title = "Example Dictionary")
#'
#' #Convert R list to JSON
#' e.g.dictionary = rjson::toJSON(newjson)
#'
#' #Export JSON to disk
#' write(e.g.dictionary, "e.g.dictionary.json")


build.mdJSON <- function(data,title) {

  # Prepare the dictionary
  Data.Dictionary<-data

  for( a in 1:ncol(Data.Dictionary)){
  if(!colnames(Data.Dictionary[a]) %in% c("codeName","domainItem_name","domainItem_value",
                                     "definition","dataType","allowNull","units",
                                     "unitsResolution","minValue","maxValue",
                                     "isCaseSensitive","notes")) stop('Data frame contains an invalid column: ', paste0(colnames(Data.Dictionary[a])), '.\n  Print `?mdJSONdictio::build.mdJSON` for more information on data frame requirements.')
  }



  ## Replace values and add domain column
  Data.Dictionary <- Data.Dictionary %>%
    mutate_if(is.character, str_replace_all, "\"", "'") %>%
    mutate_at(vars(allowNull, isCaseSensitive), ~ replace(., which(.=="yes"), "true")) %>%
    mutate_at(vars(allowNull, isCaseSensitive), ~ replace(., which(.=="no"), "false")) %>%
    select(-notes) %>%
    add_column(domainId=NA)

  for(b in 1:nrow(Data.Dictionary)){
    if(Data.Dictionary$domainItem_name[b]!="colname"){next}
    else if(sum(Data.Dictionary$codeName==Data.Dictionary$codeName[b])>1)
    {Data.Dictionary$domainId[b]="true"}}


  ## Generate uuids
  id<- UUIDgenerate(use.time=FALSE, n=1)
  id<- sub("\\-.*", "", id)

  dictionaryId<- UUIDgenerate(use.time=FALSE, n=1)
  entityId<- UUIDgenerate(use.time=FALSE, n=1)

  ## Create date in IOS format
  # "date-updated":"2019-10-16T20:13:48.641Z"
  date<-strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%dT%H:%M")
  date<- paste0(date,":00.000Z",collapse = "")


  ## Add domain ids to original file
  ## Isolate colname rows with domain notations
  domaincolumns<-Data.Dictionary %>%
    filter(domainItem_value=="colname") %>%
    filter(domainId=="true") %>%
    select(codeName,domainId)

  ## Fill domain column with uuids
  domaincolumns$domainId<-UUIDgenerate(use.time=FALSE, n=nrow(domaincolumns))

  ## Join domainIds with origianl data frame
  for(e in 1:nrow(domaincolumns)){
    for (d in 1:nrow(Data.Dictionary)){
      if(domaincolumns$codeName[e]==Data.Dictionary$codeName[d] &
         Data.Dictionary$domainItem_value[d]=="colname"){
        Data.Dictionary$domainId[d]=domaincolumns$domainId[e]
      }
    }}



  # Create entity string

  ## REFERENCE
  ## \"entity\":[{\"entityId\":\"df6e75bb-450a-4b17-ab89-2bb9ea916a89\",\"attribute\":[
  ## {\"allowNull\":true,
  ## \"codeName\":\"Project\",
  ## \"dataType\":\"character varying\",
  ## \"domainId\":\"5c7daa81-d3b0-405c-9e6d-85632f4b2d2a\"},
  ## ]}]

  ## Create entity data frame for loop
  EntityDictionary<-Data.Dictionary %>%
    filter(domainItem_value=="colname") %>%
    select(-c("domainItem_name","domainItem_value"))

  ## Create empty lists for loop
  elementlist<- list()
  entitylist<- list()
  entityelement=NULL


  ## Loop to create entity list
  for (h in 1:nrow(EntityDictionary)){

    elementlist<- list()

    for (i in 1:ncol(EntityDictionary)){
      if(is.na(EntityDictionary[h,i])==TRUE)
      {next}
      else if(colnames(EntityDictionary[h,i]) %in% c("allowNull","isCaseSensitive") & EntityDictionary[h,i]=="true")
      {entityelement<- paste('\"',colnames(EntityDictionary[i]), '"', ':true', sep="")}
      else if(colnames(EntityDictionary[h,i])%in% c("allowNull","isCaseSensitive") & EntityDictionary[h,i]=="false")
      {entityelement<- paste('\"',colnames(EntityDictionary[i]), '"', ':false', sep="")}
      else{entityelement<- paste('\"',colnames(EntityDictionary[i]), '"', ':\"',EntityDictionary[h,i], '"', sep="")}

      m<-length(elementlist)+1
      elementlist[[m]] <- entityelement
    }

    elementstring <- paste0("{",paste0(elementlist, collapse = ","),"}")

    entitylist[[h]]<-elementstring

  }

  entitystring<- paste0(entitylist, collapse = ",")



  ## Create final entity string
  entitystring<- paste0("\"","entity","\":[{\"","entityId","\":\"",entityId,"\",\"","attribute","\":[",entitystring,"]}]")


  # Create domain string

  ## REFERENCE
  ## {\"domainId\":\"f37eeba4-648b-4f7c-9311-e5a007b41ae1\",\"codeName\":\"FatScore\",\"domainItem\":[
  ## 	{\"name\":\"0\",\"value\":\"0\",\"definition\":\"No Fat\"},
  ## 	{\"name\":\"1\",\"value\":\"1\"},
  ## 	{\"name\":\"3\",\"value\":\"3\"},
  ## 	{\"name\":\"2\",\"value\":\"2\"},
  ## 	{\"name\":\"4\",\"value\":\"4\"},
  ## 	{\"name\":\"6\",\"value\":\"6\"},
  ## 	{\"name\":\"5\",\"value\":\"5\"},
  ## 	{\"name\":\"7\",\"value\":\"7\"}],\"description\":\"State where banded\"},


  ## Create domain data frames for loop
  DomainDictionaryColname<- Data.Dictionary %>%
    select(c("domainId","codeName","domainItem_name","domainItem_value","definition")) %>%
    filter(domainItem_value=="colname" & !is.na(domainId))

  DomainDictionaryItem <- Data.Dictionary %>%
    select(c("codeName","domainItem_name","domainItem_value","definition")) %>%
    filter(domainItem_value!="colname")

  ## Create empty lists for loop
  elementlist<- list()
  subelementlist<- list()
  domainlist<- list()
  domainstring<- list()
  domainelement=NULL

  ## Loop to create domain list
  for (i in 1:nrow(DomainDictionaryColname)){
    elementlist1<- list()
    elementlist2<- list()
    subelementlist<- list()

    ### Create start of individual domain string
    if(is.na(DomainDictionaryColname$codeName[i])==TRUE){next}else{
      domainelement<- paste0('\"','domainId', '"', ':\"',DomainDictionaryColname$domainId[i], '"', sep="")}

    m<-length(elementlist1)+1
    elementlist1[[m]] <- domainelement

    if(is.na(DomainDictionaryColname$codeName[i])==TRUE){next}else{
      domainelement<- paste0('\"','codeName', '"', ':\"',DomainDictionaryColname$codeName[i], '"', sep="")}

    elementlist1[[m+1]] <- domainelement

    elementstring1 <- paste0(paste0(elementlist1, collapse = ","))


    ### Create item list for the individual domain
    subelementlist<-list()

    for (k in 1:nrow(DomainDictionaryItem)){

      if(DomainDictionaryItem$codeName[k]!=DomainDictionaryColname$codeName[i]){next}else{
        subdomainelement<- paste0('{\"','name', '"', ':\"', DomainDictionaryItem$domainItem_name[k],'",',
                                  '\"','value', '"', ':\"', DomainDictionaryItem$domainItem_value[k],'",',
                                  '\"','definition', '"', ':\"', DomainDictionaryItem$definition[k],'"}', sep="")}

      n<-length(subelementlist)+1
      subelementlist[[n]] <- subdomainelement
    }

    # Create item string for the individual domain
    subelementstring <- paste0('\"','domainItem','\":',"[",paste0(subelementlist, collapse = ","),"]")


    ### Create end of individual domain string
    elementlist2<-list()

    if(is.na(DomainDictionaryColname$codeName[i])==TRUE){next}else{
      domainelement2<- paste0('\"','description', '"', ':\"',DomainDictionaryColname$definition[i], '"', sep="")}

    m<-length(elementlist2)+1
    elementlist2[[m]] <- domainelement2

    elementstring2 <- paste0(paste0(elementlist2, collapse = ","))


    templist<-list(elementstring1,subelementstring,elementstring2)

    elementstring<-paste0("{",paste0(templist, collapse = ","),"}")



    domainlist[[i]]<-elementstring

  }

  ## Create final domain string
  domainstring<-paste0("\"","domain","\":[",paste0(domainlist, collapse = ","),"]}}")



  # write mdJSON dictionary

  ## REFERENCE
  ## {"data":[{"id":"dl8o5d0h",
  ## "attributes":{"profile":"org.adiwg.profile.full",
  ## "json":"{\"dictionaryId\":\"e2218d0f-856d-4d0f-88a8-27ae0035222c\",
  ## \"dataDictionary\":{
  ## \"citation\":{\"title\":\"Banding Test\",
  ## \"date\":[{\"date\":\"2022-01-26T06:24:10.297Z\",\"dateType\":\"creation\"}]},
  ## \"subject\":[\"dataDictionary\"],


  ## Update id in blank json
  blankjson[[1]][[id]]
  blankjson$data[[1]]$id<- id

  ## Update date in blank json
  blankjson$data[[1]]$attributes$`date-updated` <- paste0(date)

  ## Combine json attribute sections: start, entry, and domain

  startstring<- paste0('{\"dictionaryId\":\"',
                       dictionaryId,'\",\"dataDictionary\":{\"citation\":{\"title\":\"',
                       title,'\",\"date\":[{\"date\":\"',
                       date,'\",\"dateType\":\"creation\"}]},\"subject\":[\"dataDictionary\"]')



  templist<-list(startstring,entitystring,domainstring)

  attributesjson<-paste0(templist, collapse = ",")

  ## Update json attributes string in blank json
  blankjson$data[[1]]$attributes$json <- paste0(templist, collapse = ",")

  assign("newjson",blankjson)

}



