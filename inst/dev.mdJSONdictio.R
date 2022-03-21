
#### Update ####

library(mdJSONdictio)
library(devtools)
library(roxygen2)
library(pkgdown)

# Add data
setwd("~/Documents/GitHub/mdJSONdictio/")
use_data()

# Ignore files
setwd("~/Documents/GitHub/mdJSONdictio/R")
usethis::use_build_ignore()

# Write html markdown
setwd("~/Documents/GitHub/mdJSONdictio/vignettes/")
rmarkdown::render('mdJSONdictio.Rmd')

# Update site
setwd("~/Documents/GitHub/mdJSONdictio/")
# pkgdown::build_site()
deploy_to_branch()

# Reload package with local changes
setwd("~/Documents/GitHub/mdJSONdictio/")
document()
load_all()
install()

# Create package zip
build()


# Install package from GitHub
install_github("hdvincelette/mdJSONdictio", build_vignettes = TRUE)


# View package info
library(mdJSONdictio)
??mdJSONdictio
?mdJSONdictio::build.mdJSON
vignette("mdJSONdictio")


#### Standard run ####

library(mdJSONdictio)
setwd("~/Desktop")

#Import tabular dictionary
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
e.g.dictionary<-readxl::read_excel(path)

test<- read.csv("datatype_definitions.csv")

#Run function to create an R object
newjson<- build.mdJSON(data = test, title = "Example Dictionary")

#Convert R object to JSON
e.g.dictionary = rjson::toJSON(newjson)

#Export JSON
setwd("~/Desktop")
write(e.g.dictionary, "e.g.dictionary.json")


#### Error check ####

# Import data frame
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
e.g.dictionary<-readxl::read_excel(path)

# invalid column
e.g.dictionary<- e.g.dictionary %>%
  rename(
    newname2 = notes,
    newname1 = codeName
  )

# required field incomplete
e.g.dictionary$codeName[c(30,45)]<-NA

# conflicting entries
e.g.dictionary$domainItem_name[c(107)]<-"test"

# colname row missing dataType
e.g.dictionary$dataType[c(10)]<-NA

# colname row missing allowNull
e.g.dictionary$allowNull[c(4)]<-NA


Data.Dictionary<-e.g.dictionary

Required.cols<-c("codeName","domainItem_name","domainItem_value",
       "definition","dataType","allowNull")

if(length(setdiff(Required.cols, names(Data.Dictionary)))!=0) stop('Data frame missing required columns: ',
                                                                   toString(setdiff(Required.cols, names(Data.Dictionary))),
                                                                                '.\n  Print vignette("mdJSONdictio") for more information on data frame requirements.')

for(a in 1:ncol(Data.Dictionary)){
  if(!colnames(Data.Dictionary[a]) %in% c("codeName","domainItem_name","domainItem_value",
                                          "definition","dataType","allowNull","units",
                                          "unitsResolution","minValue","maxValue",
                                          "isCaseSensitive","notes")) stop('Data frame contains an invalid column: ',
                                                                           colnames(Data.Dictionary[a]),
                                                                           '.\n  Print vignette("mdJSONdictio") for more information on data frame requirements.')
  for(aa in 1:nrow(Data.Dictionary)){
    if(colnames(Data.Dictionary[a]) %in% c("codeName","domainItem_name","domainItem_value",
                                           "definition") &
       is.na(Data.Dictionary[aa,a]) == TRUE) stop('Required field incomplete. \n  ',
                                                  colnames(Data.Dictionary[a]),
                                                  '==NA in row ',aa,
                                                  '.\n  Print vignette("mdJSONdictio") for more information on data frame requirements.')
    if(Data.Dictionary$domainItem_name[aa]=="colname" &
       Data.Dictionary$domainItem_value[aa]!="colname") stop('Data frame contains conflicting entries.',
                                                             '\n  Row ',aa,' has "',Data.Dictionary$domainItem_name[aa],
                                                             '" for domainItem_name and "',Data.Dictionary$domainItem_value[aa],
                                                             '" for domainItem_value. \n  Print vignette("mdJSONdictio") for more information on data frame requirements.')
    if(Data.Dictionary$domainItem_name[aa]!="colname" &
       Data.Dictionary$domainItem_value[aa]=="colname") stop('Data frame contains conflicting entries.\n  domainItem_name=="',
                                                             Data.Dictionary$domainItem_name[aa],'" and domainItem_value=="',
                                                             Data.Dictionary$domainItem_value[aa],'" in row ',aa,
                                                             '.\n  Print vignette("mdJSONdictio") for more information on data frame requirements.')

    if(Data.Dictionary$domainItem_name[aa]=="colname" &
       is.na(Data.Dictionary$dataType[aa])) stop('Required field incomplete. \n  dataType==NA in row ',aa,
                                                 '.\n  Print vignette("mdJSONdictio") for more information on data frame requirements.')

    if(Data.Dictionary$domainItem_name[aa]=="colname" &
       is.na(Data.Dictionary$allowNull[aa])) stop('Required field incomplete. \n  allowNull==NA in row ',aa,
                                                  '.\n  Print vignette("mdJSONdictio") for more information on data frame requirements.')


  }
}

