
#### Update ####

library(devtools)
library(roxygen2)
library(pkgdown)
library(mdJSONdictio)

update_packages()

setwd("~/Documents/GitHub/mdJSONdictio/")
setwd("~/GitHub/mdJSONdictio/")


# Add data
use_data()

# Ignore files
usethis::use_build_ignore()

# Add packages
usethis::use_package("qdap","Depends")

# Write html markdown
rmarkdown::render('vignettes/Intro_mdJSONdictio.Rmd')

# Update site
# pkgdown::build_site()
deploy_to_branch()

 # Reload package with local changes
document()
load_all()
install()

exists("build.table", where=globalenv(), inherits = TRUE)

# Create package zip
build()


# Install package from GitHub
remove.packages("mdJSONdictio")
devtools::install_github("hdvincelette/mdJSONdictio",build_vignettes= TRUE)

# build_vignettes= TRUE

# Check which packages used in function
rfile <- file.choose()
NCmisc::list.functions.in.file(rfile)


# View package info
library(mdJSONdictio)
??mdJSONdictio
help.search("mdJSONdictio")
?mdJSONdictio::build.table
?mdJSONdictio::build.mdJSON
vignette("mdJSONdictio")


#### build.mdJSON() ####

## Standard run ##

setwd("~/Desktop")
setwd("C:/Users/hvincelette/Desktop")

library(mdJSONdictio)

# Import tabular dictionary
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")

e.g.dictionary<-readxl::read_excel(path)


# Run function to create an R object
newjson<- build.mdJSON(data = e.g.dictionary, title = "Example Dictionary")

# Convert R object to JSON
e.g.dictionary = rjson::toJSON(newjson)

# Export JSON
write(e.g.dictionary, "e.g.dictionary.json")


## Error check ##

# Import data frame
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
e.g.dictionary<-readxl::read_excel(path)

# missing required column
e.g.dictionary<- e.g.dictionary %>%
  rename(
    newname1 = codeName
  )

# invalid column
e.g.dictionary$newcol1<- NA

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

#### build.table() ####

library(mdJSONdictio)
test<- mdJSONdictio::build.table(fromJSON(
  file = system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
),dictionary_num = 1)

library(mdJSONdictio)
test<- mdJSONdictio::build.table(fromJSON(
  file = "Test.json"
))

# test jsons
library(mdJSONdictio)

setwd("C:/Users/hvincelette/OneDrive - DOI/Data_management")
path <- "C:/Users/hvincelette/OneDrive - DOI/Data_management"

setwd("~/Desktop/test_dictionaries")
path = "~/Desktop/test_dictionaries"
files <- list.files(path = path, pattern = ".json")

test.json <- NA

for (a in 1:length(files)) {
  test.json <- fromJSON(file = files[a])
  test.json <- build.table(test.json)
  name<- paste0(files[a])
  write.csv(test.json,paste0(name,".csv"))
  print(files[a])
}

test.json <- fromJSON(file = files[10])
newtable <- build.table(test.json)

