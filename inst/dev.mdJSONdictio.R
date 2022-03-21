
#### UPDATE ####

library(mdJSONdictio)
library(devtools)
library(roxygen2)
library(pkgdown)

# Add data
setwd("~/Documents/GitHub/mdJSONdictio/")
use_data(blankjson)

# Ignore files
setwd("~/Documents/GitHub/mdJSONdictio/R")
usethis::use_build_ignore("dev.mdJSONdictio.R", escape = FALSE)

# Write html markdown
setwd("~/Documents/GitHub/mdJSONdictio/vignettes/")
rmarkdown::render('mdJSONdictio.Rmd')

# Update site
setwd("~/Documents/GitHub/mdJSONdictio/")
# pkgdown::build_site()
deploy_to_branch()

# Reload package with changes
setwd("~/Documents/GitHub/mdJSONdictio/")
document()
load_all()
install()

# Create package zip
build()


# Install package from GitHub
install_github("hdvincelette/mdJSONdictio")

# View package info
?mdJSONdictio::build.mdJSON


#### TEST ####

library(mdJSONdictio)
setwd("~/Desktop")

#Import tabular dictionary
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
e.g.dictionary<-readxl::read_excel(path)


#Run function to create an R object
newjson<- build.mdJSON(data = e.g.dictionary, title = "Example Dictionary")

#Convert R object to JSON
e.g.dictionary = rjson::toJSON(newjson)

#Export JSON
setwd("~/Desktop")
write(e.g.dictionary, "e.g.dictionary.json")



e.g.dictionary<- e.g.dictionary %>%
  rename(
    newname2 = notes,
    newname1 = codeName
  )

e.g.dictionary$domainItem_name[c(1,3)]<-NA


Data.Dictionary<-e.g.dictionary

for( a in 1:ncol(Data.Dictionary)){
  if(!colnames(Data.Dictionary[a]) %in% c("codeName","domainItem_name","domainItem_value",
                                          "definition","dataType","allowNull","units",
                                          "unitsResolution","minValue","maxValue",
                                          "isCaseSensitive","notes")) stop('Data frame contains an invalid column: ', paste0(colnames(Data.Dictionary[a])), '.\n  Print `?mdJSONdictio::build.mdJSON` for more information on data frame requirements.')
  for( aa in 1:nrow(Data.Dictionary)){
    if(Data.Dictionary[a]) %in% c("codeName","domainItem_name","domainItem_value",
                                  "definition") &
      is.na(Data.Dictionary[aa,a]) == TRUE)stop('Required field incomplete: ', paste0(Data.Dictionary[aa,a]), '.\n  View `?mdJSONdictio::build.mdJSON` for more information on data frame requirements.')



  }

}

