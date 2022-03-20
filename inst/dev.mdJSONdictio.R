
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


# e.g.dictionary<- e.g.dictionary %>%
#   rename(
#     newname2 = notes,
#     newname1 = codeName
#   )
#
# data<-e.g.dictionary

#Run function to create an R object
newjson<- build.mdJSON(data = e.g.dictionary, title = "Example Dictionary")

#Convert R object to JSON
e.g.dictionary = rjson::toJSON(newjson)

#Export JSON
setwd("~/Desktop")
write(e.g.dictionary, "e.g.dictionary.json")


