
#### Update ####
library(devtools)
library(roxygen2)
library(pkgdown)


# Reload package with local changes
setwd("~/GitHub/mdJSONdictio")
setwd("~/Documents/GitHub/mdJSONdictio")

remove.packages("mdJSONdictio")

document()
load_all()
install()


# Write markdown
rmarkdown::render('vignettes/01_Intro_mdJSONdictio.Rmd')
rmarkdown::render('vignettes/02_Dictionary_Template.Rmd')
rmarkdown::render('vignettes/03_Setup_mdJSONdictio.Rmd')
rmarkdown::render('vignettes/04_mdEditor_Reference.Rmd')


# Update site
# pkgdown::build_site()
deploy_to_branch()


# Add...

usethis::use_package("plyr")
usethis::use_build_ignore()

# Check which packages used in function
rfile <- file.choose()
NCmisc::list.functions.in.file(rfile)

# Create package zip
build()

# update all installed packages
update_packages()

# Install package from GitHub
remove.packages("mdJSONdictio")

options(download.file.method = "curl")


devtools::install_github("hdvincelette/mdJSONdictio")
devtools::install_github("hdvincelette/mdJSONdictio", INSTALL_opts = c("--no-multiarch"))
devtools::install_github("hdvincelette/mdJSONdictio", build_vignettes = TRUE)

packages<-c(
  "purrr",
  "readxl",
  "rjson",
  "stats",
  "tibble",
  "stringr",
  "uuid",
  "dplyr",
  "plyr"
)

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

installed_packages <- packages %in% rownames(installed.packages())
if ((all(installed_packages == TRUE))) {
  print("All dependent packages installed")
} else stop (
  "One or more dependent packages did not install\n", toString(packages[installed_packages == FALSE]))


# View package info
library(mdJSONdictio)
?  ? mdJSONdictio
help(package = "mdJSONdictio")
? mdJSONdictio::build.table
? mdJSONdictio::build.mdJSON
vignette("mdJSONdictio")



#### build.mdJSON() ####

## Standard run ##

setwd("~/Desktop")
setwd("C:/Users/hvincelette/Desktop")

library(mdJSONdictio)

# Import tabular dictionary
path <-
  system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")

e.g.dictionary <- readxl::read_excel(path)


# Run function to create an R object
newjson <-
  build.mdJSON(x = e.g.dictionary, title = "Example Dictionary")

# Convert R object to JSON
e.g.dictionary = rjson::toJSON(newjson)

# Export JSON
write(e.g.dictionary, "e.g.dictionary.json")



## Error check ##

# Import data frame
path <-
  system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
e.g.dictionary <- readxl::read_excel(path)

newjson <-
  build.mdJSON(x = e.g.dictionary, title = "Example Dictionary")

# missing required column
e.g.dictionary <- e.g.dictionary %>%
  rename(newname1 = codeName)

# invalid column
e.g.dictionary$newcol1 <- NA

# required field incomplete
e.g.dictionary$codeName[c(30, 45)] <- NA

# conflicting entries
e.g.dictionary$domainItem_name[c(107)] <- "test"

# colname row missing dataType
e.g.dictionary$dataType[c(10)] <- NA

# colname row missing allowNull
e.g.dictionary$allowNull[c(4)] <- NA

# invalid data type
e.g.dictionary$fieldWidth[c(4)] <-"hi"

# invalid data type
e.g.dictionary$unitsResolution[c(10)] <-"bye"


#### build.table() ####

setwd("~/Desktop")
setwd("C:/Users/hvincelette/Desktop")

library(mdJSONdictio)

## Standard run ##
# Import mdJSON data dictionary as an R list
path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
e.g.dictionary2 <- rjson::fromJSON(file = path)

# Transform R list to a data frame
newtable<- build.table(x = e.g.dictionary2, dictionary_num = 1, entity_num = 1)

# Export table to disk
write.csv(newtable, "e.g.dictionary2.csv",na="",row.names = FALSE)





## Error check ##

library(mdJSONdictio)

setwd("C:/Users/hvincelette/OneDrive - DOI/Data_management")
path <- "C:/Users/hvincelette/OneDrive - DOI/Data_management"

setwd("~/Desktop/test_dictionaries")
path = "~/Desktop/test_dictionaries"
files <- list.files(path = path, pattern = ".json")

test.json <- NA

for (a in 1:length(files)) {
  test.json <- fromJSON(file = files[18])
  test.json <- build.table(x = test.json,entity_num = 1)
  name <- gsub("\\.json$", "", paste0(files[a]))
  write.csv(test.json,paste0(name,".csv"), na="",row.names = FALSE)
  print(files[a])
}

test.json <- fromJSON(file = files[10])
newtable <- build.table(test.json)


test <- fromJSON(fromJSON(file = "~/Desktop/mdeditor-20220328-010342.json")[["data"]][[1]][["attributes"]][["json"]])

test2<- build.table(x = test, dictionary_num = 1, entity_num = 1)



