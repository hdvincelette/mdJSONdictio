

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
usethis::use_package("tibble", "Depends")

# Write html markdown
rmarkdown::render('vignettes/Intro_mdJSONdictio.Rmd')

# Update site
# pkgdown::build_site()
deploy_to_branch()

# Reload package with local changes
document()
load_all()
install()

exists("build.table", where = globalenv(), inherits = TRUE)

# Create package zip
build()


# Install package from GitHub
remove.packages("mdJSONdictio")

devtools::install_github("hdvincelette/mdJSONdictio")
devtools::install_github("hdvincelette/mdJSONdictio", INSTALL_opts = c("--no-multiarch"))
devtools::install_github("hdvincelette/mdJSONdictio", build_vignettes = TRUE)

# build_vignettes= TRUE

# Check which packages used in function
rfile <- file.choose()
NCmisc::list.functions.in.file(rfile)


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


x <- e.g.dictionary

#### build.table() ####

## Standard run ##

library(mdJSONdictio)
test <- mdJSONdictio::build.table(fromJSON(
  file = system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
))


library(mdJSONdictio)
test2 <- mdJSONdictio::build.table(x=fromJSON(file="2Entities.json"), entity_num=2)


## Error check ##

library(mdJSONdictio)

setwd("C:/Users/hvincelette/OneDrive - DOI/Data_management")
path <- "C:/Users/hvincelette/OneDrive - DOI/Data_management"

setwd("~/Desktop/test_dictionaries")
path = "~/Desktop/test_dictionaries"
files <- list.files(path = path, pattern = ".json")

test.json <- NA

for (a in 1:length(files)) {
  test.json <- fromJSON(file = files[a])
  test.json <- build.table(x = test.json)
  name <- gsub("\\.json$", "", paste0(files[a]))
  write.csv(test.json,paste0(name,".csv"), na="",row.names = FALSE)
  print(files[a])
}

test.json <- fromJSON(file = files[10])
newtable <- build.table(test.json)

############################################################################








