
# Reload package with local changes
setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/GitHub/mdJSONdictio")

remove.packages("mdJSONdictio")
.rs.restartR()

remove.packages(c("rlang", "purrr", "stringr"))
install.packages(c("rlang", "purrr", "stringr"))


remove.packages(
  c(
    "readxl",
    "rjson",
    "stats",
    "tibble",
    "uuid",
    "dplyr",
    "plyr",
    "glue",
    "cli",
    "vctrs",
    "fansi",
    "Rcpp",
    "stringi"
  )
)


#### Update ####
library(devtools)
library(roxygen2)
library(pkgdown)
library(bookdown)

document()
load_all()
install()


# Write markdown
rmarkdown::render('vignettes/01_Intro_mdJSONdictio.Rmd')
rmarkdown::render('vignettes/02_Dictionary_Template.Rmd')
rmarkdown::render('vignettes/03_Setup_mdJSONdictio.Rmd')
rmarkdown::render('vignettes/04_mdJSONdictio_Tutorial.Rmd')
rmarkdown::render('vignettes/05_Next_Steps_mdEditor.Rmd')

# Update site
pkgdown::deploy_to_branch(
  clean = TRUE,
  branch = "gh-pages",
  remote = "origin",
  github_pages = (branch == "gh-pages"),
  subdir = NULL,
  examples = FALSE
)

pkgdown::build_site_github_pages(examples = FALSE)
pkgdown::build_site(examples = FALSE)

# Update token
gitcreds::gitcreds_set()

# Add...
usethis::use_version("patch")
save(datatype.rules, file = "data/datatype.rules.rda")
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
install.packages("devtools")
install.packages("shiny")

remove.packages("mdJSONdictio")

options(download.file.method = "curl")
options(download.file.method = "wininet")

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

# Import tabular data dictionary as data frame
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
input.table<-readxl::read_excel(path = path)

# Translate data frame to list
new.list<- mdJSONdictio::build.mdJSON(x = input.table, title = "Example Dictionary")

# Convert list to JSON
new.json = rjson::toJSON(x = new.list)

# Export JSON to disk
write(x = new.json, file = "e.g.dictionary.json")


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
# Import mdJSON data dictionary as list
path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
input.json <- rjson::fromJSON(file = path)

# Translate list to data frame
new.table<- mdJSONdictio::build.table(x = input.json, dictionary_num = 1, entity_num = 1)

# Export table to disk
write.csv(x = new.table, file = "e.g.dictionary2.csv", na="", row.names = FALSE)




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


# validate.table()

## Standard run ##
setwd("~/Desktop")
setwd("C:/Users/hvincelette/Desktop")

library(mdJSONdictio)

# Import tabular data dictionary as data frame
path<-system.file("extdata", "e.g.dictionary.xlsx", package = "mdJSONdictio")
input.dict<-readxl::read_excel(path = path)

# Import dataset as data frame
path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
input.data<-read.csv(path, na.strings = "", stringsAsFactors = FALSE)

# Validate dataset against dictionary
all.warnings<- validate.table(input.dict, input.data)

# Export table to disk
write.csv(x = all.warnings2, file = "e.g.warnings.csv")


## Load local files
path<-"inst/extdata/e.g.dictionary.xlsx"
input.dict<-readxl::read_excel(path = path)

path<-"inst/extdata/e.g.dataset.csv"
input.data<-read.csv(path, na.strings = "", stringsAsFactors = FALSE)

## Update data type rules
datatype.rules <-
  read.csv("inst/extdata/datatype.rules.csv", na.strings = "")
save(datatype.rules, file = "data/datatype.rules.rda")

all.warnings<- validate.table(input.dict, input.data)



# validate.mdJSON()

## Standard run ##
setwd("~/Desktop")
setwd("C:/Users/hvincelette/Desktop")

library(mdJSONdictio)

# Import mdJSON data dictionary as list
path<-system.file("extdata", "e.g.dictionary2.json", package = "mdJSONdictio")
input.dict <- rjson::fromJSON(file = path)

# Import dataset as data frame
path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
input.data<-read.csv(path, na.strings = "", stringsAsFactors = FALSE)

# Validate dataset against dictionary
all.warnings<- validate.mdJSON(input.dict, input.data, dictionary_num = 1, entity_num = 1)

# Export table to disk
write.csv(x = all.warnings, file = "e.g.warnings.csv")



# Test dictionaries

# Import dataset as data frame
path<-system.file("extdata", "e.g.dataset.csv", package = "mdJSONdictio")
input.data<-read.csv(path, na.strings = "", stringsAsFactors = FALSE)


library(mdJSONdictio)

setwd("C:/Users/hvincelette/OneDrive - DOI/Data_management/mdJSONdictio/test_dictionaries")
path = "C:/Users/hvincelette/OneDrive - DOI/Data_management/mdJSONdictio/test_dictionaries"
files <- list.files(path = path, pattern = ".json")

test.json <- NA

for (a in 20:length(files)) {
  test.json <- rjson::fromJSON(file = files[a])
  test.json <- validate.mdJSON(x = test.json,y=input.data, 1, 1)
  name <- gsub("\\.json$", "", paste0(files[a]))
  write.csv(test.json,paste0("validate_",name,".csv"), na="",row.names = FALSE)
  print(files[a])
}

test.json <- fromJSON(file = files[10])
newtable <- build.table(test.json)



# Find multibyte characters

find_offending_character <- function(x, maxStringLength=256){
  print(x)
  for (c in 1:maxStringLength){
    offendingChar <- substr(x,c,c)
    #print(offendingChar) #uncomment if you want the indiv characters printed
    #the next character is the offending multibyte Character
  }
}

string_vector <-input.data$Notes

lapply(string_vector, find_offending_character)

#############################################################################

# markobj <- c('---',
#              'title: "test"',
#              'output: html_document',
#              '---',
#              '',
#              '## R Markdown',
#              '',
#              'This is an R Markdown document.',
#              '```{r}',
#              'print("Test Message")',
#              '```')
#
# markdown::markdownToHTML(text = knitr::knit(text = markobj), output = 'test.html')
#
#
# input.dict[is.na(input.dict)] <- ""
# input.data[is.na(input.data)] <- ""
#
# parsedate::parse_iso_8601("2013-02-08 09")
#
#


# RDatatype: time

ISO.time <- function(x,
                     time.format = c("%H")) {
  tryCatch(
    !is.na(as.Date(paste0("0",x), time.format)),
    error = function(err) {
      FALSE
    }
  )
}

is.ISO.time<-function(x){
  TRUE %in% ISO.time(x)
}

sapply(c("13:45","745","20100607", "74535.5","074535.5","2010-06-07"),is.ISO.time)


## Find problematic values

for(a in 1:nrow(data.NA)){
  if(is.ISO.datetime(data.NA[a, "Date2"])==FALSE &
     is.na(data.NA[a, "Date2"])==FALSE){
    print(data.NA[a, "Date2"])
  }
}

for(a in 1:nrow(data.NA)){
  if(is.other.datetime(data.NA[a, "Date2"])==TRUE &
     is.na(data.NA[a, "Date2"])==FALSE){
    print(data.NA[a, "Date2"])
  }
}

for(a in 1:nrow(data.NA)){
  if(is.ISO.time(data.NA[a, "CaptureTime"])==FALSE &
     is.na(data.NA[a, "CaptureTime"])==FALSE){
    print(data.NA[a, "CaptureTime"])
  }
}


TRUE %in% is.ISO.datetime('2020-02-08')
TRUE %in% is.ISO.datetime('2020-W06-5')
TRUE %in% is.ISO.datetime('2020-039')
TRUE %in% is.ISO.datetime('20200208')
TRUE %in% is.ISO.datetime('2020W065')
TRUE %in% is.ISO.datetime('2020W06')
TRUE %in% is.ISO.datetime('2020039')
TRUE %in% is.ISO.datetime('2020-02-08T09')
TRUE %in% is.ISO.datetime('2020-02-08 09')
TRUE %in% is.ISO.datetime('2020-02-08 09:30')
TRUE %in% is.ISO.datetime('2020-02-08 09:30:26')
TRUE %in% is.ISO.datetime('2020-02-08 09:30:26.123')
TRUE %in% is.ISO.datetime('20200208T080910,123')
TRUE %in% is.ISO.datetime('20200208T080910.123')
TRUE %in% is.ISO.datetime('20200208T080910')
TRUE %in% is.ISO.datetime('20200208T0809')
TRUE %in% is.ISO.datetime('20200208T08')
TRUE %in% is.ISO.datetime('2020-W06-5 09')
TRUE %in% is.ISO.datetime('2020-039 09')
TRUE %in% is.ISO.datetime('2020-02-08 09+07:00')
TRUE %in% is.ISO.datetime('2020-02-08 09-0100')
TRUE %in% is.ISO.datetime('2020-02-08 09Z')
TRUE %in% is.ISO.datetime('7:45')


TRUE %in% is.ISO.time('09')
TRUE %in% is.ISO.time('9')
TRUE %in% is.ISO.time('09Z')
TRUE %in% is.ISO.time('09+07:00')
TRUE %in% is.ISO.time('09-0100')
TRUE %in% is.ISO.time('09:30')
TRUE %in% is.ISO.time('09:30:26')
TRUE %in% is.ISO.time('09:30:26.123')
TRUE %in% is.ISO.time('080910,123')
TRUE %in% is.ISO.time('080910.123')

