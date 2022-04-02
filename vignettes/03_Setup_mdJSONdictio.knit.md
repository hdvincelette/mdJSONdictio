---
title: "Setup mdJSONdictio"
output: 
  # html_document:
  #   theme: readable
  #   highlight:
  #   fig_caption: true
  knitr:::html_vignette:
    toc: true
  pdf_document:
    highlight: null
    number_sections: yes
vignette: >
  %\VignetteIndexEntry{mdJSONdictio}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
<style type="text/css">
  body{
  font-size: 12pt;
}
</style>
---





The following describes how to setup a working space to use the mdJSONdictio functions, including how to install R, RStudio, and the mdJSONdictio package.
<br />

#### What is R?

["R is a language and environment for statistical computing and graphics." - R Core Team Team](https://www.r-project.org/about.html)
<br />

#### What is RStudio? 

["RStudio is an integrated development environment (IDE) for R." RStudio Team](https://www.rstudio.com/products/rstudio/)
<br />
<br />

# Install R on a DOI Windows PC

*Note: These instructions are catered to USFWS employees, but may be adapted to those with similar operating systems.*

Search "FWS-Apps-to-Go" in the Windows search box.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/FWS_Apps_Search.png)
<br />
<br />

Search “IFW-R” in the Applications search box. Sometimes more than one release of R or RStudio is available in FWS-Apps-to-Go. If it is not clear which is the most recent, reference the current release webpages for [R](https://cran.r-project.org/bin/windows/base/) and [RStudio](https://www.rstudio.com/products/rstudio/download/).
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/Install_R.png)
<br />
<br />

Search "RStudio" in the Windows search box.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/RStudio_Search.png)
<br />
<br />
<br />

# Get to know RStudio

There are four main panes in RStudio: Source (upper left), Console (lower left), Environment, History, Connections, Tutorial (upper right), and Files, Plots, Packages, Help, Viewer (lower right). Expand the Source pane with Ctrl + 1 or by clicking the double window icon in the upper right corner of the pane.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/RStudio_Open.png)
<br />
<br />

The Console pane is where code is executed. Code can be submitted directly to the Console pane or written in "scripts" in the Source pane and sent to the Console pane. Code written in the Source pane can be saved for later, whereas the Console pane only keeps a short-term record of executed code. 
<br />

#### Operation tips

* Code in the Source pane is sent to the Console with Ctrl + Enter or by clicking the Run button.
* The blinking cursor in the Source pane indicates the line of code that will be executed. Chunks of code can be executed by highlighting multiple lines of code.
* Lines of code that start with **#** are "comments" and are not executed in the Console pane.
* A blue **>** at the bottom of the Console pane indicates R finished processing the code.
<br />

For a more thorough instructions on R and RStudio, visit the Tutorial tab in the Environment, History, Connections, Tutorial pane, or refer to the [Hands-On Programming with R website](https://rstudio-education.github.io/hopr/).
<br />
<br />
<br />

# Install mdJSONdictio in R

### Run installation code in RStudio

Paste the following code in the RStudio Source pane and run each line. Run the code chunks separately to ensure each is successfully executed.


```r
# Install devtools from CRAN
install.packages("devtools")
library(devtools)

# Install mdJSONdictio from GitHub
devtools::install_github("hdvincelette/mdJSONdictio")
library(mdJSONdictio)
```
<br />

### Update dependent packages

mdJSONdictio requires other packages to work properly. R may prompt you to update these dependent packages during installation. To update all, type "1" in the Console pane and hit Enter.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/RStudio_Package_Update.png)
<br />
<br />

If a pop-up asks “Do you want to install from sources the packages which need compilation?”, click Yes.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/RStudio_Package_Update_2.png)
<br />
<br />

### Verify mdJSONdictio installation

Re-run the following code to verify mdJSONdictio was successfully installed.


```r
library(mdJSONdictio)
```

If the R simply prints the executed code in the Console pane, then installation was successful. If R returns the Error message “there is no package called ‘mdJSONdictio’,” then installation failed. 
<br />
<br />
<br />

# Troubleshooting tips

### If mdJSONdictio installation fails...

Paste the following code in the RStudio Source pane and run each line.


```r
# Install mdJSONdictio with compiled code only for sub-architecture used by R CMD INSTALL
devtools::install_github("hdvincelette/mdJSONdictio",
                         INSTALL_opts = c("--no-multiarch"))
```
<br />                         
                         
### If mdJSONdictio installation fails again...

Navigate to the mdJSONdictio GitHub repository and download the package zip: Code -> Download ZIP.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/GitHub_Repo.png)
<br />
<br />

In RStudio, set the working directory to the zip location: Session -> Set Working Directory -> Choose Directory.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/RStudio_setwd.png)
<br />
<br />

Paste the following in the RStudio Source pane and run each line of code.


```r
# Install mdJSONdictio from zip source
install.packages("mdJSONdictio-master")
```
<br />
<br />

### If dependent package updates fail…

Uninstall the package in the Files, Plots, Packages, Help Viewer pane: Packages tab -> click the "x" next to the package. When a pop-up asks "Are you sure?", click Yes.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/RStudio_Unattach_Package.png)
<br />
<br />

Restart the R session: Session -> Restart R.
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/RStudio_Restart.png)
<br />
<br />

Reinstall the package: Packages tab -> Install -> type in the package name -> Install
![](https://github.com/hdvincelette/mdJSONdictio/raw/master/man/figures/RStudio_Package_Install.png)