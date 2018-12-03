## global.R ##
library(dplyr)
library(devtools)
library(leaflet)

## Eventually load packages from here!
check.packages <- function(pkg, install_dev_version=FALSE){
  if (install_dev_version) {
    pkg_name <- gsub('.*/', '', pkg)
  }
  else {
    pkg_name <- pkg
  }
  print(pkg_name)
  new.pkg <- pkg_name[!(pkg_name %in% installed.packages()[, "Package"])]
  if (length(new.pkg) && !install_dev_version)
    install.packages(new.pkg, dependencies = TRUE)
  if (length(new.pkg) && install_dev_version) {
    check.packages('devtools')
    devtools::install_github(pkg)

  }
  sapply(pkg_name, require, character.only=TRUE)
}

check.packages('ESHackathon/eviatlas', install_dev_version = TRUE)

#load sample data set
pilotdata <- eviatlas::pilotdata
