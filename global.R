## global.R ##

# load packages
library(dplyr)
library(DT)
library(ggplot2)
library(leaflet)
library(htmltools)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# suspect these packages are redundant with the above
# i.e. aren't necessary for running the app
# library(devtools)
# library(tidyverse)

## Eventually load packages from here!
# check.packages <- function(pkg, install_dev_version = FALSE){
#   if (install_dev_version) {
#     pkg_name <- gsub('.*/', '', pkg)
#   }
#   else {
#     pkg_name <- pkg
#   }
#   print(pkg_name)
#   new.pkg <- pkg_name[!(pkg_name %in% installed.packages()[, "Package"])]
#   if (length(new.pkg) && !install_dev_version)
#     install.packages(new.pkg, dependencies = TRUE)
#   if (length(new.pkg) && install_dev_version) {
#     check.packages('devtools')
#     devtools::install_github(pkg)
#
#   }
#   sapply(pkg_name, require, character.only=TRUE)
# }
#
# check.packages('ESHackathon/eviatlas', install_dev_version = TRUE) #devtools::install_github('ESHackathon/eviatlas')

# I've removed this function because it's not needed to run on shinyapps.io;
# but also because there is some question as to whether you should install on
# other users computers by default. Error messages should be informative enough
# that they can solve this problem themselves.