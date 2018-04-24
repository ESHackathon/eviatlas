## global.R ##
library(dplyr)
library(leaflet)

#load sample data set
pilotdata <- read.csv("https://raw.githubusercontent.com/ESHackathon/eviatlas/master/data-raw/pilotdata.csv")

heatmap_test <- read.csv("https://raw.githubusercontent.com/ESHackathon/eviatlas/master/exploration/NarrativeTable_GoogleMap.csv")

# Heatmap stuff
source("https://raw.githubusercontent.com/ESHackathon/eviatlas/master/R/GenHeatMap.R")

