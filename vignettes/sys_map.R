## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages------------------------------------------------------------
# Load packages.
library(tidyverse)
library(eviatlas)
library(leaflet) # remove when we get leaflet in as a dependencies.


## ----sys_map example-----------------------------------------------------
pilotdata %>% 
      leaflet() %>%
    addTiles()  %>%
    addMarkers(lat = ~latplot,
               lng =  ~lngplot)


## ------------------------------------------------------------------------


## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

