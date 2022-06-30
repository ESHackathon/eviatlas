#' A wrapper function to run Shiny Apps from \code{eviatlas}.
#' 
#' Running this function will launch the eviatlas shiny
#' @return eviatlas shiny app
#' @param app eviatlas 
#' @import shiny
#' @import shinyBS
#' @import DT
#' @import RColorBrewer
#' @import htmltools
#' @import htmlwidgets
#' @import leaflet.providers
#' @import mapview
#' @import shinyWidgets
#' @import shinydashboard
#' @import webshot
#' 
#' @export

runShiny <- function(app="eviatlas"){
  
  # find and launch the app
  appDir <- system.file("shiny-examples", app, package = "eviatlas")
  
  shiny::runApp(appDir, display.mode = "normal")
}