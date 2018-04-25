#------------------------------------
#' Simple R code to create bar plot with distribution of studies over the region from lat/long info
#' Created For	  : ES Hackathon 2018
#'
#' @param idxlonlat Input data frame with two columns for longitude and latitude in degrees
#' @param irgns Character variable with values "Country" or "Continent" to indicate regions used in bar plot
#' @param verbose Logical variable (default=FALSE) for displaying messages on console or not
#' @return Returns a bar plot object showing number of literature in different countries or continents as specified by user in \code{irgn}
#'
#' @author Sanita Dhaubanjar
#' @examples
#' GenHeatMap(mpg, colnames(mpg[c(1,4)]))
#'
#=========================

#GenLocationTrend = function(idata,idxlonlat, irgn, verbose = FALSE){
#library(tidyverse)
#library(ggplot2)

# Inputs -------
ifile <- file.path(getwd(),"data-raw","pilotdata.csv")
data("pilotdata")
idata<-pilotdata
idxlonlat<- c(17,16)
irgn<-"Country"

# Count combination of vars --------
seldata <-mutate(idata, coords2country_continent(idata[idxlonlat]))
seldata2 <-table(seldata[irgn])

idata[selcols] %>%
  add_count_(selcols) %>%
  complete_(selcols, fill = list(n=0)) %>% distinct()

# Plot bar chart
iyears<- min(seldata$Year):max(seldata$Year)
locmp<- ggplot(seldata, aes_string(x=colnames(seldata[1]),y=colnames(seldata[2]))) +
  geom_bar(alpha=0.9, stat="identity",fill="light blue") +
  #geom_text(aes( label = colnames(seldata[2]))) + #fill = colnames(seldata[3]),
  scale_x_continuous(name="Year", breaks = iyears)+
  labs(y="No of studies")+ ggtitle("Distribution of studies over time")+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),panel.background = element_blank(), plot.title = element_text(hjust = .5))
timemp
if (verbose) {
  message("GenTimeTrend: Time trend plot created!")
}
timemp


#return(heatmp)}

