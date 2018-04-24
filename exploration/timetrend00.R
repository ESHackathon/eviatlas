#------------------------------------
#' Simple R code to create barplot of number of publications over time from dataset
#' Created For	  : ES Hackathon 2018
#' @param idata Input data frame
#' @param selcols List of strings with column names
#' @return Returns a barplot object showing number of literature under different categories in user specified \code{selcols}
#'
#' @author Ezgi Tanriver-Ayder 24 April 2018
#'
#' @examples
#' ## load the data sets
#' GenHeatMap(mpg, colnames(mpg[c(1,4)]))
#'
#GenHeatMap = function(idata, selcols){

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(reshape2)

setwd("C:/Users/ezgit/Desktop/EvidenceHackathon/eviatlas")

# Import data -------
ifile <- file.path(getwd(),"exploration","NarrativeTable_GoogleMap.csv")
idata <- as.data.frame(read.csv(file=ifile,skip=0,blank.lines.skip=TRUE))

# Check if Year column exists -------
selcols <- c("Year")
if (length(which(colnames(idata)==selcols))>0){
  message("GenTimeTrend: Year column found")

  # Count for each year
  seldata<- idata[selcols] %>%
    add_count_(selcols) %>%
    complete_(selcols, fill = list(n=0)) %>% distinct()
  message("GenTimeTrend: Counting of var combination completed.")


  # Plot bar chart
  iyears<- min(seldata$Year):max(seldata$Year)
  timemp<- ggplot(seldata, aes_string(x=colnames(seldata[1]),y=colnames(seldata[2]))) +
    geom_bar(alpha=0.9, stat="identity",fill="light blue") +
    #geom_text(aes( label = colnames(seldata[2]))) + #fill = colnames(seldata[3]),
    scale_x_continuous(name="Year", breaks = iyears)+
    labs(y="No of studies")+ ggtitle("Distribution of studies over time")+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),panel.background = element_blank(), plot.title = element_text(hjust = .5))


  message("GenTimeTrend: Time trend plot created!")
  timemp

} else {
  message("GenTimeTrend: Year not found")
} # if col exists


