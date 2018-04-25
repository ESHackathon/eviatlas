#' Create GenTimeTrend from dataset
#'
#' Created For	  : ES Hackathon 2018
#' @param idata Input data frame
#' @param year_column List of strings with column names
#' @return Returns a timetrend object showing number of literature under different categories in user specified \code{year_column}
#'
#' @author Ezgi Tanriver-Ayder 24 April 2018
#'
#' @keywords SystematicReview,
#' @export

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(reshape2)


GenTimeTrend = function(idata, year_column = NULL, verbose = FALSE){

  # Check if Year column exists -------
  if (!length(year_column)) {
    year_column <- c("Year")
  }

  if (length(which(colnames(idata) == year_column))>0){
    message("GenTimeTrend: Year column found")


  # Count combination of vars --------
    seldata<- idata[year_column] %>%
      add_count_(year_column) %>%
      complete_(year_column, fill = list(n=0)) %>% distinct()

  # Plot Heatmap ------
    # Plot bar chart
    iyears<- min(seldata$Year):max(seldata$Year)
    timemp<- ggplot(seldata, aes_string(x=colnames(seldata[1]),y=colnames(seldata[2]))) +
      geom_bar(alpha=0.9, stat="identity",fill="light blue") +
      #geom_text(aes( label = colnames(seldata[2]))) + #fill = colnames(seldata[3]),
      scale_x_continuous(name="Year", breaks = iyears)+
      labs(y="No of studies") + ggtitle("Distribution of studies over time") +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.background = element_blank(),
            plot.title = element_text(hjust = .5))
    timemp
    if (verbose) {
      message("GenTimeTrend: Time trend plot created!")
    }
    timemp

  } else {
    message("GenTimeTrend: Year not found")
  } # if col exists

  return(timemp)
 }
