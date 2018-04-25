#' Create heatmap from dataset
#'
#' Feed in a dataset and get a heatmap in return
#' Created For	  : ES Hackathon 2018
#'
#' @param idata Input data frame
#' @param selcols List of strings with column names
#' @return Returns a heatmap object showing number of literature under different categories in user specified \code{selcols}
#'
#' @author Sanita Dhaubanjar 24 April 2018
#'
#' @examples
#' ## load the data sets
#' GenHeatMap(mpg, colnames(mpg[c(1,4)]))
#'
GenHeatMap = function(idata, selcols, verbose = FALSE){
  library(tidyverse)
  library(ggplot2)
  library(reshape2)

  # Convert columns to factors to allow for categorical classification for both numeric and character data -------
  tmp <- as.data.frame(sapply(idata[selcols], function(x) as.factor(x)))

  # Count combination of vars --------
  seldata<- tmp %>%
    add_count_(selcols) %>%
    complete_(selcols, fill = list(n=0)) %>% distinct()

  if (verbose) {
    message("GenHeatMap: Counting of var combination completed.")
  }

  # Plot Heatmap ------
  #ggplot(seldata, aes(x=Extracted.by,y=Appraised.by, fill=n))+geom_raster()
  heatmp<- ggplot(seldata, aes_string(x=colnames(seldata[1]),y=colnames(seldata[2]), fill=colnames(seldata[3]))) +
    geom_raster(alpha=0.9) +
    geom_text(aes_string( label = colnames(seldata[3]))) + #fill = colnames(seldata[3]),
    theme(axis.line = element_line(colour = "black"),panel.background = element_blank()) +
    scale_fill_gradient(low = "white", high = "light blue",name = "No of studies")
  heatmp
  if (verbose) {
    message("GenHeatMap: Heatmap created!")
  }
  return(heatmp)
}

