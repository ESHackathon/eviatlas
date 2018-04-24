#' Simple R code to create heatmap from dataset

# Heat Map from SM database
# Created By    : Sanita Dhaubanjar on 23 April 2018
# Created For	  : ES Hackathon 2018
#=========================
#
library(tidyverse)
library(ggplot2)
library(reshape2)

# Import data -------
ifile <- file.path(getwd(),"exploration","NarrativeTable_GoogleMap.csv")
idata <- as.data.frame(read.csv(file=ifile,skip=0,blank.lines.skip=TRUE))

# Select vars to filter by -------
selcols <- c("Extracted.by", "Appraised.by")

# Count combination of vars --------
seldata<-idata[selcols] %>% add_count_(selcols) %>%
  complete_(selcols, fill = list(n=0)) %>% distinct()
message("Counting of var combination completed.")



# Plot Heatmap ------
#ggplot(seldata, aes(x=Extracted.by,y=Appraised.by, fill=n))+geom_raster()
heatmp<- ggplot(seldata, aes_string(x=colnames(seldata[1]),y=colnames(seldata[2]), fill=colnames(seldata[3]))) +
  geom_raster(alpha=0.9) +
  geom_text(aes_string(fill = colnames(seldata[3]), label = colnames(seldata[3]))) +
  theme(axis.line = element_line(colour = "black"),panel.background = element_blank()) +
  scale_fill_gradient(low = "white", high = "light blue",name = "No of studies")
heatmp
message("Heatmap done!")
