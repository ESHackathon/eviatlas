## global.R ##
library(dplyr)
library(leaflet)

#load sample data set
pilotdata <- read.csv("https://raw.githubusercontent.com/ESHackathon/eviatlas/master/data-raw/pilotdata.csv")

heatmap_test <- read.csv("https://raw.githubusercontent.com/ESHackathon/eviatlas/master/exploration/NarrativeTable_GoogleMap.csv")

# Heatmap stuff

# Select vars to filter by -------
selcols <- c("Extracted.by", "Appraised.by")

# Count combination of vars --------
seldata<-heatmap_test[selcols] %>% 
  add_count_(selcols) %>%
  complete_(selcols, fill = list(n=0)) %>% 
  distinct()


# Plot Heatmap ------
heatmp <- ggplot(seldata, aes_string(x=colnames(seldata[1]),y=colnames(seldata[2]), fill=colnames(seldata[3]))) +
  geom_raster(alpha=0.9) +
  geom_text(aes_string(fill = colnames(seldata[3]), label = colnames(seldata[3]))) +
  theme(axis.line = element_line(colour = "black"),panel.background = element_blank()) +
  scale_fill_gradient(low = "white", high = "light blue",name = "No of studies") + ggtitle("HEATMAP!!!")