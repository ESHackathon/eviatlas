# Import data -------
ifile <- file.path(getwd(),"exploration","NarrativeTable_GoogleMap.csv")
idata <- as.data.frame(read.csv(file=ifile,skip=0,blank.lines.skip=TRUE))
selcols <- c("Extracted.by", "Appraised.by")

myplot<-GenHeatMap(idata, selcols)

#Create plot
myplot
