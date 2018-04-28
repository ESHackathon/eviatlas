#------------------------------------
#' Simple R code to create bar plot with distribution of studies over the region from lat/long info
#' Created For	  : ES Hackathon 2018
#'
#' @param idata Input dataframe
#' @param idxlonlat Input data frame with two columns for longitude and latitude in degrees
#' @param selcolidx Index for column with location information
#' @param verbose Logical variable (default=FALSE) for displaying messages on console or not
#' @return Returns a bar plot object showing number of literature in different locations
#'
#' @author Sanita Dhaubanjar
#' @examples
#'
#=========================

GenLocationTrend = function(idata,idxlonlat, selcolidx, verbose = FALSE){

  # # Inputs -------
  # data("pilotdata")
  # idata<-pilotdata
  # idxlonlat<- c(17,16)
  # selcolidx<-10     #"Country"

  selcol<-colnames(idata[selcolidx])

  # Count per locations --------
  seldata <- as.data.frame(table(idata[selcolidx]) )
  colnames(seldata)<-c(selcol, "n")

  # Plot bar chart
  locmp<- ggplot(seldata, aes_string(x=colnames(seldata[1]),y=colnames(seldata[2]))) +
    geom_bar(alpha=0.9, stat="identity",fill="light green") +
    #geom_text(aes( label = colnames(seldata[2]))) + #fill = colnames(seldata[3]),
    labs(y="No of studies")+ ggtitle("Distribution of studies over regions")+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),panel.background = element_blank(), plot.title = element_text(hjust = .5))

  # Rotate xaxis label if too many
  if (nrow(seldata)>15){
    locmp <- locmp + theme(axis.text.x=element_text(angle=45,hjust=0.95))
  }

  if (verbose) {
    message("GenLocationTrend: Location trend plot created!")
  }

  locmp
  return(locmp)
}
