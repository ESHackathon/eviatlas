#' Create bar plot with distribution of studies over the region from lat/long info
#'
#' Created For	  : ES Hackathon 2018
#' @param df Input dataframe
#' @param location_column Column with location information (preferably country-level or higher)
#' @param verbose Logical variable (default=FALSE) for displaying messages on console or not
#' @return Returns a bar plot object showing counts of literature in systematic review for each location
#'
#' @author Sanita Dhaubanjar
#'
#' @keywords SystematicReview
#'
#' @export

GenLocationTrend = function(df, location_column, verbose = FALSE){

  # Count per locations --------
  location_counts <- as.data.frame(table(df[location_column])) # table() tabulates frequency
  colnames(location_counts)<-c(location_column, "n")

  # Plot bar chart
  locmp<- ggplot(location_counts, aes_string(x=colnames(location_counts[1]),
                                             y=colnames(location_counts[2]),
                                             label = colnames(location_counts[2]))) +
    geom_bar(alpha=0.9, stat="identity",fill="light green") +
    geom_text(aes(), size = 3, nudge_y = 10) +
    labs(y="# of studies") + ggtitle("Frequency of studies across locations")+
    theme_bw()+
    theme(axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          plot.title = element_text(hjust = .5),
          text = ggplot2::element_text(size=14),
          )

  # Rotate xaxis label if too many
  if (nrow(location_counts)>15){
    locmp <- locmp + theme(axis.text.x=element_text(angle=45,hjust=0.95, size = 11))
  }

  if (verbose) {
    message("GenLocationTrend: Location trend plot created!")
  }

  locmp
}
