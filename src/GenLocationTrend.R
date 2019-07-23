#' Create bar plot with distribution of studies over the region from lat/long info
#'
#' Created For	  : ES Hackathon 2018
#' @param df Input dataframe
#' @param location_column Column with location information (preferably country-level or higher)
#' @return Returns a bar plot object showing counts of literature in systematic review for each location
#'
#' @author Sanita Dhaubanjar
#'
#' @keywords SystematicReview
#'
#' @export

GenLocationTrend = function(df, location_column, axis_txt_lim = 60){

  # if  df is a shapefile, remove geometry column
  if (any(class(df) == 'sf')) {df <- sf::st_drop_geometry(df)}
  
  # Count per locations --------
  location_counts <- as.data.frame(table(df[location_column])) # table() tabulates frequency
  colnames(location_counts)<-c(location_column, "n")

  # Plot bar chart
  locmp<- ggplot2::ggplot(location_counts, aes_string(x=colnames(location_counts[1]),
                                             y=colnames(location_counts[2]),
                                             label = colnames(location_counts[2]))) +
    ggplot2::geom_bar(alpha=0.9, stat="identity",fill="light green") +
    ggplot2::scale_x_discrete(labels = function(x) substr(x, 1, axis_txt_lim)) +
    ggplot2::geom_text(aes(), size = 3, nudge_y = 10) +
    ggplot2::labs(y="# Studies") + 
    ggplot2::ggtitle(paste(location_column, "frequency")) +
    ggplot2::theme_bw()+
    ggplot2::theme(axis.line = element_line(colour = "black"),
          panel.background = element_blank(),
          plot.title = element_text(hjust = .5),
          text = ggplot2::element_text(size=14)
          )

  # Rotate xaxis label if too many
  if (nrow(location_counts)>15){
    locmp <- locmp + ggplot2::theme(axis.text.x=element_text(angle=45,hjust=0.95, size = 11))
  }

  locmp
}
