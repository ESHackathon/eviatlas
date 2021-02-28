#' Create bar plot with distribution of studies over the region from lat/long info
#'
#' Created For	  : ES Hackathon 2018
#' @param df Input dataframe
#' @param location_column Column with location information (preferably country-level or higher)
#' @param axis_txt_lim character limit for label text
#' @return Returns a bar plot object showing counts of literature in systematic review for each location
#'
#' @author Sanita Dhaubanjar
#'
#' @keywords SystematicReview
#'
#' @keywords internal

GenLocationTrend <- function(df, location_column, axis_txt_lim = 60) {

  # if  df is a shapefile, remove geometry column
  if (any(class(df) == "sf")) {
    df <- sf::st_drop_geometry(df)
  }


  # Bind variables locally to function (helps avoid R CMD notes)
  listone <- listtwo <- n <- NULL

  # Count per locations --------
  location_counts <- dplyr::as_tibble(table(df[location_column])) # table() tabulates frequency
  colnames(location_counts) <- c(location_column, "n")

  # Plot bar chart
  locmp <- ggplot2::ggplot(location_counts, ggplot2::aes_string(
    x = colnames(location_counts[1]),
    y = colnames(location_counts[2]),
    label = colnames(location_counts[2])
  )) +
    ggplot2::geom_bar(alpha = 0.9, stat = "identity", fill = "light green") +
    ggplot2::scale_x_discrete(labels = function(x) substr(x, 1, axis_txt_lim)) +
    ggplot2::geom_text(ggplot2::aes(), size = 3, nudge_y = 10) +
    ggplot2::labs(y = "# Studies") +
    ggplot2::ggtitle(paste(location_column, "frequency")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black"),
      panel.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = .5),
      text = ggplot2::element_text(size = 14)
    )

  # Rotate xaxis label if too many
  if (nrow(location_counts) > 15) {
    locmp <- locmp + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 0.95, size = 11)
    )
  }

  locmp
}
