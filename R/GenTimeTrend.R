#' Create GenTimeTrend from dataset
#'
#' Created For	  : ES Hackathon 2018
#' @param idata Input data frame
#' @param year_column List of strings with column names
#' @param verbose Realtime commenting
#' @return Returns a timetrend object showing number of literature under different categories in user specified \code{year_column}
#'
#' @author Ezgi Tanriver-Ayder 24 April 2018
#'
#' @keywords SystematicReview,
#' @export
#'
GenTimeTrend = function(idata, year_column = NULL, verbose = FALSE){

  # Check if Year column exists ------
  if(any(colnames(idata) %in% year_column)) {
    message("GenTimeTrend: Year column found")

  tmp <- as.data.frame(sapply(idata[year_column], function(x) as.factor(x)))

    # Count combination of vars --------
    seldata <- table(tmp)
    seldata <- reshape2::melt(seldata)

    # Plot Heatmap ------
    # Plot bar chart
    iyears<- min(seldata$Year):max(seldata$Year)
    timemp<- ggplot2::ggplot(seldata,
                             ggplot2::aes_string(x=colnames(seldata[1]),
                                                 y=colnames(seldata[2]))) +
      ggplot2::geom_bar(alpha=0.9, stat="identity",fill="light blue") +
      ggplot2::scale_x_continuous(name="Year", breaks = iyears)+
      ggplot2::labs(y="No of studies") + ggplot2::ggtitle("Distribution of studies over time") +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                     panel.background = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = .5))

    if(verbose) message("GenTimeTrend: Time trend plot created!")


  } else {
    message("GenTimeTrend: Year not found")
  } # if col exists

  timemp
}
