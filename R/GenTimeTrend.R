#' Create Histogram from dataset
#' timetrend
#' Created For	  : ES Hackathon 2018
#' @param idata Input dataframe
#' @param hist_col Column to be mapped to histogram
#' @param axis_txt_lim Numeric limit of number of characters in labels
#' @return Returns a histogram
#' @author Sanita Dhaubanjar
#' @keywords SystematicReview
#' @export

GenTimeTrend = function(idata, hist_col, axis_txt_lim = 60){
  UseMethod("GenTimeTrend", object = idata[hist_col][[1]])
}

GenTimeTrend.default <- function(idata, hist_col, axis_txt_lim = 60){

  hist_col=as.numeric(hist_col)
  gttmp <- ggplot2::ggplot(idata, aes_string(x = hist_col)) +
    ggplot2::geom_bar(
      alpha = 0.9,
      stat = "count",
      fill = "dodger blue"
    ) +
    ggplot2::labs(y = "Studies") +
    ggplot2::scale_x_discrete(name = paste(hist_col), labels = function(x) substr(x, 1, axis_txt_lim)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black"),
      panel.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = .5),
      text = ggplot2::element_text(size = 13)
    )
  
  # Rotate xaxis label if too many categories
  if (dplyr::n_distinct(idata[hist_col]) > 15){
    gttmp <- gttmp + ggplot2::theme(
      axis.text.x = element_text(angle = 40, hjust = 0.95, size = 12))
  }
  gttmp
}

#' Create Histogram from dataset
#' Numeric timetrend
#' Created For	  : ES Hackathon 2018
#' @param idata Input dataframe
#' @param hist_col Column to be mapped to histogram
#' @param axis_txt_lim Numeric limit of number of characters in labels
#' @return Returns a histogram
#' @author Sanita Dhaubanjar
#' @keywords SystematicReview
#' @export

GenTimeTrend.numeric <- function(idata, hist_col, axis_txt_lim = 60){
  
  ggplot2::ggplot(idata, aes_string(x = hist_col)) +
    ggplot2::geom_bar(
      alpha = 0.9,
      stat = "count",
      fill = "dodger blue"
    ) +
    ggplot2::labs(y = "Studies") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black"),
      panel.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = .5),
      text = ggplot2::element_text(size = 13)
    )
}
