#' Create Histogram from dataset
#'
#' Created For	  : ES Hackathon 2018
#' @param idata Input data frame
#' @param hist_col string with column name
#' @return Returns an object showing number of literature under different categories in user specified \code{year_column}
#'
#' @author Ezgi Tanriver-Ayder
#'
#' @keywords SystematicReview
#' @export

GenTimeTrend = function(idata, hist_col, axis_txt_lim = 60){

  ggplot2::ggplot(eviatlas_pilotdata, aes_string(x = hist_col)) +
    ggplot2::geom_bar(
      alpha = 0.9,
      stat = "count",
      fill = "dodger blue"
    ) +
    ggplot2::labs(y = "Studies") +
    # ggplot2::ggtitle("") +
    ggplot2::scale_x_discrete(name = paste(hist_col), labels = function(x) substr(x, 1, axis_txt_lim)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.line = ggplot2::element_line(colour = "black"),
      panel.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = .5),
      text = ggplot2::element_text(size = 13),
      axis.text.x = ggplot2::element_text(angle = 50, size = 11, hjust=1) 
    )
}
