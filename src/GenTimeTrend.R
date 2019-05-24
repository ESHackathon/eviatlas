#' Create Histogram from dataset

GenTimeTrend = function(idata, hist_col, axis_txt_lim = 60){
  UseMethod("GenTimeTrend", object = idata[hist_col][[1]])
}

GenTimeTrend.default <- function(idata, hist_col, axis_txt_lim = 60){

  ggplot2::ggplot(idata, aes_string(x = hist_col)) +
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
}

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
