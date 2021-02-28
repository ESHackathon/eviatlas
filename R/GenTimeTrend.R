#' Create Histogram from dataset
#' @param idata dataset from eviatlas
#' @param hist_col column used for histogram
#' @param axis_txt_lim character limit for label text in axis
#' @keywords internal

GenTimeTrend <- function(idata, hist_col, axis_txt_lim = 60) {
  # bind variables locally to function
  idata <- hist_col <- axis_txt_lim <- NULL

  UseMethod("GenTimeTrend", object = idata[hist_col][[1]])
}

GenTimeTrend.default <- function(idata, hist_col, axis_txt_lim = 60) {
  gttmp <- ggplot2::ggplot(idata, ggplot2::aes_string(x = hist_col)) +
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
  if (dplyr::n_distinct(idata[hist_col]) > 15) {
    gttmp <- gttmp + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 40, hjust = 0.95, size = 12)
    )
  }
  gttmp
}

GenTimeTrend.numeric <- function(idata, hist_col, axis_txt_lim = 60) {
  ggplot2::ggplot(idata, ggplot2::aes_string(x = hist_col)) +
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
