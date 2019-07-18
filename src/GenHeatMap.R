#' Create heatmap from dataset
#'
#' Feed in a dataset and get a heatmap in return
#' Created For	  : ES Hackathon 2018
#'
#' @param idata Input dataframe
#' @param selcols Numeric vector of column indices
#' @param verbose Comments
#'
#' @return Returns a heatmap object showing number of literature under different categories in user specified \code{selcols}
#'
#' @author Ezgi Tanriver-Ayder and Sanita Dhaubanjar
#' @export

GenHeatMap = function(idata, selcols, axis_txt_lim = 60){

  # Convert columns to factors to allow for categorical classification for both numeric and character data -------
  tmp <- as.data.frame(sapply(idata[selcols], function(x) as.factor(x)))
  
  
  # Plot Heatmap ------
  heatmp <- tmp %>%
            dplyr::rename(listone=colnames(tmp[1]), listtwo=colnames(tmp[2]))%>%
            dplyr::count(listone, listtwo) %>%
            tidyr::complete(listone, listtwo, fill = list(n = 0)) %>%
            ggplot2::ggplot(aes(x = listone, y = listtwo, fill= n, label= n)) +
            ggplot2::geom_tile(alpha=0.3, color="grey60") +
            ggplot2::geom_text() +
            viridis::scale_fill_viridis() +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                           panel.grid = element_blank(), 
                           text = ggplot2::element_text(size = 14), 
                           axis.title = ggplot2::element_text(size = 16), 
                           title = ggplot2::element_text(size = 18)) + 
            ggplot2::xlab(paste0(selcols[1])) +
            ggplot2::ylab(paste0(selcols[2])) +
            ggplot2::labs(fill = "Count") +
            # Limit axis text to a certain number of characters, so that long text doesn't ruin the chart display
            ggplot2::scale_x_discrete(labels = function(x) substr(x, 1, axis_txt_lim)) +
            ggplot2::scale_y_discrete(labels = function(x) substr(x, 1, axis_txt_lim)) + 
            ggplot2::ggtitle("Study Heatmap", subtitle = paste(selcols[2], "by", selcols[1]))

  heatmp
}

