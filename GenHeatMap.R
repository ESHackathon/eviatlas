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

GenHeatMap = function(idata, selcols, verbose = FALSE){
  if(length(selcols)!=2) stop("Only two variables should be input. Your second argument should be a vector of two column names.")

  # Check which variable has the most unique entries. This is the x-axis.
  len_var1 <- nrow(unique(idata[selcols[1]]))
  len_var2 <- nrow(unique(idata[selcols[2]]))
  if(len_var1 > len_var2) selcols <- selcols[c(2,1)]

  # Convert columns to factors to allow for categorical classification for both numeric and character data -------
  tmp <- as.data.frame(sapply(idata[selcols], function(x) as.factor(x)))


  if(verbose) {
    message("GenHeatMap: Counting of var combination completed.")
  }


  # Plot Heatmap ------
  heatmp <- tmp %>%
            dplyr::rename(listone=colnames(tmp[1]), listtwo=colnames(tmp[2]))%>%
            dplyr::count(listone, listtwo) %>%
            tidyr::complete(listone, listtwo, fill = list(n = 0)) %>%
            ggplot2::ggplot(aes(x = listone, y = listtwo, fill= n, label= n)) +
            ggplot2::geom_tile(alpha=0.3) +
            ggplot2::geom_text() +
            viridis::scale_fill_viridis() +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text = ggplot2::element_text(angle = 45, vjust = 1))+
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
            ggplot2::xlab(paste0(selcols[1])) +
            ggplot2::ylab(paste0(selcols[2])) +
            ggplot2::ggtitle("Study Heatmap", subtitle = paste(selcols[2], "by", selcols[1]))


  heatmp
}
