#' Function to determine if columns should be considered for use in a histogram. very generous limit of 100.
#'
#' Created For ES Hackathon 2018
#' @param df Input dataframe
#' @keywords SystematicReview
#'
#' @export

get_histogram_viable_columns <- function(df) {
  colnames(df %>%
             dplyr::select_if(function(x) dplyr::n_distinct(x) < 100))
}

