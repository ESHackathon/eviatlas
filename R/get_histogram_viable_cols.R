#' Get columns viable for histogram
#' functions to determine if columns should be considered for use in a histogram.
#' very generous limit of 100.
#' @keywords internal

get_histogram_viable_columns <- function(df) {
  df %>%
    dplyr::select_if(function(x) dplyr::n_distinct(x) < 100) %>%
    colnames()
}
