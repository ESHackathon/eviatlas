#functions to determine if columns should be considered for use in a histogram

histogram_viable_columns <- function(df) {
  colnames(df %>%
             select_if(function(x) n_distinct(x) < sqrt(nrow(df))))
}

