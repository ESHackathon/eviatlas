#functions to determine if columns contain what appear to be links or emails

is_link_col <- function(col) {
  # Takes in a character vector, and returns a boolean value if that column is suspected to
  # contain URLs/URIs or other links.
  any(stringr::str_detect(col, 'http'), na.rm = T) | any(stringr::str_detect(col, 'www'), na.rm = T) | any(stringr::str_detect(col, '@'), na.rm = T)
}

get_link_cols <- function(df) {
  # looks at the first 100 rows of a dataframe, and returns a character vector (column names)
  # that might contain links or emails
  logi_list <- sapply(df[1:100,], is_link_col)
  names(logi_list)[logi_list]
}
