#' functions to determine if columns contain what appear to be latitude or longtude columns
#' 
#' Created For ES Hackathon 2018
#'
#' @param df Input dataframe
#' @export

get_latitude_cols <- function(df) {
  n_out <- colnames(df)[1]
  # waiting for a more elegant interpretation, this is quick and dirty...
  if ("Latitude" %in% colnames(df)) { n_out <- "Latitude"
  } else if ("latitude" %in% colnames(df) ) {n_out <- "latitude"
  } else if ("lat" %in% colnames(df) ) {n_out <- "lat"
  } else if ("Plotted.lat." %in% colnames(df) ) {n_out <- "Plotted.lat."
  } else if (length(agrep(pattern = "latitude", x = colnames(df)))) {n_out <- agrep(pattern = "latitude", x = colnames(df), value = T)[1]}
  n_out
}

get_longitude_cols <- function(df) {
  n_out <- colnames(df)[1]
  # waiting for a more elegant interpretation, this is quick and dirty...
  if ("Latitude" %in% colnames(df)) { n_out <- "Longitude"
  } else if ("longitude" %in% colnames(df) ) {n_out <- "longitude"
  } else if ("long" %in% colnames(df) ) {n_out <- "long"
  } else if ("lng" %in% colnames(df) ) {n_out <- "lng"
  } else if ("Plotted.long." %in% colnames(df) ) {n_out <- "Plotted.long."
  } else if (length(agrep(pattern = "longitude", x = colnames(df)))) {n_out <- agrep(pattern = "longitude", x = colnames(df), value = T)[1]}
  n_out
}
