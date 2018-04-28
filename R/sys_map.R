#' Map the studies
#'
#' Create a geographical map of the studies using `leaflet`.
#'
#' @param studies_data A dataframe of studies.
#' @param latitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @param longitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @export

sys_map <- function(studies_data, latitude, longitude) {
   leaflet::leaflet(studies_data) %>%
   leaflet::addTiles() %>%
   leaflet::addMarkers(lat = ~as.numeric(latitude), lng = ~as.numeric(longitude))
}
