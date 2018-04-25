#' Map the studies
#'
#' Create a geographical map of the studies using `leaflet`.
#'
#' @param studies_data A dataframe of studies.
#' @param latitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @param longitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @export

sys_map <- function(studies_data,
                    latitude,
                    longitude) {
  studies_data %>%
    leaflet() %>%
    addTiles()  %>%
    addMarkers(lat = ~latitude,
               lng = ~longitude)
}
