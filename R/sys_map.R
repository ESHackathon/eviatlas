#' Map the studies
#'
#' Create a geographical map of the studies using `leaflet`.
#'
#' @param studies_data A dataframe of studies.
#' @param latitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @param longitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @export

sys_map <- function(studies_data, latitude, longitude, popup_user=NULL, radius_user=NULL) {
  if (!is.null(popup_user)) {
    popups <- sapply(pilotdata[popup_user], as.character)
  } else {popups <- NULL}

  if (!is.null(radius_user)) {
    radiusby <- sapply(pilotdata[radius_user], as.numeric)
  } else {radiusby <- NULL}

  leaflet::leaflet(studies_data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(lat = ~as.numeric(latitude),
                        lng = ~as.numeric(longitude),
                        popup = ~paste(popups),
                        radius = ~as.numeric(radiusby)
                        # color = ~paste(colorby)
                        # clusterOptions = markerClusterOptions()
                        )
}

