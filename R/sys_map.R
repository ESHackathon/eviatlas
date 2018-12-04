#' Map the studies
#'
#' Create a geographical map of the studies using `leaflet`.
#'
#' @param studies_data A dataframe of studies.
#' @param latitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @param longitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @export

sys_map <- function(studies_data, latitude,
                    longitude, popup_user=NULL,
                    radius_user=NULL, links_user=NULL) {
  if (!is.null(popup_user)) {
    popups <- sapply(studies_data[popup_user], as.character)
  } else {popups <- ""}

  if (!is.null(links_user)) {
    links_input <- sapply(studies_data[links_user], as.character)
    links = paste0("<b><a href='", links_input, "'>Link to paper</a></b>")
  } else {links <- ""}

  if (!is.null(radius_user)) {
    radiusby <- sapply(studies_data[radius_user], as.numeric)
  } else {radiusby <- 5}

  lat_plotted <- as.numeric(unlist(studies_data %>% dplyr::select(latitude)))
  lng_plotted <- as.numeric(unlist(studies_data %>% dplyr::select(longitude)))

  leaflet::leaflet(studies_data) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lat = ~lat_plotted,
                              lng = ~lng_plotted,
                              popup = ~paste(popups, links),
                              radius = ~as.numeric(radiusby),
                              stroke = FALSE, fillOpacity = 0.5,
                              clusterOptions = markerClusterOptions(disableClusteringAtZoom=16)
    )
}
