# Map the studies included in a systematic review.

sys_map <- function(studies_data, latitude,
                    longitude, popup_user=NULL,
                    radius_user=NULL, 
                    cluster_size_user=2,
                    links_user="",
                    cluster_points=T) {
  if (!is.null(popup_user)) {
    #hacky for loop, should be made vectorized & pretty someday
    popup_string <- ''
    for (popup in popup_user) {
      popup_string = paste0(popup_string, "<strong>", popup, '</strong>: ',
                            studies_data[, popup], "<br/>")
    }
  } else {popup_string <- ""}

  if (links_user != "") {
    links_input <- sapply(studies_data[links_user], as.character)
    links = paste0("<strong><a target='_blank' rel='noopener noreferrer' href='", 
                   links_input, "'>Link to paper</a></strong>")
  } else {links <- ""}

  if (!is.null(radius_user)) {
    radiusby <- sapply(studies_data[radius_user], as.numeric)
  } else {radiusby <- 3}

  lat_plotted <- as.numeric(unlist(studies_data %>% dplyr::select(latitude)))
  lng_plotted <- as.numeric(unlist(studies_data %>% dplyr::select(longitude)))

  basemap <- leaflet::leaflet(studies_data,
                              options = leafletOptions(minZoom = 2)) %>%
    leaflet::addTiles()

  if (cluster_points == T) {
    map <- basemap %>%
      leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                popup = ~paste(popup_string, links),
                                radius = ~as.numeric(radiusby * 3),
                                stroke = FALSE, fillOpacity = 0.5,
                                clusterOptions = markerClusterOptions(freezeAtZoom = cluster_size_user) )
  } else {
    map <- basemap %>%
      leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                popup = ~paste(popup_string, links),
                                radius = ~as.numeric(radiusby),
                                label = ~popup_string %>% lapply(shiny::HTML)
      )
  }

  map
}
