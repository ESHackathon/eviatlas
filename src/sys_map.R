# Map the studies included in a systematic review.

sys_map <- function(studies_data, latitude,
                    longitude, popup_user=NULL,
                    radius_user=NULL, 
                    cluster_size_user=2,
                    links_user="",
                    cluster_points=T,
                    color_user="",
                    basemap_user="",
                    map_title="") {
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
  
  if (color_user != "") {
    factpal <- colorFactor(RColorBrewer::brewer.pal(9, 'Set1'), studies_data$color_user)
    colorby <- ~factpal(studies_data[[color_user]])
  } else {colorby <- "blue"}
        
        

  title <- h2(as.character(map_title))
  
  

  lat_plotted <- as.numeric(unlist(studies_data %>% dplyr::select(latitude)))
  lng_plotted <- as.numeric(unlist(studies_data %>% dplyr::select(longitude)))

  
  if(basemap_user == "OpenStreetMap") {
          basemap <- leaflet::leaflet(studies_data,
                                      options = leafletOptions(minZoom = 2)) %>%
                  leaflet::addProviderTiles(providers$OpenStreetMap) %>%
                  leaflet::addControl(title, position = "topright", className="map-title")
          } else if (basemap_user == "OpenTopoMap") {
                  basemap <- leaflet::leaflet(studies_data,
                                              options = leafletOptions(minZoom = 2)) %>%
                          leaflet::addProviderTiles(providers$OpenTopoMap) %>%
                          leaflet::addControl(title, position = "topright", className="map-title")
          } else if (basemap_user == "Stamen.TonerLite") {
                  basemap <- leaflet::leaflet(studies_data,
                                              options = leafletOptions(minZoom = 2)) %>%
                          leaflet::addProviderTiles(providers$Stamen.TonerLite) %>%
                          leaflet::addControl(title, position = "topright", className="map-title")
          } else if (basemap_user == "Esri.WorldStreetMap") {
                  basemap <- leaflet::leaflet(studies_data,
                                              options = leafletOptions(minZoom = 2)) %>%
                          leaflet::addProviderTiles(providers$Esri.WorldStreetMap) %>%
                          leaflet::addControl(title, position = "topright", className="map-title")                  
          } else {
                  basemap <- leaflet::leaflet(studies_data,
                                              options = leafletOptions(minZoom = 2)) %>%
                          leaflet::addTiles() %>%
                          leaflet::addControl(title, position = "topright", className="map-title")
          }
  

  if (cluster_points == T) {
    map <- basemap %>%
      leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                popup = ~paste(popup_string, links),
                                radius = ~as.numeric(radiusby * 3),
                                color = colorby,
                                stroke = FALSE, fillOpacity = 0.7,
                                clusterOptions = markerClusterOptions(freezeAtZoom = cluster_size_user) )
  } else {
    map <- basemap %>%
      leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                popup = ~paste(popup_string, links),
                                radius = ~as.numeric(radiusby),
                                color = colorby,
                                label = ~popup_string %>% lapply(shiny::HTML)
      )
  }

  map
}
