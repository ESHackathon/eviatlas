#' Map the studies included in a systematic review.
#'
#' Create a geographical map of the studies using `leaflet`. Designed for use as a systematic map.
#'
#' @param studies_data A dataframe of studies.
#' @param latitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @param longitude Specify which column contains the latitude in the `studies_data` dataframe.
#' @param popup_user Specify which column in the `studies_data` dataframe to use a popups when mousing over or clicking a point.
#' @param radius_user (unstable) Specify a numeric column in the `studies_data` dataframe to size points by.
#' @param links_user Specify a column in the `studies_data` dataframe that contains a URL for each study.
#' @param cluster_pints Toggle clustering feature of `leaflet` maps
#'
#' @author Andrew Feierman
#'
#' @export

sys_map <- function(studies_data, latitude,
                    longitude, popup_user=NULL,
                    radius_user=NULL, links_user=NULL,
                    cluster_points=T) {
  if (!is.null(popup_user)) {
    #hacky for loop, should be made vectorized & pretty someday
    popup_string <- ''
    for (popup in popup_user) {
      popup_string = paste0(popup_string, "<strong>", popup, '</strong>: ',
                            studies_data[, popup], "<br/>")
    }
  } else {popup_string <- ""}

  if (!is.null(links_user) & links_user != "None") {
    links_input <- sapply(studies_data[links_user], as.character)
    links = paste0("<strong><a href='", links_input, "'>Link to paper</a></strong>")
  } else {links <- ""}

  if (!is.null(radius_user)) {
    radiusby <- sapply(studies_data[radius_user], as.numeric)
  } else {radiusby <- 1}

  lat_plotted <- as.numeric(unlist(studies_data %>% dplyr::select(latitude)))
  lng_plotted <- as.numeric(unlist(studies_data %>% dplyr::select(longitude)))

  basemap <- leaflet::leaflet(studies_data,
                              options = leafletOptions(minZoom = 2)) %>%
    leaflet::addTiles()

  if (cluster_points == T) {
    map <- basemap %>%
      leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                popup = ~paste(popup_string, links),
                                radius = ~as.numeric(radiusby),
                                stroke = FALSE, fillOpacity = 0.5,
                                clusterOptions = markerClusterOptions() )
  } else {
    map <- basemap %>%
      leaflet::addCircleMarkers(lat = ~lat_plotted, lng = ~lng_plotted,
                                popup = ~paste(popup_string, links),
                                radius = ~as.numeric(radiusby * 2),
                                label = ~popup_string %>% lapply(shiny::HTML)
      )
  }

  map
}
