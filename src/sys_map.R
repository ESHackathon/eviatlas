# Map the studies included in a systematic review.

sys_map <- function(studies_data) {

  basemap <- leaflet::leaflet(studies_data,
                              options = leafletOptions(minZoom = 2)) %>%
    leaflet::addTiles(layerId = "atlas_basemap")

  basemap
}
