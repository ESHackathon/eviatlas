#' generate systematic map atlas for shapefiles
#' @param shp the shapefile used to generate atlas
#' @param popups columns to appear in popups when clicking atlas points
#' @keywords internal

sys_map_shapefile <- function(shp, popups = "") {
  leaflet::leaflet(shp) %>%
    leaflet::addTiles(layerId = "atlas_basemap") %>%
    leafem::addFeatures(
      data = shp,
      group = "atlas_shapefile",
      fillColor = "green",
      fillOpacity = 0.5,
      color = "black",
      weight = 2,
      popup = ~popups
    )
}
