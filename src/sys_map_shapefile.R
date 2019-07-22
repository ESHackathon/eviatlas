sys_map_shapefile <- function(shp) {
  leaflet() %>%
    addTiles(layerId = "atlas_basemap") %>% 
    addFeatures(data = shp)
  # , 
  #               fillColor = "green", 
  #               fillOpacity = 0.5, 
  #               color = "black", 
  #               weight = 2)
  # 
}