sys_map_shapefile <- function(shp, popups = "") {
  
  leaflet(shp) %>%
    addTiles(layerId = "atlas_basemap") %>% 
    leafem::addFeatures(data = shp, 
                group = 'atlas_shapefile', 
                fillColor = "green",
                fillOpacity = 0.5,
                color = "black",
                weight = 2,
                popup = ~popups)
  
}
