sys_map_shapefile <- function(shp) {
  leaflet() %>%
    addTiles() %>% 
    addFeatures(data = shp, 
                fillColor = "green", 
                fillOpacity = 0.5, 
                color = "black", 
                weight = 2)
  
}