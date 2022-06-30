#' Map the studies included in a systematic review.
#' 
#' Created For	  : ES Hackathon 2018
#' @param studies_data Input dataframe
#' @import leaflet
#' @keywords SystematicReview
#' @export


sys_map <- function(studies_data) {

  basemap <- leaflet::leaflet(studies_data,
                              options = leafletOptions(minZoom = 2)) %>%
    leaflet::addTiles(layerId = "atlas_basemap")

  basemap
}
