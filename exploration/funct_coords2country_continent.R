#------------------------------------
#' Simple R code to get country and continent name from input longitude and latitudes
#' Created For	  : ES Hackathon 2018
#' Based On       : https://stackoverflow.com/a/21727515/9522520
#' @param ilonlat Input data frame with two columns for longitude and latitude in degrees
#' @return Returns a data frame with same number of rows as \code{ilonlat}two columns with $Country and $Continent names for input
#'
#' @examples
#' ilonlat <-data.frame(lon=c(0, 90, -45, -100, 130), lat=c(52, 40, -10, 45, -30 ))
#' coords2country_continent(ilonlat)
#=========================

coords2country_continent = function(ilonlat)
{
  library(rworldxtra)
  library(rworldmap)

  countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail

  # converting points to a SpatialPoints object and setting CRS directly to that from rworldmap
  pointsSP  <- SpatialPoints(ilonlat, proj4string=CRS(proj4string(countriesSP)))

  # use 'over' to get indices of the Polygons object containing each point
  indices  <- over(pointsSP, countriesSP)

  # Shorten long label for South America
  levels(indices$REGION)[levels(indices$REGION)=="South America and the Caribbean"] <- "South America"

  #Get country and continent name. Convert country to character so levels dont show up in future usage
  locdetails <- data.frame(Country=as.character(indices$NAME), Continent =indices$REGION)
  return(locdetails)
}
