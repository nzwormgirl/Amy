#' makeSpatialObject
#'
#' This function allows you to create a SpatialPointsDataFrame from a data.frame and project (and potentially reproject) it to a specified projection.
#' @param myData A data.frame to be made spatial - must include columns of spatial data
#' @param myX Column of easting or longitude coordinates
#' @param myY Column of northing or latitude coordinates
#' @param currentProj The current projection of the coordinate data as a character vector
#' @param newProj A new projection as a character vector. Use if you wish to reproject to a different coordinate system.
#' @keywords shapefile
#' @export
#' @examples
#' quakesSpatial <- makeSpatialObject(quakes,myX="long",myY="lat",currentProj = projWGS84)
#'

makeSpatialObject <- function(myData,myX,myY,currentProj=NULL,newProj = NULL){
  require(sp)
  coordinates(myData) <- cbind(myData[,myX] , myData[,myY])
  if(!is.null(currentProj)) {
    proj4string(myData) <- CRS(currentProj)
    if(!is.null(newProj)){
      myData <- spTransform(myData,CRS(newProj))
    }
  }

  return(myData)
}
