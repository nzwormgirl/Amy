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
#' makeSpatialObject(quakes,myX="long",myY="lat",currentProj = projWGS84)
#'

makeSpatialObject <- function(myData,myX,myY,currentProj=NULL,newProj = NULL){

  coordinates(myData) <- cbind(myData[,myX] , myData[,myY])

  if(!is.null(currentProj)) {
    proj4string(myData) <- CRS(currentProj)
    if(!is.null(newProj)){
      myData <- sp::spTransform(myData,CRS(newProj))
    }
  }

  return(myData)
}

#' addNewCoordinates
#'
#' This function allows you to add new columns of coordinates in a different projection.
#' @param myData A data.frame to be made spatial - must include columns of spatial data
#' @param myX Quoted name of column of easting or longitude coordinates
#' @param myY Quoted name of cColumn of northing or latitude coordinates
#' @param myNewX Quoted name of new column of easting or longitude coordinates
#' @param myNewY Quoted name of new column of northing or latitude coordinates
#' @param currentProj The current projection of the coordinate data as a character vector
#' @param newProj A new projection as a character vector.
#' @keywords shapefile
#' @export
#' @examples
#' addNewCoordinates(quakes,myX="long",myY="lat",myNewX = "easting",myNewY = "northing", currentProj = projWGS84, newProj = projNZTM)
#'

addNewCoordinates <- function(myData,myX,myY,myNewX = "X",myNewY = "Y",currentProj=NULL,newProj = NULL){

  myNewX <- dplyr::quo_name(myNewX)
  myNewY <- dplyr::quo_name(myNewY)

  # identify rows with missing coordinates
  myNArows <- which(rowSums(is.na(myData[,c(myX,myY)]))>0)

  myOut <- dplyr::bind_rows(
    # calculate new coordinates for all rows with spatial data
    as.data.frame(
    makeSpatialObject(myData[-myNArows,],myX,myY,currentProj,newProj)
  ) %>% dplyr::rename(!!myNewX := coords.x1,
                      !!myNewY := coords.x2),
  # add back in rows with missing coordinate data
  myData[myNArows,]
  )

  return(myOut)
}



