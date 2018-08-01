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
  require(sp)
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


addNewCoordinates <- function (myData, myX, myY, myNewX = "X", myNewY = "Y", currentProj = NULL, newProj = NULL) {
  # identify rows that are missing spatial data
  myNArows <- which(rowSums(is.na(myData[, c(myX, myY)])) > 0)

  # add in empty columns if the new X & Y columns don't already exist
  myData[c(myNewX,myNewY)[!(c(myNewX,myNewY) %in% colnames(myData))]]  <-  NA

  # get names of original data object
  myNames <- c(names(myData), "coords.x","coords.y")

  if (length(myNArows) > 0) {
    # missing spatial data - run calculation on rows with spatial data and bind the missing rows to the bottom
    myOut <- as.data.frame(makeSpatialObject(myData[-myNArows,], myX, myY, currentProj, newProj))
    names(myOut) <- myNames

    myNAData <- myData[myNArows,]
    myNAData[,c("coords.x","coords.y")] <- NA

    myOut <- dplyr::bind_rows(myOut, myNAData)
  } else {
    # no missing spatial data - run calculation on full dataframe
    myOut <- as.data.frame(makeSpatialObject(myData, myX, myY, currentProj, newProj))
    names(myOut) <- myNames

  }

  # where coordinates don't already exist, replace with the new projected coordinates
  myOut[,myNewX] <- ifelse(is.na(myOut[,myNewX]),myOut[,"coords.x"], myOut[,myNewX])
  myOut[,myNewY] <- ifelse(is.na(myOut[,myNewY]),myOut[,"coords.y"], myOut[,myNewY])

  # remove unnecessary projected columns
  myOut[c("coords.x","coords.y")] <- NULL

  return(myOut)
}



