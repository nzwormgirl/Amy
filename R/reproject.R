#' Reproject a shapefile
#'
#' This function changes the spatial projection of a shapefile
#' @param myShapefile The filenameshapefile of interest
#' @param myProjection The desired projection as a character string. Default is projNZTM
#' @export
#' @examples
#' reproject()

# reproject shapefiles to NZTM
reproject <- function(myShapefile,myProjection = projNZTM){

  myOut <- get(myShapefile)
  if(myOut@proj4string@projargs != CRS(myProjection)@projargs){
    message(paste("Reprojecting",myShapefile,"to", myProjection))
    myOut <- spTransform(myOut, CRS(myProjection))
  }

  return(myOut)
  suppressMessages(gc())
}
