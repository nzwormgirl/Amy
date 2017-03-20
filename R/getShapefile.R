#' getShapefile
#'
#' This function allows you to open a shapefile using a filename and path
#' @param myShapefile A character string with the shapefile name. Must not end in .shp
#' @param myDir The directory path where the shapefile sits. Must end with trailing slash
#' @keywords shapefile
#' @export
#' @examples
#' getShapefile()

## produce a piechart and list of matching colours based on the text provided and then add nearby colours

getShapefile <- function(myShapefile,myDir){
  # myShapefile <- TheseShapefiles[1]
  require(rgdal)

  # is the shapefile within a folder in rawDir
  if(grepl("/",myShapefile)){
    myDSN <- paste0(myDir,"/", strsplit(myShapefile,"/")[[1]][1])
    myShapefile <- gsub("\\.shp","",strsplit(myShapefile,"/")[[1]][2])
  } else {
    myDSN <- myDir
    myShapefile <- gsub("\\.shp","",myShapefile)
  }

  myOut <- readOGR(dsn=myDSN,layer=myShapefile)
  myOut$datasource <- myShapefile

  return(myOut)
}
