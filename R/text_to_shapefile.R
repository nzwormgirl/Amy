#' Convert text files to shapefiles
#'
#' This function allows you to convert text files to shapefiles
#' @param text_file Character string of the name of a text file. The text file must have the following columns: id, lat, lon
#' @param filename The desired output filename
#' @param type The desired output type of the shapefile. 5=Polygons, 3=Polylines, 1=Points
#' @keywords textfiles, shapefiles
#' @export
#' @examples
#' convert.txt.shp()


##Convert text files to shape files
  convert.txt.shp <- function(text_file,filename,type){
    require(shapefiles)
    output <- read.table(text_file, header=TRUE)
    dd <- data.frame(Id=as.numeric(output$id), X=output$lon,Y=output$lat)
    ddTable <- data.frame(Id=as.numeric(output$id), Name=output$id)
    ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", type)
    write.shapefile(ddShapefile, filename, arcgis=T)  
  }
  
# # convert text to polygons
#   convert.txt.shp("Crozier_west.txt","Crozier_west_SA",5)
#   
# # convert text to lines
#   convert.txt.shp("bird_south_lines.txt","bird_south_lines",3)
#   
# # convert text to points
#   convert.txt.shp("bird_north_lines.txt","bird_north_points",1)
