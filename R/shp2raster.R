#' Convert shapefiles to rasters
#'
#' This function allows you to convert a shapefile to a raster.
#' @param shp The shapefile you wish to convert
#' @param mask.raster A raster with the desired resolutions and extent of the output raster
#' @param label A string giving the desired filename for the raster (if map=TRUE) and the raster name
#' @param value An integer value for cells in the raster that are within the shapefile
#' @param bkg An integer value for cells in the raster outside the shapefile. Defaults to 0.
#' @param transform Do you wish to transform the projection of the shapefile prior to conversion to a raster. Defaults to FALSE.
#' @param proj.from The projection of the shapefile. Only used if transform = TRUE.
#' @param proj.to The desired projection of the raster. Only used if transform = TRUE.
#' @param map Do you want to output a map to the plot window at the end of conversion? Defaults to TRUE
#' @param save Do you wish to save a copy of the raster to the working directory? Defaults to TRUE. Filename is based on label.
#' @param mask Do you want the output raster to be masked by the mask.raster? Defaults to TRUE
#' @keywords shapefile, raster
#' @export
#' @examples
#' shp2raster()




shp2raster <- function(shp, mask.raster, label, value, bkg = 0,transform = FALSE, proj.from = NA, proj.to = NA, map = TRUE, save=TRUE,mask=TRUE) {
  require(raster, rgdal)
  
  # use transform==TRUE if the polygon is not in the same coordinate system as
  # the output raster, setting proj.from & proj.to to the appropriate
  # projections
  if (transform == TRUE) {
    if(is.na(proj4string(shp))) proj4string(shp) <- proj.from
    shp <- spTransform(shp, proj.to)
  }
  
  # convert the shapefile to a raster based on a standardised background raster with 
  # 'value' as the areas where the shapefile are and 'bkg' representing the areas outside the polygon
  r <- rasterize(shp, mask.raster,field=value,background=bkg)
    # mask out areas outside the boundary of the mask raster
    if(mask==T) r <- mask(r,mask.raster)
  
  # save a copy of the raster as a tif in the working directory
  if(save==TRUE) writeRaster(r, filename = label, format = "GTiff",overwrite = T)
    
  # plot map of new raster
  if (map == TRUE) plot(r, main = label, axes = F, box = F)
  
  names(r) <- label
  return(r)
}