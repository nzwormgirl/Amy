#' Read KML file
#'
#' function to read kml file and get all attribute data stored in Extended Data
#' from https://stackoverflow.com/questions/45989198/how-to-load-all-fields-extendeddata-not-just-name-and-description-from-kml
#' @param file File path and name
#' @keywords kml
#' @export
#' @examples
#' readKML()

require(xml2)
require(rgdal)

readKML<-function(file,keep_name_description=FALSE,...) {
  # Set keep_name_description = TRUE to keep "Name" and "Description" columns
  #   in the resulting SpatialPolygonsDataFrame. Only works when there is
  #   ExtendedData in the kml file.

  sp_obj<-readOGR(file,...)
  xml1<-read_xml(file)

  # extract name and type of variables
  variable_names<-xml_find_first(xml1,".//d1:Schema[@name]") %>%
    xml2::xml_contents() %>%
    xml2::xml_attrs() %>%
    do.call(rbind,.)

  # return sp_obj if no ExtendedData is present
  if (is.null(variable_names)) return(sp_obj)

  data<-lapply(seq(nrow(variable_names)),function(x) {
    x1<-xml_find_all(xml1,paste0(".//d1:SimpleData[@name=\"",variable_names[x,1],"\"]")) %>%
      xml_contents()
    if (variable_names[x,2]=="string") return(xml_text(x1))
    xml_double(x1)
  }) %>%
    do.call(cbind,.) %>%
    as.data.frame()

  colnames(data)<-variable_names[,1]

  if (!keep_name_description) {
    sp_obj@data<-data
  } else {
    sp_obj@data<-cbind(sp_obj@data,data)
  }
  sp_obj
}
