#' Pretty cut
#'
#' This function allows you to produce a tidy cut of breaks for nice legend plotting
#' @param myFac Factor
#' @keywords legend
#' @export
#' @examples
#' PrettyCut()

#########
PrettyCut <- function(myFac) {
  myLevels <- levels(myFac)
  myLevels <- as.character(myLevels)
  myLevels <- lapply(myLevels, function(x) substr(x, start = 2, stop = nchar(x)-1) )
  myLevels <- lapply(myLevels, function(x) strsplit(x, split = ",")[[1]] )
  myLevels <- lapply(myLevels, as.numeric)
  myLevels <- lapply(myLevels, format, scientific = F, trim = T)
  myLevels <- lapply(myLevels, as.character)
  myLevels <- sapply(myLevels, function(x) paste(x, collapse = " to "))
  levels(myFac) <- myLevels
  return(myFac)
}
#########
