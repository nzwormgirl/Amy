#' Get Observed and Predicted data
#'
#' This function allows you to produce a frame of observed and predicted values
#' @param myModel Model object from a random forest model
#' @keywords random forest
#' @export
#' @examples
#' GetObsPreds()

###############
# Produce frame of observed and predicted values
GetObsPreds <- function(myModel){
  data.frame(Obs = myModel$y, Preds = predict(myModel))
}
###############

#' Get importance
#'
#' This function allows you to calculate the importance variables
#' @param myModel Model object from a random forest model
#' @keywords random forest
#' @export
#' @examples
#' GetImportance()
#'
###############
# Produce frame of observed and predicted values
GetImportance <- function(myModel){
  return(myModel$importance)
}
###############

