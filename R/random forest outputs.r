#' Get Regression outputs
#'
#' This function allows you to run a regression model
#' @param myDataFrame Dataframe of data for regression model.
#' @param myY Name of response variable.
#' @param myX Vector of predictor variable names.
#' @param myMethod The type of regression model to run. Default = "lm".
#' @keywords random forest
#' @export
#' @examples
#' GetRegression()

###############
GetRegression <- function(myDataFrame, myY, myX, myMethod = lm) {
  # cat(paste("Running",myMethod,"model for",myY,"\n"))
  myFormula <- formula(paste(myY, paste(myX, collapse = "+"), sep = "~")) # use xyars and GetFormula
  myArgs <- list(formula = myFormula, data = myDataFrame)
  myModel <- do.call(myMethod, args = myArgs)
  return(myModel)
}
###############

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

