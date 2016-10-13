#' DougSplit
#'
#' Wrapper for base::strsplit
#' @param mytext character vector, each element of which is to be split. Other inputs, including a factor, will give an error.
#' @param myPart Identify the part of the split to return. Default = 1.
#' @param split character vector (or object which can be coerced to such) containing regular expression(s) (unless fixed = TRUE) to use for splitting. If empty matches occur, in particular if split has length 0, x is split into single characters. If split has length greater than 1, it is re-cycled along x.
#' @keywords rbind
#' @export
#' @examples
#' DougSplit()

#########

DougSplit <- function(mytext, myPart = 1, split = "\\.", ...) {
  myOut <- strsplit(mytext, split = split, ...)[[1]][myPart]
  return(myOut)
}
#########
