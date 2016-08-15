#' Rbind dataframes together
#'
#' This function allows you to rbind dataframes together even if the columns don't match.
#' @param x Dataframe 1
#' @param y Dataframe 2
#' @param type The type of rbind. all: rbinds the dataframes using all columns from both frames, filling any blank spaces with NA. match: rbinds the dataframes together, keeping only those columns that match. 
#' @keywords rbind
#' @export
#' @examples
#' rbind.columns()

rbind.columns <- function(x, y,type="all") {
  if(type=="all"){
    x.diff <- setdiff(colnames(x), colnames(y))
    y.diff <- setdiff(colnames(y), colnames(x))
    
    x[, c(as.character(y.diff))] <- NA
    
    y[, c(as.character(x.diff))] <- NA
    
    return(rbind(x, y))
  }
  
  if(type=="match"){
    n.input1 <- ncol(input1)
    n.input2 <- ncol(input2)
    
    if (n.input2 < n.input1) {
      TF.names <- which(names(input2) %in% names(input1))
      column.names <- names(input2[, TF.names])
    } else {
      TF.names <- which(names(input1) %in% names(input2))
      column.names <- names(input1[, TF.names])
    }
    
    return(rbind(input1[, column.names], input2[, column.names]))
  }
}