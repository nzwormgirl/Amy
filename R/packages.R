#' Packages
#'
#' This function tries to load a library. If the library does not exist, it will install it for you.
#' @param x The desired package
#' @keywords require, library, install.packages
#' @export
#' @examples
#' packages()

# easy way to install packages as required
packages<-function(x){
   x<-as.character(match.call()[[2]])
   if (!require(x,character.only=TRUE)){
      install.packages(pkgs=x,repos="http://cran.r-project.org")
      require(x,character.only=TRUE)
   }
}