#' Error bars for barplots
#'
#' Error bar function for bar plots from The R Book (Page 56)
#' @param yv height of the bars
#' @param z length of the error bars (+/-)
#' @param nn labels for the x-axis
#' @keywords errorbar barplot
#' @export
#' @examples
#' errorbarsBarplot()

#Error bar function for bar plots from The R Book (Page 56)
errorbarsBarplot<-function(yv,z,nn){
  xv<-barplot(yv,las=3,ylim=c(0,(max(yv)+max(z))),names=nn,ylab=deparse(substitute(yv)))
  g=(max(xv)-min(xv))/50
  for (i in 1:length(xv)){
    lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i],yv[i]+z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i],yv[i]-z[i]))
    }}

#' Y Error bars for scatterplots
#'
#' error bar function from http://monkeysuncle.stanford.edu/?p=485
#' @param x vector of x-values
#' @param y vector of y-values
#' @param upper vector of upper limits for errorbars
#' @param lower vector of lower limits for errorbars. Defaults to upper
#' @param length width of cross bars on error bars
#' @keywords errorbar scatterplot
#' @export
#' @examples
#' errorbarsScatterplot()
#'
#error bar function from http://monkeysuncle.stanford.edu/?p=485
errorbarsScatterplot <- function(x, y, upper, lower=upper, length=0.1,...){
if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
stop("vectors must be same length")
arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


se<-function(x){(sqrt(var(na.omit(x))/length(na.omit(x))))}
arc.sine<-function(x){(asin(sqrt(x/100))*100)}

#' X & Y Error bars for scatterplots
#'
#' error bar function from http://monkeysuncle.stanford.edu/?p=485
#' @param x vector of x-values
#' @param y vector of y-values
#' @param xbar vector of errors for x values
#' @param ybar vector of errors for y values
#' @keywords errorbar scatterplot
#' @export
#' @examples
#' errorbarsScatterplot()
#'

#scatterplot with x and y error bars
xy.error.bars<-function(x,y,xbar,ybar){
  plot(x,y,pch=16,ylim=c(min(y-ybar),max(y+ybar)),xlim=c(min(x-xbar),max(x+xbar)))
  arrows(x,y-ybar,x,y+ybar,code=3,angle=90,length=0.1)
  arrows(x-xbar,y,x+xbar,y,code=3,angle=90,length=0.1)
  }

#' Y Error bars for scatterplots
#'
#' @param x vector of x-values
#' @param y vector of y-values
#' @param ybar vector of errors for y values
#' @param xlabel label for x-axis
#' @param ylabel label for y-axis
#' @keywords errorbar scatterplot
#' @export
#' @examples
#' y.error.bars()
#'
#scatterplot with y error bars
y.error.bars<-function(x,y,ybar,xlabel,ylabel){
  plot(x,y,pch=c(16),ylim=c(min(y-ybar),max(y+ybar)),xlim=c(min(x),max(x)),xlab=xlabel,ylab=ylabel)
  arrows(x,y-ybar,x,y+ybar,code=3,angle=90,length=0.1)
  }

#' X Error bars for scatterplots
#'
#' @param x vector of x-values
#' @param y vector of y-values
#' @param xbar vector of errors for x values
#' @keywords errorbar scatterplot
#' @export
#' @examples
#' x.error.bars()
#'
#scatterplot with x error bars
x.error.bars<-function(x,y,xbar){
  plot(x,y,pch=16,ylim=c(min(y),max(y)),xlim=c(min(x-xbar),max(x+xbar)))
  arrows(x-xbar,y,x+xbar,y,code=3,angle=90,length=0.1)
  }

