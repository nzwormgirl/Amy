#' Histogram on diagonal pairs
#' put histogram on diagonal of pairs
#'
#' @param x Numeric vector
#' @keywords outlier
#' @export
#' @examples
#' require(lattice)
#' pairs(iris,lower.panel=panel.smooth, upper.panel=panel.hist)

## put histogram on diagonal of pairs
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

#' Correlation panel for lattice plots
#'
#' put (absolute) correlations on pairs plot, with size proportional to the correlations.
#'
#' @param x Numeric vector
#' @param y Numeric vector
#' @keywords lattice correlation
#' @export
#' @examples
#'
#' pairs(iris,lower.panel=panel.smooth, upper.panel=panel.cor)
#'
## put (absolute) correlations on pairs plot, with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

#' Doug's default lattice settings
#'
#' @param myCols vector or single value of colour data. Can be r colours or hex codes. Default = 1:8
#' @param mypch single value or vector of pch values. Default = 15
#' @param mylty set line type. Default = 1
#' @param mylwd set line width. Default = 1
#' @param ChangeAxis Default = T
#' @keywords lattice correlation
#' @export
#' @examples
#'
#'

# change lattice settings
LatticeSettings <- function(myCols = 1:8, mypch = 15, mylty = 1, mylwd = 1, ChangeAxis = T) {
  myStrip <- trellis.par.get("strip.background")
  myStrip$col <- "white"
  trellis.par.set(name = "strip.background", value = myStrip)

  myStrip <- trellis.par.get("par.main.text")
  myStrip$cex <- 1
  myStrip$font <- 1
  trellis.par.set(name = "par.main.text", value = myStrip)

  myStrip <- trellis.par.get("plot.symbol")
  myStrip$col <- myCols
  myStrip$pch <- mypch
  trellis.par.set(name = "plot.symbol", value = myStrip)

  myStrip <- trellis.par.get("plot.line")
  myStrip$lty <- mylty
  myStrip$lwd <- mylwd
  trellis.par.set(name = "plot.line", value = myStrip)

  myStrip <- trellis.par.get("superpose.symbol")
  myStrip$col <- myCols
  myStrip$pch <- mypch
  trellis.par.set(name = "superpose.symbol", value = myStrip)


  myStrip <- trellis.par.get("superpose.line")
  myStrip$lty <- mylty
  myStrip$lwd <- mylwd
  myStrip$col <- myCols
  trellis.par.set(name = "superpose.line", value = myStrip)

  myStrip <- trellis.par.get("box.umbrella")
  myStrip$lty <- mylty
  myStrip$lwd <- mylwd
  myStrip$col <- myCols
  trellis.par.set(name = "box.umbrella", value = myStrip)

  myStrip <- trellis.par.get("box.rectangle")
  myStrip$lty <- mylty
  myStrip$lwd <- mylwd
  myStrip$col <- myCols
  trellis.par.set(name = "box.rectangle", value = myStrip)

  if(ChangeAxis){
    myStrip <- trellis.par.get("axis.components")
    myStrip$top$tck <- 0
    myStrip$right$tck <- 0
    myStrip$right$pad1 <- 0
    myStrip$right$pad2 <- 0
    myStrip$top$pad1 <- 0
    myStrip$top$pad2 <- 0
    myStrip$bottom$pad1 <- 1
    myStrip$bottom$pad2 <- 0
    myStrip$left$pad1 <- 1
    myStrip$left$pad2 <- 0
    trellis.par.set(name = "axis.components", value = myStrip)
  }

}


#' plot map of NZ coastline in NZMG
#'
#' @param x x variables
#' @param y y variables
#' @keywords lattice panel map NZMG
#' @export
#' @examples
#'
#'

# panel function for plotting data and map of NZ coastline
panel.NZmapMG <- function(x, y, ...) {
  panel.xyplot(x = NZmap$NZE, y = NZmap$NZN, type = "l", col = 1)
  panel.xyplot(x, y, ...)
}


#' plot map of NZ coastline in NZTM
#'
#' @param x x variables
#' @param y y variables
#' @keywords lattice panel map NZTM
#' @export
#' @examples
#'
#'
# panel function for plotting data and map of NZ coastline  (TM)
panel.NZmapTM <- function(x, y, ...) {
  panel.xyplot(x = NZ_Coast_NZTM2$X, y = NZ_Coast_NZTM2$Y, type = "l", col = "black")
  panel.xyplot(x, y, ...)
}
#########

