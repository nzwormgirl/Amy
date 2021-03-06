% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latticePanels.r
\name{panel.hist}
\alias{panel.hist}
\title{Histogram on diagonal pairs
put histogram on diagonal of pairs}
\usage{
panel.hist(x, ...)
}
\arguments{
\item{x}{Numeric vector}
}
\description{
Histogram on diagonal pairs
put histogram on diagonal of pairs
}
\examples{
require(lattice)
pairs(iris,lower.panel=panel.smooth, upper.panel=panel.hist)
}
\keyword{outlier}
