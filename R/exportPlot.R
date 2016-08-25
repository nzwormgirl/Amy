#' Export ggplot
#'
#' This function exports a ggplot2 object to a pdf, eps and png file. Code credited to https://gist.github.com/sheymann/2399659
#' @param gplot ggplot2 object
#' @param filename character string
#' @param width width of plot in inches
#' @param height height of plot in inches
#' @keywords ggplot2 pdf png eps
#' @export
#' @examples
#' is.outlier()

## Identifies outliers in a vector

ExportPlot <- function(gplot, filename, width=2, height=1.5) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}
