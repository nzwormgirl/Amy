#' Colour Wheel
#'
#' This function allows you to produce a colour wheel and list of matching colours based on the text provided and then add nearby colours
#' @param str String of colour name
#' @param nearby Proximity of nearby colours. Defaults to 3.
#' @param cex Size of labels on piechart
#' @keywords colour
#' @export
#' @examples
#' col.wheel()

## produce a piechart and list of matching colours based on the text provided and then add nearby colours
col.wheel <- function(str, nearby=3, cex=0.75) {
  cols <- colors()
  hsvs <- rgb2hsv(col2rgb(cols))
  srt <- order(hsvs[1,], hsvs[2,], hsvs[3,])
  cols <- cols[srt]
  ind <- grep(str, cols)
  if (length(ind) <1) stop("no colour matches found",
                           call.=FALSE)
  s.ind <- ind
  if (nearby>1) for (i in 1:nearby) {
    s.ind <- c(s.ind, ind+i, ind-i)
  }
  ind <- sort(unique(s.ind))
  ind <- ind[ind <= length(cols)]
  cols <- cols[ind]
  pie(rep(1, length(cols)), labels=cols, col=cols, cex=cex)
  cols
}
