#' Identify outliers
#'
#' This function produces a logical vector identifing outliers in a vector based on Davies, P.L. and Gather, U. (1993)."The identification of multiple outliers" (with discussion) J. Amer. Statist. Assoc., 88, 782-801.
#' @param x Numeric vector
#' @keywords outlier
#' @export
#' @examples
#' is.outlier()

## Identifies outliers in a vector
is.outlier = function (x) {
  # See: Davies, P.L. and Gather, U. (1993).
  # "The identification of multiple outliers" (with discussion)
  # J. Amer. Statist. Assoc., 88, 782-801.

  x <- na.omit(x)
  lims <- median(x) + c(-1, 1) * 5.2 * mad(x, constant = 1)
  x < lims[1] | x > lims[2]
}
