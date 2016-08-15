#' doug.cbind.list
#'
#' Tfunction to cbind all the elements in a list of data frames
# works even when all NA's
#' @param dfs List of dataframes
#' @keywords cbind
#' @export
#' @examples
#' Doug.cbind.list()

#########
# function to rbind all the elements in a list of data frames
# works even when all NA's
Doug.cbind.list <- function (dfs)
{
  data.frame(do.call("cbind", lapply(dfs, function(df) df)))
}
#########