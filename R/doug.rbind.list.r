#' doug.rbind.list
#'
#' Tfunction to rbind all the elements in a list of data frames
# works even when all NA's
#' @param dfs List of dataframes
#' @keywords rbind
#' @export
#' @examples
#' Doug.rbind.list()

#########
# function to rbind all the elements in a list of data frames
# works even when all NA's
Doug.rbind.list <- function (dfs)
{
  # dfs <- list(...)
  if (length(dfs) == 0)
    return(list())
  all.names <- unique(unlist(lapply(dfs, names)))
  data.frame(do.call("rbind", lapply(dfs, function(df) df)))
}
#########