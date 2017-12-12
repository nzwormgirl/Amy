#' Identify Dropbox path
#'
#' This function identifies the path to a Dropbox folder
#' @keywords dropbox
#' @export
#' @examples
#' getDropboxPath()

getDropboxPath <- function(){
  if (Sys.info()['sysname'] == 'Windows') {
    info <- RJSONIO::fromJSON(
      if (file.exists(file.path(Sys.getenv('APPDATA'), 'Dropbox','info.json'))) {
        file.path(Sys.getenv('APPDATA'), 'Dropbox', 'info.json')
      } else {
        file.path(Sys.getenv('LOCALAPPDATA'),'Dropbox','info.json')
      }
    )
  }

  return(info$personal$path)

}


