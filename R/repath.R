
# adapted from https://www.r-bloggers.com/stop-fiddling-around-with-copied-paths-in-windows-r/

.repath <- function() {
  cat("Paste windows file path and hit RETURN twice")
  x <- scan(what = "")
  xa <- gsub("\\\\", "/", x)
  writeClipboard(paste(xa, collapse=" "))
  cat("Here's your de-windowsified path. (It's also on the clipboard.)\n", xa, "\n")
  
}

