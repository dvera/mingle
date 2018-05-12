coolerZoomify <- function( coolerFiles , fileThreads=1, balance=FALSE, threads=getOption("threads",1L) ){

  numfiles <- length(coolerFiles)

  cmdString <- paste(
    "cooler", "zoomify",
    if(balance){"--balance"},
    "-p",             fileThreads,
    coolerFiles
  )

  res <- cmdRun(cmdString,threads=threads)

  return(coolerFiles)

}
