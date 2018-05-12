pairsamtoolsDedup <- function( pairsamFiles , compressOut=TRUE, outputStats=TRUE , inThreads=1, outThreads=1, pipe=FALSE, threads=getOption("threads",1L) ){

  numfiles <- length(pairsamFiles)
  outnames <- paste0(basename(removeext(gsub(".gz$","",pairsamFiles))),"_dedup.pairsam")
  if(compressOut){outnames=paste0(outnames,".gz")}
  outstats <- paste0(outnames,".stats")

  cmdString <- paste(
    "pairsamtools", "dedup",
    "--nproc-out",inThreads,
    "--nproc-in",outThreads,
    "--mark-dups",
    if(outputStats){paste("--output-stats",outstats)},
    if(!pipe){paste("-o",outnames)},
    if(!pipe){pairsamFiles}
  )

  res <- cmdRun(cmdString,threads=threads)
  return(outnames)

}
