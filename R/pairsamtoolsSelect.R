pairsamtoolsSelect <- function( pairsamFiles , compressOut=TRUE, conditions="(pair_type == \"UU\")", inThreads=1, outThreads=1, pipe=FALSE, threads=getOption("threads",1L) ){

  numfiles <- length(pairsamFiles)
  outnames <- paste0(basename(removeext(gsub(".gz$","",pairsamFiles))),"_select.pairsam")
  if(compressOut){outnames=paste0(outnames,".gz")}
  outstats <- paste0(outnames,".stats")

  cmdString <- paste(
    "pairsamtools", "select",
    "'",conditions,"'",
    "--nproc-out",inThreads,
    "--nproc-in",outThreads,
    if(!pipe){paste("-o",outnames)},
    if(!pipe){pairsamFiles}
  )

  res <- cmdRun(cmdString,threads=threads)
  return(outnames)

}
