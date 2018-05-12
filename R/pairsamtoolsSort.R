pairsamtoolsSort <- function( pairsamFiles , compressOut=TRUE, memory="2G", tmpdir=".", pipe=FALSE, inThreads=1, outThreads=1, sortThreads=1, threads=getOption("threads",1L) ){

  numfiles <- length(pairsamFiles)
  outnames <- paste0(basename(removeext(gsub(".gz$","",pairsamFiles))),"_sort.pairsam")
  if(compressOut){outnames=paste0(outnames,".gz")}
  cmdString <- paste(
    "pairsamtools", "sort",
    "--nproc",sortThreads,
    "--nproc-out",inThreads,
    "--nproc-in",outThreads,
    "--tmpdir",tmpdir,
    "--memory",memory,
    if(!pipe){paste("-o",outnames)},
    if(!pipe){pairsamFiles}
  )

  res <- cmdRun(cmdString,threads=threads)
  return(outnames)

}
