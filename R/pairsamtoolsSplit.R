pairsamtoolsSplit <- function( pairsamFiles , outputSam=FALSE , outputBam=FALSE , compressOut=TRUE, inThreads=1, outThreads=1, pipe=FALSE, threads=getOption("threads",1L) ){

  numfiles <- length(pairsamFiles)
  outnames <- paste0(basename(removeext(gsub(".gz$","",pairsamFiles))),".pairs")
  outsams <- paste0(basename(removeext(gsub(".gz$","",pairsamFiles))),".sam")
  outbams <- paste0(basename(removeext(gsub(".gz$","",pairsamFiles))),".bam")
  if(compressOut){outnames=paste0(outnames,".gz")}

  if(outputSam & outputBam){stop("can only output sams or bams, not both")}

  cmdString <- paste(
    "pairsamtools", "split",
    "--nproc-out",inThreads,
    "--nproc-in",outThreads,
    if(outputSam){paste("--output-sam",outsams)},
    if(outputBam){paste("--output-sam",outbams)},
    if(!pipe){paste("--output-pairs",outnames)},
    if(!pipe){pairsamFiles}
  )

  res <- cmdRun(cmdString,threads=threads)
  return(outnames)

}
