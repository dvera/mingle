pairsamtoolsParse <- function( alignmentFiles , chromsizes , compressOut=TRUE, minMapq=20, dropSeq=TRUE , outputStats=TRUE , threads=getOption("threads",1L) ){

  numfiles <- length(alignmentFiles)
  outnames <- paste0(basename(removeext(alignmentFiles)),".pairsam")
  threads2=floor(threads/numfiles)
  if(threads2<1){threads2=1}
  if(compressOut){outnames=paste0(outnames,".gz")}
  outstats <- paste0(outnames,".stats")
  if(missing(chromsizes)){
		chromsizes<-getOption("chromsizes",NULL)
		if(is.null(chromsizes)){stop("must define file contain chromosome sizes")}
	}
	if(!file.exists(chromsizes)){
		stop("chromsizes file does not exist")
	}
  assemblyName <- remove.suffix(chromsizes,"\\.")
  cmdString <- paste(
    "pairsamtools", "parse",
    "-c",chromsizes,
    "--min-mapq",minMapq,
    "--assembly",assemblyName,
    if(dropSeq){"--drop-seq"},
    if(outputStats){paste("--output-stats",outstats)},
    "--nproc-out",threads2,
    "--nproc-in 1",
    "-o",outnames,
    alignmentFiles
  )

  res <- cmdRun(cmdString,threads=threads)
  return(outnames)

}
