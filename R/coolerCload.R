coolerCload <- function( pairsFiles , binsize=50000, chromsizes, binThreads=1, threads=getOption("threads",1L) ){

  numfiles <- length(pairsFiles)

  if(all(file.exists(paste0(pairsFiles,".px2")))){
    subcommand="pairix"
  } else{
    subcommand="pairs"
  }

  if(missing(chromsizes)){
		chromsizes<-getOption("chromsizes",NULL)
		if(is.null(chromsizes)){stop("must define file contain chromosome sizes")}
	}
	if(!file.exists(chromsizes)){
		stop("chromsizes file does not exist")
	}
  assemblyName <- remove.suffix(chromsizes,"\\.")

  outnames <- paste0(basename(removeext(gsub(".gz$","",pairsFiles))),".cool")

  cmdString <- paste(
    "cooler", "cload",
    subcommand,
    "-p",binThreads,
    paste0(chromsizes,":",binsize),
    pairsFiles,
    outnames
  )

  res <- cmdRun(cmdString,threads=threads)

  return(outnames)

}
