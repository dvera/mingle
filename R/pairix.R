pairix <- function( filenames , force=TRUE , threads=getOption("threads",1L) ){

	cmdString <- paste(
		"pairix",
		if(force){"-f"},
		filenames
	)

	res <- cmdRun(cmdString,threads=threads)

	return(filenames)

}
