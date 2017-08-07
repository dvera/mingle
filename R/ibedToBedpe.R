ibedToBedpe <- function(bedFile , minInteractions=1 , threads=getOption("threads",1L)  ){

  numfiles <- length(bedFile)
  if(any(file_ext(bedFile)=="bedpe")){stop("input should be ibed files")}
  outnames <- paste0(basename(removeext(bedFile)),"_min",minInteractions,".bedpe")
  cmdStrings <- paste(
      "sed 's/[:-]/\t/g ; s/,/\t/g'",bedFile,
        "| awk '{if(sqrt(($7)^2)>=",minInteractions,"){for(i=1;i<=$7;i++){a++;print $1,$2,$3,$4,$5,$6,a,$5-$2}}}' OFS='\t' >",outnames
    ) 

  res <- cmdRun(cmdStrings,threads=threads)
  
  return(outnames)
}
