ibedToBedpe <- function(bedFile , minInteractions=1 , threads=getOption("threads",1L) ){

  numfiles <- length(bedFile)
  outnames <- paste0(basename(removeext(
  cmdStrings <- paste(
      "sed 's/[:-]/\t/g ; s/,/\t/g'",bedFile,
      "| awk '{for(i=1;i<=$7;i++){print $1,$2,$3,$4,$5,$6}}' OFS='\t'",
      " | awk '{",
        "if($1==$4){",
          "left=int($2/",binsize,");",
          "right=int($6/",binsize,");",
          "if(left<right){print $1,left,right}",
          "else{print $1,right,left}",
        "}}' OFS='\t' | sort -T . -S 10G -k1,1 -k2,2n -k3,3n | uniq -c | awk '{",
          "if($1>=",minInteractions,"){",
            "print $2,$3,$4,$1",
          "}",
        "}' OFS='\t'"
    ) 

  res <- cmdRun(cmdStrings,threads=threads,tsv=T)
  
  return(res)
}
