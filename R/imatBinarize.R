imatBinarize <- function( imat , minInteractions=1 , threads=getOption("threads",1L) ){

  require(Matrix)

  options(scipen=99999)

  if(mode(imat[[1]])!="list"){single=TRUE}else{single=FALSE}

  if(single){
    imat <- list(imat)
  } else{
    names(imat) <- paste0(names(imat),"_binary",minInteractions)
  }

  numfiles <- length(imat)
  
  output <- lapply(1:length(imat),function(j){
    outmat <- lapply(1:length(imat[[j]]),function(i){
      chrmat <- imat[[j]][[i]]
      chrmat[chrmat<minInteractions] <- 0
      chrmat[chrmat>minInteractions] <- 1
      return(chrmat)
    })
    return(outmat)
  })
  
  names(output) <- names(imat)
  return(output)
}
