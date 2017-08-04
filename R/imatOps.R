imatOps <- function( imat1, imat2, operation="difference" ){


  n1 <- names(imat1)
  n2 <- names(imat2)
  
  if(!identical(n1,n2)){stop("imats dont seem to share chromosome ordering/names")}
  outmat <- lapply(1:length(imat1),function(i){
    mindim <- min(c(dim(imat1[[i]]),dim(imat2[[i]])))
    imat1[[i]] <- imat1[[i]][1:mindim,1:mindim]
    imat2[[i]] <- imat2[[i]][1:mindim,1:mindim]
    if(operation=="difference"){
      out <- imat1[[i]]-imat2[[i]]
    }
    return(out)
  })
  names(outmat) <- n1

}
