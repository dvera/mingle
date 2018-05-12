coolerBalance <- function( coolerFiles , ignoreDiags=2, tol=1e-5, madMax=5, minNnz=10, minCount=0, cisOnly=FALSE, transOnly=FALSE, force=TRUE, fileThreads=1, maxIters=200, threads=getOption("threads",1L) ){

  numfiles <- length(coolerFiles)

  cmdString <- paste(
    "cooler", "balance",
    "-p",             fileThreads,
    "--ignore-diags", ignoreDiags,
    "--tol",          tol,
    "--mad-max",      madMax,
    "--min-nnz",      minNnz,
    "--min-count",    minCount,
    "--max-iters",    maxIters,
    if(cisOnly){"--cis-only"},
    if(transOnly){"--trans-only"},
    if(force){"-f"},
    coolerFiles
  )

  res <- cmdRun(cmdString,threads=threads)

  return(coolerFiles)

}
