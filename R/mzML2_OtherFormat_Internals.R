.scanChunker <- function(scans, mzRfilePointer, chunkSize){
  #Get Number of scans
  if(is.null(scans)){
    nScans <- mzR::runInfo(file)$scanCount
    scans <- c(1:nScans)
  }else{
    nScans <- length(scans)
  }

  #Split scans into chunks
  if(is.null(chunkSize)){
    chunkSize <- nScans
  }
  nGroups <- ceiling(nScans/chunkSize)
  scanChunks <- splitIndex(index = scans, nGroups = nGroups, randomize = FALSE)$groups

  scanChunks
}

