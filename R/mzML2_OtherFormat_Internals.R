#collection of functions for internal use in converting mzML files to other formats

#' Break sets of ms scans into indexed chunks for multi-import methods
#'
#' @param scans A numeric vector of all of the scan indices to be imported
#' @param mzRfilePointer A mzR pointer object linking to the file to be
#'   converted
#' @param chunkSize The number of scans to be processed at a time - actual
#'   import size will be aproximately the size of chunkSize
#'
#' @return a list object containing the group indices and order of those indicies. See splitIndex.
#' @export
#'
#' @examples
#'

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

#' Internal function to be used by mapply for splitting a data.frame/data.table
#' by a list of index values
#'
#' @param df a data frame to be split by row index
#' @param vectorList rows to return
#'
#' @return retuns a data.frame/data.table subset
#'

.splitDFbyVectorList <- function(df, vectorList){
  df[vectorList, ]
}

