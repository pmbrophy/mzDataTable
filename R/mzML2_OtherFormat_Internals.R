##################################################################################################### #
#       collection of functions for internal use in converting mzML files to other formats            #
##################################################################################################### #

#' @title Drop empty columns from data.frame
#'
#' @description Check for any columns in a data frame that contain only na and
#' remove
#'
#' @param df a data frame
#'
#' @return returns a data.frame
#'
#' @examples
#'

.dropEmptyCols <- function(df){
  #Check for cols containing all na
  emptyCols <- apply(X = df, MARGIN = 2, FUN = function(X){all(is.na(X))})

  if(any(emptyCols)){
    emptyCols <- which(emptyCols)
    df <- df[, -emptyCols]
  }

  df
}

#' @title Convert spectrumId strings to numerics
#'
#' @description Convert a character vector containing strings of the format
#' c("foobar=3 foo=45 bar=100") to a numeric data.frame
#'
#' @param spectrumId the spectrumId character vector
#'
#' @return a data.frame with column names
#'
#' @examples
#'

.getNumFromSpectrumId <- function(spectrumId){
  #Extract numeric values from spectrumId strings
  m <- gsub(pattern = "[[:alpha:]]+=", replacement = "", x = spectrumId)
  m <- strsplit(m, split = " ")
  m <- lapply(X = m, FUN = as.numeric)
  m <- do.call(rbind, m)
  m <- as.data.frame(m)

  #Get names from spectrumId
  idNames <- gsub(pattern = "=[[:digit:]]+", replacement = "", x = spectrumId)
  idNames <- unique(idNames)

  if(length(idNames) != 1){
    stop("spectrumId contains non-uniform formatting")
  }
  idNames <- strsplit(x = unique(idNames), split = " ")[[1]]

  #Set names in data.frame
  if(length(idNames) != ncol(m)){
    stop("spectrumId names differ in length from extracted data tyeps")
  }

  names(m) <- idNames

  m
}

#' @title Scan Chunker
#'
#' @description
#' Break sets of ms scans into indexed chunks for multi-import methods
#'
#' @param scans A numeric vector of all of the scan indices to be imported
#' @param mzRfilePointer A mzR pointer object linking to the file to be
#'   converted
#' @param chunkSize The number of scans to be processed at a time - actual
#'   import size will be aproximately the size of chunkSize
#'
#' @return a list object containing the group indices and order of those indicies. See splitIndex.
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

#' @title select data.frame by row
#'
#' @description
#' Internal function to be used by mapply for splitting a data.frame/data.table
#' by row using a vector of index values
#'
#' @param df a data frame to be split by row index
#' @param vectorList rows to return
#'
#' @return retuns a data.frame/data.table subset
#'

.splitDFbyVectorList <- function(df, vectorList){
  df[vectorList, ]
}




