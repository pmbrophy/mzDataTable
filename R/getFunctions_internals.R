#' Get mz start and stop as vector
#'
#' @param mz the m/z value to extract.
#' @param mz_delta Optional. A numeric value +/- the mz value to extract.
#' @param ppmTol Optional. The mass accuracy of the instrument in ppm.
#'
#' @return a vector of length two specifying the start and stop m/z value.
#'

.getMzRange <- function(mz, mz_delta, ppmTol){
  #mz Filter
  if(!is.null(mz_delta) & !is.null(ppmTol)){
    stop("mz_delta and ppmTol provided. Set only one for m/z filter or none for exact matching.")
  }else if(!is.null(mz_delta)){
    #Filter using mz_delta
    mz_min <- mz - mz_delta
    mz_max <- mz + mz_delta
  }else if(!is.null(ppmTol)){
    #Filter using ppmTol
    mz_min <- mz - mz*(1E-6)*ppmTol
    mz_max <- mz + mz*(1E-6)*ppmTol
  }else{
    mz_min <- mz
    mz_max <- mz
  }
  c(mz_min, mz_max)
}


#' Get index range from `seqNum`
#'
#' @param mzObj either a data.table or disk.frame.
#' @param iStart The integer start index to begin extracting ions.
#' @param iStop The integer stop index stop extracting ions.
#'
#' @return a vector of length two
#'

.getiRange <- function(mzObj, iStart, iStop){
  #Fill Missing Values
  if(is.null(iStart & !is.null(iStop))){
    iStart <- min(mzObj[, seqNum])

  }else if(is.null(iStop) & !is.null(iStart)){
    iStop <- max(mzObj[, seqNum])
  }

  c(iStart, iStop)
}

#' Get time range from `retentionTime`
#'
#' @param mzObj Either a data.table or disk.frame.
#' @param tStart The numeric time value to begin extracting ions.
#' @param tStop The numeric time value to stop extracting ions.
#'
#' @return a vector of length two
#'

.gettRange <- function(mzObj, tStart, tStop){
  #Fill Missing Values
  if(is.null(tStart) & !is.null(tStop)){
    tStart <- min(mzObj[, retentionTime])

  }else if(is.null(tStop) & !is.null(tStart)){
    tStop <- max(mzObj[, retentionTime])
  }

  c(tStart, tStop)
}


#' Check if all objects are in .GlobalEnv
#'
#' @param varNames a character vector of any length providing variable names
#'
#' @return logical vector of length one
#'

.inGlobalEnv <- function(varNames){
  inGlobal <- sapply(X = varNames, FUN = exists, where = .GlobalEnv)
  all(inGlobal)
}
