#' @title Get XIC from data.table or disk.frame
#'
#' @description Extract ions by mz using a variety of options for subsetting.
#'   See details.
#'
#' @param mzObj a data.table or disk.frame imported from mzML/mzXML file.
#' @param mz the m/z value to extract.
#' @param mz_delta Optional. A numeric value +/- the mz value to extract.
#'   Default = NULL.
#' @param ppmTol Optional. The mass accuracy of the instrument in ppm.
#'   Default = NULL
#' @param iStart Optional. The integer start index to begin extracting ions.
#'   Default = NULL.
#' @param iStop Optional. The integer stop index stop extracting ions. Default =
#'   NULL.
#' @param tStart Optional. The numeric time value to begin extracting ions.
#'   Default = NULL.
#' @param tStop Optional. The numeric time value to stop extracting ions.
#'   Default = NULL.
#'
#' @details mzObj and mz must be provided. Default behavior is to extract ions
#'   exactly matching mz value accross all time.
#'
#'   Specifying mz_delta or ppmTol will result in mz filtering. mz_delta
#'   will extract ions with values in the range where mz >= mz-mz_delta and mz
#'   <= mz+mz_delta. Specifying ppmTol workes similarly, but calculates
#'   the upper and lower bounds from ppm mass error.
#'
#'   Specifying iStart/iStop or tStart/tStop will result in time filtering.
#'
#' @return Returns a subset of the original mzObj as a data.table
#' @export
#'
#' @examples
#'

getXIC <- function(mzObj, mz, mz_delta = NULL, ppmTol = NULL, iStart = NULL, iStop = NULL, tStart = NULL, tStop = NULL){
  suppressWarnings(remove(mzRange, iRange, tRange, envir = .GlobalEnv))

  #Check data.table
  isDataTable <- .check_mzDataTable(mzObj)

  #Calculate ranges to extract
  mzRange <<- .getMzRange(mz, mz_delta, ppmTol)
  iRange <<- .getiRange(mzObj, iStart, iStop)
  tRange <<- .gettRange(mzObj, tStart, tStop)

  #Do the extraction
  if(isDataTable){
    xic <- .getXIC_dt(mzDt = mzObj, mzRange, iRange, tRange)

  }else{
    xic <- .getXIC_dskF(mzDskF = mzObj, mzRange, iRange, tRange)
  }

  #Cleanup
  remove(mzRange, iRange, tRange, envir = .GlobalEnv)
  setkey(x = xic, physical = TRUE, "seqNum")
  xic
}

#' Get XIC from disk.frame
#'
#' @param mzDskF A disk.frame validated by .check_mzDataTable()
#' @param mzRange m/z range returned by .getMzRange()
#' @param iRange seqNum range returned by .getiRange()
#' @param tRange retentionTime range returned by .gettRange()
#'
#' @return Returns a data.table
#'
#' @details non-exported function for use in getXIC.
#'

.getXIC_dskF <- function(mzDskF, mzRange, iRange, tRange){
  tRangeIsValid <- !is.null(tRange)
  iRangeIsValid <- !is.null(iRange)

  if(tRangeIsValid){
    #Filter time by retentionTime
    xic <- mzDskF[mz %between% mzRange & retentionTime %between% tRange,
                list(sumIntensity = sum(intensity)),
                by = list(seqNum, retentionTime),
                keep = c("mz", "seqNum", "intensity", "retentionTime")]

  }else if(iRangeIsValid){
    #Filter time by seqNum
    xic <- mzDskF[mz %between% mzRange & seqNum %between% iRange,
                list(sumIntensity = sum(intensity)),
                by = list(seqNum, retentionTime),
                keep = c("mz", "seqNum", "intensity", "retentionTime")]

  }else if(iRangeIsValid & tRangeIsValid){
    stop("iStart/iStop and tStart/tStop cannot both be specified")

  }else{
    #Do not filter time
    xic <- mzDskF[mz %between% mzRange,
                list(intensity = sum(intensity)),
                by = list(seqNum, retentionTime),
                keep = c("mz", "seqNum", "intensity", "retentionTime")]
  }

  #Return
  xic
}

#' Get XIC from data.table
#'
#' @param mzDt a data.table validated by .check_mzDataTable()
#' @param mzRange m/z range returned by .getMzRange()
#' @param iRange seqNum range returned by .getiRange()
#' @param tRange retentionTime range returned by .gettRange()
#'
#' @return Returns a data.table
#'
#' @details non-exported function for use in getXIC.
#'

.getXIC_dt <- function(mzDt, mzRange, iRange, tRange){
  tRangeIsValid <- !is.null(tRange)
  iRangeIsValid <- !is.null(iRange)

  if(tRangeIsValid){
    #Filter time by retentionTime
    xic <- mzDt[mz %between% mzRange & retentionTime %between% tRange,
                list(sumIntensity = sum(intensity)),
                by = list(seqNum, retentionTime)]

  }else if(iRangeIsValid){
    #Filter time by seqNum
    xic <- mzDt[mz %between% mzRange & seqNum %between% iRange,
                list(sumIntensity = sum(intensity)),
                by = list(seqNum, retentionTime)]

  }else if(iRangeIsValid & tRangeIsValid){
    stop("iStart/iStop and tStart/tStop cannot both be specified")

  }else{
    #Do not filter time
    xic <- mzDt[mz %between% mzRange,
                list(intensity = sum(intensity)),
                by = list(seqNum, retentionTime)]
  }

  #Return
  xic
}

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
#' @param mzObj either a data.table or disk.frame
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