#' @title Get Sum Mass Spectrum
#'
#' @description Sum multiple scans and optionally normalize the spectrum. Scans
#'   are gridded by the grouping algorithm `indexMasterSpectrum()`.
#'
#' @param mzObj a data.table or disk.frame imported from mzML/mzXML file.
#' @param ppmTol The mass accuracy of the instrument in ppm. Default =
#'   NULL
#' @param iStart Optional. The integer start index to begin extracting ions.
#'   Default = NULL.
#' @param iStop Optional. The integer stop index stop extracting ions. Default =
#'   NULL.
#' @param tStart Optional. The numeric time value to begin extracting ions.
#'   Default = NULL.
#' @param tStop Optional. The numeric time value to stop extracting ions.
#'   Default = NULL.
#' @param normalize Logical. Default = `FALSE`. Should the data be normalized?
#' @param isCentroid Logical. The default is `TRUE`. Are the spectra centroids?
#'
#' @return Returns a data.table
#' @export
#'
#' @examples
#'

getSumSpectrum <- function(mzObj, ppmTol = NULL, iStart = NULL, iStop = NULL, tStart = NULL, tStop = NULL, normalize = FALSE, isCentroid = TRUE){
  suppressWarnings(remove(iRange, tRange, envir = .GlobalEnv))
  intensity <- NULL
  mzGrid <- NULL
  mzGrid_index <- NULL

  #Check data.table
  isDataTable <- .check_mzDataTable(mzObj)

  #mz Grid Params
  if(is.null(ppmTol) & isCentroid){
    stop("Mass accuracy in ppm must be provided for centered data.")
  }
  #Calculate ranges to extract (create local, export global for `future`)
  iRange <- .getiRange(mzObj, iStart, iStop)
  tRange <- .gettRange(mzObj, tStart, tStop)
  iRange <<- iRange
  tRange <<- tRange

  #Subset by time
  if(isDataTable){
    sumSpec <- .getSumSpectrum_dt(mzDt = mzObj, iRange = iRange, tRange = tRange)
  }else{
    sumSpec <- .getSumSpectrum_dskF(mzDskF = mzObj, iRange = iRange, tRange = tRange)
  }

  remove(tRange, envir = .GlobalEnv)
  remove(iRange, envir = .GlobalEnv)

  #Grid
  sumSpec <- indexMasterSpectrum(mzDt = sumSpec, ppmTol = ppmTol, isCentroid = isCentroid)

  #Sum each grid
  sumSpec <- sumSpec[, list(intensity = sum(intensity)), by = list(mzGrid_index, mzGrid)]
  data.table::setnames(x = sumSpec, old = "mzGrid", new = "mz")

  #Normalize
  if(normalize){
    normIntensity <- .normalizeSpectrum_dt(mzDt = sumSpec)
    sumSpec[, intensity := NULL]
    sumSpec[, intensity := normIntensity]
  }

  data.table::setkey(x = sumSpec, physical = TRUE, "mzGrid_index")
}


#' Pre-process mzDskF for sum spectrum
#'
#' @details Extract mz and intensity from a disk.frame and optionally filter by
#'   time. Return only mz and intensity as a data.table.
#'
#' @param mzDskF A disk.frame validated by .check_mzDataTable()
#' @param iRange seqNum range returned by .getiRange().
#' @param tRange retentionTime range returned by .gettRange().
#'
#' @return Return only mz and intensity as a data.table.
#'

.getSumSpectrum_dskF <- function(mzDskF, iRange, tRange){
  retentionTime <- NULL
  seqNum <- NULL

  tRangeIsValid <- !is.null(tRange) & .inGlobalEnv(varNames = "tRange")
  iRangeIsValid <- !is.null(iRange) & .inGlobalEnv(varNames = "iRange")

  if(iRangeIsValid & tRangeIsValid){
    stop("iStart/iStop and tStart/tStop cannot both be specified")

  }else if(iRangeIsValid){
    #Filter time by seqNum
    sumSpec <- mzDskF[seqNum %between% iRange,
                      keep = c("mz", "seqNum", "intensity")]
    sumSpec[, seqNum := NULL]

  }else if(tRangeIsValid){
    #Filter time by retentionTime
    sumSpec <- mzDskF[retentionTime %between% tRange,
                      keep = c("mz", "retentionTime", "intensity")]
    sumSpec[, retentionTime := NULL]

  }else{
    #Do not filter time
    sumSpec <- mzDskF[keep = c("mz", "intensity")]

  }
  sumSpec
}

#' Pre-process mzDt for sum spectrum
#'
#' @details Extract mz and intensity from a data.table and optionally filter by
#'   time. Return only mz and intensity as a data.table.
#'
#' @param mzDt a data.table validated by .check_mzDataTable()
#' @param iRange seqNum range returned by .getiRange().
#' @param tRange retentionTime range returned by .gettRange().
#'
#' @return Return only mz and intensity as a data.table.
#'

.getSumSpectrum_dt <- function(mzDt, iRange, tRange){
  intensity <- NULL
  mz <- NULL
  seqNum <- NULL
  retentionTime <- NULL

  tRangeIsValid <- !is.null(tRange) & .inGlobalEnv(varNames = "tRange")
  iRangeIsValid <- !is.null(iRange) & .inGlobalEnv(varNames = "iRange")

  if(iRangeIsValid & tRangeIsValid){
    stop("iStart/iStop and tStart/tStop cannot both be specified")

  }else if(iRangeIsValid){
    #Filter time by seqNum
    sumSpec <- mzDt[seqNum %between% iRange, list(mz, intensity)]

  }else if(tRangeIsValid){
    #Filter time by retentionTime
    sumSpec <- mzDt[retentionTime %between% tRange, list(mz, intensity)]

  }else{
    #Do not filter time
    sumSpec <- mzDt[,list(mz, intensity)]

  }
  sumSpec
}
