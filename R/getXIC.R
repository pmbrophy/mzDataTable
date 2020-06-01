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
#' @param normalize Logical. Default = FALSE. Should the data be normalized?
#' @param method the normalization method. "maxPeak", "sum", or "sqrt".
#' 
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

getXIC <- function(mzObj, mz, mz_delta = NULL, ppmTol = NULL, iStart = NULL, iStop = NULL, tStart = NULL, tStop = NULL, normalize = FALSE, method = "sqrt"){
  suppressWarnings(remove(mzRange, iRange, tRange, envir = .GlobalEnv))

  #Check data.table
  isDataTable <- .check_mzDataTable(mzObj)

  #Calculate ranges to extract (create local, export global for `future`)
  mzRange <- .getMzRange(mz, mz_delta, ppmTol)
  iRange <- .getiRange(mzObj, iStart, iStop)
  tRange <- .gettRange(mzObj, tStart, tStop)
  mzRange <<- mzRange
  iRange <<- iRange
  tRange <<- tRange

  #Do the extraction
  if(isDataTable){
    xic <- .getXIC_dt(mzDt = mzObj, mzRange, iRange, tRange)

  }else{
    xic <- .getXIC_dskF(mzDskF = mzObj, mzRange, iRange, tRange)
  }
  
  #Intensity normalization
  if(normalize){
    BPI <- .normalizeSpectrum_dt(mzDt = BPI, method = method)
    #Rename summed intensity with normalized summed intensity
    BPI[, intensity := NULL]
    data.table::setnames(x = BPI, old = "intensity_norm", new = "intensity")
  }
  
  #Cleanup
  remove(mzRange, iRange, tRange, envir = .GlobalEnv)
  data.table::setkey(x = xic, physical = TRUE, "seqNum")
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
  intensity <- NULL
  mz <- NULL
  seqNum <- NULL
  retentionTime <- NULL

  mzRangeIsValid <- !is.null(mzRange) & .inGlobalEnv(varNames = "mzRange")
  tRangeIsValid <- (!is.null(tRange) & .inGlobalEnv(varNames = "tRange")) & mzRangeIsValid
  iRangeIsValid <- (!is.null(iRange) & .inGlobalEnv(varNames = "iRange")) & mzRangeIsValid

  if(tRangeIsValid & iRangeIsValid){
    #Filter time by retentionTime
    stop("iStart/iStop and tStart/tStop cannot both be specified")

  }else if(iRangeIsValid){
    #Filter time by seqNum
    xic <- mzDskF[mz %between% mzRange & seqNum %between% iRange,
                list(sumIntensity = sum(intensity)),
                by = list(seqNum, retentionTime),
                keep = c("mz", "seqNum", "intensity", "retentionTime")]

  }else if(tRangeIsValid){
    xic <- mzDskF[mz %between% mzRange & retentionTime %between% tRange,
                  list(sumIntensity = sum(intensity)),
                  by = list(seqNum, retentionTime),
                  keep = c("mz", "seqNum", "intensity", "retentionTime")]

  }else{
    #Do not filter time
    if(mzRangeIsValid){
      #Filter mz
      xic <- mzDskF[mz %between% mzRange,
                    list(intensity = sum(intensity)),
                    by = list(seqNum, retentionTime),
                    keep = c("mz", "seqNum", "intensity", "retentionTime")]
    }else{
      stop("filters not specified")

    }

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
  intensity <- NULL
  mz <- NULL
  seqNum <- NULL
  retentionTime <- NULL

  mzRangeIsValid <- !is.null(mzRange) & .inGlobalEnv(varNames = "mzRange")
  tRangeIsValid <- (!is.null(tRange) & .inGlobalEnv(varNames = "tRange")) & mzRangeIsValid
  iRangeIsValid <- (!is.null(iRange) & .inGlobalEnv(varNames = "iRange")) & mzRangeIsValid

  if(tRangeIsValid & iRangeIsValid){
    stop("iStart/iStop and tStart/tStop cannot both be specified")

  }else if(iRangeIsValid){
    #Filter time by seqNum
    xic <- mzDt[mz %between% mzRange & seqNum %between% iRange,
                list(sumIntensity = sum(intensity)),
                by = list(seqNum, retentionTime)]

  }else if(tRangeIsValid){
    #Filter time by retentionTime
    xic <- mzDt[mz %between% mzRange & retentionTime %between% tRange,
                list(sumIntensity = sum(intensity)),
                by = list(seqNum, retentionTime)]

  }else{
    if(mzRangeIsValid){
      #Do not filter time
      xic <- mzDt[mz %between% mzRange,
                  list(intensity = sum(intensity)),
                  by = list(seqNum, retentionTime)]
    }
  }

  #Return
  xic
}
