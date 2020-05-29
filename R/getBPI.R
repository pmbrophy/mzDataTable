#' Get Base Peak Intensity
#'
#' @param mzObj a data.table or disk.frame imported from mzML/mzXML file.
#' @param normalize Logical. Default = FALSE. Should the data be normalized?
#' @param method the normalization method. "maxPeak", "sum", or "sqrt".
#'
#' @return Returns a data.table
#' @export
#'
#' @examples
#'

getBPI <- function(mzObj, normalize = FALSE, method = "sqrt"){
  intensity <- NULL

  #Check mzObj
  isDataTable <- .check_mzDataTable(mzObj)

  #Calculate base peak intensity
  if(isDataTable){
    BPI <- .getBPI_dt(mzDt = mzObj)
  }else{
    BPI <- .getBPI_dskF(mzDskF = mzObj)
  }

  if(normalize){
    normIntensity <- .normalizeSpectrum_dt(mzDt = BPI, method)
    #Replace summed intensity with normalized summed intensity
    BPI[, intensity := NULL]
    BPI[, intensity := normIntensity]
  }

  data.table::setkey(x = BPI, physical = TRUE, "seqNum")

  BPI
}


#' Calculate total ion chromatogram from disk.frame
#'
#' @details Internal function used by getTIC to calculate BPI from disk.frame.
#'
#' @param mzDskF a disk.frame imported from mzML/mzXML file.
#'
#' @return returns a data.table.
#'

.getBPI_dskF <- function(mzDskF){
  intensity <- NULL
  mz <- NULL
  seqNum <- NULL
  retentionTime <- NULL

  BPI <- mzDskF[, list(intensity = max(intensity), basePeak_mz = mz[which.max(intensity)]),
                by = list(seqNum, retentionTime),
                keep = c("intensity", "mz", "seqNum", "retentionTime")]

  BPI
}


#' Calculate total ion chromatogram from data.table
#'
#' @details Internal function used by getTIC to calculate BPI from data.table.
#'
#' @param mzDt a data.table imported from mzML/mzXML file.
#'
#' @return returns a data.table.
#'

.getBPI_dt <- function(mzDt){
  intensity <- NULL
  mz <- NULL
  seqNum <- NULL
  retentionTime <- NULL

  BPI <- mzDt[, list(intensity = max(intensity), basePeak_mz = mz[which.max(intensity)]),
              by = list(seqNum, retentionTime)]

  BPI
}
