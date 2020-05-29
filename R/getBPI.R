#' Get Base Peak Intensity
#'
#' @param mzDt a data.table imported by mzML2dataTable().
#' @param normalize Logical. Default = FALSE. Should the data be normalized?
#'
#' @return Returns a data.table
#' @export
#'
#' @examples
#'

getBPI <- function(mzObj, normalize = FALSE){
  #Check mzObj
  isDataTable <- .check_mzDataTable(mzObj)

  #Calculate base peak intensity
  if(isDataTable){
    BPI <- .getBPI_dt(mzDt = mzObj)
  }else{
    BPI <- .getBPI_dskF(mzDskF = mzObj)
  }

  if(normalize){
    normIntensity <- .normalizeSpectrum_dt(BPI)
    #Replace summed intensity with normalized summed intensity
    BPI[, intensity := NULL]
    BPI[, intensity := normIntensity]
  }

  setkey(x = BPI, physical = TRUE, "seqNum")

  BPI
}


#' Calculate total ion chromatogram from disk.frame
#'
#' @details Internal function used by getTIC to calculate TIC from disk.frame.
#'
#' @param mzDskF a disk.frame imported from mzML/mzXML file.
#'
#' @return returns a data.table.
#'

.getBPI_dskF <- function(mzDskF){
  BPI <- mzDskF[, list(intensity = max(intensity), basePeak_mz = mz[which.max(intensity)]),
                by = list(seqNum, retentionTime),
                keep = c("intensity", "mz", "seqNum", "retentionTime")]

  BPI
}


#' Calculate total ion chromatogram from data.table
#'
#' @param mzDt a data.table imported from mzML/mzXML file.
#'
#' @details Internal function used by getTIC to calculate TIC from data.table.
#'
#' @return returns a data.table.
#'

.getBPI_dt <- function(mzDt){
  BPI <- mzDt[, list(intensity = max(intensity), basePeak_mz = mz[which.max(intensity)]),
              by = list(seqNum, retentionTime)]

  BPI
}
