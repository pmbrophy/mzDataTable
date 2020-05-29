#' Calculate total ion chromatogram
#'
#' @param mzObj a data.table or disk.frame imported from mzML/mzXML file.
#' @param normalize Logical. Default = FALSE. Should the data be normalized?
#'
#' @return
#' @export
#'
#' @examples
getTIC <- function(mzObj, normalize = FALSE){
  #Check data.table
  isDataTable <- .check_mzDataTable(mzObj)

  #Sum intensity
  if(isDataTable){
    TIC <- .getTIC_dt(mzDt = mzObj)

  }else{
    TIC <- .getTIC_dskF(mzDskF = mzObj)
  }

  #Option to normalize
  if(normalize){
    normIntensity <- .normalizeSpectrum_dt(TIC)
    #Replace summed intensity with normalized summed intensity
    TIC[, intensity := NULL]
    TIC[, intensity := normIntensity]
  }

  #Sort
  setkey(x = TIC, physical = TRUE, "seqNum")

  TIC
}

#' Calculate total ion chromatogram from disk.frame
#'
#' @details Internal function used by getTIC to calculate TIC from disk.frame.
#'
#' @param mzDskF a disk.frame imported from mzML/mzXML file.
#'
#' @return returns a data.table.
#'

.getTIC_dskF <- function(mzDskF){
  #Sum intensity
  TIC <- mzDskF[, list(intensity = sum(intensity)),
                by = list(seqNum, retentionTime),
                keep = c("intensity", "seqNum", "retentionTime")]

  TIC
}

#' Calculate total ion chromatogram from data.table
#'
#' @details Internal function used by getTIC to calculate TIC from data.table.
#'
#' @param mzDt a data.table imported from mzML/mzXML file.
#'
#' @return returns a data.table.
#'

.getTIC_dt <- function(mzDt){
  #Sum intensity
  TIC <- mzDt[, list(intensity = sum(intensity)),
              by = list(seqNum, retentionTime)]

  TIC
}
