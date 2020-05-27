#' Get Base Peak Intensity
#'
#' @param mzDt a data.table imported by mzML2dataTable().
#' @param normalize
#'
#' @return
#' @export
#'
#' @examples
#'

getBPI_dt <- function(mzDt, normalize = FALSE){
  #Check mzDt
  .check_mzDataTable(mzDt)

  dt <- mzDt[, list(intensity = max(intensity), basePeak_mz = mz[which.max(intensity)]), by = list(seqNum, retentionTime)]

  if(normalize){
    normIntensity <- .normalizeSpectrum_dt(dt)
    #Replace summed intensity with normalized summed intensity
    dt[, intensity := NULL]
    dt[, intensity := normIntensity]
  }else{
    dt
  }
}
