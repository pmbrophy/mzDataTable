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
    .normalizeSpectrum_dt(dt)
  }else{
    dt
  }
}
