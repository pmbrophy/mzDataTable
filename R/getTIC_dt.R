#' Calculate total ion chromatogram
#'
#' @param mzDt a data.table imported by mzML2dataTable().
#' @param normalize Logical. Default = FALSE. Should the data be normalized?
#'
#' @return returns a data.table.
#' @export
#'
#' @examples
#'

getTIC_dt <- function(mzDt, normalize = FALSE){
  #Check data.table
  .check_mzDataTable(mzDt)

  dt <- mzDt[, list(intensity = sum(intensity)), by = list(seqNum, retentionTime)]

  if(normalize){
    normIntensity <- .normalizeSpectrum_dt(dt)
    #Replace summed intensity with normalized summed intensity
    dt[, intensity := NULL]
    dt[, intensity := normIntensity]
  }else{
    dt
  }
}
