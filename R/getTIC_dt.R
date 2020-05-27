#' Calculate total ion chromatogram
#'
#' @param mzDt a data.table imported by mzML2dataTable().
#' @param normalize Logical. Default = FALSE. Should the data be normalized?
#'
#' @return returns a data.table possibly containing normalized intensity.
#' @export
#'
#' @examples
#'

getTIC_dt <- function(mzDt, normalize = FALSE){
  #Check data.table
  .check_mzDataTable(mzDt)

  dt <- mzDt[, list(intensity = sum(intensity)), by = list(seqNum, retentionTime)]

  if(normalize){
    .normalizeSpectrum_dt(dt)
  }else{
    dt
  }
}
