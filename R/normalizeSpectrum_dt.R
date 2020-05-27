#' Normalize a mzDt
#'
#' @param mzDt the mzDt to be normalized in place
#'
#' @return returns mzDt with extra column `intensity_norm`
#'
.normalizeSpectrum_dt <- function(mzDt){
  mzDt[, intensity_norm := intensity/max(intensity)]
}
