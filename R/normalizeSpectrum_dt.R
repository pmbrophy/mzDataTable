#' @title Normalize a mzDt
#'
#' @description Normalize a mzDt by one of the available methods.
#'
#' @param mzDt the mzDt to be normalized in place
#' @param method "maxPeak", "sum", or "sqrt"
#'
#' @return returns mzDt with extra column `intensity_norm`
#'
.normalizeSpectrum_dt <- function(mzDt, method){
  intensity <- NULL
  if(method == "maxPeak"){
    #Divide each point by max intensity
    mzDt[, list(intensity_norm = intensity/max(intensity))]
    
  }else if(method == "sqrt"){
    #Square root intensity transform
    mzDt <- mzDt[, list(intensity_norm = sqrt(intensity)/sqrt(sum(intensity)))]
    mzDt/max(mzDt)
    
  }else if(method == "sum"){
    #Divide each peak by sum total intensity
    mzDt[, list(intensity_norm = intensity/sum(intensity))]
    mzDt/max(mzDt)
    
  }else if(is.null(method)){
    stop("method param is NULL")
    
  }else{
    stop("supplied method param is not supported")
    
  }
}
