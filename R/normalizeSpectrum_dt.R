#' @title Normalize a mzDt
#'
#' @description Normalize intensity of mzDt by one of the available methods.
#' 
#' @details "maxPeak": divide intensity by the max intensity.
#'   "sqrt": divide intensity using sqrt intensity transform.
#'   "sum" : divide intensity by sum intensity. 
#'   
#' @param mzDt the mzDt to be normalized in place
#' @param method "maxPeak", "sum", or "sqrt"
#'
#' @return returns mzDt with extra column `intensity_norm`
#' 
#' @export
#'
.normalizeSpectrum_dt <- function(mzDt, method){
  #TODO: This function retunrs intensity rather than modifying in place... This should be updated. 
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

#' @title Normalize a mzDt by seqNum
#'
#' @description Normalize intensity of each individual spectrum in the mzDt.
#'
#' @details "maxPeak": divide intensity of each spectrum by the max intensity.
#'   "sqrt": divide intensity of each spectrum using sqrt intensity transform.
#'   "sum" : divide intensity of each spectrum by sum intensity of the spectrum. 
#'
#' @param mzDt the mzDt to be normalized in place
#' @param method "maxPeak", "sum", or "sqrt"
#'
#' @return returns mzDt with extra column `intensity_norm`
#'
#' @export
#' 
.normalizeEachSpectrum_dt <- function(mzDt, method){
  intensity <- NULL
  if(method == "maxPeak"){
    #Divide each point by max intensity
    mzDt[, intensity_norm := intensity, by = seqNum]
    
  }else if(method == "sqrt"){
    #Square root intensity transform
    mzDt[, intensity_norm := sqrt(intensity)/sqrt(sum(intensity)), by = seqNum]
    
  }else if(method == "sum"){
    #Divide each peak by sum total intensity
    mzDt[, intensity_norm := intensity/sum(intensity), by = seqNum]
    
  }else if(is.null(method)){
    stop("method param is NULL")
    
  }else{
    stop("supplied method param is not supported")
    
  }
  #normalize to 1
  mzDt[, intensity_norm := intensity_norm/max(intensity_norm), by = seqNum]
  mzDt
}