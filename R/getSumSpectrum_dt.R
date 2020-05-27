#' @title Get Sum Mass Spectrum
#'
#' @description Sum multiple scans and optionally normalize the spectrum. Scans
#'   are gridded by the grouping algorithm `indexMasterSpectrum()`.
#'
#' @param mzDt a data.table imported by `mzML2dataTable()`.
#' @param ppmTolOptional. The mass accuracy of the instrument in ppm. Default =
#'   NULL
#' @param iStart Optional. The integer start index to begin extracting ions.
#'   Default = NULL.
#' @param iStop Optional. The integer stop index stop extracting ions. Default =
#'   NULL.
#' @param tStart Optional. The numeric time value to begin extracting ions.
#'   Default = NULL.
#' @param tStop Optional. The numeric time value to stop extracting ions.
#'   Default = NULL.
#' @param normalize Logical. Default = `FALSE`. Should the data be normalized?
#' @param isCentroid Logical. The default is `TRUE`. Are the spectra centroids?
#'
#' @return Returns a data.table
#' @export
#'
#' @examples
#'

getSumSpectrum_dt <- function(mzDt, ppmTol = NULL, iStart = NULL, iStop = NULL, tStart = NULL, tStop = NULL, normalize = FALSE, isCentroid = TRUE){
  #Check data.table
  .check_mzDataTable(mzDt)

  #mz Grid Params
  if(is.null(ppmTol)){
    stop("Mass accuracy in ppm must be provided.")
  }

  #Subset by time
  if((!is.null(iStart) | !is.null(iStop)) & (!is.null(tStart) | !is.null(tStop))){
    stop("iStart/iStop and tStart/tStop cannot both be specified")
  }else if(!is.null(iStart) | !is.null(iStop)){
    ####################################################################################### #
    ###########################       TIME FILTER BY SeqNum           ##################### #
    ####################################################################################### #

    #Fill Missing Values
    if(is.null(iStart)){
      iStart <- min(mzDt$seqNum)
    }else if(is.null(iStop)){
      iStop <- max(mzDt$seqNum)
    }

    mzDt <- mzDt[seqNum %between% c(iStart, iStop),]

  }else if(!is.null(tStart) | !is.null(tStop)){
    ####################################################################################### #
    #######################       TIME FILTER BY retentionTime           ################## #
    ####################################################################################### #

    #Fill Missing Values
    if(is.null(tStart)){
      tStart <- min(mzDt$retentionTime)
    }else if(is.null(tStop)){
      tStop <- max(mzDt$retentionTime)
    }

    mzDt <- mzDt[retentionTime %between% c(tStart, tStop),]

  }

  #Grid
  mzDt <- indexMasterSpectrum(dt = mzDt, ppmTol = ppmTol, isCentroid = isCentroid)

  #Sum each grid
  mzDt <- mzDt[, list(intensity = sum(intensity)), by = list(mzGrid_index, mzGrid)]
  mzDt
}
