#' @title Get XIC from mzDataTable
#'
#' @description Extract ions by mz using a variety of options for subsetting.
#'   See details.
#'
#' @param mzDt a data.table imported by mzML2dataTable().
#' @param mz the m/z value to extract.
#' @param mz_delta Optional. A numeric value +/- the mz value to extract.
#'   Default = NULL.
#' @param ppmTol Optional. The mass accuracy of the instrument in ppm.
#'   Default = NULL
#' @param iStart Optional. The integer start index to begin extracting ions.
#'   Default = NULL.
#' @param iStop Optional. The integer stop index stop extracting ions. Default =
#'   NULL.
#' @param tStart Optional. The numeric time value to begin extracting ions.
#'   Default = NULL.
#' @param tStop Optional. The numeric time value to stop extracting ions.
#'   Default = NULL.
#'
#' @details mzDt and mz must be provided. Default behavior is to extract ions
#'   exactly matching mz value accross all time.
#'
#'   Specifying mz_delta or ppmTol will result in mz filtering. mz_delta
#'   will extract ions with values in the range where mz >= mz-mz_delta and mz
#'   <= mz+mz_delta. Specifying ppmTol workes similarly, but calculates
#'   the upper and lower bounds from ppm mass error.
#'
#'   Specifying iStart/iStop or tStart/tStop will result in time filtering.
#'
#' @return Returns a subset of the original data.table
#' @export
#'
#' @examples
#'

getXIC_dt <- function(mzDt, mz, mz_delta = NULL, ppmTol = NULL, iStart = NULL, iStop = NULL, tStart = NULL, tStop = NULL){
  #Check data.table
  .check_mzDataTable(mzDt)

  #mz Filter
  if(!is.null(mz_delta) & !is.null(ppmTol)){
    stop("mz_delta and ppmTol provided. Set only one for m/z filter or none for exact matching.")
  }else if(!is.null(mz_delta)){
    #Filter using mz_delta
    mz_min <- mz - mz_delta
    mz_max <- mz + mz_delta
  }else if(!is.null(ppmTol)){
    #Filter using ppmTol
    mz_min <- mz - mz*(1E-6)*ppmTol
    mz_max <- mz + mz*(1E-6)*ppmTol
  }else{
    mz_min <- mz
    mz_max <- mz
  }

  #Time Filter
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

    mzDt[mz %between% c(mz_min, mz_max) & seqNum %between% c(iStart, iStop),
         list(sumIntensity = sum(intensity)),
         by = list(seqNum, retentionTime)]

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

    mzDt[mz %between% c(mz_min, mz_max) & retentionTime %between% c(tStart, tStop),
         list(sumIntensity = sum(intensity)),
         by = list(seqNum, retentionTime)]

  }else{
    ####################################################################################### #
    ##############################       NO TIME FILTER           ######################### #
    ####################################################################################### #

    mzDt[mz %between% c(mz_min, mz_max),
         list(intensity = sum(intensity)),
         by = list(seqNum, retentionTime)]
  }
}


