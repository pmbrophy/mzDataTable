#' Check mzDt for data structure
#'
#' @details Function is intended to check for expected column types in mzObj
#'   passed to various processing functions. It will return `TRUE` for
#'   data.table and `FALSE` for disk.frame and throw an error if either are not
#'   properly formatted.
#'
#' @param mzDt a data.table or a disk.frame
#'
#' @return return `TRUE` for data.table and `FALSE` for disk.frame.
#' @export
#'

.check_mzDataTable <- function(mzDt){
  msg <- "\n"
  err <- FALSE
  isDiskFrame <- disk.frame::is_disk.frame(mzDt)
  isDataTable <- data.table::is.data.table(mzDt)
  #Make sure mzDt is a data.table or disk.frame
  if(!isDiskFrame & !isDataTable){
    msg <- paste(msg, "mzDt is not a data.table. \n")
    err <- TRUE
  }else if(isDataTable){
    colNames <- base::colnames(mzDt)
    rows <- base::nrow(mzDt)
  }else if(isDiskFrame){
    colNames <- disk.frame::colnames(mzDt)
    rows <- disk.frame::nrow(mzDt)
  }

  #Check Column Names
  expectedCols <- c("seqNum", "mz", "intensity", "acquisitionNum", "retentionTime")
  colCheck <- expectedCols %in% colNames

  colsMissing <- !all(colCheck)
  if(colsMissing){
    #expected columns are missing
    msg <- paste(msg, "Expected columns missing:", paste(expectedCols[!colCheck], collapse = ", "), "\n")
    err <- TRUE
  }

  #Make sure there are data
  if(rows == 0){
    msg <- paste(msg, "mzDt contains no data. \n")
    err <- TRUE
  }

  #Throw error with message
  if(err){
    stop(msg)
  }else{
    if(isDataTable){
      TRUE
    }else{
      FALSE
    }
  }
}
