#' Check mzObj for data structure
#'
#' @details Function is intended to check for expected column types in mzObj
#'   passed to various processing functions. It will return `TRUE` for
#'   data.table and `FALSE` for disk.frame and throw an error if either are not
#'   properly formatted.
#'
#' @param mzObj a data.table or disk.frame imported from mzML/mzXML file.
#'
#' @return return `TRUE` for data.table and `FALSE` for disk.frame.
#'

.check_mzDataTable <- function(mzObj){
  msg <- "\n"
  err <- FALSE
  isDiskFrame <- disk.frame::is_disk.frame(mzObj)
  isDataTable <- data.table::is.data.table(mzObj)
  #Make sure mzObj is a data.table or disk.frame
  if(!isDiskFrame & !isDataTable){
    msg <- paste(msg, "mzObj is not a data.table. \n")
    err <- TRUE
  }else if(isDataTable){
    colNames <- base::colnames(mzObj)
    rows <- base::nrow(mzObj)
  }else if(isDiskFrame){
    colNames <- disk.frame::colnames(mzObj)
    rows <- disk.frame::nrow(mzObj)
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
    msg <- paste(msg, "mzObj contains no data. \n")
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
