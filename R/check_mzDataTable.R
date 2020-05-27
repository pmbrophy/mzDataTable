#' Check mzDt for data structure
#'
#' @param mzDt a data.table
#'
#' @return
#' @export
#'

.check_mzDataTable <- function(mzDt){
  msg <- "\n"
  err <- FALSE
  #Make sure mzDt is a data.table
  if(!data.table::is.data.table(mzDt)){
    msg <- paste(msg, "mzDt is not a data.table. \n")
    err <- TRUE
  }

  #Check for expected column names
  colNames <- colnames(mzDt)
  expectedCols <- c("seqNum", "mz", "intensity", "acquisitionNum", "retentionTime")
  colCheck <- expectedCols %in% colNames

  colsMissing <- !all(colCheck)
  if(colsMissing){
    #expected columns are missing
    msg <- paste(msg, "Expected columns missing:", paste(expectedCols[!colCheck], collapse = ", "), "\n")
    err <- TRUE
  }

  #Make sure there are data
  if(nrow(mzDt) == 0){
    msg <- paste(msg, "mzDt contains no data. \n")
    err <- TRUE
  }

  #Throw error with message
  if(err){
    stop(msg)
  }else{
    NULL
  }
}
