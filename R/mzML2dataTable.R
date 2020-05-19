#' @title Import a mzML file and format the data into a data.table
#'
#' @description
#'
#' @param path Either a system path to the .mzML file or mzR pointer object
#' @param scans A numeric vecotr specifying which scans to return. Optional
#'   argument. If ommited, all peaks are returned. Default is `NULL`
#' @param header Option to provide a header data.frame/data.table if it has
#'   already been imported. Expects header spans range of `scans`. Default is
#'   NULL.
#'
#' @return a data.table
#' @export
#'

#TODO: check header for empty/NA fields

mzML2dataTable <- function(path, scans = NULL, header = NULL){
  #Link to the file
  if(is.character(path)){
    if(file.exists(path)){
      #path is character type and the file exists -- Establish mzR pointer
      file <- mzR::openMSfile(filename = path, verbose = TRUE)
    }else{
      stop("path leads to non-existent file")
    }
  }else if(class(path) == "mzRpwiz"){
    file <- path
  }else{
    stop("path argument is not a system path or a S4 mzRpwiz object")
  }

  #Import peak data and optioncally import header data using mzR interface
  if(is.null(scans)){
    print("Importing all scans")

    data <- mzR::peaks(object = file)
    if(is.null(header)){
      header <- mzR::header(object = file)
    }else if(length(data) != nrow(header)){
      #scans == NULL AND header was provided -- ensure same length
      stop("header provided is of different length than imported data")
    }
  }else{
    scanMin <- min(scans)
    scanMax <- max(scans)
    print(paste("Importing scans from:", scanMin, "to", scanMax))

    data <- mzR::peaks(object = file, scans = scans)

    if(length(scans) == 1){
      data <- list(data)
    }

    if(is.null(header)){
      header <- mzR::header(object = file, scans = scans)
    }else if(!all(header$seqNum %in% scans)){
      #Scans provided and header provided -- ensure all scan indicies are in the header
      stop("header provided does not contain scan numbers requested by scans parameter")
    }
  }

  #Scan indexing for the peak data
  nRows <- sapply(X = data, FUN = nrow)
  zeroLength <- which(nRows == 0)

  if(length(zeroLength) > 0){
    index <- rep(x = scans[-zeroLength], times = nRows[-zeroLength])
  }else{
    index <- rep(x = scans, times = nRows)
  }

  #Reformat data
  data <- do.call(what = rbind, data)
  data <- cbind(data, index)
  colnames(data) <- c("mz", "intensity", "seqNum")
  dt <- data.table::as.data.table(data)

  #Format the header and remove scans from header where there are no peaks
  if(length(zeroLength > 0)){
    header <- .formatHeader(headerDF = header[-zeroLength,])
  }else{
    header <- .formatHeader(headerDF = header)
  }

  #Merge header into dt
  data.table::merge.data.table(x = dt, y = header, by = "seqNum")
}

#' @title Format header
#'
#' @description Format a header returned by mzR::header() by removing empty
#' columns, processing spectrumId column, and converting to data.table
#'
#' @param headerDF the header returned by mzR::header()
#' @param removeEmptyCols Default is TRUE. Set FALSE if you want empty columns returned
#'
#' @return Returns a data.table
#'

.formatHeader <- function(headerDF, removeEmptyCols = TRUE){
  if(!is.data.frame(headerDF)){
    headerDF <- as.data.frame(headerDF)
  }

  #Remove empty columns
  if(removeEmptyCols){
    headerDF <- .dropEmptyCols(df = headerDF)
  }

  #Extract numeric values from spectrumId
  m <- .getNumFromSpectrumId(spectrumId = headerDF$spectrumId)

  data.table::data.table(headerDF, m)
}
