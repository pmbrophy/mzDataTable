#' Import a mzML file and format the data into a data.table
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
#' @examples
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

.formatHeader <- function(headerDF){
  if(!is.data.frame(headerDF)){
    stop("Header returned from mzR is not a data.frame")
  }

  #Convert header to data.table
  headerDF <- data.table::as.data.table(headerDF)

  #Extract numeric values
  m <- gsub(pattern = "[[:alpha:]]+=", replacement = "", x = headerDF$spectrumId)
  m <- strsplit(m, split = " ")
  m <- lapply(X = m, FUN = as.numeric)
  m <- do.call(rbind, m)
  m <- data.table::as.data.table(m)

  #Extract names of numeric values
  n <- gsub(pattern = "=[[:digit:]]+", replacement = "", x = headerDF$spectrumId)
  n <- unique(n)
  if(length(n) != 1){
    stop("spectrumId contains non-uniform formatting")
  }
  n <- strsplit(x = unique(n), split = " ")[[1]]

  #Set names
  currentNames <- names(m)
  if(length(n) != length(currentNames)){
    stop("spectrumId names differ in length from extracted data tyeps")
  }

  data.table::setnames(x = m, new = n, old = currentNames)

  data.table::data.table(headerDF, m)
}
