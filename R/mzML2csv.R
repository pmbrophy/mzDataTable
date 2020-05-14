#' Convert .mzML file to CSV using mzR and data.table
#'
#' @param path path to the mzML file
#' @param outputPath path and fileName.csv specifying write location of .csv
#'   file
#' @param scans Optional parameter. Provide a numeric vector to import select
#'   scans and write to a .csv file. If not provided, the whole file is
#'   converted. Default is NULL.
#' @param chunkSize number of scans to be extracted and written to a .csv file
#'   at a time. Useful for breaking up large data files and converting to .csv.
#'   Default is 100. If NULL, imports all scans in single operation and you will
#'   probably run out of memory.
#'
#' @return
#'
#' @export
#'
#' @examples
mzML2csv <- function(path, outputPath, scans = NULL, chunkSize = 100){
  if(file.exists(outputPath)){
    stop("OutputPath leads to file location that already exists")
  }

  #Link to the file
  file <- mzR::openMSfile(filename = path, verbose = TRUE)

  #Get Number of scans
  if(is.null(scans)){
    nScans <- mzR::runInfo(file)$scanCount
    scans <- c(1:nScans)
  }else{
    nScans <- length(scans)
  }

  #Split scans into chunks
  if(is.null(chunkSize)){
    chunkSize <- nScans
  }
  nGroups <- ceiling(nScans/chunkSize)
  scanChunks <- splitIndex(index = scans, nGroups = nGroups, randomize = FALSE)$groups

  #Write file
  writeResult <- mapply(FUN = .mzML2csvChunk,
                        scans = scanChunks,
                        MoreArgs = list(path = file,
                                        outputPath = outputPath),
                        SIMPLIFY = FALSE)
  #Cleanup
  c <- gc()

  writeResult
}

#Import data using mzML2dataTable() and write results to .csv, appending results after the first import
#Internal function to be used by mzML2csv in an mapply function.
.mzML2csvChunk <- function(path, outputPath, scans = NULL){
  #Print Statments: IMPORT
  if(is.null(scans)){
    print("Importing all scans")
  }else{
    scanMin <- min(scans)
    scanMax <- max(scans)
    print(paste("Importing scans from:", scanMin, "to", scanMax))
  }

  dt <- mzML2dataTable(path = path, scans = scans)

  #Print Statements: WRITE
  if(is.null(scans)){
    print("Writing all scans to .csv file")
  }else{
    print(paste("Writing scans from:", scanMin, "to", scanMax, "to .csv file"))
  }

  #Write the results to a csv file
  if(file.exists(outputPath)){
    #append
    data.table::fwrite(x = dt, file = outputPath, append = TRUE, row.names = FALSE, col.names = FALSE, scipen = 2)
  }else{
    #Create
    data.table::fwrite(x = dt, file = outputPath, append = FALSE, row.names = FALSE, col.names = TRUE, scipen = 2)
  }

  remove(dt)
}
