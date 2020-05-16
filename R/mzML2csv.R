#' @title Convert .mzML file to CSV
#'
#' @description Convert mz data to a CSV file using mzR and data.table.
#'   Individual scans can be indexed and selectively extracted. The operation
#'   can be performed in chunks or all at once.
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
#' @return the results from writing the .csv file
#'
#' @export
#'
#' @examples
#'

mzML2csv <- function(path, outputPath, scans = NULL, chunkSize = 100){
  if(file.exists(outputPath)){
    stop("OutputPath leads to file location that already exists")
  }

  #Link to the file
  file <- mzR::openMSfile(filename = path, verbose = TRUE)

  #Group the scans into chunks
  scanChunks <- .scanChunker(scans = scans,
                             mzRfilePointer = file,
                             chunkSize = chunkSize)

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

  dt <- mzML2dataTable(path = path, scans = scans)

  #Print Statements: WRITE
  if(is.null(scans)){
    print("Writing all scans to .csv file")
  }else{
    scanMin <- min(scans)
    scanMax <- max(scans)

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
