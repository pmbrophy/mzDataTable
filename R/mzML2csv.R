#' @title Convert .mzML file to CSV
#'
#' @description Convert mz data to a CSV file using mzR and data.table.
#'   Individual scans can be indexed and selectively extracted. The operation
#'   can be performed in chunks or all at once.
#'
#' @param path path to the mzML file
#' @param csvPath path and fileName.csv specifying write location of .csv file
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
#' @example
#' \dontrun{ #read .mzML file from system path and write to .csv
#' #100 scans (default) at a time mzML2diskFrame(path = path_2_mzML, csvPath =
#' "csvPath.csv")
#'
#' #read scans 100-200 from .mzML file specified by a system path #and write to
#' .csv 20 scans at a time mzML2diskFrame(path = path_2_mzML, diskFramePath =
#' "dfPath.df", scans = c(100:200), chunkSize = 20)
#' }
#'

mzML2csv <- function(path, csvPath, scans = NULL, chunkSize = 100){
  if(file.exists(csvPath)){
    stop("csvPath leads to file location that already exists")
  }

  #Link to the file
  file <- mzR::openMSfile(filename = path, verbose = TRUE)

  #Generate index to group the scans into chunks
  scanChunks <- .scanChunker(scans = scans,
                             mzRfilePointer = file,
                             chunkSize = chunkSize)

  #Write file
  writeResult <- mapply(FUN = .mzML2csvChunk,
                        scans = scanChunks,
                        MoreArgs = list(path = file,
                                        csvPath = csvPath),
                        SIMPLIFY = FALSE)
  #Cleanup
  c <- gc()

  writeResult
}

#' @title Write data.table of mzML/mzXML data to a .csv
#'
#' @description Internal function. Used by mzML2csv in an mapply function.
#'   Import data using mzML2dataTable() and write results to .csv, appending
#'   results after the first import.
#'
#' @param path path or mzR pointer to the mzML file
#' @param csvPath path to a .csv file
#' @param scans Provide a numeric vector to import select scans and write to a
#'   .csv file. If not provided, the whole file is converted. Likely provided by
#'   .scanChunker()
#'
#' @return NULL
#'

.mzML2csvChunk <- function(path, csvPath, scans){

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
  if(file.exists(csvPath)){
    #append
    data.table::fwrite(x = dt, file = csvPath, append = TRUE, row.names = FALSE, col.names = FALSE, scipen = 2)
  }else{
    #Create
    data.table::fwrite(x = dt, file = csvPath, append = FALSE, row.names = FALSE, col.names = TRUE, scipen = 2)
  }

  remove(dt)
}
