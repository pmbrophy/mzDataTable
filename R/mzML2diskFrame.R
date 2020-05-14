#' Convert .mzML file to disk.frame using mzR, data.table, and disk.frame
#'
#' @param path path to the mzML file
#' @param diskFramePath path and fileName.csv specifying write location of .csv
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
#' @export
#'
#' @examples
mzML2diskFrame <- function(path, diskFramePath, scans = NULL, chunkSize = NULL){
  if(dir.exists(diskFramePath)){
    stop("diskFramePath leads to directory location that already exists")
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

  #Setup disk.frame backend
  disk.frame::setup_disk.frame()
  options(future.globals.maxSize = Inf)

  #Create disk.frame
  diskF <- disk.frame::disk.frame(path = diskFramePath)

  #Write file
  writeResult <- mapply(FUN = .mzML2diskFrameChunk,
                        scans = scanChunks,
                        MoreArgs = list(path = file,
                                        diskFrame = diskF),
                        SIMPLIFY = FALSE)

  #Cleanup
  c <- gc()

  diskF
}

#Import data using mzML2dataTable() and write results to .csv, appending results after the first import
.mzML2diskFrameChunk <- function(path, diskFrame, scans = NULL){
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
    print("Writing all scans to disk.frame")
  }else{
    print(paste("Writing scans from:", scanMin, "to", scanMax, "to disk.frame"))
  }

  #Write the results to a disk.frame
  diskf <- disk.frame::add_chunk(df = diskFrame, chunk = dt)

  remove(dt)
}
